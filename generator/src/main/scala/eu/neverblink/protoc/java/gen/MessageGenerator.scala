package eu.neverblink.protoc.java.gen

import com.palantir.javapoet.*
import eu.neverblink.protoc.java.gen.PluginOptions.FieldSerializationOrder
import eu.neverblink.protoc.java.gen.PluginOptions.FieldSerializationOrder.{AscendingNumber, Quickbuf}
import eu.neverblink.protoc.java.gen.RequestInfo.{FieldInfo, MessageInfo}

import java.io.IOException
import java.util.function.Consumer
import java.util.stream.Collectors
import javax.lang.model.element.Modifier
import scala.jdk.CollectionConverters.*

/**
 * @author Florian Enner
 * @since 07 Aug 2019
 */
class MessageGenerator(val info: MessageInfo):

  final val fields = new java.util.ArrayList[FieldGenerator]
  final val m = new java.util.HashMap[String, AnyRef]
  
  info.fields.forEach(f => fields.add(new FieldGenerator(f)))
  m.put("abstractMessage", RuntimeClasses.AbstractMessage)
  m.put("unknownBytes", RuntimeClasses.unknownBytesField)
  m.put("unknownBytesKey", RuntimeClasses.unknownBytesFieldName)

  def generate: TypeSpec =
    val t = TypeSpec.classBuilder(info.typeName)
      .addJavadoc(Javadoc.forMessage(info))
      .superclass(ParameterizedTypeName.get(RuntimeClasses.AbstractMessage, info.typeName))
      .addModifiers(Modifier.PUBLIC, Modifier.FINAL)
    if (info.isNested) t.addModifiers(Modifier.STATIC)
    if (!info.isNested) {
      // Note: constants from enums and fields may have the same names
      // as constants in the nested classes. This causes Java warnings,
      // but is not fatal, so we suppress those warnings in the top-most
      // class declaration /javanano
      t.addAnnotation(AnnotationSpec
        .builder(classOf[SuppressWarnings])
        .addMember("value", "$S", "hiding")
        .build
      )
    }
    // Nested Enums
    info.nestedEnums.stream
      .map(new EnumGenerator(_))
      .map(_.generate)
      .forEach(t.addType)
    // Nested Types
    info.nestedTypes.stream
      .map(new MessageGenerator(_))
      .map(_.generate)
      .forEach(t.addType)
    // newInstance() method
    t.addMethod(MethodSpec.methodBuilder("newInstance")
      .addJavadoc(Javadoc.withComments(info.sourceLocation)
        .add("@return a new empty instance of {@code $T}", info.typeName)
        .build
      )
      .addModifiers(Modifier.PUBLIC, Modifier.STATIC)
      .returns(info.typeName)
      .addStatement("return new $T()", info.typeName)
      .build
    )
    // Constructor
    t.addMethod(MethodSpec.constructorBuilder.addModifiers(Modifier.PRIVATE).build)
    // Member state
    fields.forEach(_.generateMemberFields(t))
    // OneOf Accessors
    info.oneOfs.stream
      .map(new OneOfGenerator(_))
      .forEach(_.generateMemberMethods(t))
    // Fields accessors
    fields.forEach(_.generateMemberMethods(t))
    generateCopyFrom(t)
    generateMergeFromMessage(t)
    generateClear(t)
    generateEquals(t)
    generateWriteTo(t)
    generateComputeSerializedSize(t)
    generateMergeFrom(t)
    generateClone(t)
    // Static utilities
    generateParseFrom(t)
    generateMessageFactory(t)
    // Descriptors
    if (info.parentFile.parentRequest.pluginOptions.generateDescriptors) generateDescriptors(t)
    t.build

  private def generateClear(t: TypeSpec.Builder): Unit =
    t.addMethod(generateClearCode("clear"))

  private def generateClearCode(name: String) =
    val clear = MethodSpec.methodBuilder(name)
      .addJavadoc(Javadoc.inherit)
      .addAnnotation(classOf[Override])
      .addModifiers(Modifier.PUBLIC)
      .returns(info.typeName)
    // no fields set -> no need to clear (e.g. unused nested messages)
    // NOTE: always make sure that the constructor creates conditions that clears everything
    clear.beginControlFlow("if (isEmpty())").addStatement("return this").endControlFlow
    // clear has state
    clear.addStatement("cachedSize = -1")
    fields.forEach(_.generateClearCode(clear))
    clear.addStatement("return this")
    clear.build

  private def generateEquals(t: TypeSpec.Builder): Unit =
    val equals = MethodSpec.methodBuilder("equals")
      .addJavadoc(Javadoc.inherit)
      .addAnnotation(classOf[Override])
      .addModifiers(Modifier.PUBLIC)
      .returns(classOf[Boolean])
      .addParameter(classOf[AnyRef], "o")
    // Reference equality check
    equals.beginControlFlow("if (o == this)").addStatement("return true").endControlFlow
    // Type check
    equals.beginControlFlow("if (!(o instanceof $T))", info.typeName)
      .addStatement("return false")
      .endControlFlow
    equals.addStatement("$1T other = ($1T) o", info.typeName)
    // Check whether all of the same fields are set
    if (info.fieldCount > 0) {
      equals.addCode("return $>")
      for ((field, i) <- fields.asScala.zipWithIndex) {
        if i > 0 then equals.addCode("\n&& ")
        field.generateEqualsStatement(equals)
      }
      equals.addCode(";$<\n")
    }
    else equals.addCode("return true;\n")
    t.addMethod(equals.build)

  private def generateMergeFrom(t: TypeSpec.Builder): Unit =
    val mergeFrom = MethodSpec.methodBuilder("mergeFrom")
      .addJavadoc(Javadoc.inherit)
      .addAnnotation(classOf[Override])
      .addModifiers(Modifier.PUBLIC).returns(info.typeName)
      .addParameter(RuntimeClasses.CodedInputStream, "input", Modifier.FINAL)
      .addException(classOf[IOException])
    // Fallthrough optimization:
    //
    // Reads tag after case parser and checks if it can fall-through. In the ideal case if all fields are set
    // and the expected order matches the incoming data, the switch would only need to be executed once
    // for the first field.
    //
    // Packable fields make this a bit more complex since they need to generate two cases to preserve
    // backwards compatibility. However, any production proto file should already be using the packed
    // option whenever possible, so we don't need to optimize the non-packed case.
    val enableFallthroughOptimization = info.expectedInputOrder ne FieldSerializationOrder.None
    val sortedFields = getFieldSortedByExpectedInputOrder
    if (enableFallthroughOptimization) {
      mergeFrom.addComment("Enabled Fall-Through Optimization (" + info.expectedInputOrder + ")")
      mergeFrom.addAnnotation(AnnotationSpec
        .builder(classOf[SuppressWarnings])
        .addMember("value", "$S", "fallthrough")
        .build
      )
    }
    mergeFrom.addStatement(named("int tag = input.readTag()"))
      .beginControlFlow("while (true)")
      .beginControlFlow("switch (tag)")
    // Add fields by the expected order and type
    for (i <- 0 until sortedFields.size) {
      val field = sortedFields.get(i)
      // Assume all packable fields are written packed. Add non-packed cases to the end.
      var readTag = true
      if (field.info.isPackable) {
        mergeFrom.beginControlFlow("case $L:", field.info.packedTag)
        mergeFrom.addComment("$L [packed=true]", field.info.fieldName)
        readTag = field.generateMergingCodeFromPacked(mergeFrom)
      }
      else {
        mergeFrom.beginControlFlow("case $L:", field.info.tag)
        mergeFrom.addComment("$L", field.info.fieldName)
        readTag = field.generateMergingCode(mergeFrom)
      }
      if (readTag) mergeFrom.addCode(named("tag = input.readTag();\n"))
      if (enableFallthroughOptimization) {
        // try falling to 0 (exit) at last field
        val nextCase = if (i == sortedFields.size - 1) 0
        else getPackedTagOrTag(sortedFields.get(i + 1))
        mergeFrom.beginControlFlow("if (tag != $L)", nextCase)
        mergeFrom.addStatement("break")
        mergeFrom.endControlFlow
      }
      else mergeFrom.addStatement("break")
      mergeFrom.endControlFlow
    }
    // zero means invalid tag / end of data
    mergeFrom.beginControlFlow("case 0:").addStatement("return this").endControlFlow
    // default case -> skip field
    val ifSkipField = named("if (!input.skipField(tag))")
    mergeFrom.beginControlFlow("default:").beginControlFlow(ifSkipField).addStatement("return this")
    mergeFrom.endControlFlow.addStatement(named("tag = input.readTag()")).addStatement("break").endControlFlow
    // Generate missing non-packed cases for packable fields for compatibility reasons
    for (field <- sortedFields.asScala) {
      if (field.info.isPackable) {
        mergeFrom.beginControlFlow("case $L:", field.info.tag)
        mergeFrom.addComment("$L [packed=false]", field.info.fieldName)
        val readTag = field.generateMergingCode(mergeFrom)
        if (readTag) mergeFrom.addCode(named("tag = input.readTag();\n"))
        mergeFrom.addStatement("break").endControlFlow
      }
    }
    mergeFrom.endControlFlow
    mergeFrom.endControlFlow
    t.addMethod(mergeFrom.build)

  private def getPackedTagOrTag(field: FieldGenerator): Int =
    if (field.info.isPackable) return field.info.packedTag
    field.info.tag

  private def generateWriteTo(t: TypeSpec.Builder): Unit =
    val writeTo = MethodSpec.methodBuilder("writeTo")
      .addJavadoc(Javadoc.inherit)
      .addAnnotation(classOf[Override])
      .addModifiers(Modifier.PUBLIC)
      .returns(classOf[Unit])
      .addParameter(RuntimeClasses.CodedOutputStream, "output", Modifier.FINAL)
      .addException(classOf[IOException])
    getFieldSortedByOutputOrder.forEach(f => {
      if (f.info.isRequired) {
        // no need to check has state again
        f.generateSerializationCode(writeTo)
      }
      else {
        // TODO: implement
        // writeTo.beginControlFlow("if ($L)", f.info.hasBit)
        f.generateSerializationCode(writeTo)
        // writeTo.endControlFlow
      }
    })
    t.addMethod(writeTo.build)

  private def generateComputeSerializedSize(t: TypeSpec.Builder): Unit =
    val computeSerializedSize = MethodSpec.methodBuilder("computeSerializedSize")
      .addJavadoc(Javadoc.inherit)
      .addAnnotation(classOf[Override])
      .addModifiers(Modifier.PROTECTED)
      .returns(classOf[Int])
    // Check all required fields at once
    computeSerializedSize.addStatement("int size = 0")
    fields.forEach(f => {
      if (f.info.isRequired) {
        // no need to check has state again
        f.generateComputeSerializedSizeCode(computeSerializedSize)
      }
      else {
        // TODO: implement
        // computeSerializedSize.beginControlFlow("if ($L)", f.info.hasBit)
        f.generateComputeSerializedSizeCode(computeSerializedSize)
        // computeSerializedSize.endControlFlow
      }
    })
    computeSerializedSize.addStatement("return size")
    t.addMethod(computeSerializedSize.build)

  private def generateCopyFrom(t: TypeSpec.Builder): Unit =
    val copyFrom = MethodSpec.methodBuilder("copyFrom")
      .addJavadoc(Javadoc.inherit)
      .addAnnotation(classOf[Override])
      .addParameter(info.typeName, "other", Modifier.FINAL)
      .addModifiers(Modifier.PUBLIC)
      .returns(info.typeName)
    copyFrom.addStatement("cachedSize = other.cachedSize")
    fields.stream.forEach(_.generateCopyFromCode(copyFrom))
    copyFrom.addStatement("return this")
    t.addMethod(copyFrom.build)

  private def generateMergeFromMessage(t: TypeSpec.Builder): Unit =
    val mergeFrom = MethodSpec.methodBuilder("mergeFrom")
      .addJavadoc(Javadoc.inherit)
      .addAnnotation(classOf[Override])
      .addParameter(info.typeName, "other", Modifier.FINAL)
      .addModifiers(Modifier.PUBLIC).returns(info.typeName)
    mergeFrom.beginControlFlow("if (other.isEmpty())").addStatement("return this").endControlFlow
    mergeFrom.addStatement("cachedSize = -1")
    fields.forEach(field => {
      field.generateMergeFromMessageCode(mergeFrom)
    })
    mergeFrom.addStatement("return this")
    t.addMethod(mergeFrom.build)

  private def generateClone(t: TypeSpec.Builder): Unit =
    t.addSuperinterface(classOf[Cloneable])
    t.addMethod(MethodSpec.methodBuilder("clone")
      .addJavadoc(Javadoc.inherit)
      .addAnnotation(classOf[Override])
      .addModifiers(Modifier.PUBLIC)
      .returns(info.typeName)
      .addStatement("return new $T().copyFrom(this)", info.typeName)
      .build
    )

  private def generateParseFrom(t: TypeSpec.Builder): Unit =
    t.addMethod(MethodSpec.methodBuilder("parseFrom")
      .addModifiers(Modifier.PUBLIC, Modifier.STATIC)
      .addException(RuntimeClasses.InvalidProtocolBufferException)
      .addParameter(classOf[Array[Byte]], "data", Modifier.FINAL)
      .returns(info.typeName)
      .addStatement("return $T.mergeFrom(new $T(), data).checkInitialized()", RuntimeClasses.AbstractMessage, info.typeName)
      .build
    )
    t.addMethod(MethodSpec.methodBuilder("parseFrom")
      .addModifiers(Modifier.PUBLIC, Modifier.STATIC)
      .addException(classOf[IOException])
      .addParameter(RuntimeClasses.CodedInputStream, "input", Modifier.FINAL)
      .returns(info.typeName)
      .addStatement("return $T.mergeFrom(new $T(), input).checkInitialized()", RuntimeClasses.AbstractMessage, info.typeName)
      .build
    )

  private def generateMessageFactory(t: TypeSpec.Builder): Unit =
    val factoryReturnType = ParameterizedTypeName.get(RuntimeClasses.MessageFactory, info.typeName)
    val factoryTypeName = info.typeName.nestedClass(info.typeName.simpleName + "Factory")
    val factoryMethod = MethodSpec.methodBuilder("create")
      .addJavadoc(Javadoc.inherit)
      .addAnnotation(classOf[Override])
      .addModifiers(Modifier.PUBLIC)
      .returns(info.typeName)
      .addStatement("return $T.newInstance()", info.typeName)
      .build
    val factoryEnum = TypeSpec.enumBuilder(factoryTypeName.simpleName)
      .addModifiers(Modifier.PRIVATE)
      .addSuperinterface(factoryReturnType)
      .addEnumConstant("INSTANCE")
      .addMethod(factoryMethod)
      .build
    t.addType(factoryEnum)
    t.addMethod(MethodSpec.methodBuilder("getFactory")
      .addJavadoc("@return factory for creating $T messages", info.typeName)
      .addModifiers(Modifier.PUBLIC, Modifier.STATIC)
      .returns(factoryReturnType)
      .addStatement("return $T.INSTANCE", factoryTypeName)
      .build
    )

  private def generateDescriptors(t: TypeSpec.Builder): Unit =
    val descriptorClass = info.parentFile.outerClassName
    val fieldName = DescriptorGenerator.getDescriptorFieldName(info)
    t.addMethod(MethodSpec.methodBuilder("getDescriptor")
      .addJavadoc("@return this type's descriptor.")
      .addModifiers(Modifier.PUBLIC, Modifier.STATIC)
      .returns(RuntimeClasses.MessageDescriptor)
      .addStatement("return $T.$N", descriptorClass, fieldName)
      .build
    )

  private def getFieldSortedByExpectedInputOrder: java.util.List[FieldGenerator] =
    info.expectedInputOrder match
      case AscendingNumber =>
        val sortedFields = new java.util.ArrayList(fields)
        sortedFields.sort(FieldUtil.AscendingNumberSorter)
        return sortedFields
      case Quickbuf => // keep existing order
      case FieldSerializationOrder.None => // no optimization
    fields

  private def getFieldSortedByOutputOrder: java.util.List[FieldGenerator] =
    // Sorts output the same way as protobuf. This is always slower,
    // but it results in binary equivalence for conformance tests.
    if (info.outputOrder eq FieldSerializationOrder.AscendingNumber) {
      val sortedFields = new java.util.ArrayList(fields)
      sortedFields.sort(FieldUtil.AscendingNumberSorter)
      return sortedFields
    }
    fields

  private def named(format: String, args: AnyRef*) =
    CodeBlock.builder.addNamed(format, m).build
