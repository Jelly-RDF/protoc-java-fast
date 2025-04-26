package eu.neverblink.protoc.java.gen

import com.palantir.javapoet.*
import eu.neverblink.protoc.java.Helpers
import eu.neverblink.protoc.java.gen.RequestInfo.FieldInfo

import javax.lang.model.element.Modifier
import java.util
import java.util.function.Consumer

/**
 * This class generates all serialization logic and field related accessors.
 * It is a bit of a mess due to lots of switch statements, but I found that
 * splitting the types up similarly to how the protobuf-generator code is
 * organized makes it really difficult to find and manage duplicate code,
 * and to keep track of where things are being called.
 *
 * @author Florian Enner
 * @since 07 Aug 2019
 */
object FieldGenerator:
  private def generateWriteVarint32(value: Int) =
    var value2 = value
    // Split tag into individual bytes
    val bytes = new Array[Int](5)
    var numBytes = 0
    var continue = true
    while continue do 
      if (value2 & ~0x7F) == 0 then
        bytes(numBytes) = value2
        numBytes += 1
        continue = false
      else
        bytes(numBytes) = (value2 & 0x7F) | 0x80
        numBytes += 1
        value2 >>>= 7
      
    // Write tag bytes as efficiently as possible
    var output = ""
    numBytes match {
      case x if x >= 4 && x <= 5 =>
        val fourBytes = bytes(3) << 24 | bytes(2) << 16 | bytes(1) << 8 | bytes(0)
        output = "output.writeRawLittleEndian32(" + fourBytes + ");\n"
        if (numBytes == 5) output += "output.writeRawByte((byte) " + bytes(4) + ");\n"

      case x if x >= 2 && x <= 3 =>
        val twoBytes = bytes(1) << 8 | bytes(0)
        output = "output.writeRawLittleEndian16((short) " + twoBytes + ");\n"
        if (numBytes == 3) output += "output.writeRawByte((byte) " + bytes(2) + ");\n"

      case _ =>
        for (i <- 0 until numBytes) {
          output += "output.writeRawByte((byte) " + bytes(i) + ");\n"
        }
    }
    output

  private val EMPTY_BLOCK = CodeBlock.builder.build

class FieldGenerator(val info: FieldInfo):
  val typeName = info.getTypeName
  val storeType = info.getStoreType
  
  // Common-variable map for named arguments
  final val m = new util.HashMap[String, Any]
  m.put("field", info.fieldName)
  m.put("default", info.defaultValue)
  if (info.isEnum) {
    m.put("default",
      if (info.hasDefaultValue) info.getTypeName.toString + "." + info.defaultValue + "_VALUE"
      else "0"
    )
    m.put("defaultEnumValue", info.getTypeName.toString + "." + info.defaultValue)
    m.put("protoEnum", info.getTypeName)
  }
  m.put("storeType", storeType)
  m.put("getMutableMethod", info.mutableGetterName)
  m.put("lazyInitMethod", info.lazyInitName)
  m.put("getMethod", info.getterName)
  m.put("setMethod", info.setterName)
  m.put("addMethod", info.adderName)
  m.put("hasMethod", info.hazzerName)
  m.put("clearMethod", info.clearName)
  m.put("getHas", info.hasBit)
  m.put("setHas", info.setBit)
  m.put("clearHas", info.clearBit)
  m.put("message", info.parentType)
  m.put("type", typeName)
  m.put("number", info.number)
  m.put("tag", info.tag)
  m.put("capitalizedType", FieldUtil.getCapitalizedType(info.descriptor.getType))
  m.put("secondArgs", if (info.isGroup) ", " + info.number
  else "")
  m.put("defaultField", info.getDefaultFieldName)
  m.put("bytesPerTag", info.bytesPerTag)
  m.put("valueOrNumber", if (info.isEnum) "value.getNumber()"
  else "value")
  m.put("optional", info.getOptionalClass)
  if (info.isPackable) m.put("packedTag", info.packedTag)
  if (info.isFixedWidth) m.put("fixedWidth", info.getFixedWidth)
  if (info.isRepeated) m.put("getRepeatedIndex_i", 
    if (info.isPrimitive || info.isEnum) "array()[i]" else "get(i)"
  )
  // utility classes
  m.put("fieldNames", info.parentTypeInfo.fieldNamesClass)
  m.put("abstractMessage", RuntimeClasses.AbstractMessage)
  m.put("protoSource", RuntimeClasses.ProtoSource)
  m.put("protoSink", RuntimeClasses.ProtoSink)
  m.put("protoUtil", RuntimeClasses.ProtoUtil)
  // Common configuration-dependent code blocks
  val clearOtherOneOfs = generateClearOtherOneOfs
  val enforceHasCheck = generateEnforceHasCheck
  val ensureFieldNotNull = lazyFieldInit

  def generateMemberFields(t: TypeSpec.Builder): Unit = {
    val field = FieldSpec.builder(storeType, info.fieldName)
      .addJavadoc(Javadoc.forMessageField(info).build)
      .addModifiers(Modifier.PRIVATE)
    if (info.isLazyAllocationEnabled) field.initializer("null")
    else if (info.isRepeated || info.isMessageOrGroup || info.isBytes || info.isString) 
      field.addModifiers(Modifier.FINAL).initializer(initializer)
    else if (info.isPrimitive || info.isEnum) {
      if (info.hasDefaultValue) field.initializer(initializer)
    }
    else throw new IllegalStateException("unhandled field: " + info.descriptor)
    t.addField(field.build)
    if (info.isBytes && info.hasDefaultValue) {
      // byte[] default values are stored as utf8 strings, so we need to convert it first
      t.addField(FieldSpec
        .builder(Helpers.TYPE_BYTE_ARRAY, info.getDefaultFieldName)
        .addModifiers(Modifier.PRIVATE, Modifier.STATIC, Modifier.FINAL)
        .initializer(named("$abstractMessage:T.bytesDefaultValue(\"$default:L\")"))
        .build
      )
    }
  }

  private def initializer =
    val initializer = CodeBlock.builder
    if (info.isRepeated && info.isMessageOrGroup) initializer.add("$T.newEmptyInstance($T.getFactory())", RuntimeClasses.RepeatedMessage, info.getTypeName)
    else if (info.isRepeated && info.isEnum) initializer.add("$T.newEmptyInstance($T.converter())", RuntimeClasses.RepeatedEnum, info.getTypeName)
    else if (info.isRepeated) initializer.add("$T.newEmptyInstance()", storeType)
    else if (info.isBytes) if (!info.hasDefaultValue) initializer.add(named("$storeType:T.newEmptyInstance()"))
    else initializer.add(named("$storeType:T.newInstance($defaultField:N)"))
    else if (info.isMessageOrGroup) initializer.add(named("$storeType:T.newInstance()"))
    else if (info.isString) if (!info.hasDefaultValue) initializer.add(named("$storeType:T.newEmptyInstance()"))
    else initializer.add(named("$storeType:T.newInstance($default:S)"))
    else if (info.isPrimitive || info.isEnum) if (info.hasDefaultValue) initializer.add(named("$default:L"))
    else throw new IllegalStateException("unhandled field: " + info.descriptor)
    initializer.build

  def generateClearCode(method: MethodSpec.Builder): Unit =
    if (info.isSingularPrimitiveOrEnum) {
      method.addStatement(named("$field:N = $default:L"))
      return
    }
    if (info.isLazyAllocationEnabled) method.beginControlFlow(named("if ($field:N != null)"))
    if (info.isRepeated || info.isMessageOrGroup) method.addStatement(named("$field:N.clear()"))
    else if (info.isString) if (info.hasDefaultValue) method.addStatement(named("$field:N.copyFrom($default:S)"))
    else method.addStatement(named("$field:N.clear()"))
    else if (info.isBytes) if (info.hasDefaultValue) method.addStatement(named("$field:N.copyFrom($defaultField:N)"))
    else method.addStatement(named("$field:N.clear()"))
    else throw new IllegalStateException("unhandled field: " + info.descriptor)
    if (info.isLazyAllocationEnabled) method.endControlFlow

  def generateClearQuickCode(method: MethodSpec.Builder): Unit =
    if (info.isSingularPrimitiveOrEnum) return // no action needed
    if (info.isLazyAllocationEnabled) method.beginControlFlow(named("if ($field:N != null)"))
    if (info.isMessageOrGroup) { // includes repeated messages
      method.addStatement(named("$field:N.clearQuick()"))
    }
    else if (info.isRepeated || info.isBytes || info.isString) method.addStatement(named("$field:N.clear()"))
    else throw new IllegalStateException("unhandled field: " + info.descriptor)
    if (info.isLazyAllocationEnabled) method.endControlFlow

  def generateCopyFromCode(method: MethodSpec.Builder): Unit =
    if (info.isSingularPrimitiveOrEnum) method.addStatement(named("$field:N = other.$field:N"))
    else if (info.isRepeated || info.isBytes || info.isMessageOrGroup || info.isString) if (info.isLazyAllocationEnabled) method.addCode(named("" + "if (other.$hasMethod:N()) {$>\n" + "$lazyInitMethod:L();\n" + "$field:N.copyFrom(other.$field:N);\n" + "$<} else {$>\n" + "$clearMethod:L();\n" + "$<}\n"))
    else method.addStatement(named("$field:N.copyFrom(other.$field:N)"))
    else throw new IllegalStateException("unhandled field: " + info.descriptor)

  def generateMergeFromMessageCode(method: MethodSpec.Builder): Unit =
    if (info.isRepeated) method.addStatement(named("$getMutableMethod:N().addAll(other.$field:N)"))
    else if (info.isMessageOrGroup) method.addStatement(named("$getMutableMethod:N().mergeFrom(other.$field:N)"))
    else if (info.isBytes) method.addStatement(named("$getMutableMethod:N().copyFrom(other.$field:N)"))
    else if (info.isString) method.addStatement(named("$getMutableMethod:NBytes().copyFrom(other.$field:N)"))
    else if (info.isEnum) method.addStatement(named("$setMethod:NValue(other.$field:N)"))
    else if (info.isPrimitive) method.addStatement(named("$setMethod:N(other.$field:N)"))
    else throw new IllegalStateException("unhandled field: " + info.descriptor)

  def generateEqualsStatement(method: MethodSpec.Builder): Unit =
    if (info.isRepeated || info.isBytes || info.isMessageOrGroup || info.isString) method.addNamedCode("$field:N.equals(other.$field:N)", m)
    else if ((typeName eq TypeName.DOUBLE) || (typeName eq TypeName.FLOAT)) method.addNamedCode("$protoUtil:T.isEqual($field:N, other.$field:N)", m)
    else if (info.isPrimitive || info.isEnum) method.addNamedCode("$field:N == other.$field:N", m)
    else throw new IllegalStateException("unhandled field: " + info.descriptor)

  /**
   * @return true if the tag needs to be read
   */
  def generateMergingCode(method: MethodSpec.Builder): Boolean =
    method.addCode(clearOtherOneOfs).addCode(ensureFieldNotNull)
    if (info.isRepeated) {
      method.addNamedCode("tag = input.readRepeated$capitalizedType:L($field:N, tag);\n", m).addStatement(named("$setHas:L"))
      return false // tag is already read, so don't read again

    }
    else if (info.isString || info.isMessageOrGroup || info.isBytes) method.addStatement(named("input.read$capitalizedType:L($field:N$secondArgs:L)")).addStatement(named("$setHas:L"))
    else if (info.isPrimitive) method.addStatement(named("$field:N = input.read$capitalizedType:L()")).addStatement(named("$setHas:L"))
    else if (info.isEnum) {
      method.addStatement("final int value = input.readInt32()").beginControlFlow("if ($T.forNumber(value) != null)", typeName).addStatement(named("$field:N = value")).addStatement(named("$setHas:L"))
      // NOTE:
      //  Google's Protobuf-Java selectively moves repeated enum values that it does not know.
      //  This is problematic when going through a routing node as it may change the order of the
      //  data by sorting it as known values followed by unknown values. Even though this is
      //  the specified behavior, I don't think that this is desired and would rather have users
      //  deal with potential null values.
      if (info.storeUnknownFieldsEnabled) method.nextControlFlow("else").addStatement("input.skipEnum(tag, value, $N)", RuntimeClasses.unknownBytesField)
      method.endControlFlow
    }
    else throw new IllegalStateException("unhandled field: " + info.descriptor)
    true

  /**
   * @return true if the tag needs to be read
   */
  def generateMergingCodeFromPacked(method: MethodSpec.Builder): Boolean =
    if (!info.isPackable) throw new IllegalStateException("not a packable type: " + info.descriptor)
    method.addCode(clearOtherOneOfs).addCode(ensureFieldNotNull)
    if (info.isFixedWidth) method.addStatement(named("input.readPacked$capitalizedType:L($field:N)"))
    else method.addStatement(named("input.readPacked$capitalizedType:L($field:N, tag)"))
    method.addStatement(named("$setHas:L"))
    true

  def generateSerializationCode(method: MethodSpec.Builder): Unit = {
    m.put("writeTagToOutput", FieldGenerator.generateWriteVarint32(info.tag))
    if (info.isPacked) m.put("writePackedTagToOutput", FieldGenerator.generateWriteVarint32(info.packedTag))
    m.put("writeEndGroupTagToOutput", if (!info.isGroup) ""
    else FieldGenerator.generateWriteVarint32(info.getEndGroupTag))
    if (info.isPacked) method.addNamedCode("" + "$writePackedTagToOutput:L" + "output.writePacked$capitalizedType:LNoTag($field:N);\n", m)
    else if (info.isRepeated) method.addNamedCode("" + 
      "for (int i = 0; i < $field:N.length(); i++) {$>\n" + 
      "$writeTagToOutput:L" + 
      "output.write$capitalizedType:LNoTag($field:N.$getRepeatedIndex_i:L);\n" +
      "$writeEndGroupTagToOutput:L" + 
      "$<}\n", 
      m
    )
    else {
      // unroll varint tag loop
      method.addNamedCode("" + // non-repeated
        "$writeTagToOutput:L" + 
        "output.write$capitalizedType:LNoTag($field:N);\n" + 
        "$writeEndGroupTagToOutput:L", 
        m
      )
    }
  }

  def generateComputeSerializedSizeCode(method: MethodSpec.Builder): Unit =
    if (info.isFixedWidth && info.isPacked) method.addNamedCode("" + 
      "final int dataSize = $fixedWidth:L * $field:N.length();\n" + 
      "size += $bytesPerTag:L + $protoSink:T.computeDelimitedSize(dataSize);\n", 
      m
    )
    else if (info.isFixedWidth && info.isRepeated) { // non packed
      method.addStatement(named("size += ($bytesPerTag:L + $fixedWidth:L) * $field:N.length()"))
    }
    else if (info.isPacked) method.addNamedCode("" + 
      "final int dataSize = $protoSink:T.computeRepeated$capitalizedType:LSizeNoTag($field:N);\n" +
      "size += $bytesPerTag:L + $protoSink:T.computeDelimitedSize(dataSize);\n", 
      m
    )
    else if (info.isRepeated) { // non packed
      method.addNamedCode("" + 
        "size += ($bytesPerTag:L * $field:N.length()) + $protoSink:T.computeRepeated$capitalizedType:LSizeNoTag($field:N);\n", 
        m
      )
    }
    else if (info.isFixedWidth) 
      method.addStatement("size += $L", info.bytesPerTag + info.getFixedWidth) // non-repeated
    else method.addStatement(named(
      "size += $bytesPerTag:L + $protoSink:T.compute$capitalizedType:LSizeNoTag($field:N)"
    )) // non-repeated

  def generateMemberMethods(t: TypeSpec.Builder): Unit =
    generateInitializedMethod(t)
    generateHasMethod(t)
    generateClearMethod(t)
    generateGetMethods(t)
    if (info.isEnum) generateExtraEnumAccessors(t)
    if (info.isTryGetAccessorEnabled) generateTryGetMethod(t)
    generateSetMethods(t)

  def generateInitializedMethod(t: TypeSpec.Builder): Unit =
    if (info.isLazyAllocationEnabled)
      t.addMethod(MethodSpec.methodBuilder(info.lazyInitName)
        .addModifiers(Modifier.PRIVATE)
        .addCode(CodeBlock.builder
          .beginControlFlow("if ($N == null)", info.fieldName)
          .add(named("$field:N = "))
          .addStatement(initializer)
          .endControlFlow
          .build
        ).build
      )

  private def lazyFieldInit = if (info.isLazyAllocationEnabled)
    CodeBlock.builder.addStatement("$N()", info.lazyInitName).build
  else FieldGenerator.EMPTY_BLOCK

  def generateHasMethod(t: TypeSpec.Builder): Unit =
    t.addMethod(MethodSpec.methodBuilder(info.hazzerName)
      .addJavadoc(Javadoc.forMessageField(info)
        .add("\n@return whether the $L field is set", info.fieldName)
        .build
      )
      .addAnnotations(info.methodAnnotations)
      .addModifiers(Modifier.PUBLIC)
      .returns(TypeName.BOOLEAN)
      .addStatement(named("return $getHas:L"))
      .build
    )

  def generateSetMethods(t: TypeSpec.Builder): Unit =
    if (info.isRepeated || info.isBytes) {
      val adder = MethodSpec.methodBuilder(info.adderName)
        .addJavadoc(Javadoc.forMessageField(info)
          .add("\n@param value the $L to add", info.fieldName)
          .add("\n@return this")
          .build
        )
        .addAnnotations(info.methodAnnotations)
        .addModifiers(Modifier.PUBLIC)
        .addParameter(info.getInputParameterType, "value", Modifier.FINAL)
        .returns(info.parentType)
        .addCode(clearOtherOneOfs)
        .addCode(ensureFieldNotNull)
        .addStatement(named("$setHas:L"))
        .addStatement(named("$field:N.add(value)"))
        .addStatement(named("return this"))
        .build
      t.addMethod(adder)
      val addAll = MethodSpec.methodBuilder("addAll" + info.upperName)
        .addJavadoc(Javadoc.forMessageField(info)
          .add("\n@param values the $L to add", info.fieldName)
          .add("\n@return this")
          .build
        )
        .addAnnotations(info.methodAnnotations)
        .addModifiers(Modifier.PUBLIC)
        .addParameter(ArrayTypeName.of(info.getInputParameterType), "values", Modifier.FINAL)
        .varargs(true)
        .returns(info.parentType)
        .addCode(clearOtherOneOfs)
        .addCode(ensureFieldNotNull)
        .addStatement(named("$setHas:L"))
        .addStatement(named("$field:N.addAll(values)"))
        .addStatement(named("return this"))
      t.addMethod(addAll.build)
      if (info.isBytes) {
        val setBytes = MethodSpec.methodBuilder("set" + info.upperName)
          .addJavadoc(Javadoc.forMessageField(info)
            .add("\n@param values the $L to set", info.fieldName)
            .add("\n@return this")
            .build
          )
          .addAnnotations(info.methodAnnotations)
          .addModifiers(Modifier.PUBLIC)
          .addParameter(ArrayTypeName.of(info.getInputParameterType), "values", Modifier.FINAL)
          .varargs(true)
          .returns(info.parentType)
          .addCode(clearOtherOneOfs)
          .addCode(ensureFieldNotNull)
          .addStatement(named("$setHas:L"))
          .addStatement(named("$field:N.copyFrom(values)"))
          .addStatement(named("return this"))
        t.addMethod(setBytes.build)
      }
    }
    else if (info.isMessageOrGroup || info.isString) {
      val setter = MethodSpec.methodBuilder(info.setterName)
        .addJavadoc(Javadoc.forMessageField(info)
          .add("\n@param value the $L to set", info.fieldName)
          .add("\n@return this")
          .build
        )
        .addAnnotations(info.methodAnnotations)
        .addModifiers(Modifier.PUBLIC)
        .returns(info.parentType)
        .addParameter(info.getInputParameterType, "value", Modifier.FINAL)
        .addCode(clearOtherOneOfs)
        .addCode(ensureFieldNotNull)
        .addStatement(named("$setHas:L"))
        .addStatement(named("$field:N.copyFrom(value)"))
        .addStatement(named("return this"))
        .build
      t.addMethod(setter)
      if (info.isString) { // setString(Utf8String)
        t.addMethod(MethodSpec.methodBuilder(info.setterName)
          .addJavadoc(Javadoc.forMessageField(info)
            .add("\n@param value the $L to set", info.fieldName)
            .add("\n@return this")
            .build
          )
          .addAnnotations(info.methodAnnotations)
          .addModifiers(Modifier.PUBLIC)
          .returns(info.parentType)
          .addParameter(RuntimeClasses.StringType, "value", Modifier.FINAL)
          .addCode(clearOtherOneOfs)
          .addCode(ensureFieldNotNull)
          .addStatement(named("$setHas:L"))
          .addStatement(named("$field:N.copyFrom(value)"))
          .addStatement(named("return this"))
          .build
        )
      }
    }
    else if (info.isPrimitive || info.isEnum) {
      val setter = MethodSpec.methodBuilder(info.setterName)
        .addJavadoc(Javadoc.forMessageField(info)
          .add("\n@param value the $L to set", info.fieldName)
          .add("\n@return this")
          .build
        )
        .addAnnotations(info.methodAnnotations)
        .addModifiers(Modifier.PUBLIC)
        .addParameter(info.getTypeName, "value", Modifier.FINAL)
        .returns(info.parentType)
        .addCode(clearOtherOneOfs)
        .addCode(ensureFieldNotNull)
        .addNamedCode("" + "$setHas:L;\n" + "$field:N = $valueOrNumber:L;\n" + "return this;\n", m)
        .build
      t.addMethod(setter)
    }

  /**
   * Enums are odd because they need to be converter back and forth and they
   * don't have the same type as the internal/repeated store. The normal
   * accessors provide access to the enum value, but for performance reasons
   * we also add accessors for the internal storage type that do not require
   * conversions.
   *
   * @param type
   */
  def generateExtraEnumAccessors(t: TypeSpec.Builder): Unit = {
    if (!info.isEnum || info.isRepeated) return
    // Overload to get the internal store without conversion
    t.addMethod(MethodSpec.methodBuilder(info.getterName + "Value")
      .addAnnotations(info.methodAnnotations)
      .addJavadoc(named("" +
        "Gets the value of the internal enum store. The result is\n" +
        "equivalent to {@link $message:T#$getMethod:N()}.getNumber().\n" +
        "\n" +
        "@return numeric wire representation"
      ))
      .addModifiers(Modifier.PUBLIC)
      .returns(classOf[Int])
      .addCode(enforceHasCheck)
      .addCode(ensureFieldNotNull)
      .addStatement(named("return $field:N"))
      .build
    )
    // Overload to set the internal value without conversion
    t.addMethod(MethodSpec.methodBuilder(info.setterName + "Value")
      .addAnnotations(info.methodAnnotations)
      .addJavadoc(named("" +
        "Sets the value of the internal enum store. This does not\n" +
        "do any validity checks, so be sure to use appropriate value\n" +
        "constants from {@link $type:T}. Setting an invalid value\n" +
        "can cause {@link $message:T#$getMethod:N()} to return null\n" +
        "\n" +
        "@param value the numeric wire value to set\n" +
        "@return this"
      ))
      .addModifiers(Modifier.PUBLIC)
      .addParameter(classOf[Int], "value", Modifier.FINAL)
      .returns(info.parentType)
      .addNamedCode("" + "$setHas:L;\n" + "$field:N = value;\n" + "return this;\n", m)
      .build
    )
  }

  def generateTryGetMethod(t: TypeSpec.Builder): Unit = {
    val tryGet = MethodSpec.methodBuilder(info.tryGetName)
      .addJavadoc(Javadoc.forMessageField(info)
        .add("\n@return the value of $L if it is set, or empty if not", info.fieldName)
        .build
      )
      .addAnnotations(info.methodAnnotations)
      .addModifiers(Modifier.PUBLIC)
      .returns(info.getOptionalReturnType)
    tryGet.beginControlFlow(named("if ($hasMethod:N())"))
      .addStatement(named("return $optional:T.of($getMethod:N())"))
      .nextControlFlow("else")
      .addStatement(named("return $optional:T.empty()"))
      .endControlFlow
    t.addMethod(tryGet.build)
  }

  def generateGetMethods(t: TypeSpec.Builder): Unit =
    val getter = MethodSpec.methodBuilder(info.getterName)
      .addAnnotations(info.methodAnnotations)
      .addModifiers(Modifier.PUBLIC)
      .addCode(enforceHasCheck)
      .addCode(ensureFieldNotNull)
    if (info.isRepeated)
      getter.returns(storeType).addStatement(named("return $field:N"))
    else if (info.isString)
      getter.returns(typeName).addStatement(named("return $field:N.getString()"))
    else if (info.isEnum)
      if (info.hasDefaultValue)
        getter.returns(typeName).addStatement(named("return $type:T.forNumberOr($field:N, $defaultEnumValue:L)"))
      else
        getter.returns(typeName).addStatement(named("return $type:T.forNumber($field:N)"))
    else
      getter.returns(typeName).addStatement(named("return $field:N"))
    if (info.isRepeated || info.isMessageOrGroup || info.isBytes) {
      getter.addJavadoc(Javadoc.forMessageField(info)
        .add(named("\n\n" +
          "This method returns the internal storage object without modifying any has state.\n" +
          "The returned object should not be modified and be treated as read-only.\n" +
          "\n" +
          "Use {@link #$getMutableMethod:N()} if you want to modify it.\n" +
          "\n" +
          "@return internal storage object for reading"
        )).build
      )
      val mutableGetter = MethodSpec.methodBuilder(info.mutableGetterName)
        .addJavadoc(Javadoc.forMessageField(info)
          .add(named("\n\n" +
            "This method returns the internal storage object and sets the corresponding\n" +
            "has state. The returned object will become part of this message and its\n" +
            "contents may be modified as long as the has state is not cleared.\n\n" +
            "@return internal storage object for modifications"
          ))
          .build
        )
        .addAnnotations(info.methodAnnotations)
        .addModifiers(Modifier.PUBLIC)
        .returns(storeType)
        .addCode(clearOtherOneOfs)
        .addCode(ensureFieldNotNull)
        .addStatement(named("$setHas:L"))
        .addStatement(named("return $field:N"))
        .build
      t.addMethod(getter.build)
      t.addMethod(mutableGetter)
    }
    else t.addMethod(getter.addJavadoc(Javadoc.forMessageField(info)
      .add("\n@return the $L", info.fieldName)
      .build
    ).build)
    // Add an overload for Strings that let users get the backing Utf8Bytes
    if (!info.isRepeated && info.isString) {
      t.addMethod(MethodSpec.methodBuilder(info.getterName + "Bytes")
        .addJavadoc(Javadoc.forMessageField(info)
          .add(
            "\n@return internal {@code $T} representation of $L for reading",
            RuntimeClasses.StringType,
            info.fieldName
          )
          .build
        )
        .addAnnotations(info.methodAnnotations)
        .addModifiers(Modifier.PUBLIC)
        .returns(storeType)
        .addCode(enforceHasCheck)
        .addCode(ensureFieldNotNull)
        .addStatement(named("return this.$field:N"))
        .build
      )
      t.addMethod(MethodSpec.methodBuilder(info.mutableGetterName + "Bytes")
        .addJavadoc(Javadoc.forMessageField(info)
          .add(
            "\n@return internal {@code $T} representation of $L for modifications",
            RuntimeClasses.StringType,
            info.fieldName
          )
          .build
        )
        .addAnnotations(info.methodAnnotations)
        .addModifiers(Modifier.PUBLIC)
        .returns(storeType)
        .addCode(clearOtherOneOfs)
        .addCode(ensureFieldNotNull)
        .addStatement(named("$setHas:L"))
        .addStatement(named("return this.$field:N"))
        .build
      )
    }

  def generateClearMethod(t: TypeSpec.Builder): Unit =
    val method = MethodSpec.methodBuilder(info.clearName)
      .addJavadoc(Javadoc.forMessageField(info).add("\n@return this").build)
      .addAnnotations(info.methodAnnotations)
      .addModifiers(Modifier.PUBLIC)
      .returns(info.parentType)
      .addStatement(named("$clearHas:L"))
    generateClearCode(method)
    method.addStatement("return this")
    t.addMethod(method.build)

  private def generateClearOtherOneOfs: CodeBlock =
    if (!info.hasOtherOneOfFields) return FieldGenerator.EMPTY_BLOCK
    CodeBlock.builder.addStatement("$N()", info.getClearOtherOneOfName).build

  private def generateEnforceHasCheck: CodeBlock =
    if (!info.isEnforceHasCheckEnabled) return FieldGenerator.EMPTY_BLOCK
    CodeBlock.builder.beginControlFlow("if (!$N())", info.hazzerName)
      .addStatement(
        "throw new $T($S)",
        classOf[IllegalStateException],
        "Field is not set. Check has state before accessing."
      )
      .endControlFlow
      .build

  private def named(format: String, args: AnyRef*) =
    CodeBlock.builder.addNamed(format, m).build

  private def code(c: Consumer[CodeBlock.Builder]) =
    val block = CodeBlock.builder
    c.accept(block)
    block.build
