package eu.neverblink.protoc.java.gen

import com.google.protobuf.DescriptorProtos
import com.google.protobuf.DescriptorProtos.*
import com.google.protobuf.compiler.PluginProtos.CodeGeneratorRequest
import com.palantir.javapoet.*
import eu.neverblink.protoc.java.gen.Preconditions.*

import java.util.*
import java.util.function.Function
import java.util.stream.Collectors
import scala.jdk.CollectionConverters.*

/*-
 * #%L
 * quickbuf-generator / CrunchyProtocPlugin
 * %%
 * Copyright (C) 2019 HEBI Robotics
 * %%
 * Copyright (C) 2025 NeverBlink
 * %%
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * #L%
 */

class RequestInfo(val descriptor: CodeGeneratorRequest):
  final private val typeRegistry = TypeRegistry.empty

  val pluginOptions = new PluginOptions(descriptor)
  val files = descriptor.getProtoFileList.stream
    .map((desc: DescriptorProtos.FileDescriptorProto) => new RequestInfo.FileInfo(this, desc))
    .collect(Collectors.toList)

  def getInfoForFile(fileName: String): RequestInfo.FileInfo = {
    for (file <- files.asScala) {
      if (file.fileName.matches(fileName)) return file
    }
    throw new IllegalArgumentException("File was not found in this request: " + fileName)
  }

  def shouldEnumUseArrayLookup(lowestNumber: Int, highestNumber: Int): Boolean =
    lowestNumber >= 0 && highestNumber < 50 // parameter?


/**
 * Meta info that wraps the information in descriptors in a format that is easier to work with
 *
 * @author Florian Enner
 * @author Piotr Sowiński
 */
object RequestInfo:
  def withoutTypeMap(request: CodeGeneratorRequest) = new RequestInfo(request)

  def withTypeRegistry(request: CodeGeneratorRequest): RequestInfo =
    val info = new RequestInfo(request)
    info.typeRegistry.registerContainedTypes(info)
    info

  class FileInfo(
    val parentRequest: RequestInfo, 
    val descriptor: DescriptorProtos.FileDescriptorProto
  ) {
    val sourceMap = SourceLocations.createElementMap(descriptor)
    val fileName = descriptor.getName
    val protoPackage = NamingUtil.getProtoPackage(descriptor)
    val javaPackage = parentRequest.pluginOptions.replacePackageFunction.apply(NamingUtil.getJavaPackage(descriptor))
    val outerClassName = ClassName.get(javaPackage, NamingUtil.getJavaOuterClassname(descriptor))
    val outputDirectory = if (javaPackage.isEmpty) "" else javaPackage.replaceAll("\\.", "/") + "/"
    val options: DescriptorProtos.FileOptions = descriptor.getOptions
    val generateMultipleFiles = options.hasJavaMultipleFiles && options.getJavaMultipleFiles
    val generateDescriptors = parentRequest.pluginOptions.generateDescriptors
    val deprecated = options.hasDeprecated && options.getDeprecated
    // The first message appends a '.', so we omit it to not have two '.' in the default package
    val baseTypeId = if (protoPackage.isEmpty) "" else "." + protoPackage
    val messageTypes = descriptor.getMessageTypeList.stream.map(
      (desc: DescriptorProtos.DescriptorProto) =>
        new RequestInfo.MessageInfo(this, baseTypeId, outerClassName, !generateMultipleFiles, desc)
    ).collect(Collectors.toList)
    val enumTypes = descriptor.getEnumTypeList.stream.map(
      (desc: DescriptorProtos.EnumDescriptorProto) =>
        new RequestInfo.EnumInfo(this, baseTypeId, outerClassName, !generateMultipleFiles, desc)
    ).collect(Collectors.toList)

    def getSourceLocation(identifier: String) = sourceMap.getOrDefault(identifier, SourceCodeInfo.Location.getDefaultInstance)
  }

  abstract class TypeInfo(
    val parentFile: RequestInfo.FileInfo,
    parentTypeId: String,
    parentType: ClassName,
    val isNested: Boolean,
    name: String
  ) {
    val typeName = if (isNested) parentType.nestedClass(name) else parentType.peerClass(name)
    val fieldNamesClass = this.typeName.nestedClass("FieldNames")
    val typeId = parentTypeId + "." + name
    val fullName = if (typeId.startsWith(".")) typeId.substring(1) else typeId
    val sourceLocation = parentFile.getSourceLocation(typeId)
  }

  class MessageInfo(
    parentFile: RequestInfo.FileInfo,
    parentTypeId: String,
    parentType: ClassName,
    isNested: Boolean,
    val descriptor: DescriptorProtos.DescriptorProto
  ) extends RequestInfo.TypeInfo(
    parentFile, parentTypeId, parentType, isNested, descriptor.getName
  ) {
    val mutableTypeName = typeName.nestedClass("Mutable")
    val fieldCount = descriptor.getFieldCount
    val options = parentFile.parentRequest.pluginOptions
    // Extensions in embedded mode: treat extension fields the same as normal
    // fields and embed them directly into the message.
    var fieldList: java.util.List[DescriptorProtos.FieldDescriptorProto] = descriptor.getFieldList
    val nameCollisions = new java.util.HashSet[String]
    val nameCollisionCheck = nameCollisions.contains
    val request: RequestInfo = parentFile.parentRequest
    // Sort fields by serialization order such that they are accessed in a
    // sequential access pattern.
    val sortedFields: java.util.List[DescriptorProtos.FieldDescriptorProto] = 
      fieldList.stream
        .sorted(FieldUtil.MemoryLayoutSorter)
        .collect(Collectors.toList)
    // Build bitfield index map. In the case of OneOf fields we want them grouped
    // together so that we can check all has states in as few bitfield comparisons
    // as possible. If there are no OneOf fields, the order will match the field
    // order.
    var bitIndex = 0
    val bitIndices = new java.util.HashMap[DescriptorProtos.FieldDescriptorProto, Integer]

    for (desc <- sortedFields.stream
      .sorted(FieldUtil.GroupOneOfAndRequiredBits)
      .collect(Collectors.toList).asScala
    ) {
      bitIndices.put(desc, {
        bitIndex += 1; bitIndex - 1
      })
    }

    val implements: Seq[String] = parentFile.parentRequest.pluginOptions.implements
      .getOrElse(typeName.simpleName(), Seq())
    val implementsMutable = parentFile.parentRequest.pluginOptions.implements
      .getOrElse(typeName.simpleName() + ".Mutable", Seq())
    // Build map
    val fields = for desc <- sortedFields.asScala yield
      new RequestInfo.FieldInfo(parentFile, this, typeName, desc, bitIndices.get(desc))
    val nestedTypes = descriptor.getNestedTypeList.stream.map(
      (desc: DescriptorProtos.DescriptorProto) => new RequestInfo.MessageInfo(parentFile, typeId, typeName, true, desc)
    ).collect(Collectors.toList)
    val nestedEnums = descriptor.getEnumTypeList.stream.map(
      (desc: DescriptorProtos.EnumDescriptorProto) => new RequestInfo.EnumInfo(parentFile, typeId, typeName, true, desc)
    ).collect(Collectors.toList)

    val oneOfCount: Int = descriptor.getOneofDeclCount
    val oneOfs = for i <- 0 until oneOfCount yield
      new RequestInfo.OneOfInfo(
        parentFile, this, typeName, descriptor.getOneofDecl(i), i
      )
  }

  class FieldInfo(
    val parentFile: RequestInfo.FileInfo,
    val parentTypeInfo: RequestInfo.MessageInfo,
    val parentType: ClassName,
    val descriptor: DescriptorProtos.FieldDescriptorProto,
    val bitIndex: Int
  ) {
    val fieldId: String = parentTypeInfo.typeId + "." + descriptor.getName
    val sourceLocation = parentFile.getSourceLocation(fieldId)
    var upperCaseName: String = null
    if (isGroup) {
      // name is all lowercase, so convert the type name instead (e.g. ".package.OptionalGroup")
      val name = descriptor.getTypeName
      val packageEndIndex = name.lastIndexOf('.')
      upperCaseName = if (packageEndIndex > 0) name.substring(packageEndIndex + 1)
      else name
    }
    else upperCaseName = NamingUtil.toUpperCamel(descriptor.getName)
    if (
      NamingUtil.isCollidingFieldName(descriptor.getName) ||
        (descriptor.hasExtendee && parentTypeInfo.nameCollisionCheck.apply(descriptor.getName))
    ) upperCaseName += descriptor.getNumber
    val upperName = upperCaseName
    val lowerName = Character.toLowerCase(upperName.charAt(0)) + upperName.substring(1)
    val setterName = "set" + upperName
    val getterName = "get" + upperName
    val hazzerName = "has" + upperName
    val tryGetName = "tryGet" + upperName
    val adderName = "add" + upperName
    val lazyInitName = "init" + upperName
    val isPrimitive = FieldUtil.isPrimitive(descriptor.getType)
    val tag = FieldUtil.makeTag(descriptor)
    val bytesPerTag = FieldUtil.computeRawVarint32Size(tag) + (if (!isGroup) 0 else FieldUtil.computeRawVarint32Size(getEndGroupTag))
    val packedTag = FieldUtil.makePackedTag(descriptor)
    val number = descriptor.getNumber
    val fieldName = NamingUtil.filterKeyword(lowerName)
    val defValue: String = FieldUtil.getEmptyDefaultValue(descriptor.getType)
    val defaultValue = if (isEnum) NamingUtil.filterKeyword(defValue) else defValue
    val repeatedStoreType = RuntimeClasses.getRepeatedStoreType(descriptor.getType)
    val methodAnnotations = if (isDeprecated)
      Collections.singletonList(AnnotationSpec.builder(classOf[Deprecated]).build)
    else Collections.emptyList

    // Original field name (under_score).
    val protoFieldName = descriptor.getName

    def getRepeatedStoreType: TypeName =
      if (isGroup || isMessage) return ParameterizedTypeName.get(repeatedStoreType, getTypeName)
      else if (isEnum) return ParameterizedTypeName.get(repeatedStoreType, getTypeName)
      repeatedStoreType

    def isFixedWidth: Boolean = FieldUtil.isFixedWidth(descriptor.getType)

    def getFixedWidth: Int =
      checkState(isFixedWidth, "not a fixed width type")
      FieldUtil.getFixedWidth(descriptor.getType)

    def isMessageOrGroup: Boolean = isMessage || isGroup

    def getDefaultFieldName: String = "_default" + upperName

    def getInputParameterType: TypeName = descriptor.getType match
      case FieldDescriptorProto.Type.TYPE_STRING =>
        TypeName.get(classOf[CharSequence])
      case FieldDescriptorProto.Type.TYPE_BYTES =>
        if (isRepeated) ArrayTypeName.of(TypeName.BYTE) else TypeName.BYTE
      case _ => getTypeName

    def isPresenceEnabled: Boolean =
      // Checks whether field presence is enabled for this field. See
      // https://github.com/protocolbuffers/protobuf/blob/main/docs/implementing_proto3_presence.md
      //
      // Disabling field presence should:
      // * not generate a has method
      // * not modify bit fields
      // * serialize and compute size when the field value is not zero
      //
      // We only support proto3.
      isMessageOrGroup || isRepeated

    def pluginOptions: PluginOptions = parentFile.parentRequest.pluginOptions

    def getEndGroupTag: Int = FieldUtil.makeGroupEndTag(tag)

    def isGroup: Boolean = descriptor.getType eq FieldDescriptorProto.Type.TYPE_GROUP

    def isMessage: Boolean = descriptor.getType eq FieldDescriptorProto.Type.TYPE_MESSAGE

    def isString: Boolean = descriptor.getType eq FieldDescriptorProto.Type.TYPE_STRING

    def isBytes: Boolean = descriptor.getType eq FieldDescriptorProto.Type.TYPE_BYTES

    def isEnum: Boolean = descriptor.getType eq FieldDescriptorProto.Type.TYPE_ENUM

    def isRequired: Boolean = descriptor.getLabel eq FieldDescriptorProto.Label.LABEL_REQUIRED

    def isOptional: Boolean = descriptor.getLabel eq FieldDescriptorProto.Label.LABEL_OPTIONAL

    def isRepeated: Boolean = descriptor.getLabel eq FieldDescriptorProto.Label.LABEL_REPEATED

    def isSingular: Boolean = !isRepeated

    def isPacked: Boolean = isPackable && descriptor.getOptions.hasPacked && descriptor.getOptions.getPacked

    def isSingularPrimitiveOrEnum: Boolean = isSingular && (isPrimitive || isEnum)

    def isPackable: Boolean =
      if (!isRepeated) return false
      descriptor.getType match
        case FieldDescriptorProto.Type.TYPE_STRING => false
        case FieldDescriptorProto.Type.TYPE_GROUP => false
        case FieldDescriptorProto.Type.TYPE_MESSAGE => false
        case FieldDescriptorProto.Type.TYPE_BYTES => false
        case _ => true

    def isDeprecated: Boolean = descriptor.getOptions.hasDeprecated && descriptor.getOptions.getDeprecated

    def getTypeName: TypeName =
      // Lazy because type registry is not constructed at creation time
      parentFile.parentRequest.typeRegistry.resolveJavaTypeFromProto(descriptor)

    def isMessageOrGroupWithRequiredFieldsInHierarchy: Boolean =
      // Lazy because type registry is not constructed at creation time
      isMessageOrGroup && parentFile.parentRequest.typeRegistry.hasRequiredFieldsInHierarchy(getTypeName)

    def getStoreType: TypeName =
      if (isRepeated) return getRepeatedStoreType
      if (isString) return RuntimeClasses.StringType
      if (isEnum) return TypeName.INT
      getTypeName

    // Used for the return type in the method, e.g., Optional<String>
    def getOptionalReturnType: TypeName =
      if (isRepeated) return ParameterizedTypeName.get(ClassName.get(classOf[Optional[_]]), getRepeatedStoreType)
      val typeName = getTypeName
      if (!isPrimitive || (typeName eq TypeName.BOOLEAN)) 
        return ParameterizedTypeName.get(ClassName.get(classOf[Optional[_]]), typeName.box)
      if (typeName eq TypeName.INT) return TypeName.get(classOf[OptionalInt])
      if (typeName eq TypeName.LONG) return TypeName.get(classOf[OptionalLong])
      if (typeName eq TypeName.FLOAT) return TypeName.get(classOf[OptionalDouble])
      if (typeName eq TypeName.DOUBLE) return TypeName.get(classOf[OptionalDouble])
      throw new IllegalArgumentException("Unhandled type: " + typeName)

    // Used for creating the optional, e.g., Optional.of(string)
    def getOptionalClass: TypeName =
      val t = getOptionalReturnType
      t match
        case name: ParameterizedTypeName => name.rawType
        case _ => t
  }

  class EnumInfo(
    parentFile: RequestInfo.FileInfo, 
    parentTypeId: String, 
    parentType: ClassName,
    isNested: Boolean, 
    val descriptor: DescriptorProtos.EnumDescriptorProto
  ) extends RequestInfo.TypeInfo(
    parentFile, parentTypeId, parentType, isNested, descriptor.getName
  ) {
    var low = 0
    var high = 0
    val usedFields = new java.util.HashSet[Integer]

    val values = new java.util.ArrayList[RequestInfo.EnumValueInfo]
    val aliases = new java.util.ArrayList[RequestInfo.EnumValueInfo]

    val nameInSnakeCase = descriptor.getName
      .split("(?=\\p{Upper})")
      .map(_.toUpperCase)
      .mkString("_")

    for (value <- descriptor.getValueList.asScala) {
      if (usedFields.add(value.getNumber)) {
        values.add(new RequestInfo.EnumValueInfo(this, value))
        low = Math.min(low, value.getNumber)
        high = Math.max(high, value.getNumber)
      }
      else aliases.add(new RequestInfo.EnumValueInfo(this, value))
    }
    val lowestNumber = low
    val highestNumber = high
    val usingArrayLookup = parentFile.parentRequest.shouldEnumUseArrayLookup(lowestNumber, highestNumber)

    def findAliasedValue(alias: RequestInfo.EnumValueInfo): RequestInfo.EnumValueInfo = {
      for (value <- values.asScala) {
        if (alias.getNumber == value.getNumber) return value
      }
      throw new IllegalArgumentException("Enum value does not have an alias")
    }
  }

  class EnumValueInfo(
    var parentType: RequestInfo.EnumInfo, 
    var descriptor: DescriptorProtos.EnumValueDescriptorProto,
  ) {
    val valueId: String = parentType.typeId + "." + descriptor.getName
    val sourceLocation = parentType.parentFile.getSourceLocation(valueId)

    // Simplify names like in scalapb
    def getName: String = descriptor.getName.replace(parentType.nameInSnakeCase + "_", "")

    def getNumber: Int = descriptor.getNumber
  }

  class OneOfInfo(
    val parentFile: RequestInfo.FileInfo, 
    val parentTypeInfo: RequestInfo.MessageInfo, 
    val parentType: ClassName,
    val descriptor: DescriptorProtos.OneofDescriptorProto,
    val oneOfIndex: Int
  ) {

    val upperName = NamingUtil.toUpperCamel(descriptor.getName)
    val fieldName = {
      val lowerName = Character.toLowerCase(upperName.charAt(0)).toString + upperName.substring(1)
      NamingUtil.filterKeyword(lowerName)
    }
    val numberFieldName = fieldName + "Number"
    val getterName = "get" + upperName
    val getNumberName = "get" + upperName + "FieldNumber"
    val setterName = "set" + upperName
    val hazzerName = "has" + upperName
    val clearName = "clear" + upperName

    def getFields: Seq[RequestInfo.FieldInfo] = parentTypeInfo.fields
      .filter(field => field.descriptor.hasOneofIndex)
      .filter(field => field.descriptor.getOneofIndex eq oneOfIndex)
      .toSeq
  }

