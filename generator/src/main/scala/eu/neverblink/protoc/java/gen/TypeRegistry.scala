package eu.neverblink.protoc.java.gen

import com.google.protobuf.DescriptorProtos.FieldDescriptorProto
import com.google.protobuf.DescriptorProtos.FieldDescriptorProto.Type.*
import com.palantir.javapoet.{ClassName, TypeName}
import eu.neverblink.protoc.java.gen.PluginOptions.AllocationStrategy
import eu.neverblink.protoc.java.gen.Preconditions.*
import eu.neverblink.protoc.java.gen.RequestInfo.{MessageInfo, TypeInfo}
import eu.neverblink.protoc.java.gen.TypeRegistry.RequiredType

import scala.jdk.CollectionConverters.*

object TypeRegistry:
  def empty = new TypeRegistry

  enum RequiredType:
    case Required, Optional, Processing


class TypeRegistry:
  final val typeMap = new java.util.HashMap[String, ClassName]
  final val messageMap = new java.util.HashMap[TypeName, MessageInfo]
  final val hasRequiredMap = new java.util.HashMap[TypeName, RequiredType]
  
  def resolveJavaTypeFromProto(descriptor: FieldDescriptorProto): TypeName =
    descriptor.getType match
      case TYPE_DOUBLE => TypeName.DOUBLE
      case TYPE_FLOAT => TypeName.FLOAT
      case TYPE_SFIXED64 => TypeName.LONG
      case TYPE_FIXED64 => TypeName.LONG
      case TYPE_SINT64 => TypeName.LONG
      case TYPE_INT64 => TypeName.LONG
      case TYPE_UINT64 => TypeName.LONG
      case TYPE_SFIXED32 => TypeName.INT
      case TYPE_FIXED32 => TypeName.INT
      case TYPE_SINT32 => TypeName.INT
      case TYPE_INT32 => TypeName.INT
      case TYPE_UINT32 => TypeName.INT
      case TYPE_BOOL => TypeName.BOOLEAN
      case TYPE_STRING => TypeName.get(classOf[String])
      case TYPE_ENUM => resolveMessageType(descriptor.getTypeName)
      case TYPE_GROUP => resolveMessageType(descriptor.getTypeName)
      case TYPE_MESSAGE => resolveMessageType(descriptor.getTypeName)
      case TYPE_BYTES => RuntimeClasses.BytesType
      case _ => throw new Exception("Unsupported type: " + descriptor)

  def resolveMessageType(typeId: String) =
    checkNotNull(typeMap.get(typeId), "Unable to resolve type id: " + typeId)

  def registerContainedTypes(info: RequestInfo): Unit =
    typeMap.clear
    for (file <- info.files.asScala) {
      file.messageTypes.forEach(registerType)
      file.enumTypes.forEach(registerType)
    }

  private def registerType(typeInfo: TypeInfo): Unit =
    if (typeMap.containsValue(typeInfo.typeName)) 
      throw new Exception("Duplicate class name: " + typeInfo.typeName)
    if (typeMap.put(typeInfo.typeId, typeInfo.typeName) != null)
      throw new Exception("Duplicate type id: " + typeInfo.typeId)
    typeInfo match
      case msgInfo: MessageInfo =>
        msgInfo.nestedTypes.forEach(registerType)
        msgInfo.nestedEnums.forEach(registerType)
        messageMap.put(typeInfo.typeName, msgInfo)
      case _ =>

  /**
   * Checks message types for any required fields in their hierarchy. Many
   * cases don't have any or very few required fields, so we don't need to
   * check the messages that will always return true anyways.
   */
  def hasRequiredFieldsInHierarchy(t: TypeName): Boolean =
    if (!messageMap.containsKey(t)) throw new IllegalStateException("Not a message or group type: " + t)
    // Lazily compute for each message
    if (!hasRequiredMap.containsKey(t)) {
      hasRequiredMap.put(t, TypeRegistry.RequiredType.Processing)
      var hasRequired = false
      val info = messageMap.get(t)
      for (field <- info.fields) {
        if (isRequiredFieldOrNeedsToBeChecked(t, field)) hasRequired = true
      }
      hasRequiredMap.put(t, if (hasRequired) TypeRegistry.RequiredType.Required
      else TypeRegistry.RequiredType.Optional)
      return hasRequired
    }
    // Return cached result
    val result = hasRequiredMap.get(t)
    checkState(
      result ne TypeRegistry.RequiredType.Processing, 
      "Processing required fields did not finish"
    )
    result eq TypeRegistry.RequiredType.Required

  private def isRequiredFieldOrNeedsToBeChecked(t: TypeName, field: RequestInfo.FieldInfo): Boolean =
    // Always check message types for recursion to avoid surprises at runtime
    if (field.isMessageOrGroup) {
      val result = hasRequiredMap.get(field.getTypeName)
      if (result eq TypeRegistry.RequiredType.Processing) {
        // This state is only possible while processing nested messages, so
        // users won't see it. If any fields turn out to be required, the user
        // call still returns true
        if (field.pluginOptions.allocationStrategy eq AllocationStrategy.Eager) {
          val msg = String.format(
            "Detected recursive message definition in '%s' field '%s'. This is not " +
              "compatible with eager allocation. You need to specify lazy allocation instead.",
            t,
            field.protoFieldName
          )
          throw new Exception(msg)
        }
      }
      else if ((result eq TypeRegistry.RequiredType.Required) || field.isRequired) return true
      else if (result == null) return hasRequiredFieldsInHierarchy(field.getTypeName)
    }
    field.isRequired
