package eu.neverblink.protoc.java.gen

import com.google.protobuf.DescriptorProtos.FieldDescriptorProto
import com.google.protobuf.DescriptorProtos.FieldDescriptorProto.Type.*
import com.palantir.javapoet.ClassName

/**
 * TypeNames of all API classes that can be referenced from generated code
 *
 * @author Florian Enner
 * @since 07 Aug 2019
 */
object RuntimeClasses:
  private val API_PACKAGE = "us.hebi.quickbuf"
  val ProtoSource = ClassName.get(API_PACKAGE, "ProtoSource")
  val ProtoSink = ClassName.get(API_PACKAGE, "ProtoSink")
  val ProtoUtil = ClassName.get(API_PACKAGE, "ProtoUtil")
  val AbstractMessage = ClassName.get(API_PACKAGE, "ProtoMessage")
  val MessageFactory = ClassName.get(API_PACKAGE, "MessageFactory")
  val StringType = ClassName.get(API_PACKAGE, "Utf8String")
  val Utf8Decoder = ClassName.get(API_PACKAGE, "Utf8Decoder")
  val BytesType = ClassName.get(API_PACKAGE, "RepeatedByte")
  val InvalidProtocolBufferException = ClassName.get(API_PACKAGE, "InvalidProtocolBufferException")
  val UninitializedMessageException = ClassName.get(API_PACKAGE, "UninitializedMessageException")
  val JsonSink = ClassName.get(API_PACKAGE, "JsonSink")
  val JsonSource = ClassName.get(API_PACKAGE, "JsonSource")
  val FieldName = ClassName.get(API_PACKAGE, "FieldName")
  val ProtoEnum = ClassName.get(API_PACKAGE, "ProtoEnum")
  val EnumConverter = ProtoEnum.nestedClass("EnumConverter")
  val FileDescriptor = ClassName.get(API_PACKAGE, "Descriptors").nestedClass("FileDescriptor")
  val MessageDescriptor = ClassName.get(API_PACKAGE, "Descriptors").nestedClass("Descriptor")
  val unknownBytesField = "unknownBytes"
  val unknownBytesFieldName = "unknownBytesFieldName"
  val unknownBytesFieldHash = "[quickbuf.unknown_bytes]".hashCode
  private val RepeatedDouble = ClassName.get(API_PACKAGE, "RepeatedDouble")
  private val RepeatedFloat = ClassName.get(API_PACKAGE, "RepeatedFloat")
  private val RepeatedLong = ClassName.get(API_PACKAGE, "RepeatedLong")
  private val RepeatedInt = ClassName.get(API_PACKAGE, "RepeatedInt")
  private val RepeatedBoolean = ClassName.get(API_PACKAGE, "RepeatedBoolean")
  private val RepeatedString = ClassName.get(API_PACKAGE, "RepeatedString")
  private val RepeatedBytes = ClassName.get(API_PACKAGE, "RepeatedBytes")
  val RepeatedMessage = ClassName.get(API_PACKAGE, "RepeatedMessage")
  val RepeatedEnum = ClassName.get(API_PACKAGE, "RepeatedEnum")

  def getRepeatedStoreType(t: FieldDescriptorProto.Type) = t match
    case TYPE_DOUBLE => RepeatedDouble
    case TYPE_FLOAT => RepeatedFloat
    case TYPE_SFIXED64 => RepeatedLong
    case TYPE_FIXED64 => RepeatedLong
    case TYPE_SINT64 => RepeatedLong
    case TYPE_INT64 => RepeatedLong
    case TYPE_UINT64 => RepeatedLong
    case TYPE_SFIXED32 => RepeatedInt
    case TYPE_FIXED32 => RepeatedInt
    case TYPE_SINT32 => RepeatedInt
    case TYPE_INT32 => RepeatedInt
    case TYPE_UINT32 => RepeatedInt
    case TYPE_BOOL => RepeatedBoolean
    case TYPE_ENUM => RepeatedEnum
    case TYPE_STRING => RepeatedString
    case TYPE_GROUP => RepeatedMessage
    case TYPE_MESSAGE => RepeatedMessage
    case TYPE_BYTES => RepeatedBytes
    case _ => throw new IllegalStateException("Unexpected value: " + t)
