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
  private val API_PACKAGE = "eu.neverblink.protoc.java.runtime"
  private val JAVA_UTIL_PACKAGE = "java.util"
  private val GOOGLE_PACKAGE = "com.google.protobuf"

  val CodedInputStream = ClassName.get(GOOGLE_PACKAGE, "CodedInputStream")
  val LimitedCodedInputStream = ClassName.get(API_PACKAGE, "LimitedCodedInputStream")
  val CodedOutputStream = ClassName.get(GOOGLE_PACKAGE, "CodedOutputStream")
  val ProtoUtil = ClassName.get(API_PACKAGE, "ProtoUtil")
  val AbstractMessage = ClassName.get(API_PACKAGE, "ProtoMessage")
  val MessageFactory = ClassName.get(API_PACKAGE, "MessageFactory")
  val ObjectType = ClassName.get(classOf[Object])
  val StringType = ClassName.get(classOf[String])
  val BytesType = ClassName.get(GOOGLE_PACKAGE, "ByteString")
  val Exception = ClassName.get(classOf[Exception])
  val RuntimeException = ClassName.get(classOf[RuntimeException])
  val InvalidProtocolBufferException = ClassName.get(GOOGLE_PACKAGE, "InvalidProtocolBufferException")
  val UninitializedMessageException = ClassName.get(GOOGLE_PACKAGE, "UninitializedMessageException")
  val ProtoEnum = ClassName.get(API_PACKAGE, "ProtoEnum")
  val EnumConverter = ProtoEnum.nestedClass("EnumConverter")
  val FileDescriptor = ClassName.get(GOOGLE_PACKAGE, "Descriptors").nestedClass("FileDescriptor")
  val FileDescriptorProto = ClassName.get(GOOGLE_PACKAGE, "DescriptorProtos").nestedClass("FileDescriptorProto")
  val MessageDescriptor = ClassName.get(GOOGLE_PACKAGE, "Descriptors").nestedClass("Descriptor")
  private val RepeatedDouble = ClassName.get(API_PACKAGE, "RepeatedDouble")
  private val RepeatedFloat = ClassName.get(API_PACKAGE, "RepeatedFloat")
  private val RepeatedLong = ClassName.get(API_PACKAGE, "RepeatedLong")
  private val RepeatedInt = ClassName.get(API_PACKAGE, "RepeatedInt")
  private val RepeatedBoolean = ClassName.get(API_PACKAGE, "RepeatedBoolean")
  private val RepeatedString = ClassName.get(API_PACKAGE, "RepeatedString")
  private val RepeatedBytes = ClassName.get(GOOGLE_PACKAGE, "ByteString")
  val List = ClassName.get(JAVA_UTIL_PACKAGE, "List")
  val ArrayList = ClassName.get(JAVA_UTIL_PACKAGE, "ArrayList")
  val RepeatedEnum = ClassName.get(API_PACKAGE, "RepeatedEnum")
  val Collections = ClassName.get(JAVA_UTIL_PACKAGE, "Collections")
  val Base64 = ClassName.get(JAVA_UTIL_PACKAGE, "Base64")

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
    case TYPE_GROUP => List
    case TYPE_MESSAGE => List
    case TYPE_BYTES => RepeatedBytes
    case _ => throw new IllegalStateException("Unexpected value: " + t)
