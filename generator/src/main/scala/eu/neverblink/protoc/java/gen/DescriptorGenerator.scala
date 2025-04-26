package eu.neverblink.protoc.java.gen

import com.google.protobuf.DescriptorProtos
import com.palantir.javapoet.{CodeBlock, FieldSpec, MethodSpec, TypeSpec}

import java.util
import java.util.Base64
import javax.lang.model.element.Modifier
import scala.jdk.CollectionConverters.*

/**
 * @author Florian Enner
 * @since 21 Sep 2023
 */
object DescriptorGenerator {
  def getDescriptorBytesFieldName = "descriptorData"

  def getFileDescriptorFieldName = "descriptor"

  def getDescriptorFieldName(info: RequestInfo.MessageInfo) =
    // uniquely identifiable descriptor name. Similar to protobuf-java
    // but without the "internal_static_" prefix.
    info.fullName.replaceAll("\\.", "_") + "_descriptor"

  private def findOffset(data: Array[Byte], part: Array[Byte], start: Int): Int =
    // Start search at the start guess
    for (i <- start until data.length) {
      if (isAtOffset(data, part, i)) return i
    }
    // Search before just in case
    for (i <- 0 until start) {
      if (isAtOffset(data, part, i)) return i
    }
    throw new IllegalArgumentException("part is not contained inside data")

  private def isAtOffset(data: Array[Byte], part: Array[Byte], offset: Int): Boolean = {
    for (i <- part.indices) {
      if (data(offset + i) != part(i)) return false
    }
    true
  }

  /**
   * The Protobuf-Java descriptor does some symbol stripping (e.g. jsonName only appears if it was specified),
   * so serializing the raw descriptor does not produce binary compatibility. I don't know whether it's worth
   * implementing it, so for now we leave it empty. See
   * https://github.com/protocolbuffers/protobuf/blob/209accaf6fb91aa26e6086e73626e1884ddfb737/src/google/protobuf/compiler/retention.cc#L105-L116
   * Note that this would also need to be stripped from Message descriptors to work with offsets.
   */
  private def stripSerializedDescriptor(descriptor: DescriptorProtos.FileDescriptorProto) = descriptor
}

class DescriptorGenerator(val info: RequestInfo.FileInfo):
  final val m = new util.HashMap[String, AnyRef]
  m.put("abstractMessage", RuntimeClasses.AbstractMessage)
  m.put("protoUtil", RuntimeClasses.ProtoUtil)
  val fileDescriptorBytes = DescriptorGenerator.stripSerializedDescriptor(info.descriptor).toByteArray

  def generate(t: TypeSpec.Builder): Unit =
    // bytes shared by everything
    t.addField(FieldSpec.builder(RuntimeClasses.BytesType, DescriptorGenerator.getDescriptorBytesFieldName).addModifiers(Modifier.PRIVATE, Modifier.STATIC, Modifier.FINAL).initializer(generateEmbeddedByteBlock(fileDescriptorBytes)).build)
    // field for the main file descriptor
    val initBlock = CodeBlock.builder
    initBlock.add(
      "$T.internalBuildGeneratedFileFrom($S, $S, $N",
      RuntimeClasses.FileDescriptor,
      info.descriptor.getName,
      info.descriptor.getPackage,
      DescriptorGenerator.getDescriptorBytesFieldName
    )
    // any file dependencies
    if (info.descriptor.getDependencyCount > 0) {
      for (fileName <- info.descriptor.getDependencyList.asScala) {
        initBlock.add(", $T.getDescriptor()", info.parentRequest.getInfoForFile(fileName).outerClassName)
      }
    }
    initBlock.add(")")
    val fileDescriptor = FieldSpec
      .builder(RuntimeClasses.FileDescriptor, DescriptorGenerator.getFileDescriptorFieldName)
      .addModifiers(Modifier.STATIC, Modifier.FINAL)
      .initializer(initBlock.build)
      .build
    t.addField(fileDescriptor)
    // Add a static method
    t.addMethod(MethodSpec.methodBuilder("getDescriptor")
      .addJavadoc("@return this proto file's descriptor.")
      .addModifiers(Modifier.PUBLIC, Modifier.STATIC)
      .returns(RuntimeClasses.FileDescriptor)
      .addStatement("return $N", DescriptorGenerator.getFileDescriptorFieldName)
      .build
    )
    // Descriptor field for each nested type. The message protos should
    // be serialized sequentially, so we start the next search offset where
    // the last descriptor ended.
    var offset = 0
    for (message <- info.messageTypes.asScala) {
      offset = addMessageDescriptor(t, message, offset)
    }

  private def addMessageDescriptor(
    t: TypeSpec.Builder, message: RequestInfo.MessageInfo, startOffset: Int
  ): Int =
    var startOffset2 = startOffset
    val msgDesc = message.descriptor
    val descriptorBytes = message.descriptor.toByteArray
    val offset = DescriptorGenerator.findOffset(fileDescriptorBytes, descriptorBytes, startOffset2)
    val length = descriptorBytes.length
    t.addField(FieldSpec
      .builder(RuntimeClasses.MessageDescriptor, DescriptorGenerator.getDescriptorFieldName(message))
      .addModifiers(Modifier.STATIC, Modifier.FINAL)
      .initializer(
        "$N.internalContainedType($L, $L, $S, $S)", 
        DescriptorGenerator.getFileDescriptorFieldName,
        offset, length, msgDesc.getName, message.fullName
      )
      .build
    )
    // Recursively add nested messages
    for (nestedType <- message.nestedTypes.asScala) {
      startOffset2 = addMessageDescriptor(t, nestedType, startOffset2)
    }
    // Messages should be serialized sequentially, so we start
    // the next search where the current descriptor ends.
    offset + length

  private def generateEmbeddedByteBlock(descriptor: Array[Byte]) =
    // Inspired by Protoc's SharedCodeGenerator::GenerateDescriptors:
    //
    // Embed the descriptor.  We simply serialize the entire FileDescriptorProto
    // and embed it as a string literal, which is parsed and built into real
    // descriptors at initialization time.  We unfortunately have to put it in
    // a string literal, not a byte array, because apparently using a literal
    // byte array causes the Java compiler to generate *instructions* to
    // initialize each and every byte of the array, e.g. as if you typed:
    //   b[0] = 123; b[1] = 456; b[2] = 789;
    // This makes huge bytecode files and can easily hit the compiler's internal
    // code size limits (error "code to large").  String literals are apparently
    // embedded raw, which is what we want.
    // Note: Protobuf uses escaped ISO_8859_1 strings, but for now we use Base64
    // Every block of bytes, start a new string literal, in order to avoid the
    // 64k length limit. Note that this value needs to be <64k.
    val charsPerLine = 80 // should be a multiple of 4

    val linesPerPart = 20
    val charsPerPart = linesPerPart * charsPerLine
    val bytesPerPart = charsPerPart * 3 / 4 // 3x 8 bit => 4x 6 bit

    // Construct bytes from individual base64 String sections
    val initBlock = CodeBlock.builder
    initBlock.addNamed("$protoUtil:T.decodeBase64(", m).add("$L$>", descriptor.length)
    var partIx = 0
    while (partIx < descriptor.length) {
      val part = util.Arrays.copyOfRange(descriptor, partIx, Math.min(descriptor.length, partIx + bytesPerPart))
      val block = Base64.getEncoder.encodeToString(part)
      var line = block.substring(0, Math.min(charsPerLine, block.length))
      initBlock.add(",\n$S", line)
      var blockIx = line.length
      while (blockIx < block.length) {
        line = block.substring(blockIx, Math.min(blockIx + charsPerLine, block.length))
        initBlock.add(" + \n$S", line)
        blockIx += charsPerLine
      }
      partIx += bytesPerPart
    }
    initBlock.add(")$<")
    initBlock.build

