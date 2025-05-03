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
    t.addField(FieldSpec
      .builder(Helpers.TYPE_BYTE_ARRAY, DescriptorGenerator.getDescriptorBytesFieldName)
      .addModifiers(Modifier.PRIVATE, Modifier.STATIC, Modifier.FINAL)
      .initializer(generateEmbeddedByteBlock(fileDescriptorBytes))
      .build
    )
    // field for the main file descriptor
    val initBlock = CodeBlock.builder
    initBlock.add(
      "$T.buildFrom($T.parseFrom($N), new $T[] {})",
      RuntimeClasses.FileDescriptor,
      RuntimeClasses.FileDescriptorProto,
      DescriptorGenerator.getDescriptorBytesFieldName,
      RuntimeClasses.FileDescriptor,
    )
    // any file dependencies
//    if (info.descriptor.getDependencyCount > 0) {
//      for (fileName <- info.descriptor.getDependencyList.asScala) {
//        initBlock.add(", $T.getDescriptor()", info.parentRequest.getInfoForFile(fileName).outerClassName)
//      }
//    }
//    initBlock.add(")")
    val fileDescriptor = FieldSpec
      .builder(RuntimeClasses.FileDescriptor, DescriptorGenerator.getFileDescriptorFieldName)
      .addModifiers(Modifier.STATIC, Modifier.FINAL)
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
    // Descriptor field for each nested type
    val staticBlock = CodeBlock.builder
    for (message, ix) <- info.messageTypes.asScala.zipWithIndex do
      addMessageDescriptor(t, staticBlock, message, ix)
    
    t.addStaticBlock(CodeBlock.builder
      .beginControlFlow("try")
      .addStatement("descriptor = $L", initBlock.build)
      .add(staticBlock.build)
      .nextControlFlow("catch ($T e)", RuntimeClasses.Exception)
      .addStatement("throw new $T(e)", RuntimeClasses.RuntimeException)
      .endControlFlow
      .build
    )

  private def addMessageDescriptor(
    t: TypeSpec.Builder, staticBlock: CodeBlock.Builder, message: RequestInfo.MessageInfo, index: Int
  ): Unit =
    val msgDesc = message.descriptor
    val descriptorBytes = message.descriptor.toByteArray
    t.addField(FieldSpec
      .builder(RuntimeClasses.MessageDescriptor, DescriptorGenerator.getDescriptorFieldName(message))
      .addModifiers(Modifier.STATIC, Modifier.FINAL)
      .build
    )
    staticBlock.addStatement(
      "$N = $N.getMessageTypes().get($L)",
      DescriptorGenerator.getDescriptorFieldName(message),
      DescriptorGenerator.getFileDescriptorFieldName,
      index
    )
    // Recursively add nested messages
//    for (nestedType <- message.nestedTypes.asScala) {
//      addMessageDescriptor(t, staticBlock, nestedType, startOffset2)
//    }
    

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
    val charsPerLine = 80 // should be a multiple of 4

    // Construct bytes from individual base64 String sections
    val initBlock = CodeBlock.builder
    initBlock
      .add("$T.getDecoder().decode(", RuntimeClasses.Base64)
      .add("$>")
    val block = Base64.getEncoder.encodeToString(descriptor)
    var line = block.substring(0, Math.min(charsPerLine, block.length))
    var blockIx = line.length
    while (blockIx < block.length) {
      line = block.substring(blockIx, Math.min(blockIx + charsPerLine, block.length))
      if blockIx > line.length then initBlock.add(" + \n$S", line)
      else initBlock.add("\n$S", line)
      blockIx += charsPerLine
    }
    initBlock.add(")$<")
    initBlock.build

