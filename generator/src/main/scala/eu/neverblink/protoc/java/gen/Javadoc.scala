package eu.neverblink.protoc.java.gen

import com.google.protobuf.DescriptorProtos
import com.google.protobuf.DescriptorProtos.SourceCodeInfo
import com.palantir.javapoet.CodeBlock

import java.util
import java.util.Locale

/**
 * Utilities for creating Javadoc comments on methods and fields.
 * For the most part similar to Protobuf-Java.
 *
 * @author Florian Enner
 * @since 16 Jun 2023
 */
object Javadoc:
  private val cachedFields = new java.util.HashMap[DescriptorProtos.FieldDescriptorProto, CodeBlock]

  def inherit: String =
    // Note: JavaDoc automatically adds the superclass doc,
    // so we don't need to manually call it out.
    //        return "{@inheritDoc}";
    ""

  def forMessage(info: RequestInfo.MessageInfo): CodeBlock = forType("type", info)

  def forMessageField(info: RequestInfo.FieldInfo): CodeBlock.Builder =
    // Fields get called a lot, so we cache the expensive parts
    var block = cachedFields.get(info.descriptor)
    if (block == null) {
      block = withComments(info.sourceLocation)
        .add("<code>$L</code>", getFieldDefinitionLine(info.descriptor)).build
      cachedFields.put(info.descriptor, block)
    }
    block.toBuilder

  def forOneOfField(info: RequestInfo.OneOfInfo): CodeBlock.Builder =
    CodeBlock.builder
      .add("<code>oneof $L { ... }</code>", info.descriptor.getName)

  def forEnum(info: RequestInfo.EnumInfo): CodeBlock = forType("enum", info)

  def forEnumValue(info: RequestInfo.EnumValueInfo): CodeBlock =
    withComments(info.sourceLocation)
      .add("<code>$L = $L;</code>", info.getName, info.getNumber)
      .build

  def withComments(location: SourceCodeInfo.Location): CodeBlock.Builder =
    // Protobuf-java seems to prefer leading comments and only use trailing as a fallback.
    // They also remove the first space as well as empty lines, but that'
    val builder = CodeBlock.builder
    val format = "<pre>\n$L</pre>\n\n"
    if (location.hasLeadingComments)
      builder.add(format, escapeCommentClose(location.getLeadingComments))
    else if (location.hasTrailingComments)
      builder.add(format, escapeCommentClose(location.getTrailingComments))
    builder

  private def forType(name: String, info: RequestInfo.TypeInfo) =
    withComments(info.sourceLocation)
      .add("Protobuf $L {@code $T}", name, info.typeName)
      .add("\nDO NOT INHERIT FROM THIS CLASS!\n" +
        "It's not <code>final</code> only to facilitate the Mutable nested subclass."
      )
      .build

  private def getFieldDefinitionLine(descriptor: DescriptorProtos.FieldDescriptorProto) = {
    // optional int32 my_field = 2 [default = 1];
    val label = descriptor.getLabel.toString.substring("LABEL_".length).toLowerCase(Locale.US)
    var t = descriptor.getTypeName
    if (t.isEmpty) t = descriptor.getType.toString.substring("TYPE_".length).toLowerCase(Locale.US)
    val definition = String.format("%s %s %s = %d", label, t, descriptor.getName, descriptor.getNumber)
    var options = ""
    if (descriptor.hasDefaultValue) {
      val defaultValue = escapeCommentClose(descriptor.getDefaultValue)
      options = " [default = " + defaultValue + "]"
    }
    else if (descriptor.getOptions.hasPacked) options = " [packed = " + descriptor.getOptions.getPacked + "]"
    val line = definition + options + ";"
    if (!descriptor.hasExtendee) line
    else "extend {\n  " + line + "\n}"
  }

  private def escapeCommentClose(string: String) =
    if (string.contains("*/")) string.replaceAll("\\*/", "*\\\\/")
    else string
