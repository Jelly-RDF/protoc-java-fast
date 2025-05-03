package eu.neverblink.protoc.java.gen

import com.google.protobuf.compiler.PluginProtos
import eu.neverblink.protoc.java.gen.PluginOptions.parseImplements

import java.lang.Boolean.*
import java.util
import java.util.regex.Pattern
import scala.jdk.CollectionConverters.*

object PluginOptions:
  enum FieldSerializationOrder:
    case Quickbuf, AscendingNumber, None

  object FieldSerializationOrder:
    // parsing messages from unknown sources
    def parseInputOrder(string: String): FieldSerializationOrder = string.toLowerCase match
      case "quickbuf" => Quickbuf
      case "number" => AscendingNumber
      case "random" => None
      case "none" => None
      case _ =>
        throw new Exception("'input_order' parameter accepts ['quickbuf', 'number', 'random']. Found: " + string)

    def parseOutputOrder(string: String): FieldSerializationOrder = string.toLowerCase match
      case "quickbuf" => Quickbuf
      case "number" => AscendingNumber
      case _ =>
        throw new Exception("'output_order' parameter accepts ['quickbuf', 'number']. Found: " + string)

  enum AllocationStrategy:
    case Lazy, LazyMessage, Eager

  object AllocationStrategy:
    def parseFromString(string: String): AllocationStrategy = string.toLowerCase match
      case "lazy" => Lazy
      case "lazymsg" => LazyMessage
      case "eager" => Eager
      case _ =>
        throw new Exception("'allocation' parameter accepts ['lazy', 'lazymsg', 'eager']. Found: " + string)


  enum ExtensionSupport:
    case Disabled, Embedded

  object ExtensionSupport:
    def parseFromString(string: String): ExtensionSupport = string.toLowerCase match
      case "embedded" => Embedded
      case "disabled" => Disabled
      case _ =>
        throw new Exception("'extensions' parameter accepts ['disabled', 'embedded']. Found: " + string)

  private def parseIndentString(indent: String): String = indent match
      case "8" => "        "
      case "4" => "    "
      case "2" => "  "
      case "tab" => "\t"
      case _ => throw new Exception("Expected 2,4,8,tab. Found: " + indent)

  private def parseImplements(map: util.Map[String, String]): Map[String, Seq[String]] =
    map.asScala
      .filter((k, _) => k.startsWith("implements_"))
      .map((k, v) => (k.substring(11), v.split(";").toSeq))
      .toMap

class PluginOptions(request: PluginProtos.CodeGeneratorRequest):
  val map = ParserUtil.getGeneratorParameters(request)
  val indentString = PluginOptions.parseIndentString(map.getOrDefault("indent", "2"))
  val replacePackageFunction = parseReplacePackage(map.get("replace_package"))
  val expectedInputOrder = PluginOptions.FieldSerializationOrder.parseInputOrder(map.getOrDefault("input_order", "quickbuf"))
  val outputOrder = PluginOptions.FieldSerializationOrder.parseInputOrder(map.getOrDefault("output_order", "quickbuf"))
  val allocationStrategy = PluginOptions.AllocationStrategy.parseFromString(map.getOrDefault("allocation", "lazy"))
  val extensionSupport = PluginOptions.ExtensionSupport.parseFromString(map.getOrDefault("extensions", "disabled"))
  val generateDescriptors = parseBoolean(map.getOrDefault("gen_descriptors", "true"))
  val implements = parseImplements(map)

  def parseReplacePackage(replaceOption: String): String => String =
    // leave as is
    if (replaceOption == null) return (str) => str
    // parse "pattern=replacement"
    val parts = replaceOption.split("=")
    if (parts.length != 2) throw new Exception("'replace_package' expects 'pattern=replacement'. Found: '" + replaceOption + "'")
    // regex replace
    val pattern = Pattern.compile(parts(0))
    val replacement = parts(1)
    input => pattern.matcher(input).replaceAll(replacement)

