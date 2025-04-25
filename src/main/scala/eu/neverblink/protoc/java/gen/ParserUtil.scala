package eu.neverblink.protoc.java.gen

import com.google.protobuf.compiler.PluginProtos.CodeGeneratorRequest
import com.google.protobuf.compiler.PluginProtos.CodeGeneratorResponse

import java.io.ByteArrayOutputStream
import java.io.PrintStream
import java.io.UnsupportedEncodingException
import java.nio.charset.StandardCharsets
import java.util.Collections
import java.util

/**
 * @author Florian Enner
 * @since 06 Aug 2019
 */
object ParserUtil {
  def getGeneratorParameters(request: CodeGeneratorRequest): util.Map[String, String] =
    if (!request.hasParameter) return Collections.emptyMap
    parseGeneratorParameters(request.getParameter)

  /**
   * Returns a map of input arguments added before the proto path, e.g.,
   * <p>
   * PROTOC INPUT: "--GEN_out=option1=value1,option2=value2,optionFlag3:./my-output-directory"
   * PARAMETER STRING: "option1=value1,option2=value2,optionFlag3"
   *
   * @param parameter parameter string input into protoc
   * @return map
   */
  def parseGeneratorParameters(parameter: String): util.Map[String, String] =
    if (parameter == null || parameter.isEmpty) return Collections.emptyMap
    val map = new util.HashMap[String, String]
    val parts = parameter.split(",")
    for (part <- parts) {
      val equalsIndex = part.indexOf("=")
      if (equalsIndex == -1) map.put(part, "")
      else {
        val key = part.substring(0, equalsIndex)
        val value = part.substring(equalsIndex + 1)
        map.put(key, value)
      }
    }
    map

  def asErrorWithStackTrace(e: Exception): CodeGeneratorResponse =
    // Print error with StackTrace
    val baos = new ByteArrayOutputStream
    val ps = new PrintStream(baos, true, "UTF-8")
    try e.printStackTrace(ps)
    catch {
      case ex: UnsupportedEncodingException =>
        throw new AssertionError("UTF-8 encoding not supported")
    } finally if (ps != null) ps.close()
    val errorWithStackTrace = new String(baos.toByteArray, StandardCharsets.UTF_8)
    CodeGeneratorResponse.newBuilder.setError(errorWithStackTrace).build

  def asError(errorMessage: String): CodeGeneratorResponse = 
    CodeGeneratorResponse.newBuilder.setError(errorMessage).build
}
