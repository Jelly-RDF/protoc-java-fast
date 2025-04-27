package eu.neverblink.protoc.java

import com.google.protobuf.compiler.PluginProtos.CodeGeneratorRequest
import eu.neverblink.protoc.java.gen.FastJavaPlugin
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.io.FileOutputStream
import scala.util.Using

class FastJavaPluginSpec extends AnyWordSpec, Matchers:
  "FastJavaPlugin" should {
    "compile" in {
      val is = getClass.getResourceAsStream("/rdf_descriptor.pb")
      val request = CodeGeneratorRequest.parseFrom(is)
      val response = FastJavaPlugin.handleRequest(request)
      val path = "test-project/src/main/java/eu/ostrzyciel/jelly/core/proto/v1/Rdf.java"
      Using.resource(FileOutputStream(path)) { fos =>
        fos.write(response.getFile(0).getContentBytes.toByteArray)
      }
    }
  }
