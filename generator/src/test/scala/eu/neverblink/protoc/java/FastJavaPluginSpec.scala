package eu.neverblink.protoc.java

import com.google.protobuf.compiler.PluginProtos.CodeGeneratorRequest
import eu.neverblink.protoc.java.gen.FastJavaPlugin
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.io.FileOutputStream
import scala.jdk.CollectionConverters.*
import scala.util.Using

class FastJavaPluginSpec extends AnyWordSpec, Matchers:
  "FastJavaPlugin" should {
    "compile" in {
      val is = getClass.getResourceAsStream("/rdf_descriptor.pb")
      val request = CodeGeneratorRequest.parseFrom(is)
      val response = FastJavaPlugin.handleRequest(request)
      val basePath = "test-project/src/main/java/"
      // Delete all files in the directory
      val dir = new java.io.File(basePath + "eu/ostrzyciel/jelly/core/proto/v1/")
      if (dir.exists && dir.isDirectory) {
        dir.listFiles().foreach(_.delete())
      }
      // Save the generated files
      for (file <- response.getFileList.asScala) {
        val fileName = basePath + file.getName
        Using.resource(FileOutputStream(fileName)) { fos =>
          fos.write(file.getContentBytes.toByteArray)
        }
      }
    }
  }
