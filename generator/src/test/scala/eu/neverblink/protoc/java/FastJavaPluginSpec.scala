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
      val options = request.getParameter
      val newRequest = request.toBuilder.setParameter(options +
        ",implements_RdfTriple=eu.neverblink.jelly.core.internal.proto.SpoBase" +
        ",implements_RdfTriple.Mutable=eu.neverblink.jelly.core.internal.proto.SpoBase.Setters" +
        ",implements_RdfQuad=eu.neverblink.jelly.core.internal.proto.SpoBase;" +
        "eu.neverblink.jelly.core.internal.proto.GraphBase" +
        ",implements_RdfQuad.Mutable=eu.neverblink.jelly.core.internal.proto.SpoBase.Setters;" +
        "eu.neverblink.jelly.core.internal.proto.GraphBase.Setters" +
        ",implements_RdfGraphStart=eu.neverblink.jelly.core.internal.proto.GraphBase" +
        ",implements_RdfGraphStart.Mutable=eu.neverblink.jelly.core.internal.proto.GraphBase.Setters" +
        ",implements_RdfNamespaceDeclaration=eu.neverblink.jelly.core.internal.proto.NsBase" +
        ",implements_RdfNamespaceDeclaration.Mutable=eu.neverblink.jelly.core.internal.proto.NsBase.Setters" +
        ",replace_package=eu.ostrzyciel=eu.neverblink"
      ).build()
      val response = FastJavaPlugin.handleRequest(newRequest)
      val basePath = "test-project/src/main/java/"
      // Delete all files in the directory
      val dir = new java.io.File(basePath + "eu/neverblink/jelly/core/proto/v1/")
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
