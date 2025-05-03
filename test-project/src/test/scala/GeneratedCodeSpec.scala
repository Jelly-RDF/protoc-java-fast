import com.google.protobuf.{DescriptorProtos, Descriptors}
import eu.neverblink.jelly.core.proto.v1.Rdf
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GeneratedCodeSpec extends AnyWordSpec, Matchers:
  "generated code" should {
    "run" in {
      // Rdf.getDescriptor
      val descriptor = Rdf.getDescriptor
    }
  }
