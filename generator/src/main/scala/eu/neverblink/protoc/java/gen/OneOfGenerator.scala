package eu.neverblink.protoc.java.gen

import com.palantir.javapoet.{MethodSpec, TypeSpec}
import eu.neverblink.protoc.java.gen.RequestInfo.{FieldInfo, OneOfInfo}

import java.util
import java.util.stream.Collectors
import javax.lang.model.element.Modifier
import scala.jdk.CollectionConverters.*

/**
 * @author Florian Enner
 * @since 19 Nov 2019
 */
class OneOfGenerator(val info: OneOfInfo):
  val fields = info.getFields

  def generateMemberMethods(t: TypeSpec.Builder): Unit =
    // Checks if any has state is true
    val has = MethodSpec.methodBuilder(info.hazzerName)
      .addModifiers(Modifier.PUBLIC)
      .returns(classOf[Boolean])
      // TODO: implement
      .addStatement("return true")
      // .addStatement("return $L", BitField.hasAnyBit(fields))
    // Method that clears all fields
    val clear = MethodSpec.methodBuilder(info.clearName)
      .addModifiers(Modifier.PUBLIC)
      .returns(info.parentType)
      .beginControlFlow("if ($L())", info.hazzerName)
    for (field <- fields.asScala) {
      clear.addStatement("$N()", field.clearName)
    }
    clear.endControlFlow
    clear.addStatement("return this")
    t.addMethod(has.build)
    t.addMethod(clear.build)
    // Add a utility method that clears all but one fields
    if (fields.size > 1) {
      for (field <- fields.asScala) {
        val otherFields = fields.stream
          .filter(info => info ne field)
          .collect(Collectors.toList)
        val clearOthers = MethodSpec
          .methodBuilder(field.getClearOtherOneOfName)
          .addModifiers(Modifier.PRIVATE)
          // TODO: implement
          // .beginControlFlow("if ($L)", BitField.hasAnyBit(otherFields))
        for (otherField <- otherFields.asScala) {
          clearOthers.addStatement("$N()", otherField.clearName)
        }
        // TODO: implement
        // clearOthers.endControlFlow
        t.addMethod(clearOthers.build)
      }
    }
