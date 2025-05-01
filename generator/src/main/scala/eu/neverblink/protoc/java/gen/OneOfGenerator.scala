package eu.neverblink.protoc.java.gen

import com.palantir.javapoet.{FieldSpec, MethodSpec, TypeName, TypeSpec}
import eu.neverblink.protoc.java.gen.RequestInfo.OneOfInfo

import javax.lang.model.element.Modifier

/**
 * @author Florian Enner
 * @author Piotr Sowiński
 * @since 19 Nov 2019
 */
class OneOfGenerator(val info: OneOfInfo):
  val fields = info.getFields

  def generateMemberFields(t: TypeSpec.Builder): Unit =
    val field = FieldSpec.builder(RuntimeClasses.ObjectType, info.fieldName)
      .addJavadoc(Javadoc.forOneOfField(info).build)
      .addModifiers(Modifier.PRIVATE)
    field.initializer("null")
    t.addField(field.build)
    val numberField = FieldSpec.builder(TypeName.BYTE, info.numberFieldName)
      .addModifiers(Modifier.PRIVATE)
      .initializer("$L", 0)
    t.addField(numberField.build)

  def generateMemberMethods(t: TypeSpec.Builder): Unit =
    // Checks if any has state is true
    val has = MethodSpec.methodBuilder(info.hazzerName)
      .addModifiers(Modifier.PUBLIC)
      .returns(classOf[Boolean])
      .addStatement("return $N != 0", info.numberFieldName)
    t.addMethod(has.build)
    // Set the value -- general method
    val set = MethodSpec.methodBuilder(info.setterName)
      .addJavadoc("Low-level setter for the <code>$L</code> oneof field.\n" +
        "Use with care, as it will not check the type of the value.",
        info.descriptor.getName
      )
      .addModifiers(Modifier.PUBLIC)
      .addParameter(RuntimeClasses.ObjectType, info.fieldName)
      .addParameter(TypeName.BYTE, "number")
      .addStatement("this.$N = $N", info.fieldName, info.fieldName)
      .addStatement("this.$N = $L", info.numberFieldName, "number")
    t.addMethod(set.build)
    // Set the value -- specific methods
    for field <- fields do
      val setField = MethodSpec.methodBuilder(field.setterName)
        .addJavadoc("Sets the <code>$L</code> oneof field to $L.",
          info.descriptor.getName, field.fieldName
        )
        .addModifiers(Modifier.PUBLIC)
        .addParameter(field.getTypeName, field.fieldName)
        .addStatement("this.$N = $N", info.fieldName, field.fieldName)
        .addStatement("this.$N = $L", info.numberFieldName, field.descriptor.getNumber)
      t.addMethod(setField.build)
    // Get the value -- general method
    val get = MethodSpec.methodBuilder(info.getterName)
      .addJavadoc("Returns the <code>$L</code> oneof field.",
        info.descriptor.getName
      )
      .addModifiers(Modifier.PUBLIC)
      .returns(RuntimeClasses.ObjectType)
      .addStatement("return $N", info.fieldName)
    t.addMethod(get.build)
    // Get the value -- field number method
    val getNumber = MethodSpec.methodBuilder(info.getNumberName)
      .addJavadoc("Returns the set field number of the <code>$L</code> oneof field.",
        info.descriptor.getName
      )
      .addModifiers(Modifier.PUBLIC)
      .returns(TypeName.BYTE)
      .addStatement("return $N", info.numberFieldName)
    t.addMethod(getNumber.build)
    // Get the value -- specific methods
    for field <- fields do
      val getField = MethodSpec.methodBuilder(field.getterName)
        .addJavadoc("Returns the <code>$L</code> oneof field.\n" +
          "Use with care, as it will not check if the correct field numeber is actually set.",
          info.descriptor.getName
        )
        .addModifiers(Modifier.PUBLIC)
        .returns(field.getTypeName)
        .addStatement("return ($T) $N", field.getTypeName, info.fieldName)
      t.addMethod(getField.build)


  def generateCopyFromCode(method: MethodSpec.Builder): Unit =
    method.addStatement("this.$N = other.$N", info.fieldName, info.fieldName)
    method.addStatement("this.$N = other.$N", info.numberFieldName, info.numberFieldName)

  def generateMergeFromMessageCode(method: MethodSpec.Builder): Unit =
    // Not an actual merge, we just set the value
    generateCopyFromCode(method)

  def generateEqualsStatement(method: MethodSpec.Builder): Unit =
    method.addCode(
      "$N == other.$N && ($N == 0 || $N.equals(other.$N))",
      info.numberFieldName, info.numberFieldName, info.numberFieldName, info.fieldName, info.fieldName
    )

  def generateClearCode(method: MethodSpec.Builder): Unit =
    method.addStatement("this.$N = null", info.fieldName)
    method.addStatement("this.$N = 0", info.numberFieldName)
