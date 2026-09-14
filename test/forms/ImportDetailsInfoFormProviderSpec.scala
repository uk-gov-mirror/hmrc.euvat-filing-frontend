package forms

import forms.behaviours.StringFieldBehaviours
import play.api.data.FormError

class ImportDetailsInfoFormProviderSpec extends StringFieldBehaviours {

  val requiredKey = "importDetailsInfo.error.required"
  val lengthKey = "importDetailsInfo.error.length"
  val maxLength = 255

  val form = new ImportDetailsInfoFormProvider()()

  ".value" - {

    val fieldName = "value"

    behave like fieldThatBindsValidData(
      form,
      fieldName,
      stringsWithMaxLength(maxLength)
    )

    behave like fieldWithMaxLength(
      form,
      fieldName,
      maxLength = maxLength,
      lengthError = FormError(fieldName, lengthKey, Seq(maxLength))
    )

    behave like mandatoryField(
      form,
      fieldName,
      requiredError = FormError(fieldName, requiredKey)
    )
  }
}
