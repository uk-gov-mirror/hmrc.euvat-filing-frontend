package forms

import javax.inject.Inject

import forms.mappings.Mappings
import play.api.data.Form

class ImportDetailsInfoFormProvider @Inject() extends Mappings {

  def apply(): Form[String] =
    Form(
      "value" -> text("importDetailsInfo.error.required")
        .verifying(maxLength(255, "importDetailsInfo.error.length"))
    )
}
