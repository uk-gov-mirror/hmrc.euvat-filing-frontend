/*
 * Copyright 2026 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package views

import base.SpecBase
import forms.PurchaseOrImportSubTypeFormProvider
import play.api.mvc.Call
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.radios.RadioItem

class PurchaseOrImportSubTypeViewSpec extends SpecBase {

  private val formProvider = new PurchaseOrImportSubTypeFormProvider()
  private val form = formProvider("importSubCode.fuel.error.required")

  private val items = Seq(
    RadioItem(content = Text("Fuel for passenger transport"), value = Some("1.3"), id      = Some("value_0")),
    RadioItem(content = Text("None"), value                         = Some("__none__"), id = Some("value_1"))
  )

  private def render(application: play.api.Application, captionKey: String, errored: Boolean): String = {
    val view = application.injector.instanceOf[views.html.PurchaseOrImportSubTypeView]
    val theForm = if (errored) form.bind(Map.empty[String, String]) else form
    view(theForm, items, "What is the fuel used for?", "What is the fuel used for?", captionKey, Call("POST", "/submit"), "/back")(
      FakeRequest(),
      messages(application)
    ).toString()
  }

  private def mainRegion(html: String): String = {
    val mainStart = html.indexOf("<main")
    val mainEnd = html.indexOf("</main>")
    if (mainStart >= 0 && mainEnd > mainStart) html.substring(mainStart, mainEnd) else html
  }

  "PurchaseOrImportSubTypeView" - {

    Seq("purchase.caption", "import.caption").foreach { captionKey =>
      s"with caption $captionKey" - {

        "should render a single H1, no H2, and the caption and radios inside the form" in {
          val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

          running(application) {
            val mainHtml = mainRegion(render(application, captionKey, errored = false))

            "(?i)<h1\\b".r.findAllMatchIn(mainHtml).length mustBe 1
            "(?i)<h2\\b".r.findAllMatchIn(mainHtml).length mustBe 0

            val formIndex = mainHtml.indexOf("<form")
            val groupIndex = mainHtml.indexOf("govuk-form-group")
            val captionIndex = mainHtml.indexOf("<span class=\"govuk-caption-l\">")
            val radiosIndex = mainHtml.indexOf("govuk-radios")
            val buttonIndex = mainHtml.indexOf("govuk-button")

            formIndex    must be >= 0
            groupIndex   must be > formIndex
            captionIndex must be > groupIndex
            radiosIndex  must be > captionIndex
            buttonIndex  must be > radiosIndex

            mainHtml must include("govuk-fieldset__legend--l")
            mainHtml must not include "govuk-fieldset__legend--xl"
            mainHtml must not include "govuk-form-group--error"
            mainHtml must include(messages(application)(captionKey))
            mainHtml must include("value_0")
          }
        }

        "should render the error summary first inside the form and above the H1 when the form has errors" in {
          val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

          running(application) {
            val mainHtml = mainRegion(render(application, captionKey, errored = true))

            val formIndex = mainHtml.indexOf("<form")
            val summaryIndex = mainHtml.indexOf("govuk-error-summary")
            val h1Index = mainHtml.indexOf("<h1")

            summaryIndex must be > formIndex
            summaryIndex must be < h1Index
            mainHtml     must include("href=\"#value_0\"")
            mainHtml     must include("govuk-form-group--error")
          }
        }
      }
    }

    "should render the back link" in {
      val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

      running(application) {
        val html = render(application, "purchase.caption", errored = false)
        html must include("govuk-back-link")
        html must include("href=\"/back\"")
      }
    }

    "should use the caption of the journey it is rendered for" in {
      val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

      running(application) {
        val purchaseHtml = mainRegion(render(application, "purchase.caption", errored = false))
        val importHtml = mainRegion(render(application, "import.caption", errored = false))
        val purchaseCaption = messages(application)("purchase.caption")
        val importCaption = messages(application)("import.caption")

        purchaseHtml must include(purchaseCaption)
        purchaseHtml must not include importCaption
        importHtml   must include(importCaption)
        importHtml   must not include purchaseCaption
      }
    }
  }
}
