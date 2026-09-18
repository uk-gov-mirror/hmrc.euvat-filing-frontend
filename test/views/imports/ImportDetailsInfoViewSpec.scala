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

package views.html.imports

import base.SpecBase
import forms.ImportDetailsInfoFormProvider
import models.NormalMode
import play.api.mvc.Call
import play.api.test.FakeRequest
import play.api.test.Helpers.*

class ImportDetailsInfoViewSpec extends SpecBase {

  private val formProvider = new ImportDetailsInfoFormProvider()
  private val form = formProvider()

  private val backLinkCall = Call("GET", "/back")

  private def mainRegion(html: String): String = {
    val mainStart = html.indexOf("<main")
    val mainEnd = html.indexOf("</main")
    if (mainStart >= 0 && mainEnd > mainStart) html.substring(mainStart, mainEnd) else html
  }

  private def render(application: play.api.Application, errored: Boolean): String = {
    val view = application.injector.instanceOf[views.html.imports.ImportDetailsInfoView]
    val theForm = if (errored) form.bind(Map("value" -> "")) else form
    view(theForm, NormalMode, backLinkCall)(
      FakeRequest(),
      messages(application)
    ).toString()
  }

  "ImportDetailsInfoView" - {

    "should render a single H1, the caption, the bullet list, and the character count field inside the form" in {
      val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

      running(application) {
        val mainHtml = mainRegion(render(application, errored = false))

        "(?i)<h1\\b".r.findAllMatchIn(mainHtml).length mustBe 1
        "(?i)<h2\\b".r.findAllMatchIn(mainHtml).length mustBe 0

        val formIndex = mainHtml.indexOf("<form")
        val captionIndex = mainHtml.indexOf("<span class=\"govuk-caption-l\">")
        val h1Index = mainHtml.indexOf("<h1")
        val bulletIndex = mainHtml.indexOf("govuk-list--bullet")
        val textAreaIndex = mainHtml.indexOf("textarea")
        val buttonIndex = mainHtml.indexOf("<button")

        formIndex     must be >= 0
        captionIndex  must be > formIndex
        h1Index       must be > captionIndex
        bulletIndex   must be > h1Index
        textAreaIndex must be > bulletIndex
        buttonIndex   must be > textAreaIndex

        mainHtml must not include "govuk-form-group--error"
        mainHtml must include(messages(application)("import.caption"))
        mainHtml must include(messages(application)("importDetailsInfo.heading"))
        mainHtml must include(messages(application)("importDetailsInfo.bullet1"))
        mainHtml must include(messages(application)("importDetailsInfo.bullet2"))
        mainHtml must include(messages(application)("importDetailsInfo.label"))
      }
    }

    "should render the error summary first inside the form and above the H1 when the form has errors" in {
      val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

      running(application) {
        val mainHtml = mainRegion(render(application, errored = true))

        val formIndex = mainHtml.indexOf("<form")
        val summaryIndex = mainHtml.indexOf("govuk-error-summary")
        val h1Index = mainHtml.indexOf("<h1")

        summaryIndex must be > formIndex
        summaryIndex must be < h1Index
        mainHtml     must include("govuk-form-group--error")
        mainHtml     must include(messages(application)("importDetailsInfo.error.required"))
      }
    }

    "should render the back link" in {
      val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

      running(application) {
        val html = render(application, errored = false)
        html must include("govuk-back-link")
        html must include("href=\"/back\"")
      }
    }

    "should enforce a maximum of 255 characters on the field" in {
      val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

      running(application) {
        val html = render(application, errored = false)
        html must include("maxlength=\"255\"")
      }
    }
  }
}
