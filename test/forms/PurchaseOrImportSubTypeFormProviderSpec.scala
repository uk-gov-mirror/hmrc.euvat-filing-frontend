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

package forms

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.data.{Form, FormError}

class PurchaseOrImportSubTypeFormProviderSpec extends AnyWordSpec with Matchers {

  val formProvider = new PurchaseOrImportSubTypeFormProvider()

  "PurchaseOrImportSubTypeFormProvider" should {

    "create a form with default error key" in {
      val form = formProvider()
      form.mapping.key should be("value")
    }

    "create a form with custom error key" in {
      val customKey = "custom.error.required"
      val form = formProvider(customKey)
      form.mapping.key should be("value")
    }

    "bind successfully with a valid value" in {
      val form = formProvider()
      val boundForm = form.bind(Map("value" -> "1.1"))

      boundForm.hasErrors should be(false)
      boundForm.get       should be("1.1")
    }

    "bind successfully with a value containing spaces trimmed" in {
      val form = formProvider()
      val boundForm = form.bind(Map("value" -> "  1.2  "))

      boundForm.hasErrors should be(false)
      boundForm.get       should be("  1.2  ")
    }

    "fail to bind with an empty string" in {
      val form = formProvider("test.error.required")
      val boundForm = form.bind(Map("value" -> ""))

      boundForm.hasErrors should be(true)
      boundForm.errors    should contain(FormError("value", List("test.error.required")))
    }

    "fail to bind with whitespace only" in {
      val form = formProvider("test.error.required")
      val boundForm = form.bind(Map("value" -> "   "))

      boundForm.hasErrors should be(true)
      boundForm.errors    should contain(FormError("value", List("test.error.required")))
    }

    "fail to bind when value is missing" in {
      val form = formProvider("test.error.required")
      val boundForm = form.bind(Map())

      boundForm.hasErrors should be(true)
      boundForm.errors    should contain(FormError("value", List("test.error.required")))
    }

    "fail to bind with default error key when not provided" in {
      val form = formProvider()
      val boundForm = form.bind(Map())

      boundForm.hasErrors should be(true)
      boundForm.errors    should contain(FormError("value", List("error.required")))
    }

    "fill a form with an existing value" in {
      val form = formProvider().fill("3.5")

      form.value should be(Some("3.5"))
    }

    "transform between optional and string correctly" in {
      val form = formProvider()

      val boundForm = form.bind(Map("value" -> "10.17"))
      boundForm.get should be("10.17")

      val filledForm = form.fill("10.17")
      filledForm.value should be(Some("10.17"))
    }
  }
}
