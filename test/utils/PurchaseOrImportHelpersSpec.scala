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

package utils

import base.SpecBase
import forms.PurchaseOrImportSubTypeFormProvider
import pages.ImportSubCategoryPage
import play.api.i18n.{Lang, Messages, MessagesImpl}
import play.api.test.Helpers.stubMessagesApi
import queries.ImportSubCategoryLabelQuery
import utils.PurchaseOrImportHelpers.*

class PurchaseOrImportHelpersSpec extends SpecBase {

  private def messagesWith(entries: (String, String)*): Messages =
    MessagesImpl(Lang("en"), stubMessagesApi(Map("en" -> entries.toMap)))

  private val noMessages: Messages = messagesWith()
  private val config = new ConfigPurchaseOrImportMapping()
  private val formProvider = new PurchaseOrImportSubTypeFormProvider()

  private val options = Seq(("1.2.6", "sub.fuel.2.6"), ("1.2.7", "sub.fuel.2.7"))
  private val optionsWithNoneOfThese = Seq(("10.17", "sub.other.17"), (ConfigPurchaseOrImportMapping.NoneOfTheseSubCode, "sub.other.99"))

  "PurchaseOrImportHelpers" - {

    "radioItems" - {

      "must include the generic None item when options do not contain None of these" in {
        radioItems(config, options)(noMessages).flatMap(_.value) mustBe Seq("1.2.6", "1.2.7", ConfigPurchaseOrImportMapping.NoneValue)
      }

      "must drop the generic None item when options contain None of these" in {
        radioItems(config, optionsWithNoneOfThese)(noMessages).flatMap(_.value) mustBe Seq("10.17", ConfigPurchaseOrImportMapping.NoneOfTheseSubCode)
      }
    }

    "allowedValues" - {

      "must add the None value when options do not contain None of these" in {
        allowedValues(options) mustBe Seq("1.2.6", "1.2.7", ConfigPurchaseOrImportMapping.NoneValue)
      }

      "must not add the None value when options contain None of these" in {
        allowedValues(optionsWithNoneOfThese) mustBe Seq("10.17", ConfigPurchaseOrImportMapping.NoneOfTheseSubCode)
      }
    }

    "requiredErrorKey" - {

      "must use the code-specific key when it exists" in {
        val msgs = messagesWith("sub.fuel.2.error.required" -> "specific", "sub.fuel.error.required" -> "parent")
        requiredErrorKey("fuel", Some("1.2"))(msgs) mustBe "sub.fuel.2.error.required"
      }

      "must fall back to the parent key when the code-specific key does not exist" in {
        val msgs = messagesWith("sub.fuel.error.required" -> "parent")
        requiredErrorKey("fuel", Some("1.2"))(msgs) mustBe "sub.fuel.error.required"
      }

      "must use the parent key when no code is given" in {
        val msgs = messagesWith("sub.fuel.error.required" -> "parent")
        requiredErrorKey("fuel")(msgs) mustBe "sub.fuel.error.required"
      }

      "must fall back to error.required when no keys exist" in {
        requiredErrorKey("fuel", Some("1.2"))(noMessages) mustBe "error.required"
      }
    }

    "subCategoryTitle" - {

      "must use the last-segment title when it exists" in {
        val msgs = messagesWith("sub.fuel.2.title" -> "Last segment title", "sub.fuel.heading" -> "Heading")
        subCategoryTitle("fuel", "1.2", options)(msgs) mustBe "Last segment title"
      }

      "must use the full-code title when the last-segment title does not exist" in {
        val msgs = messagesWith("sub.fuel.1.2.title" -> "Full code title", "sub.fuel.heading" -> "Heading")
        subCategoryTitle("fuel", "1.2", options)(msgs) mustBe "Full code title"
      }

      "must use a child label title when no code titles exist" in {
        val msgs = messagesWith("sub.fuel.2.7.title" -> "Child title", "sub.fuel.heading" -> "Heading")
        subCategoryTitle("fuel", "1.2", options)(msgs) mustBe "Child title"
      }

      "must fall back to the parent heading when no titles exist" in {
        val msgs = messagesWith("sub.fuel.heading" -> "Heading")
        subCategoryTitle("fuel", "1.2", options)(msgs) mustBe "Heading"
      }
    }

    "preparedForm" - {

      "must return an empty form when there is no existing answer" in {
        preparedForm(formProvider, "error.required", None).value mustBe None
      }

      "must return a filled form when there is an existing answer" in {
        preparedForm(formProvider, "error.required", Some("1.2.6")).value mustBe Some("1.2.6")
      }
    }

    "labelFor" - {

      "must return the message for a known code" in {
        val msgs = messagesWith("sub.fuel.2.6" -> "Diesel")
        labelFor("1.2.6", options)(msgs) mustBe "Diesel"
      }

      "must return the value itself for an unknown code" in {
        labelFor(ConfigPurchaseOrImportMapping.NoneValue, options)(noMessages) mustBe ConfigPurchaseOrImportMapping.NoneValue
      }
    }

    "setSelection" - {

      "must set both the value and the label" in {
        val result = setSelection(emptyUserAnswers, ImportSubCategoryPage, ImportSubCategoryLabelQuery, "1.2.6", "Diesel").success.value

        result.get(ImportSubCategoryPage) mustBe Some("1.2.6")
        result.get(ImportSubCategoryLabelQuery) mustBe Some("Diesel")
      }
    }
  }
}
