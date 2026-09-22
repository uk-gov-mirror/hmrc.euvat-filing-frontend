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

import forms.PurchaseOrImportSubTypeFormProvider
import models.UserAnswers
import play.api.data.Form
import play.api.i18n.Messages
import queries.Settable
import uk.gov.hmrc.govukfrontend.views.viewmodels.radios.RadioItem

import scala.util.Try

object PurchaseOrImportHelpers {

  private def lastSegment(code: String): String =
    code.split("\\.").lastOption.getOrElse(code)

  def radioItems(config: ConfigPurchaseOrImportMapping, options: Seq[(String, String)])(implicit messages: Messages): Seq[RadioItem] = {
    val items = config.buildRadioItems(options, messages)
    if (options.exists(_._1 == ConfigPurchaseOrImportMapping.NoneOfTheseSubCode))
      items.filterNot(_.value.contains(ConfigPurchaseOrImportMapping.NoneValue))
    else items
  }

  def allowedValues(options: Seq[(String, String)]): Seq[String] = {
    val codes = options.map(_._1)
    if (codes.contains(ConfigPurchaseOrImportMapping.NoneOfTheseSubCode)) codes else codes :+ ConfigPurchaseOrImportMapping.NoneValue
  }

  def requiredErrorKey(parentKey: String, parentCode: Option[String] = None)(implicit messages: Messages): String = {
    val candidates =
      parentCode.map(code => s"sub.$parentKey.${lastSegment(code)}.error.required").toSeq :+ s"sub.$parentKey.error.required"
    candidates.find(messages.isDefinedAt).getOrElse("error.required")
  }

  def subCategoryTitle(parentKey: String, parentCode: String, options: Seq[(String, String)])(implicit messages: Messages): String = {
    val candidates =
      Seq(s"sub.$parentKey.${lastSegment(parentCode)}.title", s"sub.$parentKey.$parentCode.title") ++
        options.map { case (_, labelKey) => s"$labelKey.title" }
    candidates.find(messages.isDefinedAt).map(messages(_)).getOrElse(messages(s"sub.$parentKey.heading"))
  }

  def preparedForm(formProvider: PurchaseOrImportSubTypeFormProvider, requiredKey: String, existing: Option[String]): Form[String] = {
    val form = formProvider(requiredKey)
    existing.fold(form)(form.fill)
  }

  def labelFor(value: String, options: Seq[(String, String)])(implicit messages: Messages): String =
    options.collectFirst { case (code, labelKey) if code == value => messages(labelKey) }.getOrElse(value)

  def setSelection(
    answers: UserAnswers,
    valuePage: Settable[String],
    labelQuery: Settable[String],
    value: String,
    label: String
  ): Try[UserAnswers] =
    answers.set(valuePage, value).flatMap(_.set(labelQuery, label))
}
