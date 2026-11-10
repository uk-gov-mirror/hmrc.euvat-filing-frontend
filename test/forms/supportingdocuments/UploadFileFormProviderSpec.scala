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

package forms.supportingdocuments

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class UploadFileFormProviderSpec extends AnyWordSpec with Matchers {

  "UploadFileFormProvider" should {

    "accept an optional description of up to 255 characters" in {
      val form = new UploadFileFormProvider()()

      form.bind(Map("fileuploadDescription" -> ("a" * 255))).hasErrors mustBe false
      form.bind(Map.empty[String, String]).hasErrors mustBe false
      form.bind(Map("fileuploadDescription" -> ("a" * 256))).errors.map(_.message) must contain(
        "supportingDocuments.uploadFile.error.description.maxLength"
      )
    }
  }
}
