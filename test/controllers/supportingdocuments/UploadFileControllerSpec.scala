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

package controllers.supportingdocuments

import base.SpecBase
import play.api.libs.Files.TemporaryFileCreator
import play.api.mvc.MultipartFormData
import play.api.test.FakeRequest
import play.api.test.Helpers.*

import java.nio.charset.StandardCharsets
import java.nio.file.Files

class UploadFileControllerSpec extends SpecBase {

  "UploadFileController" - {

    "must display the supporting document upload form" in {
      val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

      running(application) {
        val result = route(application, FakeRequest(GET, "/file-eu-vat/supporting-documents/upload-file")).value
        val content = contentAsString(result)

        status(result) mustEqual OK
        routes.UploadFileController.onPageLoad().url mustEqual "/file-eu-vat/supporting-documents/upload-file"
        content must include("<title>Upload file - EU VAT - GOV.UK</title>")
        content must include("<h1 class=\"govuk-label-wrapper\">")
        content must include(messages(application)("supportingDocuments.caption"))
        content must include(messages(application)("supportingDocuments.uploadFile.file.hint"))
        content must include(messages(application)("supportingDocuments.uploadFile.description.label"))
        content must include("name=\"file-upload-1\"")
        content must include("name=\"fileuploadDescription\"")
        content must include("data-module=\"govuk-file-upload\"")
        content must include("You have 255 characters remaining")
      }
    }

    "must redirect a valid upload to journey recovery until the uploading page exists" in {
      val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

      running(application) {
        val temporaryFile = application.injector.instanceOf[TemporaryFileCreator].create()
        Files.write(temporaryFile.path, "receipt contents".getBytes(StandardCharsets.UTF_8))
        val multipartData = MultipartFormData(
          dataParts = Map.empty,
          files     = Seq(MultipartFormData.FilePart("file-upload-1", "receipt.pdf", Some("application/pdf"), temporaryFile)),
          badParts  = Seq.empty
        )
        val request = FakeRequest(POST, routes.UploadFileController.onSubmit().url)
          .withMultipartFormDataBody(multipartData)
        val result = route(application, request).value

        try {
          status(result) mustEqual SEE_OTHER
          redirectLocation(result).value mustEqual controllers.routes.JourneyRecoveryController.onPageLoad().url
        } finally {
          Files.deleteIfExists(temporaryFile.path)
        }
      }
    }
  }
}
