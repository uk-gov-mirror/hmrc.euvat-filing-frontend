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
import play.api.libs.Files.{TemporaryFile, TemporaryFileCreator}
import play.api.mvc.MultipartFormData
import play.api.test.FakeRequest
import play.api.test.Helpers.*

import java.nio.charset.StandardCharsets
import java.nio.file.Files

class UploadFileControllerSpec extends SpecBase {

  private val formTokenKey = "uploadFile.formToken"
  private val formTokenField = "uploadFileFormToken"

  private def formTokenFrom(content: String): String =
    """name="uploadFileFormToken" value="([^"]+)"""".r.findFirstMatchIn(content).map(_.group(1)).get

  "UploadFileController" - {

    "must display the supporting document upload form" in {
      val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

      running(application) {
        val result = route(application, FakeRequest(GET, "/file-eu-vat/supporting-documents/upload-file")).value
        val content = contentAsString(result)

        status(result) mustEqual OK
        header("Cache-Control", result).value mustEqual "no-store"
        routes.UploadFileController.onPageLoad().url mustEqual "/file-eu-vat/supporting-documents/upload-file"
        content must include("<title>Upload file - EU VAT - GOV.UK</title>")
        content must include("<h1 class=\"govuk-label-wrapper\">")
        content must include(s"""<span class="govuk-caption-l">${messages(application)("supportingDocuments.caption")}</span>""")
        content must include(messages(application)("supportingDocuments.uploadFile.file.hint"))
        content must include(messages(application)("supportingDocuments.uploadFile.description.label"))
        content must include("name=\"file-upload-1\"")
        content must include("name=\"fileuploadDescription\"")
        content must include(s"""name="$formTokenField"""")
        content must include("data-module=\"govuk-file-upload\"")
        content must include("""href="#" class="govuk-back-link" data-module="hmrc-back-link">Back</a>""")
        content must include("You have 255 characters remaining")
      }
    }

    "must redirect a valid upload to journey recovery until the uploading page exists" in {
      val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

      running(application) {
        val temporaryFile = application.injector.instanceOf[TemporaryFileCreator].create()
        Files.write(temporaryFile.path, "receipt contents".getBytes(StandardCharsets.UTF_8))
        val multipartData: MultipartFormData[TemporaryFile] = MultipartFormData(
          dataParts = Map(formTokenField -> Seq("test-token")),
          files     = Seq(MultipartFormData.FilePart("file-upload-1", "receipt.pdf", Some("application/pdf"), temporaryFile)),
          badParts  = Seq.empty
        )
        val request = FakeRequest(POST, routes.UploadFileController.onSubmit().url)
          .withSession(formTokenKey -> "test-token")
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

    "must show an error when the uploaded file is empty" in {
      val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

      running(application) {
        val temporaryFile = application.injector.instanceOf[TemporaryFileCreator].create()
        val multipartData = MultipartFormData(
          dataParts = Map(formTokenField -> Seq("test-token")),
          files     = Seq(MultipartFormData.FilePart("file-upload-1", "empty.pdf", Some("application/pdf"), temporaryFile)),
          badParts  = Seq.empty
        )
        val request = FakeRequest(POST, routes.UploadFileController.onSubmit().url)
          .withSession(formTokenKey -> "test-token")
          .withMultipartFormDataBody(multipartData)
        val result = route(application, request).value

        try {
          status(result) mustEqual SEE_OTHER
          redirectLocation(result).value mustEqual routes.UploadFileController.onPageLoad().url
          flash(result).get("uploadFile.fileError") mustBe Some("supportingDocuments.uploadFile.error.empty")

          val errorPageRequest = FakeRequest(GET, routes.UploadFileController.onPageLoad().url)
            .withFlash("uploadFile.fileError" -> "supportingDocuments.uploadFile.error.empty")
          val errorPage = route(application, errorPageRequest).value

          status(errorPage) mustEqual OK
          val errorContent = contentAsString(errorPage)
          errorContent must include(messages(application)("supportingDocuments.uploadFile.error.empty"))
          errorContent must include("""id="file-upload-1-error"""")
          errorContent must include("""aria-describedby="file-upload-1-error file-upload-1-hint"""")
          val errorGroupStart = errorContent.indexOf("""class="govuk-form-group govuk-form-group--error"""")
          val captionPosition = errorContent.indexOf(
            s"""<span class="govuk-caption-l">${messages(application)("supportingDocuments.caption")}</span>"""
          )
          val headingPosition = errorContent.indexOf("<h1 class=\"govuk-label-wrapper\">")
          errorGroupStart must be >= 0
          captionPosition must be > errorGroupStart
          headingPosition must be > captionPosition
          header("Cache-Control", errorPage).value mustEqual "no-store"
          val freshToken = formTokenFrom(contentAsString(errorPage))

          val noFileMultipartData: MultipartFormData[TemporaryFile] = MultipartFormData(
            dataParts = Map(formTokenField -> Seq(freshToken)),
            files     = Seq.empty,
            badParts  = Seq.empty
          )
          val noFileRequest = FakeRequest(POST, routes.UploadFileController.onSubmit().url)
            .withSession(formTokenKey -> freshToken)
            .withMultipartFormDataBody(noFileMultipartData)
          val noFileResult = route(application, noFileRequest).value

          status(noFileResult) mustEqual SEE_OTHER
          flash(noFileResult).get("uploadFile.fileError") mustBe Some("supportingDocuments.uploadFile.error.required")
        } finally {
          Files.deleteIfExists(temporaryFile.path)
        }
      }
    }

    "must show a select-file error when no file is submitted" in {
      val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

      running(application) {
        val multipartData: MultipartFormData[TemporaryFile] = MultipartFormData(
          dataParts = Map(formTokenField -> Seq("test-token")),
          files     = Seq.empty,
          badParts  = Seq.empty
        )
        val request = FakeRequest(POST, routes.UploadFileController.onSubmit().url)
          .withSession(formTokenKey -> "test-token")
          .withMultipartFormDataBody(multipartData)
        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual routes.UploadFileController.onPageLoad().url
        flash(result).get("uploadFile.fileError") mustBe Some("supportingDocuments.uploadFile.error.required")

        val errorPageRequest = FakeRequest(GET, routes.UploadFileController.onPageLoad().url)
          .withFlash("uploadFile.fileError" -> "supportingDocuments.uploadFile.error.required")
        val errorPage = route(application, errorPageRequest).value

        status(errorPage) mustEqual OK
        contentAsString(errorPage) must include(messages(application)("supportingDocuments.uploadFile.error.required"))
        contentAsString(errorPage) must not include messages(application)("supportingDocuments.uploadFile.error.empty")
      }
    }

    "must show a select-file error when the browser submits an empty file part after a size error" in {
      val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

      running(application) {
        val oversizedFile = application.injector.instanceOf[TemporaryFileCreator].create()
        val emptyFile = application.injector.instanceOf[TemporaryFileCreator].create()
        Files.write(oversizedFile.path, Array.fill(5 * 1024 * 1024 + 1)(0.toByte))
        val oversizedMultipartData = MultipartFormData(
          dataParts = Map(formTokenField -> Seq("old-token")),
          files = Seq(
            MultipartFormData.FilePart("file-upload-1", "large.pdf", Some("application/pdf"), oversizedFile)
          ),
          badParts = Seq.empty
        )
        val oversizedRequest = FakeRequest(POST, routes.UploadFileController.onSubmit().url)
          .withSession(formTokenKey -> "old-token")
          .withMultipartFormDataBody(oversizedMultipartData)
        val oversizedResult = route(application, oversizedRequest).value

        try {
          status(oversizedResult) mustEqual SEE_OTHER
          flash(oversizedResult).get("uploadFile.fileError") mustBe Some("supportingDocuments.uploadFile.error.size")

          val errorPageRequest = FakeRequest(GET, routes.UploadFileController.onPageLoad().url)
            .withFlash("uploadFile.fileError" -> "supportingDocuments.uploadFile.error.size")
            .withSession(formTokenKey -> "old-token")
          val errorPage = route(application, errorPageRequest).value
          status(errorPage) mustEqual OK
          val freshToken = formTokenFrom(contentAsString(errorPage))

          val staleFileRequest = FakeRequest(POST, routes.UploadFileController.onSubmit().url)
            .withSession(formTokenKey -> freshToken)
            .withMultipartFormDataBody(oversizedMultipartData)
          val staleFileResult = route(application, staleFileRequest).value

          status(staleFileResult) mustEqual SEE_OTHER
          flash(staleFileResult).get("uploadFile.fileError") mustBe Some("supportingDocuments.uploadFile.error.required")

          val noSelectionMultipartData = MultipartFormData(
            dataParts = Map(formTokenField -> Seq(freshToken)),
            files = Seq(
              MultipartFormData.FilePart("file-upload-1", "", Some("application/octet-stream"), emptyFile)
            ),
            badParts = Seq.empty
          )
          val noSelectionRequest = FakeRequest(POST, routes.UploadFileController.onSubmit().url)
            .withSession(formTokenKey -> freshToken)
            .withMultipartFormDataBody(noSelectionMultipartData)
          val noSelectionResult = route(application, noSelectionRequest).value

          status(noSelectionResult) mustEqual SEE_OTHER
          flash(noSelectionResult).get("uploadFile.fileError") mustBe Some("supportingDocuments.uploadFile.error.required")
        } finally {
          Files.deleteIfExists(oversizedFile.path)
          Files.deleteIfExists(emptyFile.path)
        }
      }
    }
  }
}
