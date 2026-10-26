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

package connectors

import models.PurchaseImport
import models.requests.*
import models.responses.*
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers.*
import org.mockito.Mockito.*
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.mockito.MockitoSugar
import play.api.libs.json.{JsValue, Json}
import uk.gov.hmrc.http.*
import uk.gov.hmrc.http.client.{HttpClientV2, RequestBuilder}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import java.time.LocalDateTime
import scala.concurrent.{ExecutionContext, Future}

class EuVatRefundsConnectorSpec extends AnyWordSpec with Matchers with MockitoSugar with ScalaFutures {

  implicit val ec: ExecutionContext = ExecutionContext.global
  implicit val hc: HeaderCarrier = HeaderCarrier()

  val mockHttp: HttpClientV2 = mock[HttpClientV2]
  val mockRequestBuilder: RequestBuilder = mock[RequestBuilder]
  val mockConfig: ServicesConfig = mock[ServicesConfig]
  val baseUrl = "http://localhost:9000/euvat-refunds"
  when(mockConfig.baseUrl("euvat-refunds")).thenReturn("http://localhost:9000")
  val connector = new EuVatRefundsConnector(mockConfig, mockHttp)

  "EuVatRefundsConnector.retrieveTradersKnownFacts" should {

    "call the correct URL and return the expected response" in {
      val expected = TraderKnownFactsResponse(
        vatRegNumber = 123,
        traderName   = Some("ABC GmbH"),
        tradeClass   = Some("49200")
      )
      when(mockHttp.get(any())(any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.execute[TraderKnownFactsResponse](any(), any()))
        .thenReturn(Future.successful(expected))

      val result = connector.retrieveTradersKnownFacts().futureValue
      result shouldBe expected

      verify(mockHttp).get(url"$baseUrl/traders/get-known-facts")
      verify(mockRequestBuilder).execute[TraderKnownFactsResponse](any(), any())
    }

    "propagate failures from the HTTP client" in {
      val failure = new RuntimeException("boom")

      when(mockHttp.get(any())(any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.execute[TraderKnownFactsResponse](any(), any()))
        .thenReturn(Future.failed(failure))

      val result = connector.retrieveTradersKnownFacts()
      whenReady(result.failed) { ex =>
        ex shouldBe failure
      }
    }
  }

  "EuVatRefundsConnector.getLatestApplications" should {
    val request = LatestApplicationRequest(
      applicantVatRegNumber = "123456789",
      refundingCountry      = Some("LV"),
      startDate             = Some(LocalDateTime.of(2025, 2, 1, 0, 0)),
      endDate               = Some(LocalDateTime.of(2025, 5, 31, 0, 0)),
      representativeId      = Some("rep123"),
      maxNumber             = 10,
      orderBy               = None,
      sortOrder             = None,
      startAt               = None
    )
    val expectedResponse = LatestApplicationResponse(
      applications     = List.empty,
      totalApplication = 0
    )

    "call the correct URL and return the expected response" in {
      reset(mockHttp, mockRequestBuilder)

      when(mockHttp.post(any())(any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.withBody(any())(any(), any(), any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.execute[LatestApplicationResponse](any(), any()))
        .thenReturn(Future.successful(expectedResponse))

      val result = connector.getLatestApplications(request).futureValue

      result shouldBe expectedResponse

      verify(mockHttp).post(url"$baseUrl/get-latest-application")
      verify(mockRequestBuilder).execute[LatestApplicationResponse](any(), any())
    }

    "propagate failures from the HTTP client" in {
      val failure = new RuntimeException("boom")

      when(mockHttp.post(any())(any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.withBody(any())(any(), any(), any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.execute[LatestApplicationResponse](any(), any()))
        .thenReturn(Future.failed(failure))

      val result = connector.getLatestApplications(request)

      whenReady(result.failed) { ex =>
        ex shouldBe failure
      }
    }
  }

  "EuVatRefundsConnector.getSupplierVrnCount" should {
    val request = SupplierVrnCountRequest(
      applicationId = 133,
      itemNumber    = 4,
      vatNumber     = "500000881",
      invoiceNumber = "a444"
    )
    val expectedResponse = SupplierVrnCountResponse(duplicateCount = 1)

    "call the correct URL and return the expected response" in {
      reset(mockHttp, mockRequestBuilder)

      when(mockHttp.post(any())(any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.withBody(any())(any(), any(), any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.execute[SupplierVrnCountResponse](any(), any()))
        .thenReturn(Future.successful(expectedResponse))

      val result = connector.getSupplierVrnCount(request).futureValue

      result shouldBe expectedResponse

      verify(mockHttp).post(url"$baseUrl/get-supplier-vrn-count")
      verify(mockRequestBuilder).execute[SupplierVrnCountResponse](any(), any())
    }

    "propagate failures from the HTTP client" in {
      val failure = new RuntimeException("boom")

      when(mockHttp.post(any())(any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.withBody(any())(any(), any(), any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.execute[SupplierVrnCountResponse](any(), any()))
        .thenReturn(Future.failed(failure))

      val result = connector.getSupplierVrnCount(request)

      whenReady(result.failed) { ex =>
        ex shouldBe failure
      }
    }
  }

  "EuVatRefundsConnector.addPurchase" should {
    val request = AddPurchaseRequest(
      applicationId            = 123456,
      goodsDescriptionCategory = "1",
      goodsDescriptionText     = Some("Fuel"),
      updateSequenceNumber     = 1
    )

    val expectedResponse = AddPurchaseResponse(itemNumber = 4, updateSequenceNumber = 1)

    "call the correct URL and return the expected response" in {
      reset(mockHttp, mockRequestBuilder)

      when(mockHttp.post(any())(any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.withBody(any())(any(), any(), any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.execute[AddPurchaseResponse](any(), any()))
        .thenReturn(Future.successful(expectedResponse))

      val result = connector.addPurchase(request).futureValue

      result shouldBe expectedResponse

      verify(mockHttp).post(url"$baseUrl/add-purchase")
      verify(mockRequestBuilder).execute[AddPurchaseResponse](any(), any())
    }

    "propagate failures from the HTTP client" in {
      when(mockHttp.post(any())(any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.withBody(any())(any(), any(), any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.execute[AddPurchaseResponse](any(), any()))
        .thenReturn(Future.failed(new RuntimeException("boom")))

      whenReady(connector.addPurchase(request).failed) { ex =>
        ex shouldBe a[RuntimeException]
      }
    }

  }

  "EuVatRefundsConnector.addImport" should {

    val request = AddImportRequest(
      applicationId = 123456,
      goodsDescriptionCategory = "1",
      goodsDescriptionText = Some("Fuel"),
      updateSequenceNumber = 1
    )

    val expectedResponse = AddImportResponse(itemNumber = 4, updateSequenceNumber = 1)

    "call the correct URL and return the expected response" in {
      reset(mockHttp, mockRequestBuilder)

      when(mockHttp.post(any())(any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.withBody(any())(any(), any(), any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.execute[AddImportResponse](any(), any()))
        .thenReturn(Future.successful(expectedResponse))

      val result = connector.addImport(request).futureValue

      result shouldBe expectedResponse

      verify(mockHttp).post(url"$baseUrl/add-import")
      verify(mockRequestBuilder).execute[AddImportResponse](any(), any())
    }

    "propagate failures from the HTTP client" in {
      when(mockHttp.post(any())(any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.withBody(any())(any(), any(), any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.execute[AddImportResponse](any(), any()))
        .thenReturn(Future.failed(new RuntimeException("boom")))

      whenReady(connector.addImport(request).failed) { ex =>
        ex shouldBe a[RuntimeException]
      }
    }

  }

  "EuVatRefundsConnector.getSupplierTaxIdentifierCount" should {
    val requestPayload = SupplierTaxIdentifierCountRequest(applicationId = 123L, itemNumber = 1, taxIdentifier = "TAX123", invoiceNumber = "INV1")
    val expectedResponse = SupplierTaxIdentifierCountResponse(duplicateCount = 2)

    "call the correct URL and return the expected response" in {
      reset(mockHttp, mockRequestBuilder)

      when(mockHttp.post(any())(any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.withBody(any())(any(), any(), any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.execute[SupplierTaxIdentifierCountResponse](any(), any()))
        .thenReturn(Future.successful(expectedResponse))

      val result = connector.getSupplierTaxIdentifierCount(requestPayload).futureValue

      result shouldBe expectedResponse

      verify(mockHttp).post(url"$baseUrl/get-supplier-taxIdentifier-count")
      verify(mockRequestBuilder).execute[SupplierTaxIdentifierCountResponse](any(), any())
    }

    "propagate failures from the HTTP client" in {
      when(mockHttp.post(any())(any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.withBody(any())(any(), any(), any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.execute[SupplierTaxIdentifierCountResponse](any(), any()))
        .thenReturn(Future.failed(new RuntimeException("boom")))

      whenReady(connector.getSupplierTaxIdentifierCount(requestPayload).failed) { ex =>
        ex shouldBe a[RuntimeException]
      }
    }
  }

  "EuVatRefundsConnector.updatePurchase" should {
    val updateRequest = models.requests.UpdatePurchaseRequest(
      applicationId               = 123L,
      itemNumber                  = 1,
      goodsDescriptionCategory    = "1.2",
      goodsDescriptionSubCategory = None,
      goodsDescriptionText        = Some("Fuel"),
      simplifiedInvoiceIndicator  = Some("Y"),
      supplierName                = Some("Supplier"),
      supplierAddress1            = None,
      supplierAddress2            = None,
      supplierAddress3            = None,
      supplierVatRegNumber        = None,
      supplierTaxIdentifier       = None,
      invoiceDate                 = None,
      invoiceNumber               = None,
      currencyCode                = None,
      taxableAmount               = None,
      vatAmount                   = None,
      deductibleVatAmount         = None,
      updateSequenceNumber        = 1
    )

    val expectedResponse = UpdatePurchaseResponse(updateSequenceNumber = 42)

    "call the correct URL and return the expected response" in {
      reset(mockHttp, mockRequestBuilder)

      when(mockHttp.put(any())(any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.withBody(any())(any(), any(), any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.execute[UpdatePurchaseResponse](any(), any()))
        .thenReturn(Future.successful(expectedResponse))

      val result = connector.updatePurchase(updateRequest).futureValue

      result shouldBe expectedResponse

      verify(mockHttp).put(url"$baseUrl/update-purchase-details")
      verify(mockRequestBuilder).execute[UpdatePurchaseResponse](any(), any())
    }

    "propagate failures from the HTTP client" in {
      val failure = new RuntimeException("boom")

      when(mockHttp.put(any())(any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.withBody(any())(any(), any(), any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.execute[UpdatePurchaseResponse](any(), any()))
        .thenReturn(Future.failed(failure))

      val result = connector.updatePurchase(updateRequest)

      whenReady(result.failed) { ex =>
        ex shouldBe failure
      }
    }
  }

  "EuVatRefundsConnector.getPurchaseImportList" should {
    val requestPayload = PurchaseImportListRequest(applicationId = 123)
    val expectedResponse = PurchaseImportListResponse(
      totalVatClaims = BigDecimal(334),
      totalItems     = 2,
      purchaseImportList = List(
        PurchaseImport(
          itemNumber                  = 123,
          itemType                    = "P",
          goodsDescriptionCategory    = "1",
          goodsDescriptionSubCategory = Some("1.2.3"),
          currencyCode                = Some("EU"),
          taxableAmount               = BigDecimal(300),
          vatAmount                   = BigDecimal(200),
          deductibleVatAmount         = BigDecimal(100)
        ),
        PurchaseImport(
          itemNumber                  = 456,
          itemType                    = "I",
          goodsDescriptionCategory    = "7",
          goodsDescriptionSubCategory = Some("7.9"),
          currencyCode                = Some("EU"),
          taxableAmount               = BigDecimal(456),
          vatAmount                   = BigDecimal(345),
          deductibleVatAmount         = BigDecimal(234)
        )
      )
    )

    "call the correct URL and return the expected response" in {
      reset(mockHttp, mockRequestBuilder)

      when(mockHttp.post(any())(any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.withBody(any())(any(), any(), any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.execute[PurchaseImportListResponse](any(), any()))
        .thenReturn(Future.successful(expectedResponse))

      val result = connector.getPurchaseImportList(requestPayload).futureValue
      result shouldBe expectedResponse

      verify(mockHttp).post(url"$baseUrl/get-purchases-and-imports")
      verify(mockRequestBuilder).execute[PurchaseImportListResponse](any(), any())
    }

    "propagate failures from the HTTP client" in {
      when(mockHttp.post(any())(any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.withBody(any())(any(), any(), any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.execute[PurchaseImportListResponse](any(), any()))
        .thenReturn(Future.failed(new RuntimeException("boom")))

      whenReady(connector.getPurchaseImportList(requestPayload).failed) { ex =>
        ex shouldBe a[RuntimeException]
      }
    }
  }

  "EuVatRefundsConnector.deleteApplication" should {

    "call the delete endpoint with JSON body and return Unit" in {
      reset(mockHttp, mockRequestBuilder)
      val req = DeleteApplicationRequest(123L, 1)

      when(mockHttp.delete(any())(any())).thenReturn(mockRequestBuilder)

      doReturn(mockRequestBuilder).when(mockRequestBuilder).withBody(any[JsValue]())(any(), any(), any())
      doReturn(Future.successful(())).when(mockRequestBuilder).execute[Unit](any(), any())

      val result: Unit = connector.deleteApplication(req).futureValue

      result shouldBe ()

      verify(mockHttp).delete(url"$baseUrl/delete-application")
      val bodyCaptor: ArgumentCaptor[JsValue] = ArgumentCaptor.forClass(classOf[JsValue])
      verify(mockRequestBuilder).withBody(bodyCaptor.capture())(any(), any(), any())
      bodyCaptor.getValue shouldBe Json.toJson(req)
      verify(mockRequestBuilder).execute[Unit](any(), any())
    }
  }

  "EuVatRefundsConnector.updateApplicationDetails" should {

    val updateRequest = UpdateApplicationDetailsRequest(
      applicationId              = 133,
      applicationLanguage        = "en",
      refundingCountry           = "LV",
      periodStartDate            = LocalDateTime.of(2011, 6, 1, 0, 0),
      periodEndDate              = LocalDateTime.of(2011, 10, 31, 23, 59, 59),
      applicantEmailAddress      = "test@hotmail.com",
      applicantPhoneNumber       = None,
      representativeCountry      = None,
      representativeEmailAddress = None,
      representativePhoneNumber  = None,
      bankAccountOwnerName       = None,
      bankAccountOwnerType       = None,
      ibanCode                   = None,
      bicCode                    = None,
      bankAccountCurrencyCode    = None,
      businessActivityCode2      = None,
      businessActivityCode3      = None,
      cipherText                 = None,
      encryptionStatus           = None,
      updateSequenceNumber       = 30
    )

    val expectedResponse = UpdateApplicationDetailsResponse(updateSequenceNumber = 32)

    "call the correct URL and return the expected response" in {
      reset(mockHttp, mockRequestBuilder)

      when(mockHttp.put(any())(any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.withBody(any())(any(), any(), any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.execute[UpdateApplicationDetailsResponse](any(), any()))
        .thenReturn(Future.successful(expectedResponse))

      val result = connector.updateApplicationDetails(updateRequest).futureValue

      result shouldBe expectedResponse

      verify(mockHttp).put(url"$baseUrl/update-application-details")
      verify(mockRequestBuilder).execute[UpdateApplicationDetailsResponse](any(), any())
    }

    "propagate failures from the HTTP client" in {
      val failure = new RuntimeException("boom")

      when(mockHttp.put(any())(any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.withBody(any())(any(), any(), any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.execute[UpdateApplicationDetailsResponse](any(), any()))
        .thenReturn(Future.failed(failure))

      val result = connector.updateApplicationDetails(updateRequest)

      whenReady(result.failed) { ex =>
        ex shouldBe failure
      }
    }
  }

  "EuVatRefundsConnector.getSadReferenceCount" should {
    val request = SadReferenceCountRequest(
      applicationId = 133,
      itemNumber = 4,
      sadReferenceNumber = "12345"
    )
    val expectedResponse = SadReferenceCountResponse(duplicateCount = 1)

    "call the correct URL and return the expected response" in {
      reset(mockHttp, mockRequestBuilder)

      when(mockHttp.post(any())(any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.withBody(any())(any(), any(), any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.execute[SadReferenceCountResponse](any(), any()))
        .thenReturn(Future.successful(expectedResponse))

      val result = connector.getSadReferenceCount(request).futureValue

      result shouldBe expectedResponse

      verify(mockHttp).post(url"$baseUrl/get-sad-reference-count")
      verify(mockRequestBuilder).execute[SadReferenceCountResponse](any(), any())
    }

    "propagate failures from the HTTP client" in {
      val failure = new RuntimeException("boom")

      when(mockHttp.post(any())(any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.withBody(any())(any(), any(), any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.execute[SadReferenceCountResponse](any(), any()))
        .thenReturn(Future.failed(failure))

      val result = connector.getSadReferenceCount(request)

      whenReady(result.failed) { ex =>
        ex shouldBe failure
      }
    }
  }

  "EuVatRefundsConnector.getReferenceInfoCount" should {
    val itemNumber = 3
    val applicationId = 123L
    val url = url"$baseUrl/get-reference-information-count"
    val request = ReferenceInfoCountRequest(applicationId, itemNumber)

    "call the correct URL and return the expected response" in {
      reset(mockHttp, mockRequestBuilder)

      val expectedResponse = ReferenceInfoCountResponse(2)

      when(mockHttp.post(url)(hc)).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.withBody(any())(any(), any(), any[ExecutionContext]())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.execute[ReferenceInfoCountRequest](any(), any[ExecutionContext]()))
        .thenReturn(Future.successful(expectedResponse))

      val result = connector.getReferenceInfoCount(request).futureValue

      result shouldBe expectedResponse
    }

    "propagate failures from the HTTP client" in {
      reset(mockHttp, mockRequestBuilder)
      val failure = new RuntimeException("failure")

      when(mockHttp.post(any())(any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.withBody(any())(any(), any(), any())).thenReturn(mockRequestBuilder)
      when(mockRequestBuilder.execute[ReferenceInfoCountRequest](any(), any()))
        .thenReturn(Future.failed(failure))

      val result = connector.getReferenceInfoCount(request)

      result.failed.futureValue shouldBe failure
    }
  }

}
