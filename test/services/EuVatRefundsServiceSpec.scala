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

package services

import base.SpecBase
import config.FrontendAppConfig
import connectors.EuVatRefundsConnector
import models.requests.*
import models.responses.*
import org.mockito.ArgumentMatchers.*
import org.mockito.Mockito.*
import org.scalatest.concurrent.ScalaFutures
import org.scalatestplus.mockito.MockitoSugar
import uk.gov.hmrc.http.HeaderCarrier

import java.time.LocalDateTime
import scala.concurrent.{ExecutionContext, Future}

class EuVatRefundsServiceSpec extends SpecBase with MockitoSugar with ScalaFutures {

  implicit val ec: ExecutionContext = ExecutionContext.global
  implicit val hc: HeaderCarrier = HeaderCarrier()

  val mockConnector: EuVatRefundsConnector = mock[EuVatRefundsConnector]
  val mockConfig: FrontendAppConfig = mock[FrontendAppConfig]
  val service = new EuVatRefundsService(mockConnector, mockConfig)

  "EuVatRefundsService.retrieveTraderKnownFacts" - {
    val expected = TraderKnownFactsResponse(
      vatRegNumber = 123,
      traderName   = Some("ABC GmbH"),
      tradeClass   = Some("49200")
    )

    "should retrieve existing details from Cache first" in {
      when(mockConnector.retrieveTradersKnownFacts()(any()))
        .thenReturn(Future.successful(expected))

      val result = service.retrieveTraderKnownFacts()(any()).futureValue
      result mustEqual expected
    }
  }
  "EuVatRefundsService.getLatestApplications" - {

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

    "should return latest applications from the connector" in {
      when(mockConnector.getLatestApplications(any())(any()))
        .thenReturn(Future.successful(expectedResponse))

      val result = service.getLatestApplications(request)(hc).futureValue
      result mustEqual expectedResponse
    }

    "should propagate an exception from the connector" in {
      val failure = new RuntimeException("Connector failed")

      when(mockConnector.getLatestApplications(any())(any()))
        .thenReturn(Future.failed(failure))

      val result = service.getLatestApplications(request)

      whenReady(result.failed) { ex =>
        ex mustEqual failure
      }
    }
  }

  "EuVatRefundsService.getSupplierVrnCount" - {

    val request = SupplierVrnCountRequest(
      applicationId = 133,
      itemNumber    = 4,
      vatNumber     = "500000881",
      invoiceNumber = "a444"
    )

    val expectedResponse = SupplierVrnCountResponse(duplicateCount = 1)

    "should return the supplier VRN count from the connector" in {
      when(mockConnector.getSupplierVrnCount(any())(any()))
        .thenReturn(Future.successful(expectedResponse))

      val result = service.getSupplierVrnCount(request)(hc).futureValue
      result mustEqual expectedResponse
    }

    "should propagate an exception from the connector" in {
      val failure = new RuntimeException("Connector failed")

      when(mockConnector.getSupplierVrnCount(any())(any()))
        .thenReturn(Future.failed(failure))

      val result = service.getSupplierVrnCount(request)

      whenReady(result.failed) { ex =>
        ex mustEqual failure
      }
    }
  }

  "EuVatRefundsService.deleteApplication" - {

    "should delegate to connector and return Unit" in {
      val req = models.requests.DeleteApplicationRequest(123L, 1)
      when(mockConnector.deleteApplication(any())(any())).thenReturn(Future.successful(()))

      service.deleteApplication(req)(hc).futureValue mustEqual ()
      verify(mockConnector).deleteApplication(req)(hc)
    }

    "should propagate connector failures" in {
      val req = models.requests.DeleteApplicationRequest(123L, 1)
      val failure = new RuntimeException("boom")
      when(mockConnector.deleteApplication(any())(any())).thenReturn(Future.failed(failure))

      val result = service.deleteApplication(req)
      whenReady(result.failed) { ex =>
        ex mustEqual failure
      }
    }

  }

  "EuVatRefundsService.addPurchase" - {

    val request = AddPurchaseRequest(
      applicationId              = 123456,
      goodsDescriptionCategory   = "1",
      goodsDescriptionText       = Some("Fuel"),
      purchaseSubcategory        = None,
      simplifiedInvoiceIndicator = None,
      supplierName               = None,
      supplierAddress1           = None,
      supplierAddress2           = None,
      supplierAddress3           = None,
      supplierVatRegNumber       = None,
      supplierTaxIdentifier      = None,
      invoiceDate                = None,
      invoiceNumber              = None,
      currencyCode               = None,
      taxableAmount              = None,
      vatAmount                  = None,
      deductibleVatAmount        = None,
      updateSequenceNumber       = 1
    )

    val expectedResponse = AddPurchaseResponse(itemNumber = 4, updateSequenceNumber = 1)

    "should return the add purchase response from the connector" in {
      when(mockConnector.addPurchase(any())(any()))
        .thenReturn(Future.successful(expectedResponse))

      service.addPurchase(request)(hc).futureValue mustEqual expectedResponse
    }

    "should propagate an exception from the connector" in {
      val failure = new RuntimeException("Connector failed")

      when(mockConnector.addPurchase(any())(any()))
        .thenReturn(Future.failed(failure))

      whenReady(service.addPurchase(request).failed) { ex =>
        ex mustEqual failure
      }
    }
  }

  "EuVatRefundsService.addImport" - {

    val request = AddImportRequest(
      applicationId            = 123456,
      goodsDescriptionCategory = "1",
      updateSequenceNumber     = 1
    )

    val expectedResponse = AddImportResponse(itemNumber = 4, updateSequenceNumber = 1)

    "should return the add import response from the connector" in {
      when(mockConnector.addImport(any())(any()))
        .thenReturn(Future.successful(expectedResponse))

      service.addImport(request)(hc).futureValue mustEqual expectedResponse
    }

    "should propagate an exception from the connector" in {
      val failure = new RuntimeException("Connector failed")

      when(mockConnector.addImport(any())(any()))
        .thenReturn(Future.failed(failure))

      whenReady(service.addImport(request).failed) { ex =>
        ex mustEqual failure
      }
    }
  }

  "EuVatRefundsService.updatePurchase" - {

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

    val expectedResponse = models.responses.UpdatePurchaseResponse(updateSequenceNumber = 42)

    "should return the update purchase response from the connector" in {
      when(mockConnector.updatePurchase(any())(any()))
        .thenReturn(Future.successful(expectedResponse))

      service.updatePurchase(updateRequest)(hc).futureValue mustEqual expectedResponse
    }

    "should propagate an exception from the connector" in {
      val failure = new RuntimeException("Connector failed")

      when(mockConnector.updatePurchase(any())(any()))
        .thenReturn(Future.failed(failure))

      whenReady(service.updatePurchase(updateRequest).failed) { ex =>
        ex mustEqual failure
      }
    }
  }

  "EuVatRefundsService.getSupplierTaxIdentifierCount" - {

    val request = SupplierTaxIdentifierCountRequest(applicationId = 123L, itemNumber = 1, taxIdentifier = "TAX123", invoiceNumber = "INV1")
    val expectedResponse = SupplierTaxIdentifierCountResponse(duplicateCount = 0)

    "should return the response from the connector" in {
      when(mockConnector.getSupplierTaxIdentifierCount(any())(any()))
        .thenReturn(Future.successful(expectedResponse))

      service.getSupplierTaxIdentifierCount(request)(hc).futureValue mustEqual expectedResponse
    }

    "should propagate an exception from the connector" in {
      val failure = new RuntimeException("Connector failed")

      when(mockConnector.getSupplierTaxIdentifierCount(any())(any()))
        .thenReturn(Future.failed(failure))

      whenReady(service.getSupplierTaxIdentifierCount(request).failed) { ex =>
        ex mustEqual failure
      }
    }
  }

  "EuVatRefundsService.updateApplicationDetails" - {

    val request = UpdateApplicationDetailsRequest(
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

    "should return the update application details response from the connector" in {
      when(mockConnector.updateApplicationDetails(any())(any()))
        .thenReturn(Future.successful(expectedResponse))

      service.updateApplicationDetails(request)(hc).futureValue mustEqual expectedResponse
    }

    "should propagate an exception from the connector" in {
      val failure = new RuntimeException("Connector failed")

      when(mockConnector.updateApplicationDetails(any())(any()))
        .thenReturn(Future.failed(failure))

      whenReady(service.updateApplicationDetails(request).failed) { ex =>
        ex mustEqual failure
      }
    }
  }

  "EuVatRefundsService.getSadReferenceCount" - {

    val request = SadReferenceCountRequest(
      applicationId      = 133,
      itemNumber         = 4,
      sadReferenceNumber = "500000881"
    )

    val expectedResponse = SadReferenceCountResponse(duplicateCount = 1)

    "should return the SAD reference count from the connector" in {
      when(mockConnector.getSadReferenceCount(any())(any()))
        .thenReturn(Future.successful(expectedResponse))

      val result = service.getSadReferenceCount(request)(hc).futureValue
      result mustEqual expectedResponse
    }

    "should propagate an exception from the connector" in {
      val failure = new RuntimeException("Connector failed")

      when(mockConnector.getSadReferenceCount(any())(any()))
        .thenReturn(Future.failed(failure))

      val result = service.getSadReferenceCount(request)

      whenReady(result.failed) { ex =>
        ex mustEqual failure
      }
    }
  }

  "EuVatRefundsService.getReferenceInfoCount" - {
    val itemNumber = 3
    val applicationId = 123
    val request = ReferenceInfoCountRequest(applicationId, itemNumber)

    "should return reference information count when connector succeeds" in {
      val response = ReferenceInfoCountResponse(2)
      when(mockConnector.getReferenceInfoCount(request)(hc))
        .thenReturn(Future.successful(response))

      val result = service.getReferenceInfoCount(request).futureValue

      result mustEqual response
    }

    "should propagate error when connector fails" in {
      val exception = RuntimeException("Connector error")
      when(mockConnector.getReferenceInfoCount(any[ReferenceInfoCountRequest]())(any[HeaderCarrier]()))
        .thenReturn(Future.failed(exception))

      val result = service.getReferenceInfoCount(request).failed.futureValue

      result mustEqual exception
    }
  }
}
