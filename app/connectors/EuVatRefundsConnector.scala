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

import models.requests.*
import models.responses.*
import play.api.Logging
import play.api.libs.json.Json
import play.api.libs.ws.JsonBodyWritables.writeableOf_JsValue
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{HeaderCarrier, HttpReadsInstances, HttpResponse, StringContextOps}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class EuVatRefundsConnector @Inject() (config: ServicesConfig, http: HttpClientV2)(implicit ec: ExecutionContext)
    extends HttpReadsInstances
    with Logging {

  private val euVatRefundsBaseUrl: String = config.baseUrl("euvat-refunds") + "/euvat-refunds"

  def retrieveTradersKnownFacts()(implicit hc: HeaderCarrier): Future[TraderKnownFactsResponse] = {
    http
      .get(url"$euVatRefundsBaseUrl/traders/get-known-facts")
      .execute[TraderKnownFactsResponse]
  }

  def getLatestApplications(request: LatestApplicationRequest)(implicit hc: HeaderCarrier): Future[LatestApplicationResponse] = {
    http
      .post(url"$euVatRefundsBaseUrl/get-latest-application")
      .withBody(Json.toJson(request))
      .execute[LatestApplicationResponse]
  }

  def createApplication(request: ApplicationRequest)(implicit hc: HeaderCarrier): Future[ApplicationResponse] = {
    http
      .post(url"$euVatRefundsBaseUrl/create-application")
      .withBody(Json.toJson(request))
      .execute[ApplicationResponse]
  }

  def addPurchase(request: AddPurchaseRequest)(implicit hc: HeaderCarrier): Future[AddPurchaseResponse] = {
    http
      .post(url"$euVatRefundsBaseUrl/add-purchase")
      .withBody(Json.toJson(request))
      .execute[AddPurchaseResponse]
  }

  def updatePurchase(request: UpdatePurchaseRequest)(implicit hc: HeaderCarrier): Future[UpdatePurchaseResponse] = {
    http
      .put(url"$euVatRefundsBaseUrl/update-purchase-details")
      .withBody(Json.toJson(request))
      .execute[UpdatePurchaseResponse]
  }

  def getSupplierVrnCount(request: SupplierVrnCountRequest)(implicit hc: HeaderCarrier): Future[SupplierVrnCountResponse] = {
    http
      .post(url"$euVatRefundsBaseUrl/get-supplier-vrn-count")
      .withBody(Json.toJson(request))
      .execute[SupplierVrnCountResponse]
  }

  def updateApplicationDetails(request: UpdateApplicationDetailsRequest)(implicit hc: HeaderCarrier): Future[UpdateApplicationDetailsResponse] =
    http
      .put(url"$euVatRefundsBaseUrl/update-application-details")
      .withBody(Json.toJson(request))
      .execute[UpdateApplicationDetailsResponse]

  def getSupplierTaxIdentifierCount(
    request: SupplierTaxIdentifierCountRequest
  )(implicit hc: HeaderCarrier): Future[SupplierTaxIdentifierCountResponse] = {
    http
      .post(url"$euVatRefundsBaseUrl/get-supplier-taxIdentifier-count")
      .withBody(Json.toJson(request))
      .execute[SupplierTaxIdentifierCountResponse]
  }

  def getPurchaseImportList(request: PurchaseImportListRequest)(implicit hc: HeaderCarrier): Future[PurchaseImportListResponse] = {
    http
      .post(url"$euVatRefundsBaseUrl/get-purchases-and-imports")
      .withBody(Json.toJson(request))
      .execute[PurchaseImportListResponse]
  }

  def deleteApplication(request: DeleteApplicationRequest)(implicit hc: HeaderCarrier): Future[HttpResponse] = {
    http
      .delete(url"$euVatRefundsBaseUrl/delete-application")
      .withBody(Json.toJson(request))
      .execute[HttpResponse]
  }

  def addImport(request: AddImportRequest)(implicit hc: HeaderCarrier): Future[AddImportResponse] =
    http
      .post(url"$euVatRefundsBaseUrl/add-import")
      .withBody(Json.toJson(request))
      .execute[AddImportResponse]

  def getSadReferenceCount(request: SadReferenceCountRequest)(implicit hc: HeaderCarrier): Future[SadReferenceCountResponse] = {
    http
      .post(url"$euVatRefundsBaseUrl/get-sad-reference-count")
      .withBody(Json.toJson(request))
      .execute[SadReferenceCountResponse]
  }

  def getReferenceInfoCount(
    request: ReferenceInfoCountRequest
  )(implicit hc: HeaderCarrier): Future[ReferenceInfoCountResponse] =
    http
      .post(url"$euVatRefundsBaseUrl/get-reference-information-count")
      .withBody(Json.toJson(request))
      .execute[ReferenceInfoCountResponse]

}
