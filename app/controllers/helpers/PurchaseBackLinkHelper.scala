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

package controllers.helpers

import controllers.purchase.routes
import models.requests.DataRequest
import models.{Mode, PurchaseOrImportSubCategoryType, PurchaseOrImportType}
import models.PurchaseOrImportSubCategoryType.{defaultSlugFor, purchaseOrImportSubCategoryUrlSlugFor}
import pages.{PurchaseSubCategoryPage, PurchaseSubTypePage, PurchaseTypePage}
import play.api.mvc.Call
import utils.MountPrefix

object PurchaseBackLinkHelper {

  def computeBackTarget(mode: Mode)(implicit request: DataRequest[?]): Call = {
    val maybePurchaseTypeSlug = request.userAnswers.get(PurchaseTypePage).map(PurchaseOrImportType.urlSlugForPurchaseType)
    val maybeParentCode = request.userAnswers.get(PurchaseSubTypePage)
    val maybeChildCode = request.userAnswers.get(PurchaseSubCategoryPage)
    lazy val purchaseType = maybePurchaseTypeSlug
      .flatMap(urlSlug => PurchaseOrImportType.values.find(PurchaseOrImportType.urlSlugForPurchaseType(_) == urlSlug))

    (maybePurchaseTypeSlug, maybeParentCode, maybeChildCode) match {
      case (Some(urlSlug), None, Some(child)) if child.contains(".") && purchaseType.isDefined =>
        val parentKey = purchaseType.get.toString

        purchaseOrImportSubCategoryUrlSlugFor(parentKey, child)
          .orElse(purchaseOrImportSubCategoryUrlSlugFor(parentKey, child.split("\\.").head))
          .orElse(defaultSlugFor(parentKey))
          .map(urlSlug => Call("GET", s"${MountPrefix.getFromRequest}/$urlSlug"))
          .getOrElse(routes.PurchaseTypeController.onPageLoad(mode))
      case (Some(_), Some(parent), Some(_)) if purchaseType.isDefined =>
        val slugPath = PurchaseOrImportSubCategoryType.pathFor(purchaseType.get.toString, parent)
        Call("GET", s"${MountPrefix.getFromRequest}/$slugPath")
      case (Some(slug), Some(_), None) =>
        Call("GET", s"${MountPrefix.getFromRequest}/$slug")
      case _ => routes.PurchaseTypeController.onPageLoad(mode)
    }
  }
}
