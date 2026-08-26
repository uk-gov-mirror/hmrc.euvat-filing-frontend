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

import models.requests.DataRequest
import play.api.data.Form
import play.api.libs.json.{Format, Reads}
import play.api.mvc.{Call, Result}
import queries.Gettable
import pages.QuestionPage
import repositories.SessionRepository
import scala.concurrent.{ExecutionContext, Future}
import play.api.mvc.Results.*
import models.{CheckMode, Mode, UserAnswers}

object ControllerHelpers {

  extension [T](form: Form[T]) {
    def preparedFromAnswers(page: Gettable[T], userAnswers: UserAnswers)(implicit rds: Reads[T]): Form[T] =
      userAnswers.get(page).fold(form)(form.fill)
  }

  // Combine two Option values into a tuple when both are defined.
  def bothDefined[A, B](first: Option[A], second: Option[B]): Option[(A, B)] =
    for {
      a <- first
      b <- second
    } yield (a, b)

  // Resolve the human-friendly currency name and prefix for views using the
  // central CurrencyResolver. This helper simply delegates and provides a
  // consistent signature for controllers to call.
  def currencyNameAndPrefix(userAnswers: models.UserAnswers, configCurrencyMapping: Map[String, Seq[Currency]])(implicit
    request: DataRequest[?]
  ): (String, String) = CurrencyResolver.currencyNameAndPrefix(userAnswers, configCurrencyMapping)

  // Return a display-friendly currency symbol extracted from session/config.
  // Falls back to the Euro symbol when no symbol can be resolved.
  def currencySymbolFromSession(userAnswers: models.UserAnswers, configCurrencyMapping: Map[String, Seq[Currency]])(implicit
    request: DataRequest[?]
  ): String = {
    // Reuse the name/prefix resolver and pick the symbol portion
    val (_, symbol) = currencyNameAndPrefix(userAnswers, configCurrencyMapping)
    // Fallback to Euro if the resolver returned an empty prefix
    if (symbol.isEmpty) "€" else symbol
  }

  // Generic helper to compare a submitted `value` against a BigDecimal stored
  // on another page in `UserAnswers` using a provided comparator function.
  //
  // Example usage:
  // `compareWithPage(value, TotalPurchaseAmountBeforeVatPage, updated)(_ >= _)`
  def compareWithPage(value: BigDecimal, page: pages.QuestionPage[BigDecimal], updated: models.UserAnswers)(
    cmp: (BigDecimal, BigDecimal) => Boolean
  ): Boolean =
    updated.get(page).exists(stored => cmp(value, stored))

  def pathForSlug(slug: String, mode: Mode, prefix: String): String =
    if (mode == models.CheckMode) {
      if (prefix.isEmpty) s"/change-$slug" else s"$prefix/change-$slug"
    } else {
      if (prefix.isEmpty) s"/$slug" else s"$prefix/$slug"
    }

  def redirectToInvoiceTypeOrCYA(mode: Mode): Result = {
    if (mode == models.CheckMode) {
      Redirect(controllers.purchase.routes.CheckYourPurchaseDetailsController.onPageLoad())
    } else {
      Redirect(controllers.routes.InvoiceTypeController.onPageLoad(mode))
    }
  }

  /** Shared submit helper that centralises the common CheckMode short-circuit pattern used across monetary input controllers.
    *
    * Behaviour:
    *   - If in CheckMode and a `PurchaseTypePage` is present in `userAnswers` the `purchaseCya` call is used as the unchanged-redirect target.
    *   - Otherwise `navigatorNext` is used as the unchanged-redirect target.
    *
    * The `onSaved` continuation is invoked with the updated `UserAnswers` when the value changes (or when not in CheckMode) so callers can decide the
    * appropriate redirect (including any warning-page checks).
    */
  case class ShortCircuitParams[T](
    page: QuestionPage[T],
    newValue: T,
    mode: models.Mode,
    userAnswers: models.UserAnswers,
    sessionRepository: SessionRepository,
    navigatorNext: Call,
    purchaseCya: Call
  )

  def shortCircuitPersistAndThen[T](
    params: ShortCircuitParams[T]
  )(onSaved: UserAnswers => Future[Result])(implicit fmt: Format[T], ec: ExecutionContext): Future[Result] = {
    // Choose the unchanged-redirect target according to the purchase-journey
    // short-circuit rule that routes CheckMode purchase flows back to the
    // purchase CYA without persisting when the value is unchanged.
    val unchangedRedirect: Call =
      if (params.mode == models.CheckMode && params.userAnswers.get(pages.PurchaseTypePage).isDefined) {
        params.purchaseCya
      } else {
        params.navigatorNext
      }

    CheckModeShortCircuit(
      CheckModeShortCircuit.ShortCircuitArgs(
        params.page,
        params.newValue,
        params.mode,
        params.userAnswers,
        params.sessionRepository,
        unchangedRedirect,
        onSaved
      )
    )
  }

  /** If running in CheckMode and the arrival flag page is not set, set it and persist the updated `UserAnswers`. Otherwise call `render` with the
    * existing `UserAnswers`.
    */
  def markArrivalAndRender(
    page: QuestionPage[Boolean],
    mode: Mode,
    userAnswers: UserAnswers,
    sessionRepository: SessionRepository
  )(
    render: UserAnswers => Future[Result]
  )(implicit ec: ExecutionContext, request: DataRequest[?]): Future[Result] = {
    val shouldMarkArrival =
      mode == CheckMode && userAnswers.get(page).forall(!_)

    if (shouldMarkArrival) {
      for {
        marked <- Future.fromTry(userAnswers.set(page, true))
        _      <- sessionRepository.set(marked)
        result <- render(marked)
      } yield result
    } else {
      render(userAnswers)
    }
  }

}
