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

import models.{Mode, UserAnswers}
import pages.QuestionPage
import play.api.libs.json.Format
import play.api.mvc.{Call, Result}
import repositories.SessionRepository

import scala.concurrent.{ExecutionContext, Future}

object CheckModeShortCircuit {

  /** Generic helper to implement the CheckMode short-circuit pattern used across controllers.
    *
    *   - If in CheckMode and the stored value for `page` equals `newValue` -> immediately redirect to `unchangedRedirect`.
    *   - Otherwise persist the new value and invoke `onSaved` to produce the resulting Future[Result].
    */
  case class ShortCircuitArgs[T](
    page: QuestionPage[T],
    newValue: T,
    mode: Mode,
    userAnswers: UserAnswers,
    sessionRepository: SessionRepository,
    unchangedRedirect: Call,
    onSaved: UserAnswers => Future[Result]
  )

  def apply[T](args: ShortCircuitArgs[T])(implicit fmt: Format[T], ec: ExecutionContext): Future[Result] = {

    import args.*

    def setAndPersist(onSaved: UserAnswers => Future[Result])(implicit
      fmt: Format[T],
      ec: ExecutionContext
    ): Future[Result] =
      for {
        updated <- Future.fromTry(userAnswers.set(page, newValue))
        _       <- sessionRepository.set(updated)
        res     <- onSaved(updated)
      } yield res

    mode match {
      case models.CheckMode =>
        userAnswers.get(page) match {
          case Some(prev) if prev == newValue =>
            Future.successful(play.api.mvc.Results.Redirect(unchangedRedirect))
          case _ =>
            setAndPersist(onSaved)
        }

      case _ =>
        setAndPersist(onSaved)
    }
  }

  /** Variant of the helper that does not persist the updated UserAnswers.
    *
    * Useful when callers need to perform additional updates and persist once.
    */
  case class ShortCircuitNoPersistArgs[T](
    page: QuestionPage[T],
    newValue: T,
    mode: Mode,
    userAnswers: UserAnswers,
    unchangedRedirect: Call,
    onSaved: UserAnswers => Future[Result]
  )

  def applyNoPersist[T](args: ShortCircuitNoPersistArgs[T])(implicit fmt: Format[T], ec: ExecutionContext): Future[Result] = {

    import args.*

    def setNoPersist(onSaved: UserAnswers => Future[Result])(implicit
      fmt: Format[T],
      ec: ExecutionContext
    ): Future[Result] =
      for {
        updated <- Future.fromTry(userAnswers.set(page, newValue))
        res     <- onSaved(updated)
      } yield res

    mode match {
      case models.CheckMode =>
        userAnswers.get(page) match {
          case Some(prev) if prev == newValue =>
            Future.successful(play.api.mvc.Results.Redirect(unchangedRedirect))
          case _ =>
            setNoPersist(onSaved)
        }

      case _ =>
        setNoPersist(onSaved)
    }
  }

}
