/*
 * Copyright 2019 ACINQ SAS
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

package fr.acinq.eclair.blockchain.fee

import fr.acinq.bitcoin.scalacompat.SatoshiLong
import grizzled.slf4j.Logging

import scala.concurrent.{ExecutionContext, Future}

/**
 * This provider will try all child providers in sequence, until one of them works
 *
 * @param providers         a sequence of providers; they will be tried one after the others until one of them succeeds
 * @param minFeerate a configurable minimum value for feerates
 */
case class FallbackFeeProvider(providers: Seq[FeeProvider], minFeerate: FeeratePerByte)(implicit ec: ExecutionContext) extends FeeProvider with Logging {
  require(providers.nonEmpty, "need at least one fee provider")
  require(minFeerate.feerate > 0.sat, "minimum fee rate must be strictly greater than 0")

  def getFeerates(fallbacks: Seq[FeeProvider]): Future[FeeratesPerKw] =
    fallbacks match {
      case last +: Nil => last.getFeerates
      case head +: remaining => head.getFeerates.recoverWith { case error => logger.warn(s"$head failed, falling back to next fee provider", error); getFeerates(remaining) }
    }

  override def getFeerates: Future[FeeratesPerKw] = getFeerates(providers).map(FallbackFeeProvider.enforceMinimumFeerate(_, FeeratePerKw(minFeerate)))

}

object FallbackFeeProvider {

  private def enforceMinimumFeerate(feerates: FeeratesPerKw, minFeerate: FeeratePerKw): FeeratesPerKw = FeeratesPerKw(
    minimum = feerates.minimum.max(minFeerate),
    fastest = feerates.fastest.max(minFeerate),
    fast = feerates.fast.max(minFeerate),
    medium = feerates.medium.max(minFeerate),
    slow = feerates.slow.max(minFeerate)
  )

}
