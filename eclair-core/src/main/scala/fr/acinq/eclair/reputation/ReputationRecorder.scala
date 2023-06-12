/*
 * Copyright 2023 ACINQ SAS
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

package fr.acinq.eclair.reputation

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import fr.acinq.bitcoin.scalacompat.Crypto.PublicKey
import fr.acinq.eclair.reputation.Reputation.ReputationConfig
import fr.acinq.eclair.{MilliSatoshi, NodeParams}

import java.util.UUID

object ReputationRecorder {
  sealed trait Command
  case class GetConfidence(replyTo: ActorRef[Confidence], originNode: PublicKey, isEndorsed: Boolean) extends Command
  case class AttemptRelay(originNode: PublicKey, isEndorsed: Boolean, relayId: UUID, fee: MilliSatoshi) extends Command
  case class CancelRelay(originNode: PublicKey, isEndorsed: Boolean, relayId: UUID) extends Command
  case class RecordResult(originNode: PublicKey, isEndorsed: Boolean, relayId: UUID, isSuccess: Boolean) extends Command

  case class Confidence(value: Double)

  def apply(reputationConfig: ReputationConfig, reputations: Map[(PublicKey, Boolean), Reputation]): Behavior[Command] = {
    Behaviors.receiveMessage {
      case GetConfidence(replyTo, originNode, isEndorsed) =>
        replyTo ! Confidence(reputations.get((originNode, isEndorsed)).map(_.confidence()).getOrElse(0.0))
        Behaviors.same
      case AttemptRelay(originNode, isEndorsed, relayId, fee) =>
        val updatedReputation = reputations.getOrElse((originNode, isEndorsed), Reputation.init(reputationConfig)).attempt(relayId, fee)
        ReputationRecorder(reputationConfig, reputations.updated((originNode, isEndorsed), updatedReputation))
      case CancelRelay(originNode, isEndorsed, relayId) =>
        val updatedReputation = reputations.getOrElse((originNode, isEndorsed), Reputation.init(reputationConfig)).cancel(relayId)
        ReputationRecorder(reputationConfig, reputations.updated((originNode, isEndorsed), updatedReputation))
      case RecordResult(originNode, isEndorsed, relayId, isSuccess) =>
        val updatedReputation = reputations.getOrElse((originNode, isEndorsed), Reputation.init(reputationConfig)).record(relayId, isSuccess)
        ReputationRecorder(reputationConfig, reputations.updated((originNode, isEndorsed), updatedReputation))
    }
  }
}
