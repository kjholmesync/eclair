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

package fr.acinq.eclair.transactions

import fr.acinq.bitcoin.scalacompat.SatoshiLong
import fr.acinq.eclair.blockchain.fee.FeeratePerKw
import fr.acinq.eclair.transactions.Transactions.{CommitmentFormat, ZeroFeeHtlcTxAnchorOutputsCommitmentFormat}
import fr.acinq.eclair.wire.protocol._
import fr.acinq.eclair.{BlockHeight, MilliSatoshi, MilliSatoshiLong}

import scala.annotation.tailrec

/**
 * Created by PM on 07/12/2016.
 */

sealed trait CommitmentOutput

object CommitmentOutput {
  // @formatter:off
  case object ToLocal extends CommitmentOutput
  case object ToRemote extends CommitmentOutput
  case object ToLocalAnchor extends CommitmentOutput
  case object ToRemoteAnchor extends CommitmentOutput
  case class InHtlc(incomingHtlc: IncomingHtlc) extends CommitmentOutput
  case class OutHtlc(outgoingHtlc: OutgoingHtlc) extends CommitmentOutput
  // @formatter:on
}

sealed trait DirectedHtlc {
  val add: UpdateAddHtlc

  def opposite: DirectedHtlc = this match {
    case IncomingHtlc(_) => OutgoingHtlc(add)
    case OutgoingHtlc(_) => IncomingHtlc(add)
  }

  def direction: String = this match {
    case IncomingHtlc(_) => "IN"
    case OutgoingHtlc(_) => "OUT"
  }
}

object DirectedHtlc {
  def incoming: PartialFunction[DirectedHtlc, UpdateAddHtlc] = {
    case h: IncomingHtlc => h.add
  }

  def outgoing: PartialFunction[DirectedHtlc, UpdateAddHtlc] = {
    case h: OutgoingHtlc => h.add
  }
}

case class IncomingHtlc(add: UpdateAddHtlc) extends DirectedHtlc

case class OutgoingHtlc(add: UpdateAddHtlc) extends DirectedHtlc

case class LeaseSpec(amount: MilliSatoshi, owed: MilliSatoshi, expiry: BlockHeight)

/**
 * Current state of a channel's off-chain commitment, that maps to a specific on-chain commitment transaction.
 * It contains all htlcs, even those that are below dust and won't have a corresponding on-chain output.
 */
final case class CommitmentSpec(htlcs: Set[DirectedHtlc], commitTxFeerate: FeeratePerKw, toLocal: MilliSatoshi, toLocalLeased: Seq[LeaseSpec], toRemote: MilliSatoshi, toRemoteLeased: Seq[LeaseSpec]) {

  def htlcTxFeerate(commitmentFormat: CommitmentFormat): FeeratePerKw = commitmentFormat match {
    case ZeroFeeHtlcTxAnchorOutputsCommitmentFormat => FeeratePerKw(0 sat)
    case _ => commitTxFeerate
  }

  def findIncomingHtlcById(id: Long): Option[IncomingHtlc] = htlcs.collectFirst { case htlc: IncomingHtlc if htlc.add.id == id => htlc }

  def findOutgoingHtlcById(id: Long): Option[OutgoingHtlc] = htlcs.collectFirst { case htlc: OutgoingHtlc if htlc.add.id == id => htlc }

}

object CommitmentSpec {

  private def decreaseLeaseAmount(amount: MilliSatoshi, leases: Seq[LeaseSpec]): (MilliSatoshi, Seq[LeaseSpec]) = {
    if (amount > 0.msat) {
      leases.headOption match {
        case Some(lease) if lease.amount >= amount => (0.msat, lease.copy(amount = lease.amount - amount) +: leases.tail)
        case Some(lease) =>
          val (remaining, leases1) = decreaseLeaseAmount(amount - lease.amount, leases.tail)
          (remaining, lease.copy(amount = 0.msat) +: leases1)
        case None => (amount, Nil)
      }
    } else {
      (amount, leases)
    }
  }

  def addHtlc(spec: CommitmentSpec, directedHtlc: DirectedHtlc): CommitmentSpec = {
    directedHtlc match {
      case OutgoingHtlc(add) =>
        val (nonLeasedAmount, toLocalLeased1) = decreaseLeaseAmount(add.amountMsat, spec.toLocalLeased.sortBy(_.expiry))
        spec.copy(toLocal = spec.toLocal - nonLeasedAmount, toLocalLeased = toLocalLeased1, htlcs = spec.htlcs + directedHtlc)
      case IncomingHtlc(add) =>
        val (nonLeasedAmount, toRemoteLeased1) = decreaseLeaseAmount(add.amountMsat, spec.toRemoteLeased.sortBy(_.expiry))
        spec.copy(toRemote = spec.toRemote - nonLeasedAmount, toRemoteLeased = toRemoteLeased1, htlcs = spec.htlcs + directedHtlc)
    }
  }

  // TODO: for simplicity's sake, we should have rejected leases with duplicate expiries, and this function expects them ordered
  @tailrec
  private def decreaseLeaseOwed(amount: MilliSatoshi, leases: Seq[LeaseSpec]): Seq[LeaseSpec] = {
    if (amount > 0.msat) {
      leases.headOption match {
        case Some(lease) if lease.owed > amount => lease.copy(owed = lease.owed - amount) +: leases.tail
        case Some(lease) => decreaseLeaseOwed(amount - lease.owed, leases.tail)
        case None => Nil
      }
    } else {
      leases
    }
  }

  def fulfillIncomingHtlc(spec: CommitmentSpec, htlcId: Long): CommitmentSpec = {
    spec.findIncomingHtlcById(htlcId) match {
      case Some(htlc) =>
        val toRemoteLeased1 = decreaseLeaseOwed(htlc.add.amountMsat, spec.toRemoteLeased.sortBy(_.expiry))
        spec.copy(toLocal = spec.toLocal + htlc.add.amountMsat, htlcs = spec.htlcs - htlc, toRemoteLeased = toRemoteLeased1)
      case None => throw new RuntimeException(s"cannot find htlc id=$htlcId")
    }
  }

  def fulfillOutgoingHtlc(spec: CommitmentSpec, htlcId: Long): CommitmentSpec = {
    spec.findOutgoingHtlcById(htlcId) match {
      case Some(htlc) =>
        val toLocalLeased1 = decreaseLeaseOwed(htlc.add.amountMsat, spec.toLocalLeased.sortBy(_.expiry))
        spec.copy(toRemote = spec.toRemote + htlc.add.amountMsat, htlcs = spec.htlcs - htlc, toLocalLeased = toLocalLeased1)
      case None => throw new RuntimeException(s"cannot find htlc id=$htlcId")
    }
  }

  private def increaseLeaseAmount(amount: MilliSatoshi, leases: Seq[LeaseSpec]): (MilliSatoshi, Seq[LeaseSpec]) = {
    if (amount > 0.msat) {
      leases.headOption match {
        case Some(lease) if lease.amount + amount <= lease.owed => (0.msat, lease.copy(amount = lease.amount + amount) +: leases.tail)
        case Some(lease) =>
          val (remaining, leases1) = increaseLeaseAmount(amount - lease.owed + lease.amount, leases.tail)
          (remaining, lease.copy(amount = lease.owed) +: leases1)
        case None => (amount, Nil)
      }
    } else {
      (0.msat, leases)
    }
  }

  def failIncomingHtlc(spec: CommitmentSpec, htlcId: Long): CommitmentSpec = {
    spec.findIncomingHtlcById(htlcId) match {
      case Some(htlc) =>
        val (nonLeasedAmount, toRemoteLeased1) = increaseLeaseAmount(htlc.add.amountMsat, spec.toRemoteLeased.sortBy(_.expiry).reverse)
        spec.copy(toRemote = spec.toRemote + nonLeasedAmount, htlcs = spec.htlcs - htlc, toRemoteLeased = toRemoteLeased1)
      case None => throw new RuntimeException(s"cannot find htlc id=$htlcId")
    }
  }

  def failOutgoingHtlc(spec: CommitmentSpec, htlcId: Long): CommitmentSpec = {
    spec.findOutgoingHtlcById(htlcId) match {
      case Some(htlc) =>
        val (nonLeasedAmount, toLocalLeased1) = increaseLeaseAmount(htlc.add.amountMsat, spec.toLocalLeased.sortBy(_.expiry).reverse)
        spec.copy(toLocal = spec.toLocal + nonLeasedAmount, htlcs = spec.htlcs - htlc, toLocalLeased = toLocalLeased1)
      case None => throw new RuntimeException(s"cannot find htlc id=$htlcId")
    }
  }

  /**
   * Changes to a commitment are applied in batches, when we receive new signatures or revoke a previous commitment.
   * This function applies a batch of pending changes and returns the updated off-chain channel state.
   */
  def reduce(localCommitSpec: CommitmentSpec, localChanges: List[UpdateMessage], remoteChanges: List[UpdateMessage]): CommitmentSpec = {
    val spec1 = localChanges.foldLeft(localCommitSpec) {
      case (spec, u: UpdateAddHtlc) => addHtlc(spec, OutgoingHtlc(u))
      case (spec, _) => spec
    }
    val spec2 = remoteChanges.foldLeft(spec1) {
      case (spec, u: UpdateAddHtlc) => addHtlc(spec, IncomingHtlc(u))
      case (spec, _) => spec
    }
    // We must apply all fulfilled HTLCs before any failed HTLC to correctly update lease information.
    val spec3 = localChanges.foldLeft(spec2) {
      case (spec, u: UpdateFulfillHtlc) => fulfillIncomingHtlc(spec, u.id)
      case (spec, _) => spec
    }
    val spec4 = remoteChanges.foldLeft(spec3) {
      case (spec, u: UpdateFulfillHtlc) => fulfillOutgoingHtlc(spec, u.id)
      case (spec, _) => spec
    }
    val spec5 = localChanges.foldLeft(spec4) {
      case (spec, u: UpdateFailHtlc) => failIncomingHtlc(spec, u.id)
      case (spec, u: UpdateFailMalformedHtlc) => failIncomingHtlc(spec, u.id)
      case (spec, _) => spec
    }
    val spec6 = remoteChanges.foldLeft(spec5) {
      case (spec, u: UpdateFailHtlc) => failOutgoingHtlc(spec, u.id)
      case (spec, u: UpdateFailMalformedHtlc) => failOutgoingHtlc(spec, u.id)
      case (spec, _) => spec
    }
    val spec7 = (localChanges ++ remoteChanges).foldLeft(spec6) {
      case (spec, u: UpdateFee) => spec.copy(commitTxFeerate = u.feeratePerKw)
      case (spec, _) => spec
    }
    spec7
  }

}