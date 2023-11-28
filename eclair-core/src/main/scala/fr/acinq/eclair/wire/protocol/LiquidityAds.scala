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

package fr.acinq.eclair.wire.protocol

import fr.acinq.bitcoin.scalacompat.Crypto.{PrivateKey, PublicKey}
import fr.acinq.bitcoin.scalacompat.{ByteVector32, ByteVector64, Crypto, Satoshi, SatoshiLong}
import fr.acinq.eclair.blockchain.fee.FeeratePerKw
import fr.acinq.eclair.channel.{ChannelException, InvalidLiquidityAdsSig, LiquidityRatesRejected, MissingLiquidityAds}
import fr.acinq.eclair.payment.relay.Relayer.RelayFees
import fr.acinq.eclair.transactions.Transactions
import fr.acinq.eclair.wire.protocol.CommonCodecs.{blockHeight, millisatoshi32, publicKey, satoshi32}
import fr.acinq.eclair.wire.protocol.TlvCodecs.tmillisatoshi32
import fr.acinq.eclair.{BlockHeight, MilliSatoshi, ToMilliSatoshiConversion}
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs._

import java.nio.charset.StandardCharsets

/**
 * Created by t-bast on 02/01/2023.
 */

/**
 * Liquidity ads create a decentralized market for channel liquidity.
 * Nodes advertise fee rates for their available liquidity using the gossip protocol.
 * Other nodes can then pay the advertised rate to get inbound liquidity allocated towards them.
 */
object LiquidityAds {

  val DEFAULT_LEASE_DURATION = 4032 // ~1 month

  case class Config(feeBase: Satoshi, feeProportional: Int, maxLeaseDuration: Int) {
    def leaseRates(relayFees: RelayFees): LeaseRates = {
      // We make the remote node pay for one p2wpkh input and one p2wpkh output.
      // If we need more inputs, we will pay the fees for those additional inputs ourselves.
      LeaseRates(Transactions.claimP2WPKHOutputWeight, feeProportional, (relayFees.feeProportionalMillionths / 100).toInt, feeBase, relayFees.feeBase)
    }
  }

  case class RequestRemoteFundingParams(fundingAmount: Satoshi, leaseDuration: Int, maxFee: Satoshi) {
    def withLeaseStart(leaseStart: BlockHeight): RequestRemoteFunding = RequestRemoteFunding(fundingAmount, maxFee, leaseStart, leaseDuration)
  }

  /** Request inbound liquidity from a remote peer that supports liquidity ads. */
  case class RequestRemoteFunding(fundingAmount: Satoshi, maxFee: Satoshi, leaseStart: BlockHeight, leaseDuration: Int) {
    private val leaseExpiry: BlockHeight = leaseStart + leaseDuration
    val requestFunds: ChannelTlv.RequestFunds = ChannelTlv.RequestFunds(fundingAmount, leaseExpiry, leaseDuration)

    def validateLeaseRates(remoteNodeId: PublicKey,
                           channelId: ByteVector32,
                           remoteFundingPubKey: PublicKey,
                           remoteFundingAmount: Satoshi,
                           fundingFeerate: FeeratePerKw,
                           willFund_opt: Option[ChannelTlv.WillFund]): Either[ChannelException, Lease] = {
      willFund_opt match {
        case Some(willFund) =>
          val witness = LeaseWitness(remoteFundingPubKey, leaseExpiry, leaseDuration, willFund.leaseRates)
          val fees = willFund.leaseRates.fees(fundingFeerate, fundingAmount, remoteFundingAmount)
          if (!LeaseWitness.verify(remoteNodeId, willFund.sig, witness)) {
            Left(InvalidLiquidityAdsSig(channelId))
          } else if (remoteFundingAmount <= 0.sat) {
            Left(LiquidityRatesRejected(channelId))
          } else if (maxFee < fees) {
            Left(LiquidityRatesRejected(channelId))
          } else {
            val leaseAmount = fundingAmount.min(remoteFundingAmount)
            Right(Lease(leaseAmount, fees, willFund.sig, witness))
          }
        case None =>
          // If the remote peer doesn't want to provide inbound liquidity, we immediately fail the attempt.
          // The node operator may retry this funding attempt without requesting inbound liquidity.
          Left(MissingLiquidityAds(channelId))
      }
    }
  }

  def validateLeaseRates_opt(remoteNodeId: PublicKey,
                             channelId: ByteVector32,
                             remoteFundingPubKey: PublicKey,
                             remoteFundingAmount: Satoshi,
                             fundingFeerate: FeeratePerKw,
                             willFund_opt: Option[ChannelTlv.WillFund],
                             requestRemoteFunding_opt: Option[RequestRemoteFunding]): Either[ChannelException, Option[Lease]] = {
    requestRemoteFunding_opt match {
      case Some(requestRemoteFunding) => requestRemoteFunding.validateLeaseRates(remoteNodeId, channelId, remoteFundingPubKey, remoteFundingAmount, fundingFeerate, willFund_opt) match {
        case Left(t) => Left(t)
        case Right(lease) => Right(Some(lease))
      }
      case None => Right(None)
    }
  }

  /** We propose adding funds to a channel for an optional fee at the given rates. */
  case class AddFunding(fundingAmount: Satoshi, rates_opt: Option[LeaseRates]) {
    def signLease(nodeKey: PrivateKey,
                  currentBlockHeight: BlockHeight,
                  localFundingPubKey: PublicKey,
                  fundingFeerate: FeeratePerKw,
                  requestFunds_opt: Option[ChannelTlv.RequestFunds]): Option[WillFundLease] = {
      rates_opt.flatMap(_.signLease(nodeKey, currentBlockHeight, localFundingPubKey, fundingFeerate, requestFunds_opt, Some(fundingAmount)))
    }
  }

  /**
   * Liquidity is leased using the following rates:
   *
   *  - the buyer pays [[leaseFeeBase]] regardless of the amount contributed by the seller
   *  - the buyer pays [[leaseFeeProportional]] (expressed in basis points) of the amount contributed by the seller
   *  - the buyer refunds the on-chain fees for up to [[fundingWeight]] of the inputs/outputs contributed by the seller
   *
   * The seller promises that their relay fees towards the buyer will never exceed [[maxRelayFeeBase]] and [[maxRelayFeeProportional]].
   * This cannot be enforced, but if the buyer notices the seller cheating, they should blacklist them and can prove
   * that they misbehaved.
   */
  case class LeaseRates(fundingWeight: Int, leaseFeeProportional: Int, maxRelayFeeProportional: Int, leaseFeeBase: Satoshi, maxRelayFeeBase: MilliSatoshi) {
    val maxRelayFeeProportionalMillionths: Long = maxRelayFeeProportional.toLong * 100

    /**
     * Fees paid by the liquidity buyer: the resulting amount must be added to the seller's output in the corresponding
     * commitment transaction.
     */
    def fees(feerate: FeeratePerKw, requestedAmount: Satoshi, contributedAmount: Satoshi): Satoshi = {
      val onChainFees = Transactions.weight2feeMsat(feerate, fundingWeight)
      // If the seller adds more liquidity than requested, the buyer doesn't pay for that extra liquidity.
      val proportionalFee = requestedAmount.min(contributedAmount).toMilliSatoshi * leaseFeeProportional / 10_000
      leaseFeeBase + (proportionalFee + onChainFees).truncateToSatoshi
    }

    /**
     * @param fundingAmountOverride_opt this field should be provided if we contribute a different amount than what was requested.
     */
    def signLease(nodeKey: PrivateKey,
                  currentBlockHeight: BlockHeight,
                  localFundingPubKey: PublicKey,
                  fundingFeerate: FeeratePerKw,
                  requestFunds_opt: Option[ChannelTlv.RequestFunds],
                  fundingAmountOverride_opt: Option[Satoshi] = None): Option[WillFundLease] = {
      requestFunds_opt.flatMap(requestFunds => {
        if (currentBlockHeight + 12 < requestFunds.leaseExpiry - requestFunds.leaseDuration) {
          // They're trying to cheat and pay for a smaller duration than what will actually be enforced.
          None
        } else {
          val witness = LeaseWitness(localFundingPubKey, requestFunds.leaseExpiry, requestFunds.leaseDuration, this)
          val sig = LeaseWitness.sign(nodeKey, witness)
          val fundingAmount = fundingAmountOverride_opt.getOrElse(requestFunds.amount)
          val leaseFees = fees(fundingFeerate, requestFunds.amount, fundingAmount)
          val leaseAmount = fundingAmount.min(requestFunds.amount)
          Some(WillFundLease(ChannelTlv.WillFund(sig, this), Lease(leaseAmount, leaseFees, sig, witness)))
        }
      })
    }
  }

  val leaseRatesCodec: Codec[LeaseRates] = (
    ("funding_weight" | uint16) ::
      ("lease_fee_basis" | uint16) ::
      ("channel_fee_basis_max" | uint16) ::
      ("lease_fee_base_sat" | satoshi32) ::
      ("channel_fee_max_base_msat" | tmillisatoshi32)
    ).as[LeaseRates]

  /** The seller signs the lease parameters: if they cheat, the buyer can use that signature to prove they cheated. */
  case class LeaseWitness(fundingPubKey: PublicKey, leaseEnd: BlockHeight, leaseDuration: Int, maxRelayFeeProportional: Int, maxRelayFeeBase: MilliSatoshi)

  object LeaseWitness {
    def apply(fundingPubKey: PublicKey, leaseEnd: BlockHeight, leaseDuration: Int, leaseRates: LeaseRates): LeaseWitness = {
      LeaseWitness(fundingPubKey, leaseEnd, leaseDuration, leaseRates.maxRelayFeeProportional, leaseRates.maxRelayFeeBase)
    }

    def sign(nodeKey: PrivateKey, witness: LeaseWitness): ByteVector64 = {
      Crypto.sign(Crypto.sha256(leaseWitnessCodec.encode(witness).require.bytes), nodeKey)
    }

    def verify(nodeId: PublicKey, sig: ByteVector64, witness: LeaseWitness): Boolean = {
      Crypto.verifySignature(Crypto.sha256(leaseWitnessCodec.encode(witness).require.bytes), sig, nodeId)
    }
  }

  val leaseWitnessCodec: Codec[LeaseWitness] = (
    ("tag" | constant(ByteVector("option_will_fund".getBytes(StandardCharsets.US_ASCII)))) ::
      ("funding_pubkey" | publicKey) ::
      ("lease_end" | blockHeight) ::
      ("lease_duration" | uint32.xmap(l => l.toInt, (i: Int) => i.toLong)) ::
      ("channel_fee_max_basis" | uint16) ::
      ("channel_fee_max_base_msat" | millisatoshi32)
    ).as[LeaseWitness]

  /**
   * Once a liquidity ads has been paid, we should keep track of the lease, and check that our peer doesn't raise their
   * routing fees above the values they signed up for.
   */
  case class Lease(amount: Satoshi, fees: Satoshi, sellerSig: ByteVector64, witness: LeaseWitness) {
    val expiry: BlockHeight = witness.leaseEnd
  }

  case class WillFundLease(willFund: ChannelTlv.WillFund, lease: Lease)

  case class LiquidityPurchased(isBuyer: Boolean, lease: Lease)

}
