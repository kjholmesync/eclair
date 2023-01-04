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
import fr.acinq.bitcoin.scalacompat.{ByteVector64, Crypto, Satoshi}
import fr.acinq.eclair.blockchain.fee.FeeratePerKw
import fr.acinq.eclair.payment.relay.Relayer.RelayFees
import fr.acinq.eclair.transactions.Transactions
import fr.acinq.eclair.wire.protocol.CommonCodecs.{blockHeight, millisatoshi32, publicKey, satoshi32}
import fr.acinq.eclair.wire.protocol.TlvCodecs.tmillisatoshi32
import fr.acinq.eclair.{BlockHeight, CltvExpiryDelta, MilliSatoshi, ToMilliSatoshiConversion}
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

  /** Liquidity leases are valid for a fixed duration, after which they must be renewed. */
  val LeaseDuration = CltvExpiryDelta(1008) // 1 week

  /**
   * Liquidity is leased using the following rates:
   *
   *  - the buyer pays [[leaseFeeBase]] regardless of the amount contributed by the seller
   *  - the buyer pays [[leaseFeeProportional]] (expressed in basis points) of the amount contributed by the seller
   *  - the buyer refunds the on-chain fees for up to [[fundingWeight]] of the utxos contributed by the seller
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
  }

  object LeaseRates {
    def apply(fundingWeight: Int, leaseFeeBase: Satoshi, leaseFeeProportional: Int, relayFees: RelayFees): LeaseRates = {
      LeaseRates(fundingWeight, leaseFeeProportional, (relayFees.feeProportionalMillionths / 100).toInt, leaseFeeBase, relayFees.feeBase)
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

  private val leaseWitnessCodec: Codec[LeaseWitness] = (
    ("tag" | constant(ByteVector("option_will_fund".getBytes(StandardCharsets.US_ASCII)))) ::
      ("funding_pubkey" | publicKey) ::
      ("lease_end" | blockHeight) ::
      ("lease_duration" | uint32.xmap(l => l.toInt, (i: Int) => i.toLong)) ::
      ("channel_fee_max_basis" | uint16) ::
      ("channel_fee_max_base_msat" | millisatoshi32)
    ).as[LeaseWitness]

}
