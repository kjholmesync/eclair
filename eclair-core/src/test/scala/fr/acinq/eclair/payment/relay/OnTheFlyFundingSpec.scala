/*
 * Copyright 2024 ACINQ SAS
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

package fr.acinq.eclair.payment.relay

import akka.actor.typed.scaladsl.adapter._
import akka.actor.{ActorContext, ActorRef}
import akka.testkit.{TestFSMRef, TestProbe}
import com.softwaremill.quicklens.ModifyPimp
import fr.acinq.bitcoin.scalacompat.Crypto.PublicKey
import fr.acinq.bitcoin.scalacompat.{Block, ByteVector32, Crypto, Satoshi, SatoshiLong, TxId}
import fr.acinq.eclair.blockchain.{CurrentBlockHeight, DummyOnChainWallet}
import fr.acinq.eclair.channel.Upstream.Hot
import fr.acinq.eclair.channel._
import fr.acinq.eclair.crypto.Sphinx
import fr.acinq.eclair.io.Peer._
import fr.acinq.eclair.io.PendingChannelsRateLimiter.AddOrRejectChannel
import fr.acinq.eclair.io.{Peer, PeerConnection, PendingChannelsRateLimiter}
import fr.acinq.eclair.wire.protocol
import fr.acinq.eclair.wire.protocol._
import fr.acinq.eclair.{Alias, BlockHeight, CltvExpiry, CltvExpiryDelta, FeatureSupport, Features, MilliSatoshi, MilliSatoshiLong, NodeParams, TestConstants, TestKitBaseClass, TimestampMilli, ToMilliSatoshiConversion, UInt64, randomBytes, randomBytes32, randomBytes64, randomKey, randomLong}
import org.scalatest.Outcome
import org.scalatest.funsuite.FixtureAnyFunSuiteLike

import java.util.UUID
import scala.concurrent.duration.DurationInt

class OnTheFlyFundingSpec extends TestKitBaseClass with FixtureAnyFunSuiteLike {

  import OnTheFlyFundingSpec._

  val remoteFeatures = Features(
    Features.DualFunding -> FeatureSupport.Optional,
    Features.SplicePrototype -> FeatureSupport.Optional,
    Features.OnTheFlyFunding -> FeatureSupport.Optional,
  )

  case class FixtureParam(nodeParams: NodeParams, remoteNodeId: PublicKey, peer: TestFSMRef[Peer.State, Peer.Data, Peer], peerConnection: TestProbe, channel: TestProbe, register: TestProbe, rateLimiter: TestProbe, probe: TestProbe) {
    def connect(peer: TestFSMRef[Peer.State, Peer.Data, Peer], remoteInit: protocol.Init = protocol.Init(remoteFeatures.initFeatures()), channelCount: Int = 0): Unit = {
      val localInit = protocol.Init(nodeParams.features.initFeatures())
      val address = NodeAddress.fromParts("0.0.0.0", 9735).get
      peer ! PeerConnection.ConnectionReady(peerConnection.ref, remoteNodeId, address, outgoing = true, localInit, remoteInit)
      peerConnection.expectMsgType[RecommendedFeerates]
      (0 until channelCount).foreach(_ => channel.expectMsgType[INPUT_RECONNECTED])
      probe.send(peer, Peer.GetPeerInfo(Some(probe.ref.toTyped)))
      val peerInfo = probe.expectMsgType[Peer.PeerInfo]
      assert(peerInfo.nodeId == remoteNodeId)
      assert(peerInfo.state == Peer.CONNECTED)
    }

    def openChannel(fundingAmount: Satoshi): ByteVector32 = {
      peer ! Peer.OpenChannel(remoteNodeId, fundingAmount, None, None, None, None, None, None, None)
      val temporaryChannelId = channel.expectMsgType[INPUT_INIT_CHANNEL_INITIATOR].temporaryChannelId
      val channelId = randomBytes32()
      peer ! ChannelIdAssigned(channel.ref, remoteNodeId, temporaryChannelId, channelId)
      peerConnection.expectMsgType[PeerConnection.DoSync]
      channelId
    }

    def disconnect(channelCount: Int = 0): Unit = {
      peer ! Peer.ConnectionDown(peerConnection.ref)
      (0 until channelCount).foreach(_ => channel.expectMsg(INPUT_DISCONNECTED))
    }

    def createProposal(amount: MilliSatoshi, expiry: CltvExpiry, paymentHash: ByteVector32 = randomBytes32(), upstream: Upstream.Hot = Upstream.Local(UUID.randomUUID())): ProposeOnTheFlyFunding = {
      val blindingKey = upstream match {
        case u: Upstream.Hot.Channel if u.add.blinding_opt.nonEmpty => Some(randomKey().publicKey)
        case u: Upstream.Hot.Trampoline if u.received.exists(_.add.blinding_opt.nonEmpty) => Some(randomKey().publicKey)
        case _ => None
      }
      ProposeOnTheFlyFunding(probe.ref, amount, paymentHash, expiry, TestConstants.emptyOnionPacket, blindingKey, upstream)
    }

    def proposeFunding(amount: MilliSatoshi, expiry: CltvExpiry, paymentHash: ByteVector32 = randomBytes32(), upstream: Upstream.Hot = Upstream.Local(UUID.randomUUID())): WillAddHtlc = {
      val proposal = createProposal(amount, expiry, paymentHash, upstream)
      peer ! proposal
      val willAdd = peerConnection.expectMsgType[WillAddHtlc]
      assert(willAdd.amount == amount)
      assert(willAdd.expiry == expiry)
      assert(willAdd.paymentHash == paymentHash)
      probe.expectMsg(ProposeOnTheFlyFundingResponse.Proposed)
      willAdd
    }

    def signLease(amount: Satoshi,
                  paymentDetails: LiquidityAds.PaymentDetails,
                  channelId: ByteVector32 = randomBytes32(),
                  fees: LiquidityAds.LeaseFees = LiquidityAds.LeaseFees(0 sat, 0 sat),
                  fundingTxIndex: Long = 0,
                  htlcMinimum: MilliSatoshi = 1 msat): LiquidityLeaseSigned = {
      val lease = LiquidityAds.Lease(amount, fees, paymentDetails, randomBytes64(), LiquidityAds.FundingLeaseWitness.Basic(randomBytes(33)))
      val leaseSigned = LiquidityLeaseSigned(channelId, TxId(randomBytes32()), fundingTxIndex, htlcMinimum, lease)
      peer ! leaseSigned
      leaseSigned
    }

    def makeChannelData(htlcMinimum: MilliSatoshi = 1 msat, localChanges: LocalChanges = LocalChanges(Nil, Nil, Nil)): DATA_NORMAL = {
      val commitments = CommitmentsSpec.makeCommitments(500_000_000 msat, 500_000_000 msat, nodeParams.nodeId, remoteNodeId, announceChannel = false)
        .modify(_.params.remoteParams.htlcMinimum).setTo(htlcMinimum)
        .modify(_.changes.localChanges).setTo(localChanges)
      DATA_NORMAL(commitments, ShortIds(RealScidStatus.Unknown, Alias(42), None), None, null, None, None, None, SpliceStatus.NoSplice)
    }
  }

  case class FakeChannelFactory(remoteNodeId: PublicKey, channel: TestProbe) extends ChannelFactory {
    override def spawn(context: ActorContext, remoteNodeId: PublicKey): ActorRef = {
      assert(remoteNodeId == remoteNodeId)
      channel.ref
    }
  }

  override protected def withFixture(test: OneArgTest): Outcome = {
    val nodeParams = TestConstants.Alice.nodeParams
      .modify(_.features.activated).using(_ + (Features.DualFunding -> FeatureSupport.Optional))
      .modify(_.features.activated).using(_ + (Features.SplicePrototype -> FeatureSupport.Optional))
      .modify(_.features.activated).using(_ + (Features.OnTheFlyFunding -> FeatureSupport.Optional))
    val remoteNodeId = randomKey().publicKey
    val register = TestProbe()
    val channel = TestProbe()
    val peerConnection = TestProbe()
    val rateLimiter = TestProbe()
    val probe = TestProbe()
    val peer = TestFSMRef(new Peer(nodeParams, remoteNodeId, new DummyOnChainWallet(), FakeChannelFactory(remoteNodeId, channel), TestProbe().ref, register.ref, TestProbe().ref, rateLimiter.ref))
    peer ! Peer.Init(Set.empty)
    withFixture(test.toNoArgTest(FixtureParam(nodeParams, remoteNodeId, peer, peerConnection, channel, register, rateLimiter, probe)))
  }

  test("ignore requests when peer doesn't support on-the-fly funding") { f =>
    import f._

    connect(peer, remoteInit = protocol.Init(Features.empty))
    peer ! createProposal(100_000_000 msat, CltvExpiry(561))
    probe.expectMsgType[ProposeOnTheFlyFundingResponse.NotAvailable]
  }

  test("ignore requests when disconnected") { f =>
    import f._

    peer ! createProposal(100_000_000 msat, CltvExpiry(561))
    probe.expectMsgType[ProposeOnTheFlyFundingResponse.NotAvailable]
  }

  test("receive remote failure") { f =>
    import f._

    connect(peer)

    val paymentHash = randomBytes32()
    val upstream1 = upstreamChannel(75_000_000 msat, CltvExpiry(561), paymentHash)
    val willAdd1 = proposeFunding(70_000_000 msat, CltvExpiry(550), paymentHash, upstream1)
    val upstream2 = upstreamChannel(80_000_000 msat, CltvExpiry(561), paymentHash, blinded = true)
    val willAdd2 = proposeFunding(75_000_000 msat, CltvExpiry(550), paymentHash, upstream2)
    val upstream3 = upstreamChannel(50_000_000 msat, CltvExpiry(561), paymentHash)
    val willAdd3 = proposeFunding(50_000_000 msat, CltvExpiry(550), paymentHash, upstream3)
    val upstream4 = Upstream.Hot.Trampoline(List(
      upstreamChannel(50_000_000 msat, CltvExpiry(561), paymentHash),
      upstreamChannel(50_000_000 msat, CltvExpiry(561), paymentHash),
    ))
    val willAdd4 = proposeFunding(100_000_000 msat, CltvExpiry(550), paymentHash, upstream4)
    val upstream5 = Upstream.Hot.Trampoline(List(
      upstreamChannel(50_000_000 msat, CltvExpiry(561), paymentHash, blinded = true),
      upstreamChannel(50_000_000 msat, CltvExpiry(561), paymentHash, blinded = true),
    ))
    val willAdd5 = proposeFunding(100_000_000 msat, CltvExpiry(550), paymentHash, upstream5)

    val fail1 = WillFailHtlc(willAdd1.id, paymentHash, randomBytes(42))
    peerConnection.send(peer, fail1)
    val fwd1 = register.expectMsgType[Register.Forward[CMD_FAIL_HTLC]]
    assert(fwd1.channelId == upstream1.add.channelId)
    assert(fwd1.message.id == upstream1.add.id)
    assert(fwd1.message.reason == Left(fail1.reason))
    register.expectNoMessage(100 millis)

    val fail2 = WillFailHtlc(willAdd2.id, paymentHash, randomBytes(50))
    peerConnection.send(peer, fail2)
    val fwd2 = register.expectMsgType[Register.Forward[CMD_FAIL_HTLC]]
    assert(fwd2.channelId == upstream2.add.channelId)
    assert(fwd2.message.id == upstream2.add.id)
    assert(fwd2.message.reason == Right(InvalidOnionBlinding(Sphinx.hash(upstream2.add.onionRoutingPacket))))

    val fail3 = WillFailMalformedHtlc(willAdd3.id, paymentHash, randomBytes32(), InvalidOnionHmac(randomBytes32()).code)
    peerConnection.send(peer, fail3)
    val fwd3 = register.expectMsgType[Register.Forward[CMD_FAIL_HTLC]]
    assert(fwd3.channelId == upstream3.add.channelId)
    assert(fwd3.message.id == upstream3.add.id)
    assert(fwd3.message.reason == Right(InvalidOnionHmac(fail3.onionHash)))

    val fail4 = WillFailHtlc(willAdd4.id, paymentHash, randomBytes(75))
    peerConnection.send(peer, fail4)
    upstream4.received.map(_.add).foreach(add => {
      val fwd = register.expectMsgType[Register.Forward[CMD_FAIL_HTLC]]
      assert(fwd.channelId == add.channelId)
      assert(fwd.message.id == add.id)
      assert(fwd.message.reason == Right(TemporaryNodeFailure()))
    })

    val fail5 = WillFailHtlc(willAdd5.id, paymentHash, randomBytes(75))
    peerConnection.send(peer, fail5)
    upstream5.received.map(_.add).foreach(add => {
      val fwd = register.expectMsgType[Register.Forward[CMD_FAIL_HTLC]]
      assert(fwd.channelId == add.channelId)
      assert(fwd.message.id == add.id)
      assert(fwd.message.reason == Right(TemporaryNodeFailure()))
    })
  }

  test("proposed on-the-fly funding timeout") { f =>
    import f._

    connect(peer)

    // A first funding is proposed coming from two upstream channels.
    val paymentHash1 = randomBytes32()
    val upstream1 = Seq(
      upstreamChannel(60_000_000 msat, CltvExpiry(561), paymentHash1, blinded = true),
      upstreamChannel(45_000_000 msat, CltvExpiry(561), paymentHash1, blinded = true),
    )
    proposeFunding(50_000_000 msat, CltvExpiry(550), paymentHash1, upstream1.head)
    proposeFunding(40_000_000 msat, CltvExpiry(550), paymentHash1, upstream1.last)

    // A second funding is proposed coming from a trampoline payment.
    val paymentHash2 = randomBytes32()
    val upstream2 = Upstream.Hot.Trampoline(List(
      upstreamChannel(60_000_000 msat, CltvExpiry(561), paymentHash2),
      upstreamChannel(45_000_000 msat, CltvExpiry(561), paymentHash2),
    ))
    proposeFunding(100_000_000 msat, CltvExpiry(550), paymentHash2, upstream2)

    // A third funding is signed coming from a trampoline payment.
    val paymentHash3 = randomBytes32()
    val upstream3 = Upstream.Hot.Trampoline(List(
      upstreamChannel(60_000_000 msat, CltvExpiry(561), paymentHash3),
      upstreamChannel(45_000_000 msat, CltvExpiry(561), paymentHash3),
    ))
    proposeFunding(100_000_000 msat, CltvExpiry(550), paymentHash3, upstream3)
    signLease(100_000 sat, LiquidityAds.PaymentDetails.FromChannelBalanceForFutureHtlc(paymentHash3 :: Nil))

    // The funding timeout is reached, unsigned proposals are failed upstream.
    peer ! OnTheFlyFundingTimeout(paymentHash1)
    upstream1.foreach(u => {
      val fwd = register.expectMsgType[Register.Forward[CMD_FAIL_HTLC]]
      assert(fwd.channelId == u.add.channelId)
      assert(fwd.message.id == u.add.id)
      assert(fwd.message.reason == Right(InvalidOnionBlinding(Sphinx.hash(u.add.onionRoutingPacket))))
      assert(fwd.message.commit)
    })
    peerConnection.expectMsgType[Warning]

    peer ! OnTheFlyFundingTimeout(paymentHash2)
    upstream2.received.foreach(u => {
      val fwd = register.expectMsgType[Register.Forward[CMD_FAIL_HTLC]]
      assert(fwd.channelId == u.add.channelId)
      assert(fwd.message.id == u.add.id)
      assert(fwd.message.reason == Right(UnknownNextPeer()))
      assert(fwd.message.commit)
    })
    peerConnection.expectMsgType[Warning]

    peer ! OnTheFlyFundingTimeout(paymentHash3)
    register.expectNoMessage(100 millis)
    peerConnection.expectNoMessage(100 millis)
  }

  test("proposed on-the-fly funding HTLC timeout") { f =>
    import f._

    connect(peer)

    // A first funding is proposed coming from two upstream channels.
    val paymentHash1 = randomBytes32()
    val upstream1 = Seq(
      upstreamChannel(60_000_000 msat, CltvExpiry(560), paymentHash1),
      upstreamChannel(45_000_000 msat, CltvExpiry(550), paymentHash1),
    )
    proposeFunding(50_000_000 msat, CltvExpiry(520), paymentHash1, upstream1.head)
    proposeFunding(40_000_000 msat, CltvExpiry(510), paymentHash1, upstream1.last)

    // A second funding is signed coming from two upstream channels, one of them received after signing.
    val paymentHash2 = randomBytes32()
    val upstream2 = Seq(
      upstreamChannel(45_000_000 msat, CltvExpiry(550), paymentHash2),
      upstreamChannel(60_000_000 msat, CltvExpiry(560), paymentHash2),
    )
    proposeFunding(40_000_000 msat, CltvExpiry(515), paymentHash2, upstream2.head)
    signLease(100_000 sat, LiquidityAds.PaymentDetails.FromFutureHtlc(paymentHash2 :: Nil))
    proposeFunding(50_000_000 msat, CltvExpiry(525), paymentHash2, upstream2.last)

    // A third funding is signed coming from a trampoline payment.
    val paymentHash3 = randomBytes32()
    val upstream3 = Upstream.Hot.Trampoline(List(
      upstreamChannel(60_000_000 msat, CltvExpiry(560), paymentHash3),
      upstreamChannel(45_000_000 msat, CltvExpiry(560), paymentHash3),
    ))
    proposeFunding(100_000_000 msat, CltvExpiry(512), paymentHash3, upstream3)
    signLease(100_000 sat, LiquidityAds.PaymentDetails.FromChannelBalanceForFutureHtlc(paymentHash3 :: Nil))

    // A fourth funding is proposed coming from a trampoline payment.
    val paymentHash4 = randomBytes32()
    val upstream4 = Upstream.Hot.Trampoline(List(upstreamChannel(60_000_000 msat, CltvExpiry(560), paymentHash4)))
    proposeFunding(50_000_000 msat, CltvExpiry(516), paymentHash4, upstream4)

    // The first three proposals reach their CLTV expiry.
    peer ! CurrentBlockHeight(BlockHeight(515))
    val fwds = (0 until 6).map(_ => register.expectMsgType[Register.Forward[CMD_FAIL_HTLC]])
    register.expectNoMessage(100 millis)
    fwds.foreach(fwd => {
      assert(fwd.message.reason == Right(UnknownNextPeer()))
      assert(fwd.message.commit)
    })
    assert(fwds.map(_.channelId).toSet == (upstream1 ++ upstream2 ++ upstream3.received).map(_.add.channelId).toSet)
    assert(fwds.map(_.message.id).toSet == (upstream1 ++ upstream2 ++ upstream3.received).map(_.add.id).toSet)
    awaitCond(nodeParams.db.onTheFlyFunding.listPending(remoteNodeId).isEmpty, interval = 100 millis)
  }

  test("signed on-the-fly funding HTLC timeout after disconnection") { f =>
    import f._

    connect(peer)
    probe.watch(peer.ref)

    // A first funding proposal is signed.
    val upstream1 = upstreamChannel(60_000_000 msat, CltvExpiry(560))
    proposeFunding(50_000_000 msat, CltvExpiry(520), upstream1.add.paymentHash, upstream1)
    signLease(75_000 sat, LiquidityAds.PaymentDetails.FromFutureHtlc(upstream1.add.paymentHash :: Nil))

    // A second funding proposal is signed.
    val upstream2 = upstreamChannel(60_000_000 msat, CltvExpiry(560))
    proposeFunding(50_000_000 msat, CltvExpiry(525), upstream2.add.paymentHash, upstream2)
    signLease(80_000 sat, LiquidityAds.PaymentDetails.FromFutureHtlc(upstream2.add.paymentHash :: Nil))

    // We don't fail signed proposals on disconnection.
    disconnect()
    register.expectNoMessage(100 millis)

    // But if a funding proposal reaches its CLTV expiry, we fail it.
    peer ! CurrentBlockHeight(BlockHeight(522))
    val fwd1 = register.expectMsgType[Register.Forward[CMD_FAIL_HTLC]]
    assert(fwd1.channelId == upstream1.add.channelId)
    assert(fwd1.message.id == upstream1.add.id)
    register.expectNoMessage(100 millis)
    // We still have one pending proposal, so we don't stop.
    probe.expectNoMessage(100 millis)

    // When restarting, we watch for pending proposals.
    val peerAfterRestart = TestFSMRef(new Peer(nodeParams, remoteNodeId, new DummyOnChainWallet(), FakeChannelFactory(remoteNodeId, channel), TestProbe().ref, register.ref, TestProbe().ref, TestProbe().ref))
    peerAfterRestart ! Peer.Init(Set.empty)
    probe.watch(peerAfterRestart.ref)

    // The last funding proposal reaches its CLTV expiry.
    peerAfterRestart ! CurrentBlockHeight(BlockHeight(525))
    val fwd2 = register.expectMsgType[Register.Forward[CMD_FAIL_HTLC]]
    assert(fwd2.channelId == upstream2.add.channelId)
    assert(fwd2.message.id == upstream2.add.id)
    register.expectNoMessage(100 millis)
    probe.expectTerminated(peerAfterRestart.ref)
  }

  test("receive open_channel2") { f =>
    import f._

    connect(peer)

    val upstream = upstreamChannel(60_000_000 msat, CltvExpiry(560), paymentHash)
    proposeFunding(50_000_000 msat, CltvExpiry(520), paymentHash, upstream)

    val requestFunding = LiquidityAds.RequestFunding(
      100_000 sat,
      LiquidityAds.FundingLease.Basic(10_000 sat, 500_000 sat, 0, 100, 1000 sat),
      LiquidityAds.PaymentDetails.FromFutureHtlcWithPreimage(preimage :: Nil)
    )
    val open = createOpenChannelMessage(requestFunding)
    peerConnection.send(peer, open)
    rateLimiter.expectMsgType[AddOrRejectChannel].replyTo ! PendingChannelsRateLimiter.AcceptOpenChannel
    val init = channel.expectMsgType[INPUT_INIT_CHANNEL_NON_INITIATOR]
    assert(!init.localParams.isChannelOpener)
    assert(init.localParams.paysCommitTxFees)
    assert(init.fundingContribution_opt.contains(LiquidityAds.AddFunding(requestFunding.requestedAmount, nodeParams.willFundRates_opt)))

    // The preimage was provided, so we fulfill upstream HTLCs.
    val fwd = register.expectMsgType[Register.Forward[CMD_FULFILL_HTLC]]
    assert(fwd.channelId == upstream.add.channelId)
    assert(fwd.message.id == upstream.add.id)
    assert(fwd.message.r == preimage)
  }

  test("receive splice_init") { f =>
    import f._

    connect(peer)
    val channelId = openChannel(200_000 sat)

    val upstream = upstreamChannel(60_000_000 msat, CltvExpiry(560), paymentHash)
    proposeFunding(50_000_000 msat, CltvExpiry(520), paymentHash, upstream)

    val requestFunding = LiquidityAds.RequestFunding(
      100_000 sat,
      LiquidityAds.FundingLease.Basic(10_000 sat, 500_000 sat, 0, 100, 1000 sat),
      LiquidityAds.PaymentDetails.FromFutureHtlcWithPreimage(preimage :: Nil)
    )
    val splice = createSpliceMessage(channelId, requestFunding)
    peerConnection.send(peer, splice)
    channel.expectMsg(splice)
    channel.expectNoMessage(100 millis)

    // The preimage was provided, so we fulfill upstream HTLCs.
    val fwd = register.expectMsgType[Register.Forward[CMD_FULFILL_HTLC]]
    assert(fwd.channelId == upstream.add.channelId)
    assert(fwd.message.id == upstream.add.id)
    assert(fwd.message.r == preimage)
  }

  test("reject invalid open_channel2") { f =>
    import f._

    connect(peer)

    val requestFunding = LiquidityAds.RequestFunding(
      100_000 sat,
      LiquidityAds.FundingLease.Basic(10_000 sat, 500_000 sat, 0, 0, 10_000 sat),
      LiquidityAds.PaymentDetails.FromFutureHtlc(paymentHash :: Nil)
    )
    val open = createOpenChannelMessage(requestFunding, htlcMinimum = 1_000_000 msat)

    // No matching will_add_htlc to pay fees.
    peerConnection.send(peer, open)
    rateLimiter.expectMsgType[AddOrRejectChannel].replyTo ! PendingChannelsRateLimiter.AcceptOpenChannel
    assert(peerConnection.expectMsgType[CancelOnTheFlyFunding].channelId == open.temporaryChannelId)
    channel.expectNoMessage(100 millis)

    // Requested amount is too low.
    val bigUpstream = upstreamChannel(200_000_000 msat, expiryIn, paymentHash)
    proposeFunding(150_000_000 msat, expiryOut, paymentHash, bigUpstream)
    peerConnection.send(peer, open)
    rateLimiter.expectMsgType[AddOrRejectChannel].replyTo ! PendingChannelsRateLimiter.AcceptOpenChannel
    assert(peerConnection.expectMsgType[CancelOnTheFlyFunding].channelId == open.temporaryChannelId)
    assert(register.expectMsgType[Register.Forward[CMD_FAIL_HTLC]].channelId == bigUpstream.add.channelId)
    channel.expectNoMessage(100 millis)

    // Not enough funds to pay fees.
    val upstream = upstreamChannel(11_000_000 msat, expiryIn, paymentHash)
    proposeFunding(10_999_999 msat, expiryOut, paymentHash, upstream)
    peerConnection.send(peer, open)
    rateLimiter.expectMsgType[AddOrRejectChannel].replyTo ! PendingChannelsRateLimiter.AcceptOpenChannel
    assert(peerConnection.expectMsgType[CancelOnTheFlyFunding].channelId == open.temporaryChannelId)
    assert(register.expectMsgType[Register.Forward[CMD_FAIL_HTLC]].channelId == upstream.add.channelId)
    channel.expectNoMessage(100 millis)

    // Proposal already funded.
    proposeFunding(11_000_000 msat, expiryOut, paymentHash, upstream)
    signLease(requestFunding.requestedAmount, requestFunding.paymentDetails, fees = requestFunding.fees(TestConstants.feeratePerKw))
    peerConnection.send(peer, open)
    rateLimiter.expectMsgType[AddOrRejectChannel].replyTo ! PendingChannelsRateLimiter.AcceptOpenChannel
    assert(peerConnection.expectMsgType[CancelOnTheFlyFunding].channelId == open.temporaryChannelId)
    register.expectNoMessage(100 millis)
    channel.expectNoMessage(100 millis)
  }

  test("reject invalid splice_init") { f =>
    import f._

    connect(peer)
    val channelId = openChannel(500_000 sat)

    val requestFunding = LiquidityAds.RequestFunding(
      100_000 sat,
      LiquidityAds.FundingLease.Basic(10_000 sat, 500_000 sat, 0, 0, 10_000 sat),
      LiquidityAds.PaymentDetails.FromFutureHtlcWithPreimage(preimage :: Nil)
    )
    val splice = createSpliceMessage(channelId, requestFunding)

    // No matching will_add_htlc to pay fees.
    peerConnection.send(peer, splice)
    assert(peerConnection.expectMsgType[CancelOnTheFlyFunding].channelId == channelId)
    channel.expectNoMessage(100 millis)

    // Requested amount is too low.
    val bigUpstream = upstreamChannel(200_000_000 msat, expiryIn, paymentHash)
    proposeFunding(150_000_000 msat, expiryOut, paymentHash, bigUpstream)
    peerConnection.send(peer, splice)
    assert(peerConnection.expectMsgType[CancelOnTheFlyFunding].channelId == channelId)
    assert(register.expectMsgType[Register.Forward[CMD_FAIL_HTLC]].channelId == bigUpstream.add.channelId)
    channel.expectNoMessage(100 millis)

    // Not enough funds to pay fees.
    val upstream = upstreamChannel(11_000_000 msat, expiryIn, paymentHash)
    proposeFunding(9_000_000 msat, expiryOut, paymentHash, upstream)
    peerConnection.send(peer, splice)
    assert(peerConnection.expectMsgType[CancelOnTheFlyFunding].channelId == channelId)
    assert(register.expectMsgType[Register.Forward[CMD_FAIL_HTLC]].channelId == upstream.add.channelId)
    channel.expectNoMessage(100 millis)

    // Proposal already funded.
    proposeFunding(11_000_000 msat, expiryOut, paymentHash, upstream)
    signLease(requestFunding.requestedAmount, requestFunding.paymentDetails, fees = requestFunding.fees(TestConstants.feeratePerKw), fundingTxIndex = 1)
    peerConnection.send(peer, splice)
    assert(peerConnection.expectMsgType[CancelOnTheFlyFunding].channelId == channelId)
    register.expectNoMessage(100 millis)
    channel.expectNoMessage(100 millis)
  }

  test("successfully relay HTLCs to on-the-fly funded channel") { f =>
    import f._

    connect(peer)

    val preimage1 = randomBytes32()
    val paymentHash1 = Crypto.sha256(preimage1)
    val preimage2 = randomBytes32()
    val paymentHash2 = Crypto.sha256(preimage2)

    val upstream1 = upstreamChannel(11_000_000 msat, expiryIn, paymentHash1)
    proposeFunding(10_000_000 msat, expiryOut, paymentHash1, upstream1)
    val upstream2 = upstreamChannel(16_000_000 msat, expiryIn, paymentHash2)
    proposeFunding(15_000_000 msat, expiryOut, paymentHash2, upstream2)

    val htlcMinimum = 1_500_000 msat
    val leaseFees = LiquidityAds.LeaseFees(10_000 sat, 5_000 sat)
    val lease = signLease(200_000 sat, LiquidityAds.PaymentDetails.FromFutureHtlc(List(paymentHash1, paymentHash2)), fees = leaseFees, htlcMinimum = htlcMinimum)

    // Once the channel is ready to relay payments, we forward HTLCs matching the proposed will_add_htlc.
    // We have two distinct payment hashes that are relayed independently.
    val channelData = makeChannelData(htlcMinimum)
    peer ! ChannelReadyForPayments(channel.ref, remoteNodeId, lease.channelId, fundingTxIndex = 0)
    val channelInfo = Seq(
      channel.expectMsgType[CMD_GET_CHANNEL_INFO],
      channel.expectMsgType[CMD_GET_CHANNEL_INFO],
    )

    // We relay the first payment.
    channelInfo.head.replyTo ! RES_GET_CHANNEL_INFO(remoteNodeId, lease.channelId, channel.ref, NORMAL, channelData)
    val cmd1 = channel.expectMsgType[CMD_ADD_HTLC]
    channel.expectNoMessage(100 millis)

    // We relay the second payment.
    channelInfo.last.replyTo ! RES_GET_CHANNEL_INFO(remoteNodeId, lease.channelId, channel.ref, NORMAL, channelData)
    val cmd2 = channel.expectMsgType[CMD_ADD_HTLC]
    channel.expectNoMessage(100 millis)

    // The fee is split across outgoing payments.
    assert(Set(cmd1.paymentHash, cmd2.paymentHash) == Set(paymentHash1, paymentHash2))
    val fundingFees = Seq(cmd1, cmd2).map(cmd => {
      assert(cmd.amount >= htlcMinimum)
      assert(cmd.cltvExpiry == expiryOut)
      assert(cmd.commit)
      assert(cmd.fundingFee_opt.nonEmpty)
      assert(cmd.fundingFee_opt.get.fundingTxId == lease.txId)
      assert(cmd.fundingFee_opt.get.amount > 0.msat)
      cmd.fundingFee_opt.get
    })
    val feesPaid = fundingFees.map(_.amount).sum
    assert(feesPaid == leaseFees.total.toMilliSatoshi)
    assert(cmd1.amount + cmd2.amount + feesPaid == 25_000_000.msat)

    // The payments are fulfilled.
    val (add1, add2) = if (cmd1.paymentHash == paymentHash1) (cmd1, cmd2) else (cmd2, cmd1)
    val outgoing = Seq(add1, add2).map(add => UpdateAddHtlc(lease.channelId, randomHtlcId(), add.amount, add.paymentHash, add.cltvExpiry, add.onion, add.nextBlindingKey_opt, add.fundingFee_opt))
    add1.replyTo ! RES_ADD_SETTLED(add1.origin, outgoing.head, HtlcResult.RemoteFulfill(UpdateFulfillHtlc(lease.channelId, outgoing.head.id, preimage1)))
    val fwd1 = register.expectMsgType[Register.Forward[CMD_FULFILL_HTLC]]
    assert(fwd1.channelId == upstream1.add.channelId)
    assert(fwd1.message.id == upstream1.add.id)
    assert(fwd1.message.r == preimage1)
    add2.replyTo ! RES_ADD_SETTLED(add2.origin, outgoing.last, HtlcResult.OnChainFulfill(preimage2))
    val fwd2 = register.expectMsgType[Register.Forward[CMD_FULFILL_HTLC]]
    assert(fwd2.channelId == upstream2.add.channelId)
    assert(fwd2.message.id == upstream2.add.id)
    assert(fwd2.message.r == preimage2)
    awaitCond(nodeParams.db.onTheFlyFunding.listPending(remoteNodeId).isEmpty, interval = 100 millis)
    register.expectNoMessage(100 millis)
  }

  test("successfully relay HTLCs to on-the-fly spliced channel") { f =>
    import f._

    // We create a channel, that can later be spliced.
    connect(peer)
    val channelId = openChannel(250_000 sat)

    val htlcMinimum = 1_000_000 msat
    val leaseFees = LiquidityAds.LeaseFees(1000 sat, 4000 sat)
    val upstream = Seq(
      upstreamChannel(50_000_000 msat, expiryIn, paymentHash),
      upstreamChannel(60_000_000 msat, expiryIn, paymentHash),
      Upstream.Hot.Trampoline(upstreamChannel(50_000_000 msat, expiryIn, paymentHash) :: Nil)
    )
    proposeFunding(50_000_000 msat, expiryOut, paymentHash, upstream(0))
    proposeFunding(60_000_000 msat, expiryOut, paymentHash, upstream(1))
    val lease = signLease(200_000 sat, LiquidityAds.PaymentDetails.FromFutureHtlcWithPreimage(preimage :: Nil), channelId, leaseFees, fundingTxIndex = 5, htlcMinimum)
    // We receive the last payment *after* signing the lease.
    proposeFunding(50_000_000 msat, expiryOut, paymentHash, upstream(2))

    // Once the splice with the right funding index is locked, we forward HTLCs matching the proposed will_add_htlc.
    val channelData = makeChannelData(htlcMinimum)
    peer ! ChannelReadyForPayments(channel.ref, remoteNodeId, channelId, fundingTxIndex = 4)
    channel.expectNoMessage(100 millis)
    peer ! ChannelReadyForPayments(channel.ref, remoteNodeId, channelId, fundingTxIndex = 5)
    channel.expectMsgType[CMD_GET_CHANNEL_INFO].replyTo ! RES_GET_CHANNEL_INFO(remoteNodeId, channelId, channel.ref, NORMAL, channelData)
    val adds1 = Seq(
      channel.expectMsgType[CMD_ADD_HTLC],
      channel.expectMsgType[CMD_ADD_HTLC],
      channel.expectMsgType[CMD_ADD_HTLC],
    )
    adds1.foreach(add => {
      assert(add.paymentHash == paymentHash)
      assert(add.fundingFee_opt.nonEmpty)
      assert(add.fundingFee_opt.get.fundingTxId == lease.txId)
    })
    adds1.take(2).foreach(add => assert(!add.commit))
    assert(adds1.last.commit)
    assert(adds1.map(_.fundingFee_opt.get.amount).sum == leaseFees.total.toMilliSatoshi)
    assert(adds1.map(add => add.amount + add.fundingFee_opt.get.amount).sum == 160_000_000.msat)
    channel.expectNoMessage(100 millis)

    // The recipient fails the payments: we don't relay the failure upstream and will retry.
    adds1.take(2).foreach(add => {
      val htlc = UpdateAddHtlc(channelId, randomHtlcId(), add.amount, paymentHash, add.cltvExpiry, add.onion, add.nextBlindingKey_opt, add.fundingFee_opt)
      val fail = UpdateFailHtlc(channelId, htlc.id, randomBytes(50))
      add.replyTo ! RES_ADD_SETTLED(add.origin, htlc, HtlcResult.RemoteFail(fail))
    })
    adds1.last.replyTo ! RES_ADD_FAILED(adds1.last, TooManyAcceptedHtlcs(channelId, 5), None)
    register.expectNoMessage(100 millis)

    // When the next splice completes, we retry the payment.
    peer ! ChannelReadyForPayments(channel.ref, remoteNodeId, channelId, fundingTxIndex = 6)
    channel.expectMsgType[CMD_GET_CHANNEL_INFO].replyTo ! RES_GET_CHANNEL_INFO(remoteNodeId, channelId, channel.ref, NORMAL, channelData)
    val adds2 = Seq(
      channel.expectMsgType[CMD_ADD_HTLC],
      channel.expectMsgType[CMD_ADD_HTLC],
      channel.expectMsgType[CMD_ADD_HTLC],
    )
    channel.expectNoMessage(100 millis)

    // The payment succeeds.
    adds2.foreach(add => {
      val htlc = UpdateAddHtlc(channelId, randomHtlcId(), add.amount, paymentHash, add.cltvExpiry, add.onion, add.nextBlindingKey_opt, add.fundingFee_opt)
      add.replyTo ! RES_ADD_SETTLED(add.origin, htlc, HtlcResult.OnChainFulfill(preimage))
    })
    val fwds = Seq(
      register.expectMsgType[Register.Forward[CMD_FULFILL_HTLC]],
      register.expectMsgType[Register.Forward[CMD_FULFILL_HTLC]],
      register.expectMsgType[Register.Forward[CMD_FULFILL_HTLC]],
    )
    val (channelsIn, htlcsIn) = upstream.flatMap {
      case u: Hot.Channel => Seq(u)
      case u: Hot.Trampoline => u.received
      case _: Upstream.Local => Nil
    }.map(c => (c.add.channelId, c.add.id)).toSet.unzip
    assert(fwds.map(_.channelId).toSet == channelsIn)
    assert(fwds.map(_.message.id).toSet == htlcsIn)
    fwds.foreach(fwd => assert(fwd.message.r == preimage))
    awaitCond(nodeParams.db.onTheFlyFunding.listPending(remoteNodeId).isEmpty, interval = 100 millis)

    // We don't retry anymore.
    peer ! ChannelReadyForPayments(channel.ref, remoteNodeId, channelId, fundingTxIndex = 7)
    channel.expectNoMessage(100 millis)
  }

  test("successfully relay HTLCs after restart") { f =>
    import f._

    // We create a channel, that can later be spliced.
    connect(peer)
    val channelId = openChannel(250_000 sat)

    // We relay an on-the-fly payment.
    val upstream = upstreamChannel(50_000_000 msat, expiryIn, paymentHash)
    proposeFunding(50_000_000 msat, expiryOut, paymentHash, upstream)
    val leaseFees = LiquidityAds.LeaseFees(1000 sat, 1000 sat)
    val lease = signLease(200_000 sat, LiquidityAds.PaymentDetails.FromChannelBalanceForFutureHtlc(paymentHash :: Nil), channelId, leaseFees, fundingTxIndex = 1)
    peer ! ChannelReadyForPayments(channel.ref, remoteNodeId, channelId, fundingTxIndex = 1)
    val channelData1 = makeChannelData()
    channel.expectMsgType[CMD_GET_CHANNEL_INFO].replyTo ! RES_GET_CHANNEL_INFO(remoteNodeId, channelId, channel.ref, NORMAL, channelData1)
    // We don't collect additional fees if they were paid from our peer's channel balance already.
    val cmd1 = channel.expectMsgType[CMD_ADD_HTLC]
    val htlc = UpdateAddHtlc(channelId, 0, cmd1.amount, paymentHash, cmd1.cltvExpiry, cmd1.onion, cmd1.nextBlindingKey_opt, cmd1.fundingFee_opt)
    assert(cmd1.fundingFee_opt.contains(LiquidityAds.FundingFee(0 msat, lease.txId)))
    channel.expectNoMessage(100 millis)

    // We disconnect: on reconnection, we don't attempt the payment again since it's already pending.
    disconnect(channelCount = 1)
    connect(peer, channelCount = 1)
    peer ! ChannelReadyForPayments(channel.ref, remoteNodeId, channelId, fundingTxIndex = 1)
    channel.expectNoMessage(100 millis)
    register.expectNoMessage(100 millis)

    // On restart, we don't attempt the payment again: it's already pending.
    val peerAfterRestart = TestFSMRef(new Peer(nodeParams, remoteNodeId, new DummyOnChainWallet(), FakeChannelFactory(remoteNodeId, channel), TestProbe().ref, register.ref, TestProbe().ref, TestProbe().ref))
    peerAfterRestart ! Peer.Init(Set.empty)
    connect(peerAfterRestart)
    peerAfterRestart ! ChannelReadyForPayments(channel.ref, remoteNodeId, channelId, fundingTxIndex = 1)
    val channelData2 = makeChannelData(localChanges = LocalChanges(Nil, htlc :: Nil, Nil))
    channel.expectMsgType[CMD_GET_CHANNEL_INFO].replyTo ! RES_GET_CHANNEL_INFO(remoteNodeId, channelId, channel.ref, NORMAL, channelData2)
    channel.expectNoMessage(100 millis)

    // The payment is failed by our peer but we don't see it (it's a cold origin): we attempt it again.
    val channelData3 = makeChannelData()
    peerAfterRestart ! ChannelReadyForPayments(channel.ref, remoteNodeId, channelId, fundingTxIndex = 1)
    channel.expectMsgType[CMD_GET_CHANNEL_INFO].replyTo ! RES_GET_CHANNEL_INFO(remoteNodeId, channelId, channel.ref, NORMAL, channelData3)
    val cmd2 = channel.expectMsgType[CMD_ADD_HTLC]
    assert(cmd2.paymentHash == paymentHash)
    assert(cmd2.amount == cmd1.amount)
    channel.expectNoMessage(100 millis)

    // The payment is fulfilled by our peer.
    cmd2.replyTo ! RES_ADD_SETTLED(cmd2.origin, htlc, HtlcResult.OnChainFulfill(preimage))
    assert(register.expectMsgType[Register.Forward[CMD_FULFILL_HTLC]].channelId == upstream.add.channelId)
    nodeParams.db.onTheFlyFunding.addPreimage(preimage)
    register.expectNoMessage(100 millis)
    awaitCond(nodeParams.db.onTheFlyFunding.listPending(remoteNodeId).isEmpty, interval = 100 millis)
  }

  test("don't relay payments too close to expiry") { f =>
    import f._

    connect(peer)

    val upstream = upstreamChannel(100_000_000 msat, expiryIn, paymentHash)
    proposeFunding(100_000_000 msat, CltvExpiry(TestConstants.defaultBlockHeight), paymentHash, upstream)
    val lease = signLease(200_000 sat, LiquidityAds.PaymentDetails.FromFutureHtlc(List(paymentHash)))

    // We're too close the HTLC expiry to relay it.
    peer ! ChannelReadyForPayments(channel.ref, remoteNodeId, lease.channelId, fundingTxIndex = 0)
    channel.expectNoMessage(100 millis)
    peer ! CurrentBlockHeight(BlockHeight(TestConstants.defaultBlockHeight))
    val fwd = register.expectMsgType[Register.Forward[CMD_FAIL_HTLC]]
    assert(fwd.channelId == upstream.add.channelId)
    assert(fwd.message.id == upstream.add.id)
    awaitCond(nodeParams.db.onTheFlyFunding.listPending(remoteNodeId).isEmpty, interval = 100 millis)
  }

  test("don't relay payments for known preimage") { f =>
    import f._

    connect(peer)

    val upstream = upstreamChannel(100_000_000 msat, expiryIn, paymentHash)
    proposeFunding(100_000_000 msat, expiryOut, paymentHash, upstream)
    val lease = signLease(200_000 sat, LiquidityAds.PaymentDetails.FromFutureHtlc(List(paymentHash)))

    // We've already relayed that payment and have the matching preimage in our DB.
    // We don't relay it again to avoid paying our peer twice.
    nodeParams.db.onTheFlyFunding.addPreimage(preimage)
    peer ! ChannelReadyForPayments(channel.ref, remoteNodeId, lease.channelId, fundingTxIndex = 0)
    channel.expectMsgType[CMD_GET_CHANNEL_INFO].replyTo ! RES_GET_CHANNEL_INFO(remoteNodeId, lease.channelId, channel.ref, NORMAL, makeChannelData())
    channel.expectNoMessage(100 millis)

    val fwd = register.expectMsgType[Register.Forward[CMD_FULFILL_HTLC]]
    assert(fwd.channelId == upstream.add.channelId)
    assert(fwd.message.id == upstream.add.id)
    assert(fwd.message.r == preimage)
    register.expectNoMessage(100 millis)
  }

  test("stop when disconnecting without pending proposals") { f =>
    import f._

    connect(peer)
    probe.watch(peer.ref)
    disconnect()
    probe.expectTerminated(peer.ref)
  }

  test("stop when disconnecting with non-funded pending proposals") { f =>
    import f._

    connect(peer)
    probe.watch(peer.ref)

    // We have two distinct pending funding proposals.
    val paymentHash1 = randomBytes32()
    val upstream1 = upstreamChannel(300_000_000 msat, CltvExpiry(1200), paymentHash1)
    proposeFunding(250_000_000 msat, CltvExpiry(1105), paymentHash1, upstream1)
    val paymentHash2 = randomBytes32()
    val upstream2 = Upstream.Hot.Trampoline(List(
      upstreamChannel(100_000_000 msat, CltvExpiry(1250), paymentHash2),
      upstreamChannel(150_000_000 msat, CltvExpiry(1240), paymentHash2),
    ))
    proposeFunding(225_000_000 msat, CltvExpiry(1105), paymentHash2, upstream2)

    // All incoming HTLCs are failed on disconnection.
    disconnect()
    (upstream1.add +: upstream2.received.map(_.add)).foreach(add => {
      val fwd = register.expectMsgType[Register.Forward[CMD_FAIL_HTLC]]
      assert(fwd.channelId == add.channelId)
      assert(fwd.message.id == add.id)
      assert(fwd.message.reason == Right(UnknownNextPeer()))
      assert(fwd.message.commit)
    })
    register.expectNoMessage(100 millis)
    probe.expectTerminated(peer.ref)
  }

  test("don't stop when disconnecting with funded pending proposals") { f =>
    import f._

    connect(peer)
    probe.watch(peer.ref)

    // We have one funded proposal and one that was not funded yet.
    val upstream1 = upstreamChannel(300_000_000 msat, CltvExpiry(1200))
    proposeFunding(250_000_000 msat, CltvExpiry(1105), upstream1.add.paymentHash, upstream1)
    val upstream2 = upstreamChannel(250_000_000 msat, CltvExpiry(1000))
    proposeFunding(220_000_000 msat, CltvExpiry(1105), upstream2.add.paymentHash, upstream2)
    signLease(500_000 sat, LiquidityAds.PaymentDetails.FromFutureHtlc(upstream2.add.paymentHash :: Nil))

    // Only non-funded proposals are failed on disconnection, and we don't stop before the funded proposal completes.
    disconnect()
    val fwd = register.expectMsgType[Register.Forward[CMD_FAIL_HTLC]]
    assert(fwd.channelId == upstream1.add.channelId)
    assert(fwd.message.id == upstream1.add.id)
    assert(fwd.message.reason == Right(UnknownNextPeer()))
    assert(fwd.message.commit)
    register.expectNoMessage(100 millis)
    probe.expectNoMessage(100 millis)
  }

}

object OnTheFlyFundingSpec {

  val expiryIn = CltvExpiry(TestConstants.defaultBlockHeight + 750)
  val expiryOut = CltvExpiry(TestConstants.defaultBlockHeight + 500)

  val preimage = randomBytes32()
  val paymentHash = Crypto.sha256(preimage)

  def randomOnion(): OnionRoutingPacket = OnionRoutingPacket(0, randomKey().publicKey.value, randomBytes(1300), randomBytes32())

  def randomHtlcId(): Long = Math.abs(randomLong()) % 50_000

  def upstreamChannel(amountIn: MilliSatoshi, expiryIn: CltvExpiry, paymentHash: ByteVector32 = randomBytes32(), blinded: Boolean = false): Upstream.Hot.Channel = {
    val blindingKey = if (blinded) Some(randomKey().publicKey) else None
    val add = UpdateAddHtlc(randomBytes32(), randomHtlcId(), amountIn, paymentHash, expiryIn, TestConstants.emptyOnionPacket, blindingKey, None)
    Upstream.Hot.Channel(add, TimestampMilli.now())
  }

  def createWillAdd(amount: MilliSatoshi, paymentHash: ByteVector32, expiry: CltvExpiry, blinding_opt: Option[PublicKey] = None): WillAddHtlc = {
    WillAddHtlc(Block.RegtestGenesisBlock.hash, randomBytes32(), amount, paymentHash, expiry, randomOnion(), blinding_opt)
  }

  def createStatus(): OnTheFlyFunding.Status = OnTheFlyFunding.Status.Funded(randomBytes32(), TxId(randomBytes32()), 0, 2500 msat)

  def createOpenChannelMessage(requestFunding: LiquidityAds.RequestFunding, fundingAmount: Satoshi = 250_000 sat, htlcMinimum: MilliSatoshi = 1 msat): OpenDualFundedChannel = {
    val channelFlags = ChannelFlags(nonInitiatorPaysCommitFees = true, announceChannel = false)
    val tlvs = TlvStream[OpenDualFundedChannelTlv](ChannelTlv.RequestFundingTlv(requestFunding))
    OpenDualFundedChannel(Block.RegtestGenesisBlock.hash, randomBytes32(), TestConstants.feeratePerKw, TestConstants.anchorOutputsFeeratePerKw, fundingAmount, 483 sat, UInt64(100), htlcMinimum, CltvExpiryDelta(144), 10, 0, randomKey().publicKey, randomKey().publicKey, randomKey().publicKey, randomKey().publicKey, randomKey().publicKey, randomKey().publicKey, randomKey().publicKey, channelFlags, tlvs)
  }

  def createSpliceMessage(channelId: ByteVector32, requestFunding: LiquidityAds.RequestFunding): SpliceInit = {
    SpliceInit(channelId, 0 sat, 0, TestConstants.feeratePerKw, randomKey().publicKey, 0 msat, requireConfirmedInputs = false, Some(requestFunding))
  }

}
