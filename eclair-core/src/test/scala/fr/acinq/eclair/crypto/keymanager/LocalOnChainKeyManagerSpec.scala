package fr.acinq.eclair.crypto.keymanager

import fr.acinq.bitcoin.psbt.{KeyPathWithMaster, Psbt, TaprootBip32DerivationPath}
import fr.acinq.bitcoin.scalacompat.{Block, DeterministicWallet, MnemonicCode, OutPoint, Satoshi, Script, Transaction, TxIn, TxOut}
import fr.acinq.bitcoin.{KeyPath, ScriptFlags, SigHash}
import fr.acinq.eclair.TimestampSecond
import fr.acinq.eclair.blockchain.AddressType
import org.scalatest.funsuite.AnyFunSuite
import scodec.bits.ByteVector

import java.util.Base64
import scala.jdk.CollectionConverters.SeqHasAsJava
import scala.util.{Failure, Success}

class LocalOnChainKeyManagerSpec extends AnyFunSuite {
  test("sign psbt (non-reg test)") {
    val entropy = ByteVector.fromValidHex("01" * 32)
    val seed = MnemonicCode.toSeed(MnemonicCode.toMnemonics(entropy), "")
    val onChainKeyManager = new LocalOnChainKeyManager("eclair", seed, TimestampSecond.now(), Block.TestnetGenesisBlock.hash)
    // data generated by bitcoin core on regtest
    val psbt = Psbt.read(
      Base64.getDecoder.decode("cHNidP8BAHECAAAAAfZo4nGIyTg77MFmEBkQH1Au3Jl8vzB2WWQGGz/MbyssAAAAAAD9////ArAHPgUAAAAAFgAU6j9yVvLg66Zu3GM/xHbmXT0yvyiAlpgAAAAAABYAFODscQh3N7lmDYyV5yrHpGL2Zd4JAAAAAAABAH0CAAAAAaNdmqUNlziIjSaif3JUcvJWdyF0U5bYq13NMe+LbaBZAAAAAAD9////AjSp1gUAAAAAFgAUjfFMfBg8ulo/874n3+0ode7ka0BAQg8AAAAAACIAIPUn/XU17DfnvDkj8gn2twG3jtr2Z7sthy9K2MPTdYkaAAAAAAEBHzSp1gUAAAAAFgAUjfFMfBg8ulo/874n3+0ode7ka0AiBgM+PDdyxsVisa66SyBxiUvhEam8lEP64yujvVsEcGaqIxgPCfOBVAAAgAEAAIAAAACAAQAAAAMAAAAAIgIDWmAhb/sCV9+HjwFpPuy2TyEBi/Y11wrEHZUihe3N80EYDwnzgVQAAIABAACAAAAAgAEAAAAFAAAAAAA=")
    ).getRight

    val Success(psbt1) = onChainKeyManager.sign(psbt, psbt.inputs.toArray().indices, Seq(0))
    val tx = psbt1.extract()
    assert(tx.isRight)
  }

  test("sign psbt (BIP84") {
    import fr.acinq.bitcoin.scalacompat.KotlinUtils._

    val seed = ByteVector.fromValidHex("01" * 32)
    val onChainKeyManager = new LocalOnChainKeyManager("eclair", seed, TimestampSecond.now(), Block.TestnetGenesisBlock.hash)

    // create a watch-only BIP84 wallet from our key manager xpub
    val (_, accountPub) = DeterministicWallet.ExtendedPublicKey.decode(onChainKeyManager.masterPubKey(0, AddressType.Bech32))
    val mainPub = DeterministicWallet.derivePublicKey(accountPub, 0)

    def getPublicKey(index: Long) = DeterministicWallet.derivePublicKey(mainPub, index).publicKey

    val utxos = Seq(
      Transaction(version = 2, txIn = Nil, txOut = TxOut(Satoshi(1_000_000), Script.pay2wpkh(getPublicKey(0))) :: Nil, lockTime = 0),
      Transaction(version = 2, txIn = Nil, txOut = TxOut(Satoshi(1_100_000), Script.pay2wpkh(getPublicKey(1))) :: Nil, lockTime = 0),
      Transaction(version = 2, txIn = Nil, txOut = TxOut(Satoshi(1_200_000), Script.pay2wpkh(getPublicKey(2))) :: Nil, lockTime = 0),
    )
    val bip32paths = Seq(
      new KeyPathWithMaster(0, new fr.acinq.bitcoin.KeyPath("m/84'/1'/0'/0/0")),
      new KeyPathWithMaster(0, new fr.acinq.bitcoin.KeyPath("m/84'/1'/0'/0/1")),
      new KeyPathWithMaster(0, new fr.acinq.bitcoin.KeyPath("m/84'/1'/0'/0/2")),
    )

    val tx = Transaction(version = 2,
      txIn = utxos.map(tx => TxIn(OutPoint(tx, 0), Nil, fr.acinq.bitcoin.TxIn.SEQUENCE_FINAL)),
      txOut = TxOut(Satoshi(1000_000), Script.pay2wpkh(getPublicKey(0))) :: Nil, lockTime = 0)

    val Right(psbt) = for {
      p0 <- new Psbt(tx).updateWitnessInput(OutPoint(utxos(0), 0), utxos(0).txOut(0), null, Script.pay2pkh(getPublicKey(0)).map(scala2kmp).asJava, null, java.util.Map.of(getPublicKey(0), bip32paths(0)), null, null, java.util.Map.of())
      p1 <- p0.updateWitnessInput(OutPoint(utxos(1), 0), utxos(1).txOut(0), null, Script.pay2pkh(getPublicKey(1)).map(scala2kmp).asJava, null, java.util.Map.of(getPublicKey(1), bip32paths(1)), null, null, java.util.Map.of())
      p2 <- p1.updateWitnessInput(OutPoint(utxos(2), 0), utxos(2).txOut(0), null, Script.pay2pkh(getPublicKey(2)).map(scala2kmp).asJava, null, java.util.Map.of(getPublicKey(2), bip32paths(2)), null, null, java.util.Map.of())
      p3 <- p2.updateNonWitnessInput(utxos(0), 0, null, null, java.util.Map.of())
      p4 <- p3.updateNonWitnessInput(utxos(1), 0, null, null, java.util.Map.of())
      p5 <- p4.updateNonWitnessInput(utxos(2), 0, null, null, java.util.Map.of())
      p6 <- p5.updateWitnessOutput(0, null, null, java.util.Map.of(getPublicKey(0), bip32paths(0)), null, java.util.Map.of())
    } yield p6

    {
      // sign all inputs and outputs
      val Success(psbt1) = onChainKeyManager.sign(psbt, Seq(0, 1, 2), Seq(0))
      val signedTx = psbt1.extract().getRight
      Transaction.correctlySpends(signedTx, utxos, ScriptFlags.STANDARD_SCRIPT_VERIFY_FLAGS)
    }
    {
      // sign the first 2 inputs only
      val Success(psbt1) = onChainKeyManager.sign(psbt, Seq(0, 1), Seq(0))
      // extracting the final tx fails because no all inputs as signed
      assert(psbt1.extract().isLeft)
      assert(psbt1.getInput(2).getScriptWitness == null)
    }
    {
      // provide a wrong derivation path for the first input
      val updated = psbt.updateWitnessInput(OutPoint(utxos(0), 0), utxos(0).txOut(0), null, Script.pay2pkh(getPublicKey(0)).map(scala2kmp).asJava, null, java.util.Map.of(getPublicKey(0), bip32paths(2)), null, null, java.util.Map.of()).getRight // wrong bip32 path
      val Failure(error) = onChainKeyManager.sign(updated, Seq(0, 1, 2), Seq(0))
      assert(error.getMessage.contains("derived public key doesn't match"))
    }
    {
      // provide a wrong derivation path for the first output
      val updated = psbt.updateWitnessOutput(0, null, null, java.util.Map.of(getPublicKey(0), bip32paths(1)), null, java.util.Map.of()).getRight // wrong path
      val Failure(error) = onChainKeyManager.sign(updated, Seq(0, 1, 2), Seq(0))
      assert(error.getMessage.contains("could not verify output 0"))
    }
    {
      // lie about the amount being spent
      val updated = psbt.updateWitnessInput(OutPoint(utxos(0), 0), utxos(0).txOut(0).copy(amount = Satoshi(10)), null, Script.pay2pkh(getPublicKey(0)).map(scala2kmp).asJava, null, java.util.Map.of(getPublicKey(0), bip32paths(0)), null, null, java.util.Map.of()).getRight
      val Failure(error) = onChainKeyManager.sign(updated, Seq(0, 1, 2), Seq(0))
      assert(error.getMessage.contains("utxo mismatch"))
    }
    {
      // do not provide non-witness utxo for utxo #2
      val Right(psbt) = for {
        p0 <- new Psbt(tx).updateWitnessInput(OutPoint(utxos(0), 0), utxos(0).txOut(0), null, Script.pay2pkh(getPublicKey(0)).map(scala2kmp).asJava, null, java.util.Map.of(getPublicKey(0), bip32paths(0)), null, null, java.util.Map.of())
        p1 <- p0.updateWitnessInput(OutPoint(utxos(1), 0), utxos(1).txOut(0), null, Script.pay2pkh(getPublicKey(1)).map(scala2kmp).asJava, null, java.util.Map.of(getPublicKey(1), bip32paths(1)), null, null, java.util.Map.of())
        p2 <- p1.updateWitnessInput(OutPoint(utxos(2), 0), utxos(2).txOut(0), null, Script.pay2pkh(getPublicKey(2)).map(scala2kmp).asJava, null, java.util.Map.of(getPublicKey(2), bip32paths(2)), null, null, java.util.Map.of())
        p3 <- p2.updateNonWitnessInput(utxos(0), 0, null, null, java.util.Map.of())
        p4 <- p3.updateNonWitnessInput(utxos(1), 0, null, null, java.util.Map.of())
        p5 <- p4.updateWitnessOutput(0, null, null, java.util.Map.of(getPublicKey(0), bip32paths(0)), null, java.util.Map.of())
      } yield p5
      val Failure(error) = onChainKeyManager.sign(psbt, Seq(0, 1, 2), Seq(0))
      assert(error.getMessage.contains("non-witness utxo is missing"))
    }
    {
      // use sighash type != SIGHASH_ALL
      val updated = psbt.updateWitnessInput(OutPoint(utxos(0), 0), utxos(0).txOut(0), null, Script.pay2pkh(getPublicKey(0)).map(scala2kmp).asJava, SigHash.SIGHASH_SINGLE, java.util.Map.of(getPublicKey(0), bip32paths(0)), null, null, java.util.Map.of()).getRight
      val Failure(error) = onChainKeyManager.sign(updated, Seq(0, 1, 2), Seq(0))
      assert(error.getMessage.contains("input sighash must be SIGHASH_ALL"))
    }
  }

  test("sign psbt (BIP86") {
    import fr.acinq.bitcoin.scalacompat.KotlinUtils._

    val seed = ByteVector.fromValidHex("01" * 32)
    val onChainKeyManager = new LocalOnChainKeyManager("eclair", seed, TimestampSecond.now(), Block.TestnetGenesisBlock.hash)

    // create a watch-only BIP84 wallet from our key manager xpub
    val (_, accountPub) = DeterministicWallet.ExtendedPublicKey.decode(onChainKeyManager.masterPubKey(0, AddressType.Bech32m))
    val mainPub = DeterministicWallet.derivePublicKey(accountPub, 0)

    def getPublicKey(index: Long) = DeterministicWallet.derivePublicKey(mainPub, index).publicKey.xOnly

    val utxos = Seq(
      Transaction(version = 2, txIn = Nil, txOut = TxOut(Satoshi(1_000_000), Script.pay2tr(getPublicKey(0), None)) :: Nil, lockTime = 0),
      Transaction(version = 2, txIn = Nil, txOut = TxOut(Satoshi(1_100_000), Script.pay2tr(getPublicKey(1), None)) :: Nil, lockTime = 0),
      Transaction(version = 2, txIn = Nil, txOut = TxOut(Satoshi(1_200_000), Script.pay2tr(getPublicKey(2), None)) :: Nil, lockTime = 0),
    )
    val bip32paths = Seq(
      new TaprootBip32DerivationPath(java.util.List.of(), 0, new KeyPath("m/86'/1'/0'/0/0")),
      new TaprootBip32DerivationPath(java.util.List.of(), 0, new fr.acinq.bitcoin.KeyPath("m/86'/1'/0'/0/1")),
      new TaprootBip32DerivationPath(java.util.List.of(), 0, new fr.acinq.bitcoin.KeyPath("m/86'/1'/0'/0/2")),
    )

    val tx = Transaction(version = 2,
      txIn = utxos.map(tx => TxIn(OutPoint(tx, 0), Nil, fr.acinq.bitcoin.TxIn.SEQUENCE_FINAL)),
      txOut = TxOut(Satoshi(1000_000), Script.pay2tr(getPublicKey(0), None)) :: Nil, lockTime = 0)

    val Right(psbt) = for {
      p0 <- new Psbt(tx).updateWitnessInput(OutPoint(utxos(0), 0), utxos(0).txOut(0), null, null, null, java.util.Map.of(), null, getPublicKey(0), java.util.Map.of(getPublicKey(0), bip32paths(0)))
      p1 <- p0.updateWitnessInput(OutPoint(utxos(1), 0), utxos(1).txOut(0), null, null, null, java.util.Map.of(), null, getPublicKey(1), java.util.Map.of(getPublicKey(1), bip32paths(1)))
      p2 <- p1.updateWitnessInput(OutPoint(utxos(2), 0), utxos(2).txOut(0), null, null, null, java.util.Map.of(), null, getPublicKey(2), java.util.Map.of(getPublicKey(2), bip32paths(2)))
      p3 <- p2.updateWitnessOutput(0, null, null, java.util.Map.of(), getPublicKey(0), java.util.Map.of(getPublicKey(0), bip32paths(0)))
    } yield p3

    {
      // sign all inputs and outputs
      val Success(psbt1) = onChainKeyManager.sign(psbt, Seq(0, 1, 2), Seq(0))
      val signedTx = psbt1.extract().getRight
      Transaction.correctlySpends(signedTx, utxos, ScriptFlags.STANDARD_SCRIPT_VERIFY_FLAGS)
    }
    {
      // sign the first 2 inputs only
      val Success(psbt1) = onChainKeyManager.sign(psbt, Seq(0, 1), Seq(0))
      // extracting the final tx fails because no all inputs as signed
      assert(psbt1.extract().isLeft)
      assert(psbt1.getInput(2).getScriptWitness == null)
    }
    {
      // provide a wrong derivation path for the first input
      val updated = psbt.updateWitnessInput(OutPoint(utxos(0), 0), utxos(0).txOut(0), null, null, null, java.util.Map.of(), null, null, java.util.Map.of(getPublicKey(0), bip32paths(2))).getRight // wrong bip32 path
      val Failure(error) = onChainKeyManager.sign(updated, Seq(0, 1, 2), Seq(0))
      assert(error.getMessage.contains("derived public key doesn't match"))
    }
    {
      // provide a wrong derivation path for the first output
      val updated = psbt.updateWitnessOutput(0, null, null, java.util.Map.of(), null, java.util.Map.of(getPublicKey(0), bip32paths(1))).getRight // wrong path
      val Failure(error) = onChainKeyManager.sign(updated, Seq(0, 1, 2), Seq(0))
      assert(error.getMessage.contains("could not verify output 0"))
    }
    {
      // use sighash type != SIGHASH_ALL
      val updated = psbt.updateWitnessInput(OutPoint(utxos(0), 0), utxos(0).txOut(0), null, null, SigHash.SIGHASH_SINGLE, java.util.Map.of(), null, null, java.util.Map.of(getPublicKey(0), bip32paths(0))).getRight
      val Failure(error) = onChainKeyManager.sign(updated, Seq(0, 1, 2), Seq(0))
      assert(error.getMessage.contains("input sighash must be SIGHASH_DEFAULT"))
    }
  }

}
