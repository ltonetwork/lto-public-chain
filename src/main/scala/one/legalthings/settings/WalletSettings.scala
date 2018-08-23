package one.legalthings.settings

import java.io.File

import one.legalthings.state.ByteStr

case class WalletSettings(file: Option[File], password: String, seed: Option[ByteStr])
