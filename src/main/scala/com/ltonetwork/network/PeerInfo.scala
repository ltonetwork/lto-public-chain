package com.ltonetwork.network

import java.net.{InetSocketAddress, SocketAddress}

case class PeerInfo(remoteAddress: SocketAddress,
                    declaredAddress: Option[InetSocketAddress],
                    applicationName: String,
                    applicationVersion: (Int, Int, Int),
                    nodeName: String,
                    nodeNonce: Long)
