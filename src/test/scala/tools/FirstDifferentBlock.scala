package tools

import play.api.libs.json.Json

object FirstDifferentBlock extends App {

  def get(url: String) = scala.io.Source.fromURL(url).mkString

  def blockAt(nodeHttp: String, blockHeight: Int)    = get(nodeHttp + "/blocks/at/" + blockHeight)
  def blockSigAt(nodeHttp: String, blockHeight: Int) = (Json.parse(blockAt(nodeHttp, blockHeight)) \ "signature").get.as[String]

  def nodeComparator(node1: String, node2: String)(h: Int): Boolean = {
    blockSigAt(node1, h) == blockSigAt(node2, h)
  }

  val TESTNET1 = "http://1.1.1.1:6869"

  def firstDifferent(min: Int, max: Int, areSame: Int => Boolean): Int = {
    println("searching [" + min + ", " + max + ")")
    if (max - min <= 1)
      max
    else {
      val split = (min + max) / 2
      if (areSame(split))
        firstDifferent(split, max, areSame)
      else firstDifferent(min, split, areSame)
    }
  }

  println("first different block height is " + firstDifferent(1, 258, nodeComparator(TESTNET1, TESTNET1)))
}
