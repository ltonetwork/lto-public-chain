package one.legalthings.lang.v1.evaluator.ctx

import one.legalthings.lang.v1.compiler.Types.CASETYPEREF

case class CaseObj(caseType: CASETYPEREF, fields: Map[String, Any]) {
  override def toString: String = {
    s"""
       |${caseType.name} {
       |  ${fields.map({ case (k, v) => s"$k -> $v" }).mkString(", ")}
       |}
     """.stripMargin
  }
}
