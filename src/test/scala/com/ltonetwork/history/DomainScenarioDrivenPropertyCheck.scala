package com.ltonetwork.history

import com.ltonetwork.db.WithState
import com.ltonetwork.settings.LtoSettings
import org.scalacheck.Gen
import org.scalatest.Assertion
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

trait DomainScenarioDrivenPropertyCheck extends WithState { _: ScalaCheckDrivenPropertyChecks =>
  def scenario[S](gen: Gen[S], bs: LtoSettings = DefaultLtoSettings)(assertion: (Domain, S) => Assertion): Assertion =
    forAll(gen) { s =>
      withDomain(bs) { domain =>
        assertion(domain, s)
      }
    }
}
