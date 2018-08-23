package one.legalthings.history

import one.legalthings.db.WithState
import one.legalthings.settings.WavesSettings
import org.scalacheck.Gen
import org.scalatest.Assertion
import org.scalatest.prop.GeneratorDrivenPropertyChecks

trait DomainScenarioDrivenPropertyCheck extends WithState { _: GeneratorDrivenPropertyChecks =>
  def scenario[S](gen: Gen[S], bs: WavesSettings = DefaultWavesSettings)(assertion: (Domain, S) => Assertion): Assertion =
    forAll(gen) { s =>
      withDomain(bs) { domain =>
        assertion(domain, s)
      }
    }
}
