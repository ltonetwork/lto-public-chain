package one.legalthings.it

import one.legalthings.account.AddressScheme

trait IntegrationTestsScheme {
  AddressScheme.current = new AddressScheme {
    override val chainId: Byte = 'I'
  }
}
