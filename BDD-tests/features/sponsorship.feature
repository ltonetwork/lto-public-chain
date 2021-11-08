Feature: Sponsorship Transaction

  Scenario: Successfull sponsorship transaction
    Given Alice has 100 lto
    And Alice is not sponsoring Bob
    And Alice has 100 lto
    When Alice sponsors Bob
    Then Alice has 95 lto
    And Alice is sponsoring Bob



  Scenario: Unsuccessful sponsorship transaction due to insufficient balance
    Given Alice has 0 lto
    When Alice tries to sponsor Bob
    Then The transaction fails



Scenario: Successfull CancelSponsorship transaction
    Given Alice has 100 lto
    And Alice is sponsoring Bob
    And Alice has 100 lto
    When Alice cancel the sponsorship for Bob
    Then Alice has 95 lto
    And Alice is not sponsoring bob


  Scenario: Unsuccessful cancel sponsorship transaction due to insufficient balance
    Given Alice has 0 lto
    When Alice tries to cancel the sponsorship for Bob
    Then The transaction fails


