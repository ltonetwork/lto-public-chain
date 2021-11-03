#Feature: Transfer Transaction

  #Scenario Outline: Successfull Transfer transaction
    #Given "<sender>" has 100 lto
   # And "<receiver>" has 0 lto
    #When "<sender>" transfer 10 lto to "<receiver>"
    #Then "<sender>" has 81 lto
    #And "<receiver>" has 10 lto

   # Examples:
  #  | receiver | sender |
 #   | Bob      | Alice  |

  #Scenario: Unsuccessful anchor transaction
  #  Given  has 0 lto
  #  Then The Transfer transaction fails