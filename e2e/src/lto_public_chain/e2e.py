import unittest
import time

import config
import http_requests
import api

class E2eTests(unittest.TestCase):
    faucet = api.create_account('cable sniff exchange judge gym rifle prevent traffic picture firm shaft exist cute unusual check')
    alice = api.create_account('prepare awake mule vital rescue when radio view sibling bread spatial abstract visual insane crisp')
    bob = api.create_account('home visit certain universe adjust thing estate pyramid age puzzle update ensure fatal crucial hat')
    charlie = api.create_account('loud forum youth tourist discover prosper lawn wisdom cattle twelve rule grow cry music stomach')

    def test_connectivity(self):
        self.assertEqual(
            http_requests.get("/").status_code,
            200)

    # Scenario:
    # 1. Alice's and Charlie's balances initialisation
    # 2. Create and setup smart contract for Charlie
    # 3. Alice funds Charlie
    # 4. Alice can't take money from Charlie
    # 5.1 Bob takes funds because he knows secret hash and 5.2 after rollback wait height and Alice takes funds back
    #TODO
    def atomic_swap(self):
        secret_text = 'some secret message from Alice'
        sha_secret = 'BN6RTYGWcwektQfSFzH8raYo9awaLgQ7pLyWLQY4S4F5'
        # Step 1: Alice's and Charlie's balances initialisation
        tx_1 = api.transfer(self.faucet, self.alice, 10000)
        tx_2 = api.transfer(self.faucet, self.charlie, 10000)

        # Step 2: Create and setup smart contract for Charlie

        before_height = api.get_height()

        script = (
                f"let Alice = Address(base58'{self.alice.address}') "
                f"let Bob = Address(base58'{self.bob.address}') "
                f"let BeforeHeight = {before_height} "
                "match tx { "
                "    case ttx: TransferTransaction => "
                f"        let txToBob = (ttx.recipient == Bob) && (sha256(ttx.proofs[0]) == base58'{sha_secret}') && ((10 + BeforeHeight) >= height) "
                "        let backToAliceAfterHeight = ((height >= (11 + BeforeHeight)) && (ttx.recipient == Alice)) "

                "        txToBob || backToAliceAfterHeight "
                "    case other => false "
                "}")
        
        print(script)

        set_script_tx = api.set_script(self.charlie, script)
        
        # self.assertEqual(
        #     http_requests.get("/debug/minerInfo").status_code,
        #     200)

def run():
    suite = unittest.TestSuite()
    suite.addTest(E2eTests("test_connectivity"))
    suite.addTest(E2eTests("atomic_swap"))
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    assert result.wasSuccessful()

if __name__ == "__main__":
    run()
