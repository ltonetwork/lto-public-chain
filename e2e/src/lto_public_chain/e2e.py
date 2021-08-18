import unittest
import time

import config
import http_requests
import api
import utils

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
    # 1. Alice gets associated with Bob
    # 2. Alice revokes her association with Bob
    def test_invoke_and_revoke_association(self):
        anchor = utils.random_string()

        # Step 1: Alice gets associated with Bob
        invoke_assoc_tx = api.invoke_association(self.alice, self.bob, anchor)
        invoke_assoc_tx_id = invoke_assoc_tx['id']
        polled_invoke_assoc_tx = api.get_tx(invoke_assoc_tx_id)

        self.assertEqual(
            polled_invoke_assoc_tx['id'],
            invoke_assoc_tx_id)

        alice_assocs = api.list_associations(self.alice.address).json()
        alice_assoc = next((assoc for assoc in alice_assocs['outgoing'] if assoc['issueTransactionId'] == invoke_assoc_tx_id), None)

        self.assertIsNotNone(alice_assoc)
        self.assertEqual(
            alice_assoc['associationType'],
            1
        )
        self.assertEqual(
            alice_assoc['party'],
            self.bob.address
        )
        self.assertFalse('revokeTransactionId' in alice_assoc)
        self.assertFalse('revokeHeight' in alice_assoc)

        bob_assocs = api.list_associations(self.bob.address).json()
        bob_assoc = next((assoc for assoc in bob_assocs['incoming'] if assoc['issueTransactionId'] == invoke_assoc_tx_id), None)

        self.assertIsNotNone(bob_assoc)
        self.assertEqual(
            bob_assoc['associationType'],
            1
        )
        self.assertEqual(
            bob_assoc['party'],
            self.alice.address
        )
        self.assertEqual(
            polled_invoke_assoc_tx['id'],
            invoke_assoc_tx['id'])
        self.assertFalse('revokeTransactionId' in bob_assoc)
        self.assertFalse('revokeHeight' in bob_assoc)

        # Step 2: Alice revokes her association with Bob

        revoke_assoc_tx = api.revoke_association(self.alice, self.bob, anchor)
        revoke_assoc_tx_id = revoke_assoc_tx['id']
        polled_revoke_assoc_tx = api.get_tx(revoke_assoc_tx_id)

        self.assertEqual(
            polled_revoke_assoc_tx['id'],
            revoke_assoc_tx_id)

        alice_assocs_after_revoke = api.list_associations(self.alice.address).json()
        alice_assoc_after_revoke = next((assoc for assoc in alice_assocs_after_revoke['outgoing'] if 'revokeTransactionId' in assoc and assoc['revokeTransactionId'] == revoke_assoc_tx_id), None)

        self.assertIsNotNone(alice_assoc_after_revoke)
        self.assertEqual(
            alice_assoc_after_revoke['issueTransactionId'],
            alice_assoc['issueTransactionId']
        )
        self.assertEqual(
            alice_assoc_after_revoke['associationType'],
            alice_assoc['associationType']
        )
        self.assertEqual(
            alice_assoc_after_revoke['party'],
            self.bob.address
        )
        self.assertTrue(alice_assoc_after_revoke['revokeHeight'] >= alice_assoc_after_revoke['issueHeight'])

        bob_assocs = api.list_associations(self.bob.address).json()
        bob_assoc = next((assoc for assoc in bob_assocs['incoming'] if assoc['issueTransactionId'] == revoke_assoc_tx_id), None)

        self.assertIsNotNone(bob_assoc)
        self.assertEqual(
            bob_assoc['associationType'],
            1
        )
        self.assertEqual(
            bob_assoc['party'],
            self.alice.address
        )
        self.assertEqual(
            polled_invoke_assoc_tx['id'],
            invoke_assoc_tx['id'])


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
    suite.addTest(E2eTests("test_invoke_and_revoke_association"))
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    assert result.wasSuccessful()

if __name__ == "__main__":
    run()
