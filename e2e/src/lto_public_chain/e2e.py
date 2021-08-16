import unittest

import config
import http_requests
import utils

class E2eTests(unittest.TestCase):
    def test_connectivity(self):
        self.assertEqual(
            http_requests.get("/").status_code,
            200)

    def atomic_swap(self):
        alice_address = utils.create_wallet().json()['address']
        bob_address = utils.create_wallet().json()['address']
        swap_address = utils.create_wallet().json()['address']
        acc = utils.create_account('bEaMXTP5K1bm3uH8AKXJeBfHBP6TBP2dexxHzbEiqXHADmT3mrLaTUjGdK1XRXayMjdimB3kXPUd8pACPKuWAqt7iEohZyhHAuCnxBRqZbXw9VmdaRmvtoNu3SBRCswG4wC')
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
