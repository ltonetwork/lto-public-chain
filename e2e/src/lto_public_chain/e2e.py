import unittest

import config
import http_requests

class E2eTests(unittest.TestCase):
    def test_connectivity(self):
        self.assertEqual(
            http_requests.get("/").status_code,
            200)

def run():
    suite = unittest.TestSuite()
    suite.addTest(E2eTests("test_connectivity"))
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    assert result.wasSuccessful()

run()
