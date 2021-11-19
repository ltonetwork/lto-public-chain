from e2e.common import node
from behave import *

def before_all(context):
    if node.is_node_up() is False:
        node.start_node()


def after_all(context):
    node.stop_node()
