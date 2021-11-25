from e2e.common import node

started_node = False


def before_all(context):
    global started_node
    
    if not node.is_node_up():
        node.start_node()
        started_node = True

def after_all(context):
    if started_node:
        node.stop_node()
