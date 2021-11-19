from e2e.common import node

NODE_RUNNING = False


def before_all(context):
    global NODE_RUNNING
    if node.is_node_up() is False:
        node.start_node()
    else:
        NODE_RUNNING = True


def after_all(context):
    if NODE_RUNNING is False:
        node.stop_node()
