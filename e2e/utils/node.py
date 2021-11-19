import subprocess
import polling


def shutdown_node():
    http_requests.post("/node/stop")

def start_node():
    subprocess.call("bin/run_public_node", shell=True)

def is_node_down():
    try:
        polling.poll(
            lambda: http_requests.get("/"),
            check_success=lambda response: response.status_code != 200,
            step=1,
            timeout=180
        )
        return False
    except requests.exceptions.ConnectionError:
        return True

def is_node_up():
    return polling.poll(
        lambda: http_requests.get("/").status_code == 200,
        step=1,
        ignore_exceptions=(requests.exceptions.ConnectionError),
        timeout=180
    )