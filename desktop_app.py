#!/usr/bin/env python3
"""Launch the Household Budgeting Shiny app inside a Python desktop window."""

from __future__ import annotations

import argparse
import os
import signal
import socket
import subprocess
import sys
import time
import urllib.error
import urllib.request
from contextlib import closing

# --------------------------------------------------------------------
# Determine the directory where the .exe is located (PyInstaller-safe)
# --------------------------------------------------------------------
def resource_path(relative_path: str) -> str:
    """
    Return the absolute path to a resource, whether running as a script
    or as a PyInstaller bundle.
    """
    base_path = getattr(sys, "_MEIPASS", os.path.dirname(os.path.abspath(sys.argv[0])))
    return os.path.join(base_path, relative_path)



def find_free_port() -> int:
    """Return an available localhost port."""

    with closing(socket.socket(socket.AF_INET, socket.SOCK_STREAM)) as sock:
        sock.bind(("127.0.0.1", 0))
        sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        return sock.getsockname()[1]


def wait_for_server(url: str, timeout: float = 30.0) -> None:
    """Block until the Shiny app responds or timeout is reached."""

    deadline = time.monotonic() + timeout

    while time.monotonic() < deadline:
        try:
            with urllib.request.urlopen(url, timeout=1):
                return
        except (urllib.error.URLError, TimeoutError, ConnectionError, socket.timeout):
            time.sleep(0.25)

    raise TimeoutError(f"Timed out waiting for Shiny app at {url}")


def launch_shiny(rscript: str, app_script: str, port: int) -> subprocess.Popen:
    """Start the R Shiny process and return the handle."""

    env = os.environ.copy()
    env.update(
        {
            "SHINY_PORT": str(port),
            "SHINY_HOST": "127.0.0.1",
            "SHINY_LAUNCH_BROWSER": "false",
        }
    )

    process = subprocess.Popen([rscript, app_script], env=env)
    return process


def terminate_process(process: subprocess.Popen, *, timeout: float = 5.0) -> None:
    """Terminate the R process gracefully, falling back to kill."""

    if process.poll() is not None:
        return

    process.terminate()

    try:
        process.wait(timeout=timeout)
    except subprocess.TimeoutExpired:
        process.kill()


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--rscript",
        default="Rscript",
        help="Path to the Rscript executable (default: %(default)s)",
    )
    parser.add_argument(
        "--app-script",
        default=resource_path("run_app.R"),
        help="Path to the R launcher script",
    )
    parser.add_argument(
        "--timeout",
        type=float,
        default=30.0,
        help="Seconds to wait for Shiny to start (default: %(default)s)",
    )

    args = parser.parse_args(argv)

    try:
        import webview  # type: ignore
    except ImportError as exc:  # pragma: no cover - import guard
        parser.error(
            "pywebview is required to launch the desktop window (install with 'pip install pywebview')."
        )
        raise exc

    port = find_free_port()
    url = f"http://127.0.0.1:{port}"

    process = launch_shiny(args.rscript, args.app_script, port)

    def handle_exit(signum, frame):  # type: ignore[override]
        terminate_process(process)
        sys.exit(0)

    signal.signal(signal.SIGINT, handle_exit)
    signal.signal(signal.SIGTERM, handle_exit)

    try:
        wait_for_server(url, timeout=args.timeout)
    except Exception:
        terminate_process(process)
        raise

    window = webview.create_window("Household Budgeting", url)

    def on_closed() -> None:
        """Ensure the Shiny process stops if the window is closed."""

        terminate_process(process)

    window.events.closed += on_closed  # type: ignore[attr-defined]

    try:
        webview.start()
    finally:
        terminate_process(process)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
