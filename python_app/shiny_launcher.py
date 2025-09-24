"""Utilities for booting and monitoring the bundled R Shiny app."""
from __future__ import annotations

import os
import shutil
import signal
import socket
import subprocess
import time
from pathlib import Path
from typing import Optional, TextIO
from urllib import error, request


class ShinyAppProcess:
    """Launch and manage the lifetime of the R Shiny budgeting application."""

    def __init__(self, project_root: Path, port: Optional[int] = None) -> None:
        self.project_root = project_root
        self.port = port or self._find_free_port()
        self.url = f"http://127.0.0.1:{self.port}"

        self._process: subprocess.Popen[str] | None = None
        self._log_handle: Optional[TextIO] = None

    # ------------------------------------------------------------------
    def start(self) -> None:
        """Start the R Shiny process in the background."""

        rscript = self._find_rscript()
        launch_script = self.project_root / "r_app" / "launch_shiny.R"
        if not launch_script.exists():
            raise RuntimeError(
                "The R launcher script is missing. Expected to find 'r_app/launch_shiny.R'."
            )

        data_dir = self.project_root / "user_data"
        data_dir.mkdir(parents=True, exist_ok=True)
        log_path = data_dir / "shiny_app.log"
        self._log_handle = log_path.open("w", encoding="utf-8")

        env = os.environ.copy()
        env.setdefault("R_SHINY_BUDGET_DATA_DIR", str(data_dir))
        env.setdefault("R_SHINY_APP_ROOT", str(self.project_root / "r_app"))

        args = [rscript, str(launch_script), str(self.port)]
        try:
            self._process = subprocess.Popen(
                args,
                cwd=str(self.project_root / "r_app"),
                stdout=self._log_handle,
                stderr=subprocess.STDOUT,
                env=env,
                text=True,
            )
        except Exception:
            if self._log_handle is not None:
                self._log_handle.close()
                self._log_handle = None
            raise

    # ------------------------------------------------------------------
    def wait_until_ready(self, timeout: float = 60.0) -> None:
        """Block until the Shiny app responds to HTTP requests."""

        if self._process is None:
            raise RuntimeError("Shiny process was not started.")

        deadline = time.time() + timeout
        while time.time() < deadline:
            if self._process.poll() is not None:
                raise RuntimeError(
                    "The R Shiny process exited before it became ready."
                )

            try:
                with request.urlopen(self.url, timeout=1):
                    return
            except error.URLError:
                time.sleep(0.5)

        raise TimeoutError(
            "Timed out waiting for the R Shiny app to start. "
            "Review 'user_data/shiny_app.log' for details."
        )

    # ------------------------------------------------------------------
    def stop(self) -> None:
        """Terminate the Shiny process if it is still running."""

        if self._process is None:
            if self._log_handle is not None:
                self._log_handle.close()
                self._log_handle = None
            return

        if self._process.poll() is None:
            try:
                if os.name == "nt":
                    self._process.terminate()
                else:
                    self._process.send_signal(signal.SIGINT)
                self._process.wait(timeout=10)
            except subprocess.TimeoutExpired:
                self._process.kill()
        self._process = None

        if self._log_handle is not None:
            self._log_handle.close()
            self._log_handle = None

    # ------------------------------------------------------------------
    @staticmethod
    def _find_free_port() -> int:
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
            sock.bind(("127.0.0.1", 0))
            return int(sock.getsockname()[1])

    @staticmethod
    def _find_rscript() -> str:
        candidate = shutil.which("Rscript")
        if candidate is None:
            raise RuntimeError(
                "Unable to locate the 'Rscript' executable. Install R and ensure it is on your PATH."
            )
        return candidate


__all__ = ["ShinyAppProcess"]
