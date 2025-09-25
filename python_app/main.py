"""Launch the budgeting R Shiny application inside a desktop window."""
from __future__ import annotations

import os
import sys
from pathlib import Path

from PySide6 import QtCore, QtGui, QtWidgets
from PySide6.QtCore import QUrl
from PySide6.QtWebEngineWidgets import QWebEngineView

from .data_store import BudgetDataStore
from .shiny_launcher import ShinyAppProcess

def _determine_data_dir(project_root: Path) -> Path:
    """Return the directory used to persist user data."""

def _platform_user_data_dir(app_name: str) -> Path:
    """Return a user-writable data directory for the current platform."""

    if sys.platform.startswith("win"):
        base = os.environ.get("LOCALAPPDATA") or os.environ.get("APPDATA")
        if base is None:
            base = Path.home() / "AppData" / "Local"
        return Path(base) / app_name

    if sys.platform == "darwin":
        return Path.home() / "Library" / "Application Support" / app_name

    # Linux and everything else follows the XDG Base Directory spec.
    xdg_data_home = os.environ.get("XDG_DATA_HOME")
    base = Path(xdg_data_home) if xdg_data_home else Path.home() / ".local" / "share"
    return base / app_name


def _determine_data_dir(project_root: Path) -> Path:
    """Return the directory used to persist user data."""

    if getattr(sys, "frozen", False):
        return _platform_user_data_dir("Budgeting Tool")
    return project_root / "user_data"

    QtCore.QCoreApplication.setAttribute(QtCore.Qt.AA_ShareOpenGLContexts, True)
    app = QtWidgets.QApplication(sys.argv)

class ShinyWindow(QtWidgets.QMainWindow):
    """Qt window embedding the running R Shiny budgeting app."""

    def __init__(self, shiny_process: ShinyAppProcess) -> None:
        super().__init__()
        self._shiny_process = shiny_process

        self.setWindowTitle("Household Budgeting (R Shiny)")
        self.resize(1200, 800)

        self._view = QWebEngineView(self)
        self._view.loadFinished.connect(self._handle_load_finished)
        self._view.setUrl(QUrl.fromUserInput(f"{shiny_process.url}/"))
        self.setCentralWidget(self._view)

    # ------------------------------------------------------------------
    def _handle_load_finished(self, ok: bool) -> None:
        """Surface connection failures from the embedded web view."""

        if ok:
            return

        log_path = self._shiny_process.data_dir / "shiny_app.log"
        QtWidgets.QMessageBox.warning(
            self,
            "Unable to display budgeting app",
            (
                "The embedded browser could not load the Shiny interface.\n"
                f"Check '{log_path}' for errors and ensure R is installed."
            ),
        )
        if shiny_process is not None:
            shiny_process.stop()
        return 1

    # ------------------------------------------------------------------
    def closeEvent(self, event: QtGui.QCloseEvent) -> None:  # type: ignore[override]
        """Ensure the R process stops when the window closes."""

        try:
            self._shiny_process.stop()
        finally:
            super().closeEvent(event)


def main() -> int:
    """Entry point executed by ``run_desktop.py`` and PyInstaller."""

    project_root = Path(__file__).resolve().parents[1]
    data_dir = _determine_data_dir(project_root)

    # Prepare the data directory and seed files before the R app boots.
    BudgetDataStore(base_dir=data_dir)

    QtCore.QCoreApplication.setAttribute(QtCore.Qt.AA_ShareOpenGLContexts, True)
    app = QtWidgets.QApplication(sys.argv)

    shiny_process: ShinyAppProcess | None = None

    try:
        shiny_process = ShinyAppProcess(project_root, data_dir=data_dir)
        shiny_process.start()
        shiny_process.wait_until_ready()
    except Exception as exc:  # pragma: no cover - GUI error dialog
        log_path = data_dir / "shiny_app.log"
        QtWidgets.QMessageBox.critical(
            None,
            "Failed to start budgeting app",
            f"{exc}\n\nCheck '{log_path}' for detailed logs.",
        )
        if shiny_process is not None:
            shiny_process.stop()
        return 1

    window = ShinyWindow(shiny_process)
    window.show()

    exit_code = app.exec()
    shiny_process.stop()
    return int(exit_code)


if __name__ == "__main__":
    raise SystemExit(main())
