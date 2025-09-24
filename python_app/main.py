"""Launch the budgeting R Shiny application inside a desktop window."""
from __future__ import annotations

import sys
from pathlib import Path

from PySide6 import QtCore, QtGui, QtWidgets
from PySide6.QtCore import QUrl
from PySide6.QtWebEngineWidgets import QWebEngineView

from .data_store import BudgetDataStore
from .shiny_launcher import ShinyAppProcess

def _determine_data_dir(project_root: Path) -> Path:
    """Return the directory used to persist user data."""

    if getattr(sys, "frozen", False):
        executable_dir = Path(sys.executable).resolve().parent
        return executable_dir / "user_data"
    return project_root / "user_data"


class ShinyWindow(QtWidgets.QMainWindow):
    """Qt window embedding the running R Shiny budgeting app."""

    def __init__(self, shiny_process: ShinyAppProcess) -> None:
        super().__init__()
        self._shiny_process = shiny_process
        self.setWindowTitle("Household Budgeting (R Shiny)")
        self.resize(1200, 800)

        view = QWebEngineView(self)
        view.setUrl(QUrl(shiny_process.url))
        self.setCentralWidget(view)

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
        QtWidgets.QMessageBox.critical(
            None,
            "Failed to start budgeting app",
            f"{exc}\n\nCheck 'user_data/shiny_app.log' for detailed logs.",
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
