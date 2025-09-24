"""Build a standalone executable for the budgeting app using PyInstaller."""
from __future__ import annotations

import os
import subprocess
import sys
from pathlib import Path


def build() -> None:
    project_root = Path(__file__).resolve().parents[1]
    spec_path = project_root / "BudgetingTool.spec"
    if spec_path.exists():
        spec_path.unlink()

    env = os.environ.copy()
    cmd = [
        sys.executable,
        "-m",
        "PyInstaller",
        "--noconfirm",
        "--clean",
        "--windowed",
        "--name",
        "BudgetingTool",
        "--add-data",
        f"{project_root / 'python_app'}{os.pathsep}python_app",
        str(project_root / "run_desktop.py"),
    ]
    print("Running", " ".join(cmd))
    subprocess.check_call(cmd, env=env)


if __name__ == "__main__":
    try:
        build()
        print("Installer build complete. Check the dist/ directory for the executable bundle.")
    except FileNotFoundError as exc:
        raise SystemExit(
            "PyInstaller is required. Install it with 'pip install pyinstaller' before running this script."
        ) from exc
