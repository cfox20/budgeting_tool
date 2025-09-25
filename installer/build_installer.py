"""Build a standalone executable for the budgeting app using PyInstaller."""
from __future__ import annotations

import os
import platform
import shutil
import subprocess
import sys
from datetime import datetime
from pathlib import Path


def build() -> None:
    project_root = Path(__file__).resolve().parents[1]
    spec_path = project_root / "BudgetingTool.spec"
    if spec_path.exists():
        spec_path.unlink()

    env = os.environ.copy()
    dist_root = project_root / "installer" / "dist"
    build_root = project_root / "installer" / "build"
    cmd = [
        sys.executable,
        "-m",
        "PyInstaller",
        "--noconfirm",
        "--clean",
        "--windowed",
        "--distpath",
        str(dist_root),
        "--workpath",
        str(build_root),
        "--name",
        "BudgetingTool",
        "--add-data",
        f"{project_root / 'python_app'}{os.pathsep}python_app",
        "--add-data",
        f"{project_root / 'r_app'}{os.pathsep}r_app",
        str(project_root / "run_desktop.py"),
    ]
    print("Running", " ".join(cmd))
    subprocess.check_call(cmd, env=env)

    bundle_dir = dist_root / "BudgetingTool"
    if not bundle_dir.exists():
        print(
            "PyInstaller completed but the expected installer/dist/BudgetingTool folder was not found."
        )

        return

    archive_suffix = platform.system().lower()
    if archive_suffix == "darwin":
        archive_suffix = "macos"
    archive_name = f"BudgetingTool-{archive_suffix}"
    timestamp = datetime.utcnow().strftime("%Y%m%d")
    archive_path = dist_root / f"{archive_name}-{timestamp}"

    print(f"Creating release archive {archive_path.name}.zip from {bundle_dir}…")
    shutil.make_archive(str(archive_path), "zip", root_dir=bundle_dir)
    print(f"Release archive ready: {archive_path.name}.zip")


if __name__ == "__main__":
    try:
        build()
        print(
            "Installer build complete. Check the installer/dist/ directory for the executable bundle."
        )
    except FileNotFoundError as exc:
        raise SystemExit(
            "PyInstaller is required. Install it with 'pip install pyinstaller' before running this script."
        ) from exc
