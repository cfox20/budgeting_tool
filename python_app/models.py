"""Qt data models used by the budgeting application."""
from __future__ import annotations

from typing import Any

import pandas as pd
from PySide6 import QtCore


class PandasModel(QtCore.QAbstractTableModel):
    """A minimal editable Qt model backed by a pandas DataFrame."""

    frame_changed = QtCore.Signal(pd.DataFrame)

    def __init__(self, frame: pd.DataFrame | None = None, parent=None) -> None:
        super().__init__(parent)
        self._frame = frame.copy() if frame is not None else pd.DataFrame()

    # ------------------------------------------------------------------
    # Qt model API
    def rowCount(self, parent=QtCore.QModelIndex()) -> int:  # type: ignore[override]
        if parent.isValid():
            return 0
        return len(self._frame.index)

    def columnCount(self, parent=QtCore.QModelIndex()) -> int:  # type: ignore[override]
        if parent.isValid():
            return 0
        return len(self._frame.columns)

    def data(self, index: QtCore.QModelIndex, role: int = QtCore.Qt.DisplayRole) -> Any:  # type: ignore[override]
        if not index.isValid():
            return None
        value = self._frame.iat[index.row(), index.column()]
        if role in (QtCore.Qt.DisplayRole, QtCore.Qt.EditRole):
            return "" if pd.isna(value) else str(value)
        return None

    def headerData(  # type: ignore[override]
        self, section: int, orientation: QtCore.Qt.Orientation, role: int = QtCore.Qt.DisplayRole
    ) -> Any:
        if role != QtCore.Qt.DisplayRole:
            return None
        if orientation == QtCore.Qt.Horizontal:
            try:
                return str(self._frame.columns[section])
            except IndexError:
                return None
        return str(section + 1)

    def flags(self, index: QtCore.QModelIndex) -> QtCore.Qt.ItemFlags:  # type: ignore[override]
        if not index.isValid():
            return QtCore.Qt.ItemIsEnabled
        return (
            QtCore.Qt.ItemIsEnabled
            | QtCore.Qt.ItemIsSelectable
            | QtCore.Qt.ItemIsEditable
        )

    def setData(  # type: ignore[override]
        self, index: QtCore.QModelIndex, value: Any, role: int = QtCore.Qt.EditRole
    ) -> bool:
        if not index.isValid() or role != QtCore.Qt.EditRole:
            return False
        self._frame.iat[index.row(), index.column()] = value
        self.dataChanged.emit(index, index, [role])
        self.frame_changed.emit(self._frame.copy())
        return True

    def insertRows(  # type: ignore[override]
        self, row: int, count: int, parent: QtCore.QModelIndex = QtCore.QModelIndex()
    ) -> bool:
        if count <= 0:
            return False
        self.beginInsertRows(parent, row, row + count - 1)
        for _ in range(count):
            empty = {col: "" for col in self._frame.columns}
            self._frame = pd.concat(
                [
                    self._frame.iloc[:row],
                    pd.DataFrame([empty], columns=self._frame.columns),
                    self._frame.iloc[row:],
                ],
                ignore_index=True,
            )
        self.endInsertRows()
        self.frame_changed.emit(self._frame.copy())
        return True

    def removeRows(  # type: ignore[override]
        self, row: int, count: int, parent: QtCore.QModelIndex = QtCore.QModelIndex()
    ) -> bool:
        if count <= 0:
            return False
        if row < 0 or row + count > len(self._frame.index):
            return False
        self.beginRemoveRows(parent, row, row + count - 1)
        mask = [True] * len(self._frame.index)
        for offset in range(count):
            mask[row + offset] = False
        self._frame = self._frame.iloc[mask].reset_index(drop=True)
        self.endRemoveRows()
        self.frame_changed.emit(self._frame.copy())
        return True

    # ------------------------------------------------------------------
    # helpers
    def update_frame(self, frame: pd.DataFrame) -> None:
        self.beginResetModel()
        self._frame = frame.copy()
        self.endResetModel()
        self.frame_changed.emit(self._frame.copy())

    def append_record(self, record: dict[str, Any]) -> None:
        frame = self._frame.copy()
        frame = pd.concat([frame, pd.DataFrame([record], columns=frame.columns)])
        frame = frame.reset_index(drop=True)
        self.update_frame(frame)

    def to_dataframe(self) -> pd.DataFrame:
        return self._frame.copy()

    def column_names(self) -> list[str]:
        return list(self._frame.columns)
