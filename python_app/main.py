"""Standalone budgeting desktop application entry point."""
from __future__ import annotations

import sys
from datetime import date
from pathlib import Path
from typing import Optional

import pandas as pd
from PySide6 import QtCore, QtGui, QtWidgets
from matplotlib.backends.backend_qtagg import FigureCanvasQTAgg as FigureCanvas
from matplotlib.figure import Figure

from .data_store import BudgetDataStore
from .models import PandasModel


class SavePreviewDialog(QtWidgets.QDialog):
    """Modal dialog asking the user to confirm an expense save."""

    def __init__(self, frame: pd.DataFrame, parent: Optional[QtWidgets.QWidget] = None) -> None:
        super().__init__(parent)
        self.setWindowTitle("Confirm save")
        self.setModal(True)
        layout = QtWidgets.QVBoxLayout(self)

        info_label = QtWidgets.QLabel(
            "You are about to overwrite the expense log and its backup. "
            "Review the most recent entries below before confirming."
        )
        info_label.setWordWrap(True)
        layout.addWidget(info_label)

        preview_table = QtWidgets.QTableWidget(self)
        recent = frame.tail(15)
        preview_table.setRowCount(len(recent.index))
        preview_table.setColumnCount(len(recent.columns))
        preview_table.setHorizontalHeaderLabels([col.title() for col in recent.columns])
        for row_idx, (_, row) in enumerate(recent.iterrows()):
            for col_idx, value in enumerate(row):
                item = QtWidgets.QTableWidgetItem(str(value))
                preview_table.setItem(row_idx, col_idx, item)
        preview_table.resizeColumnsToContents()
        layout.addWidget(preview_table)

        button_box = QtWidgets.QDialogButtonBox(QtWidgets.QDialogButtonBox.Save | QtWidgets.QDialogButtonBox.Cancel)
        button_box.accepted.connect(self.accept)
        button_box.rejected.connect(self.reject)
        layout.addWidget(button_box)


class ExpensesTab(QtWidgets.QWidget):
    """Tab for entering and editing expenses."""

    data_saved = QtCore.Signal(pd.DataFrame)

    def __init__(self, data_store: BudgetDataStore, parent: Optional[QtWidgets.QWidget] = None) -> None:
        super().__init__(parent)
        self.data_store = data_store
        self.model = PandasModel(self.data_store.load_expenses())
        self.model.frame_changed.connect(self._update_form_choices)

        self._build_ui()
        self._update_form_choices(self.model.to_dataframe())

    def _build_ui(self) -> None:
        layout = QtWidgets.QVBoxLayout(self)

        form_box = QtWidgets.QGroupBox("Add a new expense")
        form_layout = QtWidgets.QGridLayout(form_box)

        self.date_input = QtWidgets.QDateEdit(self)
        self.date_input.setCalendarPopup(True)
        self.date_input.setDate(QtCore.QDate.currentDate())

        self.description_input = QtWidgets.QLineEdit(self)

        self.category_input = QtWidgets.QComboBox(self)
        self.category_input.setEditable(True)

        self.amount_input = QtWidgets.QDoubleSpinBox(self)
        self.amount_input.setMaximum(1_000_000_000)
        self.amount_input.setPrefix("$")
        self.amount_input.setDecimals(2)

        self.payer_input = QtWidgets.QComboBox(self)
        self.payer_input.setEditable(True)

        self.account_input = QtWidgets.QComboBox(self)
        self.account_input.setEditable(True)

        add_button = QtWidgets.QPushButton("Add expense")
        add_button.clicked.connect(self._handle_add_expense)

        form_layout.addWidget(QtWidgets.QLabel("Date"), 0, 0)
        form_layout.addWidget(self.date_input, 0, 1)
        form_layout.addWidget(QtWidgets.QLabel("Description"), 1, 0)
        form_layout.addWidget(self.description_input, 1, 1)
        form_layout.addWidget(QtWidgets.QLabel("Category"), 2, 0)
        form_layout.addWidget(self.category_input, 2, 1)
        form_layout.addWidget(QtWidgets.QLabel("Amount"), 3, 0)
        form_layout.addWidget(self.amount_input, 3, 1)
        form_layout.addWidget(QtWidgets.QLabel("Payer"), 4, 0)
        form_layout.addWidget(self.payer_input, 4, 1)
        form_layout.addWidget(QtWidgets.QLabel("Account"), 5, 0)
        form_layout.addWidget(self.account_input, 5, 1)
        form_layout.addWidget(add_button, 6, 0, 1, 2)

        layout.addWidget(form_box)

        self.table_view = QtWidgets.QTableView(self)
        self.table_view.setModel(self.model)
        self.table_view.horizontalHeader().setStretchLastSection(True)
        self.table_view.setSelectionBehavior(QtWidgets.QTableView.SelectRows)
        self.table_view.setSelectionMode(QtWidgets.QTableView.SingleSelection)
        layout.addWidget(self.table_view)

        button_bar = QtWidgets.QHBoxLayout()
        save_button = QtWidgets.QPushButton("Save changes")
        save_button.clicked.connect(self._handle_save)
        reload_button = QtWidgets.QPushButton("Reload from disk")
        reload_button.clicked.connect(self._reload_from_disk)
        remove_button = QtWidgets.QPushButton("Delete selected")
        remove_button.clicked.connect(self._delete_selected)
        button_bar.addWidget(save_button)
        button_bar.addWidget(reload_button)
        button_bar.addWidget(remove_button)
        button_bar.addStretch()
        layout.addLayout(button_bar)

        help_label = QtWidgets.QLabel(
            "Double-click cells to edit them. Saving will replace the backup after you confirm the preview."
        )
        help_label.setWordWrap(True)
        layout.addWidget(help_label)

    # ------------------------------------------------------------------
    def _handle_add_expense(self) -> None:
        record = {
            "date": self.date_input.date().toString("yyyy-MM-dd"),
            "description": self.description_input.text().strip(),
            "category": self.category_input.currentText().strip(),
            "amount": round(self.amount_input.value(), 2),
            "payer": self.payer_input.currentText().strip(),
            "account": self.account_input.currentText().strip(),
        }
        self.model.append_record(record)
        self.description_input.clear()
        self.amount_input.setValue(0.0)
        self.description_input.setFocus()

    def _handle_save(self) -> None:
        frame = self.model.to_dataframe()
        dialog = SavePreviewDialog(frame, self)
        if dialog.exec() == QtWidgets.QDialog.Accepted:
            self.data_store.save_expenses(frame)
            QtWidgets.QMessageBox.information(
                self,
                "Expenses saved",
                "The expense log and backup were updated successfully.",
            )
            self.data_saved.emit(frame)

    def _reload_from_disk(self) -> None:
        frame = self.data_store.load_expenses()
        self.model.update_frame(frame)

    def _delete_selected(self) -> None:
        selection = self.table_view.selectionModel().selectedRows()
        if not selection:
            return
        index = selection[0]
        self.model.removeRows(index.row(), 1)

    def _update_form_choices(self, frame: pd.DataFrame) -> None:
        def sync_combo(combo: QtWidgets.QComboBox, values: pd.Series) -> None:
            existing = {combo.itemText(i) for i in range(combo.count())}
            for value in sorted({str(v) for v in values if str(v).strip()}):
                if value not in existing:
                    combo.addItem(value)

        if "category" in frame.columns:
            sync_combo(self.category_input, frame["category"].dropna())
        budget_categories = self.data_store.load_category_budget()
        if not budget_categories.empty and "category" in budget_categories.columns:
            sync_combo(self.category_input, budget_categories["category"].dropna())
        if "payer" in frame.columns:
            sync_combo(self.payer_input, frame["payer"].dropna())
        if "account" in frame.columns:
            sync_combo(self.account_input, frame["account"].dropna())


class BudgetTab(QtWidgets.QWidget):
    """Tab for configuring income sources and budget targets."""

    data_saved = QtCore.Signal()

    def __init__(self, data_store: BudgetDataStore, parent: Optional[QtWidgets.QWidget] = None) -> None:
        super().__init__(parent)
        self.data_store = data_store
        self.income_model = PandasModel(self.data_store.load_income_sources())
        self.budget_model = PandasModel(self.data_store.load_category_budget())

        self._build_ui()

    def _build_ui(self) -> None:
        layout = QtWidgets.QVBoxLayout(self)

        income_box = QtWidgets.QGroupBox("Monthly income sources")
        income_layout = QtWidgets.QVBoxLayout(income_box)
        self.income_table = QtWidgets.QTableView(self)
        self.income_table.setModel(self.income_model)
        self.income_table.horizontalHeader().setStretchLastSection(True)
        income_layout.addWidget(self.income_table)

        income_buttons = QtWidgets.QHBoxLayout()
        add_income = QtWidgets.QPushButton("Add income source")
        add_income.clicked.connect(lambda: self._add_row(self.income_model))
        remove_income = QtWidgets.QPushButton("Delete selected")
        remove_income.clicked.connect(lambda: self._remove_selected(self.income_table, self.income_model))
        save_income = QtWidgets.QPushButton("Save income")
        save_income.clicked.connect(self._save_income_sources)
        income_buttons.addWidget(add_income)
        income_buttons.addWidget(remove_income)
        income_buttons.addWidget(save_income)
        income_buttons.addStretch()
        income_layout.addLayout(income_buttons)

        layout.addWidget(income_box)

        budget_box = QtWidgets.QGroupBox("Monthly category targets")
        budget_layout = QtWidgets.QVBoxLayout(budget_box)
        self.budget_table = QtWidgets.QTableView(self)
        self.budget_table.setModel(self.budget_model)
        self.budget_table.horizontalHeader().setStretchLastSection(True)
        budget_layout.addWidget(self.budget_table)

        budget_buttons = QtWidgets.QHBoxLayout()
        add_budget = QtWidgets.QPushButton("Add category")
        add_budget.clicked.connect(lambda: self._add_row(self.budget_model))
        remove_budget = QtWidgets.QPushButton("Delete selected")
        remove_budget.clicked.connect(lambda: self._remove_selected(self.budget_table, self.budget_model))
        save_budget = QtWidgets.QPushButton("Save targets")
        save_budget.clicked.connect(self._save_budget)
        budget_buttons.addWidget(add_budget)
        budget_buttons.addWidget(remove_budget)
        budget_buttons.addWidget(save_budget)
        budget_buttons.addStretch()
        budget_layout.addLayout(budget_buttons)

        layout.addWidget(budget_box)

        help_label = QtWidgets.QLabel(
            "Use these tables to define the plan for the month. "
            "The reports tab compares actual spending against your targets."
        )
        help_label.setWordWrap(True)
        layout.addWidget(help_label)

    def _add_row(self, model: PandasModel) -> None:
        empty = {}
        for name in model.column_names():
            empty[name] = 0.0 if "amount" in name else ""
        model.append_record(empty)

    def _remove_selected(self, view: QtWidgets.QTableView, model: PandasModel) -> None:
        selection = view.selectionModel().selectedRows()
        if not selection:
            return
        model.removeRows(selection[0].row(), 1)

    def _save_income_sources(self) -> None:
        frame = self.income_model.to_dataframe()
        self.data_store.save_income_sources(frame)
        QtWidgets.QMessageBox.information(self, "Income saved", "Income sources updated.")
        self.data_saved.emit()

    def _save_budget(self) -> None:
        frame = self.budget_model.to_dataframe()
        self.data_store.save_category_budget(frame)
        QtWidgets.QMessageBox.information(self, "Targets saved", "Category targets updated.")
        self.data_saved.emit()


class ReportsTab(QtWidgets.QWidget):
    """Tab that visualises spending versus the plan."""

    def __init__(self, data_store: BudgetDataStore, parent: Optional[QtWidgets.QWidget] = None) -> None:
        super().__init__(parent)
        self.data_store = data_store

        self._build_ui()
        self._set_default_dates()
        self.refresh_report()

    def _build_ui(self) -> None:
        layout = QtWidgets.QVBoxLayout(self)

        controls = QtWidgets.QHBoxLayout()
        controls.addWidget(QtWidgets.QLabel("Start date"))
        self.start_date = QtWidgets.QDateEdit(self)
        self.start_date.setCalendarPopup(True)
        controls.addWidget(self.start_date)
        controls.addWidget(QtWidgets.QLabel("End date"))
        self.end_date = QtWidgets.QDateEdit(self)
        self.end_date.setCalendarPopup(True)
        controls.addWidget(self.end_date)
        refresh_btn = QtWidgets.QPushButton("Refresh")
        refresh_btn.clicked.connect(self.refresh_report)
        controls.addWidget(refresh_btn)
        controls.addStretch()
        layout.addLayout(controls)

        self.summary_table = QtWidgets.QTableWidget(self)
        layout.addWidget(self.summary_table)

        split_layout = QtWidgets.QHBoxLayout()
        self.over_list = QtWidgets.QListWidget(self)
        self.over_list.setSelectionMode(QtWidgets.QAbstractItemView.NoSelection)
        self.over_list.setMaximumWidth(250)
        self.over_list.setMinimumWidth(200)
        split_layout.addWidget(_with_label("Over budget", self.over_list))

        self.under_list = QtWidgets.QListWidget(self)
        self.under_list.setSelectionMode(QtWidgets.QAbstractItemView.NoSelection)
        self.under_list.setMaximumWidth(250)
        self.under_list.setMinimumWidth(200)
        split_layout.addWidget(_with_label("Under budget", self.under_list))

        self.figure = Figure(figsize=(6, 4))
        self.canvas = FigureCanvas(self.figure)
        split_layout.addWidget(self.canvas, stretch=1)

        layout.addLayout(split_layout)

        self.income_label = QtWidgets.QLabel("Total planned income: $0.00")
        layout.addWidget(self.income_label)

    def _set_default_dates(self) -> None:
        end = QtCore.QDate.currentDate()
        start = end.addDays(-30)
        self.start_date.setDate(start)
        self.end_date.setDate(end)

    # ------------------------------------------------------------------
    def refresh_report(self) -> None:
        expenses = self.data_store.load_expenses()
        budgets = self.data_store.load_category_budget()
        income = self.data_store.load_income_sources()

        if expenses.empty:
            self.summary_table.clear()
            self.summary_table.setRowCount(0)
            self.summary_table.setColumnCount(0)
            self.over_list.clear()
            self.under_list.clear()
            self.figure.clear()
            self.canvas.draw()
            self.income_label.setText("No expenses recorded yet.")
            return

        start = _qdate_to_date(self.start_date.date())
        end = _qdate_to_date(self.end_date.date())
        expenses["date"] = pd.to_datetime(expenses["date"])
        mask = (expenses["date"].dt.date >= start) & (expenses["date"].dt.date <= end)
        filtered = expenses.loc[mask]
        if filtered.empty:
            summary = pd.DataFrame(columns=["category", "amount"])
        else:
            summary = (
                filtered.groupby("category", as_index=False)["amount"].sum().sort_values(
                    "amount", ascending=False
                )
            )

        merged = budgets.copy()
        merged = merged.rename(columns={"target_amount": "target"})
        merged = merged.merge(summary, on="category", how="outer")
        merged["target"] = merged["target"].fillna(0.0)
        merged["amount"] = merged["amount"].fillna(0.0)
        merged["difference"] = merged["target"] - merged["amount"]
        merged["progress_pct"] = merged.apply(
            lambda row: 0.0 if row["target"] == 0 else (row["amount"] / row["target"]) * 100,
            axis=1,
        )
        merged = merged.sort_values("amount", ascending=False)

        headers = ["Category", "Spent", "Target", "Remaining", "% of target"]
        self.summary_table.setRowCount(len(merged.index))
        self.summary_table.setColumnCount(len(headers))
        self.summary_table.setHorizontalHeaderLabels(headers)
        for row_idx, (_, row) in enumerate(merged.iterrows()):
            values = [
                row["category"],
                f"${row['amount']:.2f}",
                f"${row['target']:.2f}",
                f"${row['difference']:.2f}",
                f"{row['progress_pct']:.1f}%",
            ]
            for col_idx, value in enumerate(values):
                item = QtWidgets.QTableWidgetItem(value)
                if col_idx > 0:
                    item.setTextAlignment(QtCore.Qt.AlignRight | QtCore.Qt.AlignVCenter)
                self.summary_table.setItem(row_idx, col_idx, item)
        self.summary_table.resizeColumnsToContents()

        over_budget = merged[merged["difference"] < 0]
        under_budget = merged[merged["difference"] > 0]
        self.over_list.clear()
        for _, row in over_budget.sort_values("difference").iterrows():
            self.over_list.addItem(f"{row['category']}: over by ${abs(row['difference']):.2f}")
        self.under_list.clear()
        for _, row in under_budget.sort_values("difference", ascending=False).iterrows():
            self.under_list.addItem(f"{row['category']}: ${row['difference']:.2f} remaining")

        self.figure.clear()
        axis = self.figure.add_subplot(111)
        axis.set_title("Spending by category")
        if not summary.empty:
            axis.bar(summary["category"], summary["amount"], color="#2E86AB")
            axis.set_ylabel("Amount ($)")
            axis.tick_params(axis="x", rotation=45, ha="right")
        else:
            axis.text(0.5, 0.5, "No expenses in range", ha="center", va="center")
        self.figure.tight_layout()
        self.canvas.draw()

        total_income = income["amount"].sum() if not income.empty else 0.0
        total_spent = merged["amount"].sum()
        self.income_label.setText(
            f"Total planned income: ${total_income:.2f} — Spending in range: ${total_spent:.2f}"
        )


class MainWindow(QtWidgets.QMainWindow):
    def __init__(self, data_store: BudgetDataStore, parent: Optional[QtWidgets.QWidget] = None) -> None:
        super().__init__(parent)
        self.setWindowTitle("Household Budgeting Tool")
        self.resize(1100, 700)

        tabs = QtWidgets.QTabWidget(self)
        self.expenses_tab = ExpensesTab(data_store, self)
        self.budget_tab = BudgetTab(data_store, self)
        self.reports_tab = ReportsTab(data_store, self)

        tabs.addTab(self.expenses_tab, "Expenses")
        tabs.addTab(self.budget_tab, "Budget planning")
        tabs.addTab(self.reports_tab, "Reports")
        self.setCentralWidget(tabs)

        self.expenses_tab.data_saved.connect(lambda _: self.reports_tab.refresh_report())
        self.budget_tab.data_saved.connect(self.reports_tab.refresh_report)
        self.budget_tab.data_saved.connect(
            lambda: self.expenses_tab._update_form_choices(self.expenses_tab.model.to_dataframe())
        )


def _with_label(title: str, widget: QtWidgets.QWidget) -> QtWidgets.QWidget:
    container = QtWidgets.QWidget()
    layout = QtWidgets.QVBoxLayout(container)
    label = QtWidgets.QLabel(title)
    label.setAlignment(QtCore.Qt.AlignHCenter)
    layout.addWidget(label)
    layout.addWidget(widget)
    return container


def _qdate_to_date(value: QtCore.QDate) -> date:
    if hasattr(value, "toPython"):
        return value.toPython()
    return value.toPyDate()


def main() -> None:
    base_dir = Path.cwd()
    data_store = BudgetDataStore(base_dir / "user_data")

    app = QtWidgets.QApplication(sys.argv)
    app.setWindowIcon(QtGui.QIcon.fromTheme("document-edit"))
    window = MainWindow(data_store)
    window.show()
    sys.exit(app.exec())


if __name__ == "__main__":
    main()
