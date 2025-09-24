"""Persistence layer for the budgeting desktop application."""
from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
import shutil
from typing import List

import pandas as pd

EXPENSE_COLUMNS = [
    "date",
    "description",
    "category",
    "amount",
    "payer",
    "account",
]

INCOME_COLUMNS = ["source", "amount"]
BUDGET_COLUMNS = ["category", "target_amount"]


@dataclass
class StoragePaths:
    """Collection of file paths used by the data store."""

    expenses: Path
    expenses_backup: Path
    income_sources: Path
    category_budget: Path


class BudgetDataStore:
    """Read and write budgeting data to CSV files on disk."""

    def __init__(self, base_dir: Path | str | None = None) -> None:
        root = Path(base_dir) if base_dir is not None else Path.cwd() / "user_data"
        root.mkdir(parents=True, exist_ok=True)
        self.paths = StoragePaths(
            expenses=root / "expenses.csv",
            expenses_backup=root / "expenses_backup.csv",
            income_sources=root / "income_sources.csv",
            category_budget=root / "category_budget.csv",
        )
        self._ensure_defaults()

    # ------------------------------------------------------------------
    # public API
    def load_expenses(self) -> pd.DataFrame:
        return self._read_csv(self.paths.expenses, EXPENSE_COLUMNS, numeric_columns=["amount"])

    def save_expenses(self, df: pd.DataFrame) -> None:
        cleaned = self._prepare_expense_frame(df)
        if self.paths.expenses.exists():
            shutil.copy2(self.paths.expenses, self.paths.expenses_backup)
        cleaned.to_csv(self.paths.expenses, index=False)

    def load_income_sources(self) -> pd.DataFrame:
        return self._read_csv(
            self.paths.income_sources, INCOME_COLUMNS, numeric_columns=["amount"]
        )

    def save_income_sources(self, df: pd.DataFrame) -> None:
        cleaned = self._prepare_numeric_frame(df, INCOME_COLUMNS, amount_field="amount")
        cleaned.to_csv(self.paths.income_sources, index=False)

    def load_category_budget(self) -> pd.DataFrame:
        return self._read_csv(
            self.paths.category_budget, BUDGET_COLUMNS, numeric_columns=["target_amount"]
        )

    def save_category_budget(self, df: pd.DataFrame) -> None:
        cleaned = self._prepare_numeric_frame(
            df, BUDGET_COLUMNS, amount_field="target_amount"
        )
        cleaned.to_csv(self.paths.category_budget, index=False)

    # ------------------------------------------------------------------
    # helpers
    def _read_csv(
        self, path: Path, columns: List[str], numeric_columns: List[str] | None = None
    ) -> pd.DataFrame:
        if not path.exists():
            return pd.DataFrame(columns=columns)
        df = pd.read_csv(path)
        for column in columns:
            if column not in df.columns:
                if numeric_columns and column in numeric_columns:
                    df[column] = 0.0
                else:
                    df[column] = ""
        df = df[columns].copy()
        if numeric_columns:
            for column in numeric_columns:
                df[column] = pd.to_numeric(df[column], errors="coerce").fillna(0.0)
        return df

    def _prepare_expense_frame(self, df: pd.DataFrame) -> pd.DataFrame:
        frame = df.copy()
        for column in EXPENSE_COLUMNS:
            if column not in frame.columns:
                frame[column] = ""
        frame = frame[EXPENSE_COLUMNS]
        dates = pd.to_datetime(frame["date"], errors="coerce")
        dates = dates.fillna(pd.Timestamp.today())
        frame["date"] = dates.dt.date.astype(str)
        frame["description"] = frame["description"].fillna("").astype(str)
        frame["category"] = frame["category"].fillna("").astype(str)
        frame["payer"] = frame["payer"].fillna("").astype(str)
        frame["account"] = frame["account"].fillna("").astype(str)
        frame["amount"] = pd.to_numeric(frame["amount"], errors="coerce").fillna(0.0)
        return frame

    def _prepare_numeric_frame(
        self, df: pd.DataFrame, columns: List[str], amount_field: str
    ) -> pd.DataFrame:
        frame = df.copy()
        for column in columns:
            if column not in frame.columns:
                frame[column] = "" if column != amount_field else 0.0
        frame = frame[columns]
        frame[columns[0]] = frame[columns[0]].fillna("").astype(str)
        frame[amount_field] = pd.to_numeric(
            frame[amount_field], errors="coerce"
        ).fillna(0.0)
        return frame

    def _ensure_defaults(self) -> None:
        if not self.paths.expenses.exists():
            self._write_default_expenses()
        if not self.paths.income_sources.exists():
            self._write_default_income_sources()
        if not self.paths.category_budget.exists():
            self._write_default_category_budget()

    def _write_default_expenses(self) -> None:
        sample_rows = [
            {
                "date": "2024-03-01",
                "description": "Rent",
                "category": "Housing",
                "amount": 2150.0,
                "payer": "Alex",
                "account": "Checking",
            },
            {
                "date": "2024-03-02",
                "description": "Groceries",
                "category": "Food",
                "amount": 235.42,
                "payer": "Sam",
                "account": "Credit Card",
            },
            {
                "date": "2024-03-03",
                "description": "Auto insurance",
                "category": "Transportation",
                "amount": 165.73,
                "payer": "Alex",
                "account": "Checking",
            },
            {
                "date": "2024-03-05",
                "description": "Dinner out",
                "category": "Dining",
                "amount": 86.15,
                "payer": "Sam",
                "account": "Credit Card",
            },
            {
                "date": "2024-03-07",
                "description": "Internet",
                "category": "Utilities",
                "amount": 78.0,
                "payer": "Alex",
                "account": "Checking",
            },
            {
                "date": "2024-03-09",
                "description": "Gasoline",
                "category": "Transportation",
                "amount": 94.32,
                "payer": "Sam",
                "account": "Debit Card",
            },
            {
                "date": "2024-03-11",
                "description": "Movie night",
                "category": "Entertainment",
                "amount": 38.5,
                "payer": "Alex",
                "account": "Credit Card",
            },
            {
                "date": "2024-03-14",
                "description": "Dog food",
                "category": "Pets",
                "amount": 52.16,
                "payer": "Sam",
                "account": "Credit Card",
            },
            {
                "date": "2024-03-16",
                "description": "Student loan",
                "category": "Debt",
                "amount": 410.0,
                "payer": "Alex",
                "account": "Checking",
            },
            {
                "date": "2024-03-21",
                "description": "Groceries",
                "category": "Food",
                "amount": 189.77,
                "payer": "Sam",
                "account": "Credit Card",
            },
            {
                "date": "2024-03-24",
                "description": "Gym membership",
                "category": "Health",
                "amount": 72.0,
                "payer": "Alex",
                "account": "Debit Card",
            },
            {
                "date": "2024-03-28",
                "description": "Electric bill",
                "category": "Utilities",
                "amount": 134.88,
                "payer": "Alex",
                "account": "Checking",
            },
            {
                "date": "2024-03-30",
                "description": "Charitable donation",
                "category": "Giving",
                "amount": 120.0,
                "payer": "Sam",
                "account": "Checking",
            },
        ]
        df = pd.DataFrame(sample_rows, columns=EXPENSE_COLUMNS)
        df.to_csv(self.paths.expenses, index=False)
        shutil.copy2(self.paths.expenses, self.paths.expenses_backup)

    def _write_default_income_sources(self) -> None:
        sample = pd.DataFrame(
            [
                {"source": "Alex salary", "amount": 4650.0},
                {"source": "Sam salary", "amount": 3100.0},
                {"source": "Savings interest", "amount": 80.0},
            ],
            columns=INCOME_COLUMNS,
        )
        sample.to_csv(self.paths.income_sources, index=False)

    def _write_default_category_budget(self) -> None:
        sample = pd.DataFrame(
            [
                {"category": "Housing", "target_amount": 2200.0},
                {"category": "Food", "target_amount": 650.0},
                {"category": "Transportation", "target_amount": 450.0},
                {"category": "Utilities", "target_amount": 320.0},
                {"category": "Dining", "target_amount": 200.0},
                {"category": "Entertainment", "target_amount": 180.0},
                {"category": "Health", "target_amount": 160.0},
                {"category": "Giving", "target_amount": 150.0},
                {"category": "Pets", "target_amount": 90.0},
                {"category": "Debt", "target_amount": 450.0},
                {"category": "Savings", "target_amount": 1000.0},
            ],
            columns=BUDGET_COLUMNS,
        )
        sample.to_csv(self.paths.category_budget, index=False)
