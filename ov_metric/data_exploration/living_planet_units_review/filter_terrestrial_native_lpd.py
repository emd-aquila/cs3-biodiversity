#!/usr/bin/env python3
"""Filter LPD to native terrestrial time series and apply the unit screen."""

from __future__ import annotations

import csv
from collections import Counter
from pathlib import Path

from classify_lpd_units import INPUT_PATH, OUTPUT_DIR, classify, normalized


ELIGIBLE_OUTPUT = OUTPUT_DIR / "lpd_terrestrial_native_timeseries.csv"
ACCEPTED_TIMESERIES_OUTPUT = (
    OUTPUT_DIR / "lpd_terrestrial_native_accepted_timeseries.csv"
)
REJECTED_TIMESERIES_OUTPUT = (
    OUTPUT_DIR / "lpd_terrestrial_native_rejected_timeseries.csv"
)
CLASSIFICATION_OUTPUT = OUTPUT_DIR / "lpd_terrestrial_native_units_classification.csv"
ACCEPTED_OUTPUT = OUTPUT_DIR / "lpd_terrestrial_native_units_accepted.csv"
REJECTED_OUTPUT = OUTPUT_DIR / "lpd_terrestrial_native_units_rejected.csv"
SUMMARY_OUTPUT = OUTPUT_DIR / "lpd_terrestrial_native_filter_summary.csv"


def main() -> None:
    all_rows: list[dict[str, str]] = []
    with INPUT_PATH.open(encoding="utf-8-sig", newline="") as source:
        reader = csv.DictReader(source)
        source_fields = [field for field in reader.fieldnames or [] if field]
        for row in reader:
            all_rows.append({field: row.get(field, "") for field in source_fields})

    terrestrial_rows = [row for row in all_rows if row["System"].strip() == "Terrestrial"]
    eligible_rows = [row for row in terrestrial_rows if row["Native"].strip() == "1"]

    non_terrestrial_rows = len(all_rows) - len(terrestrial_rows)
    non_native_rows_overall = sum(row["Native"].strip() != "1" for row in all_rows)
    non_native_rows_after_terrestrial = len(terrestrial_rows) - len(eligible_rows)
    filter_overlap = non_native_rows_overall - non_native_rows_after_terrestrial

    with ELIGIBLE_OUTPUT.open("w", encoding="utf-8", newline="") as destination:
        writer = csv.DictWriter(destination, fieldnames=source_fields)
        writer.writeheader()
        writer.writerows(eligible_rows)

    unit_counts = Counter(row["Units"].strip() for row in eligible_rows)
    records = []
    for unit, time_series in unit_counts.items():
        three_way_decision, reason = classify(unit)
        binary_decision = (
            "accepted" if three_way_decision == "accepted" else "rejected"
        )
        records.append(
            {
                "decision": binary_decision,
                "review_status": three_way_decision,
                "reason": reason,
                "time_series": time_series,
                "units": unit,
            }
        )
    records.sort(
        key=lambda row: (
            0 if row["decision"] == "accepted" else 1,
            0 if row["review_status"] == "rejected" else 1,
            -row["time_series"],
            normalized(row["units"]),
        )
    )
    binary_by_unit = {row["units"]: row["decision"] for row in records}

    for decision, output_path in (
        ("accepted", ACCEPTED_TIMESERIES_OUTPUT),
        ("rejected", REJECTED_TIMESERIES_OUTPUT),
    ):
        with output_path.open("w", encoding="utf-8", newline="") as destination:
            writer = csv.DictWriter(destination, fieldnames=source_fields)
            writer.writeheader()
            writer.writerows(
                row
                for row in eligible_rows
                if binary_by_unit[row["Units"].strip()] == decision
            )

    classification_fields = (
        "decision",
        "review_status",
        "reason",
        "time_series",
        "units",
    )
    with CLASSIFICATION_OUTPUT.open(
        "w", encoding="utf-8", newline=""
    ) as destination:
        writer = csv.DictWriter(destination, fieldnames=classification_fields)
        writer.writeheader()
        writer.writerows(records)

    list_fields = ("review_status", "reason", "time_series", "units")
    for decision, output_path in (
        ("accepted", ACCEPTED_OUTPUT),
        ("rejected", REJECTED_OUTPUT),
    ):
        with output_path.open("w", encoding="utf-8", newline="") as destination:
            writer = csv.DictWriter(destination, fieldnames=list_fields)
            writer.writeheader()
            writer.writerows(
                {field: row[field] for field in list_fields}
                for row in records
                if row["decision"] == decision
            )

    labels_by_binary = Counter(row["decision"] for row in records)
    series_by_binary = Counter()
    labels_by_review = Counter(row["review_status"] for row in records)
    series_by_review = Counter()
    for row in records:
        series_by_binary[row["decision"]] += row["time_series"]
        series_by_review[row["review_status"]] += row["time_series"]

    summary_rows = [
        ("full_database", len(all_rows), ""),
        ("removed_non_terrestrial", non_terrestrial_rows, "standalone_and_first_filter"),
        ("remaining_terrestrial", len(terrestrial_rows), "after_system_filter"),
        ("removed_non_native_overall", non_native_rows_overall, "standalone"),
        (
            "non_native_already_removed_as_non_terrestrial",
            filter_overlap,
            "filter_overlap",
        ),
        (
            "removed_non_native_after_terrestrial",
            non_native_rows_after_terrestrial,
            "second_sequential_filter",
        ),
        ("remaining_terrestrial_native", len(eligible_rows), "final_eligible_subset"),
        ("remaining_unique_unit_labels", len(unit_counts), "final_eligible_subset"),
        ("accepted_unit_labels", labels_by_binary["accepted"], "binary_unit_screen"),
        ("accepted_time_series", series_by_binary["accepted"], "binary_unit_screen"),
        ("rejected_unit_labels", labels_by_binary["rejected"], "binary_unit_screen"),
        ("rejected_time_series", series_by_binary["rejected"], "binary_unit_screen"),
        (
            "rejected_time_series_definite",
            series_by_review["rejected"],
            "original_three_way_screen",
        ),
        (
            "rejected_time_series_conservative_unsure",
            series_by_review["unsure"],
            "original_three_way_screen",
        ),
        (
            "rejected_unit_labels_definite",
            labels_by_review["rejected"],
            "original_three_way_screen",
        ),
        (
            "rejected_unit_labels_conservative_unsure",
            labels_by_review["unsure"],
            "original_three_way_screen",
        ),
    ]
    with SUMMARY_OUTPUT.open("w", encoding="utf-8", newline="") as destination:
        writer = csv.DictWriter(
            destination, fieldnames=("metric", "count", "notes")
        )
        writer.writeheader()
        writer.writerows(
            {"metric": metric, "count": count, "notes": notes}
            for metric, count, notes in summary_rows
        )

    if len(all_rows) != 35_996:
        raise RuntimeError(f"Unexpected LPD row count: {len(all_rows)}")
    if len(eligible_rows) != 11_702:
        raise RuntimeError(
            f"Unexpected native terrestrial row count: {len(eligible_rows)}"
        )
    if sum(unit_counts.values()) != len(eligible_rows):
        raise RuntimeError("Unit counts do not sum to the eligible time-series count")


if __name__ == "__main__":
    main()
