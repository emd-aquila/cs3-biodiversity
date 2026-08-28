#!/usr/bin/env python3
"""Conservatively classify Living Planet Database unit labels for OV integration."""

from __future__ import annotations

import csv
import re
from collections import Counter
from pathlib import Path


PROJECT_DIR = Path(__file__).resolve().parents[2]
INPUT_PATH = PROJECT_DIR / "00_biodiversity_data/living_planet/LPD_2024_public.csv"
OUTPUT_DIR = Path(__file__).resolve().parent
UNIT_OUTPUT_PATH = OUTPUT_DIR / "lpd_units_classification.csv"
SUMMARY_OUTPUT_PATH = OUTPUT_DIR / "lpd_units_summary.csv"


def normalized(text: str) -> str:
    text = text.casefold().replace("²", "2").replace("³", "3")
    text = re.sub(r"[\u0000-\u001f]+", " ", text)
    text = re.sub(r"\s+", " ", text)
    return text.strip()


def has(pattern: str, text: str) -> bool:
    return re.search(pattern, text, flags=re.IGNORECASE) is not None


def classify(unit: str) -> tuple[str, str]:
    u = normalized(unit)

    # Metrics that cannot be combined as individual abundance without returning
    # to the source data and applying a model or conversion.
    rejected_rules = (
        (
            r"\b(biomass|bio-mass|spawning stock|standing stock|ssb|bpue|weight|"
            r"kilograms?|kilogrammes?|kg\b|grams?|grammes?|g\b|tonnes?|tons?|"
            r"metric tons?|\bmt\b|pounds?|lbs?|landings|yield)\b",
            "biomass_or_weight",
        ),
        (
            r"\b(density|densities)\b|(?:/|\bper\s+)(?:[0-9,.]+\s*)?"
            r"(?:sq\.?\s*)?(?:km2|km\^?2|"
            r"km²|kilometres?\s+squared|kilometers?\s+squared|ha\b|hectares?|"
            r"m2|m\^?2|m²|metres?\s+squared|meters?\s+squared|100m2|750m2|"
            r"2000m2|acre|acres)\b|\b(?:ha|km|m)[-–—]?2\b|"
            r"\b(?:ha|km|m)\s*[-^]?\s*1\b|(?:m|km)â[¯]?[â²]+|sq\.?\s*km",
            "density_or_area_standardized",
        ),
        (
            r"\b(cpue|catch per unit effort|catch rate|capture rate|trapping success|"
            r"trap success|detection rate|encounter rate|sighting rate|sightings per|"
            r"kilometric|per unit effort|unit effort|effort standardized|"
            r"effort standardised)\b",
            "effort_standardized_rate",
        ),
        (
            r"(?:/|\bper\s+)(?:[0-9,.]+\s*)?(?:trap[- ]?nights?|trap[- ]?days?|"
            r"net[- ]?hours?|net[- ]?nights?|hook[- ]?hours?|hooks?|tows?|hauls?|"
            r"trawls?|gillnets?|nets?|seines?|transects?|routes?|stations?|points?|"
            r"samples?|sampling points?|sampling effort|surveyed km|surveys?|visits?|"
            r"sites?|localities?|plots?|trapping lines?|trapping sites?|traps?|"
            r"pitfall[- ]?days?|ringed birds?|volunteer respondents?|tours?|"
            r"observer[- ]?hours?|person[- ]?hours?|person[- ]?days?|fisher(?:men)?|"
            r"anglers?|boats?|vessels?|dives?|nights?|hours?|minutes?|days?|"
            r"catching hours?|fishing days?|moon phases?|"
            r"kilometres?|kilometers?|km\b|metres?|meters?|m\b)\b",
            "effort_or_distance_standardized",
        ),
        (
            r"\b(index|indices|relative abundance|relative biomass|relative density|"
            r"percentage|percent|proportion|occupancy|occurrence|frequency|"
            r"reporting rate|change relative|population change|trend|base year|"
            r"scaled|normalised|normalized|standardized index|standardised index|"
            r"trim index|we[bs]+ index)\b|%",
            "index_relative_or_occupancy",
        ),
        (
            r"\b(nests?|nesting sites?|burrows?|holes?|tracks?|trackways?|signs?|"
            r"calls?|calling activity|territories|territorial sites?|occupied sites?|"
            r"breeding sites?|dens\b|den sites?|lodges?|setts?|spraints?|scats?|"
            r"redds?|egg masses?|eggs?|clutches?|colonies|colony sites?|"
            r"bat passes?|bird-days?|animal-days?)\b",
            "proxy_structure_sign_or_activity",
        ),
        (
            r"\b(proxy|apparently occupied|sites positive|positive sites|"
            r"presence(?:s)?|presences/surveys|scent stations?|camera trap events?|"
            r"independent events?|roadkills?|roadkilled|harvest|hunting bags?)\b",
            "other_abundance_proxy",
        ),
    )
    for pattern, reason in rejected_rules:
        if has(pattern, u):
            return "rejected", reason

    # Catch idiosyncratic denominator wording and encoding variants that evade
    # the more descriptive rules above. Annual/seasonal reporting periods are
    # not sampling-effort denominators and remain eligible for later rules.
    if has(r"\bper\b|/", u) and not has(
        r"\bper\s+(?:calendar\s+)?(?:year|annum|season|month)\b|"
        r"\bannual count/individuals\b",
        u,
    ):
        return "rejected", "other_denominator_or_rate"
    if has(r"\b(trap nights?|net hours?|hook hours?)\s*\^?[-–—]?1\b", u):
        return "rejected", "effort_or_distance_standardized"

    # These may be convertible or biologically defensible, but not safely usable
    # as individual abundance without a documented decision.
    uncertain_rules = (
        (
            r"\b(unknown|not determined|undetermined|unspecified|assume individuals|"
            r"probably|no units?|unit not given)\b",
            "unit_unknown_or_inferred",
        ),
        (
            r"\b(pairs?|breeding pairs?|nesting pairs?|couples?)\b",
            "pairs_require_conversion",
        ),
        (
            r"\b(adults?|mature individuals?|males?|females?|females with|"
            r"breeding females?|breeding males?|breeding individuals?|breeders?|"
            r"breeding birds?|breeding population|breeding numbers?|"
            r"nesting individuals?|nesting turtles?|nesting females?|"
            r"turtles nesting|birds nesting|animals nesting|"
            r"adult abundance|mature abundance|female abundance|male abundance|"
            r"juveniles?|young|young-of-the-year|fry\b|larvae|larval|pups?|"
            r"chicks?|fledglings?|calves?|smolts?|spawners?|escapement|"
            r"recruits?|age[- ]?[0-9]+|yearlings?|broods?|hibernating|"
            r"overwintering|winter residents?)\b",
            "demographic_subset",
        ),
        (
            r"\b(groups?|packs?|schools?|herds?)\b",
            "group_count_not_individual_count",
        ),
        (
            r"^(?:annual |monthly |yearly |daily |mean |average |maximum |minimum |"
            r"peak |total |estimated |observed |adjusted |winter |spring |autumn |"
            r"fall |midwinter |mid-winter |mid-january |february )*"
            r"(?:abundance|population)$",
            "generic_abundance_wording",
        ),
    )
    for pattern, reason in uncertain_rules:
        if has(pattern, u):
            return "unsure", reason

    # Explicit direct counts or estimates of organisms. Averages and maxima are
    # retained because BioTIME/PREDICTS also include sampling-based abundance,
    # but effort- and area-standardized values were removed above.
    accepted_rules = (
        (
            r"^sample:\s*abundance\s*\(counts?\)$",
            "explicit_sample_abundance_count",
        ),
        (
            r"\b(individuals?|individ(?:ual)?s?|animals?|birds?|fish|fishes|frogs?|"
            r"salamanders?|newts?|turtles?|whales?|dolphins?|seals?|penguins?|"
            r"sharks?|snakes?|lizards?|deer|zebra|tigers?|lions?|hyenas?|"
            r"gazelles?|pheasants?|anatids?|bats?|hares?|rabbits?|squirrels?|"
            r"monkeys?|gorillas?|chimpanzees?|manatees?|porpoises?|eels?)\b",
            "explicit_organism_count",
        ),
        (
            r"\b(population size|population estimate|estimated population|"
            r"total population|population abundance|abundance estimate|"
            r"estimated abundance|total abundance|annual abundance estimates?)\b",
            "explicit_population_or_abundance_estimate",
        ),
        (
            r"^(?:annual |monthly |yearly |daily |mean |average |maximum |minimum |"
            r"peak |total |estimated |observed |adjusted |winter |spring |autumn |"
            r"fall |midwinter |mid-winter |mid-january |february )*"
            r"(?:number|numbers|count|counts|census counts)$",
            "generic_direct_count_wording",
        ),
        (
            r"\b(total count|direct counts?|census counts?|number sighted|"
            r"number observed|number caught|number captured|numbers recorded|"
            r"count of organisms|annual total number|maximum count|minimum number alive|"
            r"minimum number known alive)\b",
            "direct_observation_or_capture_count",
        ),
    )
    for pattern, reason in accepted_rules:
        if has(pattern, u):
            return "accepted", reason

    return "unsure", "ambiguous_free_text_unit"


def main() -> None:
    unit_counts: Counter[str] = Counter()
    ids: set[str] = set()
    total_rows = 0

    with INPUT_PATH.open(encoding="utf-8-sig", newline="") as source:
        reader = csv.DictReader(source)
        for row in reader:
            total_rows += 1
            ids.add(row["ID"])
            unit_counts[row["Units"].strip()] += 1

    records = []
    for unit, time_series in unit_counts.items():
        decision, reason = classify(unit)
        records.append(
            {
                "decision": decision,
                "reason": reason,
                "time_series": time_series,
                "units": unit,
            }
        )
    decision_order = {"accepted": 0, "unsure": 1, "rejected": 2}
    records.sort(
        key=lambda row: (
            decision_order[row["decision"]],
            -row["time_series"],
            normalized(row["units"]),
        )
    )

    with UNIT_OUTPUT_PATH.open("w", encoding="utf-8", newline="") as destination:
        writer = csv.DictWriter(
            destination,
            fieldnames=("decision", "reason", "time_series", "units"),
        )
        writer.writeheader()
        writer.writerows(records)

    for decision in ("accepted", "unsure", "rejected"):
        decision_path = OUTPUT_DIR / f"lpd_units_{decision}.csv"
        with decision_path.open("w", encoding="utf-8", newline="") as destination:
            writer = csv.DictWriter(
                destination,
                fieldnames=("reason", "time_series", "units"),
            )
            writer.writeheader()
            writer.writerows(
                {
                    "reason": row["reason"],
                    "time_series": row["time_series"],
                    "units": row["units"],
                }
                for row in records
                if row["decision"] == decision
            )

    series_by_decision = Counter()
    labels_by_decision = Counter()
    for row in records:
        series_by_decision[row["decision"]] += row["time_series"]
        labels_by_decision[row["decision"]] += 1

    summary_rows = [
        {
            "category": "full_database",
            "unit_labels": len(unit_counts),
            "time_series": total_rows,
            "share_of_time_series": 1.0,
        }
    ]
    for decision in ("accepted", "unsure", "rejected"):
        summary_rows.append(
            {
                "category": decision,
                "unit_labels": labels_by_decision[decision],
                "time_series": series_by_decision[decision],
                "share_of_time_series": series_by_decision[decision] / total_rows,
            }
        )
    summary_rows.append(
        {
            "category": "excluded_by_strict_accepted_only_filter",
            "unit_labels": labels_by_decision["unsure"] + labels_by_decision["rejected"],
            "time_series": series_by_decision["unsure"] + series_by_decision["rejected"],
            "share_of_time_series": (
                series_by_decision["unsure"] + series_by_decision["rejected"]
            )
            / total_rows,
        }
    )

    with SUMMARY_OUTPUT_PATH.open("w", encoding="utf-8", newline="") as destination:
        writer = csv.DictWriter(
            destination,
            fieldnames=(
                "category",
                "unit_labels",
                "time_series",
                "share_of_time_series",
            ),
        )
        writer.writeheader()
        writer.writerows(summary_rows)

    if total_rows != len(ids):
        raise RuntimeError(
            f"Expected one unique ID per row, found {total_rows} rows and {len(ids)} IDs"
        )


if __name__ == "__main__":
    main()
