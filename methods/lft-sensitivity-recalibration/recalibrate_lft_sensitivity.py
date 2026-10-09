"""Recalibrating SARS-CoV-2 antigen lateral flow test (LFT) sensitivity.

Python port of recalibrate_lft_sensitivity.R (see that file for the model,
notation and assumptions). Implements the formula in:

    Petersen I, Crozier A, Buchan I, Mina MJ, Bartlett JW. Recalibrating
    SARS-CoV-2 antigen rapid lateral flow test relative sensitivity from
    validation studies to absolute sensitivity for indicating individuals
    shedding transmissible virus. Clin Epidemiol 2021;13:935-940.
    https://doi.org/10.2147/CLEP.S311977

    absolute sensitivity  P(LFT+ | shedding antigen)  =  P(LFT+ | PCR+) / pi

where pi is the proportion of PCR-positive individuals who are shedding
antigen. No dependencies beyond the standard library.

Run ``python3 recalibrate_lft_sensitivity.py`` to self-test against Table 1
of the paper and print it.
"""

from __future__ import annotations

from decimal import ROUND_HALF_UP, Decimal
from typing import Optional


def recalibrate_lft_sensitivity(apparent_sensitivity: float, pi: float) -> Optional[float]:
    """Absolute sensitivity from apparent (PCR-relative) sensitivity.

    Returns None where apparent_sensitivity exceeds pi, which is not possible
    under the model (shown as a dash in the paper's Table 1).
    """
    if not 0.0 <= apparent_sensitivity <= 1.0:
        raise ValueError("apparent_sensitivity must be a proportion between 0 and 1")
    if not 0.0 < pi <= 1.0:
        raise ValueError("pi must be a proportion greater than 0 and at most 1")
    if apparent_sensitivity > pi + 1e-9:
        return None
    return apparent_sensitivity / pi


def implied_pi(apparent_sensitivity: float, absolute_sensitivity: float) -> float:
    """The value of pi implied by a pair of apparent and absolute sensitivities."""
    if not 0.0 < absolute_sensitivity <= 1.0:
        raise ValueError("absolute_sensitivity must be a proportion greater than 0 and at most 1")
    return apparent_sensitivity / absolute_sensitivity


def percent_half_up(x: Optional[float]) -> Optional[int]:
    """Whole-number percentage rounded half up, as printed in the paper."""
    if x is None:
        return None
    return int(Decimal(repr(x * 100)).quantize(Decimal("1"), rounding=ROUND_HALF_UP))


def recalibration_table(apparent=None, pi=None):
    """Reproduce Table 1 of the paper as (pi_values, apparent_values, rows)."""
    apparent = apparent or [round(0.30 + 0.05 * i, 2) for i in range(11)]
    pi = pi or [round(0.300 + 0.025 * i, 3) for i in range(21)]
    rows = [[recalibrate_lft_sensitivity(a, p) for a in apparent] for p in pi]
    return pi, apparent, rows


# Table 1 of the paper, transcribed (None = dash).
_PAPER_TABLE = [
    [100, None, None, None, None, None, None, None, None, None, None],
    [92, None, None, None, None, None, None, None, None, None, None],
    [86, 100, None, None, None, None, None, None, None, None, None],
    [80, 93, None, None, None, None, None, None, None, None, None],
    [75, 88, 100, None, None, None, None, None, None, None, None],
    [71, 82, 94, None, None, None, None, None, None, None, None],
    [67, 78, 89, 100, None, None, None, None, None, None, None],
    [63, 74, 84, 95, None, None, None, None, None, None, None],
    [60, 70, 80, 90, 100, None, None, None, None, None, None],
    [57, 67, 76, 86, 95, None, None, None, None, None, None],
    [55, 64, 73, 82, 91, 100, None, None, None, None, None],
    [52, 61, 70, 78, 87, 96, None, None, None, None, None],
    [50, 58, 67, 75, 83, 92, 100, None, None, None, None],
    [48, 56, 64, 72, 80, 88, 96, None, None, None, None],
    [46, 54, 62, 69, 77, 85, 92, 100, None, None, None],
    [44, 52, 59, 67, 74, 81, 89, 96, None, None, None],
    [43, 50, 57, 64, 71, 79, 86, 93, 100, None, None],
    [41, 48, 55, 62, 69, 76, 83, 90, 97, None, None],
    [40, 47, 53, 60, 67, 73, 80, 87, 93, 100, None],
    [39, 45, 52, 58, 65, 71, 77, 84, 90, 97, None],
    [38, 44, 50, 56, 63, 69, 75, 81, 88, 94, 100],
]


def _self_test() -> None:
    pi, apparent, rows = recalibration_table()
    computed = [[percent_half_up(round(v, 8)) if v is not None else None for v in row] for row in rows]
    assert computed == _PAPER_TABLE, "Table 1 not reproduced"
    assert abs(recalibrate_lft_sensitivity(0.40, 0.5) - 0.80) < 1e-12
    assert recalibrate_lft_sensitivity(0.55, 0.5) is None
    assert abs(implied_pi(0.40, 0.80) - 0.5) < 1e-12
    print("Table 1 reproduced (121 cells, 110 dashes); worked example 40% -> 80% at pi = 0.5")
    header = "pi     " + " ".join(f"{int(a * 100):>4d}%" for a in apparent)
    print(header)
    for p, row in zip(pi, computed):
        print(f"{p:.3f}  " + " ".join(f"{v:>4d}%" if v is not None else "    -" for v in row))


if __name__ == "__main__":
    _self_test()
