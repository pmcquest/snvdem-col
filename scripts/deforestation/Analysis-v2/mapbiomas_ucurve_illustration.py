import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

# MapBiomas decile means, mean d_forest_pct by sndem decile (1=least democratic, 10=most)
# Source: deforestation_update_v2.md, Section 6 "Non-parametric check" table (MapBiomas row)
deciles = np.arange(1, 11)
values = np.array([-0.131, -0.130, -0.152, -0.104, -0.082, -0.062, -0.073, -0.053, -0.032, 0.071])

# Quadratic fit through the ten decile means themselves (illustrates the U-shape
# non-parametrically -- this is a fit to the bars shown, not a re-plot of the
# FE-adjusted regression coefficients, which live on a different, demeaned scale).
coeffs = np.polyfit(deciles, values, 2)
xs = np.linspace(1, 10, 200)
curve = np.polyval(coeffs, xs)
a, b, _ = coeffs
turning_point_decile = -b / (2 * a)

POS = "#2a78d6"   # blue -- net gain
NEG = "#e34948"   # red -- diverging pole, net loss
INK = "#0b0b0b"
SEC = "#52514e"
MUT = "#898781"
GRID = "#e1e0d9"
BASE = "#c3c2b7"
SURF = "#fcfcfb"

fig, ax = plt.subplots(figsize=(7.5, 5), dpi=200)
fig.patch.set_facecolor(SURF)
ax.set_facecolor(SURF)

colors = [POS if v >= 0 else NEG for v in values]
ax.bar(deciles, values, color=colors, width=0.62, zorder=3)

ax.plot(xs, curve, color=INK, linewidth=1.6, linestyle=(0, (5, 3)), zorder=4,
        label="Quadratic fit through decile means")

ax.axhline(0, color=BASE, linewidth=1, zorder=2)

ax.set_xlabel("Municipal democratic quality decile (sndem), 1 = lowest, 10 = highest", color=SEC)
ax.set_ylabel("Mean annual forest-cover change (percentage points)", color=SEC)
ax.set_title(
    "Subnational forest-cover change follows a U-shape across democracy deciles --\n"
    "consistent with Boehmelt & Bernauer's (2025) national-level finding",
    color=INK, fontsize=12, loc="left", pad=12
)
ax.set_xticks(deciles)
ax.grid(axis="y", color=GRID, linewidth=0.8, zorder=0)
ax.spines[["top", "right"]].set_visible(False)
ax.spines[["left", "bottom"]].set_color(BASE)
ax.tick_params(colors=MUT)

ax.legend(frameon=False, loc="upper center", labelcolor=INK, fontsize=9)

ax.text(0.99, 0.03,
        "Source: MapBiomas Colombia Collection 3.0, 1,103 municipalities, 2000-2023.\n"
        "Bars = mean change within each sndem decile (no functional-form assumption).\n"
        "FE regression (Section 4, memo v2): turning point = 0.487 on the normalized 0-1 scale.",
        transform=ax.transAxes, ha="right", va="bottom", fontsize=7.5, color=MUT)

fig.tight_layout()
out_path = r"G:\Shared drives\snvdem\snvdem-col\scripts\deforestation\Analysis-v2\mapbiomas_ucurve_illustration.png"
fig.savefig(out_path, facecolor=fig.get_facecolor())
print("Saved:", out_path)
print("Decile-space turning point (raw, non-parametric fit):", round(turning_point_decile, 2), "of 10")
