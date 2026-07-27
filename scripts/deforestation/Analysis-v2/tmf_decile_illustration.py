import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

# TMF decile means, mean d_forest_pct by sndem decile (1=least democratic, 10=most)
# Source: deforestation_update_v2.md, Section 6 "Non-parametric check" table (TMF row)
deciles = np.arange(1, 11)
values = np.array([-0.088, -0.047, -0.020, 0.037, 0.116, 0.178, 0.251, 0.293, 0.380, 0.516])

# OLS trend line through the decile means (illustrates "linear, not curvilinear")
b1, b0 = np.polyfit(deciles, values, 1)
trend = b0 + b1 * deciles

POS = "#2a78d6"   # blue -- same "TMF" blue used across the team's other figures
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
bars = ax.bar(deciles, values, color=colors, width=0.62, zorder=3)

ax.plot(deciles, trend, color=INK, linewidth=1.6, linestyle=(0, (5, 3)), zorder=4,
        label="Linear trend across deciles")

ax.axhline(0, color=BASE, linewidth=1, zorder=2)

ax.set_xlabel("Municipal democratic quality decile (sndem), 1 = lowest, 10 = highest", color=SEC)
ax.set_ylabel("Mean annual forest-cover change (percentage points)", color=SEC)
ax.set_title(
    "More democratic municipalities see better forest-cover outcomes --\n"
    "a steady rise across deciles",
    color=INK, fontsize=12, loc="left", pad=12
)
ax.set_xticks(deciles)
ax.grid(axis="y", color=GRID, linewidth=0.8, zorder=0)
ax.spines[["top", "right"]].set_visible(False)
ax.spines[["left", "bottom"]].set_color(BASE)
ax.tick_params(colors=MUT)

leg = ax.legend(frameon=False, loc="upper left", labelcolor=INK, fontsize=9)

ax.text(0.99, 0.03,
        "Source: TMF (JRC Tropical Moist Forest v1_2025), 1,122 Colombian municipalities, 2000-2023.\n"
        "Bars = mean change within each sndem decile (no functional-form assumption).",
        transform=ax.transAxes, ha="right", va="bottom", fontsize=7.5, color=MUT)

fig.tight_layout()
out_path = r"G:\Shared drives\snvdem\snvdem-col\scripts\deforestation\Analysis-v2\tmf_decile_illustration.png"
fig.savefig(out_path, facecolor=fig.get_facecolor())
print("Saved:", out_path)
print("Trend line slope (per decile):", round(b1, 4))

