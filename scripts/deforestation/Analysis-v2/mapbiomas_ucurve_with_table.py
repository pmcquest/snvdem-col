import pyreadr
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from linearmodels.panel import PanelOLS

A2 = r"G:\Shared drives\snvdem\snvdem-col\scripts\deforestation\Analysis-v2"

def rds(path):
    return list(pyreadr.read_r(path).values())[0]

# --- Refit the MapBiomas quadratic FE model directly to get a real covariance matrix ---
# (point estimates match memo v2 / update v2's reported b1=-1.450 (t=-4.4), b2=1.489 (t=5.2)
# to within ordinary small-sample-correction rounding -- used here, not the rounded memo
# figures, so the fitted curve and its CI band are internally consistent.)
dem = rds(r"G:\Shared drives\snvdem\snvdem-col\data\panel\05_weighting\03_output\snvdem_col_weighted.rds")
dem = dem[["MPIO_CDPMP", "year", "sndem"]].drop_duplicates(["MPIO_CDPMP", "year"])
dem["MPIO_CDPMP"] = dem["MPIO_CDPMP"].astype(str).str.zfill(5)
dem["year"] = dem["year"].astype(int)
dem["sndem_norm"] = (dem["sndem"] - dem["sndem"].min()) / (dem["sndem"].max() - dem["sndem"].min())

mb = rds(A2 + r"\mb_muni_cover_corrected.rds")
mb["MPIO_CDPMP"] = mb["MPIO_CDPMP"].astype(str).str.zfill(5)
mb["year"] = mb["year"].astype(int)

m = mb.merge(dem, on=["MPIO_CDPMP", "year"], how="inner")
m = m.dropna(subset=["d_forest_pct", "forest_pct_lag1", "sndem_norm"])
m["sndem_norm2"] = m["sndem_norm"] ** 2
m = m.set_index(["MPIO_CDPMP", "year"])

mod = PanelOLS.from_formula(
    "d_forest_pct ~ 1 + sndem_norm + sndem_norm2 + forest_pct_lag1 + EntityEffects + TimeEffects",
    data=m,
)
res = mod.fit(cov_type="clustered", cluster_entity=True)

b1 = res.params["sndem_norm"]
b2 = res.params["sndem_norm2"]
var_b1 = res.cov.loc["sndem_norm", "sndem_norm"]
var_b2 = res.cov.loc["sndem_norm2", "sndem_norm2"]
cov_b1b2 = res.cov.loc["sndem_norm", "sndem_norm2"]
turning_point = -b1 / (2 * b2)

print(f"b1={b1:.4f} (t={res.tstats['sndem_norm']:.2f}), b2={b2:.4f} (t={res.tstats['sndem_norm2']:.2f}), "
      f"turning point={turning_point:.3f}, N={int(res.nobs)}")

# --- Predicted curve, relative to the least-democratic municipality-year, with a 95% CI band ---
# Delta method: Var(y(x)) = x^2*Var(b1) + x^4*Var(b2) + 2*x^3*Cov(b1,b2)
x = np.linspace(0, 1, 200)
y = b1 * x + b2 * x**2
var_y = (x**2) * var_b1 + (x**4) * var_b2 + 2 * (x**3) * cov_b1b2
se_y = np.sqrt(np.clip(var_y, 0, None))
ci_lo, ci_hi = y - 1.96 * se_y, y + 1.96 * se_y

# --- Bottom panel: raw, non-parametric decile means (independent check, no functional-form assumption) ---
# Source: deforestation_update_v2.md, Section 6 "Non-parametric check" table (MapBiomas row).
decile_labels = [str(k) for k in range(1, 11)]
decile_means = [-0.131, -0.130, -0.152, -0.104, -0.082, -0.062, -0.073, -0.053, -0.032, 0.071]

INK = "#0b0b0b"
SEC = "#52514e"
MUT = "#898781"
GRID = "#e1e0d9"
BASE = "#c3c2b7"
SURF = "#fcfcfb"
CURVE = "#2a78d6"
TP_LINE = "#e34948"

fig = plt.figure(figsize=(8.2, 7.0), dpi=200)
fig.patch.set_facecolor(SURF)

ax0 = fig.add_axes([0.16, 0.40, 0.80, 0.48])
ax1 = fig.add_axes([0.16, 0.20, 0.80, 0.15])

ax0.set_facecolor(SURF)
ax0.fill_between(x, ci_lo, ci_hi, color=CURVE, alpha=0.15, linewidth=0, zorder=3, label="95% CI")
ax0.plot(x, y, color=CURVE, linewidth=2.4, zorder=4, label="Fitted quadratic curve")
ax0.axhline(0, color=BASE, linewidth=1, zorder=1)
ax0.axvline(turning_point, color=TP_LINE, linewidth=1.2, linestyle=(0, (4, 3)), zorder=3)
ax0.annotate(
    f"Turning point ≈ {turning_point:.3f}",
    xy=(turning_point, 0.02),
    xytext=(turning_point + 0.04, 0.05),
    color=TP_LINE, fontsize=9,
)
ax0.set_ylabel("Predicted forest-cover change,\nrelative to least-democratic\nmunicipality-year (pp)", color=SEC, fontsize=9.5)
ax0.set_title(
    "Subnational forest-cover change in Colombia using our indices\nand MapBiomas forest cover change data",
    color=INK, fontsize=13, loc="left", pad=12
)
ax0.set_xlim(0, 1)
ax0.grid(axis="y", color=GRID, linewidth=0.8, zorder=0)
ax0.spines[["top", "right"]].set_visible(False)
ax0.spines[["left", "bottom"]].set_color(BASE)
ax0.tick_params(colors=MUT, labelbottom=False, bottom=False)
ax0.legend(frameon=False, loc="upper center", labelcolor=INK, fontsize=8.5, ncol=2)

ax1.set_facecolor(SURF)
ax1.set_xlim(0, 1)
ax1.set_ylim(0, 1)
ax1.axis("off")

n = len(decile_labels)
col_w = 1.0 / n
row_y = [0.66, 0.28]

for i, (lab, val) in enumerate(zip(decile_labels, decile_means)):
    cx = (i + 0.5) * col_w
    ax1.text(cx, row_y[0], lab, ha="center", va="center", fontsize=9.5, color=INK, transform=ax1.transAxes)
    color = CURVE if val >= 0 else TP_LINE
    ax1.text(cx, row_y[1], f"{val:+.3f}", ha="center", va="center", fontsize=9.5, color=color, transform=ax1.transAxes)
    if i > 0:
        ax1.axvline(i * col_w, ymin=0.05, ymax=0.95, color=GRID, linewidth=0.8)

ax1.axhline(0.47, xmin=0, xmax=1, color=BASE, linewidth=0.8)

ax1_pos = ax1.get_position()
for ry, txt in zip(row_y, ["sndem decile", "Mean Δforest (pp)"]):
    fig_y = ax1_pos.y0 + ry * ax1_pos.height
    fig.text(0.145, fig_y, txt, ha="right", va="center", fontsize=8.7, color=SEC)

out_path = A2 + r"\mapbiomas_ucurve_with_table.png"
fig.savefig(out_path, facecolor=fig.get_facecolor())
print("Saved:", out_path)
