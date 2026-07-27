import pyreadr
import pandas as pd
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.font_manager as fm

OUT = r"G:\Shared drives\snvdem\snvdem-col\scripts\deforestation\output"
A2 = r"G:\Shared drives\snvdem\snvdem-col\scripts\deforestation\Analysis-v2"

def rds(path):
    return list(pyreadr.read_r(path).values())[0]

dem = rds(r"G:\Shared drives\snvdem\snvdem-col\data\panel\05_weighting\03_output\snvdem_col_weighted.rds")
dem = dem[["MPIO_CDPMP", "year", "snelect"]].drop_duplicates(["MPIO_CDPMP", "year"])
dem["MPIO_CDPMP"] = dem["MPIO_CDPMP"].astype(str).str.zfill(5)

def norm01(s):
    return (s - s.min()) / (s.max() - s.min())

def decile_means(df, dvcol):
    df = df.copy()
    df["MPIO_CDPMP"] = df["MPIO_CDPMP"].astype(str).str.zfill(5)
    m = df.merge(dem, on=["MPIO_CDPMP", "year"], how="inner")
    m["snelect_norm"] = norm01(m["snelect"])
    m = m.dropna(subset=[dvcol, "snelect_norm"])
    m["decile"] = pd.qcut(m["snelect_norm"], 10, labels=False, duplicates="drop") + 1
    return m.groupby("decile")[dvcol].mean()

tmf = rds(A2 + r"\tmf_muni_cover.rds")
gfc = rds(OUT + r"\gfc_muni_cover.rds")
mb = rds(A2 + r"\mb_muni_cover_corrected.rds")
tmf_lo_raw = rds(OUT + r"\tmf_muni_lossonly.rds")
tmf_cover_small = tmf[["MPIO_CDPMP", "year", "area_ha"]].copy()
tmf_lo_raw["MPIO_CDPMP"] = tmf_lo_raw["MPIO_CDPMP"].astype(str).str.zfill(5)
tmf_cover_small["MPIO_CDPMP"] = tmf_cover_small["MPIO_CDPMP"].astype(str).str.zfill(5)
tmf_lo = tmf_lo_raw.merge(tmf_cover_small, on=["MPIO_CDPMP", "year"], how="inner")
tmf_lo["d_forest_pct_lossonly"] = -(tmf_lo["new_def_ha"] / tmf_lo["area_ha"] * 100)

series = {
    "TMF (primary)": (decile_means(tmf, "d_forest_pct"), "#2a78d6"),
    "TMF-lossonly": (decile_means(tmf_lo, "d_forest_pct_lossonly"), "#008300"),
    "GFC": (decile_means(gfc, "d_forest_pct"), "#e87ba4"),
    "MapBiomas": (decile_means(mb, "d_forest_pct"), "#eda100"),
}

fig, ax = plt.subplots(figsize=(7.5, 5), dpi=200)
fig.patch.set_facecolor("#fcfcfb")
ax.set_facecolor("#fcfcfb")

for label, (s, color) in series.items():
    ax.plot(s.index, s.values, marker="o", markersize=5, linewidth=2, color=color, label=label)

ax.axhline(0, color="#c3c2b7", linewidth=1, zorder=0)
ax.set_xlabel("Decile of municipal democratic quality (snelect), 1 = lowest, 10 = highest", color="#52514e")
ax.set_ylabel("Mean annual forest-cover change (pp)", color="#52514e")
ax.set_title("Forest-cover change by democracy decile: only TMF-bidirectional lacks a dip --\nTMF-lossonly tracks GFC and MapBiomas, not TMF-bidirectional", color="#0b0b0b", fontsize=11, loc="left")
ax.set_xticks(range(1, 11))
ax.grid(axis="y", color="#e1e0d9", linewidth=0.8, zorder=-1)
ax.spines[["top", "right"]].set_visible(False)
ax.spines[["left", "bottom"]].set_color("#c3c2b7")
ax.tick_params(colors="#898781")
leg = ax.legend(frameon=False, loc="upper left", labelcolor="#0b0b0b")

fig.tight_layout()
out_path = A2 + r"\decile_ushape_comparison.png"
fig.savefig(out_path, facecolor=fig.get_facecolor())
print("Saved:", out_path)
for label, (s, _) in series.items():
    print(label, dict(s.round(3)))
