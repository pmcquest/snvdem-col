import pyreadr
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

OUT = r"G:\Shared drives\snvdem\snvdem-col\scripts\deforestation\output"
A2 = r"G:\Shared drives\snvdem\snvdem-col\scripts\deforestation\Analysis-v2"

def rds(path):
    return list(pyreadr.read_r(path).values())[0]

# Each series uses exactly the forest_ha column the regressions themselves use --
# no separate class-level breakdown, no representative subcategories.
tmf = rds(A2 + r"\tmf_muni_cover.rds").groupby("year")["forest_ha"].sum() / 1e6
gfc = rds(OUT + r"\gfc_muni_cover.rds").groupby("year")["forest_ha"].sum() / 1e6
mb = rds(A2 + r"\mb_muni_cover_corrected.rds").groupby("year")["forest_ha"].sum() / 1e6

fig, ax = plt.subplots(figsize=(7.5, 5), dpi=200)
fig.patch.set_facecolor("#fcfcfb")
ax.set_facecolor("#fcfcfb")

series = [("TMF (primary)", tmf, "#2a78d6"), ("GFC", gfc, "#e87ba4"), ("MapBiomas", mb, "#eda100")]
for label, s, color in series:
    ax.plot(s.index, s.values, linewidth=2, color=color, label=label)

ax.set_xlabel("Year", color="#52514e")
ax.set_ylabel("National forest cover (million ha)", color="#52514e")
ax.set_title("National forest cover by source, 2000-2023 -- same forest_ha column each\nsource's own regressions use; levels differ by definition, trends are the point",
             color="#0b0b0b", fontsize=11, loc="left")
ax.grid(axis="y", color="#e1e0d9", linewidth=0.8, zorder=-1)
ax.spines[["top", "right"]].set_visible(False)
ax.spines[["left", "bottom"]].set_color("#c3c2b7")
ax.tick_params(colors="#898781")
ax.legend(frameon=False, loc="center left", labelcolor="#0b0b0b")

fig.tight_layout()
out_path = A2 + r"\forest_stock_by_source.png"
fig.savefig(out_path, facecolor=fig.get_facecolor())
print("Saved:", out_path)
for label, s, _ in series:
    print(label, "2000:", round(s.loc[2000], 2), "2023:", round(s.loc[2023], 2))
