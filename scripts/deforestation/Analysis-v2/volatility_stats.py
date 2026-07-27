import pyreadr
import pandas as pd
import numpy as np

OUT = r"G:\Shared drives\snvdem\snvdem-col\scripts\deforestation\output"
A2 = r"G:\Shared drives\snvdem\snvdem-col\scripts\deforestation\Analysis-v2"

def rds(path):
    return list(pyreadr.read_r(path).values())[0]

def zfill(df):
    df = df.copy()
    df["MPIO_CDPMP"] = df["MPIO_CDPMP"].astype(str).str.zfill(5)
    return df

def muni_sd(df, dvcol):
    g = df.dropna(subset=[dvcol]).groupby("MPIO_CDPMP")[dvcol].agg(["std", "count"])
    g = g[g["count"] >= 2]["std"]
    return g.median(), g.quantile(.25), g.quantile(.75), len(g)

def national_series(df, dvcol):
    # Unweighted across-municipality mean of d_forest_pct each year (matches
    # the "mean_d" convention already used in national_trend_tmf.csv /
    # colombia_deforestation_v3.R), NOT the diff of the size-weighted national total.
    s = df.dropna(subset=[dvcol]).groupby("year")[dvcol].mean()
    return s.max() - s.min(), s.std(), len(s)

tmf = zfill(rds(A2 + r"\tmf_muni_cover.rds"))
gfc = zfill(rds(OUT + r"\gfc_muni_cover.rds"))
mb = zfill(rds(A2 + r"\mb_muni_cover_corrected.rds"))

tmf_cover = zfill(rds(A2 + r"\tmf_muni_cover.rds"))[["MPIO_CDPMP", "year", "area_ha"]]
tlo = zfill(rds(OUT + r"\tmf_muni_lossonly.rds"))
tlo = tlo.merge(tmf_cover, on=["MPIO_CDPMP", "year"], how="inner")
tlo["d_forest_pct_lossonly"] = -(tlo["new_def_ha"] / tlo["area_ha"] * 100)

rows = []
for label, df, col in [
    ("TMF", tmf, "d_forest_pct"),
    ("TMF-lossonly", tlo, "d_forest_pct_lossonly"),
    ("GFC", gfc, "d_forest_pct"),
    ("MapBiomas", mb, "d_forest_pct"),
]:
    med, q25, q75, n_munis = muni_sd(df, col)
    rng, sd, n_years = national_series(df, col)
    rows.append(dict(source=label, muni_sd_median=med, muni_sd_q25=q25, muni_sd_q75=q75,
                      n_munis=n_munis, national_range_pp=rng, national_sd=sd, n_years=n_years))

res = pd.DataFrame(rows)
pd.set_option("display.width", 220)
print(res.to_string(index=False))
res.to_csv(A2 + r"\volatility_stats_results.csv", index=False)
print("\nSaved to volatility_stats_results.csv")
