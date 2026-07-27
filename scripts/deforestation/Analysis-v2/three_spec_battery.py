import pyreadr
import pandas as pd
import numpy as np
from linearmodels.panel import PanelOLS, RandomEffects

OUT = r"G:\Shared drives\snvdem\snvdem-col\scripts\deforestation\output"
A2 = r"G:\Shared drives\snvdem\snvdem-col\scripts\deforestation\Analysis-v2"

def rds(path):
    return list(pyreadr.read_r(path).values())[0]

dem = rds(r"G:\Shared drives\snvdem\snvdem-col\data\panel\05_weighting\03_output\snvdem_col_weighted.rds")
dem = dem[["MPIO_CDPMP", "year", "snelect", "sncivlib", "sndem"]].drop_duplicates(["MPIO_CDPMP", "year"])
dem["MPIO_CDPMP"] = dem["MPIO_CDPMP"].astype(str).str.zfill(5)

def norm01(s):
    return (s - s.min()) / (s.max() - s.min())

def prep(df):
    df = df.copy()
    df["MPIO_CDPMP"] = df["MPIO_CDPMP"].astype(str).str.zfill(5)
    m = df.merge(dem, on=["MPIO_CDPMP", "year"], how="inner")
    for v in ["snelect", "sncivlib", "sndem"]:
        m[v + "_norm"] = norm01(m[v])
    return m

def run_three_specs(df, dvcol, label):
    df = df.dropna(subset=[dvcol, "forest_pct_lag1"]).copy()
    df = df.set_index(["MPIO_CDPMP", "year"])
    rows = []
    for dv in ["snelect_norm", "sncivlib_norm", "sndem_norm"]:
        y = df[dvcol]

        # Two-way FE
        X_tw = df[[dv, "forest_pct_lag1"]].assign(const=1.0)
        m_tw = PanelOLS(y, X_tw, entity_effects=True, time_effects=True, drop_absorbed=True)
        r_tw = m_tw.fit(cov_type="clustered", cluster_entity=True)

        # Year-only FE (drop entity effects, keep time effects), muni-clustered SEs
        X_yo = df[[dv, "forest_pct_lag1"]].assign(const=1.0)
        m_yo = PanelOLS(y, X_yo, entity_effects=False, time_effects=True, drop_absorbed=True)
        r_yo = m_yo.fit(cov_type="clustered", cluster_entity=True)

        # Random effects: year entered as dummy covariates (matches R's plm(... + factor(year), model="random"))
        year_dummies = pd.get_dummies(df.index.get_level_values("year"), prefix="yr", drop_first=True)
        year_dummies.index = df.index
        X_re = pd.concat([df[[dv, "forest_pct_lag1"]], year_dummies], axis=1).assign(const=1.0).astype(float)
        m_re = RandomEffects(y, X_re)
        r_re = m_re.fit(cov_type="robust")

        rows.append(dict(
            source=label, measure=dv.replace("_norm", ""),
            b_tw=r_tw.params[dv], se_tw=r_tw.std_errors[dv], t_tw=r_tw.params[dv] / r_tw.std_errors[dv],
            b_yo=r_yo.params[dv], se_yo=r_yo.std_errors[dv], t_yo=r_yo.params[dv] / r_yo.std_errors[dv],
            b_re=r_re.params[dv], se_re=r_re.std_errors[dv], t_re=r_re.params[dv] / r_re.std_errors[dv],
            n=int(r_tw.nobs),
        ))
    return pd.DataFrame(rows)

# ---- TMF-lossonly ----
tmf_cover = rds(A2 + r"\tmf_muni_cover.rds")[["MPIO_CDPMP", "year", "area_ha", "forest_pct_lag1"]]
tmf_lossonly = rds(OUT + r"\tmf_muni_lossonly.rds")
tmf_lossonly["MPIO_CDPMP"] = tmf_lossonly["MPIO_CDPMP"].astype(str).str.zfill(5)
tmf_cover["MPIO_CDPMP"] = tmf_cover["MPIO_CDPMP"].astype(str).str.zfill(5)
tlo = tmf_lossonly.merge(tmf_cover, on=["MPIO_CDPMP", "year"], how="inner")
tlo["d_forest_pct_lossonly"] = -(tlo["new_def_ha"] / tlo["area_ha"] * 100)
tlo = prep(tlo)
tlo_res = run_three_specs(tlo, "d_forest_pct_lossonly", "TMF-lossonly")

# ---- GFC ----
gfc = prep(rds(OUT + r"\gfc_muni_cover.rds"))
gfc_res = run_three_specs(gfc, "d_forest_pct", "GFC")

# ---- MapBiomas (corrected) ----
mb = prep(rds(A2 + r"\mb_muni_cover_corrected.rds"))
mb_res = run_three_specs(mb, "d_forest_pct", "MapBiomas")

pd.set_option("display.width", 220)
all_res = pd.concat([tlo_res, gfc_res, mb_res], ignore_index=True)
print(all_res.to_string())
all_res.to_csv(r"C:\Users\patri\AppData\Local\Temp\claude\g--Shared-drives-snvdem\29d2576d-8164-4197-a978-51c27c27f4fa\scratchpad\three_spec_battery_results.csv", index=False)
print("\nSaved.")
