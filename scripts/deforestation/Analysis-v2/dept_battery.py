import pyreadr
import pandas as pd
import numpy as np
from linearmodels.panel import PanelOLS

OUT = r"G:\Shared drives\snvdem\snvdem-col\scripts\deforestation\output"
A2 = r"G:\Shared drives\snvdem\snvdem-col\scripts\deforestation\Analysis-v2"

def rds(path):
    return list(pyreadr.read_r(path).values())[0]

dem = rds(OUT + r"\..\..\..\data\panel\05_weighting\03_output\snvdem_col_weighted.rds") if False else rds(
    r"G:\Shared drives\snvdem\snvdem-col\data\panel\05_weighting\03_output\snvdem_col_weighted.rds"
)
dem = dem[["MPIO_CDPMP", "year", "DPTO_CCDGO", "snelect", "sncivlib", "sndem"]].drop_duplicates(["MPIO_CDPMP", "year"])
dem["MPIO_CDPMP"] = dem["MPIO_CDPMP"].astype(str).str.zfill(5)
dem["DPTO_CCDGO"] = dem["DPTO_CCDGO"].astype(str)

def norm01(s):
    return (s - s.min()) / (s.max() - s.min())

def prep(df):
    df = df.copy()
    df["MPIO_CDPMP"] = df["MPIO_CDPMP"].astype(str).str.zfill(5)
    m = df.merge(dem, on=["MPIO_CDPMP", "year"], how="inner")
    for v in ["snelect", "sncivlib", "sndem"]:
        m[v + "_norm"] = norm01(m[v])
    return m

def run_battery(df, dvcol, label):
    df = df.dropna(subset=[dvcol, "forest_pct_lag1"]).copy()
    df = df.set_index(["MPIO_CDPMP", "year"])
    rows = []
    for dv in ["snelect_norm", "sncivlib_norm", "sndem_norm"]:
        for quad in [False, True]:
            X = df[[dv, "forest_pct_lag1"]].copy()
            if quad:
                X[dv + "_sq"] = df[dv] ** 2
                X = X[[dv, dv + "_sq", "forest_pct_lag1"]]
            X = X.assign(const=1.0)
            y = df[dvcol]
            mod = PanelOLS(y, X, entity_effects=True, time_effects=True, drop_absorbed=True)
            # muni-clustered (entity clustering)
            res_muni = mod.fit(cov_type="clustered", cluster_entity=True)
            # department-clustered
            dept_ids = df["DPTO_CCDGO"]
            res_dept = mod.fit(cov_type="clustered", clusters=dept_ids)
            b1 = res_muni.params[dv]; se1_m = res_muni.std_errors[dv]; se1_d = res_dept.std_errors[dv]
            row = dict(source=label, measure=dv.replace("_norm", ""), quad=quad,
                       n=int(res_muni.nobs), b1=b1, se1_muni=se1_m, t1_muni=b1/se1_m,
                       se1_dept=se1_d, t1_dept=b1/se1_d)
            if quad:
                b2 = res_muni.params[dv + "_sq"]; se2_m = res_muni.std_errors[dv + "_sq"]
                se2_d = res_dept.std_errors[dv + "_sq"]
                row.update(b2=b2, se2_muni=se2_m, t2_muni=b2/se2_m, se2_dept=se2_d, t2_dept=b2/se2_d,
                           tp=-b1/(2*b2))
            rows.append(row)
    return pd.DataFrame(rows)

# ---- GFC ----
gfc = prep(rds(OUT + r"\gfc_muni_cover.rds"))
gfc_res = run_battery(gfc, "d_forest_pct", "GFC")

# ---- MapBiomas (CORRECTED crosswalk) ----
mb = prep(rds(A2 + r"\mb_muni_cover_corrected.rds"))
mb_res = run_battery(mb, "d_forest_pct", "MapBiomas")

# ---- TMF loss-only ----
tmf_cover = rds(A2 + r"\tmf_muni_cover.rds")[["MPIO_CDPMP", "year", "area_ha", "forest_pct_lag1"]]
tmf_lossonly = rds(OUT + r"\tmf_muni_lossonly.rds")
tmf_lossonly["MPIO_CDPMP"] = tmf_lossonly["MPIO_CDPMP"].astype(str).str.zfill(5)
tmf_cover["MPIO_CDPMP"] = tmf_cover["MPIO_CDPMP"].astype(str).str.zfill(5)
tlo = tmf_lossonly.merge(tmf_cover, on=["MPIO_CDPMP", "year"], how="inner")
tlo["d_forest_pct_lossonly"] = -(tlo["new_def_ha"] / tlo["area_ha"] * 100)
tlo = prep(tlo)
tlo_res = run_battery(tlo, "d_forest_pct_lossonly", "TMF-lossonly")

pd.set_option("display.width", 220)
print("\n=== GFC ===")
print(gfc_res.to_string())
print("\n=== MapBiomas (corrected) ===")
print(mb_res.to_string())
print("\n=== TMF loss-only ===")
print(tlo_res.to_string())

all_res = pd.concat([gfc_res, mb_res, tlo_res], ignore_index=True)
all_res.to_csv(r"C:\Users\patri\AppData\Local\Temp\claude\g--Shared-drives-snvdem\29d2576d-8164-4197-a978-51c27c27f4fa\scratchpad\dept_battery_results.csv", index=False)
print("\nSaved.")
print("\nn per source:")
for label, d in [("GFC", gfc), ("MapBiomas", mb), ("TMF-lossonly", tlo)]:
    print(label, d.shape)
