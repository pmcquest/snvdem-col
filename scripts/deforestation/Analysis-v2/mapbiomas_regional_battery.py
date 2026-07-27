import pyreadr
import pandas as pd
import numpy as np
from linearmodels.panel import PanelOLS

OUT = r"G:\Shared drives\snvdem\snvdem-col\scripts\deforestation\output"
A2 = r"G:\Shared drives\snvdem\snvdem-col\scripts\deforestation\Analysis-v2"

def rds(path):
    return list(pyreadr.read_r(path).values())[0]

dem = rds(r"G:\Shared drives\snvdem\snvdem-col\data\panel\05_weighting\03_output\snvdem_col_weighted.rds")
dem = dem[["MPIO_CDPMP", "year", "DPTO_CCDGO", "snelect", "sncivlib", "sndem"]].drop_duplicates(["MPIO_CDPMP", "year"])
dem["MPIO_CDPMP"] = dem["MPIO_CDPMP"].astype(str).str.zfill(5)
dem["DPTO_CCDGO"] = dem["DPTO_CCDGO"].astype(str)

# IGAC/IDEAM's standard five natural regions of Colombia -- the same
# regionalization MapBiomas's ATDB cites for its own forest-class legend
# (Section 2.1.2). DANE department codes (DPTO_CCDGO), all 33 present in
# the panel accounted for exactly once.
REGION = {
    "05": "Andes", "11": "Andes", "15": "Andes", "17": "Andes", "25": "Andes",
    "41": "Andes", "54": "Andes", "63": "Andes", "66": "Andes", "68": "Andes", "73": "Andes",
    "08": "Caribe", "13": "Caribe", "20": "Caribe", "23": "Caribe", "44": "Caribe",
    "47": "Caribe", "70": "Caribe", "88": "Caribe",
    "19": "Pacifico", "27": "Pacifico", "52": "Pacifico", "76": "Pacifico",
    "50": "Orinoquia", "81": "Orinoquia", "85": "Orinoquia", "99": "Orinoquia",
    "18": "Amazonia", "86": "Amazonia", "91": "Amazonia", "94": "Amazonia",
    "95": "Amazonia", "97": "Amazonia",
}
dem["region"] = dem["DPTO_CCDGO"].map(REGION)
assert dem["region"].isna().sum() == 0, "unmapped department codes present"

def norm01(s):
    return (s - s.min()) / (s.max() - s.min())

def prep(df):
    df = df.copy()
    df["MPIO_CDPMP"] = df["MPIO_CDPMP"].astype(str).str.zfill(5)
    m = df.merge(dem, on=["MPIO_CDPMP", "year"], how="inner")
    for v in ["snelect", "sncivlib", "sndem"]:
        m[v + "_norm"] = norm01(m[v])
    return m

def run_region(df, dvcol, region_name):
    df = df[df["region"] == region_name].dropna(subset=[dvcol, "forest_pct_lag1"]).copy()
    df = df.set_index(["MPIO_CDPMP", "year"])
    n_munis = df.index.get_level_values("MPIO_CDPMP").nunique()
    n_depts = df["DPTO_CCDGO"].nunique()
    rows = []
    for dv in ["snelect_norm", "sncivlib_norm", "sndem_norm"]:
        for quad in [False, True]:
            X = df[[dv, "forest_pct_lag1"]].copy()
            if quad:
                X[dv + "_sq"] = df[dv] ** 2
                X = X[[dv, dv + "_sq", "forest_pct_lag1"]]
            X = X.assign(const=1.0)
            y = df[dvcol]
            try:
                mod = PanelOLS(y, X, entity_effects=True, time_effects=True, drop_absorbed=True)
                res = mod.fit(cov_type="clustered", cluster_entity=True)
            except Exception as e:
                rows.append(dict(region=region_name, measure=dv.replace("_norm", ""), quad=quad,
                                  n=int(len(df)), n_munis=n_munis, n_depts=n_depts, error=str(e)))
                continue
            b1 = res.params[dv]; se1 = res.std_errors[dv]
            row = dict(region=region_name, measure=dv.replace("_norm", ""), quad=quad,
                       n=int(res.nobs), n_munis=n_munis, n_depts=n_depts,
                       b1=b1, se1=se1, t1=b1 / se1)
            if quad:
                b2 = res.params[dv + "_sq"]; se2 = res.std_errors[dv + "_sq"]
                row.update(b2=b2, se2=se2, t2=b2 / se2, tp=-b1 / (2 * b2))
            rows.append(row)
    return pd.DataFrame(rows)

mb = prep(rds(A2 + r"\mb_muni_cover_corrected.rds"))

print("Municipality-years and departments per region (MapBiomas):")
print(mb.groupby("region").agg(n_munis=("MPIO_CDPMP", "nunique"),
                                n_depts=("DPTO_CCDGO", "nunique"),
                                n_muniyears=("MPIO_CDPMP", "size")))

all_res = pd.concat(
    [run_region(mb, "d_forest_pct", r) for r in ["Andes", "Caribe", "Pacifico", "Orinoquia", "Amazonia"]],
    ignore_index=True,
)
pd.set_option("display.width", 240)
print("\n=== MapBiomas, regional battery (two-way FE, muni-clustered SEs) ===")
print(all_res.to_string(index=False))
all_res.to_csv(A2 + r"\mapbiomas_regional_battery_results.csv", index=False)
print("\nSaved to mapbiomas_regional_battery_results.csv")
