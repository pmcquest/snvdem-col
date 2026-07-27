"""
National extension of tmf_guaviare_validation.py (Open Question 9, memo v2 Section 2.3):
computes the same pixel-level "new transition into TMF Class 3 (Deforested land)" measure,
but for all 1,122 Colombian municipios rather than just Guaviare's four, giving a loss-only
TMF series directly comparable to gfc_muni_loss.rds (GFC lossyear) on equal (loss-only) terms
-- as opposed to the primary bidirectional tmf_muni_cover.rds DV used in the main regressions.

Years 2000-2023, using 1999 as the anchor year for 2000's transition -- same convention as
tmf_extract.py / build_tmf_cover.R use for the national bidirectional panel's year-2000 lag.
"""
import ee, time, json

ee.Initialize(project='snvdem-col-tmf')

munis = ee.FeatureCollection('projects/snvdem-col-tmf/assets/colombia_municipios')
sam = ee.Image('projects/JRC/TMF/v1_2025/AnnualChanges/SAM')

px_area_ha = 0.09
years = list(range(2000, 2024))  # 2000-2023, matching tmf_muni_cover.rds's national panel range

rows = []
t_start = time.time()
for yr in years:
    prev_band = sam.select(f'Dec{yr-1}')
    cur_band = sam.select(f'Dec{yr}')
    new_deforested = cur_band.eq(3).And(prev_band.neq(3)).rename('new_def')

    t0 = time.time()
    result = new_deforested.reduceRegions(
        collection=munis,
        reducer=ee.Reducer.sum(),
        scale=30
    )
    info = result.getInfo()
    for f in info['features']:
        p = f['properties']
        rows.append({
            'MPIO_CDPMP': p['MPIO_CDPMP'],
            'year': yr,
            'new_def_px': p['sum'],
            'new_def_ha': p['sum'] * px_area_ha,
        })
    print(f'year {yr}: {len(info["features"])} municipalities, {round(time.time()-t0,1)}s', flush=True)

print('Total elapsed:', round(time.time()-t_start,1), 's', flush=True)

out_path = r'G:\Shared drives\snvdem\snvdem-col\scripts\deforestation\output\tmf_muni_lossonly.json'
with open(out_path, 'w') as f:
    json.dump(rows, f)
print('Wrote', len(rows), 'rows to', out_path, flush=True)

total_ha = sum(r['new_def_ha'] for r in rows)
n_years = len(years)
print(f'\nNational total new-deforestation {years[0]}-{years[-1]}: {total_ha:,.0f} ha')
print(f'National annual average: {total_ha/n_years:,.0f} ha/yr')
