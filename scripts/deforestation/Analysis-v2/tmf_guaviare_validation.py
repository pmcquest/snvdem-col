"""
Reproduces Katz-Asprilla et al. (2024, Regional Environmental Change) Guaviare
deforestation totals from JRC TMF v1_2025/AnnualChanges, as an external validation
check on our own municipal TMF extraction (tmf_extract.py / build_tmf_cover.R).

Katz-Asprilla et al. compute "annual deforestation area" as newly-deforested land
each year -- i.e. pixels that become Class 3 (Deforested land) in year t having not
been Class 3 in year t-1 -- not the cross-sectional Class 3 stock in a given year
(which is not monotonic: pixels graduate from Class 3 to Class 4/regrowth after 3
stable years, so a raw year-over-year diff of Class 3 stock would undercount).
Their reported figure: 322,160 ha total, 2000-2020 (15,341 ha/yr average).

This computes the same pixel-level transition-into-Class-3 quantity, summed over
Guaviare's 4 municipios (San Jose del Guaviare 95001, Calamar 95015, El Retorno
95025, Miraflores 95200), for years 2000-2020 using 1999 as the anchor year for
2000's transition -- the same convention our own tmf_extract.py already uses for
the national panel's year-2000 lag.
"""
import ee, time, json

ee.Initialize(project='snvdem-col-tmf')

munis = ee.FeatureCollection('projects/snvdem-col-tmf/assets/colombia_municipios')
guaviare = munis.filter(ee.Filter.stringStartsWith('MPIO_CDPMP', '95'))
sam = ee.Image('projects/JRC/TMF/v1_2025/AnnualChanges/SAM')

px_area_ha = 0.09
years = list(range(2000, 2021))  # 2000-2020, matching Katz-Asprilla's 21-year window

rows = []
t_start = time.time()
for yr in years:
    prev_band = sam.select(f'Dec{yr-1}')
    cur_band = sam.select(f'Dec{yr}')
    new_deforested = cur_band.eq(3).And(prev_band.neq(3)).rename('new_def')

    t0 = time.time()
    result = new_deforested.reduceRegions(
        collection=guaviare,
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
    print(f'year {yr}: {round(time.time()-t0,1)}s', flush=True)

print('Total elapsed:', round(time.time()-t_start,1), 's', flush=True)

out_path = r'G:\Shared drives\snvdem\snvdem-col\scripts\deforestation\output\tmf_guaviare_validation.json'
with open(out_path, 'w') as f:
    json.dump(rows, f)
print('Wrote', len(rows), 'rows to', out_path, flush=True)

total_ha = sum(r['new_def_ha'] for r in rows)
print(f'\nTotal new-deforestation 2000-2020, Guaviare (4 municipios): {total_ha:,.0f} ha')
print(f'Annual average: {total_ha/len(years):,.0f} ha/yr')
print(f'Katz-Asprilla et al. (2024) reported: 322,160 ha total / 15,341 ha/yr average')
