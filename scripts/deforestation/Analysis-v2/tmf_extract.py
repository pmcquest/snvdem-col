import ee, time, json

ee.Initialize(project='snvdem-col-tmf')

munis = ee.FeatureCollection('projects/snvdem-col-tmf/assets/colombia_municipios')
sam = ee.Image('projects/JRC/TMF/v1_2025/AnnualChanges/SAM')

# 1999 through 2023: 1999 only needed to compute the year-2000 lag; matches the
# existing GFC/MapBiomas panel's 2000-2023 coverage exactly.
years = list(range(1999, 2024))

all_rows = []
t_start = time.time()
for yr in years:
    band = f'Dec{yr}'
    t0 = time.time()
    result = sam.select(band).reduceRegions(
        collection=munis,
        reducer=ee.Reducer.frequencyHistogram(),
        scale=30
    )
    info = result.getInfo()
    for f in info['features']:
        p = f['properties']
        hist = p.get('histogram', {})
        all_rows.append({
            'MPIO_CDPMP': p['MPIO_CDPMP'],
            'year': yr,
            'c0': hist.get('0', 0),
            'c1': hist.get('1', 0),
            'c2': hist.get('2', 0),
            'c3': hist.get('3', 0),
            'c4': hist.get('4', 0),
            'c5': hist.get('5', 0),
            'c6': hist.get('6', 0),
        })
    print(f'year {yr}: {len(info["features"])} municipalities, {round(time.time()-t0,1)}s', flush=True)

print('Total elapsed:', round(time.time()-t_start,1), 's', flush=True)

out_path = r'G:\Shared drives\snvdem\snvdem-col\scripts\deforestation\output\tmf_muni_classcounts.json'
with open(out_path, 'w') as f:
    json.dump(all_rows, f)
print('Wrote', len(all_rows), 'rows to', out_path, flush=True)
