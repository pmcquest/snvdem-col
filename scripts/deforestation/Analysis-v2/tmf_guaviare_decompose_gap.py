"""
Decomposes the 5.4% gap between our Guaviare TMF total (339,700 ha, tmf_guaviare_validation.py,
memo v2 Section 2.3) and Katz-Asprilla et al. (2024)'s reported 322,160 ha, using their actual
GEE template script (shared by the user, JRC's official "Extraction of statistics based on
annual changes in TMF" template) as ground truth for what they actually ran. Three candidate
differences, tested independently against the same Guaviare municipio geometry so boundary
choice is held constant and isolated from the other three factors:

  (A) TMF version: they used v1_2021, we used v1_2025.
  (B) Area convention: they used ee.Image.pixelArea() (true geodesic pixel area), we used a
      fixed 0.09 ha/pixel constant.
  (C) Deforestation-transition definition: their script only counts Class1->3 (undisturbed) and
      Class2->3 (degraded) transitions as "deforestation"; our original script counted any
      prior state != 3 (including Class4/regrowth, Class5/water, Class6/other -> 3), which is a
      superset.

Each variant re-sums the same Guaviare 4-municipio geometry (95001, 95015, 95025, 95200),
2000-2020, so any remaining gap after all three corrections points to something else (most
likely the municipal boundary shapefile itself, which we don't have their exact copy of).
"""
import ee, time, json

ee.Initialize(project='snvdem-col-tmf')

munis = ee.FeatureCollection('projects/snvdem-col-tmf/assets/colombia_municipios')
guaviare = munis.filter(ee.Filter.stringStartsWith('MPIO_CDPMP', '95'))
guaviare_geom = guaviare.geometry()

years = list(range(2000, 2021))

def run_variant(asset_path, use_pixel_area, narrow_definition, label):
    img = ee.ImageCollection(asset_path).mosaic()
    total_ha = 0.0
    by_year = {}
    t0 = time.time()
    for yr in years:
        prev = img.select(f'Dec{yr-1}')
        cur = img.select(f'Dec{yr}')
        if narrow_definition:
            # matches Katz-Asprilla's script: only Class1->3 or Class2->3
            new_def = (prev.eq(1).Or(prev.eq(2))).And(cur.eq(3))
        else:
            new_def = cur.eq(3).And(prev.neq(3))

        if use_pixel_area:
            area_img = new_def.multiply(ee.Image.pixelArea().divide(1e4))
            val = area_img.reduceRegion(
                reducer=ee.Reducer.sum(), geometry=guaviare_geom, scale=30, maxPixels=5e9
            ).getInfo()
            ha = list(val.values())[0]
        else:
            px_count = new_def.reduceRegion(
                reducer=ee.Reducer.sum(), geometry=guaviare_geom, scale=30, maxPixels=5e9
            ).getInfo()
            ha = list(px_count.values())[0] * 0.09

        by_year[yr] = ha
        total_ha += ha
    print(f'[{label}] total 2000-2020: {total_ha:,.0f} ha ({round(time.time()-t0,1)}s)', flush=True)
    return total_ha, by_year

results = {}

# Baseline: our original memo v2 Section 2.3 construction (v1_2025, fixed area, broad definition)
results['our_original_v1_2025'] = run_variant(
    'projects/JRC/TMF/v1_2025/AnnualChanges', use_pixel_area=False, narrow_definition=False,
    label='our_original_v1_2025 (fixed area, broad def)'
)

# (A) alone: swap to v1_2021, keep our original conventions otherwise
results['v1_2021_only'] = run_variant(
    'projects/JRC/TMF/v1_2021/AnnualChanges', use_pixel_area=False, narrow_definition=False,
    label='v1_2021_only (fixed area, broad def)'
)

# (A) + (B): v1_2021 + pixelArea()
results['v1_2021_pixelarea'] = run_variant(
    'projects/JRC/TMF/v1_2021/AnnualChanges', use_pixel_area=True, narrow_definition=False,
    label='v1_2021 + pixelArea (broad def)'
)

# (A) + (B) + (C): full match to Katz-Asprilla's script conventions
results['v1_2021_pixelarea_narrowdef'] = run_variant(
    'projects/JRC/TMF/v1_2021/AnnualChanges', use_pixel_area=True, narrow_definition=True,
    label='v1_2021 + pixelArea + narrow def (full match)'
)

print('\n\n=== Summary ===')
print(f"Katz-Asprilla et al. (2024) reported:        322,160 ha")
for k, (total, _) in results.items():
    diff_pct = (total - 322160) / 322160 * 100
    print(f"{k:35s}: {total:>10,.0f} ha  ({diff_pct:+.1f}% vs. reported)")

out_path = r'G:\Shared drives\snvdem\snvdem-col\scripts\deforestation\output\tmf_guaviare_decompose_gap.json'
with open(out_path, 'w') as f:
    json.dump({k: {'total_ha': v[0], 'by_year': v[1]} for k, v in results.items()}, f, indent=2)
print('\nWrote', out_path)
