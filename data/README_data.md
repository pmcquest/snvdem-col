
# Data repository
This folder contains data for the project. A central task for this project is to translate V-Dem response variables (n=16, see [here](https://docs.google.com/document/d/1i0Isx-ZnRlNuyg15rCJZc-FCtkK_rDmAW7N5mfGgxT4/edit?usp=sharing) for full list) into map polygons in Colombia. This folder includes both V-Dem and mapping data, in addition to other data such as development outcomes for further statistical analysis. 

## [V-Dem data](Vdem/v13/)
V-Dem data on subnational elections and respect for civil liberties has been collected in 2 surveys, Elections and Civil Liberties. The latest dataset is [version 13](Vdem/v13). We will be using data from four survey questions (v2elsnlfc, v2elsnmrfc, v2clrgstch, v2clrgwkch) with 16 relevant responses per year (see [here](https://docs.google.com/document/d/1DsavwLRZYN3-xjcztbvyxg4riY1ylmg0j2OaC9cXZ4s/edit?usp=sharing) for more information):
1. **Elections**: v2elsnlfc, v2elsnmrfc
2. **Civil liberties**: v2clrgstch, v2clrgwkch

V-Dem also contains the following textual variables in each survey (see [comments](Vdem/comments)):
1. **Elections**:
   - v2elsnless (id 411)
   - v2elsnmore (id 414)
   - v2elcomcom (id 400; v3_id 2496)
   - v2elcomsn (id 418; v3_id 2506)
3. **Civil liberties**:
   - v2clcommnt (id 328)

_* Note: In some cases, we will use historical data from previous V-Dem surveys. These variables are labeled "v3"._

## Mapping data
Maps will be drawn using spatial analysis software and existing shapefiles. This is useful because maps may draw from pre-loaded data on income, population density, terrain, and other key attributes. Colombian maps can be downloaded from the official geographic institute, IGAC. [Here](https://geoportal.igac.gov.co/contenido/datos-abiertos-cartografia-y-geografia) are shapefiles for download at different aspect ratios. The Truth Commission has been mapping _violence_ extensively, so [updated information](https://docsgeoportal.comisiondelaverdad.co/descripcion-proyecto/funcionalidad-descripcion-proyecto) may be available from them (here’s another [publication](https://4107273305-files.gitbook.io/~/files/v0/b/gitbook-x-prod.appspot.com/o/spaces%2F-MjFCW8A_OZ-a2CP3wMG%2Fuploads%2FJ5Fg564AsH0VHVZOoeKS%2FManual%20de%20usuario%20geoportal.pdf?alt=media&token=3c59168a-ff1e-4583-abff-ddcec6638eea).

## Development outcomes 
These will be measured using Colombian Census data.
