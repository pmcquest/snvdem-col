
# Data repository
This folder contains data for the project. A central task for this project is to translate relevant V-Dem response variables (n=16, see [here](https://docs.google.com/document/d/1i0Isx-ZnRlNuyg15rCJZc-FCtkK_rDmAW7N5mfGgxT4/edit?usp=sharing) for full list) into map polygons in Colombia. This 'data' folder includes both V-Dem and geospatial data, in addition to other data such as development outcomes for further statistical analysis. 

## [V-Dem](Vdem/)
Nominal and textual V-Dem data on subnational elections and respect for civil liberties have been collected in 2 survey clusters. The latest dataset is [version 13](Vdem/v13). 

We will be using **nominal data** (coder scores) from four survey questions. These questions are composed of 21 sub-items, of which 16 are relevant for the study of subnational democracy in Colombia (see [here](https://docs.google.com/document/d/1DsavwLRZYN3-xjcztbvyxg4riY1ylmg0j2OaC9cXZ4s/edit?usp=sharing) for more information). The question items are listed below beneath each survey cluster:
   1. **Elections**:
      - v2elsnlfc -> "How would you describe the areas of the country in which elections are significantly less free and fair?"
      - v2elsnmrfc -> "How would you describe the areas of the country in which elections are significantly more free and fair?"
   2. **Civil liberties**:
      - v2clrgstch -> "How would you describe the areas of the country where government officials’ respect for civil liberties is significantly stronger?"
      - v2clrgwkch -> "How would you describe the areas of the country where government officials’ respect for civil liberties is significantly weaker?"

V-Dem also collects **textual data** (coder comments) with the following variables in each survey (see [comments](Vdem/comments)):
   1. **Elections**:
      - v2elsnless (id 411) -> "In which areas of the country are subnational elections significantly _less_ free and fair than the country average for subnational elections?"
      - v2elsnmore (id 414) -> "In which areas of the country are subnational elections significantly _more_ free and fair than the country average for subnational elections?"
      - v2elcomcom (id 400; v3_id 2496) -> 
      - v2elcomsn (id 418; v3_id 2506) -> 
   2. **Civil liberties**:
      - v2clcommnt (id 328) -> "Add here any comments you have about any of the previous questions on civil liberties."

_* Note: In some cases, we will use historical data from previous V-Dem surveys. These variables are labeled "v3"._

## [Geospatial data](geospatial/)
Maps will be drawn using spatial analysis software and existing shapefiles. This is useful because maps may draw from pre-loaded data on income, population density, terrain, and other key attributes. Colombian maps can be downloaded from the official geographic institute, IGAC. [Here](https://geoportal.igac.gov.co/contenido/datos-abiertos-cartografia-y-geografia) are shapefiles for download at different aspect ratios. The Truth Commission has been mapping _violence_ extensively, so [updated information](https://docsgeoportal.comisiondelaverdad.co/descripcion-proyecto/funcionalidad-descripcion-proyecto) may be available from them (here’s another [publication](https://4107273305-files.gitbook.io/~/files/v0/b/gitbook-x-prod.appspot.com/o/spaces%2F-MjFCW8A_OZ-a2CP3wMG%2Fuploads%2FJ5Fg564AsH0VHVZOoeKS%2FManual%20de%20usuario%20geoportal.pdf?alt=media&token=3c59168a-ff1e-4583-abff-ddcec6638eea)).

## Development outcomes 
These will be measured using Colombian Census data and other sources. Commonly used databases are listed below: 
- Health: [Global Burden of Disease (GBD) database](https://www.healthdata.org/research-analysis/health-by-location/profiles/colombia)
- Multiple:
  1. [World Bank Development Indicators](https://databank.worldbank.org/source/world-development-indicators)
  2. [CNTS](https://www.cntsdata.com/)
