# ArcMap exercise: Layers for 2018
Author: Patrick McQuestion

This folder contains the data and description of the methods I used to create layers for the Colombia map. 

## [Base Layer shapefile](BaseLayer/)
The base layer shapefile I used comes from the 2018 National Census (*Censo Nacional de Poblacion y Vivienda 2018*), available from DANE (see [here](https://www.dane.gov.co/index.php/estadisticas-por-tema/demografia-y-poblacion/censo-nacional-de-poblacion-y-vivenda-2018)). This shapefile contains the **key ID variable**--Codigo DANE municipal--that allows me to integrate external databases at the municipal level, in addition to other survey-related variables such as number of homes and population. The shapefile uses the official Marco Geoestadistico Nacional ([MGN](https://geoportal.dane.gov.co/servicios/descarga-y-metadatos/descarga-mgn-marco-geoestadistico-nacional/#gsc.tab=0)).

## [#6-9 Cardinal directions](6-9_Cardinal/)
I created these layers by merging Colombia's 6 macro-regions (as classified by [DANE](https://www.dane.gov.co/index.php/estadisticas-por-tema/informacion-regional/informacion-estadistica-desagregada-con-enfoque-territorial-y-diferencial/informacion-del-dane-para-la-toma-de-decisiones-en-departamentos-y-ciudades-capitales)) in the following manner:

```
def getCardinalDirection(REGION):
    if REGION in ['Caribe', 'Eje Cafetero - Antioquia']:
        return 'North'
    elif REGION in ['Centro Oriente', 'Llano']:
        return 'East'
    elif REGION in ['Centro Sur']:
        return 'South'
    elif REGION in ['Pacifico']:
        return 'West'
    else:
        return 'Unknown'
```

## [#15-16 Ruling Party support](15-16_RulingParty/)
I created these layers using data from Colombia's Registraduria ([RNEC](https://cedae.registraduria.gov.co/datos-para-la-democracia/resultados-electorales/descarga-datos)), and if necessary, Electoral Observation Mission ([MOE](https://www.datoselectorales.org/datos/resultados-electorales)). 
- voting since 1957-1991 will likely be Liberal or Conservative. Fragmented afterwards.
- % of vote that is same as ruling party, then later it could be the ally of Presidential party
- Datasets?
    - ask: Tom Mustillo.
    - Constituency Level Elections Archive ([CLEA](https://electiondataarchive.org/data-and-documentation/clea-lower-chamber-elections-archive/countries-and-elections/)): repository at constituency-level at lower-level legislative. Geo-Referenced electoral districts. Colombia: 1998-2014.
    - V-Party: has governing party "does this party support govt after selection" (use: Senior or Junior Partners). Without a majority, incentive to form coalitions. Governing coalitions (Altman: historical data; also project manager for direct democracy).
    - Dieter Nohlen: national-level, contains alliances and number of seats in Congress. 
