# ArcMap exercise: Layers for 2018
Author: Patrick McQuestion

This folder contains the data and description of the methods I used to create layers for the Colombia map. -Patrick

## [6-9_Cardinal](6-9_Cardinal/)
I created these layers by merging Colombia's 6 macro-regions (as classified by [DANE]([url](https://www.dane.gov.co/index.php/estadisticas-por-tema/informacion-regional/informacion-estadistica-desagregada-con-enfoque-territorial-y-diferencial/informacion-del-dane-para-la-toma-de-decisiones-en-departamentos-y-ciudades-capitales))) in the following manner:

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
