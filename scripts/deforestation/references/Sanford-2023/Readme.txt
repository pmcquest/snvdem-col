Readme.txt

Date: 2021-05-19

Title: "Democratization, Elections, and Public Goods: The Evidence from Deforestation"

Contact information:
    Luke Sanford <lcsanford@ucsd.edu>

This file describe the contents of the replication archive used to conduct the analyses in the main text and supplemental information.
The replication data and code is organized into three folders:
1) code/ contains all code necessary to clean, merge, and analyze the data
2) data/output contains the datasets used in analysis
    2.1) data/ includes datasets used to construct analysis datasets
3) figures/ is where the paper figures will be saved

The file Codebook.pdf contains information on all variables in the analysis datasets.

All data cleaning and analysis was done using:
R version 4.0.2 (2020-06-22) -- "Taking Off Again"
Copyright (C) 2020 The R Foundation for Statistical Computing

Platform: x86_64-pc-linux-gnu

Hardware: Quad Xeon E7-4870v2 - 120 threads and 1tb ram (via UCSD's Social Sciences Research & Development Environment)

Estimated run time for all cleaning, merging, and analyses: 18-24 hours

Package versions used:
countrycode 1.2.0
data.table 1.13.2
dplyr 1.0.2
foreign 0.8-80
ggplot2 3.3.2
lfe 2.8-5.1
raster 3.4-5
readxl 1.3.1
sf 0.9-6
texreg 1.36.23
tidyverse 1.3.0
broom 0.7.2
dotwhisker 0.5.0

###################################
Code to construct analysis datasets
###################################
First, download the gadm dataset from https://gadm.org/download_world.html, unzip the file, and place the gadm36_levels.gpkg in the data folder.

Run the files in the order listed to produce analysis datasets. Files will be saved in the data/output folder

Files:
    process_forest.R
        takes the Hansen 2018 data, converts to panel forest change with country labels
        Requires: 
            - Hansen dataset: VCF5KYR_1982.tif through VCF5KYR_2016.tif (https://www.nature.com/articles/s41586-018-0411-9)
            - Global administrative boundaries data level 2 (https://gadm.org/)
            - Global ecological zones data http://www.fao.org/geonetwork/srv/en/metadata.show?id=1255
        Produces:
            - full_forest.Rdata
            
    process_upsampled.R
        takes the Hansen 2018 data, upsamples to 0.5dd, converts to panel forest change with country labels
        Requires: 
            - Hansen dataset: VCF5KYR_1982.tif through VCF5KYR_2016.tif (https://www.nature.com/articles/s41586-018-0411-9)
            - Global administrative boundaries data level 2 (https://gadm.org/)
            - Global ecological zones data http://www.fao.org/geonetwork/srv/en/metadata.show?id=1255
        Produces:
            - full_upsampled.Rdata

    merge.R
        Takes full_forest.Rdata, full_upsampled.Rdata, merges with data on economic, demographic, and political units to
        produce analysis datasets at 0.05dd and 0.5dd scales
        Requires:
            - full_forest.Rdata
            - full_updsampled.Rdata
            - polityIV data (2015 release, https://www.systemicpeace.org/inscrdata.html)
            - Database of Political Institutions (Downloaded 2018, https://datacatalog.worldbank.org/dataset/wps2283-database-political-institutions)
            - VDem dataset (Downloaded 2018, https://www.v-dem.net/en/)
            - World Development Indicators (Downloaded 2018, https://databank.worldbank.org/reports.aspx?source=world-development-indicators)
                    Indicators downloaded: "NV.AGR.TOTL.ZS","SL.AGR.EMPL.ZS","SP.POP.GROW","SP.POP.TOTL","NY.GDP.PCAP.KD.ZG","NY.GDP.PCAP.KD"
            - International Political Economy Data Resource (https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/28003)
        Produces:
            - full.Rdata
            - full_upsampled_10.Rdata
            
    Aggregate_Nat_L1_L2.R
        Takes the high resolution dataset and creates datasets aggregated to administrative levels 0, 1, and 2 for tropical and nontropical forest types
        Requires:
            - full.Rdata
        Produces:
            - GID_1_aggregated.Rdata
            - GID_2_aggregated.Rdata
            - country_aggregated.Rdata
            - GID_1_aggregated_tropical.Rdata
            - GID_2_aggregated_tropical.Rdata
            - country_aggregated_tropical.Rdata
            - GID_1_aggregated_nontropical.Rdata
            - GID_2_aggregated_nontropical.Rdata
            - country_aggregated_nontropical.Rdata

###################################
Code to produce main results
###################################

The files here produce the tables and figures for the manuscript. 

    coef_plots_democracy.R
        Produces Figure 2 and appendix figure 1
        Requires:
            - full.Rdata
            - country_aggregated.Rdata
        Produces:
            - coef_plot_democracy.pdf
            - coef_plot_democracy_appendix.pdf

    coef_plots_election.R
        Produces Figure 3 and appendix Figure 2
        Requires:
            - full.Rdata
            - country_aggregated.Rdata
        Produces:
            - coef_plot_election.pdf
            - coef_plot_election_appendix.pdf

    coef_plots_margin.R
        Produces Figure 4 and appendix Figure 3
        Requires:
            - full.Rdata
            - country_aggregated.Rdata
        Produces:
            - coef_plot_margin.pdf
            - coef_plot_margin_appendix.pdf

    Paper_tables.R
        Requires:
            - full.Rdata
            - country_aggregated.Rdata
        Produces: Tables 3, 4, and 5 and statistics for tables 1 and 2
        
        
###################################
Code to produce SI results
###################################
The files in this section produce the results shown in the supporting information. Note that SI figures 1-3 are produced alongside the main results figures

    HTE.R
        Produces Supporting Information tables 1-3 and table 11 which show heterogeneous treatment effects with agriculture and electoral system (1-3) and 
            the regressions of close elections with lead and lag years included
        Requires 
            - full.Rdata
            - country_aggregated.Rdata
        Produces
            - SI tables 1-4, 11
    GWR.R
        Produces geographically weighted regression results which show geographic variation in the effects
        Requires:
            - full_upsampled_10.Rdata
            - data/ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp 
                National borders from https://github.com/nvkelso/natural-earth-vector/blob/master/50m_cultural/ne_50m_admin_0_countries.shp because they are 
                lower resolution and plot easier
        Produces:
            - GWR_democ.pdf
            - GWR_elec.pdf
            - GWR_close80.pdf
            - GWR_close90.pdf

    levels_of_aggregation.R
        Produces tables 5-7, results with different levels of aggregation
        Requires:
            - full.Rdata
            - full_upsampled_10.Rdata
            - GID_1_aggregated.Rdata
            - GID_2_aggregated.Rdata
            - country_aggregated.Rdata
        Produces:
            - SI tables 5-7

    competitiveness_measures.R
        Produces SI tables 8-11, results using different measures of electoral competitiveness
        Requires:
            - full.Rdata
            - country_aggregated.Rdata
        Produces:
            - SI tables 8-11
    
    
    Paper_tables_saturated.R
            Produces SI tables 12-14, results with interacted fixed effects on previous level of forest cover and previous level of neighbors' forest cover
        Requires:
            - full.Rdata
            - country_aggregated.Rdata
        Produces: 
            - SI tables 12-14
            
            
###################################
Included data files
###################################
Note: all user-generated files are in the "output" subdirectory. All other files are used to construct the analysis dataset. Currently the gadm36_levels.gpkg file must be downloaded from the source website. All data citations are in the Codebook.
FOLDER: /data

    gadm36_levels.gpkg
        global administrative boundaries used to demarcate countries and subunits. Must be downloaded from: https://gadm.org/download_world.html approx 1.4gb. Please unzip the file provided and place the file in the /data folder.
        
    IPE_V3.tab
        IPE dataset used to construct analysis dataset.
    
    
SUBFOLDER: /output

    full.Rdata
        The full 0.05 decimal degree dataset, including all of the variables in the codebook
        
    full_upsampled_10.Rdata
        The upsampled 0.5 decimal degree dataset, including all variables in the codebook
        
    GID_2_aggregated.Rdata
        Level 2 administrative unit dataset. Does not contain cell-level characteristics from the codebook, forest cover variables are aggregated to the administrative unit level

    GID_2_aggregated_tropical.Rdata
        Level 2 administrative unit dataset including only tropical and subtropical forest types. Does not contain cell-level characteristics from the codebook, 
        forest cover variables are aggregated to the administrative unit level.
        
    GID_2_aggregated_nontropical.Rdata
        Level 2 administrative unit dataset including only temperate and boreal forest types. Does not contain cell-level characteristics from the codebook, 
        forest cover variables are aggregated to the administrative unit level.
        
    GID_1_aggregated.Rdata
        Level 1 administrative unit dataset. Does not contain cell-level characteristics from the codebook, forest cover variables are aggregated to the administrative unit level

    GID_1_aggregated_tropical.Rdata
        Level 1 administrative unit dataset including only tropical and subtropical forest types. Does not contain cell-level characteristics from the codebook, 
        forest cover variables are aggregated to the administrative unit level.
        
    GID_1_aggregated_nontropical.Rdata
        Level 1 administrative unit dataset including only temperate and boreal forest types. Does not contain cell-level characteristics from the codebook, 
        forest cover variables are aggregated to the administrative unit level.
        
    country_aggregated.Rdata
        Country level dataset. Does not contain cell-level characteristics from the codebook, forest cover variables are aggregated to the country level

    country_aggregated_tropical.Rdata
        Country level dataset including only tropical and subtropical forest types. Does not contain cell-level characteristics from the codebook, 
        forest cover variables are aggregated to the country level.
        
    country_aggregated_nontropical.Rdata
        Country level dataset including only temperate and boreal forest types. Does not contain cell-level characteristics from the codebook, 
        forest cover variables are aggregated to the country level.
        
SUBFOLDER: /Elections
    DPI2015_stata12.dta
        DPI data used to create analysis dataset
    
    p4v2015.xls
        PolityIV data used to create analysis dataset
    
    V-Dem-CY-Full+Others-v10.rds
        Varieties of Democracy data used to create analysis dataset

    
SUBFOLDER: /GEZ

    eco_zone.shp
        global ecological zone and all accompanying files (.apr, .avl, .dbf, .sbn, .sbx, .shx) used to demarcate forest types
    
SUBFOLDER: /ne_50m_admin_0_countries

    ne_50m_admin_0_countries.shp
        lower-resolution country shapefiles for plotting, includes (.cpg, .dbf, .prj, .shx)
        
SUBFOLDER: /WB

    WBI.csv
        World Bank development indicators used to construct the analysis dataset


SUBFOLDER: /VCF
    VCF5KYR_1982.TIF
    VCF5KYR_1983.TIF
    VCF5KYR_1984.TIF
    VCF5KYR_1985.TIF
    VCF5KYR_1986.TIF
    VCF5KYR_1987.TIF
    VCF5KYR_1988.TIF
    VCF5KYR_1989.TIF
    VCF5KYR_1990.TIF
    VCF5KYR_1991.TIF
    VCF5KYR_1992.TIF
    VCF5KYR_1993.TIF
    VCF5KYR_1995.TIF
    VCF5KYR_1996.TIF
    VCF5KYR_1997.TIF
    VCF5KYR_1998.TIF
    VCF5KYR_1999.TIF
    VCF5KYR_2001.TIF
    VCF5KYR_2002.TIF
    VCF5KYR_2003.TIF
    VCF5KYR_2004.TIF
    VCF5KYR_2005.TIF
    VCF5KYR_2006.TIF
    VCF5KYR_2007.TIF
    VCF5KYR_2008.TIF
    VCF5KYR_2009.TIF
    VCF5KYR_2010.TIF
    VCF5KYR_2011.TIF
    VCF5KYR_2012.TIF
    VCF5KYR_2013.TIF
    VCF5KYR_2014.TIF
    VCF5KYR_2015.TIF
    VCF5KYR_2016.TIF
        Forest cover data for each year. Note that years 1994 and 2000 are not available.