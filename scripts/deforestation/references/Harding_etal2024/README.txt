*****************************************
*This readme file explains the contents of the replication folder for:

Buying a Blind Eye: Campaign Donations, Regulatory Enforcement, and Deforestation

Published in the American Political Science Review. 

Authors:

1. Robin Harding, University of Oxford
2. Mounu Prem, Universidad del Rosario 
3. David Vargas, Inter-American Development Bank
4. Nelson Ruiz (corresponding author), University of Essex, nelson.ruiz@essex.ac.uk

*******************************
*This readme is divided into two parts:
* 1. Replication of the paper:
* 2. Construction of the data and data sources:
****************************************

***************************
*1. Replication of the paper:
***************************


Computational Environment:

Operating System: Windows 10 Enterprise
Statistical Software: Stata MP 16.1 (64-bit x86-64) and R 
Additional Stata packages: rdrobust(latest update, Winter 2020), texdoc, rddensity, egenmore, rdperm
                           grstyle, parmest
Note: ado files used in the analysis can easily be installed from Stata by typing "ssc install" followed by the name of the respective package. For rdperm the ado file is provided at 2_programs/_ado


Main folders:

I. 1_data. This folder contains all the datasets needed to replicate the paper. All variables have detailed labels. 
1.	muni_donors_deforestation: final dataset at the municipality level with all necessary variables for the main analysis.
2.	Codebook.xls: file with information on each variable in each of the previous four datasets used to construct the final dataset, including detailed data sources.
3.   _data_construction: This folder contains all the non-sensitive codes and data (we are allowed to directly share) to replicate the main dataset. 
          PLEASE CHECK THE "DATA RECONSTRUCTION README" INSIDE THE _data_construction.

II. 2_programs. This folder contains the .do files that replicate all tables and figures, as well as an R code file to replicate Figure 1.
Note: We included one code file for each figure/table. You can find the name of the table/figure in the name of the file. Files in other folders must preserve the folder structure and the path to the main folder must be specified in each do-file. Also, the do-file 0_Master.do contains the needed paths, packages installation instructions and executes all other do-files in the folder. 

III. 3_output
1.	figures. This folder contains all the figures of the paper generated with the codes in 2_programs.
2. 	tables. This folder contains all the tables of the paper generated with the codes in 2_programs.

To properly display the tables in latex the following packges are advised:
graphicx , amsmath , amssymb , float , booktabs , tabularx , lscape , longtable , 
multirow , array , arydshln , subcaption , pdflscape , threeparttable , placeins , url.


***************************
*2. Construction of the data and data sources:
***************************

*Construction of the data folder is located in 1_data\_data_construction

*For code construction of the main dataset please see clean up folder 000_master_construct.do.*

Main folders:
I. Raster_processing: This folder contains the codes, and when possible data, needed to process the spatial data used on the paper.
     Subfolders:
     1. Deforestation: This folder contains the codes and file structure to process the main deforestation measures used in the paper.
         Contents:
         a. gfc: An empty folder to store the raster files to be processed. Data available on-line from: https://glad.earthengine.app/view/global-forest-change. 
         We used the version 2019-v1.7.
         b. output_data: An empty folder to store the process data.
         c. Hansen_V2.R: This script processes the raster data. This script can also download the required data.
         
    2. Landuse: This folder contains the codes and file structure to process the landuse data from the ESA used in the paper.
        Contents:
        a. byYear: And empty folder to store the yearly rasters to be processed. Follow the download instructions at: maps.elie.ucl.ac.be/CCI/viewer/download/ESACCI-LC-Ph2-PUGv2_2.0.pdf
        b. documentation: ESA documentation related to this data. 
        c. output_data: folder to store the processed data.
        d. landcover.R: This script processes the raster data.
        
    3. Methane: This folder contains the codes and file structure to process the methane emissions data used in the paper.
        Contents:
        a. 2016averageppb: An empty folder to store the raster files to be processed. The original data comes form NASA and can be found at: 
        https://airs.jpl.nasa.gov/data/get-data/standard-data/. We use the Annual average methane concentration raster, 
        pre-processed by The Carnegie Endowment for International Peace and availiable at: 
        https://open.africa/dataset/global-annual-average-methane-concentration-2016
        b. NASA_PPB_2016_CO.R: This script processes the raster data.
        c. output_data: folder that stores the post-processing output.
        
    4. Nasa fires: This folder contains the data delivered by the NASA firms platform. Online at: https://www.earthdata.nasa.gov/learn/find-data/near-real-time/firms
        Contents:
        a. Processed: This folder contains data delimited files with the raw data used on the paper.
        b. Readme.txt: This is the data README as delivered by FIRMS
        c. shapefile: Contains the fire_archive_M6_181184.shp (Shapefile containing the fires data as delivered by FIRMS) and related files.
        
    5. Shapefiles: Contains the shapefile of the Colombia municipalities. This is a resource used by several of these scripts. 
        
II. Clean_up: This folder contains all the data organisation do-files. ORDER MATTERS PLEASE RUN 000_master_construct.do first. Please run each do-file following the number order provided. 

    Contents:
    0. 000_master_construct: Calls all the do files below and constructs the paper data except the sensitive data with personally identifiable information (PII).
    1. 001_data_deforestation.do: Prepares the deforestation data. Including the paper's main outcome variable. Please, before running this file make sure
    to have already generated the required data. The required data is created by processing the files in the Deforestation subfolder in Raster_processing.
    2. 002_prep_fires.do : Prepares the fire intensity data. Please, before running this file make sure to have already generated the required data. 
    The required data is created by processing the files in the Fires subfolder in Raster_processing.
    3. 003_prep_landuse.do : Prepares the European Space Agency data on landuse. Please, before running this file make sure to have already generated the required data. The required data is created by processing the files in the Landuse subfolder in Raster_processing.
    4. 004_prep_methane.do : Prepares the data on methane emissions. Please, before running this file make sure to have already generated the required data. 
    The required data is created by processing the files in the Methane subfolder in Raster_processing.
    5. 005_controls.do : Loads and merges the control variables. Also executes the required transformations over these. We provide all data we have rights to share but you must download some third party data including:
        - Panel CEDE. Accessible in https://datoscede.uniandes.edu.co/es/
        - Rosarios violence dataset - Violence (Restrepo, Vargas, and Spagat (2003))
    6. 006_add_variables.do: This script merges the fires, landuse and methane data to the main dataset, and also adds some additional variables to the dataset. Also adds the contracts data. The data used here was acquired from SECOP. This data is massive and also third party. Please visit the online portal: https://www.colombiacompra.gov.co/secop-ii
    7. 007_fiscal.do: This script adds the municipal fiscal measures used on the paper. The original source of the data is the Colombian's National Planning Department (DNP). The data was originally acquired at:  https://www.dnp.gov.co/programas/desarrollo-territorial/Estudios-Territoriales/Indicadores-y-Mediciones/Paginas/desempeno-fiscal.aspx

  II.a. individual_data: Contains codes needed to replicate some of the individual candidate level dataset from: Ruiz, Nelson. 2017. "The Power of Money. The Consequences of Electing a Donor Funded Politician."
	For further information on the needed data sources and how to request access to them, see the section THIRD PARTY SOURCES / HOW TO GAIN ACCESS, numeral IV of this README file.
	Some of these may be restricted in the public version of the replication files due to privacy concerns.

	Contents: 
	0. 000_Master_construct_individual.do: Calls all the do files below and constructs the individual candidate dataset.
	1. 001_mayor all elections.do: Builds the base dataset with the winner and runner up for each mayoral race in 2011. Source Uniandes and Ferguson et. al. (see the section THIRD PARTY SOURCES / HOW TO GAIN ACCESS, numeral IV, subnumeral A and B.)
	2. 002_adding national_id.do: Adds national IDs to each candidate observation. Comptollers office data (see the section THIRD PARTY SOURCES / HOW TO GAIN ACCESS, numeral IV, subnumeral C.)
	3. 003_income campaigns.do: Adds campaign income data as reported by Cuentas Claras. (see the section THIRD PARTY SOURCES / HOW TO GAIN ACCESS, numeral IV, subnumeral D.)
	4. 004_adding sanctions.do: Adds charges by the comptrollership office before running for 2011 office. (see the section THIRD PARTY SOURCES / HOW TO GAIN ACCESS, numeral IV, subnumeral E.)
	5. 005_2015_outcomes.do: Adds information on 2015 election. Source Uniandes (see the section THIRD PARTY SOURCES / HOW TO GAIN ACCESS, numeral IV, subnumeral A.)
	6. 006_facial_features.do: Add candidates facial features. Source Gulzar et. al. (see the section THIRD PARTY SOURCES / HOW TO GAIN ACCESS, numeral IV, subnumeral F.)
	7. 007_political_experience.do: Adds variables capturing previous political experience. Source Uniandes and Ferguson et. al. (see the section THIRD PARTY SOURCES / HOW TO GAIN ACCESS, numeral IV, subnumeral A and B.)
	8. 007a_pol_exp.do: Aux file for 007_political_experience.do
	9. 007b_elected_exp.do: Aux file for 007_political_experience.do
	10. 008_private_def.do: Builds the definition of privately funded candidate. 
	11. 009_runs_future_report.do: Adds variables capturing future political participation. Source Uniandes and National Registry (see the section THIRD PARTY SOURCES / HOW TO GAIN ACCESS, numeral IV, subnumeral A and C.)
	12. 010_forcing.do: creates the forcing variable for the different RDs that are being used. Donor vs not donor funded, rightwing (leftwing) vs not rightwing (leftwing), political experienced vs not.
	13. 011_annonimisation.do: Annonimises municipality codes. 
	14. 012_more_stringent_donor_def.do: Creates different measures on how to measure donor funded beyond a binary measure of being donor-funded vs not bieng donor funded.

III. output_datasets: An empty folder to store the resulting datasets.

IV. input_datasets: This includes all the needed datasets we have rights to share.
    a. Panel_municipios: Folder with the Panel CEDE files. Accessible in https://datoscede.uniandes.edu.co/es/
    b. car.dta : Dataset with the distance to CAR offices from each municipality. Source: https://www.car.gov.co/
    c. fiscal.dta: Dataset with the aggregated fiscal measures. Data shared by DNP. Accessible at: https://www.dnp.gov.co/programas/desarrollo-territorial/Estudios-Territoriales/Indicadores-y-Mediciones/Paginas/desempeno-fiscal.aspx
    d. council_size.dta : Data with the municipal council size. Source: Hangartner, Dominik and Ruiz, Nelson and Tukiainen, Janne and Tukiainen, Janne (2019) Acessible at: https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3418767
    e. Database_URosario_2008-2011 Short.dta : Selected variables from the Rosarios violence dataset - Violence (Restrepo, Vargas, and Spagat (2003))
    f. entrada_agro.dta : Dataset of the entry of agro and cattle firms by municipality. Souce: Data shared by confecámaras
    g. fiscal.dta : National Department of Planning of Colombia's (DNP) fiscal accounts. Source: DNP - https://www.dnp.gov.co/programas/desarrollo-territorial/Estudios-Territoriales/Indicadores-y-Mediciones/Paginas/desempeno-fiscal.aspx
    h. MunCharacteristics_v1.dta : Dataset with additional municipality characteristics. Source: Panel CEDE - https://datoscede.uniandes.edu.co/es/
    i. NighttimeLights.dta : Night time lights data. Source:"Landmines: The Local Effects of Demining," Prem, Purroy and Vargas. (Data shared by the authors)
    j. parques.dta : Dataset with area of natural parks by municipality. Source: "Landmines: The Local Effects of Demining," Prem, Purroy and Vargas. (Data shared by the authors)
    k. prod_agro_2011.dta : Dataset with total agro production in tons. Source: Panel CEDE - https://datoscede.uniandes.edu.co/es/
    l. roads_munstats_2012_2016.dta : Dataset with length and count of roads. Source: "The Perils of Top-down State Building: Evidence from Colombia's False Positives" (2020), American Economic Journal: Economic Policy, Vol. 12(3): 1-43 (D. Acemoglu, L. Fergusson. J. Robinson, D. Romero and J. Vargas) (Data shared by the authors)
    m. rueda_ruiz.dta : Salaries data. Source: Rueda Robayo, Miguel; Nelson A. Ruiz (2020). Accessible at: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/P7NAJW
    n. total_offices.dta : Dataset with total offices . Source: "Landmines: The Local Effects of Demining," Prem, Purroy and Vargas. (Data shared by the authors)
    o. contract.dta. Dataset with the aggregated measures of contracts signed by the municipality. The data used here was acquired from SECOP. This data is massive and also third party. Please visit the online portal: https://www.colombiacompra.gov.co/secop-ii
    p. candidate_individual_data.dta NOT SHARED due to privacy concerns: Data contains personally identifiable information (PII) that is sensitive in nature. Individual-level data from politicians from  Ruiz, Nelson. 2017. "The Power of Money. The Consequences of Electing a Donor Funded Politician.", for data sources of this paper please check data section.
    q. actual_muni_codes.dta NOT SHARED due to privacy concerns: Data contains personally identifiable information (PII) that is sensitive in nature. Anonymisation of municipalities codes so they can't be identified.
 
  Individual data
    r. Alcalde_2011_corregida.dta: Electoral dataset from Universidad de los Andes. 
    s. electoral_info.dta: Ideology party done by Fergusson, Leopoldo, et al. “The Real Winner’s Curse.” American Journal of Political Science, vol. 65, no. 1, 2021, pp. 52–68.
    t. candidates_2011.txt: Data by formal request to the national registry office. 
    u. divipol.csv: Codes from registraduria to DANE codes. Provided by the Colombia's National Registy
    v. municipality codes2.dta: Data with the official municipalities' codes from DANE.
    w. Income2011.dta: Campaing income data. Formal request to Cuentas Claras.
    x. Sanciones_alcaldes.csv: Contains the date of execution of the processes in which each of the candidates for Council in Colombia were involved, in all municipalities and from 2003 to 2015. Council candidates in Colombia, in all municipalities and from the elections of 2003 to 2015. Source: Contraloria General de la Republica.
    y. base_censo_electoral.csv: Registration patters. Delivered by the electoral commission.
    z. facial_char.csv: Data on candidates facial characteristics from: Ruiz, Nelson. 2017. "The Power of Money. The Consequences of Electing a Donor Funded Politician."
    aa. pending 

*---------------- THIRD PARTY SOURCES / HOW TO GAIN ACCESS

I. Deforestation:
    The forest cover data used in the paper comes from: Hansen, M. C., P. V. Potapov, R. Moore, M. Hancher, S. A. Turubanova, A. Tyukavina, D. Thau, S. V. Stehman, 
    S. J. Goetz, T. R. Loveland, A. Kommareddy, A. Egorov, L. Chini, C. O. Justice, and J. R. G. Townshend. 2013. “High-Resolution Global Maps of 21st-Century Forest 
    Cover Change.” Science 342 (15 November): 850–53. 
    
    HOW TO ACCESS IT:
    Data available on-line from: https://glad.earthengine.app/view/global-forest-change. 
    Please download the corresponding images for Colombia. 
	We used the version 2019-v1.7 of the data. The latest version at the time we constructed the data. 
    	The provided code is able to download the needed data for you. 
    	
II. Landuse: 
    Land cover data: ESA. Land Cover CCI Product User Guide Version 2. Tech. Rep. (2017). 

    HOW TO ACCESS IT:
    Data available on-line from: maps.elie.ucl.ac.be/CCI/viewer/download/ESACCI-LC-Ph2-PUGv2_2.0.pdf 
    Please download the corresponding images following ESA's instructions: 
	We used the version 2019-v1.7 of the data. The latest version at the time we constructed the data.

III. Methane: 
    The original data comes from NASA and can be found at: https://airs.jpl.nasa.gov/data/get-data/standard-data/. 
    We use the Annual average methane concentration raster, 
    pre-processed by The Carnegie Endowment for International Peace and available at: https://open.africa/dataset/global-annual-average-methane-concentration-2016
    
    HOW TO ACCESS IT:
    Download the pre-processed raster availible at:
    Available at: https://open.africa/dataset/global-annual-average-methane-concentration-2016
   
IV. Fires: 
    Data delivered by the NASA firms platform. Online at: https://www.earthdata.nasa.gov/learn/find-data/near-real-time/firms. This data is shared within this replication package.
     
   HOW TO ACCESS IT:
   Open a online request at the FIRES platform:
	Online at: https://www.earthdata.nasa.gov/learn/find-data/near-real-time/firms.

V. Controls:
    a. Panel CEDE. Accessible in https://datoscede.uniandes.edu.co/es/
	HOW TO ACCESS IT: 
	Online at: https://datoscede.uniandes.edu.co/es/catalogo-de-microdata
	To gain access to the data as a third party (without filiation to Universidad de los Andes) you must make a formal request emailing to datoscede@uniandes.edu.co.

    b. Rosarios violence dataset - Violence (Restrepo, Vargas, and Spagat (2003))
    
    c. Data constructed by Prem, Purroy, and Vargas (2022) Landmines: The Local Effects of Demining. 
	HOW TO GAIN ACCESS:
	This is not yet publicly available but a copy of the data is shared within this replication package; with previous authors authorisation.
    d. Data constructed by : Rueda Robayo and Ruiz (2020). Political Agency, Election Quality, and Corruption.  
	HOW TO GAIN ACCESS:
	A copy of the dataset is availible within the replication package:
	Accessible at: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/P7NAJW

    e. Data constructed by : Hangartner, Dominik and Ruiz, Nelson and Tukiainen, Janne and Tukiainen, Janne (2019). "Open or Closed? How List Type Affects Electoral Performance, Candidate Selection, and Campaign Effort". 
	HOW TO GAIN ACCESS:
	Available at SSRN: https://ssrn.com/abstract=3418767 or http://dx.doi.org/10.2139/ssrn.3418767
	For data on council size contact: nelson.ruiz@essex.ac.uk

IV. Electoral data / Campaign/Personal Data of Politicians.
    Ruiz, Nelson. 2017. "The Power of Money. The Consequences of Electing a Donor Funded Politician.". This data is restricted and is not shared within this replication package due to containing personal identifiable information (PII). For data sources of this paper please check data section.
    
   Reconstruction of individual candidate data form Ruiz (2017). Needed sources:

   A. Get candidates winner and runner up data: 
	Source: Bases de datos electorales from Universidad de los Andes
	HOW TO ACCESS IT: 
	Online at: https://datoscede.uniandes.edu.co/es/noticias/49-infografia-bases-electorales
	To gain access to the data as a third party (without affiliation to Universidad de los Andes) you must make a formal request emailing to datoscede@uniandes.edu.co.

   B. Party ideology: 
	Source: Fergusson, L., Querubin, P., Ruiz, N.A. and Vargas, J.F. (2021), The Real Winner's Curse. American Journal of Political Science, 65: 52-68. https://doi.org/10.1111/ajps.12508
	HOW TO ACCESS IT: 
	Online at: https://onlinelibrary.wiley.com/doi/10.1111/ajps.12508
	This paper has a dataverse repository including the data on party ideology. 

   C. Candidates national ID / Candidates ID in the future
	Source: Official request to Colombia's National Registry.
	How TO ACCESS IT:
	To gain access you must fill a formal request (know as PQRS). 
	The request can be made online at: https://gestordocumental.registraduria.gov.co:4450/pqrs/nonReg
	File a request asking for mayoral candidates national IDs for 2011 mayoral elections. 

   D. Campaign contributions:
	Source: Formal data request to Cuentas Claras. Cuentas Claras is a portal that reports campaign donations.
	HOW TO ACCESS IT:	
	Cuentas Claras portal is available at: https://www.cnecuentasclaras.gov.co/
	For requesting the data you'll have to fill a formal data request emailing to atencionalciudadano@cne.gov.co
   	File a request for campaign donations data, namely campaign donors to mayoral campaigns during the 2011 election.

   E. Sanctions by the comptrollership office before term:
 	Source: Ongoing research from Can political fiscal cycles be stopped? Evidence from a public spending ban near elections. by Luis R. Martinez and Nelson A. Ruiz.
	Original source: Formal request to the Colombia's Comptoller Office.
	HOW TO ACCESS IT:
	You can get more information online at: https://www.contraloria.gov.co/contraloria/sala-de-prensa/boletines-de-prensa
	To gain access to the data needed for this paper, you must file a formal request at cgr@contraloria.gov.co.
	File a request asking for the compilation of all indictment news and bulletins.

   F. Candidates facial features:
	Source: How Campaigns Respond to Ballot Position: A New Mechanism for Order Effects. Saad Gulzar, Thomas S. Robinson, and Nelson A. Ruiz. The Journal of Politics 2022 84:2, 1256-1261
	HOW TO ACCESS IT:
	To obtain this data please email the source paper author at nelson.ruiz@essex.ac.uk
	The raw data comes from running the Face Plus (https://www.faceplusplus.com/) following algorithm on ballots with pictures of 2011 mayoral elections.

   G. Political Experience:
	Source: Bases de datos electorales from Universidad de los Andes
	HOW TO ACCESS IT: 
	Online at: https://datoscede.uniandes.edu.co/es/noticias/49-infografia-bases-electorales
	To gain access to the data as a third party (without filiation to Universidad de los Andes) you must make a formal request emailing to datoscede@uniandes.edu.co.
	Code to produce the variables available in 007_political_experience.do.

*---------------- NOTICE ON THE NATURE OF THE SHARED DATA

As authors we shared all the possible information within our rights, including codes and datasets. However we do not share any restricted or sensitive information. This includes the codes and data related to the characteristics and political participation of particular individuals. 
    
    
    
*** END ***    
