library(stringr)

govscitbl <- read.csv("data_raw/govscitbl.csv")

govscitbl$Agency <- str_remove(govscitbl$Agency, '^The')

#remove "US" from beginning including duplicates
#TODO do we remove "US" and "California" as the whole word? [^\\b]
#TODO US underscores?
govscitbl$Agency <- str_remove(govscitbl$Agency, '^(US\\s|U\\.S\\.\\s){1,}')

#kings-custom filter for California and fed only
govscitbl <- govscitbl[govscitbl$State %in% c("federal", "California"),]

#list below adapted from govscienceuseR > referenceExtract > clean_functions.R
#govscienceuseR Development Team 
#(Tyler A. Scott, Liza Wood, and Arthur Koehl) 
#(2022-2023). govscienceuseR: 
#Tools for automated extraction and disambiguation of 
#scientific resources cited in government documents. 
#govscienceuseR.github.io

org_words <- c("Administration", "Agency", "Association", "Associates", "Authority",  
               "Board", "Bureau", "Center", "^Consult[a-z]+$",
               "Commission", "Council", "County",  "Department", "Datacenter", "District", 
               "Foundation", "Government[s]*", "Group", 
               "Institute", "LLC", "Laboratory", "Office", "Service", "Society", "Survey",  
               "Univeristy")
org_phrases <- as.vector(outer(org_words, c("of","on","for"), paste, sep = " "))
org_phrases <- paste(org_phrases, collapse = "|")

spl <- strsplit(govscitbl$Agency, ',\\s*')

orgs_reordered <- sapply(1:length(spl), function (x) {
   #if one of splits (opt: besides the first one) has an org word followed by "of" or "for" 
   if(sum(grepl(org_phrases, spl[[x]]))>0){
      #then move all of the following splits to the beginning:
      #find first match: grep(org_phrases, x)[1]
      #and put all the elements after that at the front of the vector
      sorted <- c(spl[[x]][grep(org_phrases, spl[[x]])[1]:length(spl[[x]])], spl[[x]][-(grep(org_phrases, spl[[x]])[1]:length(spl[[x]]))])
      #combine the first two elements so there's no comma after "of" / "for" / "on"
      if(length(sorted)>1){
         sorted <- c(paste(sorted[1],sorted[2]), sorted[-(1:2)])
      }
      sorted
   }else{spl[[x]]
   }
})


orgs_with_commas <- sapply(orgs_reordered, function (x) {paste(x, collapse = ", ")})

orgs_underscore <- sapply(strsplit(orgs_with_commas,',*\\s+'), function(x) paste(x, collapse = "_"))

govscitbl$Agency <- orgs_underscore

#sort alphabetically
govscitbl <- govscitbl[order(govscitbl$Agency),]
#removing certain abbrevs and unofficial names from agencies. We will move these to abbrev column.
govscitbl <- govscitbl[!(govscitbl$Agency %in% c("Agriculture_Department",
                                                 "Archives_National_Archives_and_Records_Administration",
                                                 "Bureau_of_Alcohol_and_Tobacco_Tax_and_Trade",
                                                 "Alcohol_Tobacco_Firearms_and_Explosives_Bureau",
                                                 "Bureau_of_Consumer_Financial_Protection",
                                                 "Bureau_of_the_Census",
                                                 "California_Energy_Commission",
                                                 "California_Governor‚Äôs_Office_of_Business_and_Economic_Development",
                                                 "California_Office_of_Statewide_Health_Planning_and_Development",
                                                 "CDC",
                                                 "Commerce_Department",
                                                 "Consumer Services, and Housing Agency California Business",
                                                 "Defense_Department",
                                                 "Energy_Department",
                                                 "Education_Department",
                                                 "Fair_Housing_and_Equal_Opportunity",
                                                 "Fannie_Mae",
                                                 "Freddie_Mac",
                                                 "Ginnie_Mae",
                                                 "Health_and_Human_Services_Department",
                                                 "Homeland_Security_Department",
                                                 "Indian_Affairs",
                                                 "Interior_Department",
                                                 "Interpol",
                                                 "Justice_Department",
                                                 "Kennedy_Center",
                                                 "Labor_Department",
                                                 "NASA",
                                                 "Archives_National_Archives_and_Records_Administration"
)
                         )]

#TODO also remove acronyms from $Agency list
#TODO make sure this occurs before tolower

#abbrevs for unique california and federal orgs/agencies
customabbr <- rbind(
   c("Administration_for_Children_and_Families", "ACF"),
   c("Administration_for_Community_Living","ACL"),
   c("Administrative_Conference_of_the_United_States","ACUS"),
   c("Administrative_Office_of_the_US_Courts","AOUSC"),
   c("Advisory_Council_on_Historic_Preservation","ACHP"),
   c("Agency_for_Global_Media","USAGM"),
   c("Agency_for_Healthcare_Research_and_Quality","AHRQ"),
   c("Agency_for_International_Development","USAID"),
   c("Agency_for_Toxic_Substances_and_Disease_Registry","ATSDR"),
   c("Alcohol_and_Tobacco_Tax_and_Trade_Bureau","TTB"),
   c("Alcohol_and_Tobacco_Tax_and_Trade_Bureau","Bureau_of_Alcohol_and_Tobacco_Tax_and_Trade"),
   c("Bureau_of_Alcohol_Tobacco_Firearms_and_Explosives","Alcohol_Tobacco_Firearms_and_Explosives_Bureau"),
   c("Bureau_of_Alcohol_Tobacco_Firearms_and_Explosives","ATF"),
   c("Animal_and_Plant_Health_Inspection_Service","APHIS"),
   c("National_Archives_and_Records_Administration","Archives_National_Archives_and_Records_Administration"),
   c("National_Archives_and_Records_Administration","NARA"),
   c("National_Archives_and_Records_Administration","National_Archives"),
   c("Center_for_Legislative_Archives",NA),
   c("Consumer_Financial_Protection_Bureau","Bureau_of_Consumer_Financial_Protection"),
   c("Consumer_Financial_Protection_Bureau","CFPB"),
   c("Bureau_of_Justice_Statistics","BJS"),
   c("Bureau_of_Labor_Statistics","BLS"),
   c("Bureau_of_Ocean_Energy_Management","BOEM"),
   c("Bureau_of_Safety_and_Environmental_Enforcement","BSEE"),
   c("Census_Bureau","Bureau_of_the_Census"),
   c("Census_Bureau","USCB"),
   c("California_Agricultural_Labor_Relations_Board", "ALRB"),
   c("California_Agricultural_Labor_Relations_Board","Agricultural_Labor_Relations_Board"),
   c("California_Air_Resources_Board","CARB"),#before tolower
   c("California_Alcoholic_Beverage_Control_Appeals_Board","ABCAB"),
   c("California_Alcoholic_Beverage_Control_Appeals_Board","ABC_Appeals_Board"),
   c("California_Alternative_Energy_and_Advanced_Transportation_Financing_Authority","CAEATFA"),
   c("California_Bureau_of_Household_Goods_and_Services","BHGS"),
   c("California_Central_Valley_Flood_Protection_Board","CVFPB"),
   c("California_Commission_on_Health_and_Safety_and_Workers'_Compensation","CHSWC"),
   c("California_Council_on_Criminal_Justice_and_Behavioral_Health","CCJBH"),
   c("California_Debt_Limit_Allocation_Committee","CDLAC"),
   c("California_Department_of_Child_Support_Services","DCSS"),#can have local chapters called the same thing
   c("California_Department_of_Corrections_and_Rehabilitation", "CDCR"),
   c("California_Department_of_FI$Cal","FI$Cal"),
   c("California_Department_of_Financial_Protection_and_Innovation","DFPI"),
   c("California_Department_of_Fish_and_Wildlife","CDFW"),
   c("California_Department_of_Food_and_Agriculture","CDFA"),
   c("California_Department_of_Forestry_and_Fire_Protection","CAL FIRE"),
   c("California_Department_of_Forestry_and_Fire_Protection","CALFIRE"),
   c("California_Department_of_Human_Resources","CalHR"),
   c("California_Department_of_Managed_Health_Care","DMHC"),
   c("California_Department_of_Motor_Vehicles","DMV"),#all states have one of these
   c("California_Department_of_Parks_and_Recreation","California_State_Parks"),
   c("California_Department_of_Pesticide_Regulation","CDPR"),
   c("California_Department_of_Public_Health","CDPH"),
   c("California_Department_of_Rehabilitation","DOR"),#other states have dor that means something different
   c("California_Department_of_Resources_Recycling_and_Recovery","CalRecycle"),
   c("California_Department_of_Tax_and_Fee_Administration","CDTFA"),
   c("California_Department_of_Toxic_Substances_Control","DTSC"),
   c("California_Department_of_Transportation","Caltrans"),
   c("California_Department_of_Veterans_Affairs","CalVet"),
   c("California_Department_of_Water_Resources","DWR"),
   c("California_Disabled_Veterans_Business_Enterprise_Advisory_Council", "DVBE_Advisory_Council"),
   c("California_Emergency_Medical_Services_Authority","EMSA"),#this also stands for European Maritime Safety Agency
   c("Energy_Resources_Conservation_and_Development_Commission","California_Energy_Commission"),
   c("California_Environmental_Protection_Agency","CalEPA"),
   c("California_Exposition_and_State_Fair","Cal Expo"),
   c("California_Fair_Political_Practices_Commission","FPPC"),
   c("California_Financing_Coordinating_Committee","CFCC"),#also a college in North Carolina
   c("California_Franchise_Tax_Board","FTB"),
   c("California_Government_Operations_Agency","GovOps"),
   c("California_Governor's_Office_of_Emergency_Services", "Cal OES"),
   c("California_Governor's_Office_of_Planning_and_Research", "Cal OPR"),#also called OPR but DOJ's Office of Prof. Resp. is also abbreviated OPR
   c("California_Governor's_Office_of_Business_and_Economic_Development", "GO-Biz"),
   c("California_Health_and_Human_Services_Agency","CalHHS"),
   c("California_Health_Facilities_Financing_Authority","CHFFA"),
   c("California_Healthy_Food_Financing_Initiative_Council", "CHFFIC"),
   c("California_Highway_Patrol","CHP"),#also community housing partners, in VA
   c("California_Historical_Records_Advisory_Board","CHRAB"),
   c("California_Housing_Finance_Agency","CalHFA"),
   c("California_Industrial_Development_Financing_Advisory_Commission","CIDFAC"),
   c("California_Infrastructure_and_Economic_Development_Bank", "CIEDB"),
   c("California_Legislative_Analyst's_Office","LAO"),
   c("California_Mental_Health_Services_Oversight_and_Accountability_Commission","MHSOAC"),
   #Cal Guard vs Calif. State Guard vs Calif National Guard vs Calif. Military Dept??
   c("California_Native_American_Heritage_Commission","NAHC"),#also stands for Natl. Assn. for Home Care and Hospice
   c("California_Natural_Resources_Agency","CNRA"),#also Calif. North Referee Administration for soccer
   c("California_Ocean_Protection_Council","OPC"),#also a company and a religious org
   c("California_Office_of_Administrative_Hearings","OAH"),#also org of american historians
   c("California_Office_of_Administrative_Law",""),
   c("California_Office_of_Digital_Innovation","ODI"),
   c("California_Office_of_Environmental_Health_Hazard_Assessment","OEHHA"),
   c("Center_for_Data_Insights_and_Innovation","CalOHII"),#old acronym
   c("Center_for_Data_Insights_and_Innovation","California_Office_of_Health_Information_Integrity"),#old name
   c("Center_for_Data_Insights_and_Innovation","CDII"),
   c("Center_for_Data_Insights_and_Innovation","CalCDII"),
   c("California_Department_of_Health_Care_Access_and_Information","HCAI"),
   c("California_Department_of_Health_Care_Access_and_Information","California_Office_of_Statewide_Health_Planning_and_Development"),#previous name
   c("California_Department_of_Health_Care_Access_and_Information","OSHPD"),
   #OSI is the name of a CA company and CA gov office
   c("California_Office_of_the_State_Fire_Marshal","OSFM"),
   c("California_Pollution_Control_Financing_Authority","CPCFA"),
   c("California_Prison_Industry_Authority","CalPIA"),
   c("California_Prison_Industry_Authority","CALPIA"),
   c("California_Public_Employees_Retirement_System","CalPERS"),
   c("California_Public_Employment_Relations_Board","PERB"),#many states have one of these
   c("California_Public_Utilities_Commission","CPUC"),
   c("California_San_Francisco_Bay_Conservation_and_Development_Commission","BCDC"),
   c("California_San_Francisco_Bay_Conservation_and_Development_Commission","Bay_Conservation_and_Development_Commission"),
   c("California_State_Council_on_Developmental_Disabilities","SCDD"),
   c("California_State_Historical_Resources_Commission","SHRC"),
   c("California_State_Mining_and_Geology_Board","SMGB"),
   c("California_State_Parks_and_Recreation_Commission","SPRC"),#also a national mental health center
   c("California_State_Teachers_Retirement_System","CalSTRS"),
   c("California_State_Transportation_Agency","CalSTA"),
   c("California_State_Water_Resources_Control_Board","SWRCB"),
   c("California_State_Water_Resources_Control_Board","State_Water_Board"),
   c("California_Tax_Credit_Allocation_Committee","CTCAC"),
   c("California_Veterans_Board","CalVet"),
   c("California_Veterinary_Medical_Board","VMB"),
   c("California_Victim_Compensation_Board","CalVCB"),
   c("California_Worker's_Compensation_Appeals_Board","WCAB"),
   c("California_Workforce_Development_Board","CWDB"),
   c("Center_for_Food_Safety_and_Applied_Nutrition","CFSAN"),
   c("Center_for_Nutrition_Policy_and_Promotion","CNPP"),
   c("Center_for_Parent_Information_and_Resources","CPIR"),
   c("Centers_for_Medicare_and_Medicaid_Services","CMS"),
   c("Central_Intelligence_Agency","CIA"),
   c("Chemical_Safety_Board","CSB"),
   c("Chief_Acquisition_Officers_Council","CAO_Council"),
   c("Chief_Financial_Officers_Council","CFO_Council"),
   c("Chief_Human_Capital_Officers_Council","CHCO_Council"),
   c("Chief_Information_Officers_Council","CIO_Council"),
   #csac means too many things
   c("Citizenship_and_Immigration_Services","UCSIS"),
   c("Coast_Guard","USCG"),
   c("Department_of_Commerce","Commerce_Department"),
   c("Commission_on_International_Religious_Freedom","USCIRF"),
   c("Commission_on_Security_and_Cooperation_in_Europe","CSCE"),
   c("Committee_on_Foreign_Investment_in_the_United_States","CFIUS"),
   c("Commodity_Futures_Trading_Commission","CFTC"),
   c("Community_Oriented_Policing_Services","COPS"),
   c("Congressional_Budget_Office","CBO"),
   c("Consumer_Product_Safety_Commission","CPSC"),
   c("California_Business_Consumer_Services_and_Housing_Agency","BCSH"),
   c("Physical Therapy Board of California","PTBC"),
   c("University_of_California","UC"),
   c("Coordinating_Council_on_Juvenile_Justice_and_Delinquency_Prevention","CCJJDP"),
   c("Army_Corps_of_Engineers","Corps_of_Engineers"),
   c("Army_Corps_of_Engineers","ACE"),#before tolower!
   c("Army_Corps_of_Engineers","ACOE"),
   c("Army_Corps_of_Engineers","USACE"),
   c("Army_Corps_of_Engineers","USACOE"),
   c("Council_of_the_Inspectors_General_on_Integrity_and_Efficiency","CIGIE"),
   c("Council_on_Environmental_Quality","CEQ"),
   c("Court_Services_and_Offender_Supervision_Agency_for_the_District_of_Columbia","CSOSA"),
   c("Customs_and_Border_Protection","CBP"),
   c("Defense_Acquisition_University","DAU"),
   c("Defense_Advanced_Research_Projects_Agency","DARPA"),
   c("Defense_Contract_Audit_Agency","DCAA"),
   c("Defense_Contract_Management_Agency","DCMA"),
   c("Defense_Counterintelligence_and_Security_Agency","DCSA"),
   c("Department_of_Defense","Defense_Department"),
   c("Department_of_Defense","DOD"),
   c("Department_of_Defense","DoD"),
   c("Department_of_Defense","USDOD"),
   c("Defense_Information_Systems_Agency","DISA"),
   c("Defense_Nuclear_Facilities_Safety_Board","DNFSB"),
   c("Defense_Technical_Information_Center","DTIC"),
   c("Defense_Threat_Reduction_Agency","DTRA"),
   c("Delaware_River_Basin_Commission","DRBC"),
   c("Dental_Hygiene_Board_of_California","DHBC"),
   c("Department_of_Agriculture","USDA"),
   c("Department_of_Agriculture","Agriculture_Department"),
   c("Department_of_Education","Education_Department"),
   #dept of commerce to "doc" not ideal to disambiguate in a document
   c("Department_of_Energy","DoE"),#dept of education is not shortened to doe
   c("Department_of_Energy","DOE"),
   c("Department_of_Energy","Energy_Department"),
   c("Department_of_Health_and_Human_Services","HHS"),
   c("Department_of_Health_and_Human_Services","Health_and_Human_Services_Department"),
   c("Department_of_Health_and_Human_Services_Office_for_Civil_Rights","OCR"),#also used to denote pdf to word extraction
   c("Department_of_Homeland_Security","DHS"),
   c("Department_of_Homeland_Security","Homeland_Security_Department"),
   c("Department_of_Housing_and_Urban_Development","HUD"),
   c("Department_of_Justice","DoJ"),
   c("Department_of_Justice","DOJ"),
   c("Department_of_Justice","Justice_Department"),
   c("Department_of_Labor","DoL"),
   c("Department_of_Labor","DOL"),
   c("Department_of_Labor","Labor_Department"),
   #dept of interior doi could get picked up in citations
   c("Department_of_the_Interior","Interior_Department"),
   c("Department_of_Transportation","DoT"),
   c("Department_of_Transportation","DOT"),
   #Veterans Affairs VA could get picked up as virginia
   c("Drug_Enforcement_Administration","DEA"),
   c("Economic_Research_Service","ERS"),
   c("Election_Assistance_Commission","EAC"),#also an intergovernmental org in East Africa
   c("Employee_Benefits_Security_Administration","EBSA"),
   c("Environmental_Protection_Agency","EPA"),
   c("Environmental_Protection_Agency","US_EPA"),
   c("Environmental_Protection_Agency","USEPA"),
   c("Equal_Employment_Opportunity_Commission","EEOC"),
   c("Executive_Office_for_Immigration_Review","EOIR"),
   c("Export-Import_Bank_of_the_United_States","EXIM"),#exim is also an email keyword
   c("Office_of_Fair_Housing_and_Equal_Opportunity","FHEO"),
   c("Office_of_Fair_Housing_and_Equal_Opportunity","Fair_Housing_and_Equal_Opportunity"),
   c("Farm_Credit_System_Insurance_Corporation","FCSIC"),
   c("Federal_Accounting_Standards_Advisory_Board","FASAB"),
   c("Federal_Aviation_Administration","FAA"),
   c("Federal_Bureau_of_Investigation","FBI"),
   c("Federal_Deposit_Insurance_Corporation","FDIC"),
   c("Federal_Election_Commission","FEC"),
   c("Federal_Emergency_Management_Agency","FEMA"),
   c("Federal_Energy_Regulatory_Commission","FERC"),
   c("Federal_Financial_Institutions_Examination_Council","FFIEC"),
   c("Federal_Geographic_Data_Committee","FGDC"),
   c("Federal_Highway_Administration","FHWA"),
   c("Federal_Housing_Administration","FHA"),
   c("Federal_Home_Loan_Mortgage_Corporation","Freddie_Mac"),
   c("Federal_Housing_Finance_Agency","FHFA"),
   c("Federal_Labor_Relations_Authority","FLRA"),
   c("Federal_Laboratory_Consortium_for_Technology_Transfer","FLC"),
   c("Federal_Laboratory_Consortium_for_Technology_Transfer","Federal_Laboratory_Consortium"),
   c("Federal_Law_Enforcement_Training_Center","FLETC"),
   c("Federal_Mediation_and_Conciliation_Service","FMCS"),
   c("Federal_Motor_Carrier_Safety_Administration","FMCSA"),
   c("Federal_National_Mortgage_Association","Fannie_Mae"),
   c("Federal_Railroad_Administration","FRA"),
   c("Federal_Retirement_Thrift_Investment_Board","FRTIB"),
   c("Federal_Student_Aid_Information_Center","FSAIC"),
   c("Federal_Trade_Commission","FTC"),
   c("Federal_Transit_Administration","FTA"),
   c("Fish_and_Wildlife_Service","FWS"),#also federal work study
   c("Fish_and_Wildlife_Service","USFWS"),
   c("Fish_and_Wildlife_Service","US_FWS"),
   c("Food_and_Drug_Administration","FDA"),
   c("Food_and_Nutrition_Service","FNS"),
   c("Food_Safety_and_Inspection_Service","FSIS"),
   c("Foreign_Claims_Settlement_Commission","FCSC"),#also soccer
   c("Forest_Service","USFS"),
   c("Geological_Survey","USGS"),
   c("Government_Accountability_Office","GAO"),
   c("Government_National_Mortgage_Association","Ginnie_Mae"),
   c("Grain_Inspection_Packers_and_Stockyards_Administration","GIPSA"),
   c("Health_Resources_and_Services_Administration","HRSA"),
   c("Immigration_and_Customs_Enforcement","ICE"),#before tolower
   c("Bureau_of_Indian_Affairs","BIA"),
   c("Bureau_of_Indian_Affairs","Indian_Affairs"),
   c("Indoor_Air_Quality","IAQ"),
   c("Interagency_Alternative_Dispute_Resolution_Working_Group","Interagency_ADR_Working_Group"),
   c("Interagency_Committee_for_the_Management_of_Noxious_and_Exotic_Weeds","FICMNEW"),
   c("Internal_Revenue_Service","IRS"),
   c("International_Criminal_Police_Organization","Interpol"),
   c("International_Criminal_Police_Organization","INTERPOL"),
   c("International_Criminal_Police_Organization","InterPol"),
   c("International_Criminal_Police_Organization","National_Central_Bureau_-_Interpol"),
   c("Japan-United_States_Friendship_Commission","JUSFC"),
   c("John_F_Kennedy_Center_for_the_Performing_Arts","Kennedy_Center"),
   c("Joint_Congressional_Committee_on_Inaugural_Ceremonies","JCCIC"),
   c("Joint_Fire_Science_Program","JFSP"),
   c("Joint_Program_Executive_Office_for_Chemical_and_Biological_Defense","JPEO-CBRND"),
   c("Judicial_Panel_on_Multidistrict_Litigation","JPML"),
   c("Maritime_Administration","MARAD"),
   c("Merit_Systems_Protection_Board","MSPB"),
   c("Military_Academy_West_Point","West_Point"),
   c("Mine_Safety_and_Health_Administration","MSHA"),
   c("California_Division_of_Occupational_Safety_and_Health","Cal/OSHA"),#TODO make sure this slash doesn't mess stuff up
   c("National_Aeronautics_and_Space_Administration","NASA"),
   c("National_Agricultural_Statistics_Service","NASS"),
   c("National_Cancer_Institute","NCI"),
   c("National_Credit_Union_Administration","NCUA"),
   c("National_Flood_Insurance_Program","NFIP"),
   c("National_Geospatial-Intelligence_Agency","NGIA"),
   c("National_Health_Information_Center","NHIC"),
   c("National_Heart_Lung_and_Blood_Institute","NHLBI"),
   c("National_Highway_Traffic_Safety_Administration","NHTSA"),
   c("National_Indian_Gaming_Commission","NIGC"),
   
   
)
#TODO check on 2293 and 2232
#TODO check on entries with apostrophes
#TODO throw error if identical acronyms
#TODO remove acronyms
#TODO house of reps is duplicated


names(customabbr) <- c("org", "abbrevs")
#make first blank entry for these agencies equal to these new abbreviations:
govscitbl$Abbr[govscitbl$Agency==customabbr$org & nchar(govscitbl$Abbr)==0] <- customabbr$abbrevs
#make lists of agencies without 
