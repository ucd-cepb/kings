pack = c('readtext','tokenizers','pbapply','data.table','doParallel','stringr','tidyverse','pdftools','googleLanguageR','rvest')
need = pack[!pack %in% installed.packages()[,'Package']]
lapply(need,install.packages)
lapply(pack,require,character.only=T)


############## THIS CODE DEMONSTRATES HOW THE INITIAL PAGE IDENTIFICATION AND FILTERING WAS DONE ######
##### THIS WAS PERFORMED ON RAW PDF FILES (ALL PUBLICLY AVAILABLE FROM THE EPA) THAT ARE NOT STORED WITH THE PROJECT ###### 
#########
  
tfiles <- list.files('Multipurpose_Files/portal_files/',pattern = 'txt$',full.names = T)

test <- fread(tfiles[1],'<<PAGE BREAK>>')



good_string = 'PREPARERS|CONTRIBUTORS|Preparers|Contributors|Consultants|Interdisciplinary Team'#LIST OF PREPARERS|List of Preparers|List Of Preparers|PREPARERS AND CONTRIBUTORS|Preparers and Contributors|Preparers \\& Contributors|Preparers And Contributors'
bad_string = 'TABLE OF CONTENTS|Table of Contents|^CONTENTS|Contents|How to Use This Document|Date Received'
pages_list = pblapply(tfiles,function(i) {
  base_file = basename(i)
  id = str_remove(basename(base_file),'_.*')
  #start_dt = data.table(FILE_NAME = base_file,PROJECT_ID = id,people = list(),orgs = list())
  temp_txt =  fread(i,sep = '\t')
  temp_txt$FILE = base_file
  temp_txt$keep = temp_txt$keep2 = 0
  temp_txt$looks_like_frontmatter = !grepl('[A-Z0-9]',str_remove(temp_txt$text,'.*\\s'))
  #  temp_txt = temp_txt[grepl('[A-Z0-9]',str_remove(temp_txt$text,'.*\\s')),]
  prep_papers = grepl(good_string,str_sub(temp_txt$text,1,200))&!grepl(bad_string,str_sub(temp_txt$text,1,200))
  temp_txt$keep[prep_papers] <- 1
  prep_papers2 = grepl(good_string,  str_sub(temp_txt$text,nchar(temp_txt$text)-200,nchar(temp_txt$text)))&!grepl(bad_string,  str_sub(temp_txt$text,1,200))
  temp_txt$keep2[prep_papers2] <- 1
  temp_txt = temp_txt[!duplicated(text),]
  temp_txt$count = str_count(temp_txt$text,good_string) - str_count(temp_txt$text,bad_string)

  if(any(temp_txt$count[!temp_txt$looks_like_frontmatter>0])){
    temp_txt = temp_txt[!temp_txt$looks_like_frontmatter,]
  }
  if(all(temp_txt$count<=0)&any(temp_txt$keep)){
    temp_txt = temp_txt[keep==1,]
  }
  temp_txt[,.(Page,count,keep,keep2)]
  temp_txt = temp_txt[Page>=min(temp_txt$Page[which(temp_txt$count==max(temp_txt$count))]),]

  if(any(temp_txt$keep!=0)){
    add = sort(unique(c(sapply(which(temp_txt$keep==1), function(x) x + 0:2))))
    add = add[add<=nrow(temp_txt)]
    temp_txt$keep[add]<-1
  }
  if(any(temp_txt$keep2!=0)){
    add = sort(unique(c(sapply(which(temp_txt$keep2==1), function(x) x + 0:2))))
    add = add[add<=nrow(temp_txt)]
    temp_txt$keep2[add]<-1
  }
  if(any(temp_txt$keep>0|temp_txt$keep2>0)){
    temp_txt = temp_txt[keep==1|keep2==1,]}
  if(all(temp_txt$keep==0&temp_txt$keep2==0)){
    temp_txt = temp_txt[count>0,]
  }

  temp_txt = temp_txt[keep!=0|keep2!=0|temp_txt$count>0,]
  #  temp_txt = temp_txt[!grepl('\\.{8,}',text),]
  temp_txt = temp_txt[!{grepl('References Cited|Works Cited|REFERENCES|Index|INDEX',temp_txt$text) & !grepl('Preparers|Contributors|PREPARERS|CONTRIBUTORS',temp_txt$text)},]
  temp_txt
},cl = 6)
pages_dt = rbindlist(pages_list,use.names = T)
pages_dt$USE_KEEP2 = pages_dt$FILE %in% pages_dt[,list(sum(keep),sum(keep2)),by=.(FILE)][V1>10&V2>0]$FILE
pages_dt = pages_dt[{!USE_KEEP2}|keep2==1,]

pdf_file_list = list.files('enepa_repository/documents/',full.names = T,recursive = T)
base_name = basename(pdf_file_list)
raw_pages = pblapply(seq(nrow(pages_dt)),function(i){
  print(i)
  pdf_file = str_replace(pages_dt[i,]$FILE,'txt$','pdf')
  floc = pdf_file_list[match(pdf_file,base_name)]
  text_pages = tryCatch({pdftools::pdf_text(floc)[pages_dt[i,]$Page]},
                        error = function(e) NULL)
  text_pages},cl = 4)
raw_pages[sapply(raw_pages,is.null)] <- NA
pages_dt$text <- unlist(raw_pages)
saveRDS(pages_dt,'../tuolumne/boilerplate_project/input/detected_preparer_pages_uncleaned.RDS')
###############################


pages_dt = readRDS('boilerplate_project/input/detected_preparer_pages_uncleaned.rds')
pages_by_file = split(pages_dt$Page,pages_dt$FILE)

cluster = makeCluster(detectCores() * 0.75)
registerDoParallel(cluster)
clusterEvalQ(cluster,require(data.table))
clusterEvalQ(cluster,require(pdftools))
clusterEvalQ(cluster,require(stringr))
clusterEvalQ(cluster,require(googleLanguageR))
clusterExport(cluster,varlist = list('account_key'))
clusterEvalQ(cluster,googleLanguageR::gl_auth(json_file = account_key))


dir.create('boilerplate_project/data_products/')

nlp_results = foreach(t = pages_dt$text,f = pages_dt$FILE,id = str_extract(pages_dt$FILE,'^[0-9]{8}')) %dopar% {
  temp = googleLanguageR::gl_nlp(string = t,nlp_type = 'analyzeEntities',type = 'PLAIN_TEXT',language = 'en')
  data.table(FILE = f,PROJECT_ID = id,temp$entities[[1]])
}



ent_all = rbindlist(nlp_results,use.names = T,fill = T)
ents_dt = ent_all[type!='NUMBER'&!is.na(type)&mention_type == 'PROPER',]
ents_dt = ents_dt[!ents_dt$name %in% state.name,]

ents_dt$name = str_remove(ents_dt$name,'\\s{4,}.*')
ents_dt = ents_dt[type == 'PERSON',]
ents_dt = ents_dt[!grepl('et_al',name),]
ents_dt = ents_dt[!grepl('_',name),]
ents_dt = ents_dt[grepl('[A-Z]',name),]


ents_dt$name <- str_remove(ents_dt$name,'\\s+$')
#ents_dt$name <- str_remove_all(ents_dt$name,'\\s')
ents_dt$name <- str_remove(ents_dt$name,'_{1,}$')
ents_dt$name <- str_remove(ents_dt$name,'^_{1,}')

ents_dt = ents_dt[!grepl('^[A-Z]\\._[A-Z]',name),]
ents_dt = ents_dt[!grepl('^[A-Z]\\.[A-Z]\\._[A-Z]',name),]
ents_dt = ents_dt[!grepl('^[0-9]',name),]
ents_dt = ents_dt[!grepl('[0-9]{3,}',name),]
ents_dt = ents_dt[!grepl('County',name),]
ents_dt = ents_dt[!grepl('District',name),]
ents_dt = ents_dt[!grepl('^sandy_',name),]

ents_dt = ents_dt[!grepl('Biologist|Planner|Engineer|Environmental|Biology|Wildlife|Grazing|Canyon|Physical|Technician|Land_Use|Nez_Perce|Anchorage|Alaska|Pacific|Oxide|Harvest|Apache|Ranger|Reviewer|Landscape|Geography|Polytechnic|Shoshone|Draft|^Map_|Salmon|Stakeholder|Watershed|Scientist|Fisheries|Design|Specialist|Appendix|Riparian|EIS|Ecologist|University|Raw_Score',name),]

ents_dt = ents_dt[grepl('\\s',name),]



td = tempdir()
adm_url = "https://geonames.usgs.gov/docs/stategaz/NationalFile.zip"
tf = tempfile(tmpdir=td, fileext=".zip")
download.file(adm_url, tf)
fname = unzip(tf, list=TRUE)
unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
fpath = file.path(td, grep('txt$',fname$Name,value=T))

ngaz = fread(fpath,sep = '|',fill = T,stringsAsFactors = F,quote = "")
ngaz$CFIPS = ifelse(!is.na(ngaz$COUNTY_NUMERIC),paste0(formatC(ngaz$STATE_NUMERIC,width = 2,flag = '0'),formatC(ngaz$COUNTY_NUMERIC,width = 3,flag = '0')),NA)
ngaz = ngaz[!ngaz$FEATURE_NAME %in% c('Pacific Ocean','South Pacific Ocean','Atlantic Ocean','Gulf of Mexico','Caribbean Sea'),]
ngaz = ngaz[!is.na(ngaz$FEATURE_CLASS),]
ngaz = ngaz[!is.na(ngaz$COUNTY_NUMERIC),]
drop_types = c('Tower')
ngaz = ngaz[!ngaz$FEATURE_CLASS %in% drop_types,]
ngaz = ngaz[ngaz$STATE_NAME != ngaz$FEATURE_NAME,]
ngaz = ngaz[!ngaz$FEATURE_NAME %in% c('River','Wilderness','Forest','Lake','Field','Beach','Mountain','Pacific','Atlantic','Earth','Acres','North','South','East','West',state.name),]
gc()
ngaz$FEATURE_NAME2 = str_replace_all(ngaz$FEATURE_NAME,pattern = '\\s','_')


ents_dt = ents_dt[!name %in% ngaz$FEATURE_NAME2,]
ents_dt = ents_dt[!grepl('[A-Z]{3,}',name),]
ents_dt = ents_dt[!grepl('[0-9]{2,}',name),]
ents_dt = ents_dt[!grepl('^([A-Z]\\.){2,}',name),]
ents_dt$name = str_remove(ents_dt$name,'_(–|-|—).*')
ents_dt$name = str_remove(ents_dt$name,'(__|_\\().*')
ents_dt = ents_dt[!grepl('_[a-z]',ents_dt$name),]


ents_dt$PROJECT_ID = str_remove(ents_dt$FILE,'_.*')
ents_dt= ents_dt[name %in% ents_dt[!duplicated(paste(name,PROJECT_ID)),][,.N,by=.(name)][order(-N)][N>1,]$name,]


bad_ents = c('QC_Reference','Date_Received','Wildlife_Biologist',"Environmental_Scientist" ,"Wetland_Geomorphic_Setting"   ,"Environmental_Science" ,
             "Land_Use"   , "Date_Received"   ,"QC_Reference"     , "Wetland_Site_Name" ,"Automatic_Hammer"  ,"Landscape_Architect"  ,"STLC_DI"  ,"Soil_Scientist" ,
             "CDOT_Geot","Wet_Sand_/_Muck_Flats" , "Landscape_Architecture" ,"Marine_Science"  ,"District_Ranger" ,"Colorado_Parks"   ,"Wildlife_Science" ,
             "Kootenai_Tribes" ,"YEH_ASSOCIATES_W_LONG", "Soil_Science", "Earthstar_Geographics"     ,  "Hydrologically_Disconnected","Park_Ranger" ,"STLC_DI__Complete"  ,
             "Endosulfan_II" ,"Endosulfan_Sulfate" ,"STLC_DI__Complete"  , "Douglas_-_fir" ,"Heptachlor_Epoxide","Wildlife_Biology","Land_Use_Permitting",
             "Noise_Specialist","Build_Alternatives","Appendix_D","Realty_Specialist","Virginia_Tech","Wild_Horses","Technical_Reviewer","Endrin_ketone__<","Reduce_Bat_Fatalities" ,
             "Medicine_Bow" ,   "Mark_Udall" , "Juris_Doctor","Planetary_Science"  , "Reducing_Avian_Collisions",    "P.E.__M.S."   , "Mule_Deer",
             "Date_Received__08/08/16","SANDY_CLAYSTONE","Tetra_Tech","Booz_Allen_Hamilton","NAVFAC_Northwest","Cardno_ENTRIX","Raw_Score_/36", "Technical_Memorandum" ,
             "Southeast_Alaska","I.__Soils","Soil_Profile","Hydrologically__Disconnected","Grassy_Forks","Stream_Photograph","Buffalo_PRMP","John_Hickenlooper",
             "Casa_Diablo_IV","Noise_Analyst","Stream_Photograph","Peckham_Ave","Map_ID_Site__Database(s","John_Barrasso","Mt._Baker","Groundwater_Resources",
             "Canada_Lynx","Ute_Mountain_Ute","Dianne_Feinstein","Northwest_Science","Kootenai_Tribe","Word_Processor","Graphic_Artist","Dick_Artley","Jeff_Merkley",
             "Noise_Analysis","Hazardous_Waste","Barbara_Boxer","Juris_Doctorate","Shoshone","Potomac_Yard","A-_Gil","Casa_Diablo_IV","Myotis_sodalis","Klamath_Tribes",
             "Burns_Paiute","Illustrated_Flora","Visual_Resources","Chief_Ranger","Indiana_Bat","PO_Box","PF_Doc","Arizona_Game","Forestry_Technician","P.E.__B.S.",
             "Montana_Fish","Suite_B__LOCATIONS","USDI_Fish","EIS_Reviewer","Page_1","Pool_Bypass","Sherman_Cattle","Good__","Airspeed_Altitude__SEL","SAND_FILL","CH2_M_Hill",
             "Jicarilla_Apache_Tribe","Marine_Mammal_Science"   ,"Mar._Res","Sand_Point_Way_NE"  ,"Fremont Weir","L Margolis",
             "American Indian","Architectural Historian","Forest Supervisor","Native Hawaiian","Native Americans","Shallow Aquitard",
             "TPH_Gasoline_C4-C12" , "CH2_M_HILL","Louis_Berger" ,"Livestock_Grazing", "Endrin_Aldehyde" ,"Amec_Foster_Wheeler" ,"Wildlife_Biology","Bog__Fen","Boise_Idaho","BA_Geography" ,
             'Marl_Seeps',"Juris Doctor" ,"Native American"  ,"Dianne Feinstein" , "Barbara Boxer"  ,"Certified Silviculturist" , "American Midland Naturalist",
             "John Day","Responsible Official",'Confederated Salish','Confederated Tribes','Dick Artley','Wild Horse',"Theodore Roosevelt",
             "Forest Botanist","Migratory Birds","Ron Wyden","John Barrasso","Interdisciplinary Team Members","Longworth H.O.B.","John Muir Project","Title VI","Odobenus rosmarus divergens")


ents_dt = ents_dt[!name%in% bad_ents,]
ents_dt = ents_dt[!grepl('^[A-Z]\\.',name),]

person_dt = ents_dt

ent_dt = ent_all
ent_dt$PROJECT_ID <- str_remove(ent_dt$FILE,'_.*')
#ent_dt = ent_dt[grepl('[A-Za-z0-9]',name),]
ent_dt = ent_dt[grepl('[A-Z]',name),]
ent_dt = ent_dt[!grepl('[0-9]{3,}',name),]
ent_dt = ent_dt[!grepl('[0-9]$',name),]
ent_dt$name = str_remove(ent_dt$name,'\\s{4,}.*')
ent_dt = ent_dt[!grepl('http',name,perl = T),]
ent_dt = ent_dt[!grepl('\\=',name,perl = T),]
ent_dt = ent_dt[!name %in%state.name,]
# drop all names observed in more than 100 EISs

ent_dt = ent_dt[!name %in% ent_dt[!duplicated(paste(name,PROJECT_ID)),.N,by=.(name)][order(-N),][N>80,]$name,]
ent_dt = ent_dt[!grepl('\\(',ent_dt$name),]

#full_tlist <- readRDS('scratch/boilerplate/big_text_files/big_eis_text.rds')
ent_dt$name[grepl('STRATIFIED ENVIRONMENTAL (&|AND) ARCHAEOLOGICAL',toupper(ent_dt$name))] <- "STRATIFIED ENVIRONMENTAL & ARCHAEOLOGICAL SERVICES, LLC"
ent_dt$name[grepl('WALSH ENVIRON',toupper(ent_dt$name))] <- "WALSH ENVIRONMENTAL SCIENTISTS AND ENGINEERS, LLC"
ent_dt$name[grepl('KERLINGER',toupper(ent_dt$name))] <- "CURRY & KERLINGER, LLC"
ent_dt$name[grepl('SOLV LLC',toupper(ent_dt$name))|toupper(ent_dt$name)=='SOLV'] <- 'SOLVE, LLC'
ent_dt$name[grepl('GRETTE ASSOC',toupper(ent_dt$name))] <- "GRETTE ASSOCIATES, LLC"
ent_dt$name[grepl('^HELIA ENV',toupper(ent_dt$name))] <- "HELIA ENVIRONMENTAL, LLC"
ent_dt$name[grepl('PEAK ECOLOG',toupper(ent_dt$name))] <- "PEAK ECOLOGICAL SERVICES, LLC"
ent_dt$name[grepl('ALPINE GEOPHYSICS',toupper(ent_dt$name))] <- "ALPINE GEOPHYSICS, LLC"
ent_dt$name[grepl("HAYDEN(\\s|-)WING",toupper(ent_dt$name))]<- "HAYDEN-WING ASSOCIATES, LLC"
ent_dt$name[grepl("WILD TROUT ENTERPRISES",toupper(ent_dt$name))] <- "WILD TROUT ENTERPRISES, LLC"
ent_dt$name[grepl("ZEIGLER GEOLOGIC",toupper(ent_dt$name))] <- "ZEIGLER GEOLOGIC CONSULTING, LLC"
ent_dt$name[grepl("NORTHERN LAND USE",toupper(ent_dt$name))] <- "NORTHERN LAND USE RESEARCH ALASKA, LLC"
ent_dt$name[grepl("LEGACY LAND",toupper(ent_dt$name))] <- "LEGACY LAND & ENVIRONMENTAL SOLUTIONS, LLC"
ent_dt$name[grepl("SOUTHWEST GEOPHY",toupper(ent_dt$name))] <- 'SOUTHWEST GEOPHYSICAL CONSULTING, LLC'
ent_dt$name[grepl("SYRACUSE ENVIRONMENTAL RESEARCH ASSOCIATES",toupper(ent_dt$name))] <- "SYRACUSE ENVIRONMENTAL RESEARCH ASSOCIATES"
ent_dt$name[grepl("ANDERSON PERRY (&|AND) ASSOCIATES",toupper(ent_dt$name))] <- "ANDERSON PERRY & ASSOCIATES, INC." 
ent_dt$name[grepl("VEATCH",toupper(ent_dt$name))] <- "BLACK & VEATCH" 


ent_dt$name[grepl("TETRA TECH",toupper(ent_dt$name))] <- "TETRA TECH INC." 
ent_dt$name[grepl("FLUOR",toupper(ent_dt$name))] <- "FLUOR CORP." 
ent_dt$name[grepl('\\bERM\\b',ent_dt$name)] <- "ERM" 
ent_dt$name[grepl('\\b(Kiewit|KIEWIT)\\b',ent_dt$name)] <- "KIEWIT CORP." 
ent_dt$name[grepl('\\b(ARCADIS|Arcadis)\\b',ent_dt$name)] <- "ARCADIS NV" 
ent_dt$name[grepl('\\bECC\\b',ent_dt$name)] <- "ECC" 
ent_dt$name[grepl('CDM.*SMITH',toupper(ent_dt$name))] <- "CDM SMITH" 
ent_dt$name[grepl('^Jacobs(\\W|$)|^JACOBS(\\W|$)',ent_dt$name)]<-'JACOBS ENGINEERING GROUP'

ent_dt$name[grepl('PARSONS BRINCKERHOFF|PARSONS BRINKERHOFF',toupper(ent_dt$name))] <- "WSP" 


ent_dt$name[grepl('LOUIS.*BERGER',toupper(ent_dt$name))] <- "LOUIS BERGER" 
#note this next example is why toupper is not made permanent -- megawatt hours are written as MWh
ent_dt$name[grepl('\\bMWH\b',ent_dt$name)] <- "MWH CONSTRUCTORS, INC." 
ent_dt$name[grepl("MOTT MACDONALD",toupper(ent_dt$name))] <- "MOTT MACDONALD" 

ent_dt$name[grepl("AECOM",toupper(ent_dt$name))] <- "AECOM" 
ent_dt$name[grepl("RAMBOLL",toupper(ent_dt$name))] <- "RAMBOLL"
ent_dt$name[grepl("BATTELLE",toupper(ent_dt$name))] <- "BATTELLE"
ent_dt$name[grepl("GOLDER ASSOCIATES",toupper(ent_dt$name))] <-"GOLDER ASSOCIATES CORP."
ent_dt$name[grepl('ICF',ent_dt$name)] <- 'ICF INTERNATIONAL'
ent_dt$name[grepl('LEIDOS',toupper(ent_dt$name))] <- 'LEIDOS INC.'

ent_dt$name[grepl('PARSONS TRANSPORTATION GROUP|PARSONS HBA|PARSONS HARLAND BARTHOLOMEW|PARSONS INFRASTRUCTURE AND TECHNOLOGY|PARSONS GOVERNMENT SERVICES',toupper(ent_dt$name))] <-'PARSONS CORPORATION'

ent_dt$name[grepl('^PARSONS',toupper(ent_dt$name))] <- 'PARSONS CORPORATION'


ent_dt$name[grepl('CARDNO',toupper(ent_dt$name))] <- 'CARDNO'


ent_dt$name[grepl('STANTEC',toupper(ent_dt$name))] <- 'STANTEC INC.'
ent_dt$name[grepl('PARAMETRIX',toupper(ent_dt$name))] <- 'PARAMETRIX INC.'
ent_dt$name[grepl('SWCA ENVIRONMENTAL',toupper(ent_dt$name))] <- 'SWCA ENVIRONMENTAL CONSULTANTS'
ent_dt$name[grepl('CH2M',toupper(ent_dt$name))] <- 'CH2M HILL'



ent_dt = ent_dt[nchar(ent_dt$name)>1,]
ent_dt = ent_dt[str_count(ent_dt$name,'[0-9]')<=2,]
ent_dt = ent_dt[!tolower(ent_dt$name) %in% stopwords::stopwords(),]
ent_dt = ent_dt[!name %in% person_dt$name,]
ent_dt = ent_dt[!grepl(paste(state.name,collapse = '|'),name,perl =T),]



ent_dt = ent_dt[!name%in%c('Notes','Material','Rock','SAND','Qualifications','Presence','Yes','Size','FINAL','Corridor','NF',"Tim Parsons","John Parsons",'Reclamation','Reference','C-17','Present','Mineral','RESOURCE','EIR',"NPS","INC","NP",'Sample','Bachelor','Sheet','Author','WATER','Tier','Mr.','Type','ASSOCIATES',"RMP","CEQA","USCS","USGS",'Page','Little',"PAGE","England",'Pond',"Wood",'Sam','SAM','Johnson','Fullerton','Payette'),]
ent_dt = ent_dt[!name%in%state.abb,]


firm_scrapes = readRDS('boilerplate_project/data_products/enr_firm_lists.RDS')
nms = unlist(firm_scrapes)
clean_names = unique(str_remove(nms,'\\,.+'))
clean_names = unique(str_remove(clean_names,'\\s\\(.+'))
clean_names = gsub('|','\\|',clean_names,fixed = T)
clean_names = gsub('&','\\&',clean_names,fixed = T)
clean_names = gsub('\\s{1,}',' ',clean_names)
clean_names = unique(clean_names)


person_dt = person_dt[!str_replace_all(name,'_',' ') %in% clean_names,]
saveRDS(person_dt,'boilerplate_project/data_products/person_entities_extracted.RDS')

uq_names = unique(ent_dt$name);length(uq_names)
mcores = 4
match1 = pblapply(clean_names,function(x) {grep(paste0('\\b',x,'\\b'),uq_names,perl = T)})
match2 = pblapply(tm::removePunctuation(clean_names),function(x) grep(paste0('\\b',x,'\\b'),uq_names,perl = T),cl = mcores)
match3 = pblapply(str_remove(clean_names,'\\s(Inc\\.$|LLC$|LLC\\.$|Corp\\.$)'),function(x) grep(paste0('\\b',x,'\\b'),uq_names,perl = T),cl = mcores)
match1A = pblapply(toupper(clean_names),function(x) {grep(paste0('\\b',x,'\\b'),toupper(uq_names),perl = T)},cl = mcores)
match2B = pblapply(tm::removePunctuation(toupper(clean_names)),function(x) grep(paste0('\\b',x,'\\b'),toupper(uq_names),perl = T),cl = mcores)
match3C = pblapply(str_remove(toupper(clean_names),'\\sINC\\.$|\\sLLC$|\\sLLC\\.$|\\sCORP\\.$'),function(x) grep(paste0('\\b',x,'\\b'),toupper(uq_names),perl = T),cl = mcores)

# # # # #

consult_matches = mapply(function(m1,m2,m3,m4,m5,m6) unique(c(m1,m2,m3,m4,m5,m6)),m1 = match1,m2 = match2,m3 = match3,m4 = match1A,m5 = match2B,m6 = match3C)

consult_dt = rbindlist(lapply(seq_along(clean_names),function(i) data.table(clean_names[i],uq_names[consult_matches[[i]]])),use.names = T,fill = T)
consult_dt <- consult_dt[!is.na(V2),]

orgs = ent_dt
orgs$NAME = toupper(orgs$name)
orgs$CONSULTANT <- consult_dt$V1[match(orgs$name,consult_dt$V2)]
other_consultants =  c("STRATIFIED ENVIRONMENTAL & ARCHAEOLOGICAL SERVICES, LLC",
                       "WALSH ENVIRONMENTAL SCIENTISTS AND ENGINEERS, LLC",
                       "CURRY & KERLINGER, LLC",
                       'SOLVE, LLC',
                       "GRETTE ASSOCIATES, LLC",
                       "HELIA ENVIRONMENTAL, LLC",
                       "PEAK ECOLOGICAL SERVICES, LLC",
                       "ALPINE GEOPHYSICS, LLC",
                       "HAYDEN-WING ASSOCIATES, LLC",
                       "WILD TROUT ENTERPRISES, LLC",
                       "ZEIGLER GEOLOGIC CONSULTING, LLC",
                       "NORTHERN LAND USE RESEARCH ALASKA, LLC",
                       "LEGACY LAND & ENVIRONMENTAL SOLUTIONS, LLC",
                       'SOUTHWEST GEOPHYSICAL CONSULTING, LLC',
                       "ALPINE ENVIRONMENTAL CONSULTANTS, LLC",
                       "SYRACUSE ENVIRONMENTAL RESEARCH ASSOCIATES",
                       "NUTTER AND ASSOCIATES",
                       "GARCIA AND ASSOCIATES","HORIZON ENVIRONMENTAL SERVICES, INC.",
                       "WESTERN ECOSYSTEMS TECHNOLOGY, INC.",
                       "ANDERSON PERRY & ASSOCIATES, INC." )

orgs$CONSULTANT[toupper(orgs$name) %in% other_consultants] <- toupper(orgs$name[toupper(orgs$name) %in% other_consultants])

orgs$CONSULTANT <- toupper(orgs$CONSULTANT)
orgs = orgs[!is.na(CONSULTANT),]
orgs = orgs[!CONSULTANT%in%c('PAYETTE','MCMAHON','HYDROGEOLOGIC INC.','WHITNEY','WOOD','SAM LLC','LITTLE','PAGE','CRAWFORD','NELSON','HERBERT','WHITMAN','Johnson','JOHNSON','POND','Whitman','BOWEN','STEWART','ENGLAND','HUNT','Crawford','Page','SAM LLC','Cathy Bechtel','Little','EXP'),]

handcoded_firms = orgs[NAME %in% other_consultants,.(PROJECT_ID,NAME)]
setnames(handcoded_firms,'NAME','FIRM')
autocoded_firms = orgs[grepl('CONSULTING|CONSULTANTS',NAME),.(PROJECT_ID,NAME)]
setnames(autocoded_firms,'NAME','FIRM')
detected_firms = orgs[!is.na(CONSULTANT),.(PROJECT_ID,CONSULTANT)]
setnames(detected_firms,'CONSULTANT','FIRM')

consults = rbindlist(list(detected_firms,handcoded_firms,autocoded_firms))
consults$FIRM <- toupper(consults$FIRM)
consults = consults[!duplicated(consults),]


saveRDS(consults,'boilerplate_project/data_products/consultant_project_matches.RDS')




