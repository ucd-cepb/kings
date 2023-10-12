#compile list of EJ orgs and their scope
#it will play best with the existing code if you use these column names:
#"Agency" column is the full name of the org
#"Abbr" column is a nickname used for the org
#"State" column isn't used by the textnet package, but you & the team can decide if you'd like to use it in the analysis.
#"State" is the scope of the agency, such as "federal" or "California" or "Local."
#For the orgs here you could put "Local" or "NGO" or whatever you think a good category is, or leave it blank)

#Then replace spaces with underlines in each column like this
ej_orgs$Abbr <- str_replace_all(ej_orgs$Abbr,"-|\\s","_")
ej_orgs$Agency <- str_replace_all(ej_orgs$Agency,"-|\\s","_")
ej_orgs$State <- str_replace_all(ej_orgs$State,"-|\\s","_")