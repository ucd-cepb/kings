##### github is not good for large file storage, so this project uses a combination of Box Drive and github. 
#Just about everything is in github, except for the /data subdirectory
## this subdirectory can be accessed in many ways, but to make the code run as-is, use a symbolic link:
## ln -s ~/Library/CloudStorage/Box-Box/Kings_Large_Files/data .
## (link) (symbolic) (location of folder in Box drive) (location of folder in repository)
system("ln -s ~/Library/CloudStorage/Box-Box/Kings_Large_Files/data .")

