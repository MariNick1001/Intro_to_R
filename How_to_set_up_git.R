#set up github version control (this should be run line by line, can be run from the command line)

install.packages("usethis")
install.packages("gitcreds")

#learned that when you use package:: you don't need to import via library(package), this is useful for quick command line stuff
usethis::use_git_config(user.name = "Your github username", user.email = "you@example.com")
usethis::create_github_token() #opens github to create a token

gitcreds::gitcreds_set() #prompts entering the created github token

#create a repository in github
#click code>https>copy link ("Clone using web URL")
#create a new project in RStudio (top left: file > new project)
# > select "version control"
# > enter the copied link and a directory for local saving

# Now you are set up!

# (git panel is in the top right, next to environment/history/connections)
# stage and commit (with message) after changes


