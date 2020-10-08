# projectLive
Track the impact of our funding partners in real time

## Shiny app configuration:
There are two editions of the front end:

https://shiny.synapse.org/users/jbanerjee/testprojectlive/ (pulling from the develop branch)
https://shiny.synapse.org/users/jbanerjee/projectLive/ (pulling from the master branch)

## Development pipeline:
1. Please branch off of develop branch
2. Reference the issue you are working on in the new branch name (eg. user/issueX)
3. Make your additions, test locally, and then file a PR against develop (no review required). 
4. Once the changes have been deployed in testprojectlive and tested, PR against master (1 review required). 
Note - as with all Shiny apps, touch restart.txt any time you make changes to make sure they are reflected in your testing.

## Setting up the app in Shiny server:
1. SSH into the Shiny server 
2. `cd` into `ShinyApps` dir 
3. Clone the code on this github branch (master):

`git clone --single-branch --branch master https://github.com/jaybee84/projectLive`

4. `cd` into the `projectLive` dir
5. `touch restart.txt`
6. Start R
7. Restore the development environment by typing in `renv::restore()`
8. Quit R and `touch restart.txt`
