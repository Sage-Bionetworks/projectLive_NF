# projectLive_NF
Track the impact of our funding partners in real time

## Shiny app configuration:
There are two editions of the front end:

https://sagebio.shinyapps.io/projectLive_NF/ (pulling from the master branch)
https://sagebio.shinyapps.io/projectLive_NF-staging/ (pulling from the develop branch) [work in progress]

## Development pipeline:
1. Please branch off of develop branch
2. Reference the issue you are working on in the new branch name (eg. user/issueX)
3. Make your additions, test locally, and then file a PR against develop (no review required). 
4. Once the changes have been deployed in the staging app and tested, PR against master (1 review required). 
Note - as with all Shiny apps, touch restart.txt any time you make changes to make sure they are reflected in your testing.


## Setting up the app locally:
 
1. Clone the code on this github branch (develop):

`git clone --single-branch --branch develop https://github.com/SageBionetworks/projectLive_NF`

2. `cd` into the `projectLive_NF` dir
3. Start RStudio
4. Restore the development environment using `renv::restore()`

## Getting the virtual environment working locally
1. cd into the app directory
2. `python3 -m venv virtual_env`
3. `chmod 755 virtual_env/bin/activate`
4. `source virtual_env/bin/activate`
5. `pip3 install synapseclient`