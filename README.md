# *HSCP Portal - Unscheduled Care*
*The aim of this project is to provide a user friendly app that allows HSCP based analysis for unscheduled care services. The RShiny web app will serve as a portal where users will choose their desired HSCP and be able to view a number of indicators and customise what they want to see.*

### Directories
  * `data` - data required for project
  * `www` - Images, videos, HTML, javascript
  
### Code scripts
  * `app` - if your app is small use this file which contains all parts of an app.
  * `ui` - user interface: layout and elements the user interacts with.
  * `server` - code that produces the outputs shown in the ui.
  * `global` - non-reactive elements used by both server and ui: packages, functions, data, etc. 
  * `rsessioninfo` - includes the versions of your packages, of R and your environment settings.
  
### Others
  * `gitignore` -what files GIT will ignore and not track nor upload.
  * `r-project` - R project.
  * `Rprofile` - R profile settings.
  
## Notes

If you are using git for version control then please be aware that the .gitignore contains the minimum recommended file types and folders to stop data being tracked and pushed to GitHub. Further guidance on using git and GitHub securely can be found [here](https://github.com/NHS-NSS-transforming-publications/GitHub-guidance).
