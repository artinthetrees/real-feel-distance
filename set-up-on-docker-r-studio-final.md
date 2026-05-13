1. make your project folder into an R project - this will add an Rproj file to your project folder
2. renv/renv.lock should be in your project folder
3. map your project folder to home/rstudio/project in container
4. to start container
    ```docker compose build```
    ```docker compose up```
5. go to localhost 8787, login to rstudio, go to open project and open your Rproj file in your project folder - this will automatically set working directory to your project folder and will then recognize your renv.lock file

**NOTE**: check this link - https://rstudio.github.io/renv/articles/docker.html