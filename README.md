# Pain threshold

## Bibliometric information
(to be provided later)

## Abstract
(to be added later)

## Analysis outputs

**The data required to run the scripts have not been included in the repo because study participants did not consent to public release of their data. However, the data are available on request from Tory Madden (torymadden@gmail.com) or Peter Kamerman (peter.kamerman@gmail.com), or by submitting an [issue](https://github.com/kamermanpr/SPARS/issues).**

The outputs from all analysis scripts are located in the _/outputs_ directory. The outputs are formatted as markdown and html. The markdown documents are intermediate outputs generated during the production of the html documents, and while they allow quick browsing of the analysis outputs on GitHub, MathJax formulae and tables are not formatted. 

## Run the analysis scripts

For reproducibility, we have created a [_Docker_](https://www.docker.com) image with the _R_ environment required to run the _pain\_threshold_ data analysis scripts. The image is built using the [_rocker/verse_](https://hub.docker.com/r/rocker/verse/) image of [_base R_](https://cran.r-project.org/) _v3.5.1_, and includes [_RStudio server_](https://www.rstudio.com/products/rstudio/#Server), the [_TinyTex_](https://yihui.name/tinytex/) Latex distribution, the [_tidyverse_](https://www.tidyverse.org/) suite of R packages (with dependencies), and several R packages (with dependencies) that are required to run the markdown scripts in [_SPARS_](https://github.com/kamermanpr/SPARS). CRAN packages were installed from [_MRAN_](https://mran.microsoft.com/timemachine) using the 2018-10-03 snapshot for _R v3.5.1_. The only package installed from GitHub (_thomasp85/patchwork_) was locked to the 22 September 2018 commit: [_fd7958bae3e7a1e30237c751952e412a0a1d1242_](https://github.com/thomasp85/patchwork/tree/fd7958bae3e7a1e30237c751952e412a0a1d1242).

### Details
- **OS:**  
    - Debian:stretch  
- **R:**  
    - v3.5.1   
- **RStudio server:**  
    - v1.1.456
- **GitHub packages:**  
    - patchwork  
- **MRAN packages:**  
    - boot  
    - car  
    - kableExtra
    - ggplot2
    - ggridges
    - ggeffects
    - HLMdiag
    - influence.ME
    - lme4
    - lmerTest
    - lqmm
    - robustlmm
    - sjPlot
    - tidyverse 
- **LaTex:**   
    - TinyTex

### Using Docker to run the SPARS analysis

**These instructions are for running the analysis on your local machine.**

You need to have _Docker_ installed on your computer. To do so, go to [_docker.com_](https://www.docker.com/community-edition#/download) and follow the instructions for downloading and installing Docker for your operating system. Once _Docker_ has been installed, follow the steps below, noting that _Docker_ commands are entered in a terminal window (_Linux_ and _OSX/macOS_) or command prompt window (_Windows_). _Windows_ users also may wish to install [_GNU Make_](http://gnuwin32.sourceforge.net/downlinks/make.php) (required for the `make` method of running the scripts) and [_Git_](https://gitforwindows.org/) version control software (not essential). 

#### Download the latest image

Enter: `docker pull kamermanpr/docker-pain-threshold:v1.1.2`

#### Download the repository

Download the compressed _zip_ file from _GitHub_ ([_kamermanpr/SPARS_](https://github.com/kamermanpr/pain_threshold), or from _figshare_ ([DOI: 10.6084/m9.figshare.6561743](https://doi.org/10.6084/m9.figshare.6561743)). 

#### Run the container

Enter: `docker run --name threshold -d -p 8787:8787 -e USER=user -e PASSWORD=password kamermanpr/docker-pain-threshold:v1.1.2`

#### Login to RStudio Server

- Open a web browser window and navigate to: `localhost:8787`

- Use the following login credentials: 
    - Username: _user_	
    - Password: _password_
    
#### Prepare the SPARS directory

On the **Files** tab in the bottom right panel of _RStudio_, click on the **'Upload'** button, navigate to the downloaded _zip_ file, and upload the file (it will self extract).

The _pain\_threshold_ directory comes with the outputs for all the analysis scripts in the _/outputs_ directory (_html_ and _md_ formats). However, should you wish to run the scripts yourself, there are several preparatory steps that are required:  

1. Acquire the data. The data required to run the scripts have not been included in the repo because participants in the studies did not consent to public release of their data. However, the data are available on request from Tory Madden (torymadden@gmail.com) or Peter Kamerman (peter.kamerman@gmail.com). We will send you a _zip_ file with the data.

    Using the directory tree in the **Files** tab of _RStudio_, open the _SPARS_ directory. Repeat the upload procedure described above, but upload the zipped data file we supplied you with into the _SPARS_ directory. 

2. In the _pain\_threshold_ directory, double-click on the _pain\_threshold.Rproj_ file, and follow the prompts (_RStudio_ will reload).

3. Clean the _/outputs_ and _/figures_ directories by entering `make clean` in the **Terminal** tab in bottom right panel of _RStudio_.

#### Run the SPARS analysis scripts

To run all the scripts (including the data cleaning scripts), enter `make` in the **Terminal** tab. 

To run individual RMarkdown scripts (_\*.Rmd_ files)

1. Generate the cleaned data using one of the following methods:  
    - Enter `make data-cleaned/SPARS_A.rds` and then `make data-cleaned/SPARS_B.rds` in the **Terminal** tab;  
    - Enter `source('clean-SPARSA.R')` and then `source('clean-SPARSB.R')` in the **Console** tab in bottom left panel of _RStudio_.  
    - Open _clean-SPARSA.R_ and _clean-SPARSB.R_ scripts through the **File** tab, and then click the **'Source'** button on top of the panel on the top left of _RStudio_ for each script.  
    
2. Run the individual script using one of the following methods:  
    - Enter `make outputs/<NAME_OF_INPUT_FILE>.html` in the **Terminal** tab;  
    - Open the relevant _\*.Rmd_ file through the **File** tab, and then click the **'knit'** button on the top of the panel on the top left of _RStudio_.   

#### Shutting down

Once done, log out of _RStudio_ and enter the following into a terminal to stop the _Docker_ container: `docker stop spars`. If you then want to remove the container, enter: `docker rm threshold`. If you also want to remove the _Docker_ image you downloaded, enter: `docker rmi kamermanpr/docker-pain-threshold:v1.1.2`
