# Pain threshold

## Bibliometric information
Madden VJ, Kamerman PR, Catley MJ, Bellan V, Russek LN, Camfferman D, Lorimer Moseley G. Rethinking pain threshold as a zone of uncertainty. _bioRxiv_ 2019:521302. DOI: [10.1101/521302](https://doi.org/10.1101/521302).

## Abstract
**Background** The pain threshold is traditionally conceptualised as a boundary that lies between painful and non-painful events, suggesting a reasonably stable relationship between stimulus and response. In two previous experiments, participants received laser stimuli of various intensities and rated each stimulus on the Sensation and Pain Rating Scale (SPARS), which includes ranges for rating painful and non-painful events and clearly defines the presumed boundary between them. In the second experiment, participants also provided ratings on the conventional 0-100 Numerical Rating Scale for pain (NRS) and a new rating scale for non-painful events. Those data showed the SPARS to have a curvilinear stimulus-response relationship, reflecting that several different intensities may be rated as painful and non-painful in different trials. This suggests that participants were uncertain about painfulness over a range of intensities and calls into question the idea of a boundary between non-painful and painful events. The current study aimed to determine the number of different stimulus intensities across which each participant provided ‘painful’ and ‘non-painful’ reports in different trials.

**Methods** We undertook novel exploratory analyses on data from the aforementioned two experiments (n = 19, 11 female, 18-31 years old; n = 7, 5 female, 21-30 years old). We used the binomial test to formally determine the width of this ‘zone of uncertainty’ about painfulness, using ratings on the SPARS and the comparator scales, and data visualisation to assess whether trial-to-trial change in stimulus intensity influences ratings.

**Results** We found that the width of the zone of uncertainty varied notably between individuals and that the zone was non-continuous for most participants. Plots of group-level data concealed the inter-individual variability apparent in the individual plots, but still showed a wide zone of uncertainty on both the SPARS and the NRS, but a narrow zone on the scale for non-painful events. There was no evidence that trial-to-trial change in stimulus intensity influenced ratings.

**Conclusions** The variability revealed by this study has important design implications for experiments that include initial calibration of repeatedly delivered stimuli. The variability also stands to inflate the size of sample that is required for adequate statistical powering of experiments, and provides rationale for the use of statistical approaches that account for individual variability in studies of pain. Finally, the high variability implies that, if experimental stimuli are to be used in clinical phenotyping, many trials may be required to obtain results that represent a single patient’s actual response profile.

## Analysis outputs

**The data required to run the scripts have not been included in the repo because study participants did not consent to public release of their data. However, the data are available on request from Tory Madden (torymadden@gmail.com) or Peter Kamerman (peter.kamerman@gmail.com), or by submitting an [issue](https://github.com/kamermanpr/pain-threshold/issues).**

The outputs from all analysis scripts are located in the _/outputs_ directory. The outputs are formatted as markdown and html. The markdown documents are intermediate outputs generated during the production of the html documents, and while they allow quick browsing of the analysis outputs on GitHub, MathJax formulae and tables are not formatted. 

## Run the analysis scripts

For reproducibility, we have created a [_Docker_](https://www.docker.com) image with the _R_ environment required to run the _pain-threshold_ data analysis scripts. The image is built using the [_rocker/verse_](https://hub.docker.com/r/rocker/verse/) image of [_base R_](https://cran.r-project.org/) _v3.5.1_, and includes [_RStudio server_](https://www.rstudio.com/products/rstudio/#Server), the [_TinyTex_](https://yihui.name/tinytex/) Latex distribution, the [_tidyverse_](https://www.tidyverse.org/) suite of R packages (with dependencies), and several R packages (with dependencies) that are required to run the markdown scripts in [_pain-threshold_](https://github.com/kamermanpr/pain-threshold). CRAN packages were installed from [_MRAN_](https://mran.microsoft.com/timemachine) using the 2019-01-12 snapshot for _R v3.5.1_.

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
    - knitr
    - magrittr
    - readxl
    - skimr
- **LaTex:**   
    - TinyTex

### Using Docker to run the pain-threshold analysis

**These instructions are for running the analysis on your local machine.**

You need to have _Docker_ installed on your computer. To do so, go to [_docker.com_](https://www.docker.com/community-edition#/download) and follow the instructions for downloading and installing Docker for your operating system. Once _Docker_ has been installed, follow the steps below, noting that _Docker_ commands are entered in a terminal window (_Linux_ and _OSX/macOS_) or command prompt window (_Windows_). _Windows_ users also may wish to install [_GNU Make_](http://gnuwin32.sourceforge.net/downlinks/make.php) (required for the `make` method of running the scripts) and [_Git_](https://gitforwindows.org/) version control software (not essential). 

#### Download the latest image

Enter: `docker pull kamermanpr/docker-pain-threshold:v1.09`

#### Download the repository

Download the compressed _zip_ file from _GitHub_ ([_kamermanpr/pain-thershold_](https://github.com/kamermanpr/pain-threshold), or from _figshare_ ([DOI: TO BE CONFIRMED](https://doi.org/)). 

#### Run the container

Enter: `docker run --name threshold -d -p 8787:8787 -e USER=user -e PASSWORD=password kamermanpr/docker-pain-threshold:v1.0.9`

#### Login to RStudio Server

- Open a web browser window and navigate to: `localhost:8787`

- Use the following login credentials: 
    - Username: _user_	
    - Password: _password_
    
#### Prepare the pain-threshold directory

On the **Files** tab in the bottom right panel of _RStudio_, click on the **'Upload'** button, navigate to the downloaded _zip_ file, and upload the file (it will self extract).

The _pain-threshold_ directory comes with the outputs for all the analysis scripts in the _/outputs_ directory (_html_ and _md_ formats). However, should you wish to run the scripts yourself, there are several preparatory steps that are required:  

1. Acquire the data. The data required to run the scripts have not been included in the repo because participants in the studies did not consent to public release of their data. However, the data are available on request from Tory Madden (torymadden@gmail.com) or Peter Kamerman (peter.kamerman@gmail.com). We will send you a _zip_ file with the data.

    Using the directory tree in the **Files** tab of _RStudio_, open the _pain-threshold_ directory. Repeat the upload procedure described above, but upload the zipped data file we supplied you with into the _pain-thershold_ directory. 

2. In the _pain-threshold_ directory, double-click on the _pain-threshold.Rproj_ file, and follow the prompts (_RStudio_ will reload).

3. Clean the _/outputs_ and _/figures_ directories by entering `make clean` in the **Terminal** tab in bottom right panel of _RStudio_.

#### Run the pain-threshold analysis scripts

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

Once done, log out of _RStudio_ and enter the following into a terminal to stop the _Docker_ container: `docker stop threshold`. If you then want to remove the container, enter: `docker rm threshold`. If you also want to remove the _Docker_ image you downloaded, enter: `docker rmi kamermanpr/docker-pain-threshold:v1.0.9`
