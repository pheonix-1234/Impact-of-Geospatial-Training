# Impact of Geospatial Training on Sustainability Competencies
This repository provides the R-scripts used to organize the data and produce the results for the Impact of Geospatial Training on Sustainability Competencies.

<br/>

## Data Availability
The raw data supporting this study cannot be publicly shared due to human research ethics requirements and confidentiality agreements with participants. Human Research Ethics (protocol number: 47199) was approved by the University of Toronto Research Ethics Boards on 09/20/2024. De-identified data may be available from the corresponding author upon reasonable request and subject to institutional ethics approval.

<br/>

## Usage
Those interested in using the code from this repository may either obtain de-identified data, as specified above, or can utilize thier own dataset. Regardless, code modifications can be made wherever necessary or applicable to run the code accordingly. 

<br/>

## Getting Started
### Clone the repository
```bash
   git clone -b main https://github.com/pheonix-1234/Impact-of-Geospatial-Training.git
   ```
<br/>

### Ensuring data for running the code
If you use raw pre- and post-data csv files and want to use data_filter.R, ensure that raw data is found in an "Original" folder, and the resulting filtered data is saved in a "Filtered" folder to keep things organized. 

In the "Original" Folder, ensure that it contains the "Manual Processing" folder in this repository as a subfolder. If you prefer to store these csv files that organize the columns else where, ensure to make the proper changes in the "read.csv" lines of the R-script. If your raw data has different column names, you can edit the column names in "pre_rename_columns.csv" and "pre_columns_to_process.csv".

Filtered datasets are used for the remaining R-Scripts, including a manually-created csv files with paritipant's evaluated score on an objectively graded question. 

<br/>

*Note: The "Major" column can be also overwritten at your discretion if raw entries can be further catogrized (e.g. "Commerce" and "Business" can be overwritten as "Finance").*

*Note: Participants that have multiple programs of studies (ex. Finance Major, Computer Science Minor) can have thier second program in "Major2" in your filtered dataset/frame.*

<br/>

### Install the necessary packages
```bash
   install.packages(c("dplyr","tidyverse","stringr","openxlsx","purrr","effectsize","effsize","tidyr","psych"))
   ```
You can also install other packages as you run and modify the code.

<br/>

### Ensuring folder for storing results
The code is such that results will be stored in a "Results" folder. Please ensure it also exists while you run the each R-scripts.

<br/>

## Running the Code
Once you have followed the instruction in the "Getting Started" section, run and modify the code at your discretion. Results and statistical analyses will depend on the datasets used.

<br/>

## Acknowledgement
This work is supported by the Pedagogical Research Fund at the University of Toronto Mississauga. We sincerely thank research assistants Peter Haoxuan Ge for selecting and modifying the training, Meggie Chan for drafting the initial survey instrument, and Rosalinda Hefan Zhang for collaborative thematic coding with the first author. We highly appreciate Professor Joseph Leydon’s feedback throughout the different phases of the study.
