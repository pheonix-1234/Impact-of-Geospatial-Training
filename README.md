# Impact of Geospatial Training on Sustainability Competencies
This repository provides the R-scripts used to organize the data and produce the results for the Impact of Geospatial Training on Sustainability Competencies.

## Data Availability
The raw data supporting this study cannot be publicly shared due to human research ethics requirements and confidentiality agreements with participants. Human Research Ethics (protocol number: 47199) was approved by the University of Toronto Research Ethics Boards on 09/20/2024. De-identified data may be available from the corresponding author upon reasonable request and subject to institutional ethics approval.

## Usage
Those interested in using the code from this repository may either obtain de-identified data as specified above, or can utilize thier own dataset. Regardless, code modifications can be made where necessary or applicable. 

## Getting Started
### Clone the repository
```bash
   git clone -b main https://github.com/pheonix-1234/Impact-of-Geospatial-Training.git
   ```

### Ensuring data for running the code
If dealing with raw pre- and post-data, ensure it is found in the "Original" folder, and the filtered data from the data_filter.R is saved in "Filtered" folder. Please note that the "Major" column can be over written by the user's discretion if raw entries can be catogrized (e.g. "Commerce" and "Business" can be overwritten as "Finance"). The filtered datasets are used for the remaining R Scripts in addition to the manually evaluated score. 

### Install the necessary packages
```bash
   install.packages(c("dplyr","tidyverse","stringr","openxlsx","purrr","effectsize","effsize","tidyr","psych"))
   ```
You can also install other packages as you run and modify the code.

### Ensuring folder for storing results
The code is such that results will be stored in the "Results" folder. It is in this repository and ensure it also exists while you run the each R-scripts.

## Running the Code
Once you have followed the instruction in the "Gettign Started" section, run and modify the code at your discretion. Results and statistical analyses will depend on the datasets used.

## Acknowledgement
This work is supported by the Pedagogical Research Fund at the University of Toronto Mississauga. We sincerely thank research assistants Peter Haoxuan Ge for selecting and modifying the training, Meggie Chan for drafting the initial survey instrument, and Rosalinda Hefan Zhang for collaborative thematic coding with the first author. We highly appreciate Professor Joseph Leydon’s feedback throughout the different phases of the study.
