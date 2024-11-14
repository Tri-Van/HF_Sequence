# Readme:

Purpose: Project to test and analyze Heart Failure Sequence strategies

Outlines:

1.  Data folder - Contain raw data

2.  Scripts folder - Script to generate plot

3.  Output folder - Saved figures

File runs:

1.  1 Year analysis figures

-   (Importance!) Go to Data folder:
    -   Update input data: V7_1Yr_HFH.xlsx & V7_1Yr_Death.xlsx
    -   For Both files
        -   Sheet 2 (Death or HFH): Individual simulation data for 10,000 patients over 1 year across 12 strategies -\> Start here & add data for 4 new strateiges
        -   Sheet 1: Combine time till events of interest (Death or HFH) across 12 strategies (2 New Profiles -\> increases to 16 strategies)
            -   Death: Simply duplicate the Strategies name in the Profile columns (x10,000 times), then copy & paste value from sheet "Death" in the next corresponding 2 columns
            -   HFH: Read textbox
-   (Optional) Run 1Year_analysis.Rproj
-   Go to script folder
    -   Run **1Yr_analysis.R**: code (starting from line 130) to generate Figure 3A,3B,3C,3D
        -   Use Export option to manually export these are PNG, saved them in the Outputs/1_year folder
    -   Run **Create_figures.R**: code to combine 4 Figure 3A,3B,3C,3D into 1 figure
