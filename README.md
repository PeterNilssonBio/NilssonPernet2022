# Analysis of food-conditioned phenotypic plasticity in echinoid larvae

## Introduction
This code was used to analyze data from a series of experiments on the expression of phenotypic plasticity in the larvae of two echinoid species. These experiments were part of a study that is described in Nilsson & Pernet (2022).

This code can be used to generate the figures and tables that appear in the article (less a few formatting details).

To see the most recent version - which may be updated to reflect URLs not known at the time this document was written - visit the [github page](https://github.com/PeterNilssonBio/NilssonPernet2022).

## Setup
To run this script, you need...
1. The [R language](https://www.r-project.org/) (version ≥ 4.1), along with the following R packages and their dependencies:
  [config](https://cran.r-project.org/web/packages/config/index.html),
  [emmeans](https://cran.r-project.org/web/packages/emmeans/index.html),
  [ggpubr](https://cran.r-project.org/web/packages/ggpubr/index.html),
  [kableExtra](https://cran.r-project.org/web/packages/kableExtra/index.html),
  [lmerTest](https://cran.r-project.org/web/packages/lmerTest/index.html),
  [multcomp](https://cran.r-project.org/web/packages/multcomp/index.html),
  [reticulate](https://cran.r-project.org/web/packages/reticulate/index.html),
  [rstatix](https://cran.r-project.org/web/packages/rstatix/index.html), and
  [tidyverse](https://cran.r-project.org/web/packages/tidyverse/index.html).

  2. [Python](https://www.python.org/downloads/) (version ≥ 3.8), along with the [Beautiful Soup 4](https://pypi.org/project/beautifulsoup4/) package.

  3. The two data files, which are stored in the Biological & Chemical Oceanography Data Management Office database as part of an NSF-funded [project](https://www.bco-dmo.org/project/727167). At the time of writing, the URLs at which this data will be located are not known, but the script will attempt to download them automatically. Should that fail, simply obtain the data files and place them in a subdirectory named `data` like this:
        ```
        NilssonPernet2022/
        ├── data/
        │   ├── density_morphometrics.csv <- Add this!
        │   └── density_counts.csv        <- Add this too!
        ├── main.R
        └── (various other files and directories...)
        ```

If you find some dependencies are missing, you can try running the `install_dependencies()` function in `code/setup.R`, which will attempt to download and install everything for you. I haven't really tested this; it is just for the convenience of anyone trying to run this script.

## Usage
Once everything is set up, simply source the file `main.R` in the R interpreter:

```r
source("main.R")
```

This should generate HTML output files for each of seven tables (Tables 1-2, S1-S5) and SVG output files for each of five figures (Figures 2-5, S1). It will also generate an HTML output file containing a small table of data on count control larvae, which is summarized in prose in the paper. You can customize the output by editing the `config.yml` file. Note that there is no output for Figure 1 because that figure is simply an illustration and is not generated from the data.

Output files are put in a directory named `output`:
```
NilssonPernet2022/
├── output/
│   ├── Fig_2.svg
│   ├── Fig_3.svg
│   └── (various other output files...)
├── main.R
└── (various other files and directories...)
```

## References
Nilsson P, Pernet B (2022) Echinoid larvae can express food-conditioned morphological plasticity at ecologically relevant culture densities. Marine Ecology Progress Series 694 (in press).