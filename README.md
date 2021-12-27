# Repository for "A rodent paradigm for studying perceptual decisions under asymmetric reward"
Written and maintained by Xiaoyue Zhu

### What is included
- `csv/` contains the animal behavioral data, and various csv files pre-computed for plotting 
- `fits/` contains the stan fit objects using the mixture-BDT model
- `plots/` is currently empty, any outputs of the plotting functions will be stored here
- `figure_x` contains the plotting functions relevant for making the specific plots

### How to use the plotting functions
1. Set working directory to where the repo is stored locally `setwd('xx/perceptual-gambling/')`
2. Source `init.R`, make sure you have the relevant packages installed 
3. Simply go to each `figure_x.R` and run the `make_figure_x` function
