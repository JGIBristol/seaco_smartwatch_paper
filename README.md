# seaco_smartwatch_paper
Malaysia SEACO-CH20 Smartwatch Feasibility Study

## Data
First thing to do is to get access to the SEACO RDSF - ask the PI for this.
Then you'll need to mount it somewhere - the mount location that I used is in `userconf.yml`.

## Environment
Run `conda create -f environment.yml` to create an environment that contains all the required packages,
including R, python and the required libraries.

## Notebooks
Approximately correspond to the order of things in the paper:

 - [demographic_summary.ipynb](demographic_summary.ipynb): stats on the participant demographics
 - [survey.ipynb](survey.ipynb): quantitative results from the survey
 - [meal_stats.ipynb](meal_stats.ipynb): statistics on the numbers of meals, snacks, etc. per day
 - [three_level_model.ipynb](three_level_model.ipynb): linear models for response rate as the study progesses

### Linear Models
If you're just interested in the linear model: [binomial_models.R](analysis_utils/r/binomial_models.R).  
The notebook at binomial_models.ipynb runs this R script.

There are also additional notebooks and R scripts in `old_stuff/`, but these are either outdated or irrelevant for the paper.
I've kept them here just in case...

## Config files
There are two config files:
- `config.yml` - containing configuration (filepaths etc.) that you won't need to change
- `userconf.yml` - containing configuration that you might need to change, depending on e.g. where you have mounted the SEACO CH-20 RDSF