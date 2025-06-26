[![DOI](https://zenodo.org/badge/984969476.svg)](https://doi.org/10.5281/zenodo.15743783)

This repository contains code and data for analyzing discrepancies between GPS-tracked bird data and AIS-registered fishing activity for the detection of potential unreported fishing activity.

## License

This repository is licensed under the [Creative Commons Attribution-NonCommercial 4.0 International License](https://creativecommons.org/licenses/by-nc/4.0/).

[![License: CC BY-NC 4.0](https://licensebuttons.net/l/by-nc/4.0/88x31.png)](https://creativecommons.org/licenses/by-nc/4.0/)

## Data Availability

The Fishing_Tracks data folder is not publicly available in this repository due to licensing or sensitivity concerns.
However, the data can be shared for academic or research purposes upon request.  

## Folders

- **`behavioral_plots/`**  
  Contains plots using the `bird_boat_plots.R` script.

- **`bird_behavior_result/`**  
  Output folder containing the results from the bird behavior classification tool using model classified bird events.
  Generated using the [Bird Behavior Tool](https://github.com/fkariminejadasl/bird-behavior/blob/main/notebooks/classify_birds.ipynb) developed by Karimi Nejadasl.  
  Please cite their work if using this tool or its outputs.

- **`bird_behavior_result_hand_picked/`**  
  Output folder containing the results from the bird behavior classification tool using hand picked bird events.
  Also generated with the [Bird Behavior Tool](https://github.com/fkariminejadasl/bird-behavior/blob/main/notebooks/classify_birds.ipynb) developed by Karimi Nejadasl.  
  Please cite their work if using this tool or its outputs.

## Files

- **`.gitignore`**  
  Specifies files and folders to exclude from version control.

- **`LICENSE`**  
  Creative Commons Attribution-NonCommercial 4.0 International license covering this repository.

- **`bird_boat_plots.R`**  
  R script for visualizing interactions between bird and vessel events.

- **`bird_events.csv`**  
  A CSV file containing model classified bird boat-following behavior events.  
  Originally provided by de Jonghe (2024), from the study: *"Potential of seabirds as animal sentinels for illegal fishing detection; a Lesser Black-backed Gull study"*.  
  Please cite the original research if you use this data.

- **`bird_events_hand_picked.xlsx`**  
  A CSV file containing manually picked bird boat-following behavior events.  
  Originally provided by de Jonghe (2024), from the study: *"Potential of seabirds as animal sentinels for illegal fishing detection; a Lesser Black-backed Gull study"*.  
  Please cite the original research if you use this data.

- **`input.csv`**  
  Input file used for online bird behavior tool.  
  Generated using the [Bird Behavior Tool](https://github.com/fkariminejadasl/bird-behavior/blob/main/notebooks/classify_birds.ipynb) developed by Karimi Nejadasl.  
  Please cite their work if using this tool or its outputs.

- **`overlap_analysis.R`**  
  R script performing analysis to detect spatial-temporal overlap between model-classified bird events and fishing vessel activity.

- **`overlap_analysis_hand_picked.R`**  
  A variant of the overlap analysis script, specifically using the hand-picked bird event data.

## References

1. Karimi Nejadasl, F. (2023). Bird behavior classification tool [Jupyter Notebook]. GitHub. https://github.com/fkariminejadasl/bird-behavior/blob/main/notebooks/classify_birds.ipynb 
2. de Jonghe, E. (2024). Potential of seabirds as animal sentinels for illegal fishing detection: A Lesser Black-backed Gull study [Unpublished master's thesis]. University of Amsterdam.
