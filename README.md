# Chemical Agents for Occupational Exposure Databases


## Purpose

The development of this package is part of a research study led by **Jérôme Lavoué** (Professor at [University of Montreal](https://recherche.umontreal.ca/english/home/), department of environmental health and occupational health) whose title is "Portrait of multiexposure situations in the workplace in Quebec from occupational exposure databases". The essence of this study is to explore several occupational exposure databases to identify and describe patterns of co-occurrence of chemicals as defined, for example, by the presence of measurements showing detected concentrations for the same occupation within the same company. The work performed led to the creation of several R packages, including [uom.dat](https://github.com/UofMontreal-Multiexpo/uom.dat) (generic analysis tools), `uom.agents` (chemical agent data) and [uom.usis](https://github.com/UofMontreal-Multiexpo/uom.usis) (occupational exposure data).

The databases considered in this study comprise measurement data for multiple chemicals from the United States[^1], from France[^2], and from Canada[^3][^4]. The study involved merging extracts across the various databases and thus standardizing the identification of chemicals, sometimes creating groups of chemicals, selecting exposure limits as well as standardized toxicological effect categories.

This **R package** represents the results of these efforts of standardization and information gathering about **chemical agents**.

[^1]: Lavoué, J., Burstyn, I., Friesen, M. (2012). Workplace Measurements by the US Occupational Safety and Health Administration since 1979: Descriptive Analysis and Potential Uses for Exposure Assessment. *Annals of Occupational Hygiene, 57*(1), 77-97. PMID: 22952385.
[^2]: Mater, G., Paris, C., Lavoué, J. (2016). Descriptive analysis and comparison of two French occupational exposure databases: COLCHIC and SCOLA. *American Journal of Industrial Medicine, 59*(5), 379-391. PMID: 26901238.
[^3]: Hall, A. L., Peters, C. E., Demers, P. A., & Davies, H. W. (2014). Exposed! Or not? The diminishing record of workplace exposure in Canada. *Canadian Journal of Public Health = Revue Canadienne de Santé Publique, 105*(3), e214-7.
[^4]: Sarazin, P., Labrèche, F., Lesage, J., & Lavoué, J. (2018). *Étude comparative des banques de données d'exposition IMIS (OSHA) et LIMS (IRSST) (Rapport R-1032)*. Montreal, QC: Institut de recherche Robert-Sauvé en santé et en sécurité du travail.


## Data

Chemical agent data: classifications according to several occupational exposure databases, exposure limit values, toxicological classes... Raw data and CSV format of final datasets are available in this [Dropbox folder](https://www.dropbox.com/scl/fo/0k1d84kf005qc6n7xgimh/AEbgRasCpHqdsSXvRCzV9e4?rlkey=8wl7i5yu8zfqgbrpmuybt7h5j&st=8x3td6md&dl=0).


## Installation

To install the latest version, run the following instruction.
```r
remotes::install_github("UofMontreal-Multiexpo/uom.agents")
```

To install the development version, use:
```r
remotes::install_github("UofMontreal-Multiexpo/uom.agents",
                        ref = "develop")
```

To install a previous version, run the following instruction, replacing `X.X.X-X` with the desired version number.
```r
remotes::install_github("UofMontreal-Multiexpo/uom.agents",
                        ref = "vX.X.X-X")
```


## Documentation

The package main page can be accessed using:
```r
help("uom.agents")
```

In addition to the manuals of the package, data and functions accessible by the `help` function, the `inst/doc` directory contains:

* An organized list of the datasets and functions, in the file `list_of_help_pages.html`.
* A description of the chemical agent data, in the file `chemical_agents.pdf`.

These files can be accessed using `help(package = "uom.agents")` then clicking on "User guides, package vignettes and other documentation".



## Contact

For any inquiries, you can send an email to Jérôme Lavoué at <jerome.lavoue@umontreal.ca>.


## Authors

* [Gauthier Magnin](https://fr.linkedin.com/in/gauthier-magnin) - R programmer analyst.


## Collaboration

* [IRSST](https://www.irsst.qc.ca/en/): The Quebec's Occupational Health and Safety Research Institute (French: *Institut de Recherche Robert-Sauvé en Santé et en Sécurité du Travail*).
* [INRS](http://en.inrs.fr/): The French National Research and Safety Institute for the Prevention of Occupational Accidents and Diseases (French: *Institut National de Recherche et de Sécurité pour la prévention des accidents du travail et des maladies professionnelles*).
