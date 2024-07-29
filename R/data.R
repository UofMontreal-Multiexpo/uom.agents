
#' Chemical agents
#' 
#' Classification grouping substances from several occupational exposure
#'  databases.
#' Associate chemical agents with properties, exposure limits and substances
#'  from other classifications.
#' 
#' @details
#' Different CASD substance identifiers may be associated with a single agent
#'  for exposure limit and toxicity (i.e., `casd_id_oel` and `casd_id_mixie`)
#'  because chemical agents may correspond to several CASD substances. Only one
#'  limit is retained and the most relevant substance is considered for
#'  toxicity.
#' 
#' @format Data frame of 328 rows whose names are agent identifiers, and 10
#'  columns:
#'  \describe{
#'    \item{`name`}{*Character*. Name of chemical agent.}
#'    \item{`family`}{*Factor*. Chemical family.}
#'    \item{`molecular_weight`}{*Numeric*. Molar mass, expressed in grams per
#'          mole.}
#'    \item{`size_fraction`}{*Factor*. Particle size fraction considered.}
#'    \item{`oel_source`}{*Character*. Source of the exposure limit.}
#'    \item{`oel`}{*Numeric*. Occupational Exposure Limit according to the
#'          source (8h-OEL).}
#'    \item{`oel_unit`}{*Factor*. Unit in which the OEL is expressed.
#'          \itemize{
#'            \item F: fibres per cubic centimetre.
#'            \item M: milligrams per cubic metre.
#'          }}
#'    \item{`casd_id_oel`}{*Character*. Identifier of the CASD substance
#'          considered for the corresponding exposure limit used in France.}
#'    \item{`mixie_qc_substance`}{*Character*. Name of the substance considered
#'          for the corresponding toxicological classes from MiXie-QC.}
#'    \item{`casd_id_mixie`}{*Character*. Identifier of the CASD substance
#'          considered for the corresponding toxicological classes from
#'          MiXie-FR.}
#'  }
#' @source Agent identifiers and names were created based on the original
#'  substances they group together.
#'  Chemical families were partly obtained from Bosson-Rieutort et al. (see
#'  'References' section) and were assigned in a similar way for agents not
#'  considered in this paper.
#'  
#'  Molecular weights were obtained from an extract from 2007 of the PHYSPROP
#'   database hosted by the Syracuse Research Corporation, from the OSHA
#'   Occupational Chemical Database, and from the BOC Sciences website.
#'  Sources for size fraction, OEL and units are given in column `oel_source`.
#'   They were mainly obtained from the OSHA Occupational Chemical Database and
#'   the GESTIS Substance Database.
#'  CASD identifiers were provided by the [INRS](http://en.inrs.fr/).
#'  MiXie-QC substance names were provided by the
#'   [IRSST](https://www.irsst.qc.ca/en/).
#' @references
#'  Bosson-Rieutort D, Sarazin P, Bicout DJ, Ho V, Lavoué J (2020).
#'  Occupational Co-exposures to Multiple Chemical Agents from Workplace
#'  Measurements by the US Occupational Safety and Health Administration.
#'  *Annals of Work Exposures and Health*, Volume 64, Issue 4, May 2020,
#'  Pages 402–415. <https://doi.org/10.1093/annweh/wxaa008>.
#' @seealso [`CASD_substances`], [`mixie_qc`], [`original_substances`],
#'  [`substance_groups`].
#' 
"agents"


#' CASD substances
#' 
#' Substances from the CASD database.
#' Associate CASD substances with French occupational exposure limits and
#'  substances from MiXie-FR.
#' 
#' @details
#' It does not contain all substances from CASD.
#' 
#' @format Data frame of 284 rows whose names are CASD substance identifiers,
#'  and 3 columns:
#'  \describe{
#'    \item{`oel_fr`}{*Numeric*. Occupational Exposure Limit used in France
#'          (8h-OEL).}
#'    \item{`oel_fr_unit`}{*Factor*. Unit in which the OEL is expressed.
#'          \itemize{
#'            \item F: fibres per cubic centimetre.
#'            \item M: milligrams per cubic metre.
#'          }}
#'    \item{`mixie_fr_substance`}{*Character*. Corresponding substance in
#'          MiXie-FR.}
#'  }
#' @source Data provided by the [INRS](http://en.inrs.fr/).
#' @seealso [`mixie_fr`], [`agents`].
#' 
"CASD_substances"


#' Databases
#' 
#' Occupational exposure databases.
#' Associate databases with territories and organizations.
#' 
#' @details
#' The USIS database combines the IMIS (Integrated Management Information
#'  System) and the OIS (OSHA Information System) databases.
#' 
#' @format Character matrix of 4 rows whose names are database identifiers, and
#'  3 columns:
#'  \describe{
#'    \item{`name`}{Name of the database.}
#'    \item{`territory`}{Territory covered by the database.}
#'    \item{`organization`}{Organization that hosts the database.}
#'  }
#' @seealso [`original_substances`]
#' 
"databases"


#' MiXie France
#' 
#' Substances and toxicological classes from MiXie-FR.
#' Associate substances with toxicological classes.
#' 
#' @details
#' It does not contain all substances from MiXie-FR.
#' 
#' @format Character matrix of 661 rows and 2 columns:
#'  \describe{
#'    \item{`substance`}{Substance name.}
#'    \item{`toxicity_id`}{Identifier of a toxicological class.}
#'  }
#' @source Data provided by the [INRS](http://en.inrs.fr/).
#' 
#' Toxicological class identifiers were created for all provided classes.
#' @seealso [`mixie_qc`], [`toxicological_classes`].
#' 
"mixie_fr"


#' MiXie Quebec
#' 
#' Substances and toxicological classes from MiXie-QC.
#' Associate substances with toxicological classes.
#' 
#' @details
#' It does not contain all substances from MiXie-QC.
#' 
#' @format Character matrix of 952 rows and 2 columns:
#'  \describe{
#'    \item{`substance`}{Substance name.}
#'    \item{`toxicity_id`}{Identifier of a toxicological class.}
#'  }
#' @source Data provided by the [IRSST](https://www.irsst.qc.ca/en/).
#' @seealso [`mixie_fr`], [`toxicological_classes`].
#' 
"mixie_qc"


#' Original substances
#' 
#' Substances from several occupational exposure databases.
#' Associate substance identifiers with their original databases and with
#'  identifiers of groups within those databases.
#' 
#' @details
#' It does not contain all substances from each database.
#' 
#' @format Data frame of 1 065 rows whose names are original substance
#'  identifiers, and 2 columns:
#'  \describe{
#'    \item{`database_id`}{*Factor*. Identifier of the database from which the
#'          substance comes.}
#'    \item{`group_id`}{*Character*. Group identifier of substances from the
#'          same database.}
#'  }
#' @source Substance identifiers were provided
#'  by the [IRSST](https://www.irsst.qc.ca/en/) (for the LIMS database),
#'  by the [OSHA](https://www.osha.gov/) (for the USIS database), and
#'  by the [INRS](http://en.inrs.fr/) (for the CASD database).
#'  Substance identifiers from the CWED database were created.
#'  
#'  Group identifiers were created.
#' @seealso [`databases`], [`substance_groups`].
#' 
"original_substances"


#' Substance groups
#' 
#' Substance groups from several databases.
#' Associate identifiers of intra-database substance groups with main chemical
#'  agent identifiers, which can be considered as inter-database substance
#'  groups.
#' 
#' @format Named character vector of 851 elements.
#' @source Group and agent identifiers were created.
#' @seealso [`agents`], [`original_substances`].
#' 
"substance_groups"


#' Toxicological classes
#' 
#' Toxicological classes from MiXie-QC and MiXie-FR.
#' Associate class identifiers with English and French class names.
#' 
#' @format Character matrix of 24 rows whose names are toxicological class
#'  identifiers from MiXie-QC, and 3 columns:
#'  \describe{
#'    \item{`mixie_fr_id`}{Identifier for the MiXie-FR toxicological class.}
#'    \item{`name`}{Name of the toxicological class.}
#'    \item{`name_fr`}{French name of the toxicological class.}
#'  }
#' @source Data provided by the [IRSST](https://www.irsst.qc.ca/en/) and the
#'  [INRS](http://en.inrs.fr/), except identifiers of French toxicological
#'  classes.
#'  
#'  Identifiers of French toxicological classes were created for all provided
#'  classes from MiXie-FR.
#' @seealso [`mixie_fr`], [`mixie_qc`].
#' 
"toxicological_classes"
