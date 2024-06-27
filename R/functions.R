
#### Data join ####

#' Join agent data
#' 
#' Join agent data into a single data frame. Consider the main dataset (the one
#'  named `agents`) and combine it with the corresponding values from other ones
#'  (`CASD_substances`, `mixie_qc`, `mixie_fr` and `toxicological_classes`).
#' 
#' @details
#' Regarding the dataset `agents`, additional columns are the following.
#' \describe{
#'   \item{`oel_fr`}{Occupational Exposure Limit used in France corresponding to
#'         substance identifiers from `agents$casd_id_oel`.}
#'   \item{`oel_fr_unit`}{Units in which the OELs from `oel_fr` are expressed.
#'         \itemize{
#'           \item F: fibres per cubic centimetre.
#'           \item M: milligrams per cubic metre.
#'         }}
#'   \item{`toxicity_qc`}{Toxiciological classes from MiXie-QC corresponding to
#'         substances from `agents$mixie_qc_substance`.}
#'   \item{`mixie_fr_substance`}{Substance in MiXie-FR corresponding to
#'         substance identifiers from `agents$casd_id_mixie`.}
#'   \item{`toxicity_fr`}{Toxicological classes from MiXie-FR corresponding to
#'         substances from `mixie_fr_substance`.}
#' }
#' 
#' Data from column `name_fr` of dataset `toxicological_classes` are not
#'  included in the resulting data frame.
#' Data from datasets `original_substances`, `databases` and `substance_groups`
#'  are not included either.
#' 
#' @param tox_names `TRUE` or `FALSE` whether to also include toxicological
#'  class names or to only include class identifiers.
#' @return A single data frame combining most datasets from the package.
#'  Its row names are agent identifiers.
#' 
#' @author Gauthier Magnin
#' @seealso [`join_all_data`], [`join_grouping_data`].
#' 
#' @examples
#' ## Create the data frame of agent data
#' agent_data <- join_agent_data()
#' 
#' @export
#' 
join_agent_data = function(tox_names = TRUE) {
  
  # Identifier of all agents
  agent_ids = rownames(uom.agents::agents)
  
  # Join
  joined_data = data.frame(
    
    # Agent data and OELs
    uom.agents::agents[, c("name", "family", "molecular_weight", "size_fraction",
                           "oel_source", "oel", "oel_unit", "casd_id_oel")],
    
    # French OELs
    uom.agents::CASD_substances[uom.agents::agents[, "casd_id_oel"],
                                c("oel_fr", "oel_fr_unit")],
    
    # MiXie QC data
    mixie_qc_substance = uom.agents::agents[, "mixie_qc_substance"]
  )
  joined_data$toxicity_qc = unname(get_qc_toxicity(agent_ids, id_only = !tox_names))
  
  joined_data = cbind(
    joined_data,
    
    # MiXie FR data
    casd_id_mixie = uom.agents::agents[, "casd_id_mixie"],
    mixie_fr_substance = get_fr_substance(agent_ids)
  )
  joined_data$toxicity_fr = unname(get_fr_toxicity(agent_ids, id_only = !tox_names))
  
  rownames(joined_data) = agent_ids
  
  return(joined_data)
}


#' Join grouping data
#' 
#' Join grouping data into a single data frame. Consider the dataset
#'  `original_substances` and combine it with the corresponding values from
#'  the datasets `substance_groups` and `agents`.
#' 
#' @details
#' Regarding the dataset `original_substances`, additional columns are the
#'  following.
#' \describe{
#'   \item{`agent_id`}{Identifiers of the chemical agents (i.e., the
#'         inter-database groups) corresponding to
#'         `original_substances$group_id` (i.e., the intra-database groups).}
#'   \item{`agent_name`}{Names of the chemical agents corresponding to
#'         `agent_id`.}
#' }
#' 
#' @return A single data frame combining the datasets from the package that are
#'  about groupings.
#' 
#' @author Gauthier Magnin
#' @seealso [`join_all_data`], [`join_agent_data`].
#' 
#' @examples
#' ## Create the data frame of grouping data
#' grouping_data <- join_grouping_data()
#' 
#' @export
#' 
join_grouping_data = function() {
  
  # For each original substance, identifiers of other entities (groups, agents)
  group_ids = uom.agents::original_substances[, "group_id"]
  agent_ids = uom.agents::substance_groups[group_ids]
  
  # Join
  joined_data = cbind(
    
    # Original substance data and substance group data
    original_id = rownames(uom.agents::original_substances),
    uom.agents::original_substances[, c("database_id", "group_id")],
    
    # Agent data
    agent_id   = agent_ids,
    agent_name = uom.agents::agents[agent_ids, "name"]
  )
  rownames(joined_data) = NULL
  
  return(joined_data)
}


#' Join data
#' 
#' Join (almost) all datasets from the package into two data frames.
#'  One contains agent data. The other one contains grouping data about
#'  original substances in occupational exposure databases.
#' 
#' @details
#' Agent data (rownames are agent identifiers):
#' \describe{
#'   \item{`name`}{Names of the chemical agents.}
#'   \item{`family`}{Chemical families.}
#'   \item{`molecular_weight`}{Molar masses, expressed in grams per mole.}
#'   \item{`size_fraction`}{Particle size fractions considered.}
#'   \item{`oel_source`}{Sources of the exposure limits.}
#'   \item{`oel`}{ Occupational Exposure Limits according to the sources.}
#'   \item{`oel_unit`}{Units in which the OELs are expressed.
#'         \itemize{
#'           \item F: fibres per cubic centimetre.
#'           \item M: milligrams per cubic metre.
#'         }}
#'   \item{`casd_id_oel`}{Identifiers of the CASD substances considered for the
#'         corresponding exposure limits used in France.}
#'   \item{`oel_fr`}{Occupational Exposure Limits used in France.}
#'   \item{`oel_fr_unit`}{Units in which the OELs used in France are expressed.
#'         \itemize{
#'           \item F: fibres per cubic centimetre.
#'           \item M: milligrams per cubic metre.
#'         }}
#'   \item{`mixie_qc_substance`}{Names of the substances considered for the
#'         corresponding toxicological classes from MiXie-QC.}
#'   \item{`toxicity_qc`}{Toxiciological classes corresponding to the substances
#'         of MiXie-QC considered.}
#'   \item{`casd_id_mixie`}{Identifiers of the CASD substances considered for
#'         the corresponding toxicological classes from MiXie-FR.}
#'   \item{`mixie_fr_substance`}{Corresponding substances in MiXie-FR.}
#'   \item{`toxicity_fr`}{Toxicological classes corresponding to the substances
#'         of MiXie-FR considered.}
#' }
#' 
#' Grouping data:
#' \describe{
#'   \item{`original_id`}{Identifiers of the original substances.}
#'   \item{`database_id`}{Identifiers of the databases from which the original
#'         substances come.}
#'   \item{`group_id`}{Group identifiers of substances from the same database
#'         (i.e., intra-database groups).}
#'   \item{`agent_id`}{Main chemical agent identifiers (i.e., the
#'         inter-database groups) corresponding to the groups of original
#'         substances (i.e., the intra-database groups).}
#'   \item{`agent_name`}{Names of the chemical agents corresponding to the main
#'         agent identifiers.}
#' }
#' 
#' Data not included in the resulting data frames are data from the dataset
#'  `databases` and from the column `name_fr` of the dataset
#'  `toxicological_classes`.
#' 
#' @return List of two data frames, containing agent data and grouping data.
#' 
#' @author Gauthier Magnin
#' @seealso [`join_agent_data`], [`join_grouping_data`].
#' 
#' @examples
#' ## Create the list of two data frames and look at each one
#' data_list <- join_all_data()
#' View(data_list$agent)
#' View(data_list$grouping)
#' 
#' @export
#' 
join_all_data = function() {
  return(list(
    agent    = join_agent_data(),
    grouping = join_grouping_data()
  ))
}



#### Data search ####

#' Get agent identifiers
#' 
#' Check if identifiers are given and extract identifiers of all agents if not.
#' This internal function allows to use `NULL` as default value to consider all
#'  agents.
#' 
#' @param agent_id `NULL` or any subset of `agents` rownames.
#' @return Values from `agent_id` if not `NULL`; `agents` rownames otherwise.
#' 
#' @author Gauthier Magnin
#' 
#' @examples
#' get_agent_ids()
#' get_agent_ids(rownames(agents)[6:10])
#' 
#' @template function_not_exported
#' 
get_agent_ids = function(agent_id = NULL) {
  
  if (is.null(agent_id)) {
    agent_id = rownames(uom.agents::agents)
  }
  return(agent_id)
}


#' Get database identifiers
#' 
#' Extract database identifiers of the given intra-database substance groups.
#' 
#' @param group_id Substance group identifiers. Any subset of `substance_groups`
#'  names. If `NULL`, all groups are considered.
#' @return Factor whose levels are `databases` rownames.
#'  Identifier of the database corresponding to each given substance group.
#' 
#' @author Gauthier Magnin
#' 
#' @examples
#' ## Search for databases of some substance groups then of all groups
#' get_database(names(substance_groups)[6:10])
#' get_database()
#' 
#' @export
#' 
get_database = function(group_id = NULL) {
  
  if (is.null(group_id)) {
    group_id = names(uom.agents::substance_groups)
  }
  
  databases = sapply(group_id, function(group) {
    unique(uom.agents::original_substances$database_id[uom.agents::original_substances$group_id == group])
  })
  return(stats::setNames(databases, group_id))
}


#' Get MiXie-FR substances
#' 
#' Extract MiXie-FR substances associated with the given agents.
#' 
#' @param agent_id Agent identifiers. Any subset of `agents` rownames.
#'  If `NULL`, all agents are considered.
#' @return Name of the MiXie-FR substance associated with each given agent.
#' 
#' @author Gauthier Magnin
#' @seealso [`get_fr_toxicity`]
#' 
#' @examples
#' ## Search for MiXie-FR substances of some agents then of all agents
#' get_fr_substance(rownames(agents)[6:10])
#' get_fr_substance()
#' 
#' @export
#' 
get_fr_substance = function(agent_id = NULL) {
  
  # Set identifiers if none is given
  agent_id = get_agent_ids(agent_id)
  
  return(stats::setNames(
    uom.agents::CASD_substances[uom.agents::agents[agent_id, "casd_id_mixie"],
                                "mixie_fr_substance"],
    agent_id
  ))
}


#' Get toxicity names
#' 
#' Extract names of toxicological classes.
#' 
#' @param toxicity_id Toxicological class identifiers. Any subset of the
#'  rownames of `toxicological_classes` or of its column `mixie_fr_id`.
#' @param french `TRUE` or `FALSE` whether to extract French names or English
#'  names.
#' @return Name corresponding to each given toxicological class.
#' 
#' @author Gauthier Magnin
#' @seealso [`get_qc_toxicity`], [`get_fr_toxicity`].
#' 
#' @examples
#' ## Search for names of some MiXie-QC or MiXie-FR toxiciological classes
#' get_toxicity_names(rownames(toxicological_classes)[6:10])
#' get_toxicity_names(toxicological_classes[6:10, "mixie_fr_id"])
#' 
#' @export
#' 
get_toxicity_names = function(toxicity_id, french = FALSE) {
  
  # Get QC class names and FR class names for all given identifiers
  id_names = get_qc_toxicity_names(toxicity_id, french)
  fr_names = get_fr_toxicity_names(toxicity_id, french)
  
  # Merge the two vectors
  names(id_names)[is.na(id_names)] = names(fr_names)[is.na(id_names)]
  id_names[is.na(id_names)] = fr_names[is.na(id_names)]
  
  return(id_names)
}


#' Get toxicity names of MiXie-QC classes or of MiXie-FR classes
#' 
#' Extract names of toxicological classes.
#' 
#' @param toxicity_id Toxicological class identifiers.
#'  For MiXie-QC classes, any subset of `toxicological_classes` rownames.
#'  For MiXie-FR classes, any subset of column `mixie_fr_id` in
#'  `toxicological_classes`.
#'  If `NULL`, all classes are considered.
#' @inheritParams get_toxicity_names
#' @inherit get_toxicity_names return
#' 
#' @author Gauthier Magnin
#' @seealso [`get_toxicity_names`], [`get_qc_toxicity`], [`get_fr_toxicity`].
#' @name get_qcfr_toxicity_names
#' 
#' @template functions_not_exported
#' 
NULL

#' @rdname get_qcfr_toxicity_names
#' 
#' @examples
#' ## Search for names of some MiXie-QC toxicological classes
#' get_qc_toxicity_names(rownames(toxicological_classes)[6:10])
#' 
get_qc_toxicity_names = function(toxicity_id = NULL, french = FALSE) {
  
  # Column of names (French or English)
  name_column = if (french) "name_fr" else "name"
  
  # Create a named vector using IDs as values to facilitate sorting
  toxicity = stats::setNames(rownames(uom.agents::toxicological_classes),
                             uom.agents::toxicological_classes[, name_column])
  toxicity = sort(toxicity[!is.na(toxicity)])
  
  # Inverse names and values so that the IDs are vector names
  toxicity = stats::setNames(names(toxicity), toxicity)
  
  # Return all names or only those matchnig the given identifiers
  if (is.null(toxicity_id)) return(toxicity)
  return(toxicity[toxicity_id])
}

#' @rdname get_qcfr_toxicity_names
#' 
#' @examples
#' ## Search for names of some MiXie-FR toxicological classes
#' get_fr_toxicity_names(toxicological_classes[6:10, "mixie_fr_id"])
#' 
get_fr_toxicity_names = function(toxicity_id = NULL, french = FALSE) {
  
  # Column of names (French or English)
  name_column = if (french) "name_fr" else "name"
  
  # Create a named vector using IDs as values to facilitate sorting
  toxicity = stats::setNames(uom.agents::toxicological_classes[, "mixie_fr_id"],
                             uom.agents::toxicological_classes[, name_column])
  toxicity = sort(toxicity[!is.na(toxicity)])
  
  # Inverse names and values so that the IDs are vector names
  toxicity = stats::setNames(names(toxicity), toxicity)
  
  # Return all names or only those matchnig the given identifiers
  if (is.null(toxicity_id)) return(toxicity)
  return(toxicity[toxicity_id])
}


#' Get MiXie-QC toxicity or MiXie-FR toxicity
#' 
#' Extract MiXie-QC or MiXie-FR toxicological classes associated with the given
#'  agents.
#' 
#' @param agent_id Agent identifiers. Any subset of `agents` rownames.
#'  If `NULL`, all agents are considered.
#' @param as_vector Ignored if more than one identifier is given.
#'  `TRUE` or `FALSE` whether to return the result as a vector or as a named
#'  list of a single vector.
#' @param id_only `TRUE` or `FALSE` whether to search for class identifiers
#'  only or to search for class names too.
#' @return Named list or vector (according to argument `as_vector`).
#'  Toxicological classes associated with each given agent.
#' 
#' @author Gauthier Magnin
#' @seealso [`get_toxicity_names`], [`get_fr_substance`].
#' @name get_qcfr_toxicity
#' 
NULL

#' @rdname get_qcfr_toxicity
#' 
#' @examples
#' ## Search for some MiXie-QC toxicological classes
#' get_qc_toxicity(rownames(agents)[6:10])
#' 
#' ## Search for toxicological classes of a single agent
#' ## as a list or as a vector
#' get_qc_toxicity("0010", as_vector = FALSE)
#' get_qc_toxicity("0010", as_vector = TRUE)
#' 
#' ## Search for toxicological classes, identifiers and names
#' get_qc_toxicity("0010", id_only = FALSE)
#' 
#' @export
#' 
get_qc_toxicity = function(agent_id = NULL, as_vector = TRUE, id_only = TRUE) {
  
  # Set identifiers if none is given
  agent_id = get_agent_ids(agent_id)
  
  # Get toxicological classes through associated MiXie substances
  qc_substances = uom.agents::agents[agent_id, "mixie_qc_substance"]
  qc_toxicity = lapply(qc_substances, function(substance) {
    if (is.na(substance)) return(character(0))
    return(sort(unname(
      uom.agents::mixie_qc[uom.agents::mixie_qc[, "substance"] == substance,
                           "toxicity_id"]
    )))
  })
  
  # Get class names
  if (!id_only) {
    qc_toxicity = lapply(qc_toxicity, get_qc_toxicity_names)
  }
  
  # Return a vector or a list
  if (length(qc_toxicity) == 1 && as_vector) return(qc_toxicity[[1]])
  return(stats::setNames(qc_toxicity, agent_id))
}

#' @rdname get_qcfr_toxicity
#' 
#' @examples
#' ## Search for some MiXie-FR toxicological classes
#' get_fr_toxicity(rownames(agents)[6:10])
#' 
#' @export
#' 
get_fr_toxicity = function(agent_id = NULL, as_vector = TRUE, id_only = TRUE) {
  
  # Set identifiers if none is given
  agent_id = get_agent_ids(agent_id)
  
  # Get toxicological classes through associated MiXie substances
  fr_substances = get_fr_substance(agent_id)
  fr_toxicity = lapply(fr_substances, function(substance) {
    if (is.na(substance)) return(character(0))
    return(sort(unname(
      uom.agents::mixie_fr[uom.agents::mixie_fr[, "substance"] == substance,
                           "toxicity_id"]
    )))
  })
  
  # Get class names
  if (!id_only) {
    fr_toxicity = lapply(fr_toxicity, get_fr_toxicity_names)
  }
  
  # Return a vector or a list
  if (length(fr_toxicity) == 1 && as_vector) return(fr_toxicity[[1]])
  return(fr_toxicity)
}
