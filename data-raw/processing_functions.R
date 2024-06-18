## Formatting and correction functions.
## 
## Note: functions in this file require functions from the file
##       'utility_functions.R' to work.


#' Format and correct data
#' 
#' Apply formatting and corrections to specific agent data.
#' 
#' @param data Data to format and correct.
#' @param rename Logical indicating whether to rename and reorder columns.
#' @param fr_substances Named character vector of the French classification of
#'  substances. Vector names are CASD substance identifiers. Vector values are
#'  MiXie-FR names.
#' @param organizations Named character vector containing organization values to
#'  change. Vector names are old values. Vector values are new values.
#'  If `NULL`, no corrections to organization values will be made.
#' @param fractions Named character vector containing fraction values to change.
#'  Vector names are old values. Vector values are new values.
#'  If `NULL`, only writing corrections will be made (ex: "inhalable" and
#'  "Inhalable" both become "Inhalable").
#' @return Data formatted and corrected.
#' 
#' @author Gauthier Magnin
#' @template function_not_exported
#' 
format_and_correct = function(data, rename = TRUE, fr_substances = NULL,
                              organizations = NULL, fractions = NULL) {
  
  # Apply corrections
  data$main  = correct_main(data$main, rename)
  data$mixie = correct_mixie(data$mixie, rename, fr_substances)
  data$oel   = correct_oel(data$oel, rename, organizations, fractions)
  
  return(data)
}


#' Format and correct main data
#' 
#' Apply corrections to main data.
#' Remove a variable and correct writings.
#' 
#' @param data Data to format and correct.
#' @param rename Logical indicating whether to rename and reorder columns.
#' @return Corrected data.
#' 
#' @author Gauthier Magnin
#' @template function_not_exported
#' 
correct_main = function(data, rename = TRUE) {
  
  # Convert as data frame
  data = as.data.frame(data)
  
  # Remove a variable
  data[["included in analysis"]] = NULL
  
  # Correct and homogenize writings
  data$database = toupper(data$database)
  
  # Add missing intra-database group IDs
  data$db_specific_id[is.na(data$db_specific_id)] = data$db_original_id[is.na(data$db_specific_id)]
  
  # Convert data types
  data$database = typecast(data$database, "factor")
  
  if (!rename) return(data)
  
  # Rename columns
  colnames(data) = c(
    "agent_id",    # multiexpo_code
    "agent_name",  # multiexpo_name
    "database",
    "original_id", # db_original_id
    "group_id"     # db_specific_id
  )
  
  # Reorder columns
  return(data[, c(
    "original_id",
    "database",
    "group_id",
    "agent_id",
    "agent_name"
  )])
}


#' Format and correct MiXie data
#' 
#' Apply corrections to MiXie data.
#' Extract classes from text, replace NAs with empty characters, change MiXie-FR
#'  identifiers and correct MiXie-FR names.
#' 
#' @param data Data to format and correct.
#' @param rename Logical indicating whether to rename and reorder columns.
#' @param fr_substances Named character vector of the French classification of
#'  substances. Vector names are CASD substance identifiers. Vector values are
#'  MiXie-FR names.
#' @return Corrected data.
#' 
#' @author Gauthier Magnin
#' @template function_not_exported
#' 
correct_mixie = function(data, rename = TRUE, fr_substances = NULL) {
  
  # Convert as data frame
  data = as.data.frame(data)
  
  # Correct MiXie-FR names
  if (!is.null(fr_substances)) {
    data$mixieFR_fr_name = fr_substances[data$casd_sub_id]
  }
  
  # Split character values containing multiple values
  regex = "\\s+\\|\\s+"
  data$QCtox_classes = strsplit(data$QCtox_classes, regex)
  data$FRtox_classes = strsplit(data$FRtox_classes, regex)
  
  # Replace NA values by empty characters
  data$QCtox_classes = replace_na_in_list(data$QCtox_classes, "character")
  data$FRtox_classes = replace_na_in_list(data$FRtox_classes, "character")
  
  # Change MiXie-FR identifiers to allow natural sorting
  data$FRtox_classes = lapply(data$FRtox_classes, correct_mixie_fr_identifiers)
  
  if (!rename) return(data)
  
  # Rename columns
  colnames(data) = c(
    "agent_id",           # multiexpo_code
    "agent_name",         # multiexpo_name
    "casd_id",            # casd_sub_id
    "mixie_fr_substance", # mixieFR_fr_name
    "mixie_qc_substance", # mixieQC_fr_name
    "toxicity_qc_ids",    # QCtox_classes
    "toxicity_fr_ids"     # FRtox_classes
  )
  
  # Reorder columns
  return(data[, c(
    "agent_id",
    "agent_name",
    "mixie_qc_substance",
    "toxicity_qc_ids",
    "casd_id",
    "mixie_fr_substance",
    "toxicity_fr_ids"
  )])
}


#' Format and correct OEL data
#' 
#' Apply corrections to OEL data.
#' Correct writings.
#' 
#' @param data Data to format and correct.
#' @param rename Logical indicating whether to rename and reorder columns.
#' @param organizations Named character vector containing organization values to
#'  change. Vector names are old values. Vector values are new values.
#'  If `NULL`, no corrections to organization values will be made.
#' @param fractions Named character vector containing fraction values to change.
#'  Vector names are old values. Vector values are new values.
#'  If `NULL`, only writing corrections will be made (ex: "inhalable" and
#'  "Inhalable" both become "Inhalable").
#' @return Corrected data.
#' 
#' @author Gauthier Magnin
#' @template function_not_exported
#' 
correct_oel = function(data, rename = TRUE,
                       organizations = NULL, fractions = NULL) {
  
  # Convert as data frame
  data = as.data.frame(data)
  
  # Correct and homogenize writings about organizations
  if (!is.null(organizations)) {
    for (organization in names(organizations)) {
      data$oel_org[data$oel_org == organization] = organizations[organization]
    }
  }
  
  # Correct and homogenize writings about families and fractions
  data$family       = cap(data$family)
  data$oel_fraction = cap(data$oel_fraction)
  
  if (!is.null(fractions)) {
    for (fraction in names(fractions)) {
      data$oel_fraction[data$oel_fraction == fraction] = fractions[fraction]
    }
  }
  
  # Convert data types
  new_types = c(
    oel_unit     = "factor",
    oel_fraction = "factor",
    family       = "factor",
    fr_oel_unit  = "factor"
  )
  for (col in names(new_types)) {
    data[, col] = typecast(data[, col], new_types[col])
  }
  
  if (!rename) return(data)
  
  # Rename columns
  colnames(data) = c(
    "agent_id",        # multiexpo_code
    "agent_name",      # multiexpo_name
    "oel",             # oel_value
    "oel_unit",
    "oel_source",      # oel_org
    "size_fraction",   # oel_fraction
    "family",
    "oel_fr",          # fr_oel
    "oel_fr_unit",     # fr_oel_unit
    "casd_id",         # casd_sub_id
    "molecular_weight" # mol_weight
  )
  
  # Reorder columns
  return(data[, c(
    "agent_id",
    "agent_name",
    "family",
    "molecular_weight",
    "size_fraction",
    "oel_source",
    "oel",
    "oel_unit",
    "casd_id",
    "oel_fr",
    "oel_fr_unit"
  )])
}


#' Correct MiXie-FR identifiers
#' 
#' Apply corrections to MiXie-FR identifiers.
#' Add "0" before the first digit of the identifier so that all identifiers have
#'  the same number of digits and can be sorted naturally
#' 
#' @param ids Identifiers from MiXie-FR.
#' @return Corrected identifiers.
#' 
#' @author Gauthier Magnin
#' @template function_not_exported
#' 
correct_mixie_fr_identifiers = function(ids) {
  return(paste0(
    substr(ids, 1, 4),
    extend_left(substring(ids, 5), "0", 2)
  ))
}
