## Preparation of the datasets relating to chemical agents: one main data frame
## and supplementary tables.
## 
## Input files:
##  - multiexpo_agent_list.RDS
##  - mixie_classes_fr_en.RDS
##  - data_Mixie_20230505_withFRTCcode.RDS
##  - data_Mixie_20230505.RDS
##  
##  - databases.csv
##  - fractions_to_be_replaced.csv
##  - missing_db_specific_id.csv
##  - organizations_to_be_replaced.csv
##  
##  - utility_functions.R
##  - processing_functions.R
## 
## Output files:
##  - agents.RData and .csv
##  - CASD_substances.RData and .csv
##  - databases.RData and .csv
##  - mixie_fr.RData and .csv
##  - mixie_qc.RData and .csv
##  - original_substances.RData and .csv
##  - substance_groups.RData and .csv
##  - toxicological_classes.RData and .csv
## 
## Other output files, resource files for tests:
##  - tests/res/agent_data.RDS
##  - tests/res/groups_by_database.RDS
## 
## Temporary files:
##  - tmp/agent_data_main.csv
##  - tmp/agent_data_mixie.csv
##  - tmp/agent_data_oel.csv



#### Loading ####

# Loading functions
source("./data-raw/utility_functions.R",    encoding = "UTF-8")
source("./data-raw/processing_functions.R", encoding = "UTF-8")

# Function to create paths to data input files
data_path = function(x) paste0("./data-raw/create_agent_data/", x)


# Loading data
agent_data = readRDS(data_path("multiexpo_agent_list.RDS"))

# Loading MiXie reference tables
mixie_qc_classes = readRDS(data_path("mixie_classes_fr_en.RDS"))
mixie_fr_classes = readRDS(data_path("data_Mixie_20230505_withFRTCcode.RDS"))

# Loading associations between CASD substance identifiers and MiXie-FR names
fr_substances = turn_2c_into_vec(readRDS(data_path("data_Mixie_20230505.RDS"))[c(1, 2)])

# Loading other reference tables
databases = read.csv(data_path("databases.csv"),
                     header = FALSE,
                     colClasses = "character",
                     encoding = "UTF-8")

# Loading correction data
fraction_changes = turn_2c_into_vec(read.csv(data_path("fractions_to_be_replaced.csv"),
                                             colClasses = "character",
                                             encoding = "UTF-8"))
organization_changes = turn_2c_into_vec(read.csv(data_path("organizations_to_be_replaced.csv"),
                                                 colClasses = "character",
                                                 encoding = "UTF-8"))

# Loading additional data
missing_specific_ids = turn_2c_into_vec(read.csv(data_path("missing_db_specific_id.csv"),
                                                 colClasses = "character",
                                                 encoding = "UTF-8"))



#### 1. Formatting and correction of the data ####

# Agent data
agent_data = format_and_correct(agent_data, rename = TRUE, remove = TRUE,
                                specific_ids = missing_specific_ids,
                                fr_substances = fr_substances,
                                organizations = organization_changes,
                                fractions = fraction_changes)


# MiXie data (remove or replace certain characters)
mixie_qc_classes$toxicological_class_name_fr = gsub("\\s+", " ",
                                                    mixie_qc_classes$toxicological_class_name_fr)
mixie_qc_classes$toxicological_class_name_fr = gsub("oe", "œ",
                                                    mixie_qc_classes$toxicological_class_name_fr)
mixie_fr_classes$frtc_name = gsub("-", "", mixie_fr_classes$frtc_name)

# MiXie-FR identifiers
mixie_fr_classes$frtc_code = correct_mixie_fr_identifiers(mixie_fr_classes$frtc_code)



#### 2. Creation of the datasets ####

##### 2.a. Substances and groupings #####

# Data frames of susbtances and groups of substances
original_substances = unique(agent_data$main[, c("original_id", "database", "group_id")])
substance_groups = unique(agent_data$main[, c("group_id", "agent_id")])
agents = unique(agent_data$oel[, c("agent_id", "agent_name",
                                   "family", "molecular_weight",
                                   "size_fraction", "oel_source", "oel", "oel_unit")])


##### 2.b. French OEL #####

# Association between CASD substance identifiers and French OEL, and
# between final substance groups and these identifiers
# (the associations considered are not the same as for MiXie classes)
casd_oel = unique(agent_data$oel[, c("casd_id", "oel_fr", "oel_fr_unit")])
agent_casd_for_oel = unique(agent_data$oel[, c("agent_id", "casd_id")])

agents = cbind(
  agents,
  casd_id_oel = agent_casd_for_oel$casd_id[match(agents$agent_id,
                                                 agent_casd_for_oel$agent_id)]
)
rm(agent_casd_for_oel)


##### 2.c. MiXie substances and toxicity #####

# MiXie-QC and MiXie-FR (substances and toxicological classes)
mixie_qc = unique(agent_data$mixie[, c("mixie_qc_substance", "toxicity_qc_ids")])
mixie_fr = unique(agent_data$mixie[, c("mixie_fr_substance", "toxicity_fr_ids")])

# Unlist to have one value per row
mixie_qc = data.frame(substance = rep(mixie_qc$mixie_qc_substance,
                                      lengths(mixie_qc$toxicity_qc_ids)),
                      toxicity_id = unlist(mixie_qc$toxicity_qc_ids))
mixie_fr = data.frame(substance = rep(mixie_fr$mixie_fr_substance,
                                      lengths(mixie_fr$toxicity_fr_ids)),
                      toxicity_id = unlist(mixie_fr$toxicity_fr_id))

# Association between final substance groups and corresponding MiXie-QC substances
agent_mixie_qc = unique(agent_data$mixie[, c("agent_id", "mixie_qc_substance")])

agents = cbind(
  agents,
  mixie_qc_substance = agent_mixie_qc$mixie_qc_substance[match(agents$agent_id,
                                                               agent_mixie_qc$agent_id)]
)
rm(agent_mixie_qc)

# Association between CASD substance identifiers and MiXie-FR substances, and
# between final substance groups and these identifiers
casd_mixie = unique(agent_data$mixie[, c("casd_id", "mixie_fr_substance")])
agent_casd_for_mixie = unique(agent_data$mixie[, c("agent_id", "casd_id")])

agents = cbind(
  agents,
  casd_id_mixie = agent_casd_for_mixie$casd_id[match(agents$agent_id,
                                                     agent_casd_for_mixie$agent_id)]
)
rm(agent_casd_for_mixie)

# Toxicological classes from MiXie-QC and MiXie-FR
toxicological_classes = data.frame(
  mixie_qc_id = mixie_qc_classes$toxicological_class_code,
  mixie_fr_id = mixie_fr_classes$frtc_code[
    match(mixie_qc_classes$toxicological_class_name_fr,
          mixie_fr_classes$frtc_name)
  ],
  name    = mixie_qc_classes$toxicological_class_name_en,
  name_fr = mixie_qc_classes$toxicological_class_name_fr
)
rm(mixie_qc_classes, mixie_fr_classes)


##### 2.d. CASD substances #####

# Merge the datasets containing associations between CASD substance identifiers
# and French OEL and MiXie-FR substances
CASD_substances = merge(casd_oel, casd_mixie, by = "casd_id", all = TRUE)
rm(casd_oel, casd_mixie)


##### 2.e. Names and primary keys #####

# Rename columns
colnames(databases)              = c("id", "name", "territory", "organization")
colnames(original_substances)[2] = "database_id"
colnames(agents)[c(1, 2)]        = c("id", "name")

# Names of the variables
dataset_names = c("agents", "substance_groups", "original_substances",
                  "databases", "CASD_substances",
                  "mixie_qc", "mixie_fr", "toxicological_classes")

# Columns forming the primary keys of the datasets
id_columns = setNames(lapply(dataset_names, function(name) 1L),
                      dataset_names)
id_columns[["mixie_qc"]] = c(1L, 2L)
id_columns[["mixie_fr"]] = c(1L, 2L)



#### 3. Validation and formatting of the datasets ####

##### 3.1. First validation #####

cat("First validation...",
    "(Certain invalidities may be corrected by the following formatting instructions.)\n")

# Checking the validity of the datasets
for (name in dataset_names) {
  validate_keys(get(name), keys = id_columns[[name]], name = name)
}


##### 3.2. Formatting the datasets #####

# Remove NA keys from the datasets
for (name in dataset_names) {
  
  # If x has a one-column primary key: x <- x[!is.na(x[[1]]), ]
  # If x has a two-column primary key: x <- x[!is.na(x[[1]]) | !is.na(x[[2]]), ]
  eval(parse(text = paste0(name, " <- ", name, "[",
                           paste0(
                             "!is.na(", name, "[[", id_columns[[name]], "]])",
                             collapse = " | "
                           ),
                           ", ]")))
}

# Order the keys of the datasets
for (name in dataset_names) {
  
  # If x has a one-column primary key: x <- x[order(x[[1]]), ]
  # If x has a two-column primary key: x <- x[order(x[[1]], x[[2]]), ]
  eval(parse(text = paste0(name, " <- ", name, "[",
                           paste0("order(",
                                  paste0(name, "[[", id_columns[[name]], "]]",
                                         collapse = ", "),
                                  ")"),
                           ", ]")))
}

# Remove rownames of the datasets
for (name in dataset_names) {
  
  # rownames(x) <- NULL
  eval(parse(text = paste0("rownames(", name, ") <- NULL")))
}


##### 3.3. Second validation #####

cat("Second validation... ")
message("Any following invalid table message must be addressed.")

# Checking the validity of the datasets
invalid = FALSE

for (name in dataset_names) {
  if(!validate_keys(get(name), keys = id_columns[[name]], name = name)) {
    invalid = TRUE
  }
}

if (invalid) stop("Invalid datasets.")



#### 4. Export as CSV ####

# CSV directory and temporary directory
csv_dir = "./data/csv/"
tmp_dir = "./tmp/"

# Use "N/A" for NA values
missing_value = "N/A"


# Distinguish non-exported datasets (not exported as CSV and only internal in the package)
internal_datasets = character(0)
exported_datasets = setdiff(dataset_names, internal_datasets)

# Save the exported datasets
for (name in exported_datasets) {
  
  write.csv(get(name),
            file = paste0(csv_dir, name, ".csv"),
            na = missing_value,
            row.names = FALSE,
            fileEncoding = "UTF-8")
}


# Temporarily save as CSV the internal datasets
# as well as the data frames from the overall list (for assessment)
dir.create(tmp_dir, showWarnings = FALSE)

# Extract the data frames from the list and simplify list columns back to vector columns
agent_data_main  = agent_data$main
agent_data_mixie = agent_data$mixie
agent_data_oel   = agent_data$oel
agent_data_mixie$toxicity_fr_ids = sapply(agent_data_mixie$toxicity_fr_ids,
                                          function(x) paste0(x, collapse = " | "))
agent_data_mixie$toxicity_qc_ids = sapply(agent_data_mixie$toxicity_qc_ids,
                                          function(x) paste0(x, collapse = " | "))
agent_datasets = c("agent_data_main", "agent_data_mixie", "agent_data_oel")

# Save as CSV temporarily
for (name in c(internal_datasets, agent_datasets)) {
  
  write.csv(get(name),
            file = paste0(tmp_dir, name, ".csv"),
            na = missing_value,
            row.names = FALSE,
            fileEncoding = "UTF-8")
}



#### 5. Simplify the tables ####

# Distinguish tables having two columns from those having more than two
is_vectorizable = sapply(dataset_names,
                         function(name) ncol(get(name)) == 2 && length(id_columns[[name]]) == 1)
not_vectorizable_tables = names(which(!is_vectorizable))
vectorizable_tables = names(which(is_vectorizable))

# Turn two-column tables having one-column primary keys into named vectors
for (name in vectorizable_tables) {
  
  # x <- turn_2c_into_vec(x)
  eval(parse(text = paste0(name, " <- turn_2c_into_vec(", name, ")")))
}

# Use primary keys as rownames for tables having more than two columns
# and remove the associated column, if the key is formed by a single column
for (name in not_vectorizable_tables) {
  
  if (length(id_columns[[name]]) == 1) {
    
    # rownames(x) <- as.character(x[[1]])
    # x <- x[, -1]
    eval(parse(text = paste0("rownames(", name, ") <- as.character(", name, "[[1]])")))
    eval(parse(text = paste0(name, " <- ", name, "[, -1]")))
  }
  # else {
  #   # Paste columns composing the key and use the result as rownames
  #   
  #   # If x has a two-column primary key:
  #   # rownames(x) <- paste(x[[1]], x[[2]], sep = ".")
  #   eval(parse(text = paste0("rownames(", name, ") <- ",
  #                            paste0("paste(",
  #                                   paste0(name, "[[", id_columns[[name]], "]]", collapse = ", "),
  #                                   ", sep = \".\")")
  #                            )))
  # }
}

# Turn data frames into matrices if all their columns have the same type
for (name in not_vectorizable_tables) {
  if (length(unique(sapply(get(name), class))) == 1) {
    
    # x <- as.matrix(x)
    eval(parse(text = paste0(name, "<- as.matrix(", name, ")")))
  }
}



#### 6. Add prepared data to package ####

# Save exported data in data/
for (name in exported_datasets) {
  save(list = name, file = paste0("./data/", name, ".RData"))
}
files_to_compress = paste0("./data/", exported_datasets, ".RData")

# Save internal data in R/sysdata.rda
# usethis::use_data(internal_datasets[1], internal_datasets[2], ...,
#                   internal = TRUE, overwrite = TRUE)
if (length(internal_datasets) != 0) {
  eval(parse(text = paste0("usethis::use_data(",
                           paste0(internal_datasets, collapse = ", "),
                           ", internal = TRUE, overwrite = TRUE)")))
  
  files_to_compress = c(files_to_compress, "./R/sysdata.rda")
}

# Optimize compression
resave_with_best_compression(files_to_compress)



#### Assessment ####

## Compute data weight saved by the new structure

weight_env = c(
  initial = object.size(agent_data),
  final   = sum(sapply(dataset_names, function(x) object.size(get(x))))
)
weight_env["difference"] = weight_env["final"] - weight_env["initial"]

weight_rd = c(
  initial = file.size(data_path("multiexpo_agent_list.RDS")),
  final   = sum(file.size(list.files("./data/", full.names = TRUE, pattern = "*.RData")))
)
weight_rd["difference"] = weight_rd["final"] - weight_rd["initial"]

weight_csv = c(
  initial = sum(file.size(paste0(tmp_dir, agent_datasets, ".csv"))),
  final   = sum(file.size(list.files(csv_dir, full.names = TRUE)),
                if (length(internal_datasets) != 0) {
                  file.size(list.files(tmp_dir,
                                       pattern = paste(internal_datasets, collapse = "|"),
                                       full.names = TRUE))
                } else 0)
)
weight_csv["difference"] = weight_csv["final"] - weight_csv["initial"]


# Count the amount of data
amount_of_data = c(
  initial = sum(sapply(agent_data, function(x) prod(dim(x)))),
  final   = sum(sapply(not_vectorizable_tables,
                       function(name) prod(dim(get(name)) + c(0, 1))),
                sapply(vectorizable_tables,
                       function(name) length(get(name)) * 2))
)
amount_of_data["difference"] = amount_of_data["final"] - amount_of_data["initial"]


# Display the assessment
{
  cat("Amount of data:\n")
  print(amount_of_data)
  
  cat("\nWeight environment (KiB):\n")
  print(round(weight_env / 1024, 1))
  
  cat("\nWeight as RDS/RData files (KiB):\n")
  print(round(weight_rd / 1024, 1))
  
  cat("\nWeight as CSV files (KiB):\n")
  print(round(weight_csv / 1024, 1))
}



#### Test preparation ####

# Export data to use for comparisons when testing functions
test_res_dir  = "./tests/res/"
test_res_path = function(x) paste0(test_res_dir, x)

# Data based on the overall list of data frames agent_data
saveRDS(
  # Groups for each origin database
  stats::setNames(
    lapply(rownames(databases), function(database_id) {
      return(unique(agent_data$main$group_id[agent_data$main$database == database_id]))
    }),
    rownames(databases)
  ),
  file = test_res_path("groups_by_database.RDS"),
  compress = "xz"
)

# Overall list of data frames containing agent data
saveRDS(agent_data, test_res_path("agent_data.RDS"), compress = "xz")



#### Post-processing ####

# Remove tmp directory
unlink(substr(tmp_dir, 1, nchar(tmp_dir) - 1), recursive = TRUE)
