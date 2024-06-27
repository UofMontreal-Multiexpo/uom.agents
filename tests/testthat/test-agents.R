## Note: some tests require additional data for comparisons.
##       The files concerned are available in the following Dropbox folder:
##       SHARE Multiexpo_databases/Testing resources/Agents/
##       These files must be placed in the directory './tests/res/'.



#### Loading resource data for tests ####

# The following condition allows the instruction to be run both manually or automatically
if (grepl("testthat", getwd())) {
  test_res_path = function(x) paste0("../res/", x)
} else {
  test_res_path = function(x) paste0("./tests/res/", x)
}

# Data exported by the script 'create_USIS_data.R' from the overall list of data
# frames before restructuring.
# This avoids having to modify the tested values corresponding to real data when
# new versions (i.e., new data) are created.
groups_by_database = readRDS(test_res_path("groups_by_database.RDS"))

# Overall list of data frames containing agent data (before restructuring),
# exported by the script 'create_agent_data.R'
old_agent_data = readRDS(test_res_path("agent_data.RDS"))



#### Data join ####

##### join_agent_data #####

test_that("join_agent_data reconstructs the original 'mixie' dataset", {
  
  # Result of the fonction and columns to compare with the original dataset
  result = join_agent_data(tox_names = FALSE)
  cols_to_compare = c("name", "mixie_qc_substance", "toxicity_qc",
                      "casd_id_mixie", "mixie_fr_substance", "toxicity_fr")
  
  # Sort the expected values
  expected = old_agent_data$mixie[order(old_agent_data$mixie$agent_id), ]
  expected$toxicity_fr_ids = lapply(expected$toxicity_fr_ids,
                                    function(x) sort(x))
  
  # Move agent identifiers to rownames
  rownames(expected) = expected$agent_id
  expected = expected[, -1]
  
  # Compare ignoring column names
  expect_identical(unname(result[, cols_to_compare]),
                   unname(expected))
})

test_that("join_agent_data reconstructs the original 'oel' dataset", {
  
  # Result of the fonction and columns to compare with the original dataset
  result = join_agent_data()
  cols_to_compare = c("name", "family", "molecular_weight", "size_fraction",
                      "oel_source", "oel", "oel_unit",
                      "casd_id_oel", "oel_fr", "oel_fr_unit")
  
  # Sort the expected values
  expected = old_agent_data$oel[order(old_agent_data$oel$agent_id), ]
  rownames(expected) = NULL
  
  # Move agent identifiers to rownames
  rownames(expected) = expected$agent_id
  expected = expected[, -1]
  
  # Compare ignoring column names
  expect_identical(unname(result[, cols_to_compare]),
                   unname(expected))
})


##### join_grouping_data #####

test_that("join_grouping_data reconstructs the original 'main' dataset", {
  
  # Sort the expected values
  expected = old_agent_data$main[order(old_agent_data$main$original_id), ]
  rownames(expected) = NULL
  
  # Sort the result values
  result = join_grouping_data()
  result = result[order(result$original_id), ]
  rownames(result) = NULL
  # Note: the sort order is different in test_that than the one used to create
  # the dataset (because of locales), hence the need to sort both data frames
  # here
  
  # Compare ignoring column names
  expect_identical(unname(result),
                   unname(expected))
})


##### join_all_data #####

test_that("join_all_data returns a named list", {
  result = join_all_data()
  
  expect_named(result)
  expect_type(result, "list")
})



#### Data search ####

##### get_database #####

test_that("get_database returns a named factor", {
  result = get_database()
  
  expect_named(result)
  expect_s3_class(result, "factor")
})

test_that("get_database extracts original database information for the given groups", {
  
  # Get identifiers of two groups for each database
  # and use a different order than in the initial data
  ids = c(groups_by_database[[1]][c(1, 2)],
          groups_by_database[[2]][c(1, 2)],
          groups_by_database[[4]][c(1, 2)],
          groups_by_database[[3]][c(1, 2)])
  expected = factor(stats::setNames(names(groups_by_database)[c(1,1,2,2,4,4,3,3)],
                                    ids),
                    levels = names(groups_by_database))
  
  expect_equal(get_database(ids),
               expected)
})

test_that("get_database extracts original database information of all groups if none is given", {
  # Only compare the numbers of values
  # (the validation of extracted values is performed by the another test)
  expect_length(get_database(), length(substance_groups))
})


##### get_fr_substance #####

test_that("get_fr_substance returns a named character vector", {
  result = get_fr_substance()
  
  expect_named(result)
  expect_vector(result, character(0))
})

test_that("get_fr_substance extracts the associated substances in MiXie-FR for the given agents", {
  
  # Get the identifiers of an agent having an associated substance in MiXie-FR
  # and an agent having no associated substance
  ids = c(old_agent_data$mixie$agent_id[!is.na(old_agent_data$mixie$casd_id)][1],
          old_agent_data$mixie$agent_id[ is.na(old_agent_data$mixie$casd_id)][1])
  expected = stats::setNames(
    c(old_agent_data$mixie$mixie_fr_substance[old_agent_data$mixie$agent_id == ids[1]],
      old_agent_data$mixie$mixie_fr_substance[old_agent_data$mixie$agent_id == ids[2]]),
    ids
  )
  
  expect_identical(get_fr_substance(ids),
                   expected)
})

test_that("get_fr_substance extracts the associated substances in MiXie-FR of all agents if none is given", {
  # Only compare the numbers of values
  # (the validation of extracted values is performed by the another test)
  expect_length(get_fr_substance(), nrow(agents))
})


##### get_toxicity_names #####

test_that("get_toxicity_names returns a named character vector", {
  ids = rownames(toxicological_classes)[c(1, 9, 16)]
  result = get_toxicity_names(ids)
  
  expect_named(result)
  expect_vector(result, character(0))
})

test_that("get_toxicity_names returns NA values and named for unknown identifiers", {
  ids = c("A1", "B2")
  result = get_toxicity_names(ids)
  
  expect_identical(result,
                   stats::setNames(c(NA_character_, NA_character_),
                                   c(NA_character_, NA_character_)))
})

test_that("get_toxicity_names returns English names if 'french' is FALSE", {
  
  result = get_toxicity_names("TC01", french = FALSE)
  expect_identical(unname(result),
                   "Metabolic acidosis")
})

test_that("get_toxicity_names returns French names if 'french' is TRUE", {
  
  result = get_toxicity_names("TC01", french = TRUE)
  expect_identical(unname(result),
                   "Acidose métabolique")
})


##### get_qc_toxicity #####

test_that("get_qc_toxicity returns an unnamed character vector if a single ID is given, 'as_vector' is TRUE and 'id_only' is TRUE", {
  id = rownames(agents)[1]
  result = get_qc_toxicity(id, as_vector = TRUE, id_only = TRUE)
  
  expect_named(result, NULL)
  expect_vector(result, character(0))
})

test_that("get_qc_toxicity returns a named character vector if a single ID is given, 'as_vector' is TRUE and 'id_only' is FALSE", {
  id = rownames(agents)[1]
  result = get_qc_toxicity(id, as_vector = TRUE, id_only = FALSE)
  
  expect_named(result)
  expect_vector(result, character(0))
})

test_that("get_qc_toxicity returns a named list if a single ID is given and 'as_vector' is FALSE", {
  id = rownames(agents)[1]
  result = get_qc_toxicity(id, as_vector = FALSE)
  
  expect_named(result)
  expect_type(result, "list")
})

test_that("get_qc_toxicity returns a named list if several IDs are given", {
  ids = rownames(agents)[1:5]
  result = get_qc_toxicity(ids)
  
  expect_named(result)
  expect_type(result, "list")
})

test_that("get_qc_toxicity returns a named list if no IDs are given", {
  # Note: this test assumes that there is more than one agent in the corresponding dataset
  result = get_qc_toxicity()
  
  expect_named(result)
  expect_type(result, "list")
})

test_that("get_qc_toxicity returns unnamed vectors if 'id_only' is TRUE", {
  # Note: this test assumes that there is more than one agent in the corresponding dataset
  result = get_qc_toxicity(id_only = TRUE)
  
  expect_named(result[[1]], NULL)
})

test_that("get_qc_toxicity returns named vectors if 'id_only' is FALSE", {
  # Note: this test assumes that there is more than one agent in the corresponding dataset
  result = get_qc_toxicity(id_only = FALSE)
  
  expect_named(result[[1]])
})

test_that("get_qc_toxicity extracts the associated toxicological classes in MiXie-QC for the given agents", {
  
  # Get the identifiers of one agent having no toxicity, one having only one
  # class and another one having several classes
  nb_classes = lengths(old_agent_data$mixie$toxicity_qc_ids)
  ids = c(old_agent_data$mixie$agent_id[nb_classes == 0][1],
          old_agent_data$mixie$agent_id[nb_classes == 1][1],
          old_agent_data$mixie$agent_id[nb_classes > 1][1])
  
  # Expected result if 'id_only' is TRUE
  expected_t = stats::setNames(
    list(
      old_agent_data$mixie$toxicity_qc_ids[old_agent_data$mixie$agent_id == ids[1]][[1]],
      old_agent_data$mixie$toxicity_qc_ids[old_agent_data$mixie$agent_id == ids[2]][[1]],
      old_agent_data$mixie$toxicity_qc_ids[old_agent_data$mixie$agent_id == ids[3]][[1]]
    ),
    ids
  )
  
  # Expected result if 'id_only' is FALSE
  expected_f = lapply(expected_t, function(tox_ids) {
    stats::setNames(
      uom.agents::toxicological_classes[tox_ids, "name"],
      tox_ids
    )
  })
  
  expect_identical(get_qc_toxicity(ids, id_only = TRUE),
                   expected_t)
  expect_identical(get_qc_toxicity(ids, id_only = FALSE),
                   expected_f)
})

test_that("get_qc_toxicity extracts toxicological classes in MiXie-QC of all agents if none is given", {
  # Note: this test assumes that there is more than one agent in the corresponding dataset
  result = get_qc_toxicity()
  
  # Only compare the numbers of values obtained and expected
  # (the validation of extracted values is performed by another test)
  expect_identical(
    lengths(result),
    stats::setNames(
      sapply(as.matrix(table(mixie_qc)[agents$mixie_qc_substance])[, 1],
             function(x) { if (is.na(x)) return(0L); return(x) }),
      rownames(agents)
    )
  )
})


##### get_fr_toxicity #####

test_that("get_fr_toxicity returns an unnamed vector if a single ID is given, 'as_vector' is TRUE and 'id_only' is TRUE", {
  id = rownames(agents)[1]
  result = get_fr_toxicity(id, as_vector = TRUE, id_only = TRUE)
  
  expect_named(result, NULL)
  expect_vector(result, character(0))
})

test_that("get_fr_toxicity returns a named vector if a single ID is given, 'as_vector' is TRUE and 'id_only' is FALSE", {
  id = rownames(agents)[1]
  result = get_fr_toxicity(id, as_vector = TRUE, id_only = FALSE)
  
  expect_named(result)
  expect_vector(result, character(0))
})

test_that("get_fr_toxicity returns a named list if a single ID is given and 'as_vector' is FALSE", {
  id = rownames(agents)[1]
  result = get_fr_toxicity(id, as_vector = FALSE)
  
  expect_named(result)
  expect_type(result, "list")
})

test_that("get_fr_toxicity returns a named list if several IDs are given", {
  ids = rownames(agents)[1:5]
  result = get_fr_toxicity(ids)
  
  expect_named(result)
  expect_type(result, "list")
})

test_that("get_fr_toxicity returns a named list if no IDs are given", {
  # Note: this test assumes that there is more than one agent in the corresponding dataset
  result = get_fr_toxicity()
  
  expect_named(result)
  expect_type(result, "list")
})

test_that("get_fr_toxicity returns unnamed vectors 'id_only' is TRUE", {
  # Note: this test assumes that there is more than one agent in the corresponding dataset
  result = get_fr_toxicity(id_only = TRUE)
  
  expect_named(result[[1]], NULL)
})

test_that("get_fr_toxicity returns named vectors 'id_only' is FALSE", {
  # Note: this test assumes that there is more than one agent in the corresponding dataset
  result = get_fr_toxicity(id_only = FALSE)
  
  expect_named(result[[1]])
})

test_that("get_fr_toxicity extracts the associated toxicological classes in MiXie-FR for the given agents", {
  
  # Get the identifiers of one agent having no toxicity, one having only one
  # class and another one having several classes
  nb_classes = lengths(old_agent_data$mixie$toxicity_fr_ids)
  ids = c(old_agent_data$mixie$agent_id[nb_classes == 0][1],
          old_agent_data$mixie$agent_id[nb_classes == 1][1],
          old_agent_data$mixie$agent_id[nb_classes > 1][1])
  
  # Expected result if 'id_only' is TRUE
  expected_t = stats::setNames(
    list(
      old_agent_data$mixie$toxicity_fr_ids[old_agent_data$mixie$agent_id == ids[1]][[1]],
      old_agent_data$mixie$toxicity_fr_ids[old_agent_data$mixie$agent_id == ids[2]][[1]],
      old_agent_data$mixie$toxicity_fr_ids[old_agent_data$mixie$agent_id == ids[3]][[1]]
    ),
    ids
  )
  expected_f = lapply(expected_t, function(tox_ids) {
    stats::setNames(
      uom.agents::toxicological_classes[
        match(tox_ids, uom.agents::toxicological_classes[, "mixie_fr_id"]), 
        "name"],
      tox_ids
    )
  })
  
  expect_identical(get_fr_toxicity(ids, id_only = TRUE),
                   expected_t)
  expect_identical(get_fr_toxicity(ids, id_only = FALSE),
                   expected_f)
})

test_that("get_fr_toxicity extracts toxicological classes in MiXie-FR of all agents if none is given", {
  # Note: this test assumes that there is more than one agent in the corresponding dataset
  result = get_fr_toxicity()
  
  # Only compare the numbers of values obtained and expected
  # (the validation of extracted values is performed by another test)
  expect_identical(
    lengths(result),
    stats::setNames(
      sapply(as.matrix(table(mixie_fr)[get_fr_substance(rownames(agents))])[, 1],
             function(x) { if (is.na(x)) return(0L); return(x) }),
      rownames(agents)
    )
  )
})
