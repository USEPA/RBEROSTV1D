# # RBEROST Batch Watershed Analysis Wrapper
# # This script runs RBEROST for each watershed individually and submits to NEOS

# Load required packages
library(stringr)

InPath <- paste0(working_dir, "RBEROST-Northeast/Preprocessing/Inputs/")
OutPath <- paste0(working_dir, "RBEROST-Northeast/Preprocessing/Outputs/")
BaseOutPath <- OutPath  # Preserve original OutPath to prevent nesting issues
NEOSresults <- OutPath  # Use OutPath as base for NEOS results

# Filter watersheds to process
watersheds_to_process <- setdiff(all_watersheds, exclude_watersheds)

# Functions to extract solution status and cost from NEOS results
# These functions are based on the working postprocessor functions
get_batch_solvestatus <- function(results_astable) {
  
  solved_txt <- "solve_result = "
  # Use str_detect from stringr package (same as Shiny app)
  solved_rows <- results_astable[which(str_detect(results_astable[, 1], solved_txt)), 1]
  
  if(length(solved_rows) > 0) {
    solved_tmp <- strsplit(solved_rows, "\\s+")
    
    solved_tmp2 <- unlist(
      mapply(
        "[[", 
        solved_tmp, 
        index = mapply(
          which, 
          x = lapply(
            FUN = function(x, pattern) {x == pattern}, 
            X = solved_tmp, 
            pattern = "solve_result"
          )
        ) + 2
      ), use.names = FALSE
    )
    
    if ("solved" %in% solved_tmp2) {
      return("solved")
    } else if ("infeasible" %in% solved_tmp2) {
      return("infeasible")
    } else {
      return("unknown")
    }
  } else if(any(grepl("Error", results_astable[, 1]))){
    return("error")
  } else if (!any(grepl("solve_result", results_astable[, 1]))) {
    return("no_result")
  } else {
    return("unknown")
  }
}

get_batch_total_cost <- function(results_astable, scenarionumber = 1) {
  
  cost_txt <- "cost = "
  # Use str_detect from stringr package (same as Shiny app)
  cost_rows <- results_astable[which(str_detect(results_astable[, 1], cost_txt)), 1]
  
  if(length(cost_rows) >= scenarionumber) {
    cost_row <- cost_rows[scenarionumber]
    cost_tmp <- strsplit(cost_row, " ")
    cost_tmp2 <- unlist(
      suppressWarnings(as.numeric(as.character(lapply(cost_tmp, "[[", 3)))), 
      use.names = FALSE
    )
    total_cost <- cost_tmp2[which(!is.na(cost_tmp2))]
    
    if(length(total_cost) > 0) {
      return(total_cost[1])
    } else {
      return(NA)
    }
  } else {
    return(NA)
  }
}

# Function to extract error messages from NEOS results
get_batch_error_summary <- function(results_astable) {
  
  error_lines <- which(grepl(c("Error|error"), results_astable[, 1]))
  
  if(length(error_lines) > 0) {
    start_line <- min(error_lines)
    
    # Find where the actual results begin (after the errors)
    results_start <- which(grepl("solve_result|cost|point_dec|urban_frac|road_frac|ag_frac", results_astable[, 1]))
    
    if(length(results_start) > 0) {
      end_line <- min(results_start) - 1
    } else {
      end_line <- min(start_line + 10, nrow(results_astable))  # Limit to 10 lines
    }
    
    # Get error text
    error_text <- results_astable[start_line:end_line, 1]
    
    # Return first few lines as summary (limit to first 5 error lines)
    error_summary <- paste(head(error_text, 5), collapse = " | ")
    return(error_summary)
  } else {
    return("")
  }
}

# Create results tracking dataframe with additional columns
results_log <- data.frame(
  watershed = character(),
  status = character(),
  solve_status = character(),
  total_cost = numeric(),
  preprocessing_time = numeric(),
  neos_time = numeric(),
  total_time = numeric(),
  error_location = character(),
  error_message = character(),
  error_trace = character(),
  stringsAsFactors = FALSE
)


# Function to run single watershed analysis
# run_watershed_analysis <- function(watershed_name, working_dir = "C:/RBEROST/Tier_1_Optimization-SSWR.5.3.2/") {
run_watershed_analysis <- function(watershed_name, working_dir) {
    
  cat("\n", paste(rep("=", 80), collapse = ""), "\n")
  cat("Processing watershed:", watershed_name, "\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")
  
  start_time <- Sys.time()
  preprocessing_time <- NA
  neos_time <- NA
  status <- "Failed"
  solve_status <- "not_analyzed"
  total_cost <- NA
  error_message <- ""
  error_location <- ""
  error_trace <- ""
  
  tryCatch({
    
    # Preserve the original OutPath for consistent base directory
    original_outpath <- BaseOutPath  # Use BaseOutPath instead of potentially modified OutPath
    base_batch_dir <- paste0(original_outpath, "Batch Results/")
    
    # Create watershed-specific output directory
    watershed_clean <- gsub("[^A-Za-z0-9]", "_", watershed_name)
    watershed_output_dir <- paste0(base_batch_dir, watershed_clean, "/")
    
    # Create directory if it doesn't exist
    if (!dir.exists(watershed_output_dir)) {
      dir.create(watershed_output_dir, recursive = TRUE)
      cat("  Created output directory:", watershed_output_dir, "\n")
    }
    
    # Set watershed-specific parameters in global environment
    assign("MODE", "Select", envir = .GlobalEnv)
    assign("watershed_choices", watershed_name, envir = .GlobalEnv)
    assign("paramname", paste0(watershed_clean, "_", Sys.Date()), envir = .GlobalEnv)
    
    # Override OutPath and NEOSresults to point to watershed-specific directory
    assign("OutPath", watershed_output_dir, envir = .GlobalEnv)
    assign("NEOSresults", watershed_output_dir, envir = .GlobalEnv)
    
    cat("Step 1: Running preprocessing for", watershed_name, "...\n")
    cat("  Output directory:", watershed_output_dir, "\n")
    preprocessing_start <- Sys.time()
    
    # Ensure we're in the correct working directory
    setwd(working_dir)
    
    # Enhanced error handling for preprocessing with rlang tracing
    preprocessing_result <- tryCatch({
      
      cat("  1.1: Loading preprocessing gateway...\n")
      # Suppress any automatic error display messages
      suppressMessages(suppressWarnings({
        source("./RBEROST-Northeast/R/01_Optimization_Preprocessing_gateway-Northeast.R")
      }))
      list(success = TRUE)
      
    }, error = function(e) {
      # Capture error information and return it
      loc <- "Preprocessing"
      msg <- paste("Preprocessing error:", e$message)
      trace <- ""
      
      # Capture detailed trace information using rlang - only if there's actually an error
      trace_info <- tryCatch(rlang::last_trace(), error = function(e) NULL)
      if (!is.null(trace_info)) {
        trace <- paste(capture.output(print(trace_info)), collapse = "\n")
        
        # Extract the most relevant trace information
        if (length(trace_info$call) > 0) {
          # Get the last few calls for context
          relevant_calls <- tail(trace_info$call, 3)
          loc <- paste("Preprocessing -", paste(relevant_calls, collapse = " -> "))
        }
      }
      
      cat("PREPROCESSING ERROR:\n")
      cat("  Message:", e$message, "\n")
      cat("  Location:", loc, "\n")
      
      # Print condensed trace (first few lines only)
      if (trace != "") {
        trace_lines <- strsplit(trace, "\n")[[1]]
        cat("  Trace (first 5 lines):\n")
        for (i in seq_len(min(5, length(trace_lines)))) {
          cat("    ", trace_lines[i], "\n")
        }
      }
      
      # Return error information
      list(success = FALSE, location = loc, message = msg, trace = trace)
    })
    
    # Check if preprocessing failed
    if (!preprocessing_result$success) {
      error_location <- preprocessing_result$location
      error_message <- preprocessing_result$message
      error_trace <- preprocessing_result$trace
      stop(error_message)
    }
    
    preprocessing_end <- Sys.time()
    preprocessing_time <- as.numeric(difftime(preprocessing_end, preprocessing_start, units = "mins"))
    
    cat("Preprocessing completed in", round(preprocessing_time, 2), "minutes\n")
    
    # Check if AMPL files were created successfully in watershed-specific directory
    model_file <- paste0(watershed_output_dir, "STmodel_seasonal.mod")
    data_file <- paste0(watershed_output_dir, "STdata_seasonal.dat")
    
    cat("  1.2: Checking AMPL file generation...\n")
    cat("    Model file path:", model_file, "\n")
    cat("    Data file path:", data_file, "\n")
    
    if (!file.exists(model_file)) {
      error_location <<- "AMPL Model File Generation"
      stop(paste("AMPL model file not created:", model_file))
    }
    if (!file.exists(data_file)) {
      error_location <<- "AMPL Data File Generation"
      stop(paste("AMPL data file not created:", data_file))
    }
    
    # Additional file validation
    model_size <- file.info(model_file)$size
    data_size <- file.info(data_file)$size
    
    if (is.na(model_size) || model_size == 0) {
      error_location <<- "AMPL Model File Validation"
      stop("AMPL model file is empty or corrupted")
    }
    if (is.na(data_size) || data_size == 0) {
      error_location <<- "AMPL Data File Validation"
      stop("AMPL data file is empty or corrupted")
    }
    
    cat("  1.3: AMPL files validated successfully\n")
    cat("    Model file size:", round(model_size/1024, 2), "KB\n")
    cat("    Data file size:", round(data_size/1024, 2), "KB\n")
    
    cat("Step 2: Submitting to NEOS server...\n")
    cat("  NEOS parameters set:\n")
    cat("    NEOSresults:", get("NEOSresults", envir = .GlobalEnv), "\n")
    cat("    paramname:", get("paramname", envir = .GlobalEnv), "\n")
    cat("    Expected result file:", paste0(get("NEOSresults", envir = .GlobalEnv), "NEOSresult_", get("paramname", envir = .GlobalEnv), ".txt"), "\n")
    neos_start <- Sys.time()
    
    # Enhanced error handling for NEOS submission
    neos_result <- tryCatch({
      
      source("./RBEROST-Northeast/R/01_Optimization_NEOSInteraction-Northeast.R")
      list(success = TRUE)
      
    }, error = function(e) {
      loc <- "NEOS Submission"
      msg <- paste("NEOS submission error:", e$message)
      trace <- ""
      
      # Capture NEOS-specific trace - only if there's actually an error
      trace_info <- tryCatch(rlang::last_trace(), error = function(e) NULL)
      if (!is.null(trace_info)) {
        trace <- paste(capture.output(print(trace_info)), collapse = "\n")
      }
      
      cat("[ERROR] NEOS SUBMISSION ERROR:\n")
      cat("  Message:", e$message, "\n")
      cat("  Location:", loc, "\n")
      
      # Return error information
      list(success = FALSE, location = loc, message = msg, trace = trace)
    })
    
    # Check if NEOS submission failed
    if (!neos_result$success) {
      error_location <- neos_result$location
      error_message <- neos_result$message
      error_trace <- neos_result$trace
      stop(error_message)
    }
    
    neos_end <- Sys.time()
    neos_time <- as.numeric(difftime(neos_end, neos_start, units = "mins"))
    
    cat("NEOS submission completed in", round(neos_time, 2), "minutes\n")
    
    # Check if NEOS result file was created
    # Use the same paramname and NEOSresults as set for NEOS submission
    paramname_value <- get("paramname", envir = .GlobalEnv)
    neos_results_path <- get("NEOSresults", envir = .GlobalEnv)
    result_file <- paste0(neos_results_path, "NEOSresult_", paramname_value, ".txt")
    
    cat("  2.1: Checking NEOS result file:", basename(result_file), "\n")
    cat("    Result file path:", result_file, "\n")
    
    if (file.exists(result_file)) {
      result_size <- file.info(result_file)$size
      if (is.na(result_size) || result_size == 0) {
        error_location <<- "NEOS Result Validation"
        error_message <<- "NEOS result file is empty"
        status <<- "NEOS Failed"
        solve_status <<- "no_result"
      } else {
        status <- "Success"
        cat("[SUCCESS] Analysis completed successfully for", watershed_name, "\n")
        cat("  Result file size:", round(result_size/1024, 2), "KB\n")
        
        # Parse NEOS result file for solve status and cost
        cat("  2.2: Analyzing solution status and cost...\n")
        parse_result <- tryCatch({
          # Read the NEOS result file
          result_lines <- readLines(result_file, warn = FALSE)
          results_astable <- data.frame(V1 = result_lines, stringsAsFactors = FALSE)
          
          # Extract solve status
          solve_status_temp <- get_batch_solvestatus(results_astable)
          cat("    Solution status:", solve_status_temp, "\n")
          
          # Extract total cost - try for all statuses
          total_cost_temp <- get_batch_total_cost(results_astable, scenarionumber = 1)
          
          if (!is.na(total_cost_temp)) {
            cat("    Total cost: $", format(total_cost_temp, big.mark = ",", scientific = FALSE), "\n")
          } else {
            cat("    Total cost: Not available or could not extract\n")
          }
          
          # Check for errors regardless of solve status
          error_summary <- get_batch_error_summary(results_astable)
          error_loc_temp <- ""
          error_msg_temp <- ""
          
          if (error_summary != "") {
            # Update error information for logging
            error_msg_temp <- substr(error_summary, 1, 500)  # Store first 500 chars
            error_loc_temp <- "NEOS Solver"
            cat("    Errors detected in NEOS results\n")
            cat("    Error summary:", substr(error_summary, 1, 150), "...\n")
          }
          
          # Return parsed values
          list(success = TRUE, 
               solve_status = solve_status_temp, 
               total_cost = total_cost_temp,
               error_location = error_loc_temp,
               error_message = error_msg_temp)
          
        }, error = function(e) {
          cat("    Warning: Could not parse result file for solve status/cost:", e$message, "\n")
          list(success = FALSE,
               solve_status = "parse_error",
               total_cost = NA,
               error_location = "Result File Parsing",
               error_message = paste("Result parsing error:", e$message))
        })
        
        # Update variables from parsing result
        solve_status <- parse_result$solve_status
        total_cost <- parse_result$total_cost
        if (parse_result$error_location != "") {
          error_location <- parse_result$error_location
          error_message <- parse_result$error_message
        }
      }
    } else {
      error_location <<- "NEOS Result File"
      status <- "NEOS Failed"
      error_message <- "NEOS result file not created"
      solve_status <<- "no_result"
    }
    
  }, error = function(e) {
    # This catches any errors not handled by inner tryCatch blocks
    if (error_message == "") {
      error_message <- as.character(e$message)
      if (error_location == "") {
        error_location <- "Unknown Location"
      }
      
      # Get final trace if not already captured - only if there's actually an error
      if (error_trace == "") {
        trace_info <- tryCatch(rlang::last_trace(), error = function(e) NULL)
        if (!is.null(trace_info)) {
          error_trace <- paste(capture.output(print(trace_info)), collapse = "\n")
        }
      }
    }
    
    cat("✗ Error processing", watershed_name, "\n")
    cat("  Location:", error_location, "\n")
    cat("  Error:", error_message, "\n")
  })
  
  end_time <- Sys.time()
  total_time <- as.numeric(difftime(end_time, start_time, units = "mins"))
  
  # Restore the original paths to prevent nesting issues in subsequent watersheds
  assign("OutPath", BaseOutPath, envir = .GlobalEnv)
  assign("NEOSresults", BaseOutPath, envir = .GlobalEnv)
  
  # Return results with enhanced error information and solution analysis
  return(data.frame(
    watershed = watershed_name,
    status = status,
    solve_status = solve_status,
    total_cost = total_cost,
    preprocessing_time = preprocessing_time,
    neos_time = neos_time,
    total_time = total_time,
    error_location = error_location,
    error_message = error_message,
    error_trace = error_trace,
    stringsAsFactors = FALSE
  ))
}

# Main execution loop
cat("Starting batch processing of", length(watersheds_to_process), "watersheds\n")
cat("Excluded watersheds:", paste(exclude_watersheds, collapse = ", "), "\n\n")

batch_start_time <- Sys.time()

for (i in seq_along(watersheds_to_process)) {
  watershed <- watersheds_to_process[i]
  
  cat("Processing", i, "of", length(watersheds_to_process), "watersheds\n")
  
  # Run analysis for current watershed
  result <- run_watershed_analysis(watershed, working_dir)
  
  # Add to results log
  results_log <- rbind(results_log, result)
  
  # Save intermediate results to main batch results directory (use BaseOutPath to avoid nesting)
  batch_results_dir <- paste0(BaseOutPath, "Batch Results/")
  dir.create(batch_results_dir, recursive = TRUE, showWarnings = FALSE)
  write.csv(results_log, paste0(batch_results_dir, "batch_results_log.csv"), row.names = FALSE)
  
  # Brief pause between watersheds
  Sys.sleep(2)
}

batch_end_time <- Sys.time()
total_batch_time <- as.numeric(difftime(batch_end_time, batch_start_time, units = "hours"))

# Generate summary report
cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("BATCH PROCESSING COMPLETE\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("Total processing time:", round(total_batch_time, 2), "hours\n")
cat("Watersheds processed:", nrow(results_log), "\n")
cat("Successful runs:", sum(results_log$status == "Success"), "\n")
cat("Failed runs:", sum(results_log$status != "Success"), "\n\n")

# Print detailed results
print(results_log)

# Save final results to main batch results directory (use BaseOutPath to avoid nesting)
batch_results_dir <- paste0(BaseOutPath, "Batch Results/")
write.csv(results_log, paste0(batch_results_dir, "final_batch_results_", Sys.Date(), ".csv"), row.names = FALSE)

# Summary statistics
summary_stats <- results_log %>%
  summarise(
    total_watersheds = n(),
    successful = sum(status == "Success"),
    failed = sum(status != "Success"),
    success_rate = round(successful/total_watersheds * 100, 1),
    solved_models = sum(solve_status == "solved", na.rm = TRUE),
    infeasible_models = sum(solve_status == "infeasible", na.rm = TRUE),
    avg_preprocessing_time = round(mean(preprocessing_time, na.rm = TRUE), 2),
    avg_neos_time = round(mean(neos_time, na.rm = TRUE), 2),
    avg_total_time = round(mean(total_time, na.rm = TRUE), 2),
    avg_cost = round(mean(total_cost, na.rm = TRUE), 2),
    min_cost = round(min(total_cost, na.rm = TRUE), 2),
    max_cost = round(max(total_cost, na.rm = TRUE), 2)
  )

cat("\nSUMMARY STATISTICS:\n")
print(summary_stats)

# Solution status breakdown
cat("\nSOLUTION STATUS BREAKDOWN:\n")
solve_status_summary <- table(results_log$solve_status)
print(solve_status_summary)

# Cost summary for solved models
solved_models <- results_log[results_log$solve_status == "solved" & !is.na(results_log$total_cost), ]
if (nrow(solved_models) > 0) {
  cat("\nCOST SUMMARY FOR SOLVED MODELS:\n")
  cat("Number of models with cost data:", nrow(solved_models), "\n")
  cat("Average cost: $", format(mean(solved_models$total_cost), big.mark = ",", scientific = FALSE), "\n")
  cat("Minimum cost: $", format(min(solved_models$total_cost), big.mark = ",", scientific = FALSE), "\n")
  cat("Maximum cost: $", format(max(solved_models$total_cost), big.mark = ",", scientific = FALSE), "\n")
  cat("Total cost across all watersheds: $", format(sum(solved_models$total_cost), big.mark = ",", scientific = FALSE), "\n")
} else {
  cat("\nNo solved models with cost data found.\n")
}

# Additional cost summary for all models with costs (including infeasible)
all_models_with_cost <- results_log[!is.na(results_log$total_cost), ]
if (nrow(all_models_with_cost) > 0 && nrow(all_models_with_cost) > nrow(solved_models)) {
  cat("\nNOTE: Some models have cost values despite not being optimally solved:\n")
  nonsolved_with_cost <- all_models_with_cost[all_models_with_cost$solve_status != "solved", ]
  if (nrow(nonsolved_with_cost) > 0) {
    for (i in 1:nrow(nonsolved_with_cost)) {
      row <- nonsolved_with_cost[i, ]
      cat("  -", row$watershed, ": Status =", row$solve_status, ", Cost = $", 
          format(row$total_cost, big.mark = ",", scientific = FALSE), "\n")
    }
  }
}

# Successful watersheds summary
successful_watersheds <- results_log$watershed[results_log$status == "Success"]
if (length(successful_watersheds) > 0) {
  cat("\nSUCCESSFUL WATERSHEDS SUMMARY:\n")
  cat(paste(rep("-", 50), collapse = ""), "\n")
  
  for (watershed in successful_watersheds) {
    row <- results_log[results_log$watershed == watershed, ]
    cat("Watershed:", watershed, "\n")
    cat("  Solution Status:", row$solve_status, "\n")
    if (!is.na(row$total_cost)) {
      cat("  Total Cost: $", format(row$total_cost, big.mark = ",", scientific = FALSE), "\n")
    } else {
      cat("  Total Cost: N/A\n")
    }
    cat("  Processing Time:", round(row$total_time, 2), "minutes\n")
    
    # Show any errors even for successful runs
    if (!is.na(row$error_message) && row$error_message != "") {
      cat("  Warning/Errors:", substr(row$error_message, 1, 100), "\n")
    }
    
    cat(paste(rep("-", 30), collapse = ""), "\n")
  }
}

# Enhanced summary report for failed watersheds
failed_watersheds <- results_log$watershed[results_log$status != "Success"]
if (length(failed_watersheds) > 0) {
  cat("\nFAILED WATERSHEDS DETAILED REPORT:\n")
  cat(paste(rep("-", 50), collapse = ""), "\n")
  
  for (watershed in failed_watersheds) {
    row <- results_log[results_log$watershed == watershed, ]
    cat("Watershed:", watershed, "\n")
    cat("  Status:", row$status, "\n")
    cat("  Solution Status:", row$solve_status, "\n")
    cat("  Error Location:", row$error_location, "\n")
    cat("  Error Message:", row$error_message, "\n")
    cat("  Processing Time:", round(row$total_time, 2), "minutes\n")
    
    # Show trace if available
    if (!is.na(row$error_trace) && row$error_trace != "") {
      cat("  Detailed Trace:\n")
      trace_lines <- strsplit(row$error_trace, "\n")[[1]]
      for (i in seq_len(min(3, length(trace_lines)))) {
        cat("    ", trace_lines[i], "\n")
      }
    }
    cat(paste(rep("-", 30), collapse = ""), "\n")
  }
}

beepr::beep(3)
batch_results_dir <- paste0(BaseOutPath, "Batch Results/")
cat("\nBatch processing complete!\n")
cat("Summary results: ", paste0(batch_results_dir, "final_batch_results_", Sys.Date(), ".csv"), "\n")
cat("Individual watershed files are saved in subfolders within: ", batch_results_dir, "\n")

# # RBEROST Batch Watershed Analysis Wrapper
# # This script runs RBEROST for each watershed individually and submits to NEOS

# Load required packages
library(stringr)

InPath <- paste0(working_dir, "RBEROST-Northeast/Preprocessing/Inputs/")
OutPath <- paste0(working_dir, "RBEROST-Northeast/Preprocessing/Outputs/")
BaseOutPath <- OutPath  # Preserve original OutPath to prevent nesting issues
NEOSresults <- OutPath  # Use OutPath as base for NEOS results

# Filter watersheds to process
watersheds_to_process <- setdiff(all_watersheds, exclude_watersheds)

# Functions to extract solution status and cost from NEOS results
# These functions are based on the working postprocessor functions
get_batch_solvestatus <- function(results_astable) {
  
  solved_txt <- "solve_result = "
  # Use str_detect from stringr package (same as Shiny app)
  solved_rows <- results_astable[which(str_detect(results_astable[, 1], solved_txt)), 1]
  
  if(length(solved_rows) > 0) {
    solved_tmp <- strsplit(solved_rows, "\\s+")
    
    solved_tmp2 <- unlist(
      mapply(
        "[[", 
        solved_tmp, 
        index = mapply(
          which, 
          x = lapply(
            FUN = function(x, pattern) {x == pattern}, 
            X = solved_tmp, 
            pattern = "solve_result"
          )
        ) + 2
      ), use.names = FALSE
    )
    
    if ("solved" %in% solved_tmp2) {
      return("solved")
    } else if ("infeasible" %in% solved_tmp2) {
      return("infeasible")
    } else {
      return("unknown")
    }
  } else if(any(grepl("Error", results_astable[, 1]))){
    return("error")
  } else if (!any(grepl("solve_result", results_astable[, 1]))) {
    return("no_result")
  } else {
    return("unknown")
  }
}

get_batch_total_cost <- function(results_astable, scenarionumber = 1) {
  
  cost_txt <- "cost = "
  # Use str_detect from stringr package (same as Shiny app)
  cost_rows <- results_astable[which(str_detect(results_astable[, 1], cost_txt)), 1]
  
  if(length(cost_rows) >= scenarionumber) {
    cost_row <- cost_rows[scenarionumber]
    cost_tmp <- strsplit(cost_row, " ")
    cost_tmp2 <- unlist(
      suppressWarnings(as.numeric(as.character(lapply(cost_tmp, "[[", 3)))), 
      use.names = FALSE
    )
    total_cost <- cost_tmp2[which(!is.na(cost_tmp2))]
    
    if(length(total_cost) > 0) {
      return(total_cost[1])
    } else {
      return(NA)
    }
  } else {
    return(NA)
  }
}

# Function to extract error messages from NEOS results
get_batch_error_summary <- function(results_astable) {
  
  error_lines <- which(grepl(c("Error|error"), results_astable[, 1]))
  
  if(length(error_lines) > 0) {
    start_line <- min(error_lines)
    
    # Find where the actual results begin (after the errors)
    results_start <- which(grepl("solve_result|cost|point_dec|urban_frac|road_frac|ag_frac", results_astable[, 1]))
    
    if(length(results_start) > 0) {
      end_line <- min(results_start) - 1
    } else {
      end_line <- min(start_line + 10, nrow(results_astable))  # Limit to 10 lines
    }
    
    # Get error text
    error_text <- results_astable[start_line:end_line, 1]
    
    # Return first few lines as summary (limit to first 5 error lines)
    error_summary <- paste(head(error_text, 5), collapse = " | ")
    return(error_summary)
  } else {
    return("")
  }
}

# Create results tracking dataframe with additional columns
results_log <- data.frame(
  watershed = character(),
  status = character(),
  solve_status = character(),
  total_cost = numeric(),
  preprocessing_time = numeric(),
  neos_time = numeric(),
  total_time = numeric(),
  error_location = character(),
  error_message = character(),
  error_trace = character(),
  stringsAsFactors = FALSE
)


# Function to run single watershed analysis
# run_watershed_analysis <- function(watershed_name, working_dir = "C:/RBEROST/Tier_1_Optimization-SSWR.5.3.2/") {
run_watershed_analysis <- function(watershed_name, working_dir) {
    
  cat("\n", paste(rep("=", 80), collapse = ""), "\n")
  cat("Processing watershed:", watershed_name, "\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")
  
  start_time <- Sys.time()
  preprocessing_time <- NA
  neos_time <- NA
  status <- "Failed"
  solve_status <- "not_analyzed"
  total_cost <- NA
  error_message <- ""
  error_location <- ""
  error_trace <- ""
  
  tryCatch({
    
    # Preserve the original OutPath for consistent base directory
    original_outpath <- BaseOutPath  # Use BaseOutPath instead of potentially modified OutPath
    base_batch_dir <- paste0(original_outpath, "Batch Results/")
    
    # Create watershed-specific output directory
    watershed_clean <- gsub("[^A-Za-z0-9]", "_", watershed_name)
    watershed_output_dir <- paste0(base_batch_dir, watershed_clean, "/")
    
    # Create directory if it doesn't exist
    if (!dir.exists(watershed_output_dir)) {
      dir.create(watershed_output_dir, recursive = TRUE)
      cat("  Created output directory:", watershed_output_dir, "\n")
    }
    
    # Set watershed-specific parameters in global environment
    assign("MODE", "Select", envir = .GlobalEnv)
    assign("watershed_choices", watershed_name, envir = .GlobalEnv)
    assign("paramname", paste0(watershed_clean, "_", Sys.Date()), envir = .GlobalEnv)
    
    # Override OutPath and NEOSresults to point to watershed-specific directory
    assign("OutPath", watershed_output_dir, envir = .GlobalEnv)
    assign("NEOSresults", watershed_output_dir, envir = .GlobalEnv)
    
    cat("Step 1: Running preprocessing for", watershed_name, "...\n")
    cat("  Output directory:", watershed_output_dir, "\n")
    preprocessing_start <- Sys.time()
    
    # Ensure we're in the correct working directory
    setwd(working_dir)
    
    # Enhanced error handling for preprocessing with rlang tracing
    preprocessing_result <- tryCatch({
      
      cat("  1.1: Loading preprocessing gateway...\n")
      # Suppress any automatic error display messages
      suppressMessages(suppressWarnings({
        source("./RBEROST-Northeast/R/01_Optimization_Preprocessing_gateway-Northeast.R")
      }))
      list(success = TRUE)
      
    }, error = function(e) {
      # Capture error information and return it
      loc <- "Preprocessing"
      msg <- paste("Preprocessing error:", e$message)
      trace <- ""
      
      # Capture detailed trace information using rlang - only if there's actually an error
      trace_info <- tryCatch(rlang::last_trace(), error = function(e) NULL)
      if (!is.null(trace_info)) {
        trace <- paste(capture.output(print(trace_info)), collapse = "\n")
        
        # Extract the most relevant trace information
        if (length(trace_info$call) > 0) {
          # Get the last few calls for context
          relevant_calls <- tail(trace_info$call, 3)
          loc <- paste("Preprocessing -", paste(relevant_calls, collapse = " -> "))
        }
      }
      
      cat("PREPROCESSING ERROR:\n")
      cat("  Message:", e$message, "\n")
      cat("  Location:", loc, "\n")
      
      # Print condensed trace (first few lines only)
      if (trace != "") {
        trace_lines <- strsplit(trace, "\n")[[1]]
        cat("  Trace (first 5 lines):\n")
        for (i in seq_len(min(5, length(trace_lines)))) {
          cat("    ", trace_lines[i], "\n")
        }
      }
      
      # Return error information
      list(success = FALSE, location = loc, message = msg, trace = trace)
    })
    
    # Check if preprocessing failed
    if (!preprocessing_result$success) {
      error_location <- preprocessing_result$location
      error_message <- preprocessing_result$message
      error_trace <- preprocessing_result$trace
      stop(error_message)
    }
    
    preprocessing_end <- Sys.time()
    preprocessing_time <- as.numeric(difftime(preprocessing_end, preprocessing_start, units = "mins"))
    
    cat("Preprocessing completed in", round(preprocessing_time, 2), "minutes\n")
    
    # Check if AMPL files were created successfully in watershed-specific directory
    model_file <- paste0(watershed_output_dir, "STmodel_seasonal.mod")
    data_file <- paste0(watershed_output_dir, "STdata_seasonal.dat")
    
    cat("  1.2: Checking AMPL file generation...\n")
    cat("    Model file path:", model_file, "\n")
    cat("    Data file path:", data_file, "\n")
    
    if (!file.exists(model_file)) {
      error_location <<- "AMPL Model File Generation"
      stop(paste("AMPL model file not created:", model_file))
    }
    if (!file.exists(data_file)) {
      error_location <<- "AMPL Data File Generation"
      stop(paste("AMPL data file not created:", data_file))
    }
    
    # Additional file validation
    model_size <- file.info(model_file)$size
    data_size <- file.info(data_file)$size
    
    if (is.na(model_size) || model_size == 0) {
      error_location <<- "AMPL Model File Validation"
      stop("AMPL model file is empty or corrupted")
    }
    if (is.na(data_size) || data_size == 0) {
      error_location <<- "AMPL Data File Validation"
      stop("AMPL data file is empty or corrupted")
    }
    
    cat("  1.3: AMPL files validated successfully\n")
    cat("    Model file size:", round(model_size/1024, 2), "KB\n")
    cat("    Data file size:", round(data_size/1024, 2), "KB\n")
    
    cat("Step 2: Submitting to NEOS server...\n")
    cat("  NEOS parameters set:\n")
    cat("    NEOSresults:", get("NEOSresults", envir = .GlobalEnv), "\n")
    cat("    paramname:", get("paramname", envir = .GlobalEnv), "\n")
    cat("    Expected result file:", paste0(get("NEOSresults", envir = .GlobalEnv), "NEOSresult_", get("paramname", envir = .GlobalEnv), ".txt"), "\n")
    neos_start <- Sys.time()
    
    # Enhanced error handling for NEOS submission
    neos_result <- tryCatch({
      
      source("./RBEROST-Northeast/R/01_Optimization_NEOSInteraction-Northeast.R")
      list(success = TRUE)
      
    }, error = function(e) {
      loc <- "NEOS Submission"
      msg <- paste("NEOS submission error:", e$message)
      trace <- ""
      
      # Capture NEOS-specific trace - only if there's actually an error
      trace_info <- tryCatch(rlang::last_trace(), error = function(e) NULL)
      if (!is.null(trace_info)) {
        trace <- paste(capture.output(print(trace_info)), collapse = "\n")
      }
      
      cat("[ERROR] NEOS SUBMISSION ERROR:\n")
      cat("  Message:", e$message, "\n")
      cat("  Location:", loc, "\n")
      
      # Return error information
      list(success = FALSE, location = loc, message = msg, trace = trace)
    })
    
    # Check if NEOS submission failed
    if (!neos_result$success) {
      error_location <- neos_result$location
      error_message <- neos_result$message
      error_trace <- neos_result$trace
      stop(error_message)
    }
    
    neos_end <- Sys.time()
    neos_time <- as.numeric(difftime(neos_end, neos_start, units = "mins"))
    
    cat("NEOS submission completed in", round(neos_time, 2), "minutes\n")
    
    # Check if NEOS result file was created
    # Use the same paramname and NEOSresults as set for NEOS submission
    paramname_value <- get("paramname", envir = .GlobalEnv)
    neos_results_path <- get("NEOSresults", envir = .GlobalEnv)
    result_file <- paste0(neos_results_path, "NEOSresult_", paramname_value, ".txt")
    
    cat("  2.1: Checking NEOS result file:", basename(result_file), "\n")
    cat("    Result file path:", result_file, "\n")
    
    if (file.exists(result_file)) {
      result_size <- file.info(result_file)$size
      if (is.na(result_size) || result_size == 0) {
        error_location <<- "NEOS Result Validation"
        error_message <<- "NEOS result file is empty"
        status <<- "NEOS Failed"
        solve_status <<- "no_result"
      } else {
        status <- "Success"
        cat("[SUCCESS] Analysis completed successfully for", watershed_name, "\n")
        cat("  Result file size:", round(result_size/1024, 2), "KB\n")
        
        # Parse NEOS result file for solve status and cost
        cat("  2.2: Analyzing solution status and cost...\n")
        parse_result <- tryCatch({
          # Read the NEOS result file
          result_lines <- readLines(result_file, warn = FALSE)
          results_astable <- data.frame(V1 = result_lines, stringsAsFactors = FALSE)
          
          # Extract solve status
          solve_status_temp <- get_batch_solvestatus(results_astable)
          cat("    Solution status:", solve_status_temp, "\n")
          
          # Extract total cost - try for all statuses
          total_cost_temp <- get_batch_total_cost(results_astable, scenarionumber = 1)
          
          if (!is.na(total_cost_temp)) {
            cat("    Total cost: $", format(total_cost_temp, big.mark = ",", scientific = FALSE), "\n")
          } else {
            cat("    Total cost: Not available or could not extract\n")
          }
          
          # Check for errors regardless of solve status
          error_summary <- get_batch_error_summary(results_astable)
          error_loc_temp <- ""
          error_msg_temp <- ""
          
          if (error_summary != "") {
            # Update error information for logging
            error_msg_temp <- substr(error_summary, 1, 500)  # Store first 500 chars
            error_loc_temp <- "NEOS Solver"
            cat("    Errors detected in NEOS results\n")
            cat("    Error summary:", substr(error_summary, 1, 150), "...\n")
          }
          
          # Return parsed values
          list(success = TRUE, 
               solve_status = solve_status_temp, 
               total_cost = total_cost_temp,
               error_location = error_loc_temp,
               error_message = error_msg_temp)
          
        }, error = function(e) {
          cat("    Warning: Could not parse result file for solve status/cost:", e$message, "\n")
          list(success = FALSE,
               solve_status = "parse_error",
               total_cost = NA,
               error_location = "Result File Parsing",
               error_message = paste("Result parsing error:", e$message))
        })
        
        # Update variables from parsing result
        solve_status <- parse_result$solve_status
        total_cost <- parse_result$total_cost
        if (parse_result$error_location != "") {
          error_location <- parse_result$error_location
          error_message <- parse_result$error_message
        }
      }
    } else {
      error_location <<- "NEOS Result File"
      status <- "NEOS Failed"
      error_message <- "NEOS result file not created"
      solve_status <<- "no_result"
    }
    
  }, error = function(e) {
    # This catches any errors not handled by inner tryCatch blocks
    if (error_message == "") {
      error_message <- as.character(e$message)
      if (error_location == "") {
        error_location <- "Unknown Location"
      }
      
      # Get final trace if not already captured - only if there's actually an error
      if (error_trace == "") {
        trace_info <- tryCatch(rlang::last_trace(), error = function(e) NULL)
        if (!is.null(trace_info)) {
          error_trace <- paste(capture.output(print(trace_info)), collapse = "\n")
        }
      }
    }
    
    cat("✗ Error processing", watershed_name, "\n")
    cat("  Location:", error_location, "\n")
    cat("  Error:", error_message, "\n")
  })
  
  end_time <- Sys.time()
  total_time <- as.numeric(difftime(end_time, start_time, units = "mins"))
  
  # Restore the original paths to prevent nesting issues in subsequent watersheds
  assign("OutPath", BaseOutPath, envir = .GlobalEnv)
  assign("NEOSresults", BaseOutPath, envir = .GlobalEnv)
  
  # Return results with enhanced error information and solution analysis
  return(data.frame(
    watershed = watershed_name,
    status = status,
    solve_status = solve_status,
    total_cost = total_cost,
    preprocessing_time = preprocessing_time,
    neos_time = neos_time,
    total_time = total_time,
    error_location = error_location,
    error_message = error_message,
    error_trace = error_trace,
    stringsAsFactors = FALSE
  ))
}

# Main execution loop
cat("Starting batch processing of", length(watersheds_to_process), "watersheds\n")
cat("Excluded watersheds:", paste(exclude_watersheds, collapse = ", "), "\n\n")

batch_start_time <- Sys.time()

for (i in seq_along(watersheds_to_process)) {
  watershed <- watersheds_to_process[i]
  
  cat("Processing", i, "of", length(watersheds_to_process), "watersheds\n")
  
  # Run analysis for current watershed
  result <- run_watershed_analysis(watershed, working_dir)
  
  # Add to results log
  results_log <- rbind(results_log, result)
  
  # Save intermediate results to main batch results directory (use BaseOutPath to avoid nesting)
  batch_results_dir <- paste0(BaseOutPath, "Batch Results/")
  dir.create(batch_results_dir, recursive = TRUE, showWarnings = FALSE)
  write.csv(results_log, paste0(batch_results_dir, "batch_results_log.csv"), row.names = FALSE)
  
  # Brief pause between watersheds
  Sys.sleep(2)
}

batch_end_time <- Sys.time()
total_batch_time <- as.numeric(difftime(batch_end_time, batch_start_time, units = "hours"))

# Generate summary report
cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("BATCH PROCESSING COMPLETE\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("Total processing time:", round(total_batch_time, 2), "hours\n")
cat("Watersheds processed:", nrow(results_log), "\n")
cat("Successful runs:", sum(results_log$status == "Success"), "\n")
cat("Failed runs:", sum(results_log$status != "Success"), "\n\n")

# Print detailed results
print(results_log)

# Save final results to main batch results directory (use BaseOutPath to avoid nesting)
batch_results_dir <- paste0(BaseOutPath, "Batch Results/")
write.csv(results_log, paste0(batch_results_dir, "final_batch_results_", Sys.Date(), ".csv"), row.names = FALSE)

# Summary statistics
summary_stats <- results_log %>%
  summarise(
    total_watersheds = n(),
    successful = sum(status == "Success"),
    failed = sum(status != "Success"),
    success_rate = round(successful/total_watersheds * 100, 1),
    solved_models = sum(solve_status == "solved", na.rm = TRUE),
    infeasible_models = sum(solve_status == "infeasible", na.rm = TRUE),
    avg_preprocessing_time = round(mean(preprocessing_time, na.rm = TRUE), 2),
    avg_neos_time = round(mean(neos_time, na.rm = TRUE), 2),
    avg_total_time = round(mean(total_time, na.rm = TRUE), 2),
    avg_cost = round(mean(total_cost, na.rm = TRUE), 2),
    min_cost = round(min(total_cost, na.rm = TRUE), 2),
    max_cost = round(max(total_cost, na.rm = TRUE), 2)
  )

cat("\nSUMMARY STATISTICS:\n")
print(summary_stats)

# Solution status breakdown
cat("\nSOLUTION STATUS BREAKDOWN:\n")
solve_status_summary <- table(results_log$solve_status)
print(solve_status_summary)

# Cost summary for solved models
solved_models <- results_log[results_log$solve_status == "solved" & !is.na(results_log$total_cost), ]
if (nrow(solved_models) > 0) {
  cat("\nCOST SUMMARY FOR SOLVED MODELS:\n")
  cat("Number of models with cost data:", nrow(solved_models), "\n")
  cat("Average cost: $", format(mean(solved_models$total_cost), big.mark = ",", scientific = FALSE), "\n")
  cat("Minimum cost: $", format(min(solved_models$total_cost), big.mark = ",", scientific = FALSE), "\n")
  cat("Maximum cost: $", format(max(solved_models$total_cost), big.mark = ",", scientific = FALSE), "\n")
  cat("Total cost across all watersheds: $", format(sum(solved_models$total_cost), big.mark = ",", scientific = FALSE), "\n")
} else {
  cat("\nNo solved models with cost data found.\n")
}

# Additional cost summary for all models with costs (including infeasible)
all_models_with_cost <- results_log[!is.na(results_log$total_cost), ]
if (nrow(all_models_with_cost) > 0 && nrow(all_models_with_cost) > nrow(solved_models)) {
  cat("\nNOTE: Some models have cost values despite not being optimally solved:\n")
  nonsolved_with_cost <- all_models_with_cost[all_models_with_cost$solve_status != "solved", ]
  if (nrow(nonsolved_with_cost) > 0) {
    for (i in 1:nrow(nonsolved_with_cost)) {
      row <- nonsolved_with_cost[i, ]
      cat("  -", row$watershed, ": Status =", row$solve_status, ", Cost = $", 
          format(row$total_cost, big.mark = ",", scientific = FALSE), "\n")
    }
  }
}

# Successful watersheds summary
successful_watersheds <- results_log$watershed[results_log$status == "Success"]
if (length(successful_watersheds) > 0) {
  cat("\nSUCCESSFUL WATERSHEDS SUMMARY:\n")
  cat(paste(rep("-", 50), collapse = ""), "\n")
  
  for (watershed in successful_watersheds) {
    row <- results_log[results_log$watershed == watershed, ]
    cat("Watershed:", watershed, "\n")
    cat("  Solution Status:", row$solve_status, "\n")
    if (!is.na(row$total_cost)) {
      cat("  Total Cost: $", format(row$total_cost, big.mark = ",", scientific = FALSE), "\n")
    } else {
      cat("  Total Cost: N/A\n")
    }
    cat("  Processing Time:", round(row$total_time, 2), "minutes\n")
    
    # Show any errors even for successful runs
    if (!is.na(row$error_message) && row$error_message != "") {
      cat("  Warning/Errors:", substr(row$error_message, 1, 100), "\n")
    }
    
    cat(paste(rep("-", 30), collapse = ""), "\n")
  }
}

# Enhanced summary report for failed watersheds
failed_watersheds <- results_log$watershed[results_log$status != "Success"]
if (length(failed_watersheds) > 0) {
  cat("\nFAILED WATERSHEDS DETAILED REPORT:\n")
  cat(paste(rep("-", 50), collapse = ""), "\n")
  
  for (watershed in failed_watersheds) {
    row <- results_log[results_log$watershed == watershed, ]
    cat("Watershed:", watershed, "\n")
    cat("  Status:", row$status, "\n")
    cat("  Solution Status:", row$solve_status, "\n")
    cat("  Error Location:", row$error_location, "\n")
    cat("  Error Message:", row$error_message, "\n")
    cat("  Processing Time:", round(row$total_time, 2), "minutes\n")
    
    # Show trace if available
    if (!is.na(row$error_trace) && row$error_trace != "") {
      cat("  Detailed Trace:\n")
      trace_lines <- strsplit(row$error_trace, "\n")[[1]]
      for (i in seq_len(min(3, length(trace_lines)))) {
        cat("    ", trace_lines[i], "\n")
      }
    }
    cat(paste(rep("-", 30), collapse = ""), "\n")
  }
}

beepr::beep(3)
batch_results_dir <- paste0(BaseOutPath, "Batch Results/")
cat("\nBatch processing complete!\n")
cat("Summary results: ", paste0(batch_results_dir, "final_batch_results_", Sys.Date(), ".csv"), "\n")
cat("Individual watershed files are saved in subfolders within: ", batch_results_dir, "\n")
cat("Each watershed folder contains: AMPL model files (.mod, .dat) and NEOS result files (.txt)\n")
