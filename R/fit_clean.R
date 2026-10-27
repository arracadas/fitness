#####################################################
## Market Intelligence Planet Fitness              ##
## Prepare Clean Datasets                          ##
## August 2026                                     ##
## by jortega                                      ##
#####################################################

# Load required libraries
library(tidyverse)
library(duckdb)
library(lubridate)

# ============================================================================
# 1: READ DATA FILES INTO DATAFRAMES
# ============================================================================

# Read the CSV files from the data folder
club_dim <- read_csv("./data/club_master.csv")
club_members_month <- read_csv("./data/active_members.csv")
club_fin <- read_csv("./data/club_financial.csv")
club_rev <- read_csv("./data/fin_revenue.csv")
club_eft <- read_csv("./data/fin_eft_dues.csv")
club_ebitda <- read_csv("./data/fin_ebitda.csv")


# ============================================================================
# 2: CLEAN AND TRANSFORM
# ============================================================================
# Clean bottom rows in club_rev
club_rev <- club_rev %>%
  slice(1:(n() - 15))


# Clean bottom rows in club_eft
club_eft <- club_eft %>%
  slice(1:(n() - 10))


# Clean bottom rows in club_ebitda
club_ebitda <- club_ebitda %>%
  slice(1:(n() - 18))

# Rename columns in Club Dim
club_dim <- club_dim %>%
  rename(
    OverallScore = `Overall Score`,
    TradeAreaScore = `Trade Area Score`,
    SiteScore = `Site Score`,
    ClosestClub = `Closest Club`,
    InterstateVisibility = InterstateStateVisibility
  )


# Rename columns in Financials
club_rev <- club_rev %>%
  rename(
    ClubLocation = `TTM Revenue`,
    OpenDate = `Date Open`
  )

club_eft<- club_eft %>%
  rename(
    ClubLocation = `Monthly Dues`,
    OpenDate = `Date Open`
  )

club_ebitda <- club_ebitda %>%
  rename(
    ClubLocation = `TTM EBITDA`,
    OpenDate = `Date Open`
  )


# Pivot members data
members <- club_members_month %>%
  pivot_longer(
    cols = -c(ClubLocation, ClubNumber),
    names_to = "MonthDate",
    values_to = "ActiveMembers"
  ) %>%
  mutate(
    MonthDate = as.Date(MonthDate, format = "%m/%d/%Y")
  )


# Pivot financials
revenue <- club_rev %>%
  pivot_longer(
    cols = -c(ClubLocation, State, OpenDate),
    names_to = "MonthDate",
    values_to = "Revenue"
  )

eft_dues <- club_eft %>%
  pivot_longer(
    cols = -c(ClubLocation, State, OpenDate),
    names_to = "MonthDate",
    values_to = "EFTDues"
  )

ebitda <- club_ebitda %>%
  pivot_longer(
    cols = -c(ClubLocation, State, OpenDate),
    names_to = "MonthDate",
    values_to = "EBITDA"
  )


# Convert columns to factor
club_dim$ClubName <- as.factor(club_dim$ClubName)
club_dim$City <- as.factor(club_dim$City)
club_dim$State <- as.factor(club_dim$State)
club_dim$Region <- as.factor(club_dim$Region)
club_dim$PopDensityType <- as.factor(club_dim$PopDensityType)
club_dim$ADA <- as.factor(club_dim$ADA)
club_fin$ClubName <- as.factor(club_fin$ClubName)
club_dim$MarketClass <- as.factor(club_dim$MarketClass)
club_dim$StoreClass <- as.factor(club_dim$StoreClass)

# Convert columns to date
club_dim$OpenDate <- as.Date(club_dim$OpenDate, format = "%m/%d/%Y")
club_dim$MaturityDate <- as.Date(club_dim$MaturityDate, format = "%m/%d/%Y")
revenue$MonthDate <- my(revenue$MonthDate)  # lubridate
eft_dues$MonthDate <- my(eft_dues$MonthDate)
ebitda$MonthDate <- my(ebitda$MonthDate)

# Add OpenYear
club_dim$OpenYear <- as.integer(year(club_dim$OpenDate))  

# Assign PopulationRange based on MarketClass
club_dim <- club_dim %>%
  mutate(
    PopulationRange = case_when(
      MarketClass == 1 ~ "2.5M+",
      MarketClass == 2 ~ "1M",
      MarketClass == 3 ~ "500K",
      MarketClass == 4 ~ "200K",
      MarketClass == 5 ~ "80K",
      MarketClass == 6 ~ "< 80K",
      MarketClass == 7 ~ "Non-Urban",
      TRUE ~ "Unknown"
      )
    ) %>%
  mutate(
    PopulationRange = factor(
      PopulationRange,
      levels = c("Non-Urban", "< 80K", "80K", "200K", "500K", "1M", "2.5M+")
      )
    )


# Filter out clubs that opened in 2025 or later: five in 2025 and two in 2026
club_dim <- club_dim %>%
  filter(OpenDate < as.Date("2025-01-01"))  # Only clubs opened before 2025


# Clean dollar amounts for financial data
clean_and_convert <- function(column) {
  #Remove hyphens followed by empty spaces
  cleaned_column <- gsub("-\\s*", "", column)
  #Remove commas
  cleaned_column <- gsub(",", "", cleaned_column)
  #Remove any remaining non-numeric characters (except digits, decimal points, parenthesis and leading hyphens)
  cleaned_column <- gsub("\\(([0-9.,]+)\\)", "-\\1", cleaned_column)
  # Step 5: Convert to numeric, replacing NA or empty strings with 0
  as.numeric(ifelse(cleaned_column == "" | is.na(cleaned_column), 0, cleaned_column))
}


# Apply clean dollar amounts function
revenue$Revenue <- clean_and_convert(revenue$Revenue)
eft_dues$EFTDues <- clean_and_convert(eft_dues$EFTDues)
ebitda$EBITDA <- clean_and_convert(ebitda$EBITDA)


# Clean member records with null values
members_df <- members %>%
  filter(!is.na(ActiveMembers))

# Clean financial data
revenue_df <- revenue %>%
  filter(!is.na(Revenue))


eft_dues_df <- eft_dues %>%
  filter(!is.na(EFTDues))


ebitda_df <- ebitda %>%
  filter(!is.na(EBITDA))

# These clubs (Michigan) have no financial data
# 601 Comstock Park
# 602 Byron Center
# 603 Grand Haven
# 604 Grand Rapids
# 605 Greenville
# 606 Holland
# 607 Holland South
# 608 Muskegon North
# 609 Muskegon
# 610 Plainfield
# 651 Rice Lake


# ============================================================================
# 3: SUMMARY STATISTICS AND REFERENTIAL INTEGRITY
# ============================================================================
# Referential Integrity - Club Names vs Revenue, EFTDues and EBITDA
# Add new columna to club_dim
club_dim$ClubNameAlt <- gsub("^\\d+\\s*-\\s*", "", club_dim$ClubName)


# Define the mapping dictionary (named list)
club_mapping <- list(
  "NY Ave" = "Ivy City",
  "Columbia-MD" = "Columbia, MD",
  "Upper Marlboro'" = "Upper Marlboro",
  "Mt Juliet" = "Mt. Juliet",
  "La Vergne" = "LaVergne",
  "N Miami Beach" = "North Miami Beach",
  "Port Orchard'" = "Port Orchard",
  "Smokey Point'" = "Smokey Point"
)

# No club found in Club Master
# "Federal Way"
# "Randallstown"
# "Sevierville"
# "Coral Springs"
# "Margate"
# "Pine Street"
# "Silverdale"
# "Sequim"
# "Bellingham"
# "Seattle UW"
# "San Mateo"
# "Silverdale
# "Centralia"

# Function to update ClubLocation for financial datasets
update_club_location <- function(df) {
  df %>%
    mutate(ClubLocation = recode(ClubLocation, !!!club_mapping))
}

# Function to join with club_dim and select ClubNumber, MonthDate, and the value column
join_and_select <- function(df, value_col) {
  club_dim %>%
    select(ClubNumber, OpenYear, ClubNameAlt) %>%
    inner_join(df, by = c("ClubNameAlt" = "ClubLocation")) %>%
    select(ClubNumber, OpenYear, MonthDate, !!sym(value_col))
}

# !!sym() is used to dynamically pass column names as strings to functions. 
# Without !!sym(), dplyr would treat value_col as a literal string rather than a column name.

# Apply the functions to each dataframe
revenue_df <- revenue_df %>%
  update_club_location() %>%
  join_and_select("Revenue")

eft_dues_df <- eft_dues_df %>%
  update_club_location() %>%
  join_and_select("EFTDues")

ebitda_df <- ebitda_df %>%
  update_club_location() %>%
  join_and_select("EBITDA")


# Find ClubNumbers and ClubNames in club_dim that are NOT in revenue_df
missing_in_revenue <- club_dim %>%
  select(ClubNumber, ClubName, State) %>%
  anti_join(distinct(revenue_df, ClubNumber), by = "ClubNumber")

missing_in_ebitda <- club_dim %>%
  select(ClubNumber, ClubName, State) %>%
  anti_join(distinct(ebitda_df, ClubNumber), by = "ClubNumber")

# View the result:  MI clubs are missing
# missing_in_revenue
# missing_in_ebitda

# ============================================================================
# 4: ROLLUP FINANCIAL DATA BY YEAR
# ============================================================================
# Important: every month captures TTM; e.g. Revenue in Dec is Cum. Revenue TTM
# Select Month of december and add Year column
select_december <- function(df, value_col) {
  df %>%
    filter(month(MonthDate) == 12) %>%
    mutate(
      Year = as.integer(year(MonthDate)),
      ClubNumber = as.integer(ClubNumber)
    ) %>%
    filter(Year > OpenYear) %>%
    select(ClubNumber, OpenYear, Year, !!sym(value_col))
}

# Apply the function to each dataframe
revenue_year_df <- select_december(revenue_df, "Revenue")
eft_dues_year_df <- select_december(eft_dues_df, "EFTDues")
ebitda_year_df <- select_december(ebitda_df, "EBITDA")


# ============================================================================
# 5: TRANSFORM DATA - CALCULATE MEMBERS BY YEAR
# ============================================================================
# Connect to DuckDB (in-memory)
con <- dbConnect(duckdb::duckdb(), ":memory:")

# Copy the members and club_dim dataframes to DuckDB
copy_to(con, members_df, "members", overwrite = TRUE)
copy_to(con, club_dim, "club_dim", overwrite = TRUE)

# Create member_years -- Remove MI clubs
sql_average_members <- "
  WITH YearlyAverages AS (
    SELECT
      ClubNumber,
      EXTRACT(YEAR FROM MonthDate) AS Year,
      ROUND(AVG(ActiveMembers), 0) AS AvgActiveMembers
    FROM
      members
    GROUP BY
      ClubNumber, EXTRACT(YEAR FROM MonthDate)
  ),
  OpenYears AS (
    SELECT
      ClubNumber,
      ClubName,
      State,
      OpenYear
    FROM
      club_dim
  )
  SELECT
    b.ClubNumber,
    b.ClubName,
    b.State,
    b.OpenYear,
    a.Year,
    a.AvgActiveMembers
  FROM
    YearlyAverages a
  JOIN
    OpenYears b ON a.ClubNumber = b.ClubNumber
  WHERE
    a.Year > b.OpenYear  -- Some clubs have ActiveMembers for earlier years
    AND a.Year < 2026
    AND b.State != 'MI'
  ORDER BY
    a.ClubNumber, a.Year
"

members_year_df <- dbGetQuery(con, sql_average_members)

# Close the connection
dbDisconnect(con)

# Check min Year
# Minimum Year is 2015 while Financial data Starts in 2010!
# member_years %>%
#   group_by(ClubNumber) %>%
#   summarize(
#     FirstYear = min(Year)
#   )


# ============================================================================
# 6: TRANSFORM DATA - REMOVE COVID YEAR OF 2020
# ============================================================================
update_2020_with_avg <- function(df, value_col) {
  # Calculate the average of 2019 and 2021 for each ClubNumber, only if both years exist
  avg_2019_2021 <- df %>%
    filter(Year %in% c(2019, 2021)) %>%
    group_by(ClubNumber) %>%
    summarize(
      Avg2019_2021 = if (n() == 2) mean(!!sym(value_col), na.rm = TRUE) else NA_real_
    ) %>%
    ungroup()

  # Update the 2020 values with the average of 2019 and 2021, only if the average exists
  df_updated <- df %>%
    left_join(avg_2019_2021, by = "ClubNumber") %>%
    mutate(
      !!value_col := ifelse(Year == 2020 & !is.na(Avg2019_2021), Avg2019_2021, !!sym(value_col))
    ) %>%
    select(-Avg2019_2021)  # Remove the temporary column

  return(df_updated)
}


# Update datasets
members_year_df <- update_2020_with_avg(members_year_df, "AvgActiveMembers")
revenue_year_df <- update_2020_with_avg(revenue_year_df, "Revenue")
eft_dues_year_df <- update_2020_with_avg(eft_dues_year_df, "EFTDues")
ebitda_year_df <- update_2020_with_avg(ebitda_year_df, "EBITDA")


# ============================================================================
# 7: TRANSFORM DATA - ADD YEAR NUMBER
# ============================================================================
# Shift OpenYear for Frederick, Bowie and Hagerstown because Members data start in 2015
# This is temporary because YearNumber calculation substracts OpenYear from YearNumber
# The shift is needed to make sure these clubs have a CAGR 3-6.
# Also, for consistency, apply the same logic to other dataframes
# Helper function to apply the same transformations to all dataframes
apply_year_transformations <- function(df) {
  df %>%
    mutate(
      OpenYearTemp = ifelse(ClubNumber %in% c(101, 102, 103), 2011, OpenYear),
      YearNumber = Year - OpenYearTemp - 1
    ) %>%
    select(-OpenYearTemp)  # Drop the OpenYearTemp column
}

# Apply the transformations to all dataframes
members_year_df <- apply_year_transformations(members_year_df)
revenue_year_df <- apply_year_transformations(revenue_year_df)
eft_dues_year_df <- apply_year_transformations(eft_dues_year_df)
ebitda_year_df <- apply_year_transformations(ebitda_year_df)


# members_year_df %>%
#   filter(ClubNumber %in% c(112, 114, 116, 117)) %>%
#   select(ClubNumber, ClubName, OpenYear, Year, YearNumber, AvgActiveMembers)

# Check that there are no dupes
# check_unique_club_year <- function(df) {
#   # Count the number of rows per ClubNumber and Year combination
#   row_counts <- df %>%
#     group_by(ClubNumber, Year) %>%
#     summarise(RowCount = n(), .groups = "drop")
#   
#   # Check if all ClubNumber-Year combinations have exactly one row
#   all_unique <- all(row_counts$RowCount == 1, na.rm = TRUE)
#   
#   # Return the result with a message
#   if (all_unique) {
#     message("✅ Each ClubNumber-Year combination has exactly one row in the dataframe.")
#     return(TRUE)
#   } else {
#     message("❌ Some ClubNumber-Year combinations have duplicate or missing rows in the dataframe.")
#     message("ClubNumber-Year combinations with issues: ")
#     print(row_counts %>% filter(RowCount != 1))
#     return(FALSE)
#   }
# }


# check_unique_club_year(members_year_df)
# check_unique_club_year(revenue_year_df)
# check_unique_club_year(eft_dues_year_df)
# check_unique_club_year(ebitda_year_df)

# Double records for eft_dues!
# eft_dues_year_df %>%
#   filter(ClubNumber == 101 & Year == 2012)


# ============================================================================
# 8: TRANSFORM DATA - CALCULATE CAGR Y0-Y4, Y3-Y6 AND Y4-Y7
# ============================================================================
# Connect to DuckDB
con <- dbConnect(duckdb::duckdb(), ":memory:")

# Copy member_years to DuckDB for further processing
copy_to(con, members_year_df, "club_member_years", overwrite = TRUE)

# SQL query to calculate CAGR for Years 0-4 and Years 4-7
sql_cagr <- "
  -- Step 1: Calculate CAGRYears04
  WITH CAGRYears04 AS (
    SELECT
      ClubNumber,
      POWER(
        MAX(CASE WHEN YearNumber = 4 THEN AvgActiveMembers END) /
        NULLIF(MAX(CASE WHEN YearNumber = 0 THEN AvgActiveMembers END), 0),
        1.0 / 4
      ) - 1 AS CAGRYears04
    FROM
      club_member_years
    WHERE
      YearNumber IN (0, 4)
    GROUP BY
      ClubNumber
    HAVING
      COUNT(DISTINCT YearNumber) = 2
  ),

  -- Step 1b: Calculate PERCENT_RANK for CAGRYears04
  CAGRYears04WithRank AS (
    SELECT
      ClubNumber,
      CAGRYears04,
      PERCENT_RANK() OVER (ORDER BY CAGRYears04) AS CAGRYears04PercRank,
      CASE
        WHEN PERCENT_RANK() OVER (ORDER BY CAGRYears04) <= 0.2 THEN 'Low'
        WHEN PERCENT_RANK() OVER (ORDER BY CAGRYears04) > 0.7 THEN 'High'
        ELSE 'Medium'
      END AS CAGRYears04Cat
    FROM
      CAGRYears04
  ),

  -- Step 2: Calculate CAGRYears36
  CAGRYears36 AS (
    SELECT
      ClubNumber,
      POWER(
        MAX(CASE WHEN YearNumber = 6 THEN AvgActiveMembers END) /
        NULLIF(MAX(CASE WHEN YearNumber = 3 THEN AvgActiveMembers END), 0),
        1.0 / 3
      ) - 1 AS CAGRYears36
    FROM
      club_member_years
    WHERE
      YearNumber IN (3, 6)
    GROUP BY
      ClubNumber
    HAVING
      COUNT(DISTINCT YearNumber) = 2
  ),

  -- Step 2b: Calculate PERCENT_RANK for CAGRYears36
  CAGRYears36WithRank AS (
    SELECT
      ClubNumber,
      CAGRYears36,
      PERCENT_RANK() OVER (ORDER BY CAGRYears36) AS CAGRYears36PercRank,
      CASE
        WHEN PERCENT_RANK() OVER (ORDER BY CAGRYears36) <= 0.2 THEN 'Low'
        WHEN PERCENT_RANK() OVER (ORDER BY CAGRYears36) > 0.7 THEN 'High'
        ELSE 'Medium'
      END AS CAGRYears36Cat
    FROM
      CAGRYears36
  ),

  -- Step 3: Calculate CAGRYears47
  CAGRYears47 AS (
    SELECT
      ClubNumber,
      POWER(
        MAX(CASE WHEN YearNumber = 7 THEN AvgActiveMembers END) /
        NULLIF(MAX(CASE WHEN YearNumber = 4 THEN AvgActiveMembers END), 0),
        1.0 / 3
      ) - 1 AS CAGRYears47
    FROM
      club_member_years
    WHERE
      YearNumber IN (4, 7)
    GROUP BY
      ClubNumber
    HAVING
      COUNT(DISTINCT YearNumber) = 2
  ),

  -- Step 3b: Calculate PERCENT_RANK for CAGRYears47
  CAGRYears47WithRank AS (
    SELECT
      ClubNumber,
      CAGRYears47,
      PERCENT_RANK() OVER (ORDER BY CAGRYears47) AS CAGRYears47PercRank,
      CASE
        WHEN PERCENT_RANK() OVER (ORDER BY CAGRYears47) <= 0.2 THEN 'Low'
        WHEN PERCENT_RANK() OVER (ORDER BY CAGRYears47) > 0.7 THEN 'High'
        ELSE 'Medium'
      END AS CAGRYears47Cat
    FROM
      CAGRYears47
  )

  -- Step 4: Combine results using club_member_years as the driving table
  SELECT
    cm.ClubNumber,
    c04.CAGRYears04,
    c04.CAGRYears04PercRank,
    c04.CAGRYears04Cat,
    c36.CAGRYears36,
    c36.CAGRYears36PercRank,
    c36.CAGRYears36Cat,
    c47.CAGRYears47,
    c47.CAGRYears47PercRank,
    c47.CAGRYears47Cat
  FROM
    (SELECT DISTINCT ClubNumber FROM club_member_years) cm
  LEFT JOIN
    CAGRYears04WithRank c04 ON cm.ClubNumber = c04.ClubNumber
  LEFT JOIN
    CAGRYears36WithRank c36 ON cm.ClubNumber = c36.ClubNumber
  LEFT JOIN
    CAGRYears47WithRank c47 ON cm.ClubNumber = c47.ClubNumber
"

cagr_results <- dbGetQuery(con, sql_cagr)


# ============================================================================
# 9: TRANSFORM DATA - CALCULATE RevenuePerSqFt and EbitdaPerSqFt
# ============================================================================
# --- Copy intermediate results to DuckDB ---
copy_to(con, club_dim, "club_dim", overwrite = TRUE)
copy_to(con, revenue_year_df, "revenue_years_tbl", overwrite = TRUE, temporary = FALSE)
# copy_to(con, eft_dues_year_df, "eft_dues_years_tbl", overwrite = TRUE, temporary = FALSE)
copy_to(con, ebitda_year_df, "ebitda_years_tbl", overwrite = TRUE, temporary = FALSE)

# --- Consolidated Performance (without CAGR) --- Remove clubs in MI
sql_combined_data <- "
  WITH club_size AS (
    SELECT DISTINCT
      dm.ClubNumber,
      dm.Size
    FROM
      club_dim dm
    WHERE
      dm.OpenYear < 2025
      AND dm.State != 'MI'
  )
  SELECT
    m.ClubNumber,
    m.Year,
    m.YearNumber,
    COALESCE(mem.AvgActiveMembers, 0) AS ActiveMembers,
    m.Revenue,
    eb.EBITDA,
    ROUND(m.Revenue / NULLIF(cm.Size, 0), 2) AS RevenuePerSqFt,
    ROUND(eb.EBITDA / NULLIF(cm.Size, 0), 2) AS EBITDAPerSqFt
  FROM
    revenue_years_tbl m
  JOIN
    club_size cm ON m.ClubNumber = cm.ClubNumber
  JOIN
    ebitda_years_tbl eb ON m.ClubNumber = eb.ClubNumber AND m.Year = eb.Year
  LEFT JOIN
    club_member_years mem ON m.ClubNumber = mem.ClubNumber AND m.Year = mem.Year
  WHERE
    m.Year < 2025
"

club_performance <- dbGetQuery(con, sql_combined_data)


# ============================================================================
# 10: TRANSFORM DATA - CALCULATE PERCENT RANK
# ============================================================================
# Step 1: Subset club_performance to YearNumber == 4
club_performance_year4 <- subset(club_performance, YearNumber == 4)

# Copy the subsetted dataframe to DuckDB
copy_to(con, club_performance_year4, "club_performance_year4", overwrite = TRUE, temporary = FALSE)
copy_to(con, cagr_results, "cagr_results_tbl", overwrite = TRUE, temporary = FALSE)

# Step 2-4: Calculate percent_rank and add categorical columns (with CAGR)
sql_percent_rank <- "
  WITH ranked_data AS (
    SELECT
      cp.*,
      PERCENT_RANK() OVER (ORDER BY cp.RevenuePerSqFt) AS RevenuePerSqFtPercRank,
      PERCENT_RANK() OVER (ORDER BY cp.EBITDAPerSqFt) AS EBITDAPerSqFtPercRank,
      CASE
        WHEN PERCENT_RANK() OVER (ORDER BY cp.RevenuePerSqFt) <= 0.2 THEN 'Low'
        WHEN PERCENT_RANK() OVER (ORDER BY cp.RevenuePerSqFt) > 0.7 THEN 'High'
        ELSE 'Medium'
      END AS RevenuePerSqFtCat,
      CASE
        WHEN PERCENT_RANK() OVER (ORDER BY cp.EBITDAPerSqFt) <= 0.2 THEN 'Low'
        WHEN PERCENT_RANK() OVER (ORDER BY cp.EBITDAPerSqFt) > 0.7 THEN 'High'
        ELSE 'Medium'
      END AS EBITDAPerSqFtCat,
    FROM
      club_performance_year4 cp
  )
  SELECT
    dm.*,
    pm.Year,
    pm.YearNumber,
    pm.ActiveMembers,
    pm.Revenue,
    pm.EBITDA,
    pm.RevenuePerSqFt,
    pm.RevenuePerSqFtPercRank,
    pm.RevenuePerSqFtCat,
    pm.EBITDAPerSqFt,
    pm.EBITDAPerSqFtPercRank,
    pm.EBITDAPerSqFtCat,
    cg.CAGRYears04,
    cg.CAGRYears04PercRank,
    cg.CAGRYears04Cat,
    cg.CAGRYears36,
    cg.CAGRYears36PercRank,
    cg.CAGRYears36Cat,
    cg.CAGRYears47,
    cg.CAGRYears47PercRank,
    cg.CAGRYears47Cat
  FROM
    club_dim dm
  JOIN
    ranked_data pm on dm.ClubNumber = pm.ClubNumber
  LEFT JOIN
    cagr_results_tbl cg ON dm.ClubNumber = cg.ClubNumber
  WHERE
    dm.OpenYear < 2025
    AND dm.State != 'MI'
"

# Execute the query and save results
club_performance_year4_with_ranks <- dbGetQuery(con, sql_percent_rank)

# Close the connection
dbDisconnect(con)


# ============================================================================
# 11: SAVE TO FILE
# ============================================================================
# Save dataframes as RDS files
saveRDS(club_dim, file = "./data/club_master.rds")
saveRDS(members_year_df, file = "./data/members_year.rds")
saveRDS(club_performance, file = "./data/club_performance.rds")
saveRDS(club_performance_year4_with_ranks, file = "./data/club_perf_year4.rds")

# # Read data
# club_master <- readRDS("../data/club_master.rds")
# club_performance <- readRDS("../data/club_performance.rds")
# club_perf_year4 <- readRDS("../data/club_perf_year4.rds")
