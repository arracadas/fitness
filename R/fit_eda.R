#####################################################
## Market Intelligence Planet Fitness              ##
## Exploratory Data Analysis                       ##
## August 2026                                     ##
## by jortega                                      ##
#####################################################

# Load required libraries
library(tidyverse)
library(duckdb)
library(lubridate)
library(DBI)

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
# Number of clubs
nrow(club_dim)  # Tot 98

# Clean bottom rows in club_rev
club_rev <- club_rev %>%
  slice(1:(n() - 15))

# Clean bottom rows in club_eft
club_eft <- club_eft %>%
  slice(1:(n() - 10))

# Clean bottom rows in club_ebitda
club_ebitda <- club_ebitda %>%
  slice(1:(n() - 18))

# Check
tail(club_rev %>% select(`TTM Revenue`, State, `Date Open`))
tail(club_eft %>% select(`Monthly Dues`, State, `Date Open`))
tail(club_ebitda %>% select(`TTM EBITDA`, State, `Date Open`))


# Rename columns in Club Dim
club_dim <- club_dim %>%
  rename(
    OverallScore = `Overall Score`,
    TradeAreaScore = `Trade Area Score`,
    SiteScore = `Site Score`,
    ClosestClub = `Closest Club`,
    InterstateVisibility = InterstateStateVisibility
  )

# Rename columns in Club Dim
club_rev <- club_rev %>%
  rename(
    ClubLocation = `TTM Revenue`,
    OpenDate = `Date Open`
  )

# Rename columns in Club Dim
club_eft<- club_eft %>%
  rename(
    ClubLocation = `Monthly Dues`,
    OpenDate = `Date Open`
  )

# Rename columns in Club Dim
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

# Pivot club revenue
revenue <- club_rev %>%
  pivot_longer(
    cols = -c(ClubLocation, State, OpenDate),
    names_to = "MonthDate",
    values_to = "Revenue"
  )

# Pivot club EFT dues
eft_dues <- club_eft %>%
  pivot_longer(
    cols = -c(ClubLocation, State, OpenDate),
    names_to = "MonthDate",
    values_to = "EFTDues"
  )

# Pivot club EBITDA
ebitda <- club_ebitda %>%
  pivot_longer(
    cols = -c(ClubLocation, State, OpenDate),
    names_to = "MonthDate",
    values_to = "EBITDA"
  )

# Print basic info about the loaded dataframes
# glimpse(club_dim)
# glimpse(members)
# glimpse(revenue)
# glimpse(eft_dues)
# glimpse(ebitda)

# Convert columns to factor
club_dim$ClubName <- as.factor(club_dim$ClubName)
club_dim$City <- as.factor(club_dim$City)
club_dim$State <- as.factor(club_dim$State)
club_dim$Region <- as.factor(club_dim$Region)
club_dim$PopDensityType <- as.factor(club_dim$PopDensityType)
club_dim$ADA <- as.factor(club_dim$ADA)
club_fin$ClubName <- as.factor(club_fin$ClubName)

# Convert columns to date
club_dim$OpenDate <- as.Date(club_dim$OpenDate, format = "%m/%d/%Y")
club_dim$MaturityDate <- as.Date(club_dim$MaturityDate, format = "%m/%d/%Y")
revenue$MonthDate <- my(revenue$MonthDate)  # lubridate
eft_dues$MonthDate <- my(eft_dues$MonthDate)
ebitda$MonthDate <- my(ebitda$MonthDate)

# Check columns
# ebitda %>%
#   filter(
#     ClubLocation == "Elkton",
#     MonthDate > as.Date("2010-08-01")
#   ) %>%
#   head(25)

# Clean dollar amounts
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


# Apply the function to each column
revenue$Revenue <- clean_and_convert(revenue$Revenue)
eft_dues$EFTDues <- clean_and_convert(eft_dues$EFTDues)
ebitda$EBITDA <- clean_and_convert(ebitda$EBITDA)


#Notes:
# EFTDues has missing data across all clubs between April - June 2020

# Check summary stats
summary(members)
# Min Date:  2015-01-01
# Max Date:  2026-06-01

# Check for nulls in club dim
check_nulls <- function(df) {
  # Check for nulls in each column
  null_counts <- df %>%
    summarise(across(everything(), ~ sum(is.na(.x)))) %>%
    pivot_longer(
      cols = everything(),
      names_to = "Column",
      values_to = "NullCount"
    ) %>%
    filter(NullCount > 0)  # Only show columns with nulls
  
  # Return the result
  if (nrow(null_counts) == 0) {
    message("✅ No null values found in the dataframe.")
    return(null_counts)
  } else {
    message("⚠️ Null values found in the following columns:")
    print(null_counts)
  }
}


check_nulls(club_dim)
# No null values found

# Check for nulls in members
check_nulls(members)
# Six rows with null Active Members values

# Select null values in members
# members %>% filter(is.na(ActiveMembers))

# Clean the records with null values
members <- members %>%
  filter(!is.na(ActiveMembers))

# Check for nulls in club_fin
check_nulls(club_fin)
# No nulls found



# ============================================================================
# 3: MEMEBERS BY REGION USING DUCKDB 
# ============================================================================
# Create a DuckDB connection
con <- dbConnect(duckdb::duckdb(), ":memory:")

# Copy dataframes to DuckDB tables
members_tbl <- copy_to(con, members, "members", overwrite = TRUE)
club_dim_tbl <- copy_to(con, club_dim, "club_dim", overwrite = TRUE)


# Check table structures
q1 <- dbGetQuery(con, "DESCRIBE members")
q2 <- dbGetQuery(con, "DESCRIBE club_dim")

print(q1)
print(q2)


# 1. Calculate Members by Region
result1 <- dbGetQuery(con, "
  SELECT
    c.Region,
    SUM(m.ActiveMembers) AS TotalMembers,
    AVG(m.ActiveMembers) AS AvgMembers,
    (SELECT AVG(ActiveMembers) FROM members) AS AvgMembersAllRegions
  FROM
    members m
  LEFT JOIN
    club_dim c ON m.ClubNumber = c.ClubNumber
  GROUP BY
    c.Region
")

# Print the result
print(result1)

# Disconnect from DuckDB
dbDisconnect(con)


# ============================================================================
# 4: SUMMARY STATISTICS AND REFERENTIAL INTEGRITY
# ============================================================================
# Active Members stats
members %>%
  select(-c(ClubNumber, ClubLocation)) %>%
  summary()

# Club summary stats
club_dim %>%
  select(where(is.numeric)) %>%
  select(-c(ClubNumber, Latitude, Longitude)) %>%
  summary()

# Revenue stats
revenue %>%
  select(-c(ClubLocation, State, OpenDate, MonthDate)) %>%
  summary()

# Check negatives
# revenue %>% filter(Revenue < 0)

# EFT Dues stats
eft_dues %>%
  select(-c(ClubLocation, State, OpenDate, MonthDate)) %>%
  summary()

# Check negatives
# eft_dues %>% filter(EFTDues < 0)

# EBITDA stats
ebitda %>%
  select(-c(ClubLocation, State, OpenDate, MonthDate)) %>%
  summary()

# Club Fin summary stats
club_fin %>%
  select(where(is.numeric)) %>%
  summary()

# Club Categorical Column Unique Values
club_categorical <- club_dim %>%
  # select(where(is.character)) %>%
  select(where(is.factor)) %>%
  summarise(across(everything(), ~ length(unique(na.omit(.)))))
print(club_categorical)

# Referential Integrity - Club Numbers and Members
clubs_in_members <- members$ClubNumber %>% unique()
clubs_in_lookup <- club_dim$ClubNumber %>% unique()

clubs_only_in_members <- setdiff(clubs_in_members, clubs_in_lookup)
clubs_only_in_lookup <- setdiff(clubs_in_lookup, clubs_in_members)

print(clubs_only_in_members)
print(clubs_only_in_lookup)

# Referential Integrity - Club Names vs Revenue, EFTDues and EBITDA
# Add new columna to club_dim
club_dim$ClubNameAlt <- gsub("^\\d+\\s*-\\s*", "", club_dim$ClubName)

clubs_names_in_rev <- revenue$ClubLocation %>% unique()
clubs_names_in_eft <- eft_dues$ClubLocation %>% unique()
clubs_names_in_ebitda <- ebitda$ClubLocation %>% unique()
clubs_names_in_lookup <- club_dim$ClubNameAlt %>% unique()

clubs_names_only_in_rev <- setdiff(clubs_names_in_rev, clubs_names_in_lookup)
clubs_names_only_in_lookup <- setdiff(clubs_names_in_lookup, clubs_names_in_rev)
print(clubs_names_only_in_rev)
print(clubs_names_only_in_lookup)

clubs_names_only_in_eft <- setdiff(clubs_names_in_eft, clubs_names_in_lookup)
clubs_names_only_in_ebitda <- setdiff(clubs_names_in_ebitda, clubs_names_in_lookup)
print(clubs_names_only_in_eft)
print(clubs_names_only_in_ebitda)

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


# Update ClubLocation
revenue <- revenue %>%
  mutate(
    ClubLocation = recode(ClubLocation, !!!club_mapping)
  )

eft_dues <- eft_dues %>%
  mutate(
    ClubLocation = recode(ClubLocation, !!!club_mapping)
  )

ebitda <- ebitda %>%
  mutate(
    ClubLocation = recode(ClubLocation, !!!club_mapping)
  )


# Lookup Club Number
revenue <- club_dim %>%
  select(ClubNumber, ClubNameAlt) %>%
  left_join(
    revenue,
    by = c("ClubNameAlt" = "ClubLocation")
  ) %>%
  # Select all columns from revenue and add ClubNumber from club_dim
  select(
    ClubNumber,
    ClubNameAlt,
    MonthDate,
    Revenue
  )


eft_dues <- club_dim %>%
  select(ClubNumber, ClubNameAlt) %>%
  left_join(
    eft_dues,
    by = c("ClubNameAlt" = "ClubLocation")
  ) %>%
  # Select all columns from revenue and add ClubNumber from club_dim
  select(
    ClubNumber,
    ClubNameAlt,
    MonthDate,
    EFTDues
  )

ebitda <- club_dim %>%
  select(ClubNumber, ClubNameAlt) %>%
  left_join(
    ebitda,
    by = c("ClubNameAlt" = "ClubLocation")
  ) %>%
  # Select all columns from revenue and add ClubNumber from club_dim
  select(
    ClubNumber,
    ClubNameAlt,
    MonthDate,
    EBITDA
  )


# ============================================================================
# 5: TRANSFORM DATA - CALCULATE MEMBERS BY YEAR, CAGR Y0-Y3 and CAGR Y3-Y6
# ============================================================================

# Connect to DuckDB (in-memory)
con <- dbConnect(duckdb::duckdb(), ":memory:")

# Copy the members and club_dim dataframes to DuckDB
copy_to(con, members, "members", overwrite = TRUE)
copy_to(con, club_dim, "club_dim", overwrite = TRUE)

# Reuse the existing SQL to create member_years
# Reuse the existing SQL to create member_years
sql_average_members <- "
  WITH YearlyAverages AS (
    SELECT
      ClubNumber,
      EXTRACT(YEAR FROM MonthDate) AS Year,
      AVG(ActiveMembers) AS AvgActiveMembers
    FROM
      members
    GROUP BY
      ClubNumber, EXTRACT(YEAR FROM MonthDate)
  ),
  OpenYears AS (
    SELECT
      ClubNumber,
      EXTRACT(YEAR FROM OpenDate) AS OpenYear
    FROM
      club_dim
  )
  SELECT
    a.ClubNumber,
    a.Year,
    a.AvgActiveMembers,
    (a.Year - b.OpenYear - 1) AS YearNumber  -- Year 0 = first year after opening
  FROM
    YearlyAverages a
  JOIN
    OpenYears b ON a.ClubNumber = b.ClubNumber
  WHERE
    a.Year > b.OpenYear
  ORDER BY
    a.ClubNumber, a.Year
"

member_years <- dbGetQuery(con, sql_average_members)

# Copy member_years to DuckDB for further processing
copy_to(con, member_years, "club_member_years", overwrite = TRUE)

# SQL query to calculate CAGR for Years 0-3 and Years 3-6
sql_cagr <- "
  WITH Year0AndYear3 AS (
    SELECT
      ClubNumber,
      MAX(CASE WHEN YearNumber = 0 THEN AvgActiveMembers END) AS Year0AvgActiveMembers,
      MAX(CASE WHEN YearNumber = 3 THEN AvgActiveMembers END) AS Year3AvgActiveMembers
    FROM
      club_member_years
    GROUP BY
      ClubNumber
  ),
  Year3AndYear6 AS (
    SELECT
      ClubNumber,
      MAX(CASE WHEN YearNumber = 3 THEN AvgActiveMembers END) AS Year3AvgActiveMembers,
      MAX(CASE WHEN YearNumber = 6 THEN AvgActiveMembers END) AS Year6AvgActiveMembers
    FROM
      club_member_years  -- Changed from member_years to club_member_years
    GROUP BY
      ClubNumber
  )
  SELECT
    y03.ClubNumber,
    CASE
      WHEN y03.Year0AvgActiveMembers IS NOT NULL AND y03.Year3AvgActiveMembers IS NOT NULL
      THEN POWER(y03.Year3AvgActiveMembers / y03.Year0AvgActiveMembers, 1.0 / 3) - 1
      ELSE NULL
    END AS CAGR_Years0_3,
    CASE
      WHEN y36.Year3AvgActiveMembers IS NOT NULL AND y36.Year6AvgActiveMembers IS NOT NULL
      THEN POWER(y36.Year6AvgActiveMembers / y36.Year3AvgActiveMembers, 1.0 / 3) - 1
      ELSE NULL
    END AS CAGR_Years3_6
  FROM
    (SELECT DISTINCT ClubNumber FROM club_member_years) clubs  -- Changed from member_years to club_member_years
  LEFT JOIN
    Year0AndYear3 y03 ON clubs.ClubNumber = y03.ClubNumber
  LEFT JOIN
    Year3AndYear6 y36 ON clubs.ClubNumber = y36.ClubNumber
"

# Execute the query and save results
cagr_results <- dbGetQuery(con, sql_cagr)

# Close the connection
dbDisconnect(con)


# ============================================================================
# 5: TRANSFORM DATA - CALCULATE DuesPerMember, RevenuePerSqFt and EbitdaPerSqFt
# ============================================================================
# Connect to DuckDB (in-memory)
con <- dbConnect(duckdb::duckdb(), ":memory:")

# Copy all dataframes to DuckDB
copy_to(con, member_years, "club_member_years", overwrite = TRUE, temporary = FALSE)
copy_to(con, club_dim, "club_dim", overwrite = TRUE, temporary = FALSE)
copy_to(con, revenue, "club_revenue", overwrite = TRUE, temporary = FALSE)
copy_to(con, eft_dues, "club_eft", overwrite = TRUE, temporary = FALSE)
copy_to(con, ebitda, "club_ebitda", overwrite = TRUE, temporary = FALSE)

# --- Extract Revenue by Year (Last Month of the Year) ---
sql_revenue_by_year <- "
  WITH RankedRevenue AS (
    SELECT
      ClubNumber,
      MonthDate,
      EXTRACT(YEAR FROM MonthDate) AS Year,
      Revenue,
      ROW_NUMBER() OVER (PARTITION BY ClubNumber, EXTRACT(YEAR FROM MonthDate) ORDER BY MonthDate DESC) AS RowNum
    FROM
      club_revenue
  ),
  RevenueYear AS (
    SELECT
      ClubNumber,
      Year,
      Revenue
    FROM
      RankedRevenue
    WHERE
      RowNum = 1
  )
  SELECT
    a.ClubNumber,
    a.Year,
    a.Revenue,
    b.YearNumber
  FROM
    RevenueYear a
  JOIN
    club_member_years b ON a.ClubNumber = b.ClubNumber AND a.Year = b.Year
"

revenue_years <- dbGetQuery(con, sql_revenue_by_year)

# --- Extract EFT Dues by Year (Last Month of the Year) ---
sql_eft_dues_by_year <- "
  WITH RankedEFTDues AS (
    SELECT
      ClubNumber,
      MonthDate,
      EXTRACT(YEAR FROM MonthDate) AS Year,
      EFTDues,
      ROW_NUMBER() OVER (PARTITION BY ClubNumber, EXTRACT(YEAR FROM MonthDate) ORDER BY MonthDate DESC) AS RowNum
    FROM
      club_eft
  ),
  EFTDuesYear AS (
    SELECT
      ClubNumber,
      Year,
      EFTDues
    FROM
      RankedEFTDues
    WHERE
      RowNum = 1
  )
  SELECT DISTINCT
    a.ClubNumber,
    a.Year,
    a.EFTDues,
    b.YearNumber
  FROM
    EFTDuesYear a
  JOIN
    club_member_years b ON a.ClubNumber = b.ClubNumber AND a.Year = b.Year
"

eft_dues_years <- dbGetQuery(con, sql_eft_dues_by_year)

# --- Extract EBITDA by Year (Last Month of the Year) ---
sql_ebitda_by_year <- "
  WITH RankedEBITDA AS (
    SELECT
      ClubNumber,
      MonthDate,
      EXTRACT(YEAR FROM MonthDate) AS Year,
      EBITDA,
      ROW_NUMBER() OVER (PARTITION BY ClubNumber, EXTRACT(YEAR FROM MonthDate) ORDER BY MonthDate DESC) AS RowNum
    FROM
      club_ebitda
  ),
  EBITDAYear AS (
    SELECT
      ClubNumber,
      Year,
      EBITDA
    FROM
      RankedEBITDA
    WHERE
      RowNum = 1
  )
  SELECT DISTINCT
    a.ClubNumber,
    a.Year,
    a.EBITDA,
    b.YearNumber
  FROM
    EBITDAYear a
  JOIN
    club_member_years b ON a.ClubNumber = b.ClubNumber AND a.Year = b.Year
"

ebitda_years <- dbGetQuery(con, sql_ebitda_by_year)


# Check that there are no dupes
check_unique_club_year <- function(df) {
  # Count the number of rows per ClubNumber and Year combination
  row_counts <- df %>%
    group_by(ClubNumber, Year) %>%
    summarise(RowCount = n(), .groups = "drop")
  
  # Check if all ClubNumber-Year combinations have exactly one row
  all_unique <- all(row_counts$RowCount == 1, na.rm = TRUE)
  
  # Return the result with a message
  if (all_unique) {
    message("✅ Each ClubNumber-Year combination has exactly one row in the dataframe.")
    return(TRUE)
  } else {
    message("❌ Some ClubNumber-Year combinations have duplicate or missing rows in the dataframe.")
    message("ClubNumber-Year combinations with issues: ")
    print(row_counts %>% filter(RowCount != 1))
    return(FALSE)
  }
}

# check_unique_club_year(revenue_years)  # OK
# check_unique_club_year(eft_dues_years) # OK
# check_unique_club_year(ebitda_years)  # OK

# Check NAs
check_nulls(revenue_years)
check_nulls(eft_dues_years)
check_nulls(ebitda_years)

# --- Copy intermediate results to DuckDB ---
copy_to(con, revenue_years, "revenue_years", overwrite = TRUE, temporary = FALSE)
copy_to(con, eft_dues_years, "eft_dues_years", overwrite = TRUE, temporary = FALSE)
copy_to(con, ebitda_years, "ebitda_years", overwrite = TRUE, temporary = FALSE)
copy_to(con, cagr_results, "cagr_results", overwrite = TRUE, temporary = FALSE)

# --- Consolidated Performance (without CAGR) ---
sql_combined_data <- "
  SELECT
    m.ClubNumber,
    m.Year,
    m.YearNumber AS YearNumber,
    m.AvgActiveMembers,
    r.Revenue,
    e.EFTDues,
    eb.EBITDA,
    cm.Size,
    ROUND(e.EFTDues / NULLIF(m.AvgActiveMembers, 0), 2) AS DuesPerMember,
    ROUND(r.Revenue / NULLIF(cm.Size, 0), 2) AS RevenuePerSqFt,
    ROUND(eb.EBITDA / NULLIF(cm.Size, 0), 2) AS EBITDAPerSqFt
  FROM
    club_member_years m
  JOIN
    club_dim cm ON m.ClubNumber = cm.ClubNumber
  LEFT JOIN
    revenue_years r ON m.ClubNumber = r.ClubNumber AND m.Year = r.Year
  LEFT JOIN
    eft_dues_years e ON m.ClubNumber = e.ClubNumber AND m.Year = e.Year
  LEFT JOIN
    ebitda_years eb ON m.ClubNumber = eb.ClubNumber AND m.Year = eb.Year
  WHERE
    m.Year < 2025
"

club_performance <- dbGetQuery(con, sql_combined_data)

# check_unique_club_year(club_performance)
# club_performance %>%
#   filter(is.na(Revenue)) %>%
#   distinct(ClubNumber)

# no revenue data for clubs 601 through 610

# ============================================================================
# 6: TRANSFORM DATA - CALCULATE PERCENT RANK
# ============================================================================

# Step 1: Subset club_performance to YearNumber == 4
club_performance_year4 <- subset(club_performance, YearNumber == 4)

# Copy the subsetted dataframe to DuckDB
copy_to(con, club_performance_year4, "club_performance_year4", overwrite = TRUE, temporary = FALSE)

# Step 2-4: Calculate percent_rank and add categorical columns (with CAGR)
sql_percent_rank <- "
  SELECT
    cp.*,
    cg.CAGR_Years0_3,
    cg.CAGR_Years3_6,
    PERCENT_RANK() OVER (ORDER BY cp.DuesPerMember) AS DuesPerMemberPercRank,
    PERCENT_RANK() OVER (ORDER BY cp.RevenuePerSqFt) AS RevenuePerSqFtPercRank,
    PERCENT_RANK() OVER (ORDER BY cp.EBITDAPerSqFt) AS EBITDAPerSqFtPercRank,
    CASE
      WHEN PERCENT_RANK() OVER (ORDER BY cp.DuesPerMember) <= 0.2 THEN 'Low'
      WHEN PERCENT_RANK() OVER (ORDER BY cp.DuesPerMember) > 0.8 THEN 'High'
      ELSE 'Medium'
    END AS DuesPerMemberCat,
    CASE
      WHEN PERCENT_RANK() OVER (ORDER BY cp.RevenuePerSqFt) <= 0.2 THEN 'Low'
      WHEN PERCENT_RANK() OVER (ORDER BY cp.RevenuePerSqFt) > 0.8 THEN 'High'
      ELSE 'Medium'
    END AS RevenuePerSqFtCat,
    CASE
      WHEN PERCENT_RANK() OVER (ORDER BY cp.EBITDAPerSqFt) <= 0.2 THEN 'Low'
      WHEN PERCENT_RANK() OVER (ORDER BY cp.EBITDAPerSqFt) > 0.8 THEN 'High'
      ELSE 'Medium'
    END AS EBITDAPerSqFtCat
  FROM
    club_performance_year4 cp
  LEFT JOIN
    cagr_results cg ON cp.ClubNumber = cg.ClubNumber  -- Join only on ClubNumber
"

# Execute the query and save results
club_performance_year4_with_ranks <- dbGetQuery(con, sql_percent_rank)

# check_unique_club_year(club_performance_year4)

# Close the connection
dbDisconnect(con)



# ============================================================================
# 8: SAVE TO FILE
# ============================================================================
# Save dataframes as RDS files
saveRDS(club_dim, file = "./data/club_master.rds")
saveRDS(club_performance, file = "./data/club_performance.rds")
saveRDS(club_performance_year4_with_ranks, file = "./data/club_perf_year4.rds")
