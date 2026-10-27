#####################################################
## Market Intelligence Planet Fitness              ##
## Visual Data Analysis                            ##
## August 2026                                     ##
## by jortega                                      ##
#####################################################

# Load libraries
library(tidyverse)
library(dplyr)
library(knitr)

# Read data
club_master <- readRDS("./data/club_master.rds")
members_year <- readRDS("./data/members_year.rds")
club_performance <- readRDS("./data/club_performance.rds")
club_perf_year4 <- readRDS("./data/club_perf_year4.rds")
rf_importance <- readRDS("./data/rf_importance.rds")

# Note
# Change CAGR to Years 3-6 instead of 4-7!!  See query below.
# club_perf_year4 may have null values for either CAGRYears04 or CAGRYears47
# CAGRYears47 is null when OpenYear >= 2017

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


# Check rows where CAGRYears47 is NULL and select specific columns
# Total of nine clubs have no data for CAGRYears47!
club_perf_year4 %>%
  filter(is.na(CAGRYears47)) %>%
  select(ClubNumber, ClubName, State, OpenYear, YearNumber) %>%
  arrange(OpenYear, ClubNumber)

# Check rows where CAGRYears36 is NULL and select specific columns
# Total of 11 clubs have no data for CAGRYears36!
# club_perf_year4 %>%
#   filter(is.na(CAGRYears36)) %>%
#   select(ClubNumber, ClubName, State, OpenYear, YearNumber) %>%
#   arrange(OpenYear, ClubNumber)

# club_perf_year4 %>%
#   filter(!is.na(CAGRYears04) & RevenuePerSqFt > 40) %>%
#   group_by(EBITDAPerSqFtCat) %>%
#   summarise(n = n())
# 
# club_perf_year4 %>%
#   group_by(CAGRYears47Cat) %>%
#   summarise(n = n(),
#             AvgGrowth = mean(CAGRYears47, na.rm = TRUE)
#   )
# 
# club_perf_year4 %>%
#   filter(!is.na(CAGRYears04) & RevenuePerSqFt > 40) %>%
#   group_by(CAGRYears04Cat) %>%
#   summarise(n = n(),
#             AvgGrowth = mean(CAGRYears04, na.rm = TRUE)
#   )

# year_range_by_club <- club_performance %>%
#   group_by(ClubNumber) %>%
#   summarise(
#     MinYearNumber = min(YearNumber, na.rm = TRUE),
#     MaxYearNumber = max(YearNumber, na.rm = TRUE)
#   ) %>%
#   arrange(ClubNumber)
# 
# year_range_by_club
# 

# ============================================================================
# 1: TRADE AREA SCORE
# ============================================================================
# Prepare data
trade_area <- club_master %>%
  select(OverallScore, TradeAreaScore, SiteScore) %>%
  pivot_longer(cols = everything(), names_to = "ScoreType", values_to = "Score")

# Plot data
ggplot(trade_area, aes(x = Score, fill = ScoreType)) +
  geom_histogram(bins = 6, alpha = 0.7, position = "dodge") +
  facet_wrap(~ ScoreType, scales = "free_x") +
  labs(title = "Distribution of Club Scores",
       x = "Score Value",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none")


# ============================================================================
# 2: REVENUE TREND
# ============================================================================
# Join club_performance with club_dim to get PopDensityType
plot_data <- club_performance %>%
  select(ClubNumber, Year, YearNumber, Revenue, EBITDA) %>%
  mutate(YearNumber = as.factor(YearNumber)) %>%
  left_join(club_master, by = "ClubNumber") 

# Create the line plot
ggplot(plot_data, aes(x = Year, y = Revenue, 
                      group = ClubNumber, color = factor(ClubNumber))) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.5) +
  facet_wrap(~ PopDensityType, scales = "free_y", ncol = 1) +  # Horizontal facets
  labs(
    title = "Revenue Trend by Year Number and Club Number",
    x = "Year Number",
    y = "Revenue"
  ) +
  # Format Years if x axis has Years instead of YearNumber
  scale_x_continuous(
    breaks = unique(club_performance$Year),
    labels = function(x) as.integer(x)  # Format as whole numbers
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position = "none"
  )


# ============================================================================
# 3: HISTOGRAMS - Active Members, Revenue Per SqFt, EBITDA Per SqFt  OK
# ============================================================================
# CHART 1: Distribution of Active Members, EBITDA and Revenue
# Custom color palette (including chocolate3)
custom_palette <- c(
  "chocolate3",    # Your requested color
  "#E69F00",       # Orange
  "#56B4E9",       # Sky Blue
  "#009E73",       # Teal
  "#F0E442"        # Yellow
)

# Define a reusable theme
fit_theme <- function() {
  theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5, color = "gray30"),
      plot.subtitle = element_text(face = "italic", size = 10, hjust = 0.5, color = "gray30"),
      axis.title = element_text(face = "bold", size = 12, color = "gray30"),
      axis.text = element_text(size = 11, color = "gray30", family = "Helvetica"),
      strip.text = element_text(face = "bold", size = 10, color = "gray30"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank()
    )
}

# Prepare Data
perf_metrics <- club_perf_year4 %>%
  select(RevenuePerSqFt, EBITDAPerSqFt, ActiveMembers) %>%
  pivot_longer(cols = everything(), names_to = "PerformanceMetric", values_to = "Value")

# Calculate median values for each PerformanceMetric
median_values <- perf_metrics %>%
  group_by(PerformanceMetric) %>%
  summarise(Median = median(Value, na.rm = TRUE)) %>%
  ungroup()

# Plot with vertical faceting and custom colors
ggplot(perf_metrics, aes(x = Value, fill = PerformanceMetric)) +
  geom_histogram(bins = 10, alpha = 0.7, position = "dodge") +
  geom_vline(
    data = median_values,
    aes(xintercept = Median),
    linetype = "dotted",
    color = "indianred4",
    linewidth = 0.8
  ) +
  scale_fill_manual(values = custom_palette) +  # Apply custom palette
  facet_wrap(~ PerformanceMetric, scales = "free_x") +  # Vertical faceting
  labs(
    title = "Distribution of Performance Metrics",
    subtitle = "Mature Clubs. Vertical dotted lines indicate median values",
    x = "",
    y = "Frequency"
  ) +
  fit_theme()


# ============================================================================
# 4: SCATTER PLOT Revenue Per SqFt vs EBITDA Per SqFt  OK
# ============================================================================
# Scatterplot of RevenuePerSqFt vs. EBITDAPerSqFt with fitted line
ggplot(club_perf_year4, aes(x = RevenuePerSqFt, y = EBITDAPerSqFt)) +
  geom_point(
    aes(color = EBITDAPerSqFt > 80),
    #color = custom_palette[1],  # Use first color from custom_palette
    alpha = 0.6,
    size = 4
  ) +
  scale_color_manual(
    values = c("FALSE" = custom_palette[1], "TRUE" = custom_palette[4]),
  ) +
  geom_text(
    data = club_perf_year4 %>% filter(EBITDAPerSqFt > 80),
    aes(x = RevenuePerSqFt, y = EBITDAPerSqFt, label = ClubName),
    hjust = 1.2,  
    vjust = 0,  
    color = custom_palette[4],  # Use a color from custom_palette
    size = 3.5,
    fontface = "bold"
  ) +
  geom_text(
    data = club_perf_year4 %>% filter(RevenuePerSqFt < 40),
    aes(x = RevenuePerSqFt, y = EBITDAPerSqFt, label = ClubName),
    hjust = 0.1,  
    vjust = -1,  
    color = custom_palette[1],  # Use a color from custom_palette
    size = 3.5,
    fontface = "bold"
  ) +
  geom_hline(
    yintercept = 0,  # Horizontal line at EBITDAPerSqFt == 0
    color = custom_palette[2],  # Use a distinct color for visibility
    linetype = "solid",
    linewidth = 0.5
  ) +
  geom_smooth(
    method = "lm",  # Linear regression
    se = FALSE,     # Hide confidence interval
    color = custom_palette[3],  # Use third color from custom_palette
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  scale_y_continuous(breaks = seq(-20, 
                                  140, 
                                  by = 20)) + 
  labs(
    title = "Revenue vs. Profitability",
    subtitle = "Clubs with EBITDA Per SqFt > 80 are highlighted",
    x = "Revenue Per SqFt ($)",
    y = "EBITDA Per SqFt ($)"
  ) +
  fit_theme() +
  theme(
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.2)
  )



# ggplot(club_master, aes(x = Size)) +
#   geom_histogram(bins = 10, alpha = 0.7, position = "dodge")
  

# ============================================================================
# 5: SCATTER PLOT Club Size vs Revenue Per SqFt  OK
# ============================================================================
# Scatterplot of RevenuePerSqFt vs. Size
ggplot(filter(club_perf_year4, RevenuePerSqFt > 40), 
       aes(x = Size, y = RevenuePerSqFt, color = EBITDAPerSqFtCat)) +
  geom_point(
    alpha = 0.6,
    size = 4
  ) +
  scale_color_manual(
    name = "Profitability",  # Legend title
    values = c("Low" = custom_palette[1], "Medium" = custom_palette[2], "High" = custom_palette[3])
  ) +
  geom_hline(
    yintercept = 0,  # Horizontal line at EBITDAPerSqFt == 0
    color = custom_palette[2],  # Use a distinct color for visibility
    linetype = "solid",
    linewidth = 0.5
  ) +
  geom_smooth(
    method = "lm",  # Linear regression
    se = FALSE,     # Hide confidence interval
    color = custom_palette[3],  # Use a different color from custom_palette
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  scale_x_continuous(breaks = seq(10000, 
                                  34000, 
                                  by = 2500)) +
  scale_y_continuous(limits = c(-10, 200)) +
  labs(
    title = "Are Bigger Clubs More Profitable?",
    subtitle = "Excludes 309 - North Miami Beach",
    x = "Club Size",
    y = "Revenue Per SqFt ($)"
  ) +
  fit_theme() +
  theme(
    legend.position = "right",
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.2)
  )



# ============================================================================
# 6: SCATTERPLOT: Club Size vs ARPAM
# ============================================================================
# Load ggExtra for marginal plots
library(ggExtra)

# Prepare dataset
plot_data <- club_perf_year4 %>%
  filter(RevenuePerSqFt > 40) %>%  # exclude 309 - North Miami Beach
  mutate(YearNumber = as.factor(YearNumber),
         AvgRevPerActiveMember = ifelse(ActiveMembers == 0, NA, Revenue / ActiveMembers)
  )

# Scatterplot of Club Size vs. AvgRevPerActiveMember
p <- ggplot(plot_data, aes(x = Size, y = AvgRevPerActiveMember)) +
  geom_point(
    color = custom_palette[1],  # Use first color from custom_palette
    alpha = 0.6,
    size = 4
  ) +
  labs(
    title = "Club Size vs. ARPAM",
    x = "Club Size",
    y = "Avg Revenue Per Member"
  ) +
  fit_theme()

# Add marginal histograms
ggMarginal(p, type = "histogram", fill = custom_palette[2])





# ============================================================================
# 7: BOXPLOT - REVENUE PER SQFT, MATURE CLUBS (YEAR 4)
# ============================================================================
# Calculate median RevenuePerSqFt for each category
median_values <- club_perf_year4 %>%
  group_by(RevenuePerSqFtCat) %>%
  summarise(MedianRevenuePerSqFt = median(RevenuePerSqFt, na.rm = TRUE)) %>%
  ungroup()


# Plot with improved median annotations
ggplot(club_perf_year4, aes(x = RevenuePerSqFt, y = RevenuePerSqFtCat, fill = RevenuePerSqFtCat)) +
  geom_boxplot() +
  geom_point(
    position = position_jitter(height = 0.1),
    alpha = 0.5,
    color = "gray52"
  ) +
  # Annotate median values below the boxplots
  geom_text(
    data = median_values,
    aes(
      x = MedianRevenuePerSqFt,
      y = RevenuePerSqFtCat,
      label = paste0("$", round(MedianRevenuePerSqFt, 1))
    ),
    vjust = -5.4,
    hjust = 0.5,   # Center-align text horizontally
    color = "gray30",
    size = 3.5,
    fontface = "bold"
  ) +
  labs(
    title = "Revenue Per SqFt - Mature Clubs (Y4)",
    x = "",
    y = "Category"
  ) +
  scale_y_discrete(limits = c("High", "Medium", "Low")) +
  scale_fill_manual(values = custom_palette) +
  scale_x_continuous(labels = function(x) paste0("$", round(x, 1))) +
  fit_theme()



# ============================================================================
# 8: BOXPLOT - EBITDA PER SQFT, MATURE CLUBS (YEAR 4)   OK
# ============================================================================
# Calculate median EBITDAPerSqFt for each category
median_ebitda <- club_perf_year4 %>%
  group_by(EBITDAPerSqFtCat) %>%
  summarise(MedianEBITDAPerSqFt = median(EBITDAPerSqFt, na.rm = TRUE),
            n = n()
            ) %>%
  ungroup()


# EBITDA Per SqFt
ggplot(club_perf_year4, aes(x = EBITDAPerSqFt, y = EBITDAPerSqFtCat, fill = EBITDAPerSqFtCat)) +
  geom_boxplot() +
  geom_point(position = position_jitter(height = 0.1), alpha = 0.5, color = "gray52") +
  # Annotate median values below the boxplots
  geom_text(
    data = median_ebitda,
    aes(
      x = MedianEBITDAPerSqFt,
      y = EBITDAPerSqFtCat,
      label = paste0("$", round(MedianEBITDAPerSqFt, 0))
    ),
    vjust = -5.4,  # Place text below the boxplot
    hjust = 0.5,   # Center-align text horizontally
    color = "gray30",
    size = 3.5,
    fontface = "bold"
  ) +
  geom_text(
    data = median_ebitda,
    aes(
      x = 120,
      y = EBITDAPerSqFtCat,
      label = paste0("n = ", n)
    ),
    color = "grey60",
    size = 4.5,
    fontface = "bold"
  ) +
  labs(
    title = "Segmentation by Profitability - Mature Clubs",
    subtitle = "Values shown for the Average Club",
    x = "EBITDA Per SqFt",
    y = "Category"
  ) +
  scale_y_discrete(limits = c("Low", "Medium", "High")) +  # Order categories
  scale_fill_manual(values = custom_palette) +  # Apply custom color palette
  scale_x_continuous(labels = function(x) paste0("$", round(x, 1))) +  # Format x-axis with $
  fit_theme()  # Apply reusable theme


# Clubs with EBITDA close to zero
# club_perf_year4 %>%
#   filter(EBITDAPerSqFt < 10) %>%
#   select(ClubName, State, OpenYear, EBITDAPerSqFt, RevenuePerSqFt, Size)

# 301 - Coconut Creek, FL
# 303 - Pompano, FL
# 309 - North Miami Beach, FL
# 128 - Penn Branch, DC


# ============================================================================
# 9.1: PERFORMANCE METRICS - Growth % Years 0-4, MATURE CLUBS (YEAR 4)  OK
# ============================================================================
# Calculate median Growth % 04 for each category
median_cagryears04 <- club_perf_year4 %>%
  filter(!is.na(CAGRYears04)) %>%
  group_by(CAGRYears04Cat) %>%
  summarise(MedianCAGRYears04 = median(CAGRYears04, na.rm = TRUE),
            n = n()
            ) %>%
  ungroup()


# Subset to records where CAGRYears47 is not NULL and exclude 309 - Miami Beach (low revenue) 
plot_data <- club_perf_year4 %>% 
  filter(!is.na(CAGRYears04))


# Growth % (Years 0-4)
ggplot(plot_data, aes(x = CAGRYears04, y = CAGRYears04Cat, fill = CAGRYears04Cat)) +
  geom_boxplot() +
  geom_point(position = position_jitter(height = 0.1), 
             alpha = 0.5, 
             color = "gray52") +
  # Annotate median values below the boxplots
  geom_text(
    data = median_cagryears04,
    aes(
      x = MedianCAGRYears04,
      y = CAGRYears04Cat,
      label = paste0(round(MedianCAGRYears04*100, 0), "%")
    ),
    vjust = -5.4,  # Place text below the boxplot
    hjust = 0.5,   # Center-align text horizontally
    color = "gray30",
    size = 3.5,
    fontface = "bold"
  ) +
  geom_text(
    data = median_cagryears04,
    aes(
      x = .22,
      y = CAGRYears04Cat,
      label = paste0("n = ", n)
    ),
    color = "grey60",
    size = 4.5,
    fontface = "bold"
  ) +
  labs(
    title = "Segmentation by Growth % - Mature Clubs",
    subtitle = ("Growth % shown for the Average Club in each Category"),
    x = "Growth % (Years 0-4)",
    y = "Category"
  ) +
  scale_y_discrete(limits = c("Low", "Medium", "High")) +  # Order categories
  scale_fill_manual(values = custom_palette) +  # Apply custom color palette
  scale_x_continuous(labels = function(x) paste0(round(x*100, 0), "%")) +
  fit_theme()




# ============================================================================
# 9.2: PERFORMANCE METRICS - Growth % Years 4-7 MATURE CLUBS (YEAR 4)  OK
# ============================================================================
# Calculate median Growth % 04 for each category
median_cagryears47 <- club_perf_year4 %>%
  filter(!is.na(CAGRYears47)) %>%
  group_by(CAGRYears47Cat) %>%
  summarise(MedianCAGRYears47 = median(CAGRYears47, na.rm = TRUE),
            n = n()
  ) %>%
  ungroup()


# Subset to records where CAGRYears47 is not NULL and exclude 309 - Miami Beach (low revenue) 
plot_data <- club_perf_year4 %>% 
  filter(!is.na(CAGRYears47))


# Growth % (Years 0-4)
ggplot(plot_data, aes(x = CAGRYears47, y = CAGRYears47Cat, fill = CAGRYears47Cat)) +
  geom_boxplot() +
  geom_point(position = position_jitter(height = 0.1), 
             alpha = 0.5, 
             color = "gray52") +
  # Annotate median values below the boxplots
  geom_text(
    data = median_cagryears47,
    aes(
      x = MedianCAGRYears47,
      y = CAGRYears47Cat,
      label = paste0(round(MedianCAGRYears47*100, 0), "%")
    ),
    vjust = -5.4,  # Place text below the boxplot
    hjust = 0.5,   # Center-align text horizontally
    color = "gray30",
    size = 3.5,
    fontface = "bold"
  ) +
  geom_text(
    data = median_cagryears47,
    aes(
      x = .22,
      y = CAGRYears47Cat,
      label = paste0("n = ", n)
    ),
    color = "grey60",
    size = 4.5,
    fontface = "bold"
  ) +
  labs(
    title = "Segmentation by Growth % - Mature Clubs",
    subtitle = ("Growth % shown for the Average Club in each Category"),
    x = "Growth % (Years 4-7)",
    y = "Category"
  ) +
  scale_y_discrete(limits = c("Low", "Medium", "High")) +  # Order categories
  scale_fill_manual(values = custom_palette) +  # Apply custom color palette
  scale_x_continuous(labels = function(x) paste0(round(x*100, 0), "%")) +
  fit_theme()




# ============================================================================
# 9.3: PERFORMANCE METRICS - Growth % Years 3-6, MATURE CLUBS (YEAR 4)  OK
# ============================================================================
# Calculate median Growth % 04 for each category
median_cagryears36 <- club_perf_year4 %>%
  filter(!is.na(CAGRYears36)) %>%
  group_by(CAGRYears36Cat) %>%
  summarise(MedianCAGRYears36 = median(CAGRYears36, na.rm = TRUE),
            n = n()
  ) %>%
  ungroup()


# Subset to records where CAGRYears47 is not NULL and exclude 309 - Miami Beach (low revenue) 
plot_data <- club_perf_year4 %>% 
  filter(!is.na(CAGRYears36))


# Growth % (Years 0-4)
ggplot(plot_data, aes(x = CAGRYears36, y = CAGRYears36Cat, fill = CAGRYears36Cat)) +
  geom_boxplot() +
  geom_point(position = position_jitter(height = 0.1), 
             alpha = 0.5, 
             color = "gray52") +
  # Annotate median values below the boxplots
  geom_text(
    data = median_cagryears36,
    aes(
      x = MedianCAGRYears36,
      y = CAGRYears36Cat,
      label = paste0(round(MedianCAGRYears36*100, 0), "%")
    ),
    vjust = -5.4,  # Place text below the boxplot
    hjust = 0.5,   # Center-align text horizontally
    color = "gray30",
    size = 3.5,
    fontface = "bold"
  ) +
  geom_text(
    data = median_cagryears36,
    aes(
      x = .22,
      y = CAGRYears36Cat,
      label = paste0("n = ", n)
    ),
    color = "grey60",
    size = 4.5,
    fontface = "bold"
  ) +
  labs(
    title = "Segmentation by Growth % - Mature Clubs",
    subtitle = ("Growth % shown for the Average Club in each Category"),
    x = "Growth % (Years 3-6)",
    y = "Category"
  ) +
  scale_y_discrete(limits = c("Low", "Medium", "High")) +  # Order categories
  scale_fill_manual(values = custom_palette) +  # Apply custom color palette
  scale_x_continuous(labels = function(x) paste0(round(x*100, 0), "%")) +
  fit_theme()




# ============================================================================
# 10: MATRIX: Growth % (Years 4-7) vs EBITDA PER SQFT  OK
# ============================================================================
library(scales)  # For percent formatting

# Subset to records where CAGRYears47 is not NULL and exclude 309 - Miami Beach (low revenue) 
plot_data <- club_perf_year4 %>% 
  filter(!is.na(CAGRYears47) & RevenuePerSqFt > 40)

# Calculate 75 percentiles
perc75_cagr <- plot_data %>%
  filter(CAGRYears47Cat == "High") %>%  
  summarise(
    min_cagr = min(CAGRYears47, na.rm = TRUE)  
  ) %>%
  pull(min_cagr)

perc75_ebitda <- plot_data %>%
  filter(EBITDAPerSqFtCat == "High") %>%  
  summarise(
    min_ebitda = min(EBITDAPerSqFt, na.rm = TRUE)  
  ) %>%
  pull(min_ebitda)  

# Create Plot
ggplot(plot_data %>% filter(EBITDAPerSqFt < perc75_ebitda | CAGRYears47 < perc75_cagr), 
       aes(x = CAGRYears47, y = EBITDAPerSqFt, label = ClubNumber)) +
  geom_point(color = custom_palette[1], 
             alpha = 0.7, 
             size = 4) +
  geom_text(vjust = -0.8, 
            size = 4, 
            color = custom_palette[2]
  ) + 
  geom_hline(yintercept = perc75_ebitda, 
             linetype = "dashed", 
             color = custom_palette[3]
  ) +
  geom_vline(xintercept = perc75_cagr, 
             linetype = "dashed", 
             color = custom_palette[3]
  ) +
  annotate("text", x = perc75_cagr, y = 100,
           label = ">> High Growth",
           hjust = -0.2,
           vjust = 0,
           color = custom_palette[3],
           size = 3.5,
           fontface = "bold"
  ) +
  annotate("text", x = -0.05, y = perc75_ebitda, na.rm = TRUE,
           label = "^ High Profitability",
           hjust = 0.2,
           vjust = -1.2,
           color = custom_palette[3],
           size = 3.5,
           fontface = "bold") +
  geom_point(data = plot_data %>% filter(EBITDAPerSqFt >= perc75_ebitda & CAGRYears47 >= perc75_cagr),
             aes(x = CAGRYears47, y = EBITDAPerSqFt),
             color = custom_palette[1], 
             alpha = 0.7, 
             size = 4) +
  geom_text(
    data = plot_data %>% filter(EBITDAPerSqFt >= perc75_ebitda & CAGRYears47 >= perc75_cagr),
    aes(x = CAGRYears47, y = EBITDAPerSqFt, label = ClubName),
    vjust = -0.8,
    size = 4,
    color = custom_palette[2]
  ) +
  scale_x_continuous(labels = percent, limits = c(-0.07, 0.15)) +
  scale_y_continuous(limits = c(-10, 100)) +
  labs(
    title = "Growth % vs Profitabilty Matrix",
    subtitle = "Mature Clubs",
    x = "Growth % (Years 4-7)",
    y = "EBITDA Per SqFt ($)"
  ) +
  fit_theme()  # Apply your reusable theme



# ============================================================================
# 11: EBITDA PER SQFT and GROWTH % TOP CLUBS   OK
# ============================================================================
library(patchwork)

med_cagr <- plot_data %>%
  summarise(
    med_cagr = mean(CAGRYears47, na.rm = TRUE)  
  ) %>%
  pull(med_cagr)

med_ebitda <- plot_data %>%
  summarise(
    med_ebitda = median(EBITDAPerSqFt, na.rm = TRUE)  
  ) %>%
  pull(med_ebitda) 

# --- Chart 1: EBITDA per SqFt ---
ebitda_persqt_sorted <- club_perf_year4 %>%
  filter(!is.na(CAGRYears47) & RevenuePerSqFt > 40) %>%
  filter(EBITDAPerSqFtCat == "High") %>%
  arrange(EBITDAPerSqFt)

p1 <- ggplot(ebitda_persqt_sorted, aes(x = EBITDAPerSqFt, 
                                       y = reorder(ClubName, EBITDAPerSqFt))
) +
  geom_point(size = 4, 
             alpha = 0.8, 
             color = custom_palette[1]) +  # Use first color from custom_palette
  geom_segment(aes(x = 0, 
                   xend = EBITDAPerSqFt, 
                   yend = reorder(ClubName, EBITDAPerSqFt)
  ),
  color = custom_palette[1], 
  alpha = 0.4) +
  scale_x_continuous(labels = function(x) paste0("$", round(x, 1)), 
                     limits = c(0, 140)
  ) +
  geom_vline(xintercept = med_ebitda, 
             linetype = "dashed", 
             color = custom_palette[3], 
             linewidth = 0.8
  ) +  # Use third color from custom_palette
  annotate("text", 
           x = med_ebitda, 
           y = Inf, 
           na.rm = TRUE,
           label = paste0("Avg Club: $", med_ebitda),
           hjust = -0.06, 
           vjust = 1.2, 
           color = custom_palette[3], 
           size = 3.5, 
           fontface = "bold") +
  labs(
    title = "High Profitability Clubs",
    subtitle = "Mature Clubs - Y4",
    x = "",
    y = ""
  ) +
  fit_theme() +
  theme(
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.2)
  )

# --- Chart 2: CAGR Years 4-7 ---
cagr_year4_sorted <- club_perf_year4 %>%
  filter(!is.na(CAGRYears47) & RevenuePerSqFt > 40) %>%
  filter(CAGRYears47Cat == "High") %>%
  arrange(CAGRYears47PercRank)

p2 <- ggplot(cagr_year4_sorted, 
             aes(x = CAGRYears47, 
                 y = reorder(ClubName, CAGRYears47))
) +
  geom_point(size = 4, 
             alpha = 0.8, 
             color = custom_palette[2]
  ) +
  geom_segment(aes(x = 0, 
                   xend = CAGRYears47, 
                   yend = reorder(ClubName, CAGRYears47)
  ),
  color = custom_palette[2], 
  alpha = 0.45) +
  scale_x_continuous(labels = scales::percent, 
                     limits = c(0, 0.15)
  ) +
  geom_vline(xintercept = med_cagr, 
             linetype = "dashed", 
             color = custom_palette[3], 
             linewidth = 0.8) +  # Use third color from custom_palette
  annotate("text", x = med_cagr, y = Inf, 
           label = paste0("Avg Club: ", scales::percent(med_cagr, accuracy = 0.01)),
           hjust = -0.06, vjust = 1.2, 
           color = custom_palette[3], 
           size = 3.5, 
           fontface = "bold") +
  labs(
    title = "High Growth % Clubs",
    subtitle = "Years 4-7",
    x = "",
    y = ""
  ) +
  fit_theme() +
  theme(
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.2)
  )

# --- Combine plots vertically ---
p1 + plot_spacer() + p2 + plot_layout(ncol = 1, heights = c(1, 0.1, 1))



############################################################
## 12: FEATURE IMPORTANCE AFTER RUNNING STATISTICAL MODEL ##
############################################################

# Chart: Sucess Factors Behind Profitability
# Create the plot
ggplot(rf_importance, aes(x = Importance, y = Variable, fill = Importance)) +
  geom_col(width = 0.7, alpha = 0.9) +  # Horizontal bars with gradient fill
  scale_fill_gradient(
    low = "#0077bb",   # Blue for lower importance
    high = "#ee7733",  # Orange for higher importance
    name = "Importance Score"
  ) +
  scale_x_continuous(
    labels = comma,   # Format axis labels
    name = "Importance Score"
  ) +
  labs(
    title = "Random Forest Feature Importance",
    subtitle = "Top 20 Features by Mean Decrease in Gini",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.y = element_text(size = 9, color = "black"),
    legend.position = "right",
    panel.grid.major.y = element_blank(),  # Remove horizontal grid lines
    panel.grid.minor = element_blank()
  )








