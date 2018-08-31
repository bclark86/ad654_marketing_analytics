library(tidyverse)
library(ggthemes)
theme_set(theme_tufte())


# alter date for file survey file
# remove first two rows of survey question metadata
surveys <- read_csv("data/Starbucks.csv") %>%
  filter(row_number() > 2) %>%
  filter(Finished == "True") %>% # remove non-completes
  mutate(survey_id = row_number()) %>%
  select(-c(1:17)) %>%
  rename(US_qualifier = QID21,
         locations = Q1,
         mobile_app_consideration = Q2,
         rewards_pts_no_expiration = Q2.1_1,
         debit_credit_link_pop = Q2.1_2,
         preload_funds = Q2.1_3,
         exclusive_promo_discount = Q2.1_4,
         free_annual_gift = Q2.1_5,
         personalized_notifications = Q2.1_6,
         bonus_point_offers = Q2.1_7,
         surprise_free_items_coupon = Q2.1_8,
         surprise_rewards_dollars_gift_card = Q2.1_9,
         refer_friend_rewards = Q2.1_10,
         starbucks_visit_spend = Q3,
         starbucks_visits_past_7_days = Q4,
         starbucks_app_installed = Q5,
         starbucks_app_used_past_7_days = Q5.1) %>%
  filter(US_qualifier == "Yes") %>% # remove unqualified surveys
  select(survey_id, everything())

# fill na none of the above NAs
surveys$locations <- ifelse(is.na(surveys$locations), "Non-Coffee/Tea", surveys$locations)

# code locations visited
surveys$Starbucks            <- ifelse(str_detect(surveys$locations, "Starbucks"), 1, 0)
surveys$`Dunkin'' Donuts'`   <- ifelse(str_detect(surveys$locations, "Dunkin' Donuts"), 1, 0)
surveys$`McDonalds`          <- ifelse(str_detect(surveys$locations, "McDonalds"), 1, 0)
surveys$`7-Eleven`           <- ifelse(str_detect(surveys$locations, "7-Eleven"), 1, 0)
surveys$`Peets Coffee`       <- ifelse(str_detect(surveys$locations, "Peet's Coffee"), 1, 0)
surveys$`Local/Neighborhood` <- ifelse(str_detect(surveys$locations, "Local"), 1, 0)
surveys$`Home/Household`     <- ifelse(str_detect(surveys$locations, "Home"), 1, 0)
surveys$Work                 <- ifelse(str_detect(surveys$locations, "Work"), 1, 0)
surveys$Other                <- ifelse(str_detect(surveys$locations, "Other"), 1, 0)
surveys$`None of the Above`  <- ifelse(str_detect(surveys$locations, "Non-Coffee/Tea"), 1, 0)

#create customer segments
surveys$customer_segment <- ifelse(surveys$Starbucks == 1, "Starbucks Customer", "Non-Customer")
surveys$app_segment      <- ifelse(surveys$Starbucks != 1, "Non-Customer", 
                                   ifelse(surveys$Starbucks == 1 & surveys$starbucks_app_installed == "No", 
                                          "Starbucks Non-App User",
                                          ifelse(surveys$Starbucks == 1 & surveys$starbucks_app_used_past_7_days == "No",
                                                 "Starbucks Non-App User", "Starbucks App User")))


# create factors
# convert survey questions to 1 to 5 scale for importance
likert_ids <- 5:14
surveys[likert_ids] <- lapply(surveys[likert_ids], factor, 
                              levels = c("Not at all important",
                                         "Slightly important",
                                         "Moderately important",
                                         "Very important",
                                         "Extremely important"), ordered = TRUE)

# convert locations visited to Yes/No factors
# convert Yes/No questions to factors
yes_no_ids <- c(2, 4, 17, 18)
surveys[yes_no_ids] <- lapply(surveys[yes_no_ids], factor, 
                              levels = c("Yes", "No"))

# convert numeric columns
num_ids <- 15:16
surveys[num_ids] <- lapply(surveys[num_ids], as.numeric)

###############################################################################

# PLOTS - Location, Mobile App Consideration, Importance Comparison, 
#         Profile Analysis, CLV histograms, Permutation results, CLV comparison

###############################################################################
#------------------------------------------------------------------------------

# LOCATION

#------------------------------------------------------------------------------

survey_count <- length(surveys$survey_id)

# summary counts of locations visited
locations <- surveys %>%
  select(c(19:28)) %>%
  gather(location, count) %>%
  group_by(location) %>%
  summarize(count = sum(count)) %>%
  mutate(pct = paste0(round(count / survey_count * 100, 0), "%"))

# ordered factor based on frequency
locations$location <- factor(locations$location, levels = locations$location[order(locations$count, decreasing = TRUE)])
locations$Starbucks <- ifelse(locations$location == "Starbucks", TRUE, FALSE)

# create plot title label
starbucks_val <- locations[locations$Starbucks == TRUE, ]$pct

# plot locations indicated
location_plot <- ggplot(locations, aes(location, count, fill = Starbucks)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = pct), vjust = -0.5) +
  scale_fill_manual(values = c("#999999", "#009E73")) +
  labs(title = paste(starbucks_val, 
                     "of respondents indicated they've consumed a Starbucks coffee/tea beverage in the past 30 days", 
                     sep = " "),
       caption = paste("In the past 30 days, have you consumed a coffee or tea beverage from any of the following locations? N = ",
                       survey_count),
       x = "",
       y = "") +
  guides(fill = FALSE) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
print(location_plot)
ggsave("plots/fig_locations_visited_previous_30_days_plot.png", location_plot)

#------------------------------------------------------------------------------

# MOBILE APP CONSIDERATION

#------------------------------------------------------------------------------

# create plot label 
mobile_app_consider_val <- length(surveys[surveys$mobile_app_consideration == "Yes", ]$mobile_app_consideration) / survey_count
mobile_app_consider_val <- paste0(round(mobile_app_consider_val * 100, 0), "%")

mobile_app_consideration_plot <- ggplot(surveys, aes(mobile_app_consideration, fill = mobile_app_consideration)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = paste0(round(..count.. / survey_count * 100, 0), "%")), vjust = -0.5) +
  scale_fill_manual(values = c("#009E73", "#999999")) +
  labs(title = paste(mobile_app_consider_val, 
                     "of respondents indicated they have or would join a mobile app loaylty rewards program",
                     sep = " "),
       caption = paste("Have you previously joined, or would you consider joining, a mobile app loyalty rewards program? N = ",
                       survey_count),
       x = "",
       y = "") +
  guides(fill = FALSE) +
  theme(axis.text.x = element_text(size = rel(1.25)),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
print(mobile_app_consideration_plot)
ggsave("plots/fig_mobile_app_consideration_plot.png", mobile_app_consideration_plot)

#------------------------------------------------------------------------------

# IMPORTANCE BAR CHART

#------------------------------------------------------------------------------

# rating counts for extremely important or very important

rating_count <- length(surveys[surveys$mobile_app_consideration == "Yes", ]$mobile_app_consideration)

ratings <- surveys %>%
  select(customer_segment, app_segment, c(5:14)) %>% # can be used to cut in different ways
  gather(question, rating, -customer_segment, -app_segment) %>%
  filter(rating %in% c("Extremely important", "Very important")) %>%
  group_by(question, rating) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(pct = paste0(round(count / rating_count * 100, 0), "%"))


ratings$rating <- factor(ratings$rating, levels = unique(sort(ratings$rating, TRUE)))
ratings$question <- factor(ratings$question, 
                           levels = unique(ratings$question)[order(ratings[ratings$rating == "Extremely important", ]$count, 
                                                           decreasing = FALSE)]
                           )

# labels for chart in reverse order of Extremely Important for coord flip
rating_questions_labs1 <- c("Personalized\nNotifications", #10
                            "Rewards for\nReferring a Friend", #9
                            "Link Debit/Credit Card\nfor Mobile Payment\nat Point of Purchase", #8
                            "Preload Funuds\nfor Future Purchases", #7
                            "Free Gift Annually", #6
                            "Surprise Rewards\nDollars (Gift Card)", #5
                            "Exclusive\nPromotions/Discounts", #4
                            "Bonus Point Offers", #3
                            "Surprise Free\nItems (Coupon)", #2
                            "Rewards Points\nthat do not Expire") #1

ratings_bar_plot <- ggplot(ratings, aes(x = question, y = count, fill = rating, label = pct)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_manual(breaks = c("Extremely important","Very important"),
                    values = c("#999999", "#009E73")) +
  coord_flip() +
  labs(title = "Importance of Mobile App Loyalty Program Attributes",
       caption = paste("How important to you are each of the following factors for a mobile app loyalty program? N = ",
                       rating_count),
       x = "",
       y = "") +
  scale_x_discrete(labels = rating_questions_labs1) +
  theme(legend.position = c(0.80, 0.2),
        legend.title = element_blank(),
        legend.justification = 'right',
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
        )
print(ratings_bar_plot)
ggsave("plots/fig_mobile_app_attributes_importance_plot.png", ratings_bar_plot)

#------------------------------------------------------------------------------

# PROFILE ANALYSIS

#------------------------------------------------------------------------------

# profile analysis
profile_analysis <- surveys %>%
  select(app_segment, likert_ids) %>%
  drop_na() %>%
  group_by(app_segment) %>%
  mutate(n = n(),
         segment = paste0(app_segment, " (n = ", n, ")")) %>%
  ungroup() %>%
  select(-app_segment, -n) %>%
  group_by(segment) %>%
  mutate_all(funs(as.numeric(.))) %>%
  summarise_all(funs(mean)) %>%
  gather(question, rating, -segment)

rating_questions_labs2 <- c("Surprise Rewards\nDollars (Gift Card)", #10
                            "Surprise Free\nItems (Coupon)", #9
                            "Rewards Points\nthat do not Expire", #8
                            "Rewards for\nReferring a Friend", #7
                            "Preload Funuds\nfor Future Purchases", #6
                            "Personalized\nNotifications", #5
                            "Free Gift Annually", #4
                            "Exclusive\nPromotions/Discounts", #3
                            "Link Debit/Credit Card\nfor Mobile Payment\nat Point of Purchase", #2
                            "Bonus Point Offers") #1

profile_analysis_plot <- ggplot(profile_analysis, aes(x = question, y = rating, color = segment, group = segment)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 4, size = 0.1, color = "grey50") +
  scale_y_continuous(limits = c(1, 5),
                     labels = c("Not at all important",
                                "Slightly important",
                                "Moderately important",
                                "Very important",
                                "Extremely\nimportant")) +
  coord_flip() +
  scale_x_discrete(labels = rev(rating_questions_labs2)) + # reverse order due to coordflip
  labs(title = "Average Ratings of Mobile App Loyalty Program Attributes",
       caption = paste("How important to you are each of the following factors for a mobile app loyalty program? N = ",
                       rating_count),
       x = "",
       y = "") +
  theme(legend.position = "top",
        legend.title = element_blank())
print(profile_analysis_plot)
ggsave("plots/fig_profile_analysis_plot.png", profile_analysis_plot)

#------------------------------------------------------------------------------

# CLV Histrograms

#------------------------------------------------------------------------------

# customer lifetime value calculation (1 year)

# clv = (margin * retention rate) / (1 + discount rate - retention rate)
# revenue = 52 weeks * average trips * average spend
# retention = customers w/ visits > 0 in last 7 days / customers w/ visits in last 30 days
# discount rate = 10%
# margin rate = 13.51% https://ycharts.com/companies/SBUX/profit_margin

starbucks_customers_30days <- sum(surveys$Starbucks)
starbucks_customers_7days  <- sum(surveys$starbucks_visits_past_7_days > 0, na.rm = TRUE)
starbucks_retention <- starbucks_customers_7days / starbucks_customers_30days
margin_rate <- .1351
discount_rate <- .10

starbucks_clv <- surveys %>%
  filter(Starbucks == 1) %>%
  select(app_segment, starbucks_app_installed, starbucks_visits_past_7_days, starbucks_visit_spend) %>%
  mutate(visits = ifelse(starbucks_visits_past_7_days > 0, 
                         starbucks_visits_past_7_days, 0.25), # attribute partial visits for none in last 7 days
         spend = starbucks_visit_spend) %>%
  select(-starbucks_visits_past_7_days, -starbucks_visit_spend) %>%
  mutate(revenue_52 = spend * visits * 52,
         margin = revenue_52 * margin_rate,
         clv = (margin * starbucks_retention) / (1 + discount_rate - starbucks_retention))


# clv: app user vs. non-app user (past 7 days)
starbucks_clv_app_plot <- ggplot(starbucks_clv, aes(x = clv, fill = app_segment)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("#009E73", "#999999")) +
  labs(title = "Starbucks Customer Lifetime Value (CLV)",
       caption = "CLV = (margin * retention rate) / (1 + discount rate - retention rate)",
       x = "CLV",
       y = "Density",
       fill = "App Segment") +
  theme(legend.position = c(0.80, 0.80))
print(starbucks_clv_app_plot)
ggsave("plots/fig_clv_app_usage_plot.png", starbucks_clv_app_plot)

# clv: app installed vs. not installed
starbucks_clv_app_install_plot <- ggplot(starbucks_clv, aes(x = clv, fill = starbucks_app_installed)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("#009E73", "#999999")) +
  labs(title = "Starbucks Customer Lifetime Value (CLV)",
       caption = "CLV = (margin * retention rate) / (1 + discount rate - retention rate)",
       x = "CLV",
       y = "Density",
       fill = "App Installed") +
  theme(legend.position = c(0.80, 0.80))
print(starbucks_clv_app_plot)
ggsave("plots/fig_clv_app_installed_plot.png", starbucks_clv_app_install_plot)

#------------------------------------------------------------------------------

# CLV PERMUTATION TESTS

#------------------------------------------------------------------------------

# permutation test

# permutation function to randomly assign values to groups
perm_fun <- function(x, n1, n2) {
  n <- n1 + n2
  idx_b <- sample(1:n, n1)
  idx_a <- setdiff(1:n, idx_b)
  mean_diff <- mean(x[idx_b]) - mean(x[idx_a])
  return(mean_diff)
}

# permutation test for app used
table(starbucks_clv$app_segment)

app_used_mean <- mean(starbucks_clv[starbucks_clv$app_segment == "Starbucks App User", ]$clv)
no_app_used_mean <- mean(starbucks_clv[starbucks_clv$app_segment == "Starbucks Non-App User", ]$clv)
obs_app_diff <- app_used_mean - no_app_used_mean

# app used = n1, non-app used = n2
perm_diffs_app_used <- rep(0, 10000)
for (i in 1:10000) {
  perm_diffs_app_used[i] <- perm_fun(starbucks_clv$clv, 9, 25)
}

p_value_app_used <- mean(perm_diffs_app_used > obs_app_diff)

app_used_label <- paste0("Observed Difference","\np-value: ", p_value_app_used)

perm_test_app_used_plot <- ggplot() + aes(perm_diffs_app_used) +
  geom_histogram(binwidth = 10, fill = "#009E73") +
  labs(title = "Permutation Test Results - App Usage Difference in CLV",
       subtitle = "Hypothesis: App users have higher CLV than non-app users",
       caption = "App Users N = 9, Non-App Users N = 25",
       x = "CLV Differences ($)",
       y = "Frequency") +
  geom_vline(xintercept = obs_app_diff, linetype = "dashed") +
  annotate("text", x = 50, y = 600, label = app_used_label)
print(perm_test_app_used_plot)
ggsave("plots/fig_perm_test_app_used_plot.png", perm_test_app_used_plot)

# permutation test for app installed
table(starbucks_clv$starbucks_app_installed)

app_install_mean <- mean(starbucks_clv[starbucks_clv$starbucks_app_installed == "Yes", ]$clv)
no_app_install_mean <- mean(starbucks_clv[starbucks_clv$starbucks_app_installed == "No", ]$clv)
obs_app_install_diff <- app_install_mean - no_app_install_mean

# app used = n1, non-app used = n2
perm_diffs_app_install <- rep(0, 10000)
for (i in 1:10000) {
  perm_diffs_app_install[i] <- perm_fun(starbucks_clv$clv, 17, 17)
}

# p value for app installed is greater than not installed
p_value_app_install <- mean(perm_diffs_app_install > obs_app_install_diff)

app_install_label <- paste0("Observed Difference","\np-value: ", p_value_app_install)

perm_test_app_install_plot <- ggplot() + aes(perm_diffs_app_install) +
  geom_histogram(binwidth = 10, fill = "#009E73") +
  labs(title = "Permutation Test Results - App Installed Difference in CLV",
       subtitle = "Hypothesis: App installers have higher CLV than non-app installers",
       caption = "App Installed N = 17, App not Installed N = 17",
       x = "CLV Differences ($)",
       y = "Frequency") +
  geom_vline(xintercept = obs_app_install_diff, linetype = "dashed") +
  annotate("text", x = -75, y = 600, label = app_install_label)
print(perm_test_app_install_plot)
ggsave("plots/fig_perm_test_app_installed_plot.png", perm_test_app_install_plot)
#------------------------------------------------------------------------------

# CLV COMPARISON ACROSS SEGMENTS

#------------------------------------------------------------------------------
# mean clv for app install vs. no install
app_install_clv <- starbucks_clv %>%
  group_by(starbucks_app_installed) %>%
  summarize(clv = mean(clv)) %>% ungroup() %>%
  rename(segment = starbucks_app_installed)

# mean clv for app used vs. app not used
app_used_clv <- starbucks_clv %>%
  group_by(app_segment) %>%
  summarize(clv = mean(clv)) %>% ungroup() %>%
  rename(segment = app_segment)

# combine into single dataframe
stabucks_clv_segments <- rbind(app_used_clv, app_install_clv)

# comparison plot
clv_comp_plot <- ggplot(stabucks_clv_segments, aes(x = segment, y = clv, fill = segment)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0("$", round(clv, 0))), vjust = -0.5) +
  scale_x_discrete(labels = c("App not Installed", "App User", "Non-App User", "App Installed")) +
  scale_fill_manual(values = c("#009E73", "#F0E442", "#0072B2", "#CC79A7")) +
  labs(title = "Mean CLV by Starbucks Customer Segment",
       subtitle = "Significnt differences not detected between sub-groups (Installed vs. Not Installed; App User vs. Non-User)",
       x = "",
       y = "") +
  guides(fill = FALSE) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
print(clv_comp_plot)
ggsave("plots/fig_starbucks_clv_segments_plot.png", clv_comp_plot)
