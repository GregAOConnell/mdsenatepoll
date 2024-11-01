library(tidyverse)
library(hrbrthemes)
data=read.csv("UNMD0007_OUTPUT_strings.csv")

hrbrthemes::import_roboto_condensed() # Run only once to import fonts

#Alsobrooks v. Hogan Head to Head
# Calculate the weighted share of the vote for each candidate in Question 2
vote_share <- data %>%
  group_by(Question2) %>%
  summarise(weighted_votes = sum(weight, na.rm = TRUE)) %>%
  mutate(vote_share = weighted_votes / sum(weighted_votes) * 100)%>%
  select(-weighted_votes)

prezvote_share <- data %>%
  group_by(Question1) %>%
  summarise(weighted_votes = sum(weight, na.rm = TRUE)) %>%
  mutate(prezvote_share = weighted_votes / sum(weighted_votes) * 100)%>%
  select(-weighted_votes)
prezvote_share

ballotmeasure= data %>%
  group_by(Question3) %>%
  summarise(weighted_votes = sum(weight, na.rm = TRUE)) %>%
  mutate(prezvote_share = weighted_votes / sum(weighted_votes) * 100)%>%
  select(-weighted_votes)
ballotmeasure

abortionpartyID <- data %>%
  group_by(Question3, partyID = pid3) %>%
  summarise(weighted_votes = sum(weight, na.rm = TRUE)) %>%
  group_by(partyID) %>%
  mutate(vote_share = weighted_votes / sum(weighted_votes) * 100) %>%
  select(-weighted_votes)%>%
  ungroup()
abortionpartyID
# Print the results
print(vote_share)



# Calculate the weighted share of the vote by candidate and demographic factors
partyIDsplit <- data %>%
  group_by(Question2, partyID = pid3) %>%
  summarise(weighted_votes = sum(weight, na.rm = TRUE)) %>%
  group_by(partyID) %>%
  mutate(vote_share = weighted_votes / sum(weighted_votes) * 100) %>%
  select(-weighted_votes)%>%
  ungroup()

# Print the results
print(partyIDsplit)

gendersplit= data %>%
  group_by(Question2, gender = gender) %>%
  summarise(weighted_votes = sum(weight, na.rm = TRUE)) %>%
  group_by(gender) %>%
  mutate(vote_share = weighted_votes / sum(weighted_votes) * 100) %>%
  select(-weighted_votes)%>%
  ungroup()
gendersplit

racesplit=data %>%
  group_by(Question2, race = race) %>%
  summarise(weighted_votes = sum(weight, na.rm = TRUE)) %>%
  group_by(race) %>%
  mutate(vote_share = weighted_votes / sum(weighted_votes) * 100) %>%
  select(-weighted_votes)%>%
  ungroup()
racesplit

econcondition=data %>%
  group_by(Question18, race = race) %>%
  summarise(weighted_votes = sum(weight, na.rm = TRUE)) %>%
  group_by(race) %>%
  mutate(vote_share = weighted_votes / sum(weighted_votes) * 100) %>%
  select(-weighted_votes)%>%
  ungroup()
econcondition

# Count the number of respondents for each race category
racial_makeup <- data %>%
  group_by(race) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Print the racial makeup table
print(racial_makeup)

# Summarize the weighted frequency of each top issue
weighted_issue_counts <- data %>%
  group_by(Question14) %>%  # Replace Top_Issue with the actual column name for top issues
  summarise(weighted_count = sum(weight, na.rm = TRUE)) %>%
  arrange(desc(weighted_count))%>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  arrange(desc(percentage))

weighted_issue_counts

# Plot the data
ggplot(weighted_issue_counts, aes(x = reorder(Question14, -weighted_count), y = weighted_count)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip coordinates for readability if there are many issues
  labs(title = "Voters' Top Issues (Weighted)", x = "Issue", y = "Weighted Number of Voters") +
  theme_minimal()


# Calculate the weighted share of split-ticket voters (Harris and Hogan)
split_ticket_percentage <- data %>%
  mutate(weighted_split_ticket = ifelse(Question1 == "Kamala Harris (Democrat)" & Question2 == "Larry Hogan (Republican)", weight, 0)) %>%
  summarise(total_weighted_split_ticket = sum(weighted_split_ticket, na.rm = TRUE),
            total_weight = sum(weight, na.rm = TRUE)) %>%
  mutate(split_ticket_percentage = (total_weighted_split_ticket / total_weight) * 100) %>%
  select(split_ticket_percentage)

# Print the result
print(split_ticket_percentage)

intdata=read_csv("UNMD0007_OUTPUT_numeric.csv")
#Question 20

# Convert the interval variable to numeric, setting "I don't know" to NA
q20top <- intdata %>%
  mutate(Question20 = as.numeric(ifelse(Question20 == "don't know", NA, Question20)))


summary(q20top$Question20)  # View summary to see if NAs are handled
q20top <- q20top %>%
  summarise(weighted_avg = sum(Question20 * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE))

#Question 21
q21top=intdata%>%
  mutate(Question21 = as.numeric(ifelse(Question21 == "don't know", NA, Question21)))
q21top <- q21top %>%
  summarise(weighted_avg = sum(Question21 * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE))
q21top

#Question 22
# Calculate the weighted percentage for each response option
question22_weighted_percentages <- data %>%
  summarise(across(starts_with("Question22_"), 
                   ~ sum(weight[. == "selected"], na.rm = TRUE) / sum(weight, na.rm = TRUE) * 100,
                   .names = "percentage_{.col}")) %>%
  pivot_longer(cols = everything(), names_to = "Option", values_to = "Weighted_Percentage") %>%
  arrange(desc(Weighted_Percentage))

# Print the result
print(question22_weighted_percentages)

#Q24
q24top=intdata%>%
  mutate(Question24 = as.numeric(ifelse(Question24 == "don't know", NA, Question24)))
q24top <- q24top %>%
  summarise(weighted_avg = sum(Question24 * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE))
q24top

# Add an age column and create 10-year age buckets
data1 <- data %>%
  mutate(age = 2024 - birthyr,  # Calculate age
         age_group = cut(age, breaks = seq(0, 100, by = 10), 
                         labels = paste(seq(0, 90, by = 10), seq(10, 100, by = 10) - 1, sep = "-"),
                         right = FALSE))  # Create age buckets

datafunc=data %>%
  mutate(partyID = case_when(
    pid3 == "Democrat" ~ "Democrat",
    pid3 == "Republican" ~ "Republican",
    TRUE ~ "Other"  # All other values fall into "Other"
  ))

