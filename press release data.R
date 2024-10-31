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




# Plot the data
ggplot(weighted_issue_counts, aes(x = reorder(Question14, -weighted_count), y = weighted_count)) +
  geom_bar(stat = "identity", fill = "#2b8cbe") +  # Set a professional color
  coord_flip() +
  labs(title = "Voters' Top Issues (Weighted)", x = "Issue", y = "Weighted Number of Voters")

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


# Count the number of respondents for each race category
racial_makeup <- data %>%
  group_by(race) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Print the racial makeup table
print(racial_makeup)



# Calculate the weighted share of split-ticket voters (Harris and Hogan)
split_ticket_percentage <- data %>%
  mutate(weighted_split_ticket = ifelse(Question1 == "Kamala Harris (Democrat)" & Question2 == "Larry Hogan (Republican)", weight, 0)) %>%
  summarise(total_weighted_split_ticket = sum(weighted_split_ticket, na.rm = TRUE),
            total_weight = sum(weight, na.rm = TRUE)) %>%
  mutate(split_ticket_percentage = (total_weighted_split_ticket / total_weight) * 100) %>%
  select(split_ticket_percentage)

# Print the result
print(split_ticket_percentage)
