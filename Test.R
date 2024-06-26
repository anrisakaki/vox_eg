# Load necessary libraries
library(dplyr)
library(lubridate)

# Set seed for reproducibility
set.seed(123)

# Parameters
N <- 1000  # Total number of calls
num_counterparties <- 10  # Minimum number of counterparties each employee speaks to
max_counterparties <- 15  # Maximum number of counterparties each employee speaks to
predominant_counterparties <- 3  # Number of counterparties that are predominant

# Define employee names and counterparties
employee_names <- c("Oliver", "George", "Harry", "Jack", "Jacob", "Noah", "Charlie", "Muhammad", "Thomas", "Oscar",
                    "William", "James", "Henry", "Leo", "Alfie", "Joshua", "Freddie", "Archie", "Ethan", "Calum")

counterparties <- c("Goldman Sachs", "JP Morgan", "Morgan Stanley", "Citigroup", "Bank of America",
                    "Barclays", "Credit Suisse", "Deutsche Bank", "UBS", "BNP Paribas",
                    "HSBC", "Wells Fargo", "Jefferies", "Nomura", "Macquarie",
                    "RBC Capital Markets", "Santander", "Mizuho", "SMBC", "ING",
                    "Societe Generale", "Unicredit", "BNY Mellon", "State Street", "TP ICAP",
                    "ICAP", "BGC Partners", "CME Group", "Tullett Prebon", "Virtu Financial")

# Generate employee data
employee_data <- data.frame(
  employee_id = seq_along(employee_names),
  employee_name = employee_names
)

generate_counterparties <- function() {
  num_counterparties <- sample(num_counterparties:max_counterparties, 1)
  predominant <- sample(counterparties, predominant_counterparties)
  other <- sample(setdiff(counterparties, predominant), num_counterparties - predominant_counterparties)
  c(predominant, sample(other, num_counterparties, replace = TRUE))  # Shuffle to mix predominant and other counterparties
}

counterparty_data <- replicate(N, generate_counterparties())

counterparty_data <- unlist(counterparty_data)

counterparty_data <- counterparty_data[1:N]

counterparty_data <- sample(counterparty_data)

hsbc_indices <- sample(1:N, N * 0.3)
counterparty_data[hsbc_indices] <- "HSBC"

start_date <- as.Date("2023-01-01")
end_date <- as.Date("2023-12-31")
dates <- sample(seq.Date(start_date, end_date, by = "day"), N, replace = TRUE)
uk_dates <- format(dates, "%d/%m/%Y")

df1 <- data.frame(
  call_id = 1:N,
  date = uk_dates,
  employee_name = rep(employee_data$employee_name, each = ceiling(N / nrow(employee_data))),
  employee_id = rep(employee_data$employee_id, each = ceiling(N / nrow(employee_data))),
  counterparty = counterparty_data
)

df1 <- df1[1:N, ]

# Save dataset to CSV
write.csv(df1, "vox_eg.csv", row.names = FALSE)

####################
# SUMMARISING DATA #
####################

emp_sum <- df1 %>% 
  group_by(employee_name, counterparty) %>% 
  summarise(n = n())

emp_sum_monthly <- df1 %>%
  mutate(month = month(date)) %>% 
  group_by(employee_name, counterparty, month) %>% 
  summarise(n = n(), .groups = 'drop') 

calls_sum <- df1 %>% 
  group_by(counterparty) %>% 
  summarise(n = n())

#####################
## VISUALISING DATA #
#####################

ggplot(dplyr::filter(emp_sum, employee_name == "Calum"), aes(x = reorder(counterparty, -n), y = n)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Number of Calls Made by Calum to Each Counterparty",
       x = "Counterparty",
       y = "Number of Calls") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(dplyr::filter(emp_sum_monthly, employee_name == "Calum"), aes(x = month, y = n)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "No. of calles made by Calum, by month",
       x = "Month",
       y = "No. of calls") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
