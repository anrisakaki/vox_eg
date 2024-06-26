library(dplyr)
library(stringi)


# 20 unique employees
# 80 unique counterparties 
# 80% of Calum's calls are to HSBC
# 30% of all calls from firm are to HSBC 

set.seed(123)

N <- 3000
employee_names <- c("Oliver", "George", "Harry", "Jack", "Jacob", "Noah", "Charlie", "Muhammad", "Thomas", "Oscar",
                    "William", "James", "Henry", "Leo", "Alfie", "Joshua", "Freddie", "Archie", "Ethan", "Calum")
counterparties <- c("Goldman Sachs", "JP Morgan", "Morgan Stanley", "Citigroup", "Bank of America",
                    "Barclays", "Credit Suisse", "Deutsche Bank", "UBS", "BNP Paribas",
                    "HSBC", "Wells Fargo", "Jefferies", "Nomura", "Macquarie",
                    "RBC Capital Markets", "Santander", "Mizuho", "SMBC", "ING",
                    "Societe Generale", "Unicredit", "BNY Mellon", "State Street", "TP ICAP",
                    "ICAP", "BGC Partners", "CME Group", "Tullett Prebon", "Virtu Financial",
                    "Jane Street", "Citadel Securities", "Flow Traders", "Optiver", "DRW Trading",
                    "Jump Trading", "XR Trading", "Hudson River Trading", "IMC Trading", "Mako Global",
                    "Tower Research Capital", "Two Sigma Securities", "Millennium Management", "Point72", "Bridgewater Associates",
                    "Renaissance Technologies", "AQR Capital", "D. E. Shaw", "Elliott Management", "Citadel",
                    "Tudor Capital", "Man Group", "BlueCrest Capital", "Marshall Wace", "Brevan Howard",
                    "Third Point", "Balyasny Asset Management", "Och-Ziff Capital", "Caxton Associates", "Adage Capital",
                    "Lone Pine Capital", "Tiger Global Management", "Baupost Group", "Anchorage Capital", "Viking Global",
                    "Pershing Square", "PDT Partners", "Soros Fund Management", "Appaloosa Management", "Glenview Capital",
                    "ValueAct Capital", "Coatue Management", "Farallon Capital", "King Street Capital", "Greenlight Capital")

employee_ids <- 1:20

employee_data <- data.frame(
  employee_id = rep(employee_ids, each = N / length(employee_ids)),
  employee_name = rep(employee_names, each = N / length(employee_names))
)

counterparty_data <- sample(counterparties, N, replace = TRUE)

hsbc_indices <- sample(1:N, N * 0.3)
counterparty_data[hsbc_indices] <- "HSBC"

calum_indices <- which(employee_data$employee_name == "Calum")
calum_hsbc_indices <- sample(calum_indices, length(calum_indices) * 0.8)
counterparty_data[calum_hsbc_indices] <- "HSBC"

df1 <- data.frame(
  employee_name = employee_data$employee_name,
  employee_id = employee_data$employee_id,
  counterparty = counterparty_data
)

# Rest of calls are randomly distributed 
df1$ipc_counterparty <- stri_rand_strings(N, 10)

# Save dataset to CSV
write.csv(df1, "vox_eg.csv", row.names = FALSE)
