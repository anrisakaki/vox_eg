library(dplyr)
library(stringi)

set.seed(123)

N <- 1000
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

set_counterparties_for_employee <- function(employee_id, num_counterparties) {
  
  employee_counterparties <- sample(counterparties, num_counterparties)
  
  rep(employee_counterparties, length.out = N / length(employee_ids))
}

counterparty_data <- unlist(lapply(employee_ids, function(id) {
  set_counterparties_for_employee(id, sample(10:15, 1))
}))

hsbc_indices <- sample(1:N, N * 0.3)
counterparty_data[hsbc_indices] <- "HSBC"

calum_indices <- which(employee_data$employee_name == "Calum")
calum_hsbc_indices <- sample(calum_indices, length(calum_indices) * 0.8)
counterparty_data[calum_hsbc_indices] <- "HSBC"

start_date <- as.Date("2023-01-01")
end_date <- as.Date("2023-12-31")
dates <- sample(seq.Date(start_date, end_date, by = "day"), N, replace = TRUE)
uk_dates <- format(dates, "%d/%m/%Y")

df1 <- data.frame(
  call_id = 1:N,
  date = uk_dates,
  employee_name = employee_data$employee_name,
  employee_id = employee_data$employee_id,
  counterparty = counterparty_data
)

# Save dataset to CSV
write.csv(df1, "vox_eg.csv", row.names = FALSE)
