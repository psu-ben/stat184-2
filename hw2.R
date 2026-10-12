library(dplyr); library(tidyr); library(tibble)
library(readr); library(stringr); library(forcats); library(purrr)

sales_raw <- tibble(
  sale_id = 1:8,
  customer_id = c("C1", "C2", "C1", "C3", "C2", "C4", "C5", "C2"),
  item_raw = c(" Pen", "PEN ", "Notebook", " pen ",
               "FOLDER", "Folder ", " ERASER ", "pen"),
  amount_raw = c("$10.00", "$20.00", NA, "$15.00",
                 "$30.00", "$25.00", NA, "$20.00"),
  discount_raw = c("0%", "10%", "5%", "0%", "20%", "10%", "0%", "0%"),
  code_raw = c("ST-101", "ON-102", "ST-103", "ON-104",
               "bad", "ST-106", "ON-107X", "on-108"),
  tags = c("gift;urgent", "online", "gift", "member",
           "online;member", "gift", "member", "gift;member")
)

#1a

sales_clean <- sales_raw |>
  mutate(
    item_clean    = str_to_lower(str_squish(item_raw)),
    amount        = parse_number(amount_raw),
    discount_rate = parse_number(discount_raw)/100,
    net_amount    = amount * (1 - discount_rate),
    size          = case_when(
      is.na(net_amount) ~ "unrecorded",
      net_amount >=20 ~ "large",
      .default = "small"
    )
  )

sales_clean |> select(sale_id, item_clean, amount, discount_rate, net_amount, size)


#1b

sales_codes <- sales_clean |>
  mutate(
    code_clean = str_to_upper(str_trim(code_raw)),
    valid_code = str_detect(code_clean, "^(ST|ON)-[0-9]{3}$")
  )

invalid_codes <- sales_codes |>
  filter(!valid_code) |>
  select(sale_id, code_raw, code_clean)

invalid_codes

valid_code_parts <- sales_codes |>
  filter(valid_code) |>
  separate_wider_delim(code_clean, delim = "-", names = c("channel", "number"), cols_remove = FALSE)
valid_code_parts |> select(sale_id, code_clean, channel, number)

sale_tags <- sales_clean |>
  select(sale_id, tags) |>
  separate_longer_delim(tags, delim = ";")

sale_tags

names(valid_code_parts)
#separate_wider_delim() splits one column into several and leaves the row count unchanged
#separate_longer_delim() splits one value into several rows, so the unit of a row changes from one sale to one sale-tag pair


#1c

customer_summary <- sales_clean |>
  summarise(
    sales_n     = n(),
    known_n     = sum(!is.na(net_amount)),
    large_n     = sum(net_amount >= 20, na.rm = TRUE),
    large_share = if_else(known_n == 0, NA_real_, large_n/ known_n),
    .by = customer_id
  ) |>
  arrange(customer_id)

customer_summary

#c1 large_share = 0/1 = 0
#numerator is c1 sales with a known net amount of at least 20, sale 1 has net 10 so thats 0
#denominator is c1 sales with a known net amount, sale 3 is NA so it gets dropped, leaving just sale 1
#the NA sale is dropped from both sides, it does not count as "not large"

sales_factors <- sales_clean |>
  mutate(
    item_factor = fct_infreq(factor(item_clean)),
    item_group  = fct_lump_n(factor(item_clean), n = 2, other_level = "Other")
  )

levels(sales_factors$item_factor)
levels(sales_factors$item_group)
sales_factors |> count(item_factor)
sales_factors |> count(item_group)

#changing level order is rearranging data when displayed, no data is changed, can be reversed
#combining categories rewrites row values, the merge cannot be undone

reports_raw <- tibble(
  form = c("A", "A", "A", "B", "B", "B"),
  line = c(1, 2, 3, 1, 2, 3),
  customer_id = c("C1", NA, "C2", "C3", NA, "C4"),
  round = c(1, 3, 1, 1, 2, 1),
  primary_amount = c(10, -999, 20, NA, 0, 30),
  backup_amount = c(NA, 15, 25, NA, 99, NA)
)
customers <- tibble(
  customer_id = c("C1", "C2", "C3", "C5"),
  region = c("North", "South", NA, "West")
)


#2a 

reports_raw <- tibble(
  form = c("A", "A", "A", "B", "B", "B"),
  line = c(1, 2, 3, 1, 2, 3),
  customer_id = c("C1", NA, "C2", "C3", NA, "C4"),
  round = c(1, 3, 1, 1, 2, 1),
  primary_amount = c(10, -999, 20, NA, 0, 30),
  backup_amount = c(NA, 15, 25, NA, 99, NA)
)

customers <- tibble(
  customer_id = c("C1", "C2", "C3", "C5"),
  region = c("North", "South", NA, "West")
)

reports_labeled <- reports_raw |>
  arrange(form, line) |>
  group_by(form) |>
  fill(customer_id, .direction = "down") |>
  ungroup()

reports_clean <- reports_labeled |>
  mutate(
    primary_clean = na_if(primary_amount, -999),
    amount        = coalesce(primary_clean, backup_amount)
  )

reports_clean |>
  select(form, line, customer_id, round, primary_amount, backup_amount, amount)

reports_complete <- reports_clean |>
  select(customer_id, round, amount) |>
  mutate(recorded_row = TRUE) |>
  complete(customer_id, round = 1:3, fill = list(recorded_row = FALSE))

reports_complete

n_original <- sum(reports_complete$recorded_row)
n_added    <- sum(!reports_complete$recorded_row)
na_before  <- sum(is.na(reports_clean$amount))
na_after   <- sum(is.na(reports_complete$amount))

tibble(
  original_rows  = n_original,
  added_rows     = n_added,
  missing_before = na_before,
  missing_after  = na_after
)

#a recorded primary value is what the form actually reported, so we keep it even when its 0
#coalesce only reaches for backup_amount when primary_clean is NA
#0 is a real recorded amount, not missing info, so swapping in the backup would overwrite

#explicit missing: c3 round 1, the row is there but primary and backup are both NA so amount is NA
#implicit missing: c1 round 2, no row at all before complete(), c1 only has rounds 1 and 3
#missing_after = missing_before + added_rows, complete() just turns implicit missing into explicit missing


#2b

sum(duplicated(customers$customer_id))
sum(is.na(customers$customer_id))

reports_clean |>
  count(customer_id, round) |>
  filter(n > 1)

customers_marked <- customers |>
  mutate(customer_found = TRUE)

reports_joined <- reports_clean |>
  left_join(
    customers_marked,
    by           = join_by(customer_id),
    relationship = "many-to-one",
    na_matches   = "never"
  ) |>
  mutate(customer_found = coalesce(customer_found, FALSE))

reports_joined |>
  select(customer_id, round, amount, region, customer_found)

only_in_reports <- reports_clean |>
  anti_join(customers, by = join_by(customer_id), na_matches = "never") |>
  distinct(customer_id)

only_in_customers <- customers |>
  anti_join(reports_clean, by = join_by(customer_id), na_matches = "never") |>
  distinct(customer_id)

only_in_reports
only_in_customers

#c3 and c4 both show region NA so region alone cant tell them apart
#c3 has customer_found TRUE, the customer exists and its region field is just blank, missing attribute
#c4 has customer_found FALSE, nothing matched at all, so region is NA as a side effect, missing record
#without the flag a failed join looks the same as a join that matched an empty field


#2c

customers_bad <- bind_rows(customers,
                           tibble(customer_id = "C2", region = "East"))

reports_duplicated <- reports_clean |>
  left_join(
    customers_bad,
    by           = join_by(customer_id),
    relationship = "many-to-many"
  )

tibble(
  stage        = c("before", "after"),
  n_rows       = c(nrow(reports_clean), nrow(reports_duplicated)),
  total_amount = c(sum(reports_clean$amount,      na.rm = TRUE),
                   sum(reports_duplicated$amount, na.rm = TRUE))
)

reports_duplicated |>
  count(customer_id, round) |>
  filter(n > 1)

reports_wide <- reports_complete |>
  select(customer_id, round, amount) |>
  pivot_wider(
    names_from   = round,
    values_from  = amount,
    names_prefix = "round_"
  )

reports_wide

reports_wide |>
  pivot_longer(
    cols           = starts_with("round_"),
    names_to       = "round",
    values_to      = "amount",
    values_drop_na = TRUE
  ) |>
  nrow()

#repeated report is c2 round 1 with amount 20
#c2 has one report but customers_bad has two c2 rows, so the join spits that report out twice
#rows go 6 -> 7 and total goes 75 -> 95, the extra 20 isnt new data, its one measurement counted twice

#many-to-one would error instead of duplicating, it claims each report matches at most one customer row and thats false for c2
#many-to-many doesnt fix anything, it just stops the complaint
#both regions (south and east) are still there, the join has no way to pick so it keeps both
#relationship is a check, not a repair, the real fix is deduping customers_bad first

#prediction: 5 rows left, one per non-missing amount (c1 r1 10, c1 r3 15, c2 r1 20, c3 r2 0, c4 r1 30)
#the other 7 of the 12 cells are NA and get dropped
#dropping NA removes rows, it doesnt fill in values, the unknown amounts are still unknown just hidden
#it also undoes what complete() gave us, c3 round 1 was a real submitted report with an unknown amount
#while c1 round 2 was never submitted at all, after the drop both are just gone


#3a

two_sample_t <- function(x, y, equal_var = FALSE, alpha = 0.05) {
  n_x  <- length(x)
  n_y  <- length(y)
  xbar <- mean(x)
  ybar <- mean(y)
  s2_x <- var(x)
  s2_y <- var(y)
  
  if (equal_var) {
    method <- "Pooled"
    s2_p   <- ((n_x - 1) * s2_x + (n_y - 1) * s2_y) / (n_x + n_y - 2)
    se     <- sqrt(s2_p * (1 / n_x + 1 / n_y))
    df     <- n_x + n_y - 2
  } else {
    method <- "Welch"
    se     <- sqrt(s2_x / n_x + s2_y / n_y)
    df     <- (s2_x / n_x + s2_y / n_y)^2 /
      ((s2_x / n_x)^2 / (n_x - 1) + (s2_y / n_y)^2 / (n_y - 1))
  }
  
  t_stat  <- (xbar - ybar) / se
  p_value <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)
  
  result <- list(
    method  = method,
    t_stat  = t_stat,
    df      = df,
    p_value = p_value,
    reject  = p_value < alpha
  )
  
  return(result)
}

x_check <- c(12, 15, 14, 10, 13)
y_check <- c(9, 11, 8, 10, 7, 12, 9, 8)

res_welch  <- two_sample_t(x_check, y_check)
res_pooled <- two_sample_t(x_check, y_check, equal_var = TRUE)

res_welch
res_pooled

ref_welch  <- t.test(x_check, y_check, var.equal = FALSE)
ref_pooled <- t.test(x_check, y_check, var.equal = TRUE)

comparison <- data.frame(
  method    = rep(c("Welch", "Pooled"), each = 3),
  quantity  = rep(c("t_stat", "df", "p_value"), times = 2),
  mine      = c(res_welch$t_stat,  res_welch$df,  res_welch$p_value,
                res_pooled$t_stat, res_pooled$df, res_pooled$p_value),
  reference = unname(c(ref_welch$statistic,  ref_welch$parameter,  ref_welch$p.value,
                       ref_pooled$statistic, ref_pooled$parameter, ref_pooled$p.value))
)
comparison$abs_diff   <- abs(comparison$mine - comparison$reference)
comparison$under_1e_8 <- comparison$abs_diff < 1e-8

comparison
all(comparison$under_1e_8)


#all six differences are under 1e-8 for welch and pooled, so the function matches t.test()


#3b

B     <- 1000
n_x   <- 20
n_y   <- 40
alpha <- 0.05

scenarios <- data.frame(
  scenario = c("equal_null", "unequal_null",
               "equal_shift", "unequal_shift"),
  mean_x = c(0, 0, 0, 0),
  mean_y = c(0, 0, 1, 1),
  sd_x   = c(1, 3, 1, 3),
  sd_y   = c(1, 1, 1, 1),
  stringsAsFactors = FALSE
)

set.seed(2026)

total_rows  <- nrow(scenarios) * B * 2
scenario_v  <- character(total_rows)
replicate_v <- integer(total_rows)
method_v    <- character(total_rows)
t_stat_v    <- numeric(total_rows)
df_v        <- numeric(total_rows)
p_value_v   <- numeric(total_rows)
reject_v    <- logical(total_rows)

row_i <- 0L

for (s in seq_len(nrow(scenarios))) {
  for (b in seq_len(B)) {
    
    x <- rnorm(n_x, mean = scenarios$mean_x[s], sd = scenarios$sd_x[s])
    y <- rnorm(n_y, mean = scenarios$mean_y[s], sd = scenarios$sd_y[s])
    
    fits <- list(
      two_sample_t(x, y, equal_var = FALSE, alpha = alpha),
      two_sample_t(x, y, equal_var = TRUE,  alpha = alpha)
    )
    
    for (k in 1:2) {
      row_i <- row_i + 1L
      fit   <- fits[[k]]
      scenario_v[row_i]  <- scenarios$scenario[s]
      replicate_v[row_i] <- b
      method_v[row_i]    <- fit$method
      t_stat_v[row_i]    <- fit$t_stat
      df_v[row_i]        <- fit$df
      p_value_v[row_i]   <- fit$p_value
      reject_v[row_i]    <- fit$reject
    }
  }
}

simulation_results <- tibble(
  scenario  = scenario_v,
  replicate = replicate_v,
  method    = method_v,
  t_stat    = t_stat_v,
  df        = df_v,
  p_value   = p_value_v,
  reject    = reject_v
)

head(simulation_results)
nrow(simulation_results)

simulation_summary <- simulation_results |>
  summarise(
    mean_t         = mean(t_stat),
    sd_t           = sd(t_stat),
    rejection_rate = mean(reject),
    .by = c(scenario, method)
  ) |>
  arrange(scenario, method) |>
  mutate(across(c(mean_t, sd_t, rejection_rate), \(v) round(v, 3)))

simulation_summary

rate_matrix <- with(
  simulation_summary,
  tapply(rejection_rate, list(method, scenario), identity)
)

barplot(
  rate_matrix,
  beside      = TRUE,
  ylim        = c(0, 1),
  ylab        = "Rejection rate",
  main        = "Rejection rate by scenario and method",
  legend.text = rownames(rate_matrix),
  args.legend = list(x = "topleft", bty = "n")
)
abline(h = alpha, lty = 2)

#with equal variances both tests sit near 0.05 in equal_null so both hold their type I error, but in
#unequal_null pooled jumps to about 0.14 while welch stays near 0.05, since the big variance is on the
#small sample and that makes the pooled standard error too small.
#once the means differ by 1 both tests gain power, around 0.95 each in equal_shift, and in unequal_shift
#pooled rejects more often than welch.
#that isnt real power, the same too-small standard error that broke its null rate is what lifts it,
# it just rejects more often either way.
#power only means something once the type I error is right, so welch is the better default, it gives up a
#little power when variances really are equal in exchange for a correct error rate when they arent.
