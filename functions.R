get_lrdates <- function() {
  list.files("../sim-league", ".rds") %>%
    enframe(name = NULL, value = "file_name") %>%
    separate(file_name, into = c("league", "id", "the_date"), sep = ";") %>%
    drop_na(id) %>%
    group_by(league, id) %>%
    slice_max(the_date, n = 2) %>%
    slice(2) %>%
    mutate(dt = ymd_hms(the_date)) %>%
    ungroup() %>%
    filter(id == 1) %>%
    select(-the_date, -id)
}

make_league_info <- function(league_ids, last_dates) {
  league_ids %>%
    mutate(r = row_number()) %>%
    left_join(last_dates, by = c("the_country" = "league"))
}

# copied from pred_next_functions in ratings

generate_random <- function(posterior, id1, id2) {
  c1 <- as.character(id1)
  c2 <- as.character(id2)
  posterior %>% select(matches(c1), matches(c2), h) -> p
  if (ncol(p) < 4) return(list())
  p %>%
    rename(o1 = 1, d1 = 2, o2 = 3, d2 = 4) %>%
    mutate(lambda1 = exp(o1 - d2 + h), lambda2 = exp(o2 - d1)) %>%
    mutate(s1 = rpois(nrow(.), lambda1),
           s2 = rpois(nrow(.), lambda2)) %>%
    select(s1, s2) %>%
    mutate(result = case_when(
      s1 > s2 ~ 2,
      s1 == s2 ~ 1,
      s1 < s2  ~ 0)) %>%
    mutate(score_str = str_c(s1, "-", s2))
}

# from posterior predictive, compare actual

compare_actual <- function(randoms, actual1, actual2) {
  actual_sum <- actual1 + actual2
  actual_diff <- actual1 - actual2
  randoms %>% summarize(total_plus = sum(total >= actual_sum),
                        total_minus = sum(total <= actual_sum),
                        diff_plus = sum(diff >= actual_diff),
                        diff_minus = sum(diff <= actual_diff)) %>%
    mutate(across(everything()), ./nrow(randoms))
}


# random draws from posterior predictive

make_random_draws <- function(posterior, prior, tt1, tt2, actual1, actual2) {
  enframe(c(tt1, tt2), name = NULL) %>%
    left_join(prior, by = c("value" = "sw_id")) %>%
    pull(id) -> stan_ids
  generate_random(posterior, stan_ids[1], stan_ids[2]) %>%
    mutate(total = s1+s2,
           diff = s1 - s2) -> randoms
  randoms
  compare_actual(randoms, actual1, actual2)
}

make_llist <- function(league_info, league_id, prior_year = 2023) {
  league_info %>%
    filter(r == league_id) %>%
    transpose() %>% pluck(1) -> this
  this$lc <- tolower(this$the_country)
  this$year <- prior_year
  this
}

extract_games_to_ppd <- function(games, this) {
  games %>%
    filter(comp == this$id) %>%
    filter(time_stamp >= this$dt) %>%
    filter(stat == "FT") %>%
    separate(score, into = c("s1", "s2"), sep = " - ",convert = TRUE) %>%
    select(t1_name, t2_name, t1, t2, s1, s2)
}
