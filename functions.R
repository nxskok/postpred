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
