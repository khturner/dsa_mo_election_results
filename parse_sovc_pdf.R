library(tidyverse)
library(pdftools)

pdf_lines <- pdf_text("Aug18-Citywide-Precinct-Breakdown.pdf")

dem_pages <- pdf_lines[313:336] %>% str_split("\n")
clay_pages <- pdf_lines[337:360] %>% str_split("\n")

rep1_dem_results <- tibble()
# Most candidate pages
current_ward <- ""
current_precinct <- ""
for (i in 1:length(dem_pages)) {
  current_page <- dem_pages[[i]][11:length(dem_pages[[i]])]
  current_page <- current_page[-which(current_page == "" | current_page == "CITYWIDE")] %>%
    str_trim
  for (j in 1:length(current_page)) {
    if (str_detect(current_page[j], "W [0-9][0-9] P [0-9][0-9]")) {
      current_ward <- str_replace(current_page[j], "W ([0-9][0-9]) P [0-9][0-9]", "\\1") %>% as.integer
      current_precinct <- str_replace(current_page[j], "W [0-9][0-9] P ([0-9][0-9])", "\\1") %>% as.integer
    } else {
      current_fields <- str_split(current_page[j], " +")[[1]]
      if (current_fields[1] == "HOUSE") {
        current_fields[1] <- "HOUSE VOTES"
        current_fields <- current_fields[-2]
      }
      rep1_dem_results <- tibble(ward = current_ward,
             precinct = current_precinct,
             vote_category = current_fields[1],
             reg_voters = current_fields[2] %>% as.integer,
             times_counted = current_fields[3] %>% as.integer,
             total_votes = current_fields[4] %>% as.integer,
             cori_bush = current_fields[5] %>% as.integer,
             demarco_davidson = current_fields[7] %>% as.integer,
             joshua_shipp = current_fields[9] %>% as.integer) %>%
        bind_rows(rep1_dem_results)
    }
  }
}

# Lacy Clay pages
clay <- tibble()
current_ward <- ""
current_precinct <- ""
for (i in 1:length(clay_pages)) {
  current_page <- clay_pages[[i]][11:length(clay_pages[[i]])]
  current_page <- current_page[-which(current_page == "" | current_page == "CITYWIDE")] %>%
    str_trim
  for (j in 1:length(current_page)) {
    if (str_detect(current_page[j], "W [0-9][0-9] P [0-9][0-9]")) {
      current_ward <- str_replace(current_page[j], "W ([0-9][0-9]) P [0-9][0-9]", "\\1") %>% as.integer
      current_precinct <- str_replace(current_page[j], "W [0-9][0-9] P ([0-9][0-9])", "\\1") %>% as.integer
    } else {
      current_fields <- str_split(current_page[j], " +")[[1]]
      if (current_fields[1] == "HOUSE") {
        current_fields[1] <- "HOUSE VOTES"
        current_fields <- current_fields[-2]
      }
      clay <- tibble(ward = current_ward,
                                 precinct = current_precinct,
                                 vote_category = current_fields[1],
                                 lacy_clay = current_fields[2] %>% as.integer) %>%
        bind_rows(clay)
    }
  }
}


# Remove grand total
rep1_dem_results <- rep1_dem_results[-(1:6),]
clay <- clay[-(1:6),]

rep1_dem_results_wide <- inner_join(rep1_dem_results, clay)
rep1_dem_results_long <- rep1_dem_results_wide %>%
  select(ward, precinct, reg_voters, total_votes, vote_category, cori_bush:lacy_clay) %>%
  pivot_longer(names_to = "candidate",
               values_to = "votes",
               cori_bush:lacy_clay) %>%
  mutate(candidate = str_replace(candidate, "_", " ") %>%
           str_to_title(),
         vote_category = str_to_title(vote_category))

write_csv(rep1_dem_results_long, "usrep1_dem_precinct_results.csv")

