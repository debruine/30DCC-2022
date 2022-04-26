# display debugging messages in R (if local) 
# and in the console log (if running in shiny)
debug_msg <- function(...) {
  is_local <- Sys.getenv('SHINY_PORT') == ""
  in_shiny <- !is.null(shiny::getDefaultReactiveDomain())
  txt <- toString(list(...))
  if (is_local) message(txt)
  if (in_shiny) shinyjs::runjs(sprintf("console.debug(\"%s\")", txt))
}

debug_sprintf <- function(fmt, ...) {
  debug_msg(sprintf(fmt, ...))
}

## day6 functions ----

day6_data <- function(crs = "+proj=wintri") {
  data_na <- readr::read_csv("data/share-of-individuals-using-the-internet.csv",
                      col_names = c("country", "code", "year", "it_net_users"),
                      skip = 1, show_col_types = FALSE) %>%
    mutate(code = recode(code, SSD = "SDS", .default = code)) %>% 
    pivot_wider(names_from = year, 
                values_from = it_net_users) %>%
    pivot_longer(cols = -c(country, code),
                 names_to = "year",
                 values_to = "it_net_users", 
                 names_transform = list(year = as.integer))
  
  # world <- rnaturalearth::ne_countries(returnclass = "sf", scale = "medium") %>%
  #   lwgeom::st_transform_proj(crs = crs)
  # saveRDS(world, "data/world.rds")
  world <- readRDS("data/world.rds")
  
  data_map_int_no_missing <- left_join(world, data_na, 
                                       by = c("gu_a3" = "code")) %>%
    select(country, year, code = gu_a3, it_net_users, geometry) %>%
    filter(!is.na(year)) %>%
    mutate(it_net_users_no_na = it_net_users,
           missing = is.na(it_net_users)) %>%
    arrange(code, year) %>%
    group_by(country) %>%
    fill(it_net_users_no_na, .direction = "down") %>%
    ungroup()
  
  # fix south sudan
  sudan <- data_map_int_no_missing %>%
    filter(country == "Sudan",
           year <= 2012) %>%
    pull(it_net_users_no_na)
  
  ss_rows <- which(data_map_int_no_missing$country == "South Sudan" & 
                     data_map_int_no_missing$year <= 2012)
  
  nona <- data_map_int_no_missing$it_net_users_no_na
  nona[ss_rows] <- sudan
  
  data_map_int_final <- data_map_int_no_missing %>%
    mutate(it_net_users_no_na = nona)
  
  data_map_int_final
}

crop_coords <- function(crs = "+proj=wintri") {
  # translate and crop coordinates
  trans_coords <- st_sfc(
    st_point(c(-1.4e7, -6.5e6)), # lower left lat and lon
    st_point(c(2e7, 1e7)),       # upper right lat and lon
    crs = crs) %>%
    st_transform(crs = crs) %>%
    st_coordinates()
  
  coord_sf(
    datum = NULL, 
    xlim = trans_coords[,'X'], 
    ylim = trans_coords[,'Y'], 
    expand = FALSE
  )
}

day6_plot <- function(data, filter_year = 2019) {
  data %>%
    filter(year == filter_year) %>%
    ggplot() + 
    geom_sf(mapping = aes(fill = it_net_users_no_na/100),
            size = .1) +
    crop_coords() +
    scale_fill_viridis_c(
      name = NULL,
      limits = c(0, 1),
      breaks = seq(0, 1, .1),
      labels = function(x) scales::percent(x, accuracy = 1),
      guide = guide_colorbar(
        label.position = "top", 
        barheight = unit(.1, "in"),
        frame.colour = "black",
        ticks.colour = "black"
      )
    ) +
    ggthemes::theme_map() +
    theme(
      legend.background = element_blank(),
      legend.position = "bottom",
      legend.key.width = unit(.33, "snpc")
    )
}

## day18 functions ----

get_dtm <- function() {
  sw <- data.frame(word = c(stop_words$word, 0:2030, "oecd", "countries"))
  
  readRDS("data/oecd_covid_insights.rds") %>%
    filter(p != "Follow us (Social Media):") %>%
    unnest_tokens(word, p) %>%
    anti_join(sw, by = "word") %>%
    count(url, word, sort = TRUE) %>%
    cast_dtm(document = url, term = word, value = n)
}

get_lda <- function(dtm, k = 6) {
  dtm %>%
    LDA(k = k, control = list(seed = 8675309))
}

get_top_terms <- function(lda, n_terms = 7) {
  tidy(lda, matrix = "beta") %>%
    group_by(topic) %>%
    slice_max(beta, n = n_terms) %>% 
    ungroup() %>%
    arrange(topic, -beta)
}

day18_plot <- function(top_terms, topics = list()) {
  names(topics) <- seq_along(topics)
  
  top_terms %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free_y",
               labeller = as_labeller(topics)) +
    scale_x_continuous(breaks = seq(0, 1, .01)) +
    scale_y_reordered() +
    labs(x = NULL, y = NULL) +
    scale_fill_hue() +
    theme_minimal(base_size = 14)
}