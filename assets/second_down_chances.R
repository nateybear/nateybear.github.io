library(tidyverse)
library(stringr)
library(magrittr)
library(cfbfastR)
library(purrr)
library(tidymodels)
library(cowplot)
library(magick)
library(grid)

starts_valid <- . %>%
    first() %>%
    between(20, 80)

ends_valid <- \(play_type) {
    last_play <- last(play_type)
    last_play != "Kickoff" && !str_detect(last_play, "End")
}

drive_ending <- \(play_type){
    last_play <- last(play_type)
    if (str_detect(last_play, "Touchdown")) {
        "TD"
    } else if (last_play == "Field Goal Good") {
        "FG"
    } else {
        "Nothing"
    }
}

pull_drive_data <- function(game) {
    game %>%
        arrange(drive_number, play_number) %>%
        group_by(drive_number) %>%
        filter(starts_valid(yardline), ends_valid(play_type)) %>%
        mutate(result = drive_ending(play_type)) %>%
        ungroup() %>%
        filter(down == 2) %>%
        select(drive_id, yards_to_go = distance, yardline, result)
}

process_game <- function(home_team, away_team, week) {
    home_drives <- cfbd_plays(year = 2024, week = week, offense = home_team, defense = away_team)

    if (nrow(home_drives) == 0) {
        print(paste("No data for", home_team, "vs", away_team))
        return(tibble())
    }

    home_drives <- home_drives %>%
        pull_drive_data() %>%
        mutate(offense = home_team, defense = away_team)

    away_drives <- cfbd_plays(year = 2024, week = week, offense = away_team, defense = home_team) %>%
        pull_drive_data() %>%
        mutate(offense = away_team, defense = home_team)

    bind_rows(home_drives, away_drives)
}

sec_teams <- cfbd_team_info(conference = "SEC") %>% select(team = school)
sec_games <- cfbd_game_info(year = 2024, conference = "SEC") %>%
    select(game_id, home_team, away_team, week) %>%
    semi_join(sec_teams, by = c("home_team" = "team")) %>%
    semi_join(sec_teams, by = c("away_team" = "team"))

all_drives <- sec_games %>%
    mutate(game = pmap(list(home_team, away_team, week), process_game)) %>%
    select(game_id, game) %>%
    unnest(game)

offense_averages <- all_drives %>%
    distinct(game_id, drive_id, offense, result) %>%
    group_by(offense) %>%
    summarise(
        td_rate = mean(result == "TD"),
        fg_rate = mean(result == "FG")
    ) %>%
    arrange(desc(td_rate))

defense_averages <- all_drives %>%
    distinct(game_id, drive_id, defense, result) %>%
    group_by(defense) %>%
    summarise(
        td_allowed_rate = mean(result == "TD"),
        fg_allowed_rate = mean(result == "FG")
    ) %>%
    arrange(td_allowed_rate)

all_drives <- all_drives %>%
    left_join(offense_averages, by = c("offense")) %>%
    left_join(defense_averages, by = c("defense")) %>%
    mutate(across(where(is.character), as_factor))

write_csv(all_drives, "second_down_chances_2024.csv")

wf <- workflow() %>%
    add_model(rand_forest(mode = "classification", mtry = tune(), trees = tune(), min_n = tune())) %>%
    add_formula(result ~ yards_to_go + yardline + td_rate + fg_rate + td_allowed_rate + fg_allowed_rate)

set.seed(123)
folds <- vfold_cv(all_drives, v = 5)
results <- tune_grid(
    wf,
    resamples = folds,
    grid = grid_regular(
        mtry(c(2, 6)),
        trees(),
        min_n(),
        levels = 5
    )
)

model <- select_best(results, metric = "accuracy")

grid <- expand_grid(
    yards_to_go = seq(1, 15, by = 1),
    yardline = seq(20, 80, by = 1),
)

grid <- crossing(
    grid,
    offense_averages,
    defense_averages
) %>%
    filter(offense != defense) %>%
    mutate(across(where(is.character), as_factor))

grid_predictions <- wf %>%
    finalize_workflow(model) %>%
    last_fit(initial_split(all_drives)) %>%
    extract_workflow() %>%
    predict(new_data = grid, type = "prob") %>%
    bind_cols(grid) %>%
    group_by(yards_to_go, yardline) %>%
    summarise(
        td_chance = mean(.pred_TD),
        fg_chance = mean(.pred_FG),
        .groups = "drop"
    ) %>%
    mutate(expected_points = 7 * td_chance + 3 * fg_chance)

annotations <- grid_predictions %>%
    filter(yards_to_go %% 5 == 0, yardline > 30, yardline < 70) %>%
    group_by(yards_to_go) %>%
    summarize(expected_points = mean(expected_points)) %>%
    mutate(pct_change = (expected_points - lag(expected_points)) / lag(expected_points) * 100)


library(glue)
annotate_label <- function(expected_points, pct_change) {
    expected_points <- glue("Average Points: {round(expected_points,1)}")
    ifelse(
        is.na(pct_change),
        expected_points,
        {
            change_text <- ifelse(pct_change >= 0, "↑", "↓")
            change_value <- abs(round(pct_change, 1))
            glue("{expected_points} ({change_text}{change_value}%)")
        }
    )
}

write_csv(grid_predictions, "second_down_chances_predictions_2024.csv")

# Read SVG images at higher resolution
field_svg <- image_read_svg("field.svg", width = 1200)  # Increased from 400
down_marker_svg <- image_read_svg("down_marker.svg", width = 300)  # Increased from 100

# Create the main plot
main_plot <- ggplot(grid_predictions, aes(x = yardline, y = yards_to_go)) +
    geom_tile(aes(fill = expected_points), alpha = 0.9) +
    scale_fill_gradient2(
        low = "red",
        mid = "yellow",
        high = "green",
        midpoint = mean(grid_predictions$expected_points),
        name = "Expected Points",
    ) +
    scale_x_continuous(
        breaks = seq(0, 100, by = 10),
        limits = c(0, 100),
        expand = c(0, 0)
    ) +
    scale_y_reverse(expand = c(0, 0)) +
    geom_text(
        data = annotations,
        aes(x = 10, y = yards_to_go, label = annotate_label(expected_points, pct_change)),
        color = "black",
        size = 4,
    ) +
    theme_minimal() +
    theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.margin = margin(t = 20, r = 20, b = 0, l = 0),
        axis.text.y = element_text(size = 24)
    )

# Create field subplot for x-axis
# Convert SVG to a matrix for geom_raster
field_matrix <- as.raster(field_svg)
field_df <- expand.grid(
    x = seq(20, 80, length.out = dim(field_matrix)[2]),
    y = seq(0, 1, length.out = dim(field_matrix)[1])
)
field_df$raster <- as.vector(field_matrix)

field_plot <- ggplot() +
    annotation_raster(field_svg, xmin = 3, xmax = 90, ymin = 0, ymax = 1) +
    scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    theme_void() +
    theme(plot.margin = margin(t = 0, r = 20, b = 0, l = 0))

# Create down marker subplot for y-axis  
down_marker_plot <- ggplot() +
    annotation_custom(rasterGrob(down_marker_svg, interpolate = TRUE),
                     xmin = -Inf, xmax = Inf, ymin = 1, ymax = 15) +
    scale_y_continuous(limits = c(1, 15), trans = "reverse", expand = c(0, 0)) +
    theme_void() +
    theme(plot.margin = margin(t = 20, r = 0, b = 0, l = 0))

# Combine plots
plot_grid(
    down_marker_plot, main_plot,
    NULL, field_plot,
    ncol = 2, nrow = 2,
    rel_widths = c(0.1, 1),
    rel_heights = c(1, 0.3)
)

ggsave("second_down_chances_2024.png", width = 16, height = 9)
