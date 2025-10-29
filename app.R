# app.R
library(shiny)
library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)
library(plotly)
library(randomForest)
library(tidyr)

# ==========================================================
# ==== GLOBAL SECTION (runs once at app startup) ===========
# ==========================================================

# --- Load pre-trained model ---
fitted_model <- readRDS("fitted_model.rds")

# --- Define helper for prediction ---
predict_no_show <- function(newdata) {
  probs <- predict(fitted_model, newdata = newdata, type = "prob")
  if ("1" %in% colnames(probs)) {
    probs[, "1"]
  } else {
    probs[, ncol(probs)]
  }
}

# --- Define preprocessing (mirrors your Rmd) ---
preprocess_df <- function(df) {
  df %>%
    mutate(
      appt_time = lubridate::ymd_hms(appt_time),
      appt_made = lubridate::as_date(appt_made),
      lead_time_days = as.numeric(difftime(appt_time, appt_made, units = "days")),
      appt_hour = lubridate::hour(appt_time),
      appt_wday = lubridate::wday(appt_time, label = TRUE, week_start = 1),
      is_weekend = if_else(appt_wday %in% c("Sat", "Sun"), 1, 0)
    )
}

# --- Read and score the test dataset automatically ---
test_df <- read_csv("test_dataset.csv.gz", show_col_types = FALSE) %>%
  preprocess_df() %>%
  mutate(
    no_show_prob = predict_no_show(.),
    attendance_prob = 1 - no_show_prob, # calculate expected attendance upfront
    date = as.Date(appt_time),
    weekday = substr(weekdays(appt_time, abbreviate = TRUE), 1, 3)
  )

# --- Calculate the utilization for each provider + timeslot ---
grouped_test <- test_df %>% 
  group_by(provider_id, specialty, appt_time) %>% 
  summarise(count = n(),
            utilization = 1 - prod(no_show_prob),
            .groups = "drop") %>% 
  mutate(
    date = as.Date(appt_time),
    weekday = substr(weekdays(appt_time, abbreviate = TRUE), 1, 3)
  )


# ==========================================================
# ====================== UI ================================
# ==========================================================

ui <- fluidPage(
  titlePanel("St. Thomas Community Health Center Overbooking Tool"),
  sidebarLayout(
    sidebarPanel(
      dateInput(
        "week_start", "Select Week Start Date",
        value = min(test_df$date, na.rm = TRUE),
        min = min(test_df$date, na.rm = TRUE),
        max = max(test_df$date, na.rm = TRUE)
      ),
      uiOutput("provider_select_ui"),
      uiOutput("specialty_select_ui"),
      sliderInput("threshold", "Overbooking Threshold", 
                  min = 0, max = 1, value = 0.3, step = 0.01),
      actionButton("refresh", "Refresh view"),
      width = 3
    ),
    mainPanel(
      plotlyOutput("week_plot", height = "700px")
    )
  )
)

# ==========================================================
# ==================== SERVER ==============================
# ==========================================================

server <- function(input, output, session) {
  
  # Provider selector
  output$provider_select_ui <- renderUI({
    provs <- sort(unique(grouped_test$provider_id))
    selectInput("providers", "Providers:", 
                choices = c("All", provs), selected = "All", multiple = TRUE)
  })
  
  # Specialty selector
  output$specialty_select_ui <- renderUI({
    print(grouped_test)
    specs <- sort(unique(grouped_test$specialty))
    selectInput(
      "specialties", "Specialties:", 
      choices = c("All", specs), selected = "All", multiple = TRUE)
  })
  
  # Reactive filtered week utilization data
  week_data <- eventReactive(
    list(input$refresh, input$week_start, input$providers, input$specialties),{
      start_date <- as.Date(input$week_start)
      end_date <- start_date + 6
      df <- grouped_test %>%
        filter(date >= start_date & date <= end_date) # filter for correct week
      
    # filter to specified providers
    if (!is.null(input$providers) && !("All" %in% input$providers)) {
      df <- df %>% filter(provider_id %in% input$providers)
    }
    
    # filter to specified specialties
    if (!is.null(input$specialties) && !("All" %in% input$specialties)) {
      df <- df %>% filter(specialty %in% input$specialties)
    }
    
    df
  })
  
  # Reactive filtered week aggregate data
  daily_util_data <- eventReactive(list(input$refresh, input$week_start, input$providers, input$specialties), {
    start_date <- as.Date(input$week_start)
    end_date <- start_date + 6
    df <- grouped_test %>%
      filter(date >= start_date & date <= end_date) # filter for correct week

    # filter to specified providers
    if (!is.null(input$providers) && !("All" %in% input$providers)) {
      df <- df %>% filter(provider_id %in% input$providers)
    }
    
    # filter to specified specialties
    if (!is.null(input$specialties) && !("All" %in% input$specialties)) {
      df <- df %>% filter(specialty %in% input$specialties)
    }
    
    # calculate daily utilization (only including specified providers)
    df <- df %>% 
      group_by(date) %>% 
      summarize(providers = n_distinct(provider_id),
                utilization = sum(utilization) / (8*4*providers)) # 8 hours days with 15 minute timeslots for x number of providers
    
    df
  })
  
  # Bar visualization (Expected Utilization per time slot)
  output$week_plot <- renderPlotly({
    df <- week_data()
    if (nrow(df) == 0) {
      return(
        ggplotly(
          ggplot() + 
            ggtitle("No appointments in selected week") + 
            theme_minimal()))
    }
    
    daily_df <- daily_util_data()
    
    # collect variables for plot labels and spacing
    slot_mins <- 15
    slots_per_day <- 24 * 60 / slot_mins
    provs <- df %>% arrange(provider_id) %>% distinct(provider_id) %>% pull(provider_id)
    df <- df %>% mutate(provider_fac = factor(provider_id, levels = provs))

    # Calculate slot start
    df_slot <- df %>%
      mutate(slot_start = appt_time) 

    # Map grid coordinates
    min_date <- min(as.Date(df$appt_time))
    row_height <- 0.8  # total height available per provider row
    threshold_lines <- data.frame(
      provider_fac = as.numeric(provs),
      ymin = as.numeric(provs) - row_height/2,
      ymax = as.numeric(provs) - row_height/2 + input$threshold * row_height
    )
    
    # Create the 7 dates for the week using start date
    week_dates <- seq(as.Date(input$week_start), by = "day", length.out = 7)
    
    # Compute x-axis positions (center of first slot of each day)
    x_ticks <- (0:6) * slots_per_day + slots_per_day / 2


    df_slot <- df_slot %>%
      mutate(
        day_index = as.integer(as.Date(slot_start) - min_date),
        slot_index = as.numeric(difftime(slot_start, floor_date(slot_start, "day"), units = "mins")) / slot_mins,
        xmin = day_index * slots_per_day + slot_index,
        xmax = xmin + 1,
        ymin = as.numeric(provider_fac) - row_height/2,
        ymax = ymin + utilization * row_height,   # utilization now scaled to fit in row
        fill_color = ifelse(utilization >= input$threshold, "green", "red")
      )
    y_centers <- as.numeric(provs)  # provider_fac indices
    
    # Plot
    plt <- ggplot(df_slot) +
      geom_rect(aes(
        xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
        fill = fill_color, 
        # Implement hover feature to see appointment details
        text = paste0(
          "Provider: ", provider_id,
          "<br>Appt date: ", format(slot_start, "%Y-%m-%d"),
          "<br>Appt time: ", format(slot_start, "%H:%M"),
          "<br>Expected utilization: ", paste(round(utilization, 2)*100, "%", sep = "")
          )
        )
        ) +
      scale_fill_identity() +
      scale_y_continuous(
        breaks = y_centers,          # center labels on provider row
        labels = provs,              # add provider ID y-axis labels
        expand = c(0, 0)
      ) +
      scale_x_continuous(
        position = "top",
        breaks = (0:6) * slots_per_day + slots_per_day / 2,
        labels = paste0(
          format(week_dates, "%a %d-%b"),
          "<br>",
          round(daily_df$utilization * 100), "%"
        ),
        limits = c(0, 7 * slots_per_day),
        expand = c(0, 0)
      ) + 
      labs(x = "", y = "Provider") + # remove x axis label
      geom_segment(
        data = threshold_lines,
        aes(
          x = 0, xend = max(df_slot$xmax),
          y = ymax, yend = ymax
        ),
        color = "black", 
        linetype = "dashed", 
        size = 0.4) +
      theme_minimal() +
      theme(
        axis.text.x.top = element_text(angle = 45, hjust = 0),
        axis.title.x = element_blank(),
        axis.ticks.x.top = element_line(),
        legend.position = "none"
      )
    
    ggp <- ggplotly(plt, tooltip = "text") %>%
      layout(
        xaxis = list(
          side = "top",
          tickvals = ((0:6) + 0.5) * slots_per_day,
          ticktext = paste0(
            format(week_dates, "%a %d-%b"),
            "<br>",
            round(daily_df$utilization * 100), "%"
          ),
          tickmode = "array"
        ),
        showlegend = FALSE
      )
    ggp
  })
}

# ==========================================================
# ================== RUN APP ===============================
# ==========================================================
shinyApp(ui, server)
