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
    attendance_prob = 1 - no_show_prob,
    date = as.Date(appt_time),
    weekday = substr(weekdays(appt_time, abbreviate = TRUE), 1, 3)
  )

grouped_test <- test_df %>% 
  group_by(provider_id, appt_time) %>% 
  summarise(count = n(),
            utilization = 1 - prod(no_show_prob),
            .groups = "drop") %>% 
  mutate(
    date = as.Date(appt_time),
    weekday = substr(weekdays(appt_time, abbreviate = TRUE), 1, 3)
  )

print(grouped_test)
# ==========================================================
# ====================== UI ================================
# ==========================================================

ui <- fluidPage(
  titlePanel("Clinic Weekly Appointment Dashboard"),
  sidebarLayout(
    sidebarPanel(
      dateInput(
        "week_start", "Select week start (Monday):",
        value = min(test_df$date, na.rm = TRUE),
        min = min(test_df$date, na.rm = TRUE),
        max = max(test_df$date, na.rm = TRUE)
      ),
      uiOutput("provider_select_ui"),
      sliderInput("threshold", "Overbook threshold (attendance probability):", 
                  min = 0, max = 1, value = 0.3, step = 0.01),
      actionButton("refresh", "Refresh view"),
      width = 3
    ),
    mainPanel(
      p("Hover over blocks to see details. Green = high attendance probability; red = low."),
      plotlyOutput("week_plot", height = "700px"),
      hr(),
      plotlyOutput("eu_plot", height = "200px")
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
  
  # Reactive filtered week data
  week_data <- eventReactive(list(input$refresh, input$week_start, input$providers), {
    start_date <- as.Date(input$week_start)
    end_date <- start_date + 6
    df <- grouped_test %>%
      filter(date >= start_date & date <= end_date)
    
    if (!is.null(input$providers) && !("All" %in% input$providers)) {
      df <- df %>% filter(provider_id %in% input$providers)
    }
    
    df
  })
  
  # Weekly visualization (Expected Utilization per time slot)
  output$week_plot <- renderPlotly({
    df <- week_data()
    if (nrow(df) == 0) {
      return(ggplotly(ggplot() + ggtitle("No appointments in selected week") + theme_minimal()))
    }
    
    slot_mins <- 15
    slots_per_day <- 24 * 60 / slot_mins
    provs <- df %>% arrange(provider_id) %>% distinct(provider_id) %>% pull(provider_id)
    df <- df %>% mutate(provider_fac = factor(provider_id, levels = provs))

    # 1 Aggregate by provider + slot time (sum of attendance probs = expected utilization)
    df_slot <- df %>%
      mutate(slot_start = appt_time) #%>%
      # group_by(provider_id, provider_fac, slot_start) %>%
      # summarise(expected_utilization = sum(attendance_prob, na.rm = TRUE), .groups = "drop") %>%
      # mutate(expected_utilization = pmin(expected_utilization, 1))  # cap at 100%

    # 2️⃣ Map grid coordinates
    min_date <- min(as.Date(df$appt_time))
    row_height <- 0.8  # total height available per provider row
    threshold_lines <- data.frame(
      provider_fac = as.numeric(provs),
      ymin = as.numeric(provs) - row_height/2,
      ymax = as.numeric(provs) - row_height/2 + input$threshold * row_height
    )
    
    
# Create the 7 dates for the week
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
    
    
    
    # 3️⃣ Plot
    plt <- ggplot(df_slot) +
      geom_rect(aes(
        xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
        fill = fill_color,
        text = paste0(
          "Provider: ", provider_id,
          "<br>Slot start: ", format(slot_start, "%Y-%m-%d %H:%M"),
          "<br>Expected utilization: ", round(utilization, 2)
        )
      )
     # color = "black", alpha = 0.9
     ) +
     # scale_fill_gradient(low = "red", high = "green", limits = c(0, 1), name = "Expected Utilization") +
      scale_fill_identity() +
      scale_y_continuous(
        breaks = y_centers,          # center labels on provider row
        labels = provs,              # provider IDs
        expand = c(0, 0)
      ) +
      labs(x = "Day / Time", y = "Provider") +
      geom_segment(data = threshold_lines,
                   aes(
                     x = 0, xend = max(df_slot$xmax),
                     y = ymax, yend = ymax
                   ),
                   color = "black", linetype = "dashed", size = 0.4) +
      theme_minimal() +
      scale_x_continuous(
        breaks = (0:6) * slots_per_day + slots_per_day / 2,  # center of each day
        labels = format(week_dates, "%a %d-%b"),
        limits = c(0, 7 * slots_per_day),                     # full week range
        expand = c(0, 0)
      ) +
      theme(
        axis.text.x = element_text(hjust = -20)
      )
      
    ggplotly(plt, tooltip = "text")
  })
  
  
  # # Expected Utilization plot (sum of attendance probs per day)
  # output$eu_plot <- renderPlotly({
  #   df <- week_data()
  #   if (nrow(df) == 0) {
  #     return(ggplotly(ggplot() + ggtitle("No data for EU") + theme_minimal()))
  #   }
  #   
  #   eu <- df %>%
  #     group_by(date) %>%
  #     summarise(EU = sum(attendance_prob, na.rm = TRUE))
  #   
  #   plt <- ggplot(eu, aes(x = date, y = EU, text = paste0("Date: ", date, "<br>Expected utilization: ", round(EU, 2)))) +
  #     geom_col(fill = "steelblue") +
  #     theme_minimal() +
  #     labs(x = "Date", y = "Expected Attended Appointments")
  #   
  #   ggplotly(plt, tooltip = "text")
  # })
}

# ==========================================================
# ================== RUN APP ===============================
# ==========================================================
shinyApp(ui, server)
