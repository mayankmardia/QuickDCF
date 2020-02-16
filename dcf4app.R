library(shiny)
library(shinythemes)
library(data.table)
source("dcf4.R")

######

all_input_choices <- c("Linear Trend", "Average", "No Growth (i.e. Constant)", "Constant at: (Enter Value in Millions)",
                       "Custom: (Enter 5 Values in Millions)", "Grows at: (Enter Percentage Growth)", "Custom: (Enter 5 Percentages for Each Year's Growth)")
tax_choices <- c("Implied", "Custom: (Enter % Tax Rate)")
term_choices <- c("Terminal Growth Rate", "EV/EBITDA multiple")


ui <- shiny::fluidPage(theme = shinytheme("flatly"), 
  h1("Quick DCF"),
    sidebarLayout(
      sidebarPanel(
        
        ################ UI for Ticker/Source/Historical Data#################
        textInput(inputId = "ticker", label = "Ticker"),
        selectInput(inputId = "source", label = "Data Source", choices = c("Zacks", "Nasdaq", "Morningstar", "Yahoo")),
        actionButton(inputId = "display_historical", "Display Historical Data"),
        
        hr(),
        
        
        ####### UI for Assumptions Choices ###########
        h4("ASSUMPTIONS"),
        selectInput(inputId = "rev_choice", 
                    label = "Revenue", 
                    choices = all_input_choices),
        uiOutput("rev"),
        selectInput(inputId = "cogs_choice", 
                    label = "COGS", 
                    choices = all_input_choices),
        uiOutput("cogs"),
        selectInput(inputId = "opex_choice",
                    label = "Operating Expenses", 
                    choices = all_input_choices),
        uiOutput("opex"),
        selectInput(inputId = "addl_choice", 
                    label = "Additional Income/Expenses", 
                    choices = all_input_choices),
        uiOutput("addl"),
        selectInput(inputId = "tax_choice", 
                    label = "Tax Rate", 
                    choices = tax_choices),
        uiOutput("tax"),
        selectInput(inputId = "depr_choice",
                    label = "Depreciation", 
                    choices = all_input_choices),
        uiOutput("depr"),
        selectInput(inputId = "capex_choice",
                    label = "CapEx", 
                    choices = all_input_choices),
        uiOutput("capex"),
        selectInput(inputId = "nwc_choice",
                    label = "NEGATIVE Change in Net Working Capital", 
                    choices = all_input_choices),
        uiOutput("nwc"),
        selectInput(inputId = "term_choice", 
                    label = "Terminal Value Determined By", 
                    choices = term_choices),
        uiOutput("term"),
        textInput(inputId = "wacc_value", 
                  label = "WACC/Discount Rate (Enter %)", 
                  value = "10"),
        actionButton(inputId = "project_future", "Project Future Cash Flows")),                   
                        
                      
       #######UI for Results ##############
                 
       mainPanel(
         h2(shiny::textOutput("compname")),
         wellPanel(),
         fluidRow(
           column(width = 6, shiny::tableOutput("historical_data_table")),
           column(width = 6, shiny::tableOutput("projected_data_table"))
         )
         
       )
                           
   )
)





  



server <- function(input, output) {
  
  ######### HELPER FUNCTION TO CREATE A GENERIC INPUT ELEMENT IF NEEDED BASED ON USER INPUT CHOICE ##############
  create_input_element <- function(choice, line_item) {
    ui_element <- switch(choice,
                         "Constant at: (Enter Value in Millions)" = textInput(inputId = sprintf("constant_%s_value", line_item), label = "Enter one value (in millions)."),
                         "Grows at: (Enter Percentage Growth)" = textInput(inputId = sprintf("percentage_%s_growth", line_item), label = "Enter % Growth i.e. 7 for 7%"), 
                         "Custom: (Enter 5 Values in Millions)" = textInput(inputId = sprintf("custom_%s_values", line_item), label = "Enter 5 values (in millions) seperated by commas."),
                         "Custom: (Enter % Tax Rate)" = textInput(inputId = "custom_tax_rate", label = "Enter % Tax Rate"),
                         "Custom: (Enter 5 Percentages for Each Year's Growth)" = textInput(inputId = sprintf("custom_%s_percentages", line_item), label = "Enter 5 Percentages (i.e. 5, 12, -6, etc.) seperated by commas."),
                         "EV/EBITDA multiple" = textInput(inputId = "evebitda_multiple", label = "Enter EV/EBITDA Multiple"), 
                         "Terminal Growth Rate" = textInput(inputId = "terminal_growth_rate", label = "Enter % Terminal Growth Rate"))
    
    ui_element
  }
  
  
  ################# DEFINE ALL UI OUTPUTS ###############
  output$rev <- renderUI({create_input_element(input$rev_choice,"rev")})
  output$cogs <- renderUI({create_input_element(input$cogs_choice, "cogs")})
  output$opex <- renderUI({create_input_element(input$opex_choice,"opex")})
  output$addl <- renderUI({create_input_element(input$addl_choice,"addl")})
  output$tax <- renderUI({create_input_element(input$tax_choice,"tax")})
  output$depr <- renderUI({create_input_element(input$depr_choice,"depr")})
  output$capex <- renderUI({create_input_element(input$capex_choice,"capex")})
  output$nwc <- renderUI({create_input_element(input$nwc_choice,"nwc")})
  output$term <- renderUI({create_input_element(input$term_choice, "term")})
  
  
  #convert line_item to df rowname
  df_line <- function(line_item) {
    switch(line_item,
           "rev" = "Revenue",
           "cogs" = "COGS",
           "opex" = "Operating Expenses",
           "addl" = "Add'l Income",
           "tax" = "Unlevered Tax",
           "depr" = "Depreciation",
           "capex" = "Capital Expenditures",
           "nwc" = "Negative Change in Net Working Capital")
  }
  
  calc_linear_trend <- function(line_item, df) {
    row <- as.numeric(df[df_line(line_item),])
    x <- c(seq_along(row))
    y <- row
    z <- lm(y ~ x)
    new <- data.frame(x = c(length(row):(length(row) + 4)))
    ret <- predict(z, new)
    ret
  }
  
  #calculate average of the line item
  calc_average <- function(line_item, df) {
    m <- unlist(df[df_line(line_item),])
    m <- mean(m, na.rm = TRUE)
    rep(m, 5)
  }
  
  #repeat last value 5 times
  calc_no_growth <- function(line_item, df) {
    as.numeric(rep(df[df_line(line_item), ncol(df)], 5))
  }
  
  #repeat input value 5 times
  calc_constant_at_value <- function(value) {
    value <- as.numeric(value)
    rep(value, 5)
  }
  
  #calculate percentage growth
  calc_percentage_growth <- function(percentage_as_string, line_item, df) {
    percentage_as_string <- as.numeric(percentage_as_string)
    last_value <- df[df_line(line_item), ncol(df)] #get the last historical value
    vec_return <- vector(mode = "numeric", length = 5)
    
    vec_return[1] <- last_value * (1 + (percentage_as_string / 100))
    vec_return[2] <- vec_return[1] * (1 + (percentage_as_string / 100))
    vec_return[3] <- vec_return[2] * (1 + (percentage_as_string / 100))
    vec_return[4] <- vec_return[3] * (1 + (percentage_as_string / 100))
    vec_return[5] <- vec_return[4] * (1 + (percentage_as_string / 100))
    
    vec_return
  }
  
  #return input values
  calc_custom_values <- function(values) {
    #split based on commas and return the numeric vector
    as.numeric(unlist(stringr::str_split(values, ',')))
  }
  
  
  calc_custom_percentages <- function(values, line_item, df) {
    percentages <- as.numeric(unlist(stringr::str_split(values, ',')))
    old_values <- as.numeric(unlist(df[df_line(line_item),]))
    vec_return <- vector(mode = "numeric", length = 5)
    
    vec_return[1] <- old_values[5] * (1 + (percentages[1] / 100))
    vec_return[2] <- vec_return[1] * (1 + (percentages[2] / 100))
    vec_return[3] <- vec_return[2] * (1 + (percentages[3] / 100))
    vec_return[4] <- vec_return[3] * (1 + (percentages[4] / 100))
    vec_return[5] <- vec_return[4] * (1 + (percentages[5] / 100))
    
    vec_return
    
  }
  find_next_five_values <- function(choice, line_item, df) {
    switch(choice,
           "Linear Trend" = calc_linear_trend(line_item, df),
           "Average" = calc_average(line_item, df),
           "No Growth (i.e. Constant)" = calc_no_growth(line_item, df),
           "Constant at: (Enter Value in Millions)" = calc_constant_at_value(input[[sprintf("constant_%s_value", line_item)]]),
           "Grows at: (Enter Percentage Growth)" = calc_percentage_growth(input[[sprintf("percentage_%s_growth", line_item)]], line_item, df),
           "Custom: (Enter 5 Values in Millions)" = calc_custom_values(input[[sprintf("custom_%s_values", line_item)]]),
           "Custom: (Enter 5 Percentages for Each Year's Growth)" = calc_custom_percentages(input[[sprintf("custom_%s_percentages", line_item)]],
                                                                                              line_item,
                                                                                              df))
  }
  
  
  #########define behavior when we click
  observeEvent(input$project_future, {
    
    df <- getDataNasdaq(input$ticker)
  
    
    ############################ GET PROJECTED VALUES ######################################
    ###calculate new values
    new_rev_values <- find_next_five_values(input$rev_choice, "rev", df)
    new_cogs_values <- find_next_five_values(input$cogs_choice, "cogs", df)
    new_opex_values <- find_next_five_values(input$opex_choice, "opex", df)
    new_addl_values <- find_next_five_values(input$addl_choice, "addl", df)
    new_depr_values <- find_next_five_values(input$depr_choice, "depr", df)
    new_capex_values <- find_next_five_values(input$capex_choice, "capex", df)
    new_nwc_values <- find_next_five_values(input$nwc_choice, "nwc", df)
    
    ###calculate new opin and new ebit
    new_gross_profit <- new_rev_values + new_cogs_values
    new_opin_values <- new_rev_values + new_cogs_values + new_opex_values 
    new_ebit_values <- new_rev_values + new_cogs_values + new_opex_values + new_addl_values
    
    ####calculate new unlevered_tax
    old_ebit <- unlist(df["EBIT",])
    old_unlevered_tax <- unlist(df["Unlevered Tax",])
    old_unlevered_tax <- old_unlevered_tax * -1 ##make it not negative so we can get a positve tax rate
    implied_rate <- mean(old_unlevered_tax / old_ebit) * 100 ###multiply by 100 to get percentage
    
    #get the tax_rate, new unlevered tax, and new unlevered net income
    tax_rate <- switch(input$tax_choice,
                       "Implied" = implied_rate, 
                       "Custom: (Enter % Tax Rate)" = as.numeric(input$custom_tax_rate))
    
    new_unlevered_tax <- new_ebit_values * (tax_rate / 100)
    new_unlevered_tax <- -1 * new_unlevered_tax
    new_unlevered_net_income <- new_ebit_values + new_unlevered_tax
    
    #calculate the new free cash flow
    new_free_cash_flow <- new_unlevered_net_income + new_depr_values + new_capex_values + new_nwc_values
    
    ######put all the new rows together
    new_df <- rbind(data.frame(), new_rev_values, new_cogs_values, new_gross_profit, new_opex_values, new_opin_values, new_addl_values,
                    new_ebit_values, new_unlevered_tax, new_unlevered_net_income, new_depr_values, new_capex_values, new_nwc_values, new_free_cash_flow)
    
    rownames(new_df) <- c("Revenue", "COGS", "Gross Profit", "Operating Expenses",
                          "Operating Income", "Add'l Income", "EBIT", "Unlevered Tax",
                          "Unlevered Net Income", "Depreciation", "Capital Expenditures",
                          "Negative Change in Net Working Capital", "Free Cash Flow")

    colnames(new_df) <- c("t+1", "t+2", "t+3", "t+4", "t+5")

    View(new_df)
    ##############################################################################################
    
    ##################### INCORPORATE TV, DISCOUNT CASH FLOWS, AND CALCULATE SHARE PRICE #########
    
    ######get wacc######
    
    wacc <- as.numeric(input$wacc_value)
    #######calculate the terminal value
    ebitda <- new_df["EBIT", ncol(new_df)] - new_df["Depreciation", ncol(new_df)]
    fcf <- new_df["Free Cash Flow", ncol(new_df)]
      
    tv <- switch(
      input$term_choice,
      "EV/EBITDA multiple" = ebitda * as.numeric(input$evebitda_multiple),
      "Terminal Growth Rate" = fcf / ((wacc - as.numeric(input$terminal_growth_rate)) / 100)
    )
    
    ########discount the cash flows to get the EV value today##############
    all_fcf <- new_df["Free Cash Flow",]
    
    yr1 <- all_fcf[1]
    yr2 <- all_fcf[2]
    yr3 <- all_fcf[3]
    yr4 <- all_fcf[4]
    yr5 <- all_fcf[5] + tv
    r <- wacc / 100
    R <- 1 + r
    
    ev <- (yr1 / R) + (yr2 / (R^2)) + (yr3 / (R^3)) + (yr4 / (R^4)) + (yr5 / (R^5))
    #########calculate net debt####################
    
    net_debt <- getNetDebt(input$ticker)
    
    ##########subtract net debt from EV to get equity value#############
    equity_value <- ev - net_debt
    
    ##########divide by number of shares to get share price###########
    share_price <- equity_value / getSharesOutstanding(input$ticker)
    
    ##############################################################################################
    
    #######################     FORMAT ALL OF THE OUTPUT ELEMENTS           #####################
    
    
  
    
  })
  
  
  
}

shinyApp(ui = ui, server = server)
