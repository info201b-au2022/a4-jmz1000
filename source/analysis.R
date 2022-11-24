library(tidyverse)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#
inc_data <- read.csv("C:\\Users\\Jeffrey Zeng\\Documents\\info201\\a4-jmz1000\\source\\incarceration_trends.csv") 

A_inc_data <- function() {
  a_mean <- mean(inc_data$aapi_pop_15to64, na.rm = TRUE)
  a_mean <- round(a_mean)
  return(a_mean)  
} 
B_inc_data <- function() {
  b_mean <- mean(inc_data$black_pop_15to64, na.rm = TRUE)
  b_mean <- round(b_mean)
  return(b_mean)
} 
L_inc_data <- function() {
  l_mean <- mean(inc_data$latinx_pop_15to64, na.rm = TRUE)
  l_mean <- round(l_mean)
  return(l_mean)  
}
N_inc_data <- function() {
  n_mean <- mean(inc_data$native_pop_15to64, na.rm = TRUE)
  n_mean <- round(n_mean)
  return(n_mean)  
}
W_inc_data <- function() {
  w_mean <- mean(inc_data$white_pop_15to64, na.rm = TRUE)
  w_mean <- round(w_mean)
  return(w_mean)
} 


A_jail_data <- function() {
  aj_mean <- mean(inc_data$aapi_male_prison_pop, na.rm = TRUE)
  aj_mean <- round(aj_mean)
  return(aj_mean)
}
B_jail_data <- function() {
  bj_mean <- mean(inc_data$black_male_prison_pop, na.rm = TRUE)
  bj_mean <- round(bj_mean)
  return(bj_mean)
}
L_jail_data <- function() {
  lj_mean <- mean(inc_data$latinx_male_prison_pop, na.rm = TRUE)
  lj_mean <- round(lj_mean)
  return(lj_mean)
}
N_jail_data <- function() {
  nj_mean <- mean(inc_data$native_male_prison_pop, na.rm = TRUE)
  nj_mean <- round(nj_mean)
  return(nj_mean)
}
W_jail_data <- function() {
  wj_mean <- mean(inc_data$white_male_prison_pop, na.rm = TRUE)
  wj_mean <- round(wj_mean)
  return(wj_mean)
}

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
  df <- inc_data %>%
    group_by(year) %>%
    summarise(total_jail_pop = sum(total_jail_pop, na.rm = TRUE))
return(df)   
}

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  jail_graph <- ggplot(data = get_year_jail_pop(), aes(x = year, y = total_jail_pop)) + 
    geom_bar(position = "dodge", stat = "identity") +
    ggtitle("Increase of Jail Population in U.S. (1970-2018)") +
    xlab("Year") +
    ylab("Total Jail Population")
  options(scipen = 999)
  return(jail_graph)   
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
get_jail_pop_by_states <- function(states) {
  df_2 <- inc_data %>%
    group_by(year, state) %>%
    filter(str_detect(state, paste(states, collapse = "|"))) %>%
    summarize(states, total_prison_pop = sum(total_prison_pop, na.rm = TRUE))
  return(df_2)
} 
states <- c("WA", "OR", "CA", "OH") 
get_jail_pop_by_states(states)

plot_jail_pop_by_states <- function(states) {
  get_plot2 <- ggplot(data = get_jail_pop_by_states(states), aes(x = year, y = total_prison_pop, group = state, color = state)) +
    geom_line(stat = "identity") + 
    ggtitle("Growth of US Prison Populations by State") +
    xlab("Year") +
    ylab("Total Prison Population")
  return(get_plot2)
}

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <Comparing increasing rates of population and prison over the years>
# See Canvas
#----------------------------------------------------------------------------#
growth_most_pop <- function() {
  df_3 <- inc_data %>%
    group_by(urbanicity) %>%
    summarize(total_jail_pop_rate = sum(total_jail_pop_rate, na.rm = TRUE))
    df_3 = df_3[-1,]
  return(df_3)
}
growth_most_pop()

plot_of_growth <- function() {
  plot_3 <- ggplot(data = growth_most_pop()) +
    geom_bar(mapping = aes(x=urbanicity, y = total_jail_pop_rate),
             stat = "identity") +
    ggtitle("Comparison of Environments") +
    xlab("Type of Environment") +
    ylab("Jail Population Rate")
  return(plot_3)
}

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

# W_prop <- inc_data %>%
#  mutate(w_proportion = W_jail_data()/W_inc_data()) %>%
#  group_by(state) %>%

#inquality_frame <- function() {
# df_4 <- inc_data %>%
#  return(df_4)
#}

###Didn't finish :(

  
