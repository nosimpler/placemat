library(tidyverse)
library(broom)
library(NMF)

#TODO: breaks if any input columns start with W or H
#TODO: output columns are unsorted
#TODO: better variable names


replw <- function(str) {str_replace(str, 'V', 'W')}
replh <- function(str) {str_replace(str, 'V', 'H')}

tidy_inmf <- function(df, n, val, h = c(), w = c()){
  df <- ungroup(df)
  hrowname <- select(df, h)
  wcolname <- select(df, w)
  usecols <- c(h, w, val)
  id_cols <- setdiff(names(df), usecols)

  mat <- df %>%

    pivot_wider(names_from = h, values_from = val) %>%
    select(where(is.numeric), -names(wcolname), -id_cols) %>%
    as.matrix()

  nm  <- iterate_nmf(mat, n)
  H <- nm %>%
    pluck("fit") %>%
    pluck("H") %>%
    t() %>%
    as_tibble() %>%
    rename_with(replh) %>%
    cbind(hrowname) %>%
    pivot_longer(starts_with("H"), names_to = "H", values_to = "Hvalue")
  W <- nm %>%
    pluck("fit") %>%
    pluck("W") %>%
    as_tibble() %>%
    rename_with(replw) %>%
    cbind(wcolname) %>%
    pivot_longer(starts_with('W'), names_to = "W", values_to = "Wvalue")

  cbind(W,H)
}


df <- tribble(~TIME, ~AXIS, ~FILE1, ~FILE2, ~FILE3,
        1, 'x', 1, 2, 3,
        1, 'y', 4, 5, 6,
        1, 'z', 7, 8, 9,
        2, 'x', 3, 2, 1,
        2, 'y', 6, 5, 4,
        2, 'z', 12, 8, 7
        )

df <- df %>%
  pivot_longer(cols = starts_with('FILE'), names_to = "FILE", values_to = 'COUNTS')

h <- c('TIME', 'AXIS')
w <- 'FILE'
tn <- tidy_inmf(df, 2, h = h, w = w, val = 'COUNTS')

ggplot(tn, aes(x = TIME, y = Hvalue, color = W, shape = FILE))+
  geom_line()+
  facet_grid(FILE~AXIS)

ggplot(tn, aes(x = TIME, y = Wvalue, color = W, shape = FILE))+
  geom_line()+
  facet_grid(FILE~AXIS)



