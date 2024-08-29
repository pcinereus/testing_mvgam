## ---- load_libraries
library(tidyverse)
library(mvgam)
library(simstudy)
## ----end
## ---- load_functions
source("../R/design_functions.R")
## ----end

## Simulated data
{
  ## ---- define parameters 
  intercept <- 20
  n_reefs <- 6
  n_sites <- 3
  var_reefs <- 5
  var_sites <- 2
  n_species <- 3
  var_species <- 10
  species_effect <- cbind(value = c(0, 5, 10))
  rownames(species_effect) <- paste0("Sp.", 1:3)
  n_times <- 10
  mean_time_interval <- 1
  var_time_interval <- 0
  first_time <- 2000
  theta <- c(0.1, 0.8, 0.6, 0.4, 0.6, 0.9, 0.9)
  knots <- c(0.25, 0.5, 0.75)
  degree <- 3
  newrange <- "-5;5"
  noise.var <- 10
  y_sigma <- 3
  ## ----end
  {
    ## ---- define reefs
    d1 <- defData(
      varname = "Z0", dist = "normal",
      formula = 0, variance = var_reefs, id = "Reef"
    )
    d1 <- defData(d1,
      varname = "nSites", dist = "normal", formula = n_sites,
      variance = 0
    )
    set.seed(123)
    dat <- genData(n_reefs, d1)
    dat
    ## ----end
    ## ---- define sites
    dat <- genCluster(dat,
      cLevelVar = "Reef",
      numIndsVar = "nSites", level1ID = "Site"
    )
    d2 <- defDataAdd(
      varname = "Z1",
      dist = "normal", formula = 0, variance = var_sites
    )

    dat <- addColumns(d2, dat)
    dat
    ## ----end
    ## ---- define species
    dat <- genCluster(dat,
      cLevelVar = "Site",
      numIndsVar = n_species, level1ID = "ID"
    )
    ## d2 <- defDataAdd(
    ##   varname = "B_Species",
    ##   dist = "normal", formula = 0, variance = var_species
    ## )
    d2 <- defDataAdd(varname = "Species", dist = "trtAssign", formula = "1;1;1", variance = "Site")
    dat <- addColumns(d2, dat)
    dat <- genFactor(dat, varname = "Species", labels = paste0("Sp.", 1:n_species), replace = TRUE)
    ## ----end
    ## ---- define times
    ## number of measurements per individual
    def <- defData(
      varname = "nCount",
      dist = "nonrandom",
      formula = n_times
    )
    ## mean interval between measurements
    def <- defData(
      def,
      varname = "mInterval",
      dist = "nonrandom",
      formula = mean_time_interval,
      variance = 0
    )
    ## variance of interval between measurements
    def <- defData(def,
      varname = "vInterval",
      dist = "nonrandom",
      formula = var_time_interval,
    )
    dat <- addColumns(def, dat)
    dat[, id := ID]
    dat <- addPeriods(dat)
    dat
    ## ----end
    ## ---- define splines
    dat <- genSpline(dt = dat,
      newvar = "Y",
      predictor = "time",
      theta = theta,
      knots = knots,
      degree = degree,
      newrange = newrange,
      noise.var = noise.var)
    dat
    ## ----end
    ## ---- define Gaussian response 
    def3 <- defDataAdd(
      varname = "mu", dist = "normal",
      formula = "..intercept + Z0 + Z1 + ..species_effect[fSpecies, 1] + Y", variance = y_sigma
    )
    dat <- addColumns(def3, dat) |>
      mutate(
        Reef = factor(paste0("Reef", Reef)),
        Site = factor(paste0("S", Site)),
        Time = time + 2000,
        Species = fSpecies
      ) |>
      dplyr::select(Reef, Site, Time, Species, Y = mu) |>
      as_tibble()
    dat
    saveRDS(dat, file = "../data/simulated_data.rds")
    ## ----end
  }
  ## ----end
}


## ---- load_data
dat <- readRDS(file = "../data/simulated_data.rds")
## ----end


## ---- fig-visualise_design_1
Design_diagram(
  form = Y ~ 1 + (1 | Reef / Site / Time),
  data = dat,
  add_obs = FALSE,
  Colour = "Species",
  label_obs = TRUE
)
## ----end
## Design_diagram(
##   form = Y ~ 1 + (1 | Reef / Site /Time / Species),
##   data = dat,
##   add_obs = FALSE,
##   Colour = "Species",
##   label_obs = TRUE,
##   edge_filter = alist(Site == "S1")
## )
## ---- fig-visualise_design_2
Design_diagram(
  form = Y ~ 1 + (1 | Reef / Site /Time / Species),
  data = dat,
  add_obs = FALSE,
  Colour = "Species",
  label_obs = TRUE,
  edge_filter = alist(Time == 0.5 | Site == "S1")
)
## ----end


# Exploratory data analysis

## ---- fig-eda_1
ggplot(dat, aes(
  x = lubridate::date_decimal(Time),
  y = Y,
  colour = Species, group = paste(Site, Species)
)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~Reef) +
  scale_x_datetime("") +
  theme_bw()
## ----end

## ---- fig-eda_2
ggplot(dat, aes(x = lubridate::date_decimal(Time),
  y = Y,
  colour = Species)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~Reef +Site) +
  scale_x_datetime("") +
  theme_bw()
## ----end


# Data preparation

## ---- data preparation
dat <-
  dat |> 
  mutate(
    series = factor(Species),
    reef_site = factor(paste0(Reef, Site)),
    time = as.numeric(factor(Time))
  )
## ----end

## ---- analysis exploration 1
get_mvgam_priors(Y ~ 1,
                 data = dat,
                 family = gaussian())
## ----end


## ---- analysis exploration 2
dat2 <-
  dat |>
  group_by(time) |>
  summarise(Y = mean(Y))

get_mvgam_priors(Y ~ 1,
                 data = dat2,
                 family = gaussian())
## ----end

## ---- analysis exploration 3
dat3 <-
  dat |>
  group_by(Reef, time) |>
  summarise(Y = mean(Y))

get_mvgam_priors(Y ~ 0 + s(Reef, bs = "re"),
                 data = dat3,
                 family = gaussian())

get_mvgam_priors(Y ~ 0,
  trend_formula = ~ s(Reef, bs = "re"),
                 data = dat3,
                 family = gaussian())

## ----end

