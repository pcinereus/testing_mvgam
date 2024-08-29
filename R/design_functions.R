getTerms <- function(form) {
    terms <- form$fr %>% terms()
    resp <- terms %>% attr("response")
    ## fixed_terms <- terms %>% attr("varnames.fixed") %>% `[`(-1)
    fixed_terms <- attr(terms(formula(form)),"term.labels") %>% str_subset("\\|", negate = TRUE)
    if (length(fixed_terms) == 0) fixed_terms <- ""
    
    random_terms <- rev(terms %>% attr("predvars.random") %>% all.vars() %>% `[`(-1)) 
    factors <- terms %>% attr("factors") 
    if (any(fixed_terms == "")) {
        ## nested_terms <- (terms %>% attr("term.labels"))
        nested_terms <- rev(names(form$reTrms$flist))
        ## nested_terms <- (terms %>% attr("term.labels"))
    } else {
        ## nested_terms <- (terms %>% attr("term.labels") %>% str_subset(paste0("^",fixed_terms,"$", collapse = "|"), negate = TRUE))
        nested_terms <- rev(names(form$reTrms$flist) %>%
                            str_subset(paste0("^",fixed_terms,"$", collapse = "|"), negate = TRUE))
    }
    random_terms <-
        nested_terms %>%
        str_replace_all("\\(|\\)","") %>%
        str_split(":") %>%
        lapply(rev) %>%
        lapply(paste0, collapse = ":") %>%
        unlist() %>%
        str_replace(paste0(., ":", collapse = "|"), "")
   random_terms <- nested_terms %>% str_replace(":([^:]*$|\\(.*$)", "") 
    ## rev_terms <- random_terms %>% str_subset(":") %>% str_split(":") %>% lapply(rev) %>% lapply(paste0, collapse = ":") %>% unlist()
    ## random_terms <- c(random_terms, rev_terms) %>% unique()

    ## random_terms <- str_replace(nested_terms, paste0(":", nested_terms, collapse = "|"), "") %>%
    ##     str_replace_all("\\(|\\)","")
    list(resp = resp, nested_terms = nested_terms, fixed_terms = fixed_terms, random_terms = random_terms,
         factors = factors)
}
add_fixed_terms <- function(dat, fixed_terms, factors) {
    for (i in 1:length(fixed_terms)) {
        if (!fixed_terms[i] %in% colnames(dat) & fixed_terms[i] != "") {
            wch <- which(factors[,fixed_terms[i]]!=0) %>% names
            dat <-
                dat %>%
                mutate(!!sym(fixed_terms[i]) := paste(!!! syms(wch), sep = ":"))
        }
    }
    dat
}

add_other_terms <- function(dat, random_terms) {
    for (i in 1:length(random_terms)) {
        if (!random_terms[i] %in% colnames(dat) & random_terms[i] != "") {
            wch <- unlist(str_split(random_terms[[i]], ":"))
            dat <-
                dat %>%
                mutate(!!sym(random_terms[i]) := paste(!!! syms(wch), sep = ":"))
        }
    }
    dat
}
add_nested_terms <- function(dat, nested_terms, form) {
    for(i in 1:length(nested_terms)) {
        j <- length(nested_terms) - i + 1
        if (!nested_terms[i] %in% colnames(dat)) {
            dat <- dat %>%
                bind_cols(
                    ## !!sym(nested_terms[i]) := dimnames(form$reTrms$Ztlist[[j]])[[1]][form$reTrms$Ztlist[[j]]@i + 1]
                    !!sym(nested_terms[i]) := form$reTrms$flist[[nested_terms[i]]]
                )
        }
    }
    dat
}


## ---- Design_diagram function
Colour <- NULL
Design_diagram <- function(form, data, Filter = NA, edge_filter = NA, Colour = NULL, direction = "vertical",
                           add_obs = FALSE, label_obs = FALSE) {
    require(ggraph, quietly = TRUE, warn.conflicts = FALSE)
    require(igraph, quietly = TRUE, warn.conflicts = FALSE)
    require(ggplot2, quietly = TRUE, warn.conflicts = FALSE)
    dat <- as.data.frame(data) #form$fr
    if (add_obs) {
        form <- update(form, ~ . + (1|Obs))
        dat <- dat %>% mutate(Obs = factor(1:n()))
        }
    if(is.null(Colour)) {
        Colour <- 'Blank'
        dat <- dat %>% mutate(Blank = NA)
    }
    Clr <- Colour

    form <- lme4::lFormula(form, data = dat,
                           control=lme4::lmerControl(check.nobs.vs.nlev = "ignore",
                                                     check.nobs.vs.nRE = "ignore"))
    ## Get names of the various terms
    list2env(getTerms(form), envir = environment())
    ## Add nested terms to the data
    dat <- dat %>% add_nested_terms(nested_terms, form) 
    dat <- dat %>% add_fixed_terms(fixed_terms, factors) 
    dat <- dat %>% add_other_terms(random_terms) 

    ## ## Get the first level of edges
    ## edges <- data.frame(
    ##     from = 'Root',
    ##     to = unique(dat[,nested_terms[1]]),
    ##     Height = length(nested_terms) + 1,
    ##     Level = random_terms[1]) %>%
    ##     mutate(Name = to,
    ##            Label = Name,
    ##            Colour = ifelse(!is.na(to) & nested_terms[1] == Colour,
    ##                            unique(as.character(dat[,Colour])),
    ##                            NA)) %>%
    ##     unique()
    edges <- NULL
    ## Add the rest of the hierarchy
    for (i in 1:length(nested_terms)) {
        ##if (i == 1) next
        edges <- edges %>%
            bind_rows(
                dat %>%
                  ## filter(Nest == first(Nest)) %>%
                  {
                    if (length(Filter) > 0 & !is.na(Filter)) {
                      filter(., !!Filter[[1]])
                    } else {
                      .
                    }
                  } %>%
                  ## {
                  ##   if (length(edge_filter) > 0 & !is.na(edge_filter)) {
                  ##     mutate(., Flag = ifelse(!!edge_filter[[1]], 0, 1))
                  ##   } else {
                  ##     .
                  ##   }
                  ## } %>%
                  {
                    if (i == 1) {
                      mutate(.,
                        from = "Root",
                        to = !!sym(nested_terms[[i]]),
                        Label = !!sym(random_terms[[i]]),
                        Name = !!sym(nested_terms[[i]]),
                        Colour = !!sym(Colour)
                      ) %>%
                        dplyr::select(
                          from,
                          ## to = !!nested_terms[[i]],
                          ## Label = !!random_terms[[i]],
                          ## Name = nested_terms[[i]],
                          ## Colour := !!Colour,
                          ## from,
                          to,
                          Label,
                          Name,
                          Colour,
                          random_terms[1:i]
                          ## Flag
                        ) ## |>
                        ## head()
                    } # ,
                    ## mutate(., from = "Root")
                    else {
                      mutate(.,
                      from = !!sym(nested_terms[[i - 1]]),
                      to = !!sym(nested_terms[[i]]),
                      Label = !!sym(random_terms[[i]]),
                      Name = !!sym(nested_terms[[i]]),
                      Colour = !!sym(Colour)
                      ##   to = !!nested_terms[[i]],
                      ##   Label = !!random_terms[[i]],
                      ##   Name = nested_terms[[i]],
                      ## Colour := !!Colour
                      ) %>% 
                      dplyr::select(.,
                        ## from = !!nested_terms[[i - 1]],
                        ## to = !!nested_terms[[i]],
                        ## Label = !!random_terms[[i]],
                        ## Name = nested_terms[[i]],
                        ## Colour := !!Colour
                        from,
                        to,
                        Label,
                        Name,
                        Colour,
                        random_terms[1:i]
                        ## Flag
                        ## Colour = !!sym(Colour)) %>%
                      )
                    }
                    ## mutate(., from := !!nested_terms[[i-1]])
                  } %>%
                  mutate(Label = factor(Label)) %>%
                ## dplyr::select(from, #from = !!nested_terms[[i-1]],
                ##               to = !!nested_terms[[i]],
                ##               Label = !!random_terms[[i]],
                ##               Name = nested_terms[[i]],
                ##               Colour := !! Colour          #,
                ##               ## Colour = !!sym(Colour)) %>%
                ##               ) %>%
                ## mutate(Height = length(nested_terms) + 2 - i,
                mutate(Height = length(nested_terms) - i,
                       Level = random_terms[i],
                       ## Colour = ifelse(is.na(!!sym(Clr)), NA, as.character(!!sym(Clr)))
                       Colour = ifelse(!is.na(to) &  ( any(str_detect(random_terms[1:i], Clr))),
                                       as.character(!!sym(Clr)), NA)
                       ## Colour = ifelse(!is.na(to) &  ( str_detect(random_terms[[i]], as.character(Clr)) | Level == "Obs"),
                       ##                 as.character(Colour), NA)
                       ## Colour = ifelse(!is.na(to) &  str_detect(random_terms[[i]], Colour),
                       ##                 ## as.character(Label),
                       ##                unique(as.character(dat[,Colour])),
                       ##                NA)
                       ) %>% 
                distinct() %>%
                droplevels()
            )
    }

    if (length(edge_filter) > 0 & !is.na(edge_filter)) {
      edges <-
        edges |>
        filter(!!edge_filter[[1]] | is.na(!!sym(all.vars(edge_filter[[1]])[[1]])))
        ## filter(!!edge_filter[[1]] | is.na(!!edge_filter[[1]][[2]]))
        ## mutate(Flag = ifelse(!!edge_filter[[1]], 0, 1))
    }

    Coloured <- ifelse(all(is.na(edges$Colour)), FALSE, TRUE)
    
    vertices <- edges %>%
        dplyr::select(Name, Height, Level, Label) %>%
        distinct() %>%
        rbind(data.frame(Name = 'Root', Height = max(edges$Height)+1, Level = 'Root', Label = NA))
    
    if (!label_obs) {
        vertices <- vertices %>%
            mutate(Label = ifelse(Level == "Obs", NA, as.character(Label)))
    }
    ## edges <- edges %>% filter(from != "Root")
    heights <- edges %>% dplyr::select(Level, Height) %>% distinct()

    graph <- graph_from_data_frame(edges, directed = TRUE, vertices = vertices)                                          
    lyout <- create_layout(graph, "igraph", algorithm = "tree")
    ## lyout <- create_layout(graph, "dendrogram")
    ## g <- ggraph(graph, layout = "dendrogram", height = Height) +
    ## g <- ggraph(graph, layout = "manual", y = y, x = x) +
    g <- ggraph(lyout, layout = "manual", height = Height) +
        { if (Coloured) geom_edge_diagonal(aes(colour = Colour))} + 
        { if (!Coloured) geom_edge_diagonal()} + 
        ## geom_node_circle(aes(r = 1)) +
        ## geom_node_label(aes(label = Label, filter = !leaf)) +
        geom_node_label(aes(label = Label)) +
        ## geom_node_text(aes(label = Label, filter = leaf), angle = 90, hjust = 1, nudge_y = -0.1) +
        theme_graph() 

  if (direction == 'vertical') {
    g <- g +
    scale_y_continuous('', breaks = heights$Height, labels = heights$Level) +
    theme(
      axis.text.y = element_text(size = rel(2)),
      panel.grid.major.y = element_line(linetype = "dashed"),
      axis.line = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      ## axis.title.y = element_text(margin = margin(l = 5, unit = 'cm')),
      legend.position = 'top',
      ## legend.justification = c(1,1),
      legend.direction = 'horizontal') 
    } else {
      g <- g + scale_y_reverse('', breaks = heights$Height, labels = as.character(heights$Level)) +
        theme(
          axis.text.x = element_text(size = rel(2)),
          panel.grid.major.x = element_line(),
          axis.line = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          ## axis.title.x = element_text(margin = margin(l = 5, unit = 'cm')),
          legend.position = 'top',
          ## legend.justification = c(1,1),
          legend.direction = 'horizontal') +
        coord_flip()
    }
  
  g + guides(color = guide_none())
}

## ----end
