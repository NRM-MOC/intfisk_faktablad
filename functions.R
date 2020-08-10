

pretty_ci <- function(ci, loglin = FALSE){
  ci <- as.numeric(ci)
  if (loglin == FALSE){
    paste0("(", signif(ci[1], 2), ", ", signif(ci[2], 2), ")")
  }
  else
  {
    paste0("(", signif((exp(ci[1])-1)*100, 2), ", ", signif((exp(ci[2])-1)*100, 2), ")")
  }
}


hav_fig <- function(data, trend_start = 2002, first_year = 1980, last_year = 2019){
  xmin <- min(c(first_year, data$year))
  x_labs <- seq(xmin, last_year)
  d <- max(2, length(x_labs) %/% 10)
  x_labs[(x_labs %% d) != (x_labs %% d)[1]] <- ""
  
  get_p <- function(fit){
    if (nrow(coef(fit)) != 2)
      return(NA)
    coef(fit)["year","Pr(>|t|)"]
  }
  
  axis_label <- unique(data$axis_label)
  fig_title <- unique(data$fig_title)
  
  myColors <- c("#2280A7", "#2280A7", "#004371", "#3b847b", "#e84e0f", "#d1b738") # Profilfärger för havs- och vattenmynd
  names(myColors) <- c("Fjällbacka", "Fjällbacka, aug", "Fjällbacka, okt", "Holmöarna", "Kvädöfjärden", "Torhamn")
  colScale <- scale_colour_manual(values = myColors)
  fillScale <- scale_fill_manual(values = myColors)
  myShapes <- c(21, 21:25)
  names(myShapes) <- c("Fjällbacka", "Fjällbacka, aug", "Fjällbacka, okt", "Holmöarna", "Kvädöfjärden", "Torhamn")
  shapeScale <- scale_shape_manual(values = myShapes)
  fontSize <- 22
  
  p_list <- data %>% # Avgör ifall trend vid local är signifikant
    filter(year >= trend_start) %>% 
    group_by(locality) %>% 
    nest() %>% 
    mutate(lmod = map(data, ~summary(lm(value ~ year, data = .x))), 
           p = map_dbl(lmod, get_p), 
           significant = factor(p > 0.05, levels = c("TRUE", "FALSE"))) %>% 
    select(locality, significant) %>% 
    ungroup()
  
  plot_object <- data %>% 
    left_join(p_list, by = "locality") %>% 
    ggplot(aes(x = year, y = value, color = locality, fill = locality)) + 
    geom_point(aes(shape = locality), size = 3, alpha = .8) +
    geom_smooth(data = filter(data, year >= trend_start)  %>% 
                  left_join(p_list, by = "locality"), 
                aes(linetype = significant), method = "lm", alpha = .1) +
    theme_classic(base_size = fontSize) + 
    theme(panel.grid.major.y = element_blank(), 
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(), 
          legend.position = "top",
          axis.text.x = element_text(size = fontSize, angle = 90, vjust = 0.5),
          axis.text.y = element_text(size = fontSize),
          legend.text = element_text(size = fontSize),
          legend.title = element_blank(),
          plot.margin = unit(c(.5, 2, 0, .1), "cm")
    ) +
    scale_x_continuous(breaks = seq(xmin, last_year), limits = c(xmin, last_year), labels = x_labs) +
    colScale + fillScale + shapeScale + 
    xlab("") + ylab(axis_label) + 
    guides(col = guide_legend(ncol = 3, byrow = TRUE), linetype = FALSE) + 
    ggtitle(fig_title) +
    scale_linetype_manual(values=c("longdash", "solid"), drop = FALSE)
  if (data$start_zero[1] == TRUE) {
    plot_object <- plot_object + scale_y_continuous(limits = c(0, NA),
                                expand = expansion(mult = c(0, 0.1)))
  }
  plot_object
}

hav_tab <- function(data, trend_start = 2002){
  filter(data, year >= trend_start) %>% 
    group_by(locality) %>% 
    nest() %>% 
    mutate(lm_fit = map(data, ~lm(value ~ year, data = .x)),
           trend = map_dbl(lm_fit, ~coefficients(.x)["year"]),
           ci = map_chr(lm_fit, ~confint(.x)["year", ] %>% pretty_ci),
           p_value = map_dbl(lm_fit, ~summary(.x)[["coefficients"]]["year", "Pr(>|t|)"]),
           r2 = map_dbl(lm_fit, ~summary(.x)[["r.squared"]])) %>% 
    ungroup() %>% 
    select(locality, trend, ci, p_value, r2)
}
                         