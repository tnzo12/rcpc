# plot for 'no plot'
no_plot <- function(text1, text2){
  plot_ly(type = NULL) %>%
    plotly::layout(xaxis = list(color = "#999999", showticklabels = FALSE),
                   yaxis = list(range = c(-1,1), color = "#999999", showticklabels = FALSE),
                   plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
                   autosize = TRUE) %>% 
    plotly::add_text(x=0, y=0.1, mode = 'text', text=text1, showlegend = FALSE, textfont=list(color='#999999')) %>%
    plotly::add_text(x=0, y=-0.1, mode = 'text', text=text2, showlegend = FALSE, textfont=list(color='#999999'))
}




# plot for pharmacokinetic / pharmacodynamic plot
pkd_plot <- function(iiv, noiiv, fit, colr, cmt, xlabel, ylabel){
  plot_ly(data = iiv, x=~Time, type=NULL, colors=c(colr,colr,'#17a2b8')) %>%
    plotly::layout(xaxis = list(title = xlabel,
                                color = "#999999"),
                   yaxis = list(title = ylabel,
                                color = "#999999"),
                   plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
                   autosize = TRUE) %>% 
    add_ribbons(ymax = ~P05, ymin = ~P95, color = ~condi, opacity=0.3, showlegend = FALSE, name = "5-95th pct") %>% # 5 to 95th percentile
    add_ribbons(ymax = ~P25, ymin = ~P75, color = ~condi, opacity=0.3, showlegend = FALSE, name = "25-75th pct") %>% # 25 to 75th percentile
    add_lines(y = ~P50, color = ~condi, opacity=0.3, showlegend = FALSE, name = "50th pct") %>% # 50th percentile line
    add_text(data = noiiv, x=~Time, y=~Estimated, mode='text', text=~date, textfont=list(color='#999999'), showlegend = FALSE) %>% # annotation, date
    add_lines(data = noiiv, x = ~Time, y = ~Estimated, color = ~condi, showlegend = FALSE, name = "Ind pred") %>% 
    add_markers(data = base::subset(fit, CMT==cmt),
                x= ~Time, y = ~DV,
                color = I(colr), # marker = list(color = colr),
                showlegend = FALSE, name = "observation") %>% # observation
    add_segments(data = base::subset(fit, CMT==cmt),
                 x= ~Time, xend = ~Time, y = ~DV, yend = ~DV-IRES,
                 color = I(colr),# line = list(color = colr),
                 line = list(dash = "dot"),
                 showlegend = FALSE, name = "prediction err") # error

}

# pharmacokinetic plot add-on (AUC)
auc_plot <- function(noiiv, xlabel, ylabel){
  plot_ly(data=noiiv, showlegend = FALSE) %>%
    add_bars(x = ~Time, y = ~auc_divide, name="AUC_tau", color = I("#DC3545"), opacity=0.6, showlegend = FALSE) %>%
    plotly::layout(xaxis = list(title = xlabel,
                                color = "#999999"),
                   yaxis = list(title = ylabel,
                                color = "#999999"),
                   plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
                   autosize = TRUE)
}

# visual parameter diagnostics
vis_param <- function(prm_iivs){
  
  plot_ly(data = prm_iivs, x = ~Param, y = ~Z.score, color = ~Param, opacity = 0.6,
          type = "box", showlegend=FALSE, boxpoints = "all", jitter = 0.25, text = ~paste("<b>Value: </b>",Value),
          boxmean = TRUE) %>%
    add_markers(data = prm_iivs %>% filter(is.na(sim.id)),
                y = ~Z.score, x = ~Param, color = ~Param, text = ~paste("<b>Value: </b>",Value),
                marker = list(symbol = "arrow-bar-down", size = 10), 
                inherit = FALSE, showlegend=FALSE) %>%
    layout(
      xaxis = list(
        title = "Parameters",
        color = "#999999"),
      yaxis = list(
        title = "Z-scores for simulated individuals",
        color = "#999999",
        showgrid = TRUE,
        zeroline = FALSE),
      plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)" 
    )
  
}

# heatmap plot
hm_plot <- function(res, color1, color2, xlab, ylab, zvalue){
  plot_ly(type=NULL, colors = colorRamp(c(color1, color2)) ) %>% 
    layout(xaxis = list(tickmode = "array", color = "#999999", 
                        tickvals = unique(res$dose), title = xlab, type="category", showgrid=FALSE),
           yaxis = list(tickmode = "array", color = "#999999",
                        tickvals = unique(res$tau), title = ylab, type="category", showgrid=FALSE),
           plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
           autosize = TRUE
    ) %>% 
    plotly::add_heatmap(data=res, x=~dose, y=~tau, z=~res[[zvalue]],
                        showlegend=FALSE, opacity = 0.75, showscale=FALSE,
                        xgap = 3.5, ygap = 3.5) %>%
    plotly::add_text(data=res, x=~dose, y=~tau, type="scatter", name="param val.",
                     showlegend=FALSE, text=~round(res[[zvalue]],2),
                     textfont=list(color = "#FFFFFF"))
}

# functions
auc <- function(x,time){ # linear-log trapezoidal method*
  dplyr::if_else(
    (x < dplyr::lead(x)), # ifelse
    (shift(time, type = "lead")- time) * (x + shift(x, type = "lead")) / 2, # up: linear trap.
    (shift(time, type = "lead")- time) * (x - shift(x, type = "lead")) / (log(x) - log(shift(x, type = "lead"))) # down: logarithmic trap.
  )
}
