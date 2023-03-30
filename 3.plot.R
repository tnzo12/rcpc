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
  plot_ly(data = iiv, x=~Time, type=NULL, colors=c(colr,colr,'#66CCCC')) %>%
    plotly::layout(xaxis = list(title = xlabel,
                                color = "#999999"),
                   yaxis = list(title = ylabel,
                                color = "#999999"),
                   plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
                   autosize = TRUE) %>% 
    add_ribbons(ymax = ~P5, ymin = ~P95, color = ~condi, opacity=0.3, showlegend = FALSE, name = "5-95th pct") %>% # 5 to 95th percentile
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
                 showlegend = FALSE, name = "prediction err") # error

}

#sim_q
#plot_ly(data = sim_q, x=~time, type=NULL, colors=c("#FF6666",'#999999')) %>%
#  plotly::layout(xaxis = list(title="hi",
#                              color = "#999999"
#                              ),
#                 yaxis = list(color = "#999999"),
#                 plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
#                 autosize = TRUE) %>% 
#  add_ribbons(ymax = ~p05, ymin = ~p95, color = ~condi, opacity=0.5) %>% # 5 to 95th percentile
#  add_ribbons(ymax = ~p25, ymin = ~p75, color = ~condi, opacity=0.5) %>% # 25 to 75th percentile
#  add_lines(y = ~p50, color = ~condi) %>% # 50th percentile line
#  
#  
#  add_markers(data = fit, x= ~TIME, y = ~DV) %>% # observation
#  add_segments(data = fit,
#               x= ~TIME, xend = ~TIME,
#               y = ~DV, yend = ~DV-IRES) # error

  
#  names(sim_q)  
#ggp <- ggplot(sim_res_noiiv) +
#  
#  scale_fill_manual(values=c(pk_color, '#999999')) +
#  scale_color_manual(values=c(pk_color, '#999999')) +
#  
#  geom_ribbon(data=sim_res_iiv, aes(x=Time, ymin=P5, ymax=P95, fill=condi), alpha=0.15) +
#  geom_ribbon(data=sim_res_iiv, aes(x=Time, ymin=P25, ymax=P75, fill=condi), alpha=0.25) +
#  geom_line(data=sim_res_iiv, aes(x=Time, y=P50, color=condi), size=0.6, alpha=0.4, linetype="dashed") +
#  
#  geom_line(aes(x=Time, y=Estimated, color=condi), size=0.6) +
#  geom_point(data = base::subset(fit.s, CMT==pk_obs),
#             color = pk_color,
#             aes(x=Time, y=DV)) +
#  geom_errorbar(data = base::subset(fit.s, CMT==pk_obs),
#                color = pk_color,
#                aes(x=Time, y=DV, ymax=DV+IRES, ymin=DV-IRES),width=1) +
#  geom_text(color = 'gray65',
#            aes(x=Time, y=Estimated, label=date), size=3.5) +
#  xlab(pk_x_label) +
#  ylab(pk_y_label) +
#  scale_x_continuous(breaks=seq(0,720,12), # from 0 ~ 720 by 12
#                     minor_breaks=seq(6,720,12)) + # from 6 ~ 720 by 12
#  scale_y_continuous(breaks=seq(0,150,5)) +
#  # ggplot theme setting
