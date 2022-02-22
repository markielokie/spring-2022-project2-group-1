# Specify source link
urlfile = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
# Read latest data from source

# dat = read_csv(urlfile)
# 
# dat_ma = dat %>%
#   filter(county == "New York City") %>%
#   arrange(date) %>%
#   mutate(daily_cases=c(1,diff(cases)),daily_deaths=c(0,diff(deaths)))%>%
#   mutate(cases_7dayMA = rollmean(daily_cases, k=7, fill=0,align = "right"),
#          deaths_7dayMA = rollmean(daily_deaths, k=7, fill=0,align = "right")) %>%
#   select(-county,-state,-fips)
# 
# zero=tibble(date=as.Date(as.Date("2019-10-1"):as.Date("2020-2-29")),cases=rep(0,152),
#             deaths = rep(0,152),daily_cases = rep(0,152),daily_deaths = rep(0,152),
#             cases_7dayMA= rep(0,152),deaths_7dayMA = rep(0,152))
# 
# dat_ma=rbind(zero,dat_ma)
# 
# write.csv(dat_ma, "..\\output\\covid_cases.csv", row.names = F)




covid_plot <- function(dat_ma,date1,date2,cumulative=FALSE) {
  

  if(cumulative==FALSE){
  # Plot 7-day MA for cases and deaths
  p=dat_ma %>%
    ggplot(aes(x=date, y=cases_7dayMA)) +
    geom_line(color="orange") +
    theme(legend.position="none") +
    geom_line(aes(x=date, y=deaths_7dayMA*50), color="red") +
    scale_y_continuous(
      labels=scales::comma,
      name="Cases",
      sec.axis=sec_axis(deaths_7dayMA~./50,
                        name="Deaths",
                        labels=scales::comma)
    ) +
    theme(
      axis.title.y=element_text(color="orange", size=13),
      axis.title.y.right=element_text(color="red", size=13),
      axis.text.x=element_text(size=6, angle=45, hjust=1),
      axis.text.y=element_text(size=6)
    ) +
    labs(
      title="Daily Cases & Deaths in NYC",
      subtitle="7-Day Moving Average",
      x="Date"
    ) +
    geom_rect(mapping = aes(xmin=date1,xmax=date2,ymin=-Inf,ymax=Inf),
              fill="yellow",alpha=0.002)
  }

  else{
    # Plot Cumulative Cases & Deaths in NYC
    p=dat_ma %>%
      ggplot(aes(x=date, y=cases)) +
      geom_line(color="orange") +
      theme(legend.position="none") +
      geom_line(aes(x=date, y=deaths*30), color="red") +
      scale_y_continuous(
        labels=scales::comma,
        name="Cases",
        sec.axis=sec_axis(deaths~./30,
                          name="Deaths",
                          labels=scales::comma)
      ) +
      theme(
        axis.title.y=element_text(color="orange", size=13),
        axis.title.y.right=element_text(color="red", size=13),
        axis.text.x=element_text(size=6, angle=45, hjust=1),
        axis.text.y=element_text(size=6)
      ) +
      labs(
        title="Cumulative Cases & Deaths in NYC",
        x="Date"
      ) +
      geom_rect(mapping = aes(xmin=date1,xmax=date2,ymin=-Inf,ymax=Inf),
                fill="yellow",alpha=0.002)
  }
  return(p)
}

