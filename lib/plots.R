process.freq.df <- function(
  df,
  cond_descriptors = c("construction", "mandate not enforced",
                       "mass gathering", "business not allowed\nto be open",
                       "restaurant/bar not\nin compliance", "business not in\ncompliance",
                       "staff not vaccinated", "vaccine rules not\nposted",
                       "employee proof of\nvaccination not checked", "customer proof of\nvaccination not checked")
  ) {
  processed = df %>%
    group_by(Descriptor) %>%
    summarise(Complaint.Type = first(Complaint.Type), n=n()) %>%
    mutate(freq = n /sum(n) * 100) %>%
    arrange(Complaint.Type, freq) %>%
    mutate(Descriptor = cond_descriptors)
  return(processed)
}

plot.freq.bp <- function(
  df,
  cond_xlabels = rev(c("vaccine mandate\nnon-compliance",
                       "noncompliance w/\nphased reopening",
                       "mass gathering\ncomplaint",
                       "face covering\nviolation",
                       "covid-19 non-\nessential construction"))
  ) {
  df$Descriptor <- factor(df$Descriptor, levels = df$Descriptor)
  
  call_freq_bp = ggplot(df, aes(fill=Descriptor, y=freq, x=Complaint.Type)) +
    geom_bar(position="stack", stat="identity") + 
    scale_x_discrete(labels=cond_xlabels) + 
    guides(fill = guide_legend(reverse=TRUE)) + 
    theme(legend.text = element_text(size=16),
          legend.title = element_text(size = 18),
          axis.text=element_text(size=16),
          title = element_text(size=20),
          axis.title.x = element_text(size=16),
          axis.title.y = element_text(size=16)) + 
    labs(title = "Breakdown of Covid-19 Complaint Types",
         subtitle = "Pandemic Period: 2020-2022",
         x = "covid-related complaint",
         y = "% frequency")
  call_freq_bp + coord_flip()
}

get.ncalls <- function(
  df
) {
  calls_by_date_complaint = NULL
  complaints = unique(df$Complaint.Type)
  
  for (complaint in complaints) {
    
    dates_df = data.frame(unique(df$date)) %>% rename(date=unique.df.date.)
    
    tmp = df %>%
      filter(Complaint.Type == complaint) %>%
      group_by(date) %>%
      summarise(ncalls = n())
    
    tmp_merge = merge(dates_df, tmp, by='date', all.x=TRUE) %>%
      mutate(ncalls = replace(ncalls, 
                              is.na(ncalls), 
                              0)) %>%
      mutate(ncalls_MA7 = rollmean(ncalls, k=7, fill=NA)) %>%
      mutate(complaint_type = complaint) %>%
      select(date, complaint_type, ncalls, ncalls_MA7)
    
    calls_by_date_complaint = rbind(calls_by_date_complaint,
                                    tmp_merge)
  }
  # all complaints
  tmp = df %>%
    group_by(date) %>%
    summarise(ncalls = n()) %>%
    mutate(ncalls_MA7 = rollmean(ncalls, k=7, fill=NA)) %>%
    mutate(complaint_type = "all") %>%
    select(date, complaint_type, ncalls, ncalls_MA7)
  
  calls_by_date_complaint = rbind(calls_by_date_complaint,
                                  tmp)
    
  return(calls_by_date_complaint)
}

plot.ts.complaints <- function(
  df
  ) {
    
    # get data in right format
    processed = get.ncalls(df)
    processed$date = as.POSIXct(processed$date, 
                                format = "%Y-%m-%d")
    # plot
    ggplot(processed,                            
           aes(x = date,
               y = ncalls_MA7,
               col = complaint_type)) +
      geom_line() + 
      labs(x = "Date",
           y = "Number of Calls",
           title = "Covid-related Calls",
           subtitle = "NYC 2020-2022: 7-day MA") + 
      theme(legend.key.size = unit(0.5, 'cm'),
            legend.text = element_text(size=16),
            legend.title = element_text(size = 18),
            axis.text=element_text(size=16),
            title = element_text(size=20),
            axis.title.x = element_text(size=16),
            axis.title.y = element_text(size=16)) +
      scale_color_manual(values = c("gray30", "#F8766D", "chartreuse3", "#00B9E3",
                                    "#619CFF", "#DC71FA"))
}

process.borough.data <- function(df, borough_pop,
                                 complaint = "covid-19 non-essential construction") {
  
  covid_by_borough = df %>%
    filter(Complaint.Type == complaint) %>%
    group_by(Borough) %>%
    summarise(ncalls = n()) %>%
    mutate(Borough = str_to_title(Borough)) %>%
    filter(Borough != "Unspecified") %>%
    rename(borough = Borough)
  
  covid_by_borough = merge(covid_by_borough, borough_pop, by="borough") %>%
    mutate(normalized_calls = ncalls / total_pop) %>%
    mutate(prop_calls = normalized_calls / sum(normalized_calls) * 100)
  
  return(covid_by_borough)
}

plot.borough.bp <- function(df,borough_pop,
                            complaint = "covid-19 non-essential construction") {
  cmp_borough = process.borough.data(df,borough_pop, complaint)
  cmp_borough$borough <- factor(cmp_borough$borough, levels = cmp_borough$borough)
  
  call_freq_borough_bp = ggplot(cmp_borough, aes(fill=borough, y=prop_calls, x=borough)) +
    geom_bar(position="stack", stat="identity") +
    guides(fill = guide_legend(reverse=TRUE)) +
    labs(title = str_to_title(complaint),
         subtitle = "NYC 2020-2022",
         x = "borough",
         y = "call volume proportional to population")+
    theme(legend.text = element_text(size=16),
          legend.title = element_text(size = 18),
          axis.text=element_text(size=16),
          title = element_text(size=20),
          axis.title.x = element_text(size=16),
          axis.title.y = element_text(size=16))
  
  call_freq_borough_bp + coord_flip()
}

plot.corr <- function(df, thres=0.7) {
  complaint_corr_filt = subset(df,
                               ((abs(`non-compliance\nw/phased\nreopening`) >= thres) |
                                  (abs(`vaccine mandate\nnon-compliance`) >= thres) |
                                  (abs(`mass gathering\ncomplaint`) >= thres) |
                                  (abs(`covid-19\nnon-essential\nconstruction`) >= thres)))
  
  complaint_corr_filt = complaint_corr_filt[-5]
  complaint_corr_filt[is.na(complaint_corr_filt)] <- 0
  
  pheatmap(as.matrix(complaint_corr_filt), 
           main = "Correlation between Covid and Non-Covid Calls", 
           fontsize = 12)
}