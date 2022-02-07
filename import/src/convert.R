library(pacman)

pacman::p_load(argparse, tidyr, stringr, dplyr, assertr, here, readr)

fys <- c(2000:2020)

all_fys <- tibble()

for (fy in fys){
  
  # print(fy)
  
  filename <- paste('usbp_monthly_encounters_fy', fy, '.csv', sep='')
  
  if (fy == 2020) {
    skip <- 4
  } else {
    skip <- 5
  }
  
  df <- read.delim(here::here('import', 'frozen', filename),
                       sep='|',
                       skip=skip,
                       header=FALSE,
                       stringsAsFactors=FALSE)
  
  if (df[1,1] == 'Total') {
    df <- df[-c(1),]
    rownames(df) <- NULL
  }
  
  colnames(df) <- c("sector",
                    "october",
                    "november",
                    "december",
                    "january",
                    "february",
                    "march",
                    "april",
                    "may",
                    "june",
                    "july",
                    "august",
                    "september",
                    "yearly total")
  
  if (fy <= 2018) {
    big_bend <- 14
    big_bend_values <- unlist(df[big_bend,1:13], use.names = FALSE)
    big_bend_name <- df[13,1]
    df[big_bend,1] <- big_bend_name
    df[big_bend,2:14] <- big_bend_values
    
    rio_grande <- 21
    rio_grande_values <- unlist(df[rio_grande,1:13], use.names = FALSE)
    rio_grande_name <- df[20,1]
    df[rio_grande,1] <- rio_grande_name
    df[rio_grande,2:14] <- rio_grande_values
    df <- df[-c(13, 15, 20, 22, 30, 31),]
    
    df <- df %>% 
      mutate_all(~str_replace(., 'N/A', '0')) %>% 
      mutate_all(~str_replace(., 'Livermore\\*', 'Livermore'))
    
    df[,2:14] <- as.numeric(gsub(",", "", unlist(df[,2:14])))
    
    # Spot testing that row and subgroup totals are correct
    stopifnot(sum(df[1, 2:13]) == df[1, 14],
              sum(df[2, 2:13]) == df[2, 14],
              sum(df[1:21, 2]) == df[25, 2],
              sum(df[22:24, 2]) == df[25, 2])
    
  } else if (fy >= 2019) {
    df <- df[-c(25),]
    
    df[,2:14] <- as.numeric(gsub(",", "", unlist(df[,2:14])))
    
    # Spot testing that row and subgroup totals are correct
    stopifnot(sum(df[1, 2:13]) == df[1, 14],
              sum(df[2, 2:13]) == df[2, 14],
              sum(df[1:20, 2]) == df[24, 2],
              sum(df[21:23, 2]) == df[24, 2])
  }

  rownames(df) <- NULL
  
  df <- df %>%
    pivot_longer(cols = c(-'sector'), names_to="month") %>% 
    pivot_wider(names_from = c('sector'))
  
  if (fy >= 2019) {
    df$Livermore <- 0
  }
  
  prev_cy <- c('october', 'november', 'december')
  
  df <- df %>% 
    mutate(fy = fy) %>% 
    mutate(cy = case_when(month %in% prev_cy ~ .$fy - 1,
                          TRUE ~ as.numeric(.$fy)))
  
  all_fys <- rbind(all_fys, df)

}
  

write_delim(all_fys, here::here('import','output', 'usbp_monthly_encounters_fy2000-fy2020.csv.gz'), delim='|')

# DONE
