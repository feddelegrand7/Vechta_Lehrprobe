library(tabulizer)
library(tidyverse)
# Extracting East Europe --------------------------

extract_df <- function(n){
  
  
df <- tabulizer::extract_tables("phi_index_2018.pdf", 
                                       pages = n,
                                       method = "lattice")
  
df <- df[[1]]
df <- as_tibble(df)
df <- df[-1, ]
names(df) <- as.character(df[1, ])
df <- df[-1, ]
df <- janitor::clean_names(df)
df <- df[-nrow(df), ]
df <- df %>% mutate_at(vars(-economy) , as.numeric)

return(df)
  
  
}

sub_sahara <- extract_df(47)

latin <- extract_df(49)

USA <- extract_df(51)

central_asia <- extract_df(53)

east_asia <- extract_df(55)

south_asia <- extract_df(57)


MENA <- extract_df(59)

west_europ <- extract_df(61)

east_europ <- extract_df(63)

balkan <- extract_df(65)

oceania <- extract_df(67)


final_data <- bind_rows(oceania, balkan, east_europ, west_europ, MENA,
            south_asia, east_asia, central_asia, USA, latin, sub_sahara)


final_data <- final_data %>% select(-ease_of_operatng)

final_data$economy <- str_replace_all(final_data$economy, pattern = "\r", " ")


final_data<-final_data %>% arrange(desc(overall_score))


saveRDS(final_data, file = "data.Rds")

