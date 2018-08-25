# general visualisation

library('ggplot2') # visualisation
library('scales') # visualisation
library('grid') # visualisation
library('gridExtra') # visualisation
library('RColorBrewer') # visualisation
library('corrplot') # visualisation

# general data manipulation

library('dplyr') # data manipulation
library('readr') # input/output
library('data.table') # data manipulation
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('stringr') # string manipulation
library('forcats') # factor manipulation

# specific visualisation

library('ggrepel') # visualisation
library('ggridges') # visualisation
library('ggExtra') # visualisation
library('ggforce') # visualisation
library('viridis') # visualisation

# specific data manipulation

library('lazyeval') # data wrangling
library('broom') # data wrangling
library('purrr') # string manipulation

# Date plus forecast

library('lubridate') # date and time
library('timeDate') # date and time
library('tseries') # time series analysis
library('forecast') # time series analysis
library('prophet') # time series analysis
library('timetk') # time series analysis

# Maps / geospatial

library('geosphere') # geospatial locations
library('leaflet') # maps
library('leaflet.extras') # maps
library('maps') # maps

setwd("C:/Users/Architect_shwet/Desktop/all")

air_visits <- as.tibble(fread('air_visit_data.csv'))
air_reserve <- as.tibble(fread('air_reserve.csv'))
hpg_reserve <- as.tibble(fread('hpg_reserve.csv'))
air_store <- as.tibble(fread('air_store_info.csv'))
hpg_store <- as.tibble(fread('hpg_store_info.csv'))
holidays <- as.tibble(fread('date_info.csv'))
store_ids <- as.tibble(fread('store_id_relation.csv'))
test <- as.tibble(fread('sample_submission.csv'))


# Define multiple plot function
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      
      # Get the i,j matrix positions of the regions that contain this subplot
      
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      
                                      layout.pos.col = matchidx$col))
      
    }}}


air_visits <- air_visits %>%
  mutate(visit_date = ymd(visit_date))

air_reserve <- air_reserve %>%
  mutate(visit_datetime = ymd_hms(visit_datetime),
         reserve_datetime = ymd_hms(reserve_datetime))

hpg_reserve <- hpg_reserve %>%
  mutate(visit_datetime = ymd_hms(visit_datetime),
         reserve_datetime = ymd_hms(reserve_datetime))

air_store <- air_store %>%
  mutate(air_genre_name = as.factor(air_genre_name),
         air_area_name = as.factor(air_area_name))

hpg_store <- hpg_store %>%
  mutate(hpg_genre_name = as.factor(hpg_genre_name),
         hpg_area_name = as.factor(hpg_area_name))

holidays <- holidays %>%
  mutate(holiday_flg = as.logical(holiday_flg),
         date = ymd(calendar_date))


p1 <- air_visits %>%
  group_by(visit_date) %>%
  summarise(all_visitors = sum(visitors)) %>%
  ggplot(aes(visit_date,all_visitors)) +
  geom_line(col = "blue") +
  labs(y = "All visitors", x = "Date")
p1
p2 <- air_visits %>%
  ggplot(aes(visitors)) +
  geom_vline(xintercept = 20, color = "orange") +
  geom_histogram(fill = "blue", bins = 30) +
  scale_x_log10()

p3 <- air_visits %>%
  mutate(wday = wday(visit_date, label = TRUE)) %>%
  group_by(wday) %>%
  summarise(visits = median(visitors)) %>%
  ggplot(aes(wday, visits, fill = wday)) +
  geom_col() +
  theme(legend.position = "none", axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9)) +
  labs(x = "Day of the week", y = "Median visitors")

p4 <- air_visits %>%
  mutate(month = month(visit_date, label = TRUE)) %>%
  group_by(month) %>%
  summarise(visits = median(visitors)) %>%
  ggplot(aes(month, visits, fill = month)) +
  geom_col() +
  theme(legend.position = "none") +
  labs(x = "Month", y = "Median visitors")


air_visits %>%
  filter(visit_date > ymd("2016-04-15") & visit_date < ymd("2016-06-15")) %>%
  group_by(visit_date) %>%
  summarise(all_visitors = sum(visitors)) %>%
  ggplot(aes(visit_date,all_visitors)) +
  geom_line() +
  geom_smooth(method = "loess", color = "blue", span = 1/7) +
  labs(y = "All visitors", x = "Date")

foo <- air_reserve %>%
  mutate(reserve_date = date(reserve_datetime),
         reserve_hour = hour(reserve_datetime),
         reserve_wday = wday(reserve_datetime, label = TRUE),
         visit_date = date(visit_datetime),
         visit_hour = hour(visit_datetime),
         visit_wday = wday(visit_datetime, label = TRUE),
         diff_hour = time_length(visit_datetime - reserve_datetime, unit = "hour"),
         diff_day = time_length(visit_datetime - reserve_datetime, unit = "day")
  )

p1 <- foo %>%
  group_by(visit_date) %>%
  summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(visit_date, all_visitors)) +
  geom_line() +
  labs(x = "'air' visit date")

p2 <- foo %>%
  group_by(visit_hour) %>%
  summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(visit_hour, all_visitors)) +
  geom_col(fill = "blue")

p3 <- foo %>%
  filter(diff_hour < 24*5) %>%
  group_by(diff_hour) %>%
  summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(diff_hour, all_visitors)) +
  geom_col(fill = "blue") +
  labs(x = "Time from reservation to visit [hours]")

foo <- hpg_reserve %>%
  mutate(reserve_date = date(reserve_datetime),
         reserve_hour = hour(reserve_datetime),
         visit_date = date(visit_datetime),
         visit_hour = hour(visit_datetime),
         diff_hour = time_length(visit_datetime - reserve_datetime, unit = "hour"),
         diff_day = time_length(visit_datetime - reserve_datetime, unit = "day")
  )

p1 <- foo %>%
  group_by(visit_date) %>%
  summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(visit_date, all_visitors)) +
  geom_line() +
  labs(x = "'hpg' visit date")

p2 <- foo %>%
  group_by(visit_hour) %>%
  summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(visit_hour, all_visitors)) +
  geom_col(fill = "red")

p3 <- foo %>%
  filter(diff_hour < 24*5) %>%
  group_by(diff_hour) %>%
  summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(diff_hour, all_visitors)) +
  geom_col(fill = "red") +
  labs(x = "Time from reservation to visit [hours]")

library(leaflet)
leaflet(air_store) %>%
  addTiles() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addMarkers(~longitude, ~latitude,
             popup = ~air_store_id, label = ~air_genre_name,
             clusterOptions = markerClusterOptions())

p1 <- air_store %>%
  group_by(air_genre_name) %>%
  count() %>%
  ggplot(aes(reorder(air_genre_name, n, FUN = min), n, fill = air_genre_name)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = "Type of cuisine (air_genre_name)", y = "Number of air restaurants")
p1
p2 <- air_store %>%
  group_by(air_area_name) %>%
  count() %>%
  ungroup() %>%
  top_n(15,n) %>%
  ggplot(aes(reorder(air_area_name, n, FUN = min) ,n, fill = air_area_name)) +
  geom_col() +
  theme(legend.position = "none") +
  coord_flip() +
  labs(x = "Top 15 areas (air_area_name)", y = "Number of air restaurants")

leaflet(hpg_store) %>%
  addTiles() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addMarkers(~longitude, ~latitude,
             popup = ~hpg_store_id, label = ~hpg_genre_name,
             clusterOptions = markerClusterOptions())


p1 <- hpg_store %>%
  group_by(hpg_genre_name) %>%
  count() %>%
  ggplot(aes(reorder(hpg_genre_name, n, FUN = min), n, fill = hpg_genre_name)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = "Type of cuisine (hpg_genre_name)", y = "Number of hpg restaurants")

p2 <- hpg_store %>%
  mutate(area = str_sub(hpg_area_name, 1, 20)) %>%
  group_by(area) %>%
  count() %>%
  ungroup() %>%
  top_n(15,n) %>%
  ggplot(aes(reorder(area, n, FUN = min) ,n, fill = area)) +
  geom_col() +
  theme(legend.position = "none") +
  coord_flip() +
  labs(x = "Top 15 areas (hpg_area_name)", y = "Number of hpg restaurants")


foo <- holidays %>%
  mutate(wday = wday(date))

p1 <- foo %>%
  ggplot(aes(holiday_flg, fill = holiday_flg)) +
  geom_bar() +
  theme(legend.position = "none")

p2 <- foo %>%
  filter(date > ymd("2016-04-15") & date < ymd("2016-06-01")) %>%
  ggplot(aes(date, holiday_flg, color = holiday_flg)) +
  geom_point(size = 2) +
  theme(legend.position = "none") +
  labs(x = "2016 date")

p3 <- foo %>%
  filter(date > ymd("2017-04-15") & date < ymd("2017-06-01")) %>%
  ggplot(aes(date, holiday_flg, color = holiday_flg)) +
  geom_point(size = 2) +
  theme(legend.position = "none") +
  labs(x = "2017 date")


holidays %>% group_by(holiday_flg) %>% 
          count() %>% spread(holiday_flg,n) %>% mutate(frac = `TRUE`/(`TRUE`+`FALSE`))


foo <- air_visits %>%
  rename(date = visit_date) %>%
  distinct(visit_date) %>%
  mutate(dset = "train")

bar <- test %>%
  separate(id, c("foo", "bar", "date"), sep = "_") %>%
  mutate(date = ymd(date)) %>%
  distinct(date) %>%
  mutate(dset = "test")

foo <- foo %>%
  bind_rows(bar) %>%
  mutate(year = year(date))

year(foo$date) <- 2017

foo %>%
  filter(!is.na(date)) %>%
  mutate(year = fct_relevel(as.factor(year), c("2017","2016"))) %>%
  ggplot(aes(date, year, color = dset)) +
  geom_point(shape = "|", size = 10) +
  scale_x_date(date_labels = "%B", date_breaks = "1 month") +
  #scale_y_reverse() +
  theme(legend.position = "bottom", axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9)) +
  labs(color = "Data set") +
  guides(color = guide_legend(override.aes = list(size = 4, pch = 15)))

foo <- air_visits %>%
  left_join(air_store, by = "air_store_id")

foo %>%
  group_by(visit_date, air_genre_name) %>%
  summarise(mean_visitors = mean(visitors)) %>%
  ungroup() %>%
  ggplot(aes(visit_date, mean_visitors, color = air_genre_name)) +
  geom_line() +
  labs(y = "Average number of visitors to 'air' restaurants", x = "Date") +
  theme(legend.position = "none") +
  scale_y_log10() +
  facet_wrap(~ air_genre_name)


p1 <- foo %>%
  mutate(wday = wday(visit_date, label = TRUE)) %>%
  group_by(wday, air_genre_name) %>%
  summarise(mean_visitors = mean(visitors)) %>%
  ggplot(aes(air_genre_name, mean_visitors, color = wday)) +
  geom_point(size = 4) +
  theme(legend.position = "left", axis.text.y = element_blank(),
        plot.title = element_text(size = 14)) +
  coord_flip() +
  labs(x = "") +
  scale_x_discrete(position = "top") +
  ggtitle("air_genre_name") +
  scale_color_hue()

p2 <- foo %>%
  ggplot(aes(visitors, air_genre_name, fill = air_genre_name)) +
  geom_density_ridges(bandwidth = 0.1) +
  scale_x_log10() +
  theme(legend.position = "none") +
  labs(y = "") +
  scale_fill_cyclical(values = c("blue", "red"))


foo <- air_visits %>%
  mutate(calendar_date = as.character(visit_date)) %>%
  left_join(holidays, by = "calendar_date")

p1 <- foo %>%
  ggplot(aes(holiday_flg, visitors, color = holiday_flg)) +
  geom_boxplot() +
  scale_y_log10() +
  theme(legend.position = "none")

p2 <- foo %>%
  mutate(wday = wday(date, label = TRUE)) %>%
  group_by(wday, holiday_flg) %>%
  summarise(mean_visitors = mean(visitors)) %>%
  ggplot(aes(wday, mean_visitors, color = holiday_flg)) +
  geom_point(size = 4) +
  theme(legend.position = "none") +
  labs(y = "Average number of visitors")


air_store %>%
  mutate(area = str_sub(air_area_name, 1, 12)) %>%
  ggplot(aes(area, air_genre_name)) +
  geom_count(colour = "blue") +
  theme(legend.position = "bottom", axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9))


hpg_store %>%
  mutate(area = str_sub(hpg_area_name, 1, 10)) %>%
  ggplot(aes(area, hpg_genre_name)) +
  geom_count(colour = "red") +
  theme(legend.position = "bottom", axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9))



foobar <- hpg_store %>%
  group_by(hpg_genre_name, hpg_area_name) %>%
  count()

foobar %>%
  ggplot(aes(reorder(hpg_genre_name, n, FUN = mean), n)) +
  geom_boxplot() +
  geom_jitter(color = "red") +
  scale_y_log10() +
  coord_flip() +
  labs(x = "hpg genre", y = "Cases per hpg area")


foo <- air_visits %>%
  left_join(air_store, by = "air_store_id")

bar <- air_store %>%
  group_by(air_genre_name, air_area_name) %>%
  count()

foobar <- hpg_store %>%
  group_by(hpg_genre_name, hpg_area_name) %>%
  count()

p1 <- bar %>%
  ggplot(aes(n)) +
  geom_histogram(fill = "blue", binwidth = 1) +
  labs(x = "Air genres per area")

p2 <- foobar %>%
  ggplot(aes(n)) +
  geom_histogram(fill = "red", binwidth = 1) +
  labs(x = "HPG genres per area")


p3 <- foo %>%
  group_by(air_genre_name, air_area_name) %>%
  summarise(mean_log_visit = mean(log1p(visitors))) %>%
  left_join(bar, by = c("air_genre_name","air_area_name")) %>%
  group_by(n) %>%
  summarise(mean_mlv = mean(mean_log_visit),
            sd_mlv = sd(mean_log_visit)) %>%
  replace_na(list(sd_mlv = 0)) %>%
  ggplot(aes(n, mean_mlv)) +
  geom_point(color = "blue", size = 4) +
  geom_errorbar(aes(ymin = mean_mlv - sd_mlv, ymax = mean_mlv + sd_mlv), width = 0.5, size = 0.7, color = "blue") +
  labs(x = "Cases of identical Air genres per area", y = "Mean +/- SD of\n mean log1p visitors")




foo <- air_reserve %>%
  mutate(visit_date = date(visit_datetime)) %>%
  group_by(air_store_id,visit_date) %>%
  summarise(reserve_visitors_air = sum(reserve_visitors))

bar <- hpg_reserve %>%
  mutate(visit_date = date(visit_datetime)) %>%
  group_by(hpg_store_id,visit_date) %>%
  summarise(reserve_visitors_hpg = sum(reserve_visitors)) %>%
  inner_join(store_ids, by = "hpg_store_id")

all_reserve <- air_visits %>%
  inner_join(foo, by = c("air_store_id", "visit_date")) %>%
  inner_join(bar, by = c("air_store_id", "visit_date")) %>%
  mutate(reserve_visitors = reserve_visitors_air + reserve_visitors_hpg)


p <- all_reserve %>%
  filter(reserve_visitors < 120) %>%
  ggplot(aes(reserve_visitors, visitors)) +
  geom_point(color = "black", alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "grey60") +
  geom_smooth(method = "lm", color = "blue")
ggMarginal(p, type="histogram", fill = "blue", bins=50)

p1 <- all_reserve %>%
  ggplot(aes(visitors - reserve_visitors)) +
  geom_histogram(binwidth = 5, fill = "black") +
  coord_flip() +
  labs(x = "")

p2 <- all_reserve %>%
  ggplot(aes(visitors - reserve_visitors_air)) +
  geom_histogram(binwidth = 5, fill = "blue") +
  coord_flip() +
  labs(x = "")


p3 <- all_reserve %>%
  ggplot(aes(visitors - reserve_visitors_hpg)) +
  geom_histogram(binwidth = 5, fill = "red") +
  coord_flip() +
  labs(x = "")

p4 <- all_reserve %>%
  ggplot(aes(visit_date, visitors - reserve_visitors)) +
  geom_hline(yintercept = c(150, 0, -250)) +
  geom_line() +
  geom_line(aes(visit_date, visitors - reserve_visitors_air + 150), color = "blue") +
  geom_line(aes(visit_date, visitors - reserve_visitors_hpg - 250), color = "red") +
  ggtitle("Visitors - Reserved: all (black), air (blue), hpg (red)")


# Feature engineering


air_visits <- air_visits %>%
  mutate(wday = wday(visit_date, label=TRUE),
         wday = fct_relevel(wday, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
         month = month(visit_date, label=TRUE))

air_reserve <- air_reserve %>%
  mutate(reserve_date = date(reserve_datetime),
         reserve_hour = hour(reserve_datetime),
         reserve_wday = wday(reserve_datetime, label = TRUE),
         reserve_wday = fct_relevel(reserve_wday, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
         visit_date = date(visit_datetime),
         visit_hour = hour(visit_datetime),
         visit_wday = wday(visit_datetime, label = TRUE),
         visit_wday = fct_relevel(visit_wday, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
         diff_hour = time_length(visit_datetime - reserve_datetime, unit = "hour"),
         diff_day = time_length(visit_datetime - reserve_datetime, unit = "day"))

hpg_reserve <- hpg_reserve %>%
  mutate(reserve_date = date(reserve_datetime),
         reserve_hour = hour(reserve_datetime),
         reserve_wday = wday(reserve_datetime, label = TRUE),
         reserve_wday = fct_relevel(reserve_wday, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
         visit_date = date(visit_datetime),
         visit_hour = hour(visit_datetime),
         visit_wday = wday(visit_datetime, label = TRUE),
         visit_wday = fct_relevel(visit_wday, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
         diff_hour = time_length(visit_datetime - reserve_datetime, unit = "hour"),
         diff_day = time_length(visit_datetime - reserve_datetime, unit = "day"))

# count stores in area

air_count <- air_store %>%
  group_by(air_area_name) %>%
  summarise(air_count = n())

hpg_count <- hpg_store %>%
  group_by(hpg_area_name) %>%
  summarise(hpg_count = n())

# distances

med_coord_air <- air_store %>%
  summarise_at(vars(longitude:latitude), median)

med_coord_hpg <- hpg_store %>%
  summarise_at(vars(longitude:latitude), median)


air_coords <- air_store %>%
  select(longitude, latitude)

hpg_coords <- hpg_store %>%
  select(longitude, latitude)

air_store$dist <- distCosine(air_coords, med_coord_air)/1e3
hpg_store$dist <- distCosine(hpg_coords, med_coord_hpg)/1e3


# apply counts, dist; add prefecture

air_store <- air_store %>%
  mutate(dist_group = as.integer(case_when(
    dist < 80 ~ 1,
    dist < 300 ~ 2,
    dist < 500 ~ 3,
    dist < 750 ~ 4,
    TRUE ~ 5))) %>%
  left_join(air_count, by = "air_area_name") %>%
  separate(air_area_name, c("prefecture"), sep = " ", remove = FALSE)



hpg_store <- hpg_store %>%
  mutate(dist_group = as.integer(case_when(
    dist < 80 ~ 1,
    dist < 300 ~ 2,
    dist < 500 ~ 3,
    dist < 750 ~ 4,
    TRUE ~ 5))) %>%
  left_join(hpg_count, by = "hpg_area_name") %>%
  separate(hpg_area_name, c("prefecture"), sep = " ", remove = FALSE)



foo <- air_store %>% select(latitude, longitude, dist_group) %>% mutate(dset = "air")

bar <- hpg_store %>% select(latitude, longitude, dist_group) %>% mutate(dset = "hpg")



leaflet(foo) %>%
  addTiles() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addScaleBar() %>%
  addCircleMarkers(~longitude, ~latitude, group = "AIR",
                   color = "blue", fillOpacity = 0.5, radius = 3*foo$dist_group) %>%
  addCircleMarkers(lng = bar$longitude, lat = bar$latitude, group = "HPG",
                   color = "red", fillOpacity = 0.5, radius = 3*bar$dist_group) %>%
  addCircleMarkers(lng = med_coord_air$longitude, lat = med_coord_air$latitude, group = "Centre",
                   color = "darkgreen", fillOpacity = 1) %>%
  addLayersControl(
    overlayGroups = c("AIR", "HPG", "Centre"),
    options = layersControlOptions(collapsed = FALSE)
  )


