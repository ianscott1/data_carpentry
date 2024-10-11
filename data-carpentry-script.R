getwd()
?lm
??kruskal
3+5
1/3
(weight_kg<-55)
weight_kg <- 62
x2 <- 22
2.2*weight_kg
weight_kg<-57.5
2.2*weight_kg
#demo
weight_lb<-2.2*weight_kg
weight_kg<-100

sqrt(10)
args(sqrt)
?round
args(round)
round(3.13159)
round(3.14159, 3)
signif(3.14159)
signif(3.1415962)
signif(3.1415962, 4)

weight_g <- c(50, 60, 65, 82)
animals <- c("buffalo", "tiger", "stork")
length(weight_g)
length(animals)
class(weight_g)
class(animals)
str(weight_g)
str(animals)

weight_g <- c(weight_g, 90)
weight_g <- c(weight_g, 22)
is.integer(2)
is.integer(2L)

#challenge_1
num_char <- c(1, 2, 3, "a")
num_logical <- c(1, 2, 3, TRUE)
char_logical <- c("a", "b", "c", TRUE)
tricky <- c(1, 2, 3, "4")
class(num_char)
class(num_logical)
class(char_logical)
class(tricky)
combined_logical <- c(num_logical, char_logical)
combined_logical
logicals_num <- c(1, 3, TRUE, FALSE)
logicals_num

animals <- c("Mouse", "Rat", "Dog", "Cat")
animals(2)
animals[2]
animals[c(1, 2)]
more_animals <- animals[c(2, 2, 2, 2, 2)]
more_animals

weight_g <- c(20, 25, 31, 43, 62)
weight_g
weight_g[c(TRUE, TRUE, FALSE, TRUE, FALSE)]
weight_g > 40
weight_g[weight_g > 40]

weight_g[weight_g > 30 & weight_g <50]
weight_g[weight_g <= 25 | weight_g ==62]
weight_g[weight_g < 20]

animals[animals == "Cat" | animals == "Rat"]
animals[animals %in% c("Cat", "Rat")]
animals2 <- c("Bat", "Horse", "Rat")
animals[animals %in% c(animals2)]

heights <- c(2, 4, 4, NA, 6)
heights
mean(heights, na.rm = TRUE)
max(heights)
max(heights, na.rm = TRUE)
heights[!is.na(heights)]
na.omit(heights)
heights[complete.cases(heights)]

heights <- c(63, 69, 60, 65, NA, 68, 61, 70, 61, 59, 64, 69, 63, 63, NA, 72, 65, 64, 70, 63, 65)
heights_no_na <- c(heights, na.rm = TRUE)
heights_no_na <- c(na.omit(heights))
heights_no_na
median(heights_no_na)
heights_no_na[heights_no_na > 67]
nonaheights <- c(heights, na.rm = TRUE)
nonaheights
nonaheights <- heights[!is.na(heights)]
nonaheights
length(nonaheights[nonaheights > 67])

download.file(url = "https://ndownloader.figshare.com/files/2292169",destfile = "data_raw/portal_data_joined.csv")

surveys <- read.csv("data_raw/portal_data_joined.csv")

head(surveys)
tail(surveys)
str(surveys)
dim(surveys)
nrow(surveys)
ncol(surveys)
names(surveys)
summary(surveys)

surveys[11, 5]
surveys[6]
surveys[6, ]
surveys(, 6)

surveys[, -1]
surveys$species_id

surveys_200 <- surveys[200, ]
surveys_200

n_rows <- nrow(surveys)
surveys[n_rows, ] 
tail(surveys)
nrow(surveys)
surveys[nrow(surveys), ]
surveys_last <- surveys[nrow(surveys), ]
surveys_last
surveys_middle <- surveys[nrow(surveys)/2, ]
surveys_middle
head(surveys)
surveys_head <- surveys[-(7:n_rows), ]
surveys_head

sex <- factor(c("male", "female", "female", "male"))
levels(sex)
nlevels(sex)

sex <- factor(sex, levels = c("male", "female"))
levels(sex)

as.character(sex)

year_fct <- factor(c(1990, 1983, 1977, 1998, 1990))
as.numeric(year_fct)
as.numeric(levels(year_fct))[year_fct]

sex <- factor(surveys$sex)
sex
plot(sex)
levels(sex)[1] <- "undetermined"
levels(sex)[2] <- "female"
levels(sex)[3] <- "male"
levels(sex)
plot(sex)
sex <- factor(sex, levels = c("female", "male", "undetermined"))
plot(sex)

surveys$plot_type <- factor(surveys$plot_type)
str(surveys)

library(tidyverse)
library(tidyverse)

surveys <- read_csv("data_raw/portal_data_joined.csv")
help(package = "dplyr")

select(surveys, -plot_id, -species_id, -weight)
surveys
filter(surveys, year == 1995)
surveys2 <- filter(surveys, weight < 5)
surveys_sml <- select(surveys2, species_id, sex, weight)
surveys_sml

surveys_sml <- select(filter(surveys, weight < 5), species_id, sex, weight)
surveys_sml

surveys_sml <- surveys |>
  filter(weight < 5)|>
  select(species_id, sex, weight)
surveys_sml

surveys_pre1995 <- surveys |> 
  filter(year < 1995) |> 
  select(year, sex, weight)
surveys_pre1995

surveys |> 
  filter(!is.na(weight)) |> 
  mutate(weight_kg = weight/1000, weight_lb = weight_kg * 2.2) |> 
  select(record_id, weight, weight_kg, weight_lb)

surveys |> 
  filter(!is.na(hindfoot_length)) |> 
  mutate(hindfoot_cm = hindfoot_length/10) |> 
  filter(hindfoot_cm < 3) |> 
  select(species_id, hindfoot_cm, hindfoot_length)

surveys |> 
  filter(!is.na(weight)) |> 
  group_by(sex, species_id) |> 
  summarise(mean_weight = mean(weight), min_weight = min(weight)) |> 
  arrange(desc(min_weight)) |> 
  print(n=16)

surveys |> 
  count(sex, species) |> 
  arrange(species, desc(n))

#^ shortcut for:

surveys |> 
  group_by(sex) |> 
  summarise(count = n())

surveys |> 
  count(plot_type)

surveys |>
  filter(!is.na(hindfoot_length)) |> 
  group_by(species) |> 
  summarise(
    hindfoot_mean = mean(hindfoot_length), 
    hindfoot_min = min(hindfoot_length), 
    hindfoot_max = max(hindfoot_length),
    n = n())

surveys |> 
  filter(!is.na(weight)) |> 
  group_by(year) |> 
  filter(weight == max(weight)) |> 
  select(year, genus, species, weight)

  
str(surveys)

library(lubridate)

my_date <-  ymd("2015-01-01")
str(my_date)
my_date <-  ymd(paste("2015", "1", "1", sep = "-"))
str(my_date)
dates_char_vec <-  paste(surveys$year, surveys$month, surveys$day, sep = "-")
str(dates_char_vec)

date_vec <-  ymd(dates_char_vec)
summary(date_vec)

missing_dates_vec <-  dates_char_vec[is.na(date_vec)]
head(missing_dates_vec)

surveys$date <- date_vec
str(surveys)

surveys_gw <- surveys |> 
  filter(!is.na(weight)) |> 
  group_by(plot_id, genus) |> 
  summarise(mean_weight = mean(weight))

str(surveys_gw)

surveys_wide <- surveys_gw |> 
  pivot_wider(names_from = genus, values_from = mean_weight, values_fill = 0)

str(surveys_wide)

surveys_long <- surveys_wide |> 
  pivot_longer(cols = -plot_id, names_to = "genus", values_to = "mean_weight")

surveys_long_hindfoot_weight <- surveys |> 
  pivot_longer(cols = hindfoot_length, names_to = "Measurement", values_to = "mean_hindfoot") |> 
  pivot_longer(cols = weight, names_to "Measurement", values_to = "mean_weight")

surveys_long_measurement <- surveys |> 
  pivot_longer(cols = c(hindfoot_length, weight), names_to = "measurement", values_to = "value")

surveys_long_measurement |> 
  group_by(year, measurement, plot_type) |> 
  summarise(mean_value = mean(value, na.rm = TRUE)) |> 
  pivot_wider(names_from = measurement, values_from = mean_value)

surveys <- read_csv("data_raw/portal_data_joined.csv")

surveys_complete <- surveys |> 
  filter(!is.na(weight), !is.na(hindfoot_length), !is.na(sex))

species_count <- surveys_complete |>
  count(species_id) |> 
  filter(n >= 50)

surveys_complete <- surveys_complete |> 
  filter(species_id %in% species_count$species_id)

write_csv(surveys_complete, file = "data/surveys_complete.csv")

library(tidyverse)
surveys_complete <-  read_csv("data/surveys_complete.csv")

surveys_plot <- ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length))

surveys_plot +
  geom_point()

install.packages("hexbin")
("hexbin")

surveys_plot +
  geom_hex()

ggplot(surveys_complete, aes(x = weight, y = hindfoot_length))+
  geom_point(alpha = 0.1, aes(colour = species_id))

ggplot(surveys_complete, aes(x = species_id, y = weight))+
  geom_jitter(alpha = 0.3, colour = "tomato")+
  geom_boxplot(alpha = 0)

ggplot(surveys_complete, aes(x = species_id, y = weight))+
  geom_violin()+
  scale_y_log10()

ggplot(surveys_complete, aes(x = species_id, y = hindfoot_length))+
  geom_jitter(alpha=0.3, aes(colour = factor(plot_id)))+
  geom_boxplot(alpha = 0)

yearly_counts <-  surveys_complete |> 
  count(year, genus)

ggplot(yearly_counts, aes(x = year, y = n, colour = genus))+
  geom_line()

yearly_counts_graph <- surveys_complete |> 
  count(year, genus) |> 
  ggplot(aes(x = year, y = n, colour = genus))+
  geom_line()
 yearly_counts_graph

 ggplot(yearly_counts, aes(x = year, y = n))+
   geom_line()+
   facet_wrap(facets = vars(genus))
    
 yearly_counts_male_female <- surveys_complete |> 
   count(year, genus, sex)

ggplot(yearly_counts_male_female, aes(x = year, y = n, colour = sex))+
  geom_line()+
  facet_wrap(facets = vars(genus))
 
ggplot(yearly_counts_male_female, aes(x = year, y = n, colour = sex))+
  geom_line()+
  facet_wrap(facets = vars(genus))+
  theme_minimal()

install.packages("ggthemes")   
library("ggthemes")             

ggplot(yearly_counts_male_female, aes(x = year, y = n, colour = sex))+
  geom_line()+
  facet_wrap(facets = vars(genus))+
  theme_minimal()

yearly_mean_weights <- surveys_complete |> 
  group_by(year, species) |> 
  summarise(mean_weight = mean(weight))

ggplot(yearly_mean_weights, aes(x = year, y = mean_weight))+
  geom_line()+
  facet_wrap(facets = vars(species))+
  grey_theme

ggplot(yearly_counts_male_female, aes(x = year, y = n, colour = sex))+
  geom_line()+
  facet_wrap(facets = vars(genus))+
  labs(title = "Observed genera through time",
       x = "Year of observation",
       y = "Number of individuals")+
  theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 0.5, vjust = 0.5, colour = "grey20"),
        axis.text.y = element_text(size = 12, colour = "grey20"),
        text = element_text(size = 14))
  
grey_theme <- theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 0.5, vjust = 0.5, colour = "grey20"),
                    axis.text.y = element_text(size = 12, colour = "grey20"),
                    text = element_text(size = 14))

install.packages("RColorBrewer")
library("RColorBrewer")

display.brewer.all(colorblindFriendly = TRUE)

yearly_counts |> 
  ggplot(aes(x = year, y = n, color = genus))+
  geom_line(size = 1.5)+
  labs(title = "Observed genera through time",
       x = "Year of observation",
       y = "Number of individuals")+
  scale_color_brewer(palette = "Set2",
                     name = "Genus",
                     labels = c("C", "D", "N", "O", "Pg", "Py", "R", "S"))+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_line(colour = "black"))

install.packages("gridExtra")
library("gridExtra")

spp_weight_boxplot <- ggplot(surveys_complete, aes(x = species_id, y = weight))+
  geom_boxplot()+
  labs(x = "Species", y = "Weight")+
  scale_y_log10()+
  theme_minimal()
spp_weight_boxplot

spp_count_plot <- ggplot(yearly_counts, aes(x = year, y = n, colour = genus))+
  geom_line()+
  labs(x = "Year", y = "Abundance")+
  theme_minimal()
spp_count_plot

combo_plot <- grid.arrange(spp_weight_boxplot, spp_count_plot, ncol = 2, widths = c(4, 6))

ggsave("fig/combo_plot_abundance_weight_minimal.png", combo_plot, width = 10, height = 6, dpi = 300)
