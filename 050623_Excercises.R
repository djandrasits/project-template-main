install.packages("usethis")
library(usethis)
install.packages("devtools")
library(devtools)

devtools::install_github("jokergoo/circlize")
usethis::use_git_config(user.name="Damian Jandrasits", user.email="damian.jandrasics@gmail.com")
usethis::create_github_token()

install.packages("gitcreds")
library(gitcreds)
install.packages("here")
library(here)
install.packages("tidyverse")
library(tidyverse)
install.packages("medicaldata")
library(medicaldata)
install.packages("cowplot")
library(cowplot)
###################################################################

#Read file insuranc_with_date.csv into R and explore it a little
library(readr)
dat <- read_csv("data/raw/insurance_with_date.csv")
str(dat)

# read_csv(here("data", "raw",

dat2 <- read.csv("data/raw/insurance_with_date.csv")
str(dat2)

#slide 87########################

library(dplyr)

reformatted <- dat |> 
  mutate(
    across(c(sex, region), factor),
   # sex = factor(sex),
    #region = factor(region),
    gt2_children = children > 2,
    smokes = smoker == "yes",
    date_6m = date + months(6)
    # date_6m = date + 30.4 * 6
  )
############Excercise 4A################
library(dplyr)
library(ggplot2)

ebola <- read_csv("data/raw/ebola.csv")

ebola_countries <- ebola %>% filter(Date <= as.Date("2015-03-31") 
                                       & (Country == "Guinea" | Country == "Liberia" | Country == "Sierra Leone"))
plot_ebola_v0 <- ggplot(data = ebola_countries, 
                     mapping = aes(x=Date, y=Cum_conf_cases)) + 
  geom_point()
plot_ebola_v0

########### Exercise 4B #############################

plot_ebola_line_v0 <- ggplot(data = ebola_countries, 
                             mapping = aes(x = Date, y = Cum_conf_cases)) + 
  geom_line(mapping = aes(group = Country))
plot_ebola_line_v0


plot_ebola_col_v0 <- ggplot(data = ebola_countries, 
                            mapping = aes(x = Date, y = Cum_conf_cases)) + 
  geom_col(position = "stack")
plot_ebola_col_v0

########## Exercise 4C #####################
#dots 
plot_ebola_point_v1 <- ggplot(data = ebola_countries, 
                              mapping = aes(x = Date, y = Cum_conf_cases)) + 
  geom_point(alpha = 0.7, colour = "black", fill = "green", 
             shape = 21, size = 1.5, stroke = 1.5)
plot_ebola_point_v1

#lines 
plot_ebola_lines_v1 <- ggplot(data = ebola_countries, 
                             mapping = aes(x = Date, y = Cum_conf_cases)) + 
  geom_line(mapping = aes(group=Country), 
            alpha = 0.7, colour = "blue", linetype = "solid", linewidth = 1.0)
plot_ebola_lines_v1

#stack
plot_ebola_col_v1 <- ggplot(data = ebola_countries, 
                            mapping = aes(x = Date, y = Cum_conf_cases)) + 
  geom_col(position = "stack", alpha = 0.7, fill = "blue", 
           linetype = "solid", linewidth = 0.5, width = 0.7)
plot_ebola_col_v1

############# Exercise 4D ###################
plot_ebola_point_v2 <- ggplot(data = ebola_countries, 
                              mapping = aes(x = Date, y = Cum_conf_cases, fill = Country, colour = Country)) + 
  geom_point(alpha = 0.7, shape = 21, size = 1.5, stroke = 1.5)
plot_ebola_point_v2

ggplot(data = ebola_countries, 
       mapping = aes(x = Date, y = Cum_conf_cases, colour = Country, 
                     fill = Country, group_by = Country)) + 
  geom_point(alpha = 0.7, shape = 21, size = 1.5, stroke = 1.5) +
  geom_line()

ggplot(data = ebola_countries, 
       mapping = aes(x = Date, y = Cum_conf_cases, group_by = Country)) + 
  geom_point(alpha = 0.7, colour = "black", fill= "black", shape = 21, 
             size = 1.5, stroke = 1.5) +
  geom_line(colour = "red")

plot_ebola_col_v2 <- ggplot(data = ebola_countries, 
                            mapping = aes(x = Date, y = Cum_conf_cases, fill = Country, colour = Country)) + 
  geom_col(position = "stack", alpha = 0.7, 
           linetype = "solid", linewidth = 0.5, width = 0.7)
plot_ebola_col_v2


plot_ebola_point_v3 <- ggplot(data = ebola_countries, 
                              mapping = aes(x = Date, y = Cum_conf_cases, fill = Country, colour = Country)) + 
  geom_point(alpha = 0.7, shape = 21, size = 1.5, stroke = 1.5) +
  ggtitle(label = "Confirmed Ebola cases") +
  xlab(label = "Time") +
  ylab(label = "# of confirmed cases")
plot_ebola_point_v3

plot_ebola_lines_v3 <- ggplot(data = ebola_countries, 
                              mapping = aes(x = Date, y = Cum_conf_cases, fill = Country, colour = Country)) + 
  geom_line(alpha = 0.7,linetype="solid", linewidth=1.5) +
  ggtitle(label = "Confirmed Ebola cases") +
  xlab(label = "Time") +
  ylab(label = "# of confirmed cases")
plot_ebola_lines_v3

plot_ebola_col_v3 <- ggplot(data = ebola_countries,
                            mapping = aes(x = Date, y = Cum_conf_cases, fill=Country, colour = Country)) +
  geom_col(position = "stack", alpha = 0.7, 
           linetype="solid", linewidth=0.5) +
ggtitle(label = "Confirmed Ebola cases")
xlab(label = "Time") 
ylab(label="# of confirmed cases")
plot_ebola_col_v3


install.packages("unibeCols")
remotes::install_github("CTU-Bern/unibeCols")
library(unibeCols)
plot_ebola_point_v4 <- ggplot(data = ebola_countries, 
                              mapping = aes(x = Date, y = Cum_conf_cases, fill = Country, colour = Country)) + 
  geom_point(alpha = 0.7, shape = 21, size = 1.5, stroke = 1.5) +
  scale_fill_manual(name = "Country",
                    breaks = c("Guinea", "Liberia", "Sierra Leone"),
                    values = c(unibeRedS()[1], unibeMustardS()[1], unibeIceS()[1]),
                    labels = c("Guinea", "Liberia", "Sierra Leone")) +
  scale_colour_manual(name = "Country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"),
                      values = c(unibeRedS()[1], unibeMustardS()[1], unibeIceS()[1]),
                      labels = c("Guinea", "Liberia", "Sierra Leone")) +
  ggtitle(label = "Confirmed Ebola cases") +
  xlab(label = "Time") +
  ylab(label = "# of confirmed cases")
plot_ebola_point_v4

plot_ebola_lines_v4 <- ggplot(data = ebola_countries, 
                              mapping = aes(x = Date, y = Cum_conf_cases, fill = Country, colour = Country)) + 
  geom_line(mapping = aes(group=Country, colour = Country), alpha = 0.7, linetype = "solid", linewidth = 1.5) +
  scale_fill_manual(name = "Country",
                    breaks = c("Guinea", "Liberia", "Sierra Leone"),
                    values = c(unibeRedS()[1], unibeMustardS()[1], unibeIceS()[1]),
                    labels = c("Guinea", "Liberia", "Sierra Leone")) +
  scale_colour_manual(name = "Country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"),
                      values = c(unibeRedS()[1], unibeMustardS()[1], unibeIceS()[1]),
                      labels = c("Guinea", "Liberia", "Sierra Leone")) +
  ggtitle(label = "Confirmed Ebola cases") +
  xlab(label = "Time") +
  ylab(label = "# of confirmed cases")
plot_ebola_lines_v4

####################


plot_ebola_point_v5 <- ggplot(data =ebola_countries,
                              mapping = aes(x = Date, y = Cum_conf_cases, fill =Country, colour = Country)) + 
  geom_point(alpha = 0.7, shape = 21, size = 1.5, stroke = 1.5) +
  scale_fill_manual(name = "Country",
                    breaks = c("Guinea", "Liberia", "Sierra Leone"),
                    values = c(unibeRedS()[1], unibeMustardS()[1], unibeIceS()[1]),
                    labels = c("Guinea", "Liberia", "Sierra Leone")) +
  scale_colour_manual(name = "Country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"),
                      values = c(unibeRedS()[1], unibeMustardS()[1], unibeIceS()[1]),
                      labels = c("Guinea", "Liberia", "Sierra Leone")) +
  scale_x_date(breaks = as.Date(c("2014-08-24", "2015-02-01")),
               labels = c("24 February", "1 April"),
               limits = as.Date(c("2014-08-27", "2015-04-01"))) +
  scale_y_continuous(breaks = seq(from = 0, to = 15000, by = 1500),
                     limits = c(0, 15000)) +
  ggtitle(label = "Confirmed ebola cases") +
  xlab(label = "Time") +
  ylab(label = "# of confirmed cases")
plot_ebola_point_v5


######################################
plot_ebola_point_v6 <- ggplot(data = ebola_countries, 
                              mapping = aes(x = Date, y = Cum_conf_cases, fill = Country, colour = Country)) + 
  geom_point(alpha = 0.7, shape = 21, size = 1.5, stroke = 1.5) +
  scale_fill_manual(name = "Country",
                    breaks = c("Guinea", "Liberia", "Sierra Leone"),
                    values = c(unibeRedS()[1], unibeMustardS()[1], unibeIceS()[1]),
                    labels = c("Guinea", "Liberia", "Sierra Leone")) +
  scale_colour_manual(name = "Country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"),
                      values = c(unibeRedS()[1], unibeMustardS()[1], unibeIceS()[1]),
                      labels = c("Guinea", "Liberia", "Sierra Leone")) +
  scale_x_date(breaks = as.Date(c("2014-08-24", "2015-02-01")),
               labels = c("24 February", "1 April"),
               limits = as.Date(c("2014-08-27", "2015-04-01"))) +
  scale_y_continuous(breaks = seq(from = 0, to = 5000, by = 500),
                     limits = c(0, 5000))
  ggtitle("Confirmed Ebola Cases") +
  xlab(label = "Time") +
  ylab(label = "# of confirmed cases") +
  theme_bw() + theme(legend.position="bottom")
plot_ebola_point_v6

#######################################
plot_ebola_point_facet <- ggplot(data = ebola_countries, 
                                 mapping = aes(x = Date, y = Cum_conf_cases, fill = Country, colour=Country)) + 
  geom_point(alpha = 0.7, shape = 21, size = 1.5, stroke = 1.5) +
  scale_fill_manual(name = "Country",
                    breaks = c("Guinea", "Liberia", "Sierra Leone"),
                    values = c(unibeRedS()[1], unibeMustardS()[1], unibeIceS()[1]),
                    labels = c("Guinea", "Liberia", "Sierra Leone")) +
  scale_colour_manual(name = "Country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"),
                      values = c(unibeRedS()[1], unibeMustardS()[1], unibeIceS()[1]),
                      labels = c("Guinea", "Liberia", "Sierra Leone")) +
  scale_x_date(breaks = as.Date(c("2014-08-24", "2015-02-01")),
               labels = c("24 February", "1 April"),
               limits = as.Date(c("2014-08-27", "2015-04-01"))) +
  scale_y_continuous(breaks = seq(from = 0, to = 5000, by = 500),
                     limits = c(0, 5000)) +
  ggtitle(label = "Confirmed Ebola cases ") +
  xlab(label = "Time") +
  ylab(label = "# of confirmed cases") +
  theme_bw() + theme(legend.position="bottom") +
  theme(panel.spacing = unit(2, "lines")) +
  facet_grid(cols = vars(Country))

plot_ebola_point_facet

###########################################################
library(cowplot)

plot_ebola_point_grid <- plot_grid(plotlist = list(plot_ebola_point_v1, plot_ebola_point_v2, plot_ebola_point_v3, 
                                                   plot_ebola_point_v4, plot_ebola_point_v5, plot_ebola_point_v6),
                                   labels = c("V1", "V2", "V3", "V4", "V5", "V6"), label_size = 12, nrow = 2)
plot_ebola_point_grid


################################################

insurance <- read_csv("data/raw/insurance_with_date.csv")
insurance <- insurance %>% mutate(children = as.factor(children))
head(insurance)

ggplot( insurance , aes(x = bmi, colour = sex, fill = sex ) ) + 
  geom_density( alpha = 0.4 ) +
  theme(text = element_text(size=20), legend.position = "bottom") +
  xlab( expression(paste( "BMI (kg/", m^2,")")) ) + 
  scale_colour_manual(name = "" , values=c("female"=unibePastelS()[1],
                                           "male"=unibeIceS()[1]), labels = c("Female", "Male")) +
  scale_fill_manual(name = "", values=c("female"=unibePastelS()[1],
                                        "male"=unibeIceS()[1]), labels = c("Female", "Male")) 



ggplot( insurance ) + 
  geom_histogram( aes(x = charges, y = after_stat(density), colour = sex, fill = sex ),
                  alpha = 0.4, bins = 100 ) +
  geom_density( aes(x = charges, colour = sex), linewidth = 1.5 ) +
  theme(text = element_text(size=20), legend.position = "top") +
  xlab( "Charges in Dollar" ) + 
  scale_colour_manual(name = "" , values=c("female"=unibePastelS()[1],
                                           "male"=unibeIceS()[1]), labels = c("Female", "Male")) +
  scale_fill_manual(name = "", values=c("female"=unibePastelS()[1],
                                        "male"=unibeIceS()[1]), labels = c("Female", "Male")) +
  geom_vline(aes(xintercept = median(charges)), color = unibeRedS()[1], linewidth = 1)

####################################
ggplot( insurance , aes(x = age, y = bmi, color =smoker) ) + 
  geom_point(  ) +
  geom_quantile(  ) +
  theme(text = element_text(size=20), legend.position = "top") +
  xlab( "Age (years)" ) + ylab( expression(paste( "BMI (kg/", m^2,")")) ) + 
  scale_colour_manual(name = "" , values=c("no"=unibeRedS()[1],
                                           "yes"=unibeIceS()[1]), labels = c("No", "Yes")) +
  scale_fill_manual(name = "" , values=c("no"=unibeRedS()[1],
                                         "yes"=unibeIceS()[1]), labels = c("No", "Yes"))



#######################################

ggplot(insurance, aes(x=smoker, y= charges)) +
 ylab("Charges in $") + 
   geom_violin()


ggplot( insurance , aes(x = smoker, y = charges ) ) + 
  geom_boxplot(  ) + 
  ylab( "Charges ($)" ) + 
  coord_flip()

################################################