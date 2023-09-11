require(ggplot2)
require(plotly)
require(scales)
require(ggpubr)
require(ggridges)
require(ggdensity)
require(easynls)
require(aomisc)
Sys.setlocale("LC_ALL", "English")

rm(list=ls())

theme_graphs <- theme(panel.background = element_rect(fill = "white",
                                                      colour = "white",
                                                      size = NULL,
                                                      linetype = "solid",
                                                      color = NULL,
                                                      inherit.blank = FALSE))+
  theme(panel.grid.major = element_line(size = 0.5,
                                        linetype = "solid",
                                        colour = "white"))+
  theme(panel.grid.minor = element_line(size = 0.25,
                                        linetype = "solid",
                                        colour = ("white")))+
  theme(strip.background = element_rect(fill = "white", 
                                        colour = "white", 
                                        size = NULL, 
                                        linetype = "solid",
                                        color = NULL, 
                                        inherit.blank = FALSE))+
  theme(axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        axis.text.x = element_text(size =10), 
        axis.text.y = element_text(size =10)) +
  theme(axis.title=element_text(size=10))+
  theme(legend.title = element_text(face = "bold"))+
  theme(legend.text = element_text(face = "italic"))

### Loading data

raw <- read.table("inventory_posthatch.txt", header = T, sep = ",", dec = ".")

### WEIGHT and LENGTH ###
### Add intermediate biometries
BiometAMN28 <- c("AMN28", "f", 105, 26, 582, "07/03/2019", "05/08/2023", "cb")
BiometAMN28_2 <- c("AMN28", "f", 500, 43, 1477, "07/03/2019", "05/08/2023", "cb")
BiometAMN29 <- c("AMN29", "m", 92, 27, 569, "20/03/2019", "19/08/2022", "cb")
BiometAMN30 <- c("AMN30", "m", 72, 24, 561, "28/03/2019", "NA", "cb") 
BiometAMN30_2 <- c("AMN30", "m", 550, 41, 1456, "28/03/2019", "NA", "cb") 
BiometAMN35 <- c("AMN35", "f", 106, 26, 602, "28/05/2019", "NA", "cb")
BiometAMN35_2 <- c("AMN35", "f", 400, 30, 1395, "28/05/2019", "NA", "cb")
BiometAMN36 <- c("AMN36", "f", 62, 23, 596, "03/06/2019", "25/01/2021", "cb")
BiometAMN78 <- c("AMN78", "f", 15, 19, 190, "03/06/2019", "25/01/2021", "cb")
BiometAMN81 <- c("AMN81", "i", 11, 18, 163, "19/01/2022", "29/08/2022", "cb")
BiometAMN93 <- c("AMN93", "f", 23, 12, 222, "07/10/2022", "NA", "cb")
BiometAMN93_2 <- c("AMN93", "f", 59, 15, 277, "07/10/2022", "NA", "cb")
BiometAMN97 <- c("AMN97", "m", 18, 12, 87, "27/12/2022", "NA", "cb") 
BiometAMN97_2 <- c("AMN97", "m", 46.2, 17.2, 204, "27/12/2022", "NA", "cb")

rawLW <- rbind(raw, BiometAMN28)
rawLW <- rbind(rawLW, BiometAMN28_2)
rawLW <- rbind(rawLW, BiometAMN29)
rawLW <- rbind(rawLW, BiometAMN30)
rawLW <- rbind(rawLW, BiometAMN30_2)
rawLW <- rbind(rawLW, BiometAMN35)
rawLW <- rbind(rawLW, BiometAMN35_2)
rawLW <- rbind(rawLW, BiometAMN36)
rawLW <- rbind(rawLW, BiometAMN78)
rawLW <- rbind(rawLW, BiometAMN81)
rawLW <- rbind(rawLW, BiometAMN93)
rawLW <- rbind(rawLW, BiometAMN93_2)
rawLW <- rbind(rawLW, BiometAMN97)
rawLW <- rbind(rawLW, BiometAMN97_2)

### Fig weight ~ age
rawWeight <- rawLW[c("spec_nb","sex","weight","surv_days")]
rawWeight <- na.omit(rawLW)
pWeight <- ggplot(data=rawWeight, aes(x=as.numeric(surv_days), y=as.numeric(weight))) +
  geom_point(aes(color=sex), size =1.5) +
  scale_color_manual(labels = c("Female", "Indet.", "Male"),
                     values = c("f" = "#ff7557", "i" = "black", "m" = "#31c22d")) +
  guides(col=guide_legend("Sex")) +
  geom_smooth(method = "lm", formula= y~poly(x,2), colour = "black", size=0.5, alpha=0.3, se = FALSE) +
  xlab("Days post-hatching") + ylab("Weight (g)") +
  scale_y_continuous(breaks=seq(0,560,100)) +
  annotate("text", label = "A", x = 10, y = 520, size = 6, colour = "black") +
  theme_graphs
pWeight

### Fig total length ~ age
rawLength <- rawLW[c("spec_nb","sex","length","surv_days")]
rawLength <- na.omit(rawLW)
pLength <- ggplot(data=rawLength, aes(x=as.numeric(surv_days), y=as.numeric(length))) +
  geom_point(aes(color=sex),size =1.5) +
  scale_color_manual(labels = c("Female", "Indet.", "Male"),
                     values = c("f" = "#ff7557", "i" = "black", "m" = "#31c22d")) +
  guides(col=guide_legend("Sex")) +
  geom_smooth(method = "lm", formula= y~poly(x,2), colour = "black", size=0.5, alpha=0.3, se = FALSE) +
  scale_fill_discrete(labels=c('Female', 'Not determined', 'Male')) +
  xlab("Days post-hatching") + ylab("Total length (cm)") +
  ylim(7, 45) +
  annotate("text", label = "B", x = 10, y = 43, size = 6, colour = "black") +
  theme_graphs
pLength

### Fig length-weight relationship
# Fit a nonlinear regression
nlsfit <- nls(as.numeric(weight) ~ a * as.numeric(length)^b, rawWeight, start = list(a = 6.44, b = 0.10))
summary(nlsfit)
# Compute the R²
R.coef <- cor(as.numeric(rawWeight$weight), fitted(nlsfit))
R.coef^2
SSreg <- sum(fitted(nlsfit) - mean(as.numeric(rawWeight$weight)))
SStot <- sum((as.numeric(rawWeight$weight) - mean(as.numeric(rawWeight$weight)))^2 )
SSreg/SStot
SSres <- sum(residuals(nlsfit)^2)
1 - SSres/SStot

# Define the equation for the regression of POM data, based on summary(nlsfit)
nl_equation <- function(x){0.005979*x^3.041984}
# Equation from Barnett et al., 2009, after conversion from SVL to TL (based on KIng & McPhie, 2015)
barnett_eq <- function(x){1000*(2.459*10^-7*((x*10/2.4)^2.755))}
# Plot length-weight data and the regression curve
pLW <- ggplot(data=rawWeight, aes(x=as.numeric(length), y=as.numeric(weight))) + 
  geom_point(aes(color=sex),size =1.5) +
  scale_color_manual(labels = c("Female", "Indet.", "Male"),
                     values = c("f" = "#ff7557", "i" = "black", "m" = "#31c22d")) +
  guides(col=guide_legend("Sex")) +
  stat_function(fun=nl_equation) +
  stat_function(fun=barnett_eq, linetype="dashed") +
  scale_y_continuous(breaks=seq(0,560,100)) +
  xlab("Total length (cm)") + ylab("Weight (g)") +
  annotate("text", label = "C", x = 0, y = 520, size = 6, colour = "black") +
  theme_graphs
pLW

### Fig biometrics (two panels)
Fig_biometrics <- ggarrange(pWeight, pLength, pLW,
                            nrow=3, legend="none",
                            common.legend=TRUE, align = "v")
Fig_biometrics

### EGG LAYING FREQUENCY ###
eggs <- read.table("inventory_eggs.txt", header = T, sep = ",")

eggs$converted <- as.Date(eggs$laying, format="%d/%m/%Y")

pSpawning <- ggplot(data=eggs, aes(x=converted, fill=as.factor(year), 
                                               color=as.factor(year))) +
  geom_histogram(binwidth=15, show.legend = FALSE, alpha=0.6, size=0.1, colour = 1) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), text = element_text(size = 30)) +
  labs(y="Number of eggs", x="") +
  scale_fill_manual(values=c("#41e8d3","#668EDB", "#8c34e4")) +
  scale_color_manual(values=c("#41e8d3","#668EDB", "#8c34e4")) +
  scale_x_date(breaks = seq(as.Date("2020-01-01"), as.Date("2023-09-01"), by="2 months"), 
               date_labels = "%B", date_minor_breaks = "1 week") +
  theme(legend.position="none") +
  theme_graphs
pSpawning
