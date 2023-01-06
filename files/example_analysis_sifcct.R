## Clear Workspace
rm(list = ls())

## Set Working Directory to the File location
## (If using RStudio, can be set automatically)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

####################
## Import Dataset ##
####################

## Get Dataset "Survey on the Images of Foreign Countries and Current Topics"
## Check https://doi.org/10.7910/DVN/LTJEO9 for more details
library(dataverse)
d <- get_dataframe_by_name(
  filename = "sifcct_hdv_wave1-12.tab",
  dataset = "10.7910/DVN/LTJEO9", 
  server = "dataverse.harvard.edu")

## Limit the dataset to wave 10 and 11
d <- subset(d, wave %in% c(10,11) & panel == 0)

###############
## Variables ##
###############

## Initiate Dataset
dn <- data.frame(id = paste(d$wave,d$caseid, sep="_"))
head(dn)

## Dependent Variable: Feeling Towards South Korea (0 through 100)
### **Replace DK (888) NA (999) with missing 
table(d$i14a3) 
dn$feelsk <- ifelse(d$i14a3%in%c(888,999),NA,d$i14a3)
table(dn$feelsk)

## Independent Variable: Question Asked on August 2012 (versus July 2012)
### **SK president landed disputed island (Takeshima) on August 10, 2012
dn$threatened <- ifelse(d$wave==11, 1, 0)
table(dn$threatened)

## Controls

### Age
table(d$i39)
dn$age <- ifelse(d$i39==99, NA, d$i39)
### Female
table(d$i38)
dn$female <- ifelse(d$i38==2, 1, 0)
### Education
table(d$i40)
dn$juniorcollege <- ifelse(d$i40==5, NA, ifelse(d$i40==3,1,0))
dn$university <- ifelse(d$i40==5, NA, ifelse(d$i40==4,1,0))
### Income
table(d$i41)
# dn$incomelow <- ifelse(d$i41%in%c(1,2,3),1,0)
dn$incomemid <- ifelse(d$i41%in%c(4,5),1,0)
dn$incomehigh <- ifelse(d$i41%in%c(6,7,8),1,0)
dn$incomemiss <- ifelse(d$i41%in%c(9,10),1,0)

## Omit NAs
dn <- na.omit(dn)

############################
## Presenting Descriptive ##
############################

## If Nominal/Ordinal Variable, use Barplot ## 

table(dn$threatened)

plotdata <- data.frame(x = names(table(dn$threatened)), # Category Names
                       y = as.numeric(table(dn$threatened)))
plotdata$x <- factor(plotdata$x, levels=unique(plotdata$x))
plotdata

## Plot
require(ggplot2)

## The Basic
ggplot(plotdata, aes(x=x, y=y)) + 
  geom_bar(stat = "identity")

## Choose Classic Theme
ggplot(plotdata, aes(x=x, y=y)) + 
  geom_bar(stat = "identity") + 
  theme_classic()

## Choose Black-White Theme
ggplot(plotdata, aes(x=x, y=y)) + 
  geom_bar(stat = "identity") + 
  theme_bw()

## Choose Filling color and transparency of bars
ggplot(plotdata, aes(x=x, y=y)) + 
  geom_bar(stat = "identity", #color = "red", 
           fill = "palegreen4", alpha = 0.7) + 
  theme_bw()

## Find favoriate colors from some references e.g., 
## http://sape.inf.usi.ch/quick-reference/ggplot2/colour

## Use Labels on X-axis Category Names, instead of 0, 1
ggplot(plotdata, aes(x=x, y=y)) + 
  geom_bar(stat = "identity", fill = "palegreen4", alpha = 0.7) + 
  scale_x_discrete(breaks = c(0,1), labels = c("Before (0)","After (1)")) + 
  theme_bw()

## Set x and y axis titles manually
ggplot(plotdata, aes(x=x, y=y)) + 
  geom_bar(stat = "identity", fill = "palegreen4", alpha = 0.7) + 
  scale_x_discrete(breaks = c(0,1), labels = c("Before (0)","After (1)")) + 
  labs(x = "Before and After the Threatening Event \n South Korean President's Landing on Takeshima",
       y = "Frequency") + 
  theme_bw()

## Add footnote & title
ggplot(plotdata, aes(x=x, y=y)) + 
  geom_bar(stat = "identity", fill = "palegreen4", alpha = 0.7) + 
  scale_x_discrete(breaks = c(0,1), labels = c("Before (0)","After (1)")) + 
  labs(x = "Before and After the Threatening Event \n South Korean President's Landing on Takeshima",
       y = "Frequency", 
       caption = "Data: Survey on the Images of Foreign Countries and Current Topics.",
       title = "Distribution of Before and After the Threatening Event \n South Korean President's Landing on Takeshima") + # \n is line break
  theme_bw()

## Managing text of title, caption, and axis titles
ggplot(plotdata, aes(x=x, y=y)) + 
  geom_bar(stat = "identity", fill = "palegreen4", alpha = 0.7) + 
  scale_x_discrete(breaks = c(0,1), labels = c("Before (0)","After (1)")) + 
  labs(x = "Before and After the Threatening Event \n South Korean President's Landing on Takeshima",
       y = "Frequency", 
       caption = "Data: Survey on the Images of Foreign Countries and Current Topics.",
       title = "Distribution of Before and After the Threatening Event \n South Korean President's Landing on Takeshima") + # \n is line break
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5, size = 11),
        plot.caption = element_text(hjust=1, size = 8),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "italic"),
        axis.text.x = element_text(color = "red"), 
        axis.text.y = element_text(color = "blue"))

## Save Plot (that is displayed right now)
ggsave("barplot.png", width = 6, height = 4)

## If Continuous Variable, Use Histogram

require(ggplot2)

## Using Default
ggplot(dn, aes(x=feelsk)) + 
  geom_histogram() + 
  theme_bw()

## Managing Number of Bins
ggplot(dn, aes(x=feelsk)) + 
  geom_histogram(bins = 10) + # 10 bars 
  theme_bw()

## Another way
ggplot(dn, aes(x=feelsk)) + 
  geom_histogram(binwidth = 10) + # By 10 
  theme_bw()

## Adding density line
ggplot(dn, aes(x=feelsk)) + 
  geom_histogram(binwidth = 10) + # By 10 percent 
  geom_density(aes(y=..count..*10), bw=10) + 
  theme_bw()

## You can (of course) change colors and transparency
ggplot(dn, aes(x=feelsk)) + 
  geom_histogram(binwidth = 10, fill="orange", alpha=0.7) +  
  geom_density(aes(y=..count..*10), bw=10, color="darkgreen") + 
  theme_bw()

## Managing Axis labels
ggplot(dn, aes(x=feelsk)) + 
  geom_histogram(binwidth = 10, fill="orange", alpha=0.7) +  
  geom_density(aes(y=..count..*10), bw=10, color="darkgreen") + 
  scale_x_continuous(breaks = seq(0,100,by=10)) + 
  scale_y_continuous(breaks = c(0,500,1000)) + 
  theme_bw()

## Plus Some Labels
ggplot(dn, aes(x=feelsk)) + 
  geom_histogram(binwidth = 10, fill="orange", alpha=0.7) +  
  geom_density(aes(y=..count..*10), bw=10, color="darkgreen") + 
  scale_x_continuous(breaks = seq(0,100,by=10)) + 
  scale_y_continuous(breaks = c(0,500,1000)) + 
  labs(x = "Feeling Towards South Korea",
       y = "Counts", 
       caption = "Data: Survey on the Images of Foreign Countries and Current Topics.",
       title = "Distribution of \nFeeling Towards South Korea") + # \n is line break
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5, size = 11),
        plot.caption = element_text(hjust=1, size = 8),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "italic"))

## Let's Delete Y-axis labels and ticks
ggplot(dn, aes(x=feelsk)) + 
  geom_histogram(binwidth = 10, fill="orange", alpha=0.7) +  
  geom_density(aes(y=..count..*10), bw=10, color="darkgreen") + 
  scale_x_continuous(breaks = seq(0,100,by=10)) + 
  scale_y_continuous(breaks = c(0,500,1000)) + 
  labs(x = "South Korea Feeling Thermometer",
       y = NULL, 
       caption = "Data: Survey on the Images of Foreign Countries and Current Topics.",
       title = "Distribution of \nFeeling Towards South Korea") + # \n is line break
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5, size = 11),
        plot.caption = element_text(hjust=1, size = 8),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank())

## Save Plot (that is displayed right now)
ggsave("histogram.png", width = 6, height = 4)

## Generate tables instead

## Format the Summary Output
(tmp <- summary(dn[,c("threatened","age","female","juniorcollege","university","incomemid","incomehigh","incomemiss")]))
(tmp <- matrix(tmp, ncol = ncol(tmp), dimnames = dimnames(tmp))) # Make it matrix
(tmp <- t(tmp)) # Transpose
(tmp <- gsub("^.*:","",tmp)) ## Delete Unecessary labels
for(i in 1:(ncol(tmp)-1)) tmp[,i] <- sprintf("%.3f",as.numeric(tmp[,i])) ## 3 digits
colnames(tmp) <- c("Minimum","25%ile","Median","Mean","75%ile","Maximum") 
rownames(tmp) <- c("Threatened","Age","Female","Junior College","University",
                   "Middle Income","High Income","Income Missing") 
(descstat <- tmp)

## Basically, what you need is a matrix/data.frame with 
## appropriate row and column names

## Generate as More Professional Look Tables

#### HTML Table
require(xtable)
print(xtable(descstat, caption = "Distributions of predictors", # table title
             label = "descstat", # labels 
             align = "lcccccc"), 
      # alignment "l" for left, "r" for right, "c" for center
      type = "html", caption.placement = "top",
      html.table.attributes = sprintf("style='%s'",
                                      paste("border:0",
                                            "border-top: 1px solid grey", 
                                            "border-second: 1px solid grey", 
                                            "border-bottom: 1px solid grey",
                                            sep="; ")),
      file = "descstat.html")

#### TeX Table (Explained in Separate Document)
require(xtable)
print(xtable(descstat, caption = "Distributions of predictors", digits = 3,
             label = "descstat", align = "lcccccc"),
      type = "latex", booktabs = TRUE, caption.placement = "top",
      table.placement = "ht!", size = "footnotesize",
      file = "descstat.tex")

##############
## Analysis ##
##############

## Baseline Model
m0 <- lm(feelsk ~ threatened, data = dn)

## Full Model
m1 <- lm(feelsk ~ threatened + age + female + 
           juniorcollege + university + 
           incomemid + incomehigh + incomemiss, 
         data = dn)

## Age Interaction Model
m2 <- lm(feelsk ~ threatened * age + female + 
           juniorcollege + university + 
           incomemid + incomehigh + incomemiss, 
         data = dn)

#########################
## Presenting Analysis ##
#########################

## Using Texreg Package
library(texreg)

### The Basic
screenreg(list(m0,m1,m2))

### Managing Digits and Line-breaks format
screenreg(list(m0,m1,m2), digits = 3, single.row = TRUE)

### Managing Model and Coefficient Names
screenreg(list(m0,m1,m2), digits = 3, single.row = TRUE,
          custom.model.names = c("Baseline", "Interacted", "Full"),
          custom.coef.map = list("(Intercept)"="(Intercept)",
                                 "threatened"="Threatend",
                                 "age"="Age",
                                 "threatened:age"="Threatened * Age",
                                 "female"="Female",
                                 "juniorcollege"="Junior College",
                                 "university"="University",
                                 "incomemid"="Middle Income",
                                 "incomehigh"="High Income",
                                 "incomemiss"="Income Missing"))

### Managing P-values
screenreg(list(m0,m1,m2), digits = 3, single.row = TRUE,
          custom.model.names = c("Baseline", "Interacted", "Full"),
          custom.coef.map = list("(Intercept)"="(Intercept)",
                                 "threatened"="Threatend",
                                 "age"="Age",
                                 "threatened:age"="Threatened * Age",
                                 "female"="Female",
                                 "juniorcollege"="Junior College",
                                 "university"="University",
                                 "incomemid"="Middle Income",
                                 "incomehigh"="High Income",
                                 "incomemiss"="Income Missing"), 
          stars = c(0.001,0.01,0.05,0.1), symbol = "+")

### Managing Footnote
screenreg(list(m0,m1,m2), digits = 3, single.row = TRUE,
          custom.model.names = c("Baseline", "Interacted", "Full"),
          custom.coef.map = list("(Intercept)"="(Intercept)",
                                 "threatened"="Threatend",
                                 "age"="Age",
                                 "threatened:age"="Threatened * Age",
                                 "female"="Female",
                                 "juniorcollege"="Junior College",
                                 "university"="University",
                                 "incomemid"="Middle Income",
                                 "incomehigh"="High Income",
                                 "incomemiss"="Income Missing"), 
          stars = c(0.001,0.01,0.05,0.1), symbol = "+",
          custom.note = "%stars. Estimated by OLS regression.")

# Export as HTML table
htmlreg(list(m0,m1,m2), digits = 3, single.row = TRUE,
        custom.model.names = c("Baseline", "Interacted", "Full"),
        custom.coef.map = list("(Intercept)"="(Intercept)",
                               "threatened"="Threatend",
                               "age"="Age",
                               "threatened:age"="Threatened * Age",
                               "female"="Female",
                               "juniorcollege"="Junior College",
                               "university"="University",
                               "incomemid"="Middle Income",
                               "incomehigh"="High Income",
                               "incomemiss"="Income Missing"), 
        stars = c(0.001,0.01,0.05,0.1), symbol = "+",
        custom.note = "%stars. Estimated by OLS regression.",
        caption = "Explaining Japanese people's feeling towards South Korea",
        caption.above = TRUE, 
        file = "regout_feelsk.html")

# Export as HTML table (word readable)
wordreg(list(m0,m1,m2), digits = 3, single.row = TRUE,
        custom.model.names = c("Baseline", "Interacted", "Full"),
        custom.coef.map = list("(Intercept)"="(Intercept)",
                               "threatened"="Threatend",
                               "age"="Age",
                               "threatened:age"="Threatened * Age",
                               "female"="Female",
                               "juniorcollege"="Junior College",
                               "university"="University",
                               "incomemid"="Middle Income",
                               "incomehigh"="High Income",
                               "incomemiss"="Income Missing"), 
        stars = c(0.001,0.01,0.05,0.1), symbol = "+",
        custom.note = "%stars. Estimated by OLS regression.",
        caption = "Explaining Japanese people's feeling towards South Korea",
        caption.above = TRUE, 
        file = "regout_feelsk.docx")

# Export as TeX table (Explained More Later If There is A Time)
texreg(list(m0,m1,m2), digits = 3, single.row = TRUE,
       custom.model.names = c("Baseline", "Interacted", "Full"),
       custom.coef.map = list("(Intercept)"="(Intercept)",
                              "threatened"="Threatend",
                              "age"="Age",
                              "threatened:age"="Threatened * Age",
                              "female"="Female",
                              "juniorcollege"="Junior College",
                              "university"="University",
                              "incomemid"="Middle Income",
                              "incomehigh"="High Income",
                              "incomemiss"="Income Missing"), 
       stars = c(0.001,0.01,0.05,0.1), symbol = "+",
       include.deviance = FALSE, 
       custom.note = "%stars. Estimated by OLS regression.",
       caption = "Explaining Japanese people's feeling towards South Korea",
       caption.above = TRUE, 
       booktabs = TRUE, dcolumn = TRUE, use.packages = FALSE,
       label = "tab_feelsk", float.pos = "ht!", fontsize = "footnotesize", 
       file = "regout_feelsk.tex")


## Using stargazer package
require(stargazer)

## In R Console
stargazer(m0,m1,m2, type="text", digits = 3, single.row = TRUE,
          column.labels = c("Baseline", "Interacted", "Full"),
          model.numbers = FALSE, dep.var.caption = "Models",
          dep.var.labels.include = FALSE,
          # Order of coefficient appearance
          order = c("Constant","^threatened$","^age$","^threatened:age$","^female$",
                    "^juniorcollege$","^university$","^incomemid$","^incomehigh$",
                    "^incomemiss$"), 
          covariate.labels = c("(Intercept)",
                               "Threatend",
                               "Age",
                               "Threatened * Age",
                               "Female",
                               "Junior College",
                               "University",
                               "Middle Income",
                               "High Income",
                               "Income Missing"), 
          star.char = c("+","*","**","***"),
          star.cutoffs = c(0.1,0.05,0.01,0.001),
          title = "Explaining feelsk Win/Lose by Districts",
          notes = paste("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001.",
                        "Estimated by OLS regression."), 
          notes.append = FALSE)

## In HTML -> Word (Open in HTML & Copy to Word)
## This one works better than texreg html to word
stargazer(m0,m1,m2, type="html", digits = 3, single.row = TRUE,
          column.labels = c("Baseline", "Interacted", "Full"),
          model.numbers = FALSE, dep.var.caption = "Models",
          dep.var.labels.include = FALSE,
          # Order of coefficient appearance
          order = c("Constant","^threatened$","^age$","^threatened:age$","^female$",
                    "^juniorcollege$","^university$","^incomemid$","^incomehigh$",
                    "^incomemiss$"), 
          covariate.labels = c("(Intercept)",
                               "Threatend",
                               "Age",
                               "Threatened * Age",
                               "Female",
                               "Junior College",
                               "University",
                               "Middle Income",
                               "High Income",
                               "Income Missing"), 
          star.char = c("+","*","**","***"),
          star.cutoffs = c(0.1,0.05,0.01,0.001),
          title = "Explaining feelsk Win/Lose by Districts",
          notes = paste("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001.",
                        "Estimated by logistic regression."), 
          notes.append = FALSE,
          out = "regout_feelsk_stargazer.html")

## In TeX (return to double-row plot)
stargazer(m0,m1,m2, type="latex", digits = 3, single.row = TRUE,
          column.labels = c("Baseline", "Interacted", "Full"), 
          model.numbers = FALSE, dep.var.caption = "Models",
          dep.var.labels.include = FALSE,
          # Order of coefficient appearance
          order = c("Constant","^threatened$","^age$","^threatened:age$","^female$",
                    "^juniorcollege$","^university$","^incomemid$","^incomehigh$",
                    "^incomemiss$"), 
          covariate.labels = c("(Intercept)",
                               "Threatend",
                               "Age",
                               "Threatened * Age",
                               "Female",
                               "Junior College",
                               "University",
                               "Middle Income",
                               "High Income",
                               "Income Missing"), 
          star.char = c("+","*","**","***"),
          star.cutoffs = c(0.1,0.05,0.01,0.001),
          title = "Explaining Japanese People's Feeling Towards South Korea",
          notes = paste("$^+p<0.1$; $^*p<0.05$; $^{**}p<0.01$; $^{***}p<0.001$.",
                        "Estimated by OLS regression."), 
          notes.append = FALSE,
          align = TRUE, column.sep.width = "-30pt", # Column Separation Width to None.
          label = "tab_feelsk_stargazer", table.placement = "ht!", font.size = "footnotesize", 
          out = "regout_feelsk_stargazer.tex")

##########################
## Plotting Predictions ##
##########################

## Setting Profiles ##

## Age 20 others at mode or mid point
setprof1 = data.frame(threatened = c(0,1), # Independent Variable
                      threatenedlab = c("Before\nThreat","After\nThreat"),
                     age = 20, ageset = "Young (Age=20)",
                     female = 0, juniorcollege = 0, university = 1, 
                      incomemid = 1, incomehigh = 0, incomemiss = 0)
setprof1$threatenedlab <- 
  factor(setprof1$threatenedlab, levels=unique(setprof1$threatenedlab))

## Age 70 others at mode or mid point
setprof2 = data.frame(threatened = c(0,1),
                      threatenedlab = c("Before\nThreat","After\nThreat"),
                      age = 70, ageset = "Elderly (Age=70)",
                      female = 0, juniorcollege = 0, university = 1, 
                      incomemid = 1, incomehigh = 0, incomemiss = 0)
setprof2$threatenedlab <- 
  factor(setprof2$threatenedlab, levels=unique(setprof2$threatenedlab))

## Calculating Predicted Values ##

## Prediction from Profile 1
pr1 <- predict(m2, newdata = setprof1, se.fit = T)
## Storing predictions to profile
setprof1$pr = pr1$fit
setprof1$lci = pr1$fit - qnorm(0.975)*pr1$se.fit # 95% CI
setprof1$uci = pr1$fit + qnorm(0.975)*pr1$se.fit # 95% CI

## Prediction from Profile 2
pr2 <- predict(m2, newdata = setprof2, se.fit = T)
## Storing predictions to profile
setprof2$pr = pr2$fit
setprof2$lci = pr2$fit - qnorm(0.975)*pr2$se.fit # 95% CI
setprof2$uci = pr2$fit + qnorm(0.975)*pr2$se.fit # 95% CI

### Plotting One Prediction

require(ggplot2)

## The Basic
ggplot(setprof1, aes(x=threatenedlab, y=pr)) + 
  geom_point() + 
  theme_bw()

## With Confidence Interval (alpha is transparency)
ggplot(setprof1, aes(x=threatenedlab, y=pr)) + 
  geom_errorbar(aes(ymin=lci, ymax=uci), width=0.2) + 
  geom_point(size = 2.5) + 
  theme_bw()

## Add and modify labels
ggplot(setprof1, aes(x=threatenedlab, y=pr)) + 
  geom_errorbar(aes(ymin=lci, ymax=uci), width=0.2) + 
  geom_point(size = 2.5) + 
  scale_y_continuous(breaks = c(20,30,40)) +
  labs(x = "Before and After Threatening Event,\n i.e., South Korean President's Landing on Takeshima", 
       y = "Predicted Value of \nFeeling Towards South Korea",
       caption = "Estimated by OLS. For Age = 20, Male, Univerisy Graduate, and Middle Income.",
       title = "Predicting Japanese People's Feeling Towards South Korea") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5, size = 11),
        plot.caption = element_text(hjust=1, size = 8),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "italic"))

## Save Plot (that is displayed right now)
ggsave("predplot_single.png", width = 6, height = 4)

#'
#' ### Plotting Multiple Predictions
#'

setprof <- rbind(setprof1,setprof2)
setprof$ageset <- factor(setprof$ageset,
                            levels = unique(setprof$ageset))

## Split predictions by facetting 
## (Adding subtitle. Also check plot.subtitle option)
ggplot(setprof, aes(x=threatenedlab, y=pr)) + 
  geom_errorbar(aes(ymin=lci, ymax=uci), width=0.2) + 
  geom_point(size = 2.5) + 
  scale_y_continuous(breaks = c(20,30,40)) +
  facet_grid(. ~ ageset) + 
  labs(x = "Before and After Threatening Event,\n i.e., South Korean President's Landing on Takeshima", 
       y = "Predicted Value of \nFeeling Towards South Korea",
       caption = "Estimated by OLS. For Age = 20, Male, Univerisy Graduate, and Middle Income.",
       title = "Predicting Japanese People's Feeling Towards South Korea") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5, size = 11),
        plot.subtitle = element_text(hjust=0.5, size=10, face="bold"),
        plot.caption = element_text(hjust=1, size = 8),
        axis.title.x = element_text(face = "bold", size=10),
        axis.title.y = element_text(face = "italic", size=10))

## Save Plot (that is displayed right now)
ggsave("predplot_multiple1.png", width = 6, height = 4)

## Differentiate Predictions by colors and point shape
ggplot(setprof, aes(x=threatenedlab, y=pr)) + 
  geom_errorbar(aes(ymin=lci, ymax=uci, color=ageset), width=0.2, 
                position = position_dodge(width=0.5)) + 
  geom_point(aes(shape=ageset, color=ageset), size = 2.5, 
             position = position_dodge(width=0.5)) + 
  scale_y_continuous(breaks = c(20,30,40)) +
  labs(x = "Before and After Threatening Event,\n i.e., South Korean President's Landing on Takeshima", 
       y = "Predicted Value of \nFeeling Towards South Korea",
       caption = "Estimated by OLS. For Male, Univerisy Graduate, and Middle Income.",
       title = "Predicting Japanese People's Feeling Towards South Korea") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5, size = 11),
        plot.subtitle = element_text(hjust=0.5, size=10, face="bold"),
        plot.caption = element_text(hjust=1, size = 8),
        axis.title.x = element_text(face = "bold", size=10),
        axis.title.y = element_text(face = "italic", size=10),
        legend.position = "bottom")

## Modify colors, linetypes and labels
ggplot(setprof, aes(x=threatenedlab, y=pr)) + 
  geom_errorbar(aes(ymin=lci, ymax=uci, color=ageset), width=0.2, 
                position = position_dodge(width=0.5)) + 
  geom_point(aes(shape=ageset, color=ageset), size = 2.5, 
             position = position_dodge(width=0.5)) + 
  scale_y_continuous(breaks = c(20,30,40)) +
  scale_shape_discrete(name="Age") + 
  scale_color_brewer(name="Age", type="qual", palette=2) + 
  labs(x = "Before and After Threatening Event,\n i.e., South Korean President's Landing on Takeshima", 
       y = "Predicted Value of \nFeeling Towards South Korea",
       caption = "Estimated by OLS. For Male, Univerisy Graduate, and Middle Income.",
       title = "Predicting Japanese People's Feeling Towards South Korea") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5, size = 11),
        plot.subtitle = element_text(hjust=0.5, size=10, face="bold"),
        plot.caption = element_text(hjust=1, size = 8),
        axis.title.x = element_text(face = "bold", size=10),
        axis.title.y = element_text(face = "italic", size=10),
        legend.position = "bottom")

## Save Plot (that is displayed right now)
ggsave("predplot_multiple2.png", width = 6, height = 4)
