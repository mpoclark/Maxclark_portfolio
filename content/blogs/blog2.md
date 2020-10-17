---
categories:
- ""
- ""
date: "2017-10-31T22:26:09-05:00"
description: Past Projects
draft: false
image: LBS.jpg
keywords: ""
slug: N/A
title: Past Projects
---

```{r, setup, echo=FALSE}
knitr::opts_chunk$set(
  message = FALSE, 
  warning = FALSE, 
  tidy=FALSE,     # display code as typed
  size="small")   # slightly smaller font for code
options(digits = 3)

# default figure size
knitr::opts_chunk$set(
  fig.width=6.75, 
  fig.height=6.75,
  fig.align = "center"
)
```


```{r load-libraries, echo=FALSE}
library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(ggthemes)
library(GGally)
library(readxl)
library(here)
library(skimr)
library(janitor)
library(broom)
library(formattable)
library(tidyquant)
library(infer)
library(openintro)
library(tidyquant)
```


# Youth Risk Behavior Surveillance

Every two years, the Centers for Disease Control and Prevention conduct the [Youth Risk Behavior Surveillance System (YRBSS)](https://www.cdc.gov/healthyyouth/data/yrbs/index.htm) survey, where it takes data from high schoolers (9th through 12th grade), to analyze health patterns. You will work with a selected group of variables from a random sample of observations during one of the years the YRBSS was conducted.

## Load the data

This data is part of the `openintro` textbook and we can load and inspect it. There are observations on 13 different variables, some categorical and some numerical. The meaning of each variable can be found by bringing up the help file:

?yrbss

```{r}
data(yrbss)
glimpse(yrbss)
```

Before you carry on with your analysis, it's is always a good idea to check with `skimr::skim()` to get a feel for missing values, summary statistics of numerical variables, and a very rough histogram.

## Exploratory Data Analysis

You will first start with analyzing the `weight` of participants in kilograms. Using visualization and summary statistics, describe the distribution of weights. How many observations are we missing weights from?

```{r, eda_on_weight}

distribution_weight <- yrbss %>% 
select(weight) %>% 
filter(!is.na(weight)) %>% #Ensuring the filtering out of missing data points


summarise(mean=mean(weight),SD=sd(weight),median=median(weight),min=min(weight),max=max(weight))

distribution_weight #calculation display 
  
ggplot(yrbss,aes(x=weight))+  #plot building
  geom_density()+ 
  labs(title="Weights distribution appears approximately normal")+ 
  xlab("Weight (kg)")+ #axis labelling
  theme_bw() 

```


There is 1004 missing data (variable weight), and the graphs clearly show that the distribution is right-skewed. However, we can notice that it is still a bell shape and so it is quite normally distributed.


Next, consider the possible relationship between a high schooler’s weight and their physical activity. Plotting the data is a useful first step because it helps us quickly visualize trends, identify strong associations, and develop research questions.

Let’s create a new variable `physical_3plus`, which will be `yes` if they are physically active for at least 3 days a week, and `no` otherwise.

  
```{r}
yrbss <- yrbss %>% 
  mutate(physical_3plus = ifelse(physically_active_7d >= 3, "yes", "no"))

yrbss %>% filter(!is.na(physical_3plus)) %>% 
  group_by(physical_3plus) %>% 
  summarise(count = n()) %>% 
  mutate(prop= count/sum(count))

```
Can you provide a 95% confidence interval for the population proportion of high schools that are *NOT* active 3 or more days per week?

```{r}

inactive_CI <- yrbss %>%
  specify(response = physical_3plus, success="no") %>% 
  
  
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "prop") 
percentile_ci <- inactive_CI %>%
  get_ci(level = 0.95, type = "percentile") #choosing level of confidence

percentile_ci

```

  
Make a boxplot of `physical_3plus` vs. `weight`. Is there a relationship between these two variables? What did you expect and why?

```{r}

yrbss %>% 
  filter(!is.na(physical_3plus)) %>% 
ggplot(aes(x=physical_3plus,y=weight)) + geom_boxplot()+
  labs(title=" Relationship between physical activity & weight")+
  theme_bw()


```

Thanks to this graphs, we can clearly demonstrate that there is a correlation between physical activity and weight.As Median for people who do physical activity is a bit higher but because there is high weight outliers in the group without physical activity, we can condlude that there practice sport prevents from having a really high weight.


## Confidence Interval

Boxplots show how the medians of the two distributions compare, but we can also compare the means of the distributions using either a confidence interval or a hypothesis test. Note that when we calculate the mean/SD, etc weight in these groups using the mean function, we must ignore any missing values by setting the `na.rm = TRUE`.


```{r}
yrbss %>%
  group_by(physical_3plus) %>%
  filter(!is.na(physical_3plus)) %>% 
  summarise(mean_weight = mean(weight, na.rm = TRUE),
  sd_weight = sd(weight, na.rm=TRUE),
  count = n(),
  se_weight = sd_weight/sqrt(count),
  t_critical = qt(0.975, count-1), 
  margin_of_error = t_critical * se_weight,
  lower = mean_weight - t_critical * se_weight,
  upper = mean_weight + t_critical * se_weight
    )

```

There is an observed difference of about 1.77kg (68.44 - 66.67), and we notice that the two confidence intervals do not overlap. It seems that the difference is at least 95% statistically significant. Let us also conduct a hypothesis test.

## Hypothesis test with formula

Write the null and alternative hypotheses for testing whether mean weights are different for those who exercise at least times a week and those who don’t.

H0: Mean weights of students who exercise 3 or more times a week = Mean weights of students who exercise who do not

H1: Mean weights of students who exercise 3 or more times a week != Mean weights of students who exercise who do not

```{r}
t.test(weight ~ physical_3plus, data = yrbss)
```



## Hypothesis test with `infer`


Next, we will introduce a new function, `hypothesize`, that falls into the infer workflow. You will use this method for conducting hypothesis tests.

But first, we need to initialize the test, which we will save as `obs_diff`.

```{r}

obs_diff <- yrbss %>%
  specify(weight ~ physical_3plus) %>% #specifying the variable for which mean is 
  #calculated and the variable in which these means are going to differ
  calculate(stat = "diff in means", order = c("yes", "no")) #choosing the statistics: difference in means
#choosing order to satisfy the null hypothesis of yes - no != 0
obs_diff

```



Notice how you can use the functions specify and calculate again like you did for calculating confidence intervals. Here, though, the statistic you are searching for is the difference in means, with the order being yes - no != 0.

After you have initialized the test, you need to simulate the test on the null distribution, which we will save as null.


```{r}
null_dist <- yrbss %>%
  specify(weight ~ physical_3plus) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("yes", "no"))

```


Here, `hypothesize` is used to set the null hypothesis as a test for independence, i.e., that there is no difference between the two population means. In one sample cases, the null argument can be set to *point* to test a hypothesis relative to a point estimate.

Also, note that the `type` argument within generate is set to permute, which is the argument when generating a null distribution for a hypothesis test.

We can visualize this null distribution with the following code:

```{r}
ggplot(data = null_dist, aes(x = stat)) +
  geom_histogram(bins=20, fill="dark blue")+
  labs(title="The null distribution is normal")+
  theme_stata()
```



Now that the test is initialized and the null distribution formed, we can visualise to see how many of these null permutations have a difference of at least `obs_stat` of `r obs_diff %>% pull() %>% round(2)`?

We can also calculate the p-value for your hypothesis test using the function `infer::get_p_value()`.

```{r}

null_dist %>% visualize() +
  shade_p_value(obs_stat = obs_diff, direction = "two-sided")

null_dist %>%
  get_p_value(obs_stat = obs_diff, direction = "two_sided")

```


This the standard workflow for performing hypothesis tests.

# IMDB ratings: Differences between directors

Recall the IMBD ratings data. I would like you to explore whether the mean IMDB rating for Steven Spielberg and Tim Burton are the same or not. I have already calculated the confidence intervals for the mean ratings of these two directors and as you can see they overlap. 


```{r directors, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "directors.png"), error = FALSE)
```

First, I would like you to reproduce this graph. You may find `geom_errorbar()` and `geom_rect()` useful.

In addition, you will run a hpothesis test. You should use both the `t.test` command and the `infer` package to simulate from a null distribution, where you assume zero difference between the two.

> Before anything, write down the null and alternative hypotheses, as well as the resulting test statistic and the associated t-stat or p-value. At the end of the day, what do you conclude?

You can load the data and examine its structure

```{r load-movies-data}
library(readr)
movies <- read_csv("C:/Users/viktor/Desktop/Sep 2020-Mid 2021/LBS/Data Analytics for Finance/session6-workshop3/data/movies.csv")
View(movies)
```

Your R code and analysis should go here. If you want to insert a blank chunk of R code you can just hit `Ctrl/Cmd+Alt+I` 

```{r}

table <- movies %>% 
  
#Filtering data for mussing values & choosing the two previously named directors
  filter(!is.na(rating), director == 'Steven Spielberg' | director == 'Tim Burton') %>%
  group_by(director) %>% 
  
#Required statistic calculation
  summarise(mean_1= mean(rating), mean = round(mean_1, digits = 2), sd= sd(rating), t_critical=qt(.95, n() - 1), standard_error=sd(movies$rating)/sqrt(n()), lower_95_1 = mean - t_critical*(standard_error), upper_95_1 = mean + t_critical*(standard_error), lower_95= round(lower_95_1, digits = 2),upper_95= round(upper_95_1, digits = 2) )

table


CI_lower <- table %>% 
  filter(director == 'Steven Spielberg') %>% 
  select(lower_95)

CI_higher <- table %>% 
  filter(director == 'Tim Burton') %>% 
  select(upper_95)

#plotting
movies_plot <- table %>% 

#plotting relevant variables
  ggplot(aes(x = mean, y = reorder(director, mean), color = director, xmin = lower_95, xmax = upper_95)) +
  geom_errorbar(aes(xmin=lower_95, xmax=upper_95, color=director), width=0.1, size=1.5, scales = "free") +
  geom_point(aes(x = mean, y = director, color = director, size = 1.7))+

#Let's shade the common CI of the two directors.
  geom_rect(aes(xmin = CI_lower$lower_95, xmax = CI_higher$upper_95, ymin= -Inf, ymax=Inf), alpha=0.2, colour = "transparent")+
  
#Graph presentation
  geom_text(aes(label=mean), color= "grey", size=7, vjust=-0.7)+
  geom_text(aes(label=upper_95), color= "grey", size=4, vjust=-2, hjust=-7)+
  geom_text(aes(label=lower_95), color= "grey", size=4, vjust=-2, hjust=7)+
  theme_minimal()+
  labs(title= "Burton & Spielberg, do they have the same mean IMDB rating?",
               subtitle = "Overlap display of 95% CI",
               x="IMDB Mean Rating",
               y="")+
  theme(plot.title = element_text(family = "Arial", face = "bold", size = (11)),
          axis.title = element_text(family = "Arial", size = (9)),
          axis.text = element_text(family = "Arial", size = (9)))

movies_plot + theme(legend.position="none")
```



# Omega Group plc- Pay Discrimination


At the last board meeting of Omega Group Plc., the headquarters of a large multinational company, the issue was raised that women were being discriminated in the company, in the sense that the salaries were not the same for male and female executives. A quick analysis of a sample of 50 employees (of which 24 men and 26 women) revealed that the average salary for men was about 8,700 higher than for women. This seemed like a considerable difference, so it was decided that a further analysis of the company salaries was warranted. 

You are asked to carry out the analysis. The objective is to find out whether there is indeed a significant difference between the salaries of men and women, and whether the difference is due to discrimination or whether it is based on another, possibly valid, determining factor. 

## Loading the data


```{r load_omega_data}
library(readr)
omega <- read_csv("C:/Users/viktor/Desktop/Sep 2020-Mid 2021/LBS/Data Analytics for Finance/session6-workshop3/data/omega.csv")
View(omega)
```

## Relationship Salary - Gender ?

The data frame `omega`  contains the salaries for the sample of 50 executives in the company. Can you conclude that there is a significant difference between the salaries of the male and female executives?

Note that you can perform different types of analyses, and check whether they all lead to the same conclusion 

.	Confidence intervals
.	Hypothesis testing
.	Correlation analysis
.	Regression


Calculate summary statistics on salary by gender. Also, create and print a dataframe where, for each gender, you show the mean, SD, sample size, the t-critical, the SE, the margin of error, and the low/high endpoints of a 95% condifence interval

```{r, confint_single_valiables}
mosaic::favstats (salary ~ gender, data=omega)

```

> What can you conclude from your analysis? A couple of sentences would be enough

You can also run a hypothesis testing, assuming as a null hypothesis that the mean difference in salaries is zero, or that, on average, men and women make the same amount of money. You should tun your hypothesis testing using `t.test()` and with the simulation method from the `infer` package.

```{r, hypothesis_testing_1}
# hypothesis testing using t.test() 
t.test(salary ~ gender, data = omega)

# hypothesis testing using infer package
set.seed(1234)
salary_gender2 <- omega %>% 
  specify(salary ~ gender) %>% 
  hypothesize(null="independence") %>% 
  generate(reps=1000, type = "permute") %>% 
  calculate(stat="diff in means", order = c("female", "male"))

salary_gender2 %>% visualise()

salary_gender2 %>% 
  get_p_value(obs_stat= 73200 - 64500, direction="both")

```


> What can you conclude from your analysis? A couple of sentences would be enough


## Relationship Experience - Gender?

At the board meeting, someone raised the issue that there was indeed a substantial difference between male and female salaries, but that this was attributable to other reasons such as differences in experience. A questionnaire send out to the 50 executives in the sample reveals that the average experience of the men is approximately 21 years, whereas the women only have about 7 years experience on average (see table below).

```{r, experience_stats}
# Summary of salary by gender
favstats (experience ~ gender, data=omega)

# plotting
omega %>% 
  ggplot(aes(x = gender, y = experience)) +
  geom_boxplot() + 
  labs(x = "",
       title = "Experience of Men outweighs experience of women at Omega",
       subtitle = "Plotting experience Males vs. Females") +
  theme_minimal()

```


Based on this evidence, can you conclude that there is a significant difference between the experience of the male and female executives? Perform similar analyses as in the previous section. Does your conclusion validate or endanger your conclusion about the difference in male and female salaries?  


Based on the analysis, we can notice that there is a significant difference from male and female salaries, with a confidence of 95%. Furthermore because the standard deviation for female executives tends to be higher than the male one, we can conclude that male executive salaries are closer to the mean.


## Relationship Salary - Experience ?

Someone at the meeting argues that clearly, a more thorough analysis of the relationship between salary and experience is required before any conclusion can be drawn about whether there is any gender-based salary discrimination in the company.

Analyse the relationship between salary and experience. Draw a scatterplot to visually inspect the data


```{r, salary_exp_scatter}
ggplot(omega, aes(x = experience, y = salary)) +
  geom_point(aes(color = gender)) +
  geom_smooth() +
  theme_minimal() +
  labs(title = " Those with more experience tend to get better pay",
       subtitle = "Experience against pay")

ggplot(omega, aes(x = experience, y = salary, color = gender)) +
  geom_point() +
  geom_smooth() +
  theme_minimal() +
  labs(title = "Males tend to have have higher pay than females with equal experience at a middle career level",
       subtitle = " Difference between salary and experience by gender")

```

## Check correlations between the data
You can use `GGally:ggpairs()` to create a scatterplot and correlation matrix. Essentially, we change the order our variables will appear in and have the dependent variable (Y), salary, as last in our list. We then pipe the dataframe to `ggpairs()` with `aes` arguments to colour by `gender` and make ths plots somewhat transparent (`alpha  = 0.3`).

```{r, ggpairs}
omega %>% 
  select(gender, experience, salary) %>% 
  ggpairs(aes(colour=gender, alpha = 0.3))+
  theme_bw()
```

> Look at the salary vs experience scatterplot. What can you infer from this plot? Explain in a couple of sentences

Based on scatterplots of both salary vs experience, we can infer that female executive tends to have lower salaries even if they have the same amount of experience, compared to male executive salaries.
Furthermore, we can notice that female with more than 20 years of experience are less common, and in the other hand, woman with 0 years of experience or more common. We can only suppose that for example, it tends to be more and more female executives on companies due to gender inequalities concerns, but no data can we really confirm if that is the reason for this trend.

