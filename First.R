library(tidyverse)
library(tidybayes)
library(brms)
library(posterior)
library(marginaleffects)
library(gtsummary)
library(flextable)

df <- readxl::read_excel("df1.xlsx")

str(df)

df <- df |> 
 select(-c(`Дата госпитализации`, `Дата выписки`, Группы_по_CCI, ycT:ycM, `Дата операции`,
           `Сочетанные операции (какие)`, `Комбинированные операции (какие)`, `Длина кассеты`,
           `Время операции, мин`, `pCR Mandard`:Ryan, `Дата закрытия стомы`, `Дата контакта OS`,
           `Дата контакта PFS`, `Интраоперационные осложнения`)) |> 
 mutate_if(is.character, as.factor) |> 
 mutate(across(c(cT:Стадия, Послеоперационные_осложнения, `Clavien-Dindo`, Осложнения_все), 
               as.factor)) |> 
 mutate(ID = as.integer(ID)) |> 
 rename(Несостоятельность_анастомоза = `Несостоятельность анастомоза`)

tbl_summary(df, 
            type = list(all_continuous() ~ "continuous2", CCI ~ "continuous2"),
            statistic = all_continuous() ~ c("{mean} ({sd})", "{median} ({p25}, {p75})", 
                                             "{min}, {max}"))

explore::explore(df, target = Высота_опухоли, Несостоятельность_анастомоза)

explore::describe(df) |> 
 flextable()


rbeta(100, shape1 = 10, shape2 = 90) |> 
 hist()

rbeta_binomial(n = 100, size = 1, mu = 0.09, phi = 0.91) |> 
 hist()

rnorm(100, 0, 3)

fit1 <- brm(formula = Несостоятельность_анастомоза ~ Высота_опухоли, 
    data = df, 
    family = "bernoulli", 
    prior = c(prior(normal(0.1, 0.03), class = "Intercept"),
              prior(normal(9, 2), class = "b")),
    backend = "cmdstanr"
    )

remotes::install_github("stan-dev/cmdstanr")

install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

remotes::install_github("stan-dev/cmdstanr", force = T)

