install.packages("beepr")
library(beepr)

beepr::beep(11)
beep(5)

?beep

lapply(1:11, beep)

hist(replicate(1000000, mean(rlnorm(100))))
beepr::beep(11)

remotes::install_github("brooke-watson/BRRR")
library(BRRR)

BRRR::skrrrahh_list()

BRRR::skrrrahh(0)

install.packages("fun")
library(fun)

# Open the right interactive graphics device
if (.Platform$OS.type == "windows") {
  x11()
} else {
  x11(type = "Xlib")
}

fun::sliding_puzzle()
fun::random_password()
fun::mine_sweeper()
fun::tower_of_hanoi(n = 3)

install.packages("fortunes")
library(fortunes)
fortunes::fortune()
fortunes::fortune("normal")

install.packages("cowsay")
library(cowsay)
say("catfact")
say("fortune")
say("time")
say("rms", by = "rms")
say(by = 'hypnotoad')

say("Не ешь меня!", by = "bat", what_color = "green", by_color = "#DD22FF")
say("fortune", by = "longcat", length = sample(10:200, size = 1))

library(praise)
praise("${Exclamation}! Kind reader, you're ${adjective}!")
?praise
praise_parts
praise("${created}")

praise("I am sorry... (not really) ${exclamation} You are not so ${adjective}")

install.packages("skimr")
library(skimr)
iris
skim(iris)
library(tidyverse)
iris %>%
  group_by(Species) %>%
  skim()

iris %>%
  group_by(Species) %>%
  select(starts_with("Petal")) %>%
  skim()

install.packages("calendR")
library(calendR)
calendR()

devtools::install_github("gsimchoni/kandinsky")
library(kandinsky)
kandinsky(iris[,1] + c(0,0.05))
kandinsky(mtcars)
kandinsky(log(mtcars))
?kandinsky

identical(iris[,1], iris$Sepal.Length)
iris[,1] == iris$Sepal.Length

install.packages("arsenal")
library(arsenal)
df1 <- data.frame(id = paste0("person", 1:3),
                  a = c("a", "b", "c"),
                  b = c(1, 3, 4),
                  c = c("f", "e", "d"),
                  row.names = paste0("rn", 1:3),
                  stringsAsFactors = FALSE)
df2 <- data.frame(id = paste0("person", 3:1),
                  a = c("c", "b", "a"),
                  b = c(1, 3, 4),
                  d = paste0("rn", 1:3),
                  row.names = paste0("rn", c(1,3,2)),
                  stringsAsFactors = FALSE)
df1

df2
comparedf(df1, df2) %>% summary()
