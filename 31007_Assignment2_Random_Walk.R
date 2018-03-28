library(dplyr)
library(ggplot2)

theme_set(
  theme_bw()
)

#instructor
nFlips<-100000

set.seed(12345)
Flips<-sample(0:1,nFlips,repl=T)
# or rbinom(nFlips,1,.5) or (runif(nFlips)<.5)
Trajectory<-cumsum(Flips) 
freq<-Trajectory/(1:nFlips)
plot(1:length(freq),freq, ylim=c(.4,1),type="l",ylab="Frequency",xlab="Sample Length")
lines(c(0,nFlips),c(.5,.5))

nFlips<-1000000
set.seed(12345)
Flips<-(sample(0:1,nFlips,repl=T)-.5)*2

oneTrajectory<-cumsum(Flips)
plot(oneTrajectory, ylim=c(-1000,1000),type="l")
lines(c(0,nFlips),c(0,0))

#my simulation- utilizing dplyr and ggplot2
flip_n <- 6000000

set.seed(1017)
game_simulation <- data.frame(
  flip.simulation = rep(c("one", "two", "three", "four", "five", "six")),
  game.number = rep(1:nrow(game_simulation)),
  outcome = rbinom(n = flip_n, size = 1, prob = .5)
)

game_simulation <- game_simulation %>%
  group_by(flip.simulation) %>%
  mutate(pay.out = case_when(
    outcome == 1 ~ 1,
    outcome == 0 ~ -1),
    trajectory = cumsum(pay.out)) %>%
  ungroup() %>%
  mutate(flip.simulation = factor(flip.simulation, 
                             levels = c("one", "two", "three", 
                                        "four", "five", "six")))

game_simulation %>%
  count(flip.simulation, outcome)

game_simulation %>%
  filter(game.number >= 5999995) %>%
  group_by(flip.simulation) %>%
  summarise(final.trajectory = trajectory,
            proximity.to.zero = ifelse(trajectory > 0, "Over Zero", "Under Zero")) %>%
  arrange(desc(final.trajectory))

game_simulation %>%
  ggplot(aes(game.number, trajectory, colour = flip.simulation)) +
  geom_line() +
  facet_wrap(facets = "flip.simulation") +
  geom_hline(yintercept = 0, colour = "red") +
  theme(legend.position = "none") +
  labs(title = "Six simulations of 1 million coin flips alongside expected pay out (1 head = 1$ & 1 tail = -1$)",
       subtitle = "All runs have differing trajectories and none end up at exactly zero (two below, four over) -- All are within $2,000 of zero though")
