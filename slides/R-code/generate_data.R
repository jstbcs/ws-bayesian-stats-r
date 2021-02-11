set.seed(124)

I <- 2
J <- 2
K <- 30
N <- I*J*K

story <- rep(1:I, each = J*K)
pol <- rep(rep(1:J, each = K), I)

storyLab <- c('Neutral', 'Refugee plight')[story]
polLab <- c('Conservatives', 'Liberals')[pol]

t.cell <- matrix(nrow = 2, byrow = T, c(-0.4, 0, -0.6, .6) + 3)
Y <- rnorm(N, t.cell[cbind(story, pol)], 1)

immigration_study <- data.frame(
  participant = 1:N
  , story = storyLab
  , political_affiliation = polLab
  , attitude = Y
)
