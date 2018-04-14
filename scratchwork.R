df <- data.frame(
  id = 1:10,
  time = as.Date('2009-01-01') + 0:9,
  Q3.2.1. = rnorm(10, 0, 1),
  Q3.2.2. = rnorm(10, 0, 1),
  Q3.2.3. = rnorm(10, 0, 1),
  Q3.3.1. = rnorm(10, 0, 1),
  Q3.3.2. = rnorm(10, 0, 1),
  Q3.3.3. = rnorm(10, 0, 1)
)
df %<>%
  gather(key, value, -id, -time)
extract(df, key, c("question", "loop_number"), "(Q.\\..)\\.(.)") 

x <- filter(hou, raceYear == 2012 & State == 'California')
z <- full_join(x, y, by = c('State', 'AreaNumber'))

map(dat, ~.x$error)

c(93:113)[9]

st <- tbl(db, 'states') %>% collect()

ln <- str_split(dat$name, ' ', simplify = T)
ln %<>% as_tibble()
ln$id <- 1:nrow(ln)
ln %<>% gather(key, value, -id)
ln %<>% filter(value != '')
ln %<>% group_by(id) %>% mutate(m = n())
ln %<>% group_by(id) %>% mutate(r = rank(key))
ln %<>% filter(r == m)
ln$n <- map_int(ln$value, nchar)
z <- filter(ln, n <= 3 & n > 0)
w <- count(z, value)

library(plotly)
#### test data
lead <- rep("Fred Smith", 30)
lead <- append(lead, rep("Terry Jones", 30))
lead <- append(lead, rep("Henry Sarduci", 30))
proj_date <- seq(as.Date('2017-11-01'), as.Date('2017-11-30'), by = 'day')
proj_date <- append(proj_date, rep(proj_date, 2))
set.seed(1237)
actHrs <- runif(90, 1, 100)
cummActHrs <- cumsum(actHrs)
forHrs <- runif(90, 1, 100)
cummForHrs <- cumsum(forHrs)
df <- data.frame(Lead = lead, date_seq = proj_date,
                 cActHrs = cummActHrs,
                 cForHrs = cummForHrs)

plot_ly(data = df, x = ~date_seq, y = ~cActHrs, split = ~Lead)

makePlot <- function(df, x_var = date_seq, y_var, split_var) {
  plot <- plot_ly(
    data = df,
    x = ~x_var,
    y = ~y_var,
    split = ~split_var
  )
  
  return(plot)
}
makePlot(df, df$date_seq, df$cActHrs, df$Lead)

z <- dat[mis[mat >= 5], ]
w <- filter(cands, osid %in% z$osid) %>% distinct(name, osid)
ww <- w[match(z$osid, w$osid), ]
