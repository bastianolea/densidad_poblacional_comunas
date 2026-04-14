puntos <- function(n, radio) {
  r  <- runif(n)
  th <- runif(n, 0, 2 * pi)
  # radio = 1
  
  data.frame(
    x = radio * sqrt(r) * cos(th),
    y = radio * sqrt(r) * sin(th)
  ) |> list()
}

rescalar <- function(x, min_out = 1, max_out = 2) {
  min_out + (x - min(x)) / (max(x) - min(x)) * (max_out - min_out)
}