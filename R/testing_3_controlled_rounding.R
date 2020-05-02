system.time({
  for(i in 1:1000)
  {x <- rnorm(10, mean = 10, sd = 1)
  y <- controlled_rounding_off(a_vector = x)
  }
})

system.time({
  for(i in 1:1000)
  {x <- rnorm(10, mean = 10, sd = 1)
  y <- controlled_rounding_off_2(a_vector = x)
  }
})

system.time({
for(i in 1:1000)
{x <- rnorm(10, mean = 10, sd = 1)
 y <- controlled_rounding_off_3(a_vector = x)
}
})
