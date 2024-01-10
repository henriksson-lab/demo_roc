input <- list(
  
  distribution="Normal",
  
  normal_mu=0,
  normal_sigma=1,

  binom_n=10,
  binom_p=0.5
  
)

reactive <- function(f) function() f



