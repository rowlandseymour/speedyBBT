test_that("plot_abilities creates expected plot"{
set.seed("234")  
items <- wimbledon$players$name
wimbledonModel <- BBTm(
  outcome = wimbledon$matches$outcome,
  player2 = wimbledon$matches$loser,
  player1 = wimbledon$matches$winner,
  advantage = wimbledon$matches$secondWeek,
  formula = ~ rank + points,
  data = wimbledon$players,
  n.iter = 200
)
quality_plot <- plot_qualities(item_names = items, model_output = wimbledonModel)
vdiffr::expect_doppelganger("Wimbledon qualities", quality_plot)
})