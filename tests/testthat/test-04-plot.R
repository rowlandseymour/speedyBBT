test_that("plot_abilities creates expected plot", {
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
  quality_plot <- plot_qualities(
    player_names = items,
    model_output = wimbledonModel
  )
  vdiffr::expect_doppelganger("Wimbledon qualities", quality_plot)
})

test_that("plot_abilities creates expected flipped plot", {
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
  quality_plot <- plot_qualities(
    player_names = items,
    model_output = wimbledonModel,
    flip = TRUE
  )
  vdiffr::expect_doppelganger("Wimbledon qualities flipped", quality_plot)
})

test_that("plot_abilities creates expected plot with custom axes", {
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
  quality_plot <- plot_qualities(
    player_names = items,
    model_output = wimbledonModel,
    quality_label = c("Low", "Medium", "High")
  )
  vdiffr::expect_doppelganger("Wimbledon qualities custom axes", quality_plot)
})
