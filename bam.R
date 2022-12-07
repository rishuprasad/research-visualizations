

# 0 (1 population) 

if (test = "t.test") { 
  res1 <- shapiro.test({{x}})
  rest2 <- shapiro.test({{y}})
  
  if (length{{x}}) <= 20) {
    message("Sample size may be small for a valid t.test")
  }

}

if (test == "t.test") {
  #* This works when its just by itself, and no other if statements #*
  if (is.null({{y}})) { # one sample
    # Messages about assumptions
    shapiro = shapiro.test({{x}})
    if (shapiro$p.value <= .05) {
      message("Shapiro test for normality rejected null hypothesis")
    }
    if (length({{x}}) <= 20) {
      message("Sample size may be small for a valid t.test")
    }
    # adding p.value labels
    result = t.test({{x}}, alternative = {{alternative}}, mu = {{mu}})  
    if (result$p.value <= .001) { 
      lab <- grid::textGrob(label = paste0(
        "p < .001"),
        x = unit(0.05, "npc"), 
        y = unit(0.99, "npc"), just = "left",
        gp = grid::gpar(size = 14, fontface = "bold"))
      gg = gg + annotation_custom(lab)
      gg
    } else {
      lab <- grid::textGrob(label = paste0(
        "p = ", signif(result$p.value, 3)),
        x = unit(0.05, "npc"), 
        y = unit(0.99, "npc"), just = "left",
        gp = grid::gpar(size = 14, fontface = "bold"))
      gg = gg + annotation_custom(lab)
      gg
    } # Works, lemme add the two sample stuff
  } else {
    
    if (!paired) { # not paired
      # Messages about assumptions
      shapirox = shapiro.test({{x}})
      shapiroy = shapiro.test({{y}})
      if (shapirox$p.value <= .05 | shapiroy$p.value <= .05) {
        message("Shapiro test for normality rejected for at at least one of the groups")
      }
      if (length({{x}}) <= 20) {
        message("Sample size may be small for a valid t.test")
      }
      # adding p.value labels 
      result = t.test({{x}}, {{y}}, alternative = {{alternative}}, mu = {{mu}}, paired = F)
      if (result$p.value <= .001) { 
        lab <- grid::textGrob(label = paste0(
          "p < .001"),
          x = unit(0.05, "npc"), 
          y = unit(0.985, "npc"), just = "left",
          gp = grid::gpar(size = 14, fontface = "bold"))
        gg = gg + annotation_custom(lab)
        gg
      } else {
        lab <- grid::textGrob(label = paste0(
          "p = ", signif(result$p.value, 3)),
          x = unit(0.05, "npc"), 
          y = unit(0.985, "npc"), just = "left",
          gp = grid::gpar(size = 14, fontface = "bold"))
        gg = gg + annotation_custom(lab)
        gg
      }
      
    } else { # paired
      # Messages about assumptions
      shapirox = shapiro.test({{x}})
      shapiroy = shapiro.test({{y}})
      if (shapirox$p.value <= .05 | shapiroy$p.value <= .05) {
        message("Shapiro test for normality rejected for at at least one of the groups")
      }
      if (length({{x}}) <= 20) {
        message("Sample size may be small for a valid t.test")
      }
      # adding p.value labels 
      result1 = t.test({{x}}, {{y}}, alternative = {{alternative}}, mu = {{mu}}, paired = T)
      if (result1$p.value <= .001) { 
        lab <- grid::textGrob(label = paste0(
          "p < .001"),
          x = unit(0.05, "npc"), 
          y = unit(0.985, "npc"), just = "left",
          gp = grid::gpar(size = 14, fontface = "bold"))
        gg = gg + annotation_custom(lab)
        gg
      } else {
        lab <- grid::textGrob(label = paste0(
          "p = ", signif(result1$p.value, 3)),
          x = unit(0.05, "npc"), 
          y = unit(0.985, "npc"), just = "left",
          gp = grid::gpar(size = 14, fontface = "bold"))
        gg = gg + annotation_custom(lab)
        gg
      }
    }
    # Here stop
    
  }
} # end t.test
