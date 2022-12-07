library(rlang)
library(tidyverse)
data(diamonds)
head(diamonds)
diam <- diamonds %>% slice(sample(500, size = 200))


rv_scatterplot <- function(data = NULL, 
                           x, 
                           y,
                           model = NULL,
                           test = NULL,
                           alternative = NULL,
                           g = NULL,
                           method = NULL,
                           labelx = .05,
                           labely = .95,
                           just = "left",
                           color_by=NULL,
                           blocks = NULL,
                           wrap_by=NULL,
                           title=NULL,
                           xlab=NULL,
                           ylab=NULL,
                           sub=NULL,
                           classic=T,
                           method1 = NULL,
                           ...) {
  

  
  # data ----------------------------------
  
    # Goals 
    # 1. switch between tests (works with more parameters than necessary)
    # 2. output the tests
    # 3. send messages
  
  # Preparing data frames
  if (is.null(data)) {
    data <- data.frame(x, y)
  }
  data <- data %>% filter(!is.na({{x}}), !is.na({{y}}))
  

  # Statistical Analysis -----------------------------------
  
  # 2-Sample tests #
  if (!is.null(test)) {
    
    # Get test expression
    test.expr <- enexpr(test)
    
    # Set test expression
    test.string <- as.character(substitute(test))
    
    if (is_true(test.string == "t.test" | test.string == "wilcox.test" | test.string == "cor.test")) {
      test.result.expr <- expr((!!test.expr)(x, y,
                                             alternative = alternative, 
                                             method = method, ...))
    } else if (is_true(test.string == "chisq.test")) {
      test.result.expr <- expr((!!test.expr)(x, y, ...))
    } else {
      print("Test not found")
    }
    
    # Evaluate test expression 
    result <- eval_tidy(test.result.expr, data = data)
    print(result)
    
    # Labeling Work # 
    label.p.value <- signif(result$p.value, 3)
    # Formatting p-value label
    if (label.p.value < .001) {
      label.p.value = "P < 0.001"
    } else if (label.p.value > .001) {
      label.p.value = paste("P =", label.p.value)
    } 
    
    # Creating label to be used for plotting
    label <- grid::textGrob(label = label.p.value,
                            x = unit(labelx, "npc"),
                            y = unit(labely, "npc"), just = {{just}},
                            gp = grid::gpar(size = 14, fontface = "bold"))
   
  }
  
  # Modeling # 
  if (!is.null(model)) {

    # Wald Test for significance of explanatory variable #
    model.summary <- summary(model)
    label.p.value <- as.numeric(model.summary$coefficients[2, 4])
  
    # Labeling Work # 
    
    # Formatting p-value label
    if (label.p.value < .001) {
      label.p.value = "P < 0.001"
    } else if (label.p.value > .001) {
      label.p.value = paste("P =", label.p.value)
    } 
    
    # Creating label to be used for plotting
    label <- grid::textGrob(label = label.p.value,
                            x = unit(labelx, "npc"),
                            y = unit(labely, "npc"), just = {{just}},
                            gp = grid::gpar(size = 14, fontface = "bold"))
    
    
  }
  
# -----------------------------------------------
  
  
# Plotting Settings ---------------------------------
 
  gg=ggplot(data,aes(x={{x}},y={{y}},color={{color_by}})) +
    geom_point() +
    facet_wrap(vars({{wrap_by}})) +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) +
    labs(title=title,subtitle=sub)+xlab(xlab)+ylab(ylab)+
    theme(text = element_text(size=10, family = "Times New Roman"))
  if(classic) {
    gg = gg+theme_classic()
  }
  
  
  # If there was a test
  if (!is.null(test)) {
    gg = gg + annotation_custom(label)
  }
  
  # If there was a model
  if (!is.null(model)) {
    gg = gg +annotation_custom(label)
  }
  
  
  # Plot 
  
  gg
# --------------------------------------------------------  

}


rv_scatterplot(diam, x, depth, 
               model = glm(x ~ carat, family = gaussian(link = "identity"), data = diam)) 




































































































































































































