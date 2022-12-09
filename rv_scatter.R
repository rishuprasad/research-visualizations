library(rlang)
library(tidyverse)
library(performance)

data(diamonds)
head(diamonds)
diam <- diamonds %>% slice(sample(500, size = 200))


rv_scatterplot <- function(data = NULL, 
                           x, 
                           y,
                           model = NULL,
                           modelMethod = NULL,
                           bestTransformation = F,
                           modelWeights = NULL,
                           test = NULL,
                           alternative = NULL,
                           g = NULL,
                           method = NULL,
                           labelx = .05,
                           labely = .95,
                           just = "left",
                           colour = NULL,
                           fill = NULL,
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
  
  
  # 2-Sample tests # ----------------------------------
  if (!is.null(test)) {
    
    # Get test expression
    test.expr <- enexpr(test)
    
    # Set test expression
    test.string <- as.character(substitute(test))
 
    if (is_true(test.string == "t.test" | test.string == "wilcox.test" | test.string == "cor.test")) {
      test.result.expr <- expr((!!test.expr)(x, y,
                                             alternative = alternative, 
                                             method = method, ...))
    } else if (is_true(test.string == "chisq.test")   ) { 
      test.result.expr <- expr((!!test.expr)(x, y, ...))
    } else {
      print("Test not found")
    }
    
    # Evaluate test expression 
    result <- eval_tidy(test.result.expr, data = data)
    label.p.value <- signif(result$p.value, 3)
    print(result)
  }
  
  # End Two Sample Tests -------------------------------------
  
  # Modeling # ------------------------------------------------
  
  # First checks if user specified model to be nonempty, skips modeling if it is empty
  if (!is.null(model)) {
    
    # If model input is TRUE, then it'll try various models with the data, if the input 
    # is a model object then it will evaluate the model object
    if (is_true(model))  {
      
      # If model method is defined, use that method, if not, it will choose simple linear
      # model, and in both cases, 
      # if bestTransformation is T, it will do various transformations, fit different models
      # and pick the "best model" based on R^2 
      if (is.null(modelMethod)) {
        
        # Create model expression 
        model.result.expr <- expr(glm())
        model.result.expr$formula = {{y}} ~ {{x}}
        model.result.expr$family = gaussian(link = "identity")
        print(model)
        
        # Evaluate model expression
        result <- eval_tidy(model.result.expr, data = data)
        print(summary(result))
        
        # model.summary <- summary(model)
        # label.p.value <- signif(as.numeric(model.summary$coefficients[2, 4]), 3)
        
        # print(summary(model)$call)
        
      } else if (!is.null(modelMethod)) {
        
      }
      
    } else if (!is.null(model) & class(model)[1] == "glm" | class(model)[2] == "lm") {
      
      # Wald Test for significance of explanatory variable #
      model.summary <- summary(model)
      label.p.value <- signif(as.numeric(model.summary$coefficients[2, 4]), 3)
      
      print(summary(model))
      
      
      # Performance checking via the performance package
      print(check_heteroscedasticity(model))
      print(check_normality(model))
      print(check_outliers(model))
      
    }
    
  } 
  
  

#  End Modeling ----------------------------------------------- 
  
  # Labeling Work # ------------------
  
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

  # End Labeling Work 
  
# Plotting Settings ---------------------------------

   
   gg=ggplot(data,aes(x={{x}},y={{y}})) +
    geom_point(colour = colour) +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) 
 

  # If there was a test
  if (!is.null(test)) {
    gg = gg + annotation_custom(label)
  }
  
  # If there was a model
  if (!is.null(model)) {
    gg = gg + annotation_custom(label)
  }
  
  # Plot 
  gg
# --------------------------------------------------------  

}


rv_scatterplot(diam, x, depth, model = T,
               colour = "blue") +
  theme_classic()


































































































































































































