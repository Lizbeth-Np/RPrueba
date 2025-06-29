#install.packages("tinytex",dependencies = TRUE)
#install.packages("rmarkdown",dependencies = TRUE)
#install.packages("shinycssloaders",dependencies = TRUE)
library(shiny)
library(kableExtra)
library(data.table)
library(tidyverse)
library(ggplot2)
library(DT)
library(knitr)
library(rmarkdown)
library(tinytex)
library(shinycssloaders)

# Función que genera números aleatorios bajo el método congruencial multiplicativo
random_cong <- function(a, m, x0, n) {
  res <- numeric(n+1)
  res[1] <- x0
  for (k in 2:length(res)) {
    res[k] <- (a * res[k-1]) %% m
  }
  return(res[-1] / m)
}

# Función que genera números aleatorios bajo el método congruencial mixto
random_mixt <- function(a, c, m, x0, n) {
  res <- numeric(n+1)
  res[1] <- x0
  for (k in 2:length(res)) {
    res[k] <- (a * res[k-1] + c) %% m
  }
  return(round((res[-1]) / m, 6))
}

conv_matrix <- function(vector, cols = 10) {
  res <- rep(NA_real_, ceiling(length(vector) / cols) * cols)
  res[1:length(vector)] <- vector
  res <- as.data.frame(matrix(res, nrow = ceiling(length(vector) / cols), ncol = cols, byrow = TRUE))
  colnames(res) <- paste0("Cols", 1:cols)
  return(res)
}

shinyServer(function(input, output, session) {
  
  #### Generación números aleatorios #######################################################
  
  coords <- eventReactive(input$calcular, {
    x_vals <- seq(input$lim_inf, input$lim_sup, length.out = 100)
    y_vals <- sapply(x_vals, function(x) { eval(parse(text = input$funcion)) })
    coords <- data.frame(x = x_vals, y = y_vals)
  })
  
  output$tabla <- function() {
    res <- conv_matrix(
      random_cong(
        a = input$constante,
        m = input$divisor,
        x0 = input$semilla,
        n = input$num
      )
    )
    
    kbl(res, booktabs = TRUE, escape = FALSE, align = "c", digits = 6) %>%
      kable_styling(
        full_width = FALSE,
        bootstrap_options = c("bordered", "striped", "hover", "condensed"),
        font_size = 13
      ) %>%
      row_spec(0, background = "#1e81b0", color = "white", bold = TRUE) %>%
      scroll_box(width = "100%", height = "250px")
  }
  
  output$tabla1 <- function() {
    res <- conv_matrix(
      random_mixt(
        a = input$constante,
        c = input$c,
        m = input$divisor,
        x0 = input$semilla,
        n = input$num
      )
    )
    
    kbl(res, booktabs = TRUE, escape = FALSE, align = "c", digits = 6) %>%
      kable_styling(
        full_width = FALSE,
        bootstrap_options = c("striped", "hover", "condensed", "bordered"),
        font_size = 13
      ) %>%
      row_spec(0, background = "#e28743", color = "white", bold = TRUE) %>% 
      scroll_box(width = "100%", height = "230px")
  }
  
  # Histograma método multiplicativo
  output$hist_multiplicativo <- renderPlot({
    datos <- random_cong(a = input$constante,
                         m = input$divisor,
                         x0 = input$semilla,
                         n = input$num)
    
    hist(datos, 
         breaks = input$barras, 
         col = "#abdbe3", 
         border = "#fff",
         main = "",
         xlab = "Valores generados", 
         ylab = "Densidad",
         freq = FALSE, 
         col.lab = "#34495e",   
         las = 1)
  })
  
  # Histograma método mixto
  output$hist_mixto <- renderPlot({
    datos <- random_mixt(a = input$constante,
                         c = input$c,
                         m = input$divisor,
                         x0 = input$semilla,
                         n = input$num)
    
    hist(datos, 
         breaks = input$barras, 
         col = "#eab676",  
         border = "#ffffff",
         main = "",
         xlab = "Valores generados", 
         ylab = "Densidad",
         freq = FALSE,
         col.lab = "#34495e",
         las = 1)
  })
  
  ######################################### INTEGRALES ############################################################
  
  output$graf_fun01 <- renderPlot({
    a <- input$lim_inf
    b <- input$lim_sup
    limite_infinito <- b > 500
    b_plot <- if (limite_infinito) 100 else b
    
    f_input <- input$funcion
    f <- function(x_val) {
      tryCatch(
        eval(parse(text = f_input), envir = list(x = x_val)),
        error = function(e) NA
      )
    }
    
    x_vals <- seq(a, b_plot, length.out = 100)
    y_vals <- sapply(x_vals, f)
    datos <- data.frame(x = x_vals, y = y_vals)
    
    if (!limite_infinito) {
      delta_x <- (b - a) / (length(x_vals) - 1)
      area <- (delta_x / 2) * (y_vals[1] + 2 * sum(y_vals[2:(length(y_vals) - 1)]) + y_vals[length(y_vals)])
    } else {
      area <- NA
    }
    
    ggplot(datos, aes(x = x, y = y)) +
      geom_line(color = "#2C3E50", linewidth = 1.2) +
      geom_area(fill = "#AEDFF7", alpha = 0.5) +
      geom_vline(xintercept = a, linetype = "dashed", color = "#e74c3c", linewidth = 0.8) +
      geom_vline(xintercept = b_plot, linetype = "dashed", color = "#e74c3c", linewidth = 0.8) +
      labs(
        title = paste("Área bajo la curva de f(x) =", f_input),
        subtitle = if (limite_infinito) "[0, ∞) truncado a 100 para visualización" else paste("Intervalo:", a, "a", b),
        x = "x",
        y = "f(x)",
        caption = if (!is.na(area)) paste("Área aproximada (método del trapecio):", round(area, 4)) else "Área no calculada"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", color = "#2C3E50", size = 16),
        plot.subtitle = element_text(size = 13, color = "#34495e"),
        axis.title = element_text(face = "bold", size = 13),
        plot.caption = element_text(size = 11, color = "gray40", hjust = 1),
        panel.grid.minor = element_blank()
      )
  })
  
  output$graf_aprox01 <- renderPlot({
    f_input <- input$funcion
    a <- input$lim_inf
    b <- input$lim_sup
    limite_infinito <- b > 500
    es_divergente <- FALSE
    razon_divergencia <- ""
    
    f <- function(x_val) {
      tryCatch(
        eval(parse(text = f_input), envir = list(x = x_val)),
        error = function(e) NA
      )
    }
    
    # Validación de divergencia
    tryCatch({
      if (!limite_infinito) {
        puntos <- seq(a, b, length.out = 50)
        valores <- sapply(puntos, f)
        
        if (any(is.infinite(valores))) {
          es_divergente <- TRUE
          razon_divergencia <- "La función tiene asíntotas en el intervalo."
        } else if (any(is.na(valores))) {
          es_divergente <- TRUE
          razon_divergencia <- "La función no está definida en todo el intervalo."
        }
      } else {
        val_a <- f(a)
        if (is.infinite(val_a)) {
          es_divergente <- TRUE
          razon_divergencia <- paste("La función tiende a infinito en x =", a)
        }
        grandes_x <- seq(100, 1000, length.out = 10)
        vals_grandes <- sapply(grandes_x, f)
        if (any(is.infinite(vals_grandes))) {
          es_divergente <- TRUE
          razon_divergencia <- "La función tiende a infinito cuando x→∞"
        } else if (mean(abs(vals_grandes), na.rm = TRUE) > 0.1) {
          es_divergente <- TRUE
          razon_divergencia <- "La función no tiende a cero cuando x→∞"
        }
      }
    }, error = function(e) {
      es_divergente <<- TRUE
      razon_divergencia <<- paste("Error al evaluar la función:", e$message)
    })
    
    if (es_divergente) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "INTEGRAL DIVERGENTE", size = 8, color = "red") +
          annotate("text", x = 0.5, y = 0.4, label = razon_divergencia, size = 5, color = "darkred") +
          theme_void() +
          labs(caption = "La integral no converge a un valor finito")
      )
    }
    
    secuencia <- seq(100, 10000, by = 250)
    usar_mixto <- input$metodo == "Congruencial Mixto"
    generar <- function(n) {
      if (usar_mixto) {
        random_mixt(a = 123, c = 7, m = 2^31 - 1, x0 = as.numeric(Sys.time()), n = n)
      } else {
        random_cong(a = 7^5, m = 2^31 - 1, x0 = as.numeric(Sys.time()), n = n)
      }
    }
    
    aprox <- numeric(length(secuencia))
    teorico <- NA
    
    if (!limite_infinito) {
      for (i in seq_along(secuencia)) {
        n <- secuencia[i]
        u <- generar(n)
        x <- a + (b - a) * u
        fx <- sapply(x, f)
        aprox[i] <- mean(fx, na.rm = TRUE) * (b - a)
        if (abs(aprox[i]) > 1e6) {
          es_divergente <- TRUE
          razon_divergencia <- "La aproximación crece sin límite"
          break
        }
      }
      if (!es_divergente) {
        teorico <- tryCatch({
          integrate(f, lower = a, upper = b)$value
        }, error = function(e) NA)
      }
    } else {
      for (i in seq_along(secuencia)) {
        n <- secuencia[i]
        y <- generar(n)
        y <- y[y > 1e-3]
        x <- (1 - y) / y
        fx <- sapply(x, f)
        h_y <- fx / (y^2)
        h_y <- h_y[is.finite(h_y) & abs(h_y) < 1e6]
        aprox[i] <- mean(h_y, trim = 0.05, na.rm = TRUE)
        if (abs(aprox[i]) > 1e6) {
          es_divergente <- TRUE
          razon_divergencia <- "La aproximación crece sin límite"
          break
        }
      }
      if (!es_divergente) {
        teorico <- tryCatch({
          f0 <- f(0)
          if (is.finite(f0) && !is.nan(f0)) {
            integrate(f, lower = 0, upper = Inf)$value
          } else {
            integrate(f, lower = 1, upper = Inf)$value
          }
        }, error = function(e) NA)
      }
    }
    
    if (es_divergente) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "INTEGRAL DIVERGENTE", size = 8, color = "red") +
          annotate("text", x = 0.5, y = 0.4, label = razon_divergencia, size = 5, color = "darkred") +
          theme_void() +
          labs(caption = "La integral no converge a un valor finito")
      )
    }
    
    plot_data <- data.table(Aproximación = aprox,
                            Teórico = rep(teorico, length(secuencia))) %>%
      tidyr::pivot_longer(cols = c("Aproximación", "Teórico"),
                          names_to = "Etiqueta", values_to = "Valor") %>%
      mutate(Aleatorios = rep(secuencia, 2))
    
    ggplot(plot_data, aes(x = Aleatorios, y = Valor, color = Etiqueta)) +
      geom_line(size = 1.2) +
      geom_point(size = 1.5, alpha = 0.6, show.legend = FALSE) +
      scale_color_manual(values = c("Aproximación" = "#e74c3c", "Teórico" = "#3498db")) +
      labs(
        title = paste("Estimación del área de:", f_input),
        subtitle = if (limite_infinito) "Integral impropia en el intervalo [0, ∞)" else paste("Intervalo definido:", a, "a", b),
        x = "Muestras aleatorias generadas",
        y = "Área estimada",
        caption = if (!is.na(teorico)) paste("Valor teórico esperado:", round(teorico, 4)) else "Estimación por método Monte Carlo"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", size = 16, color = "#2c3e50"),
        plot.subtitle = element_text(size = 13, color = "#7f8c8d"),
        axis.title.x = element_text(face = "bold", color = "#2c3e50"),
        axis.title.y = element_text(face = "bold", color = "#2c3e50"),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 11)
      )
  })
  
  ####### DISTRIBUCIONES DISCRETAS #################################################################
  
  # Reactive values to store simulation data
  sim_data <- reactiveValues(resultados = NULL, dist_type = NULL, params = NULL, plot = NULL, summary = NULL)
  
  # UI Dinámico
  output$custom_inputs <- renderUI({
    req(input$size_custom)
    size <- input$size_custom
    inputs <- lapply(1:size, function(i) {
      fluidRow(
        column(6, numericInput(paste0("value_", i), paste("Valor", i, ":"), value = i, step = 1)),
        column(6, numericInput(paste0("prob_", i), paste("Probabilidad", i, ":"), value = 1/size, min = 0, max = 1, step = 0.01))
      )
    })
    do.call(tagList, inputs)
  })
  
  # Validar Probabilidades
  output$prob_validation <- renderText({
    req(input$size_custom)
    size <- input$size_custom
    probs <- numeric(size)
    for (i in 1:size) {
      prob <- input[[paste0("prob_", i)]]
      if (is.null(prob)) return("Por favor, ingrese todas las probabilidades.")
      probs[i] <- as.numeric(prob)
    }
    if (any(is.na(probs))) return("Todas las probabilidades deben ser números válidos.")
    prob_sum <- sum(probs)
    if (abs(prob_sum - 1) > 0.01) {
      return(sprintf("Las probabilidades deben sumar 1 (suma actual: %.3f).", prob_sum))
    }
    ""
  })
  
  # Distribución (Otro)
  observeEvent(input$simular_custom, {
    req(input$size_custom, input$simulaciones_custom)
    size <- input$size_custom
    n <- input$simulaciones_custom
    
    # Lista de probabilidades
    values <- sapply(1:size, function(i) input[[paste0("value_", i)]])
    probs <- sapply(1:size, function(i) as.numeric(input[[paste0("prob_", i)]]))
    
    # Validacion
    if (any(is.null(values)) || any(is.null(probs)) || any(is.na(probs))) {
      showNotification("Complete todos los valores y probabilidades con números válidos.", type = "error")
      return()
    }
    if (abs(sum(probs) - 1) > 0.01) {
      showNotification("Las probabilidades deben sumar aproximadamente 1.", type = "error")
      return()
    }
    
    # Almacenamiento de datos simulados
    sim_data$resultados <- sample(values, size = n, replace = TRUE, prob = probs)
    sim_data$dist_type <- "Otro"
    sim_data$params <- list(values = values, probs = probs, simulaciones = n)
    
    output$resultados_simulacion <- function() {
      res <- conv_matrix(sim_data$resultados, cols = 10)
      kbl(res, booktabs = TRUE, escape = FALSE, align = "c", digits = 0) %>%
        kable_styling(
          full_width = FALSE,
          bootstrap_options = c("bordered", "striped", "hover", "condensed"),
          font_size = 13
        ) %>%
        row_spec(0, background = "#d0d394", color = "#211d1d", bold = TRUE) %>%
        scroll_box(width = "100%", height = "250px")
    }
    
    output$histograma_simulacion <- renderPlot({
      datos <- data.frame(Valores = sim_data$resultados)
      values_ordered <- sort(unique(as.numeric(sim_data$params$values)))
      probs <- sim_data$params$probs[match(values_ordered, sim_data$params$values)]
      
      # Crear datos para la curva (PMF en los valores)
      pmf_data <- data.frame(
        x = values_ordered,
        Prob = probs
      )
      
      sim_data$plot <- ggplot() +
        geom_histogram(data = datos, aes(x = Valores, y = after_stat(density)), 
                       fill = "#06a480", color = "#2a2727", 
                       breaks = seq(min(values_ordered) - 0.5, max(values_ordered) + 0.5, by = 1)) +
        geom_line(data = pmf_data, aes(x = x, y = Prob), color = "#ceab1d", linewidth = 1) +
        geom_point(data = pmf_data, aes(x = x, y = Prob), color = "#ceab1d", size = 3) +
        scale_x_continuous(breaks = values_ordered, labels = as.character(values_ordered)) +
        labs(
          title = "Otra Distribución",
          x = "Valores Simulados",
          y = "Densidad"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", color = "#2c3e50"),
          axis.title = element_text(face = "bold")
        )
      
      sim_data$plot
    })
    
    output$resumen_estadistico <- renderPrint({
      resumen <- capture.output({
        cat("Resumen de Simulaciones:\n")
        cat("Media:", round(mean(sim_data$resultados), 4), "\n")
        cat("Desviación Estándar:", round(sd(sim_data$resultados), 4), "\n")
        cat("Mínimo:", min(sim_data$resultados), "\n")
        cat("Máximo:", max(sim_data$resultados), "\n")
        cat("Cuartiles:\n")
        print(quantile(sim_data$resultados))
      })
      sim_data$summary <- resumen
      cat(resumen, sep = "\n")
    })
  })
  
  # Binomial 
  observeEvent(input$simular_binomial, {
    req(input$n_binomial, input$p_binomial, input$simulaciones_binomial)
    
    n <- input$simulaciones_binomial
    size <- input$n_binomial
    prob <- input$p_binomial
    
    sim_data$resultados <- rbinom(n, size, prob)
    sim_data$dist_type <- "Binomial"
    sim_data$params <- list(n = size, p = prob, simulaciones = n)
    
    output$resultados_simulacion <- function() {
      res <- conv_matrix(sim_data$resultados, cols = 10)
      kbl(res, booktabs = TRUE, escape = FALSE, align = "c", digits = 0) %>%
        kable_styling(
          full_width = FALSE,
          bootstrap_options = c("bordered", "striped", "hover", "condensed"),
          font_size = 13
        ) %>%
        row_spec(0, background = "#d0d394", color = "#211d1d", bold = TRUE) %>%
        scroll_box(width = "100%", height = "250px")
    }
    
    output$histograma_simulacion <- renderPlot({
      datos <- data.frame(Valores = sim_data$resultados)
      x_range <- 0:max(max(sim_data$resultados), size)
      
      # Crear datos para la curva (PMF en los valores)
      pmf_data <- data.frame(
        x = x_range,
        Prob = dbinom(x_range, size = size, prob = prob)
      )
      
      sim_data$plot <- ggplot() +
        geom_histogram(data = datos, aes(x = Valores, y = after_stat(density)), 
                       fill = "#06a480", color = "#2a2727", breaks = seq(-0.5, max(x_range) + 0.5, by = 1)) +
        geom_line(data = pmf_data, aes(x = x, y = Prob), color = "#ceab1d", linewidth = 1) +
        geom_point(data = pmf_data, aes(x = x, y = Prob), color = "#ceab1d", size = 3) +
        labs(
          title = paste("Binomial (n =", size, ", p =", prob, ")"),
          x = "Valores Simulados",
          y = "Densidad"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", color = "#2c3e50"),
          axis.title = element_text(face = "bold")
        )
      
      sim_data$plot
    })
    
    output$resumen_estadistico <- renderPrint({
      resumen <- capture.output({
        cat("Resumen de Simulaciones:\n")
        cat("Media:", round(mean(sim_data$resultados), 4), "\n")
        cat("Desviación Estándar:", round(sd(sim_data$resultados), 4), "\n")
        cat("Mínimo:", min(sim_data$resultados), "\n")
        cat("Máximo:", max(sim_data$resultados), "\n")
        cat("Cuartiles:\n")
        print(quantile(sim_data$resultados))
      })
      sim_data$summary <- resumen
      cat(resumen, sep = "\n")
    })
  })
  
  # Poisson 
  observeEvent(input$simular_poisson, {
    req(input$lambda_poisson, input$simulaciones_poisson)
    
    n <- input$simulaciones_poisson
    lambda <- input$lambda_poisson
    
    sim_data$resultados <- rpois(n, lambda)
    sim_data$dist_type <- "Poisson"
    sim_data$params <- list(lambda = lambda, simulaciones = n)
    
    output$resultados_simulacion <- function() {
      res <- conv_matrix(sim_data$resultados, cols = 10)
      kbl(res, booktabs = TRUE, escape = FALSE, align = "c", digits = 0) %>%
        kable_styling(
          full_width = FALSE,
          bootstrap_options = c("bordered", "striped", "hover", "condensed"),
          font_size = 13
        ) %>%
        row_spec(0, background = "#d0d394", color = "#211d1d", bold = TRUE) %>%
        scroll_box(width = "100%", height = "250px")
    }
    
    output$histograma_simulacion <- renderPlot({
      datos <- data.frame(Valores = sim_data$resultados)
      x_range <- 0:max(max(sim_data$resultados), qpois(0.999, lambda))
      
      # Crear datos para la curva (PMF en los valores)
      pmf_data <- data.frame(
        x = x_range,
        Prob = dpois(x_range, lambda = lambda)
      )
      
      sim_data$plot <- ggplot() +
        geom_histogram(data = datos, aes(x = Valores, y = after_stat(density)), 
                       fill = "#06a480", color = "#2a2727", breaks = seq(-0.5, max(x_range) + 0.5, by = 1)) +
        geom_line(data = pmf_data, aes(x = x, y = Prob), color = "#ceab1d", linewidth = 1) +
        geom_point(data = pmf_data, aes(x = x, y = Prob), color = "#ceab1d", size = 3) +
        labs(
          title = paste("Poisson (λ =", lambda, ")"),
          x = "Valores Simulados",
          y = "Densidad"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", color = "#2c3e50"),
          axis.title = element_text(face = "bold")
        )
      
      sim_data$plot
    })
    
    output$resumen_estadistico <- renderPrint({
      resumen <- capture.output({
        cat("Resumen de Simulaciones:\n")
        cat("Media:", round(mean(sim_data$resultados), 4), "\n")
        cat("Desviación Estándar:", round(sd(sim_data$resultados), 4), "\n")
        cat("Mínimo:", min(sim_data$resultados), "\n")
        cat("Máximo:", max(sim_data$resultados), "\n")
        cat("Cuartiles:\n")
        print(quantile(sim_data$resultados))
      })
      sim_data$summary <- resumen
      cat(resumen, sep = "\n")
    })
  })
  
  # Binomial Negativa
  observeEvent(input$simular_binomial_neg, {
    req(input$r_binomial_neg, input$p_binomial_neg, input$simulaciones_binomial_neg)
    
    n <- input$simulaciones_binomial_neg
    size <- input$r_binomial_neg
    prob <- input$p_binomial_neg
    
    sim_data$resultados <- rnbinom(n, size, prob)
    sim_data$dist_type <- "Binomial Negativa"
    sim_data$params <- list(r = size, p = prob, simulaciones = n)
    
    output$resultados_simulacion <- function() {
      res <- conv_matrix(sim_data$resultados, cols = 10)
      kbl(res, booktabs = TRUE, escape = FALSE, align = "c", digits = 0) %>%
        kable_styling(
          full_width = FALSE,
          bootstrap_options = c("bordered", "striped", "hover", "condensed"),
          font_size = 13
        ) %>%
        row_spec(0, background = "#d0d394", color = "#211d1d", bold = TRUE) %>%
        scroll_box(width = "100%", height = "250px")
    }
    
    output$histograma_simulacion <- renderPlot({
      datos <- data.frame(Valores = sim_data$resultados)
      x_range <- 0:max(max(sim_data$resultados), qnbinom(0.999, size, prob))
      
      # Crear datos para la curva (PMF en los valores)
      pmf_data <- data.frame(
        x = x_range,
        Prob = dnbinom(x_range, size = size, prob = prob)
      )
      
      sim_data$plot <- ggplot() +
        geom_histogram(data = datos, aes(x = Valores, y = after_stat(density)), 
                       fill = "#06a480", color = "#2a2727", breaks = seq(-0.5, max(x_range) + 0.5, by = 1)) +
        geom_line(data = pmf_data, aes(x = x, y = Prob), color = "#ceab1d", linewidth = 1) +
        geom_point(data = pmf_data, aes(x = x, y = Prob), color = "#ceab1d", size = 3) +
        labs(
          title = paste("Binomial Negativa (r =", size, ", p =", prob, ")"),
          x = "Valores Simulados",
          y = "Densidad"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", color = "#2c3e50"),
          axis.title = element_text(face = "bold")
        )
      
      sim_data$plot
    })
    
    output$resumen_estadistico <- renderPrint({
      resumen <- capture.output({
        cat("Resumen de Simulaciones:\n")
        cat("Media:", round(mean(sim_data$resultados), 4), "\n")
        cat("Desviación Estándar:", round(sd(sim_data$resultados), 4), "\n")
        cat("Mínimo:", min(sim_data$resultados), "\n")
        cat("Máximo:", max(sim_data$resultados), "\n")
        cat("Cuartiles:\n")
        print(quantile(sim_data$resultados))
      })
      sim_data$summary <- resumen
      cat(resumen, sep = "\n")
    })
  })
  
  # Descarga de PDF
  output$descargar_pdf <- downloadHandler(
    filename = function() {
      paste("Reporte_", sim_data$dist_type, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf", sep = "")
    },
    content = function(file) {
      temp_rmd <- tempfile(fileext = ".Rmd")
      temp_png <- tempfile(fileext = ".png")
      
      # Grafica y tabla
      ggsave(temp_png, plot = sim_data$plot, width = 8, height = 6, dpi = 300)
      limited_results <- head(sim_data$resultados, 100)
      
      # Creamos el Rmd
      rmd_content <- paste0(
        "---\n",
        "title: 'Reporte de Simulación: ", sim_data$dist_type, "'\n",
        "date: '", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "'\n",
        "output:\n",
        "  pdf_document:\n",
        "    latex_engine: pdflatex\n",
        "    keep_tex: false\n",
        "geometry: margin=1in\n",
        "header-includes:\n",
        "  - \\usepackage{booktabs}\n",
        "  - \\usepackage{longtable}\n",
        "  - \\usepackage{graphicx}\n",
        "  - \\usepackage{array}\n",
        "  - \\usepackage{geometry}\n",
        "  - \\usepackage{amsmath}\n",
        "  - \\usepackage[utf8]{inputenc}\n",
        "  - \\usepackage[T1]{fontenc}\n",
        "  - \\usepackage{lmodern}\n",
        "---\n\n",
        "## Parámetros de Entrada\n\n",
        if (sim_data$dist_type == "Binomial") {
          paste0(
            "- **Distribución**: Binomial\n",
            "- **Número de ensayos (n)**: ", sim_data$params$n, "\n",
            "- **Probabilidad de éxito (p)**: ", sim_data$params$p, "\n",
            "- **Cantidad de simulaciones**: ", sim_data$params$simulaciones, "\n"
          )
        } else if (sim_data$dist_type == "Poisson") {
          paste0(
            "- **Distribución**: Poisson\n",
            "- **Parámetro \\lambda**: ", sim_data$params$lambda, "\n",
            "- **Cantidad de simulaciones**: ", sim_data$params$simulaciones, "\n"
          )
        } else if (sim_data$dist_type == "Binomial Negativa") {
          paste0(
            "- **Distribución**: Binomial Negativa\n",
            "- **Número de éxitos (r)**: ", sim_data$params$r, "\n",
            "- **Probabilidad de éxito (p)**: ", sim_data$params$p, "\n",
            "- **Cantidad de simulaciones**: ", sim_data$params$simulaciones, "\n"
          )
        } else if (sim_data$dist_type == "Otro") {
          values_str <- paste(sim_data$params$values, collapse = ", ")
          probs_str <- paste(round(sim_data$params$probs, 4), collapse = ", ")
          paste0(
            "- **Distribución**: Personalizada\n",
            "- **Valores**: ", values_str, "\n",
            "- **Probabilidades**: ", probs_str, "\n",
            "- **Cantidad de simulaciones**: ", sim_data$params$simulaciones, "\n"
          )
        },
        "\n\n## Resultados de Simulación\n\n",
        "```{r, echo=FALSE, results='asis'}\n",
        "library(kableExtra)\n",
        "res <- conv_matrix(limited_results, cols = 10)\n",
        "kbl(res, booktabs = TRUE, align = 'c', digits = 0, caption = 'Primeros 100 Resultados de Simulación') %>%",
        "  kable_styling(latex_options = c('striped', 'condensed'), font_size = 10) %>%",
        "  row_spec(0, bold = TRUE, background = '#1e81b0', color = 'white')\n",
        "```\n\n",
        "## Histograma de Simulaciones\n\n",
        "\\includegraphics[width=\\linewidth]{", normalizePath(temp_png, winslash = "/"), "}\n\n",
        "## Resumen Estadístico\n\n",
        "```{r, echo=FALSE, results='asis'}\n",
        "cat(sim_data$summary, sep = '\\n')\n",
        "```\n"
      )
      
      # Rmd a archivo
      writeLines(rmd_content, temp_rmd)
      
      # Render Rmd a PDF
      rmarkdown::render(temp_rmd, output_file = file, clean = TRUE, output_format = "pdf_document")
      
      # Limipiamos documentos temporales
      unlink(temp_png)
      unlink(temp_rmd)
    }
  )
  
  ########################################
  observeEvent(input$simular_exp, {
    req(input$lambda_exp, input$simulaciones_exp)
    
    lambda <- input$lambda_exp
    n <- input$simulaciones_exp
    
    X <- rexp(n, rate = lambda)
    Y <- rexp(n, rate = lambda)
    
    sim_data$resultados <- X
    sim_data$dist_type <- "Exponencial"
    sim_data$params <- list(lambda = lambda, simulaciones = n)
    sim_data$ks_test <- ks.test(X, Y)
    sim_data$ecdf_x <- ecdf(X)
    sim_data$ecdf_y <- ecdf(Y)
    
    output$resultados_exp <- function() {
      res <- conv_matrix(round(sim_data$resultados, 4), cols = 10)
      
      kbl(res, booktabs = TRUE, escape = FALSE, align = "c", digits = 4, caption = "Valores simulados (X)") %>%
        kable_styling(
          full_width = FALSE,
          bootstrap_options = c("striped", "hover", "condensed", "bordered"),
          font_size = 13
        ) %>%
        row_spec(0, background = "#457B9D", color = "white", bold = TRUE) %>%
        scroll_box(width = "100%", height = "250px")
    }
    
    
    output$histograma_exp <- renderPlot({
      datos <- data.frame(Valores = sim_data$resultados)
      ggplot(datos, aes(x = Valores)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#06a480", color = "white") +
        stat_function(fun = dexp, args = list(rate = lambda), color = "#e28743", linewidth = 1.2) +
        labs(
          title = paste("Exponencial (λ =", lambda, ")"),
          x = "Valores Simulados", y = "Densidad"
        ) +
        theme_minimal(base_size = 14)
    })
    
    output$resumen_exp <- renderPrint({
      resumen <- capture.output({
        cat("Resumen de Simulaciones:\n")
        cat("Media:", round(mean(sim_data$resultados), 4), "\n")
        cat("Desviación Estándar:", round(sd(sim_data$resultados), 4), "\n")
        cat("Mínimo:", min(sim_data$resultados), "\n")
        cat("Máximo:", max(sim_data$resultados), "\n")
        cat("Cuartiles:\n")
        print(quantile(sim_data$resultados))
      })
      sim_data$summary <- resumen
      cat(resumen, sep = "\n")
    })
    
    output$ks_resultado <- renderPrint({
      ks <- sim_data$ks_test
      cat("Resultado del test de Kolmogorov-Smirnov (X ~ Exp(λ), Y ~ Exp(λ))\n\n")
      print(ks)
      
      cat("\nInterpretación basada en el p-valor:\n")
      cat("Nivel de significancia α = 0.05\n")
      cat(paste0("p-valor obtenido = ", round(ks$p.value, 4), "\n"))
      cat("El vector Y ha sido generado con el mismo parámetro λ que X (misma distribución teórica esperada).\n")
      
      if (ks$p.value > 0.05) {
        cat("Como el p-valor es mayor que 0.05, no se rechaza la hipótesis nula (H₀).\n")
        cat("  → Concluimos que no hay evidencia suficiente para afirmar que las muestras provienen de distribuciones distintas.\n")
      } else {
        cat("Como el p-valor es menor o igual a 0.05, se rechaza la hipótesis nula (H₀).\n")
        cat("  → Concluimos que hay evidencia para afirmar que las muestras provienen de distribuciones diferentes.\n")
      }
    })
    
    
    output$graf_ks <- renderPlot({
      x_vals <- sort(unique(c(X, Y)))
      df <- data.frame(
        x = x_vals,
        ecdf_X = sim_data$ecdf_x(x_vals),
        ecdf_Y = sim_data$ecdf_y(x_vals)
      )
      
      ggplot() +
        geom_line(data = df, aes(x = x, y = ecdf_X, color = " X", linetype = " X"), size = 1.2) +
        geom_line(data = df, aes(x = x, y = ecdf_Y, color = " Y", linetype = " Y"), size = 1.2) +
        scale_color_manual(name = "Muestra", values = c(" X" = "#1D3557", " Y" = "#e63946")) +
        scale_linetype_manual(name = "Muestra", values = c(" X" = "solid", " Y" = "solid")) +
        labs(
          title = "Comparación de funciones de distribución acumuladas (ECDF)",
          subtitle = "Muestras X e Y generadas con  parámetros n, λ",
          x = "x", y = "F(x)"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", color = "#2c3e50"),
          legend.position = "top"
        )
    })
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
})
