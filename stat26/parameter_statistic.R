library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("总体均值 vs 样本均值"),
  
  sidebarLayout(
    sidebarPanel(
      h3("设置参数"),
      
      actionButton("resample", "重新抽样", class = "btn-primary btn-lg"),
      
      hr(),
      h4("说明"),
      p("总体：30个学生的分数"),
      p("样本：每次从中随机抽取10个学生"),
      p("重复5次抽样，对比每个样本的均值与总体均值")
    ),
    
    mainPanel(
      h3("5次抽样的样本均值与总体均值对比"),
      plotOutput("plot", height = "400px"),
      
      br(),
      
      h4("数值统计"),
      tableOutput("table")
    )
  )
)

server <- function(input, output, session) {
  
  # 总体：30个学生的分数
  population <- c(55, 62, 68, 71, 75, 78, 80, 82, 83, 85, 
                  86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 
                  96, 97, 98, 99, 100, 85, 76, 84, 79, 81)
  
  pop_mean <- mean(population)
  
  # 反应式表达式：5次抽样
  samples_result <- reactive({
    input$resample  # 依赖按钮点击
    
    sample_means <- numeric(5)
    samples_detail <- list()
    
    for (i in 1:5) {
      sample <- sample(population, size = 10, replace = FALSE)
      sample_means[i] <- mean(sample)
      samples_detail[[i]] <- sample
    }
    
    list(means = sample_means, details = samples_detail)
  })
  
  # 绘制图表
  output$plot <- renderPlot({
    result <- samples_result()
    sample_means <- result$means
    
    # 创建数据框
    df <- data.frame(
      样本 = c(paste0("样本", 1:5), "总体"),
      均值 = c(sample_means, pop_mean),
      颜色 = c(rep("样本均值", 5), "总体均值")
    )
    
    p <- ggplot(df, aes(x = 样本, y = 均值, fill = 颜色)) +
      geom_bar(stat = "identity", color = "black", width = 0.6) +
      geom_hline(aes(yintercept = pop_mean), 
                 color = "red", linewidth = 1, linetype = "dashed", 
                 alpha = 0.7) +
      
      scale_fill_manual(values = c("样本均值" = "steelblue", "总体均值" = "orange")) +
      
      labs(title = "5次抽样的样本均值与总体均值对比",
           x = "样本",
           y = "平均分",
           fill = "") +
      
      ylim(75, 95) +
      
      theme_minimal() +
      theme(legend.position = "topright",
            text = element_text(size = 12),
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            axis.text.x = element_text(size = 11))
    
    print(p)
  })
  
  # 输出统计表
  output$table <- renderTable({
    result <- samples_result()
    sample_means <- result$means
    
    data.frame(
      类型 = c("样本1", "样本2", "样本3", "样本4", "样本5", "总体"),
      平均分 = c(round(sample_means, 2), round(pop_mean, 2)),
      与总体均值的差异 = c(round(sample_means - pop_mean, 2), 0)
    )
  }, bordered = TRUE, striped = TRUE)
}

shinyApp(ui, server)
