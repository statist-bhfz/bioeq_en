# Sys.setlocale("LC_ALL","Russian_Russia.20866")

library(markdown)
library(ggplot2)
library(nlme)


# Function for AUC calculation using trapezoidal rule
aucCalc <- function(conc, time) {
    auc <- numeric(length(time)-1) 
    for (i in 2:length(time)) {
        auc[i-1] <- 0.5*(conc[i]+conc[i-1])*(time[i]-time[i-1])
    }
    return(sum(auc, na.rm=TRUE))
}


shinyServer(function(input, output, session) {

    DAT <- reactive({
        inFile <- input$file1
        
        if (is.null(inFile)) {return(NULL)}
        
        data <- read.csv(inFile$datapath, sep="\t", dec=",", na.strings = "NA")
        
        time <- unique(data$time)
        
        data$seq <- factor(data$seq)
        data$prd <- factor(data$prd)
        data$drug <- factor(data$drug)
        data$subj <- factor(data$subj)
        data$subj2 <- paste(data$subj, data$drug, sep="")
        data$subj2 <- factor(data$subj2, levels=unique(data$subj2), ordered=TRUE)

        results <- data[data$time == 0, c(1:4)]
        results$Cmax <- tapply(data$conc, data$subj2, FUN=max, na.rm=TRUE)
        results$Cmax <- as.numeric(results$Cmax)
        results$auc <- tapply(data$conc, data$subj2, FUN=aucCalc, time=time)
        results$auc <- as.numeric(results$auc)

        model.Cmax <- lme(log(Cmax)~drug+prd+seq, random=~1|subj, data=results)
        model.auc <- lme(log(auc)~drug+prd+seq, random=~1|subj, data=results)
        
        ci.Cmax <- intervals(model.Cmax, level=0.9, which="fixed")
        ci.auc <- intervals(model.auc, level=0.9, which="fixed")
        ci1 <- ci.Cmax$fixed[2, ]
        ci1 <- exp(ci1)*100
        ci2 <- ci.auc$fixed[2, ]
        ci2 <- exp(ci2)*100
        ci1_2 <- rbind(ci1, ci2)
        colnames(ci1_2) <- c("Lower limit of 90% CI, %", "T/R ratio, %",
                             "Upper limit of 90% CI, %")
        rownames(ci1_2) <- c("Cmax", "AUC(0-t)")
        DAT <- list(data=data, model.Cmax=model.Cmax, model.auc=model.auc,
                    ci1_2=ci1_2)
        return(DAT)
    })


    output$plot <- renderPlot({
        if (is.null(input$file1)) { return() }
        if (input$plotType == "both") {
            p <- ggplot(DAT()$data, aes(x=time, y=conc, colour=subj2)) +
                geom_line(size=0.6) + geom_point() + 
                guides(col = guide_legend(ncol = 4)) +
                theme(legend.title=element_blank()) +
                labs(list(title = "PK curves for drugs T and R", 
                          x = "Time, h", 
                          y = "Concentration"))
            print(p)
        }
        if (input$plotType == "test") {
            p <- ggplot(DAT()$data[DAT()$data$drug == "T", ], 
                        aes(x=time, y=conc, colour=subj)) +
                geom_line(size=0.6) + geom_point() + 
                guides(col = guide_legend(ncol = 4)) +
                theme(legend.title=element_blank()) +
                labs(list(title = "PK curve for drug T", 
                          x = "Time, h", 
                          y = "Concentration"))
            print(p)
        }
        if (input$plotType == "ref") {
            p <- ggplot(DAT()$data[DAT()$data$drug == "R", ], 
                        aes(x=time, y=conc, colour=subj)) +
                geom_line(size=0.6) + geom_point() + 
                guides(col = guide_legend(ncol = 4)) +
                theme(legend.title=element_blank()) +
                labs(list(title = "PK curve for drug R", 
                          x = "Time, h", 
                          y = "Concentration"))
            print(p)
        }
        
        if (input$plotType == "both_log") {
            p <- ggplot(DAT()$data, aes(x=time, y=log(conc), colour=subj2)) +
                geom_line(size=0.6) + geom_point() + 
                guides(col = guide_legend(ncol = 4)) +
                theme(legend.title=element_blank()) +
                labs(list(title = "PK curves for drugs T and R", 
                          x = "Time, h", 
                          y = "Log Concentration"))
            print(p)
        }
        if (input$plotType == "test_log") {
            p <- ggplot(DAT()$data[DAT()$data$drug == "T", ], 
                        aes(x=time, y=log(conc), colour=subj)) +
                geom_line(size=0.6) + geom_point() + 
                guides(col = guide_legend(ncol = 4)) +
                theme(legend.title=element_blank()) +
                labs(list(title = "PK curve for drug T", 
                          x = "Time, h", 
                          y = "Log Concentration"))
            print(p)
        }
        if (input$plotType == "ref_log") {
            p <- ggplot(DAT()$data[DAT()$data$drug == "R", ], 
                        aes(x=time, y=log(conc), colour=subj)) +
                geom_line(size=0.6) + geom_point() + 
                guides(col = guide_legend(ncol = 4)) +
                theme(legend.title=element_blank()) +
                labs(list(title = "PK curve for drug R", 
                          x = "Time, h", 
                          y = "Log Concentration"))
            print(p)
        }
    })
    
    output$table.all <- renderDataTable({
        if (is.null(input$file1)) { return() }
        DAT()$data
    })
    
    output$table1 <- renderTable({
        if (is.null(input$file1)) { return() }
        summary(DAT()$model.Cmax)$tTable
    })
    
    output$table2 <- renderTable({
        if (is.null(input$file1)) { return() }
        summary(DAT()$model.auc)$tTable
    })
    
    output$table3 <- renderTable({
        if (is.null(input$file1)) { return() }
        DAT()$ci1_2
    })
    
})

