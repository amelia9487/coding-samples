---
title: "SafeAuto User Pathway"
output:
  flexdashboard::flex_dashboard:
    vertical_layout: fill
runtime: shiny
---

```{r global, include=FALSE}
# load packages
library(flexdashboard)
library(shiny)
library(readr)
library(tidyverse)
library(lubridate)
library(scales)

# load dataset
data <- read_csv("https://raw.githubusercontent.com/jasonadki/SafeautoCodingChallenge/master/safeAuto_data_challenge.csv")

validEvent <- c(
  "Add Drivers",
  "Add Vehicles",
  "Bind Start",
  "Choose Coverag",
  "Download Receipt",
  "Get Premium",
  "Payment Complete",
  "Payment Start",
  "Pre-Quote Portal",
  "Quote Start",
  "Referred to Phone Rep",
  "Retrieve Existing Policy",
  "Retrieve Premium"
  )

importantEvent <- c("Pre-Quote Portal",
                    "Retrieve Premium",
                    "Bind Start",
                    "Payment Complete")

# remove user agent string
newdata <- data[, -4]
# remove rows with NA
newdata <- na.omit(newdata, cols = "RecordDateTime")
# remove rows with event date after 2016-10-13
newdata <- filter(newdata, RecordDateTime < ymd_hms("2016-10-14 00:00:00"))
# remove records that repeating the same event
newdata <- filter(newdata, EventType != 'Client Error')
# focus on more important events
newdata <- newdata[which(newdata$Event %in% importantEvent),]
# correct string in vriable "DeviceBrand"
newdata[which(newdata$DeviceBrand == 'IPad'), 7] = 'iPad'
newdata[which(newdata$DeviceBrand == 'IPhone'), 7] = 'iPhone'
newdata[which(newdata$DeviceBrand == 'iPod touch'), 7] = 'iPod'
newdata[which(newdata$DeviceBrand == 'IPod'), 7] = 'iPod'
newdata[which(newdata$DeviceBrand=='Unknown' & newdata$DeviceIsMobile=='TRUE'), 7] = 'Mobile'
newdata[which(newdata$DeviceBrand == 'Unknown' & newdata$DeviceIsMobile == 'FALSE'), 7] = 'NotMobile'
newdata[which(newdata$DeviceBrand == 'Windows Phone 10.0'), 7] = 'Win.Phone'

# generate columns "Weekday" and "Hour"
newdata <- newdata %>%
  mutate(Weekday = weekdays(RecordDateTime)) %>%
  mutate(Hour = hour(RecordDateTime))

# generate column "TimePeriod"
newdata$TimePeriod = 0
newdata[which(newdata$Hour %in% c(0:6)), 15] = 'Late Night'
newdata[which(newdata$Hour %in% c(7:12)), 15] = 'Morning'
newdata[which(newdata$Hour %in% c(13:18)), 15] = 'Afternoon'
newdata[which(newdata$Hour %in% c(19:23)), 15] = 'Night'

# calculate time spent on each step
smalldata <- newdata %>%
  group_by(InteractionId) %>%
  arrange(RecordDateTime) %>%
  mutate(diff = RecordDateTime - lag(RecordDateTime, default = first(RecordDateTime))) %>% 
  mutate(process = paste(lag(Event, default = "Start of Interaction"), "-", Event))

smalldata$diff <- as.numeric(smalldata$diff)

smalldata <- smalldata %>% 
  mutate(complete = ifelse(Event == "Payment Complete", 1, 0))

completeCheck <- smalldata %>% 
  group_by(InteractionId) %>% 
  summarise(PaymentIsCompleted = sum(complete))

completeCheck[which(completeCheck$PaymentIsCompleted >=1), "PaymentIsCompleted"] = 1
completeCheck$PaymentIsCompleted <- as.factor(completeCheck$PaymentIsCompleted)

combined <- merge(smalldata, completeCheck, by = "InteractionId", all.x = TRUE)

# calculate total time spent
Total <- combined %>% 
  group_by(InteractionId) %>% 
  summarise(TotalTime = sum(diff))

Total <- merge(Total, completeCheck, by = "InteractionId", all.x = TRUE)

forward_steps <- c("Pre-Quote Portal - Retrieve Premium",
                   "Retrieve Premium - Bind Start",
                   "Bind Start - Payment Complete")

backward_steps <- c("Retrieve Premium - Pre-Quote Portal",
                   "Bind Start - Retrieve Premium",
                   "Payment Complete - Bind Start")

# calculate conversion ratio affected by multi-device and multi-browser
DeviceCount <- combined %>% 
  group_by(UserId) %>% 
  summarise(Count = n_distinct(DeviceBrand))

DeviceCal <- merge(combined, DeviceCount, by = "UserId", all.x = TRUE) %>% 
  group_by(InteractionId) %>% 
  summarise(Count = first(Count), PaymentIsCompleted = first(PaymentIsCompleted))
  
lst_1 = c()
lst_2 = c()
lst_3 = c()
for (i in unique(DeviceCount$Count)){
  ratio = sum(DeviceCal$Count == i & DeviceCal$PaymentIsCompleted == 1) / sum(DeviceCal$Count == i)
  lst_1[[i]] = i
  lst_2[[i]] = ratio
  lst_3[[i]] = sum(DeviceCount$Count == i)
}

DeviceRatio <- as.data.frame(list("Count" = lst_1, "Ratio" = lst_2, "Number" = lst_3))

BrowserCount <- combined %>% 
  group_by(UserId) %>% 
  summarise(Count = n_distinct(Browser))

BrowserCal <- merge(combined, BrowserCount, by = "UserId", all.x = TRUE) %>% 
  group_by(InteractionId) %>% 
  summarise(Count = first(Count), PaymentIsCompleted = first(PaymentIsCompleted))

lst_1 = c()
lst_2 = c()
lst_3 = c()
for (i in unique(BrowserCount$Count)){
  ratio = sum(BrowserCal$Count == i & BrowserCal$PaymentIsCompleted == 1) / sum(BrowserCal$Count == i)
  lst_1[[i]] = i
  lst_2[[i]] = ratio
  lst_3[[i]] = sum(BrowserCount$Count == i)
}

BrowserRatio <- as.data.frame(list("Count" = lst_1, "Ratio" = lst_2, "Number" = lst_3))
```

Weekday and Hour
=====================================  

Inputs {.sidebar}
-------------------------------------

```{r}
selectInput("event1", label = "Selected Event", choices = importantEvent, selected = "Pre-Quote Portal")
selectInput("time", label = "Group By", choices = c("Weekday", "Hour"), selected = "Weekday")
```
    
Column {.tabset .tabset-fade data-height=400 data-width=600}
-------------------------------------
    
### Seasonality
    
```{r}
renderPlot({
  x <- newdata[which(newdata$Event == input$event1), input$time] %>%
    table() %>% 
    as.data.frame()
  
  colnames(x) <- c("name", "freq")

  if (input$time == "Weekday"){
    x$name <- factor(x$name, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  }
  
  p <- ggplot(data=x, aes(x = name, y = freq)) + 
    geom_bar(stat = "identity", fill = "#E69F00") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    xlab(input$time) +
    ylab("Count") +
    geom_text(aes(label = freq),hjust = 0.2,vjust = -0.2, size = 4, angle = 45) +
    ggtitle(paste("Count of", input$event1, "Group By", input$time))

  p
})
```

   
### Conversion Ratio (Event Level)
```{r}
renderPlot({
  x1 <- newdata[which(newdata$Event == importantEvent[1]), input$time] %>%
    table() %>% 
    as.data.frame()
  
  x2 <- newdata[which(newdata$Event == importantEvent[4]), input$time] %>%
    table() %>% 
    as.data.frame()
  
  colnames(x1) <- c("name", "freq1")
  colnames(x2) <- c("name", "freq2")
  
  x <- merge(x1, x2, by = "name") %>% 
    mutate(ratio = round(freq2 / freq1, 3))
  
  if (input$time == "Weekday"){
    x$name <- factor(x$name, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  }
  
  p <- ggplot(data=x, aes(x = name, y = ratio)) + 
    geom_bar(stat = "identity", fill = "#56B4E9") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    xlab(input$time) +
    ylab("Conversion Ratio") +
    scale_y_continuous(labels = percent) +
    geom_text(aes(label = paste(ratio*100, "%")),hjust = 0.2,vjust = -0.2, size = 4, angle = 45) +
    ggtitle(paste("Overall Conversion Ratio (Payment / Start) Group By", input$time))

  p
})
```
   
Column {data-width=400}
-------------------------------------
   
### Quote/Start Ratio (Event Level)

```{r}
renderPlot({
  x1 <- newdata[which(newdata$Event == importantEvent[1]), input$time] %>%
    table() %>% 
    as.data.frame()
  
  x2 <- newdata[which(newdata$Event == importantEvent[2]), input$time] %>%
    table() %>% 
    as.data.frame()
  
  colnames(x1) <- c("name", "freq1")
  colnames(x2) <- c("name", "freq2")
  
  x <- merge(x1, x2, by = "name") %>% 
    mutate(ratio = round(freq2 / freq1, 3))
  
  if (input$time == "Weekday"){
    x$name <- factor(x$name, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  }
  
  p <- ggplot(data=x, aes(x = name, y = ratio)) + 
    geom_point(shape = 21, colour = "#56B4E9", fill = "#56B4E9", size = 4) + 
    xlab(input$time) +
    ylab("Conversion Ratio") +
    scale_y_continuous(labels = percent) +
    ggtitle(paste(importantEvent[2], "/", importantEvent[1], "Group By", input$time))

  p
})
```   
 
### Bind/Quote Ratio (Eevnt Level)
    
```{r}
renderPlot({
  x1 <- newdata[which(newdata$Event == importantEvent[2]), input$time] %>%
    table() %>% 
    as.data.frame()
  
  x2 <- newdata[which(newdata$Event == importantEvent[3]), input$time] %>%
    table() %>% 
    as.data.frame()
  
  colnames(x1) <- c("name", "freq1")
  colnames(x2) <- c("name", "freq2")
  
  x <- merge(x1, x2, by = "name") %>% 
    mutate(ratio = round(freq2 / freq1, 3))
  
  if (input$time == "Weekday"){
    x$name <- factor(x$name, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  }
  
  p <- ggplot(data = x, aes(x = name, y = ratio)) + 
    geom_point(shape = 21, colour = "#56B4E9", fill = "#56B4E9", size = 4) + 
    xlab(input$time) +
    ylab("Conversion Ratio") +
    scale_y_continuous(labels = percent) +
    ggtitle(paste(importantEvent[3], "/", importantEvent[2], "Group By", input$time))

  p
})
```

### Payment/Bind Ratio (Event Level)
    
```{r}
renderPlot({
  x1 <- newdata[which(newdata$Event == importantEvent[3]), input$time] %>%
    table() %>% 
    as.data.frame()
  
  x2 <- newdata[which(newdata$Event == importantEvent[4]), input$time] %>%
    table() %>% 
    as.data.frame()
  
  colnames(x1) <- c("name", "freq1")
  colnames(x2) <- c("name", "freq2")
  
  x <- merge(x1, x2, by = "name") %>% 
    mutate(ratio = round(freq2 / freq1, 3))
  
  if (input$time == "Weekday"){
    x$name <- factor(x$name, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  }
  
  p <- ggplot(data = x, aes(x = name, y = ratio)) + 
    geom_point(shape = 21, colour = "#56B4E9", fill = "#56B4E9", size = 4) + 
    xlab(input$time) +
    ylab("Conversion Ratio") +
    scale_y_continuous(labels = percent) +
    ggtitle(paste(importantEvent[4], "/", importantEvent[3], "Group By", input$time))

  p
})
```

Device and Browser
=====================================

Inputs {.sidebar}
-------------------------------------

```{r}
selectInput("event2", label = "Selected Event", choices = importantEvent, selected = "Pre-Quote Portal")
selectInput("device", label = "Group By", choices = c("Browser", "DeviceBrand"), selected = "Browser")
```
    
Column {.tabset .tabset-fade data-height=400 data-width=600}
-------------------------------------
    
### Device and Browser Impact
    
```{r}
renderPlot({
  x <- newdata[which(newdata$Event == input$event2), input$device] %>%
    table() %>% 
    as.data.frame()
  
  colnames(x) <- c("name", "freq")
  
  p <- ggplot(data = x, aes(x = name, y = freq)) + 
    geom_bar(stat = "identity", fill = "tomato") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    xlab(input$event2) +
    ylab("Count") +
    geom_text(aes(label = freq),hjust = 0.2,vjust = -0.2, size = 4, angle = 45) +
    ggtitle(paste("Count of", input$event2, "Group By", input$device))
  
  p
})
```

### Conversion Ratio (Event Level)
```{r}
renderPlot({
  x1 <- newdata[which(newdata$Event == importantEvent[1]), input$device] %>%
    table() %>% 
    as.data.frame()
  
  x2 <- newdata[which(newdata$Event == importantEvent[4]), input$device] %>%
    table() %>% 
    as.data.frame()
  
  colnames(x1) <- c("name", "freq1")
  colnames(x2) <- c("name", "freq2")
  
  x <- merge(x1, x2, by = "name") %>% 
    mutate(ratio = round(freq2 / freq1, 3))
  
  p <- ggplot(data = x, aes(x = name, y = ratio)) + 
    geom_bar(stat = "identity", fill = "#56B4E9") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    xlab(input$device) +
    ylab("Conversion Ratio") +
    scale_y_continuous(labels = percent) +
    geom_text(aes(label = paste(ratio * 100, "%")),hjust = 0.2, vjust = -0.2, size = 4, angle = 45) +
    ggtitle(paste("Overall Conversion Ratio (Payment / Start) Group By", input$device))

  p
})
```

### Multi-Device Impact on Conversion Ratio (Interaction Level)
```{r}
p <- ggplot(data=DeviceRatio, aes(x = Count, y = Ratio)) + 
  geom_bar(stat = "identity", fill = "#56B4E9") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("Count") +
  ylab("Conversion Ratio") +
  geom_text(aes(label = paste(percent(round(Ratio, 3)), ",", Number)),hjust = 0.5,vjust = 0, size = 3, angle = 0) +
  ggtitle("Conversion Ratio by Count of Device on User Level")

p
```

### Multi-Browser Impact on Conversion Ratio (Interaction Level)
```{r}
p <- ggplot(data=BrowserRatio, aes(x = Count, y = Ratio)) + 
  geom_bar(stat = "identity", fill = "#56B4E9") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("Count") +
  ylab("Conversion Ratio") +
  geom_text(aes(label = paste(percent(round(Ratio, 3)), ",", Number)),hjust = 0.5,vjust = 0, size = 3, angle = 0) +
  ggtitle("Conversion Ratio by Count of Browser on User Level")

p
```
   
Column {data-width=400}
-------------------------------------

### Quote/Start Ratio (Event Level)
```{r}
renderPlot({
  x1 <- newdata[which(newdata$Event == importantEvent[1]), input$device] %>%
    table() %>% 
    as.data.frame()
  
  x2 <- newdata[which(newdata$Event == importantEvent[2]), input$device] %>%
    table() %>% 
    as.data.frame()
  
  colnames(x1) <- c("name", "freq1")
  colnames(x2) <- c("name", "freq2")
  
  x <- merge(x1, x2, by = "name") %>% 
    mutate(ratio = round(freq2 / freq1, 3))
  
  p <- ggplot(data=x, aes(x = name, y = ratio)) + 
    geom_point(shape = 21, colour = "#56B4E9", fill = "#56B4E9", size = 4) + 
    xlab(input$device) +
    ylab("Conversion Ratio") +
    scale_y_continuous(labels = percent) +
    ggtitle(paste(importantEvent[2], "/", importantEvent[1], "Group By", input$device))

  p
})
```
 
### Bind/Quote Ratio (Event Level)

```{r}
renderPlot({
  x1 <- newdata[which(newdata$Event == importantEvent[2]), input$device] %>%
    table() %>% 
    as.data.frame()
  
  x2 <- newdata[which(newdata$Event == importantEvent[3]), input$device] %>%
    table() %>% 
    as.data.frame()
  
  colnames(x1) <- c("name", "freq1")
  colnames(x2) <- c("name", "freq2")
  
  x <- merge(x1, x2, by = "name") %>% 
    mutate(ratio = round(freq2 / freq1, 3))
  
  p <- ggplot(data=x, aes(x = name, y = ratio)) + 
    geom_point(shape = 21, colour = "#56B4E9", fill = "#56B4E9", size = 4) + 
    xlab(input$time) +
    ylab("Conversion Ratio") +
    scale_y_continuous(labels = percent) +
    ggtitle(paste(importantEvent[3], "/", importantEvent[2], "Group By", input$device))

  p
})
```   
 
### Payment/Bind Ratio (Event Level)
    
```{r}
renderPlot({
  x1 <- newdata[which(newdata$Event == importantEvent[3]), input$device] %>%
    table() %>% 
    as.data.frame()
  
  x2 <- newdata[which(newdata$Event == importantEvent[4]), input$device] %>%
    table() %>% 
    as.data.frame()
  
  colnames(x1) <- c("name", "freq1")
  colnames(x2) <- c("name", "freq2")
  
  x <- merge(x1, x2, by = "name") %>% 
    mutate(ratio = round(freq2 / freq1, 3))
  
  p <- ggplot(data=x, aes(x = name, y = ratio)) + 
    geom_point(shape = 21, colour = "#56B4E9", fill = "#56B4E9", size = 4) + 
    xlab(input$device) +
    ylab("Conversion Ratio") +
    scale_y_continuous(labels = percent) +
    ggtitle(paste(importantEvent[4], "/", importantEvent[3], "Group By", input$device))

  p
})
```

Time Spending {data-orientation=rows}
=====================================

Inputs {.sidebar}
-------------------------------------

```{r}
radioButtons("step", "Steps:",
               c("Forward" = "forward",
                 "Backward" = "backward"))
```
   
Row {data-height=600}
-------------------------------------

### Total Time Spending Distribution

```{r}
renderPlot({
  df <- filter(Total, TotalTime <= 3600)
  
  res <- df %>% 
    group_by(PaymentIsCompleted) %>% 
    summarise(avg = mean(TotalTime))
  
  p <- ggplot(data= df, aes(x = TotalTime, color = PaymentIsCompleted)) + 
    geom_density() +
    geom_vline(data = res, aes(xintercept = avg, color = PaymentIsCompleted), linetype = "dashed") +
    theme(legend.position = c(0.8, 0.8)) +
    ggtitle(paste("Test"))

  p
})
```

Row
-------------------------------------
   
### Step One (Forward/Backward)

```{r}
renderPlot({
  selected_process = ifelse(input$step == "forward", 
                            "Pre-Quote Portal - Retrieve Premium", 
                            "Retrieve Premium - Pre-Quote Portal")

  df <- combined[which(combined$process == selected_process), ]
  
  df <- filter(df, diff <= 1200)
    
  R0 <- sum(combined$process == selected_process & combined$PaymentIsCompleted == 0) / sum(combined$PaymentIsCompleted == 0)
  R0 = round(R0, 3)
  R1 <- sum(combined$process == selected_process & combined$PaymentIsCompleted == 1) / sum(combined$PaymentIsCompleted == 1)
  R1 = round(R1, 3)
  
  res <- df %>% 
    group_by(PaymentIsCompleted) %>% 
    summarise(avg = mean(diff))
  
  p <- ggplot(data= df, aes(x = diff, color = PaymentIsCompleted)) + 
    geom_density() +
    geom_vline(data = res, aes(xintercept = avg, color = PaymentIsCompleted), linetype = "dashed") +
    theme(legend.position = c(0.8, 0.8)) +
    labs(title = selected_process, subtitle = paste("Step/Total (Unpaid):", percent(R0), ",", "Step/Total (Paid):", percent(R1)))

  p
})
```

    
### Step Two (Forward/Backward)

```{r}
renderPlot({
  selected_process = ifelse(input$step == "forward", 
                            "Retrieve Premium - Bind Start", 
                            "Bind Start - Retrieve Premium")

  df <- combined[which(combined$process == selected_process), ]
  
  df <- filter(df, diff <= 1200)
    
  R0 <- sum(combined$process == selected_process & combined$PaymentIsCompleted == 0) / sum(combined$PaymentIsCompleted == 0)
  R0 = round(R0, 3)
  R1 <- sum(combined$process == selected_process & combined$PaymentIsCompleted == 1) / sum(combined$PaymentIsCompleted == 1)
  R1 = round(R1, 3)
  
  res <- df %>% 
    group_by(PaymentIsCompleted) %>% 
    summarise(avg = mean(diff))
  
  p <- ggplot(data= df, aes(x = diff, color = PaymentIsCompleted)) + 
    geom_density() +
    geom_vline(data = res, aes(xintercept = avg, color = PaymentIsCompleted), linetype = "dashed") +
    theme(legend.position = c(0.8, 0.8)) +
    labs(title = selected_process, subtitle = paste("Step/Total (Unpaid):", percent(R0), ",", "Step/Total (Paid):", percent(R1)))

  p
})
```

### Step Three (Forward/Backward)

```{r}
renderPlot({
  selected_process = ifelse(input$step == "forward", 
                            "Bind Start - Payment Complete", 
                            "Payment Complete - Bind Start")

  df <- combined[which(combined$process == selected_process), ]
  
  df <- filter(df, diff <= 1200)
    
  R0 <- sum(combined$process == selected_process & combined$PaymentIsCompleted == 0) / sum(combined$PaymentIsCompleted == 0)
  R0 = round(R0, 3)
  R1 <- sum(combined$process == selected_process & combined$PaymentIsCompleted == 1) / sum(combined$PaymentIsCompleted == 1)
  R1 = round(R1, 3)
  
  res <- df %>% 
    group_by(PaymentIsCompleted) %>% 
    summarise(avg = mean(diff))
  
  p <- ggplot(data= df, aes(x = diff, color = PaymentIsCompleted)) + 
    geom_density() +
    geom_vline(data = res, aes(xintercept = avg, color = PaymentIsCompleted), linetype = "dashed") +
    theme(legend.position = c(0.8, 0.8)) +
    labs(title = selected_process, subtitle = paste("Step/Total (Unpaid):", percent(R0), ",", "Step/Total (Paid):", percent(R1)))

  p
})
```

IsMobile & TimePeriod {data-orientation=rows}
=====================================

Inputs {.sidebar}
-------------------------------------
```{r}
selectInput("event3", label = "Selected Event", choices = importantEvent, selected = "Pre-Quote Portal")
```

Row 
-------------------------------------

### Comparison of mobile and non-mobile users

```{r}
renderPlot({
  x <- newdata%>%filter(Event == input$event3)%>%
  group_by(TimePeriod, DeviceIsMobile)%>%summarise(totalcount=n())
  
  p <- ggplot(data = x, aes(x = TimePeriod, y = totalcount, fill=DeviceIsMobile)) + 
    geom_bar(stat = "identity", position = position_dodge(width = 0.5)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    xlab(input$event3) +
    ylab("Count") +
    geom_text(aes(label = totalcount),hjust = 0.2,vjust = -0.2, size = 4, angle = 0) +
    ggtitle(paste("Count of", input$event3, "Group By TimePeriod"))
  
  p  
})
```
