path <- 'your/path/'

# remove NA data
removeNA <- function(df) {
  for(i in seq_along(df)) {
    df[,i] <- factor(df[,i], levels=mylevels)
  }
  df
}

# remove unwanted cols
removeCols <- function(cols, df) {
  df <- df[cols]
}

# see http://jason.bryer.org/likert/
library(likert)
mylevels <- c('Strongly Disagree', 'Disagree', 'Neither Agree nor Disagree', 'Agree', 'Strongly Agree')

# q80 is Fig 2
q80data = read.csv(paste(path, 'qid80.csv'))
# q84 is Fig 4
q83data = read.csv(paste(path, 'qid83.csv'))
f51data = read.csv(paste(path, 'f51.csv'))
# #   see https://github.com/jbryer/likert/blob/master/demo/UnusedLevels.R - remove unneeded (unanswered qs)3

# all in q83 (Q14)
f52cols <- c("Technical.debt.is.not.simply.bad.quality.but.includes.strategic.architectural.decisions","Defects.are.not.technical.debt","Lack.of.process.is.not.technical.debt","New.features.not.yet.implemented.are.not.technical.debt","Technical.debt.is.not.directly.measureable","Technical.debt.should.not.be.treated.in.isolation.from.the.software.development.context")
# 14-2 "Technical.debt.can.be.introduced.by.context.shift"
# 14-10 .Technical.debt.should.not.be.completely.eliminated.

f51cols <- c("Lack of awareness of technical debt was/is a problem","Incurring technical debt is strategically used to support the business objectives (e.g., low cost/short schedule)","Technical.debt.implies.dealing.with.both.principal.and.interest.","Technical.debt.assessment.depends.on.future.outcomes.","Technical.debt.is.a.metaphor.with.little.implication.for.practice.")

q80data <- removeCols(q80cols,q80data)
q83data <- removeCols(f52cols,q83data)

colnames(q80data) <- c("Lack of Awareness is a Problem", "TD Explicitly Managed", "Consequences costly","Measuring Painful")
colnames(q83data) <- c("TD Also Architectural", "Defects Not TD", "Process Not TD", "Unimplemented Features Not TD", "TD Not Measurable","TD Part of S/W Development Context")
colnames(f51data) <- c("Lack of Awareness is a Problem","TD is Used Strategically","TD Includes Both Principal and Interest", "TD Depends on Future Outcomes","TD is Just a Metaphor")

q80data <- removeNA(q80data)
f51data <- removeNA(f51data)
q83data <- removeNA(q83data)
l <- likert(q83data)

likert.bar.plot(l,text.size=4,text.color="#383737") +
    theme(axis.text.x = element_blank(), axis.text.y = element_text(size="14",colour="black",face="italic")) +
    theme(panel.background = element_blank()) +
    theme(legend.title = element_blank(),legend.text = element_text(size = 12), legend.direction = "horizontal", legend.position="bottom")

# plot various bar charts - latest in Excel
# low.colour <- '#D8B365'
# high.colour <- '#5AB4AC' # from Likert, for standardization
# labels <- c("Company Level", "Business Unit Level", "Program Level", "Team Level", "No Std Approach", "Other")
# stdapproach <- c(6.1, 10.2,12.0,25.1,65.3,2.2)
# stdapp.data <- data.frame(labels,stdapproach)
# stdapp.data <- stdapp.data[1:5,] # get rid of None
# ggplot(stdapp.data,aes(x=factor(labels,as.character(stdapp.data$labels)),y=stdapp.data$stdapproach))  +
#   geom_bar(stat = "identity", colour='black', fill=high.colour) +
#   labs(x="",y="") +
#   theme(panel.background = element_blank(),
#         axis.text = element_text(size=15, colour = 'black'),
#         axis.text.x = element_text(colour='black',size=14,vjust=1,hjust=1,angle=30)
#         ) + geom_text(aes(label=paste(stdapp.data$stdapproach,"%")), vjust=-.5, size=7)


# crosstab created in Qualtrics but could easily do it with xtab or table()
# http://stackoverflow.com/questions/13016022/ggplot2-heatmaps-using-different-gradients-for-categories/13016912
xtab.df <- data.frame(c('Less than 3 years', '3-5 years', 'More than 6 years'), c(80, 82.71,88.63),c(14.07,15.04,8.7), c(5.93,2.26,2.68))
colnames(xtab.df) <- c('System Age', 'Agree/Strongly Agree', 'Neutral', 'Disagree/Strongly Disagree')#, 'Total')

# do for Yule's Q calc below.
xtabage.df <- data.frame(c('Less than 3 years', 'More than 6 years'), c(80, 88.63), c(5.93,2.68))
colnames(xtabage.df) <- c('System Age', 'Agree/Strongly Agree', 'Disagree/Strongly Disagree')#, 'Total')
xtab.m <- melt(xtab.df) #convert the NxN matrix into 1xN^2 vector

ggplot(xtab.m, aes(variable, xtab.m$"System Age")) +
    geom_tile(aes(fill = value), colour = "#D8B365") +
    scale_fill_gradient(low = "white", high = "#5AB4AC") + #r 90 G: 180 B: 172
    scale_x_discrete("", expand = c(0, 0)) +
    scale_y_discrete("", expand = c(0, 0)) +
    theme_grey(base_size = 9) +
    theme(legend.position = "none",
          axis.ticks = element_blank(),
          axis.text = element_text(size=10, colour = 'black'),
          axis.text.x = element_text(angle = 330, hjust = 0)) +
    geom_text(aes(label=paste(round(value,1))), vjust=-.5, size=4)
# ggsave('FSE data/age-arch-heatmap.png')

# load the whole table
all3 <- read.csv(paste(path, 'ALL3_TD_Survey.csv'), header=T,na.strings=c(""))
#cross tab of system age (75) vs. TD is also architectural (83_1) or dev experience (98)
#first, group by system age into 3
u <- factor(all3$QID75)
levels(u) <- list(
  "less than 3 years" = c("Less than 1 year", "1-2 years"),
  # "3-5 years" = c("3-5 years"),
  "6 or more years" = c("6-10 years","More than 10 years"))
#second, group by agrree/Dsiagre
v <- factor(all3$QID83_1)
levels(v) <- list(
  "Agree/Strongly Agree" = c("Agree", "Strongly Agree"),
    # "Neither" = c("Neither Agree nor Disagree"),
  "Disagree/Strongly disagree" = c("Disagree", "Strongly Disagree"))
# all3.s_age <- table(u,v)
all3.s_age <- table(all3$QID75,all3$QID83_1)
all3.s_exp <- table(all3$QID98,all3$QID83_1)
#margina frequencies for rows
margin.table(all3.s_age,1)
# row percentages
# did Yule's Q to see if there was association for a 2x2 matrix of the extremes (young system vs old, A vs Dis). It was -.42 so weak association
round(prop.table(all3.s_age,1),2)

# computing top choices for causes of TD
ranks.df <- data.frame(all3$QID100_1,all3$QID100_2,all3$QID100_3,all3$QID100_4,all3$QID100_5,all3$QID100_6,all3$QID100_7,all3$QID100_8,all3$QID100_9,all3$QID100_10,all3$QID100_11,all3$QID100_12,all3$QID100_13,all3$QID100_14) # excludes "Other" and names
colnames(ranks.df) <- c("Obsolete Code","Overly Complex Code","Inter-module dependencies","External software dependencies","Code duplication","Lack of code documentation","Dependency on external team","Bad Architecture Choices","Obsolete Technology","Poor Deployment Process", "Inadequate testing", "Insufficient test automation", "Inefficient CM/build")
a <- summary(ranks.df)
# means
a[4,]
# medians
a[3,]

# generate clusters of codes
# assumes a list of ID, Code, possibly with multiple codes per id. Order not important
codes <- read.csv(paste(path,'codes-cluster.csv'))
b <- t(table(codes))
b <- b %*% t(b)
#b now an adjacency matrix. Values are useful for showing overall codes on the diagonal.

#plot a node/arc graph weighted http://www.rdatamining.com/examples/social-network-analysis
library(igraph)
g <- graph.adjacency(b, weighted=T, mode = "undirected")
# remove loops
g <- simplify(g)
# set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)
# set seed to make the layout reproducible
set.seed(3952)
layout1 <- layout.fruchterman.reingold(g)
plot(g, layout=layout1)
V(g)$label.color <- rgb(0, 0, .2, .8)
V(g)$frame.color <- NA
egam <- (log(E(g)$weight)+.4) / max(log(E(g)$weight)+.4)
E(g)$color <- rgb(.5, .5, 0, egam)
E(g)$width <- egam
# plot the graph in layout1
plot(g, layout=layout1)
# cluster and plot dendrogram
fc <- fastgreedy.community(g)
dendPlot(fc)

removeNA_100 <- function(df) {
  for(i in seq_along(df)) {
    df[,i] <- factor(df[,i], levels=mylevels_100)
  }
  df
}
mylevels_100 <- c(1:3)
q100cols <- c("Obsolete Code","Overly complex code","Inter-module dependencies","Dependencies on external software packages","Code duplication or repetitive edits","Lack of code documentation","Dependencies on external team's code","Bad architecture choices","Obsolete technology ","Poor deployment process","Inadequate testing","Insufficient test automation","Inefficient CM/build infrastructure")

# plot grouped data for top X choices (arch, code, other)
qid100 <- all3[,substr(names(all3), 1,6) == 'QID100']
qid100 <- qid100[,1:13]
removeNA_100(qid100)
# reduce to 2 d array with value = Rank and variables = choices
qid100.melt <- melt(qid100,,c(1:13),na.rm=TRUE,value.name="Rank")
qid100.melt$Rank <- as.integer(qid100.melt$Rank)

library(reshape2)
library(ggplot2)
#stacked bar chart with 1,2,3 ranks
c <- ggplot(qid100.melt,aes(factor(qid100.melt$variable),fill=factor(Rank))) + geom_bar()+ geom_bar(width=.5)