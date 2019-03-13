
## This is the function where we complete a ggplot first,then convert it to plotly,
## and modify the hoverinfo format.
PLOTLY<-function(data){
  library(plotly)
  pdf(NULL)
  A<-ggplot(data, aes(x=Component, y=Counts)) + geom_bar(stat="identity", color="#666666",fill="white", width = 0.5) + labs(y="Total number", x=NULL) +  theme(panel.border=element_rect(fill=NA, size=1, linetype="solid", color="black")) +  theme(panel.background=element_blank(), axis.title.y=element_text(size=12, color="#666666") ) 
  P<-plotly_build(A)
  P
}
