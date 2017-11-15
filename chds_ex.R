#CHDS

setwd("/Users/hoban/Documents/diagnostics/diagnostics")
df<-read.csv(file = "CHDS.latentexample1.csv")
radical <- read.csv(file= "data1.csv")

##CHDS EXAMPLE

#HOW TO FUNCTIONIZE?
##THIS IS FOR CEG-a
cega.stages <- list("cega.w0", "cega.w1", "cega.w2", "cega.w3", "cega.w4", "cega.w5", "cega.w6", "cega.w7", "cega.w8", "cega.w9")

cega.w0 <- df %>% count(Social)
cega.w1 <- df %>% filter(Social=="High") %>% count(Economic)
cega.w2 <- df %>% filter(Social=="Low") %>% count(Economic)
cega.w3 <- df %>% filter(Social=="High", Economic=="High") %>% count(Events)
cega.w4 <- df %>% filter(Social=="High", Economic=="Low") %>% count(Events)
cega.w5 <- df %>% filter(Social=="Low", Economic=="High") %>% count(Events)
cega.w6 <- df %>% filter(Social=="Low", Economic=="Low") %>% count(Events)
cega.w7 <- df %>% filter((Social=="High" & Economic=="High"  & Events=="Low") |  (Social=="High" & Economic=="Low"  & Events=="Low")) %>% count(Admission)
cega.w8 <- df %>% filter((Social=="High" & Economic=="High"  & Events=="Average") |
                           (Social=="High" & Economic=="Low"  & Events=="Average") |
                           (Social=="Low" & Economic=="High"  & Events=="Low") |
                           (Social=="Low" & Economic=="Low"  & Events=="Low")) %>% count(Admission)
cega.w9 <- df %>% filter((Social=="Low" & Economic=="High"  & Events=="Average") |
                           (Social=="Low" & Economic=="High"  & Events=="High") |
                           (Social=="High" & Economic=="High"  & Events=="High") |
                           (Social=="High" & Economic=="Low"  & Events=="High") |
                           (Social=="Low" & Economic=="Low"  & Events=="High") |
                           (Social=="Low" & Economic=="Low"  & Events=="Average")) %>% count(Admission)
cega.struct <- list(cega.w0, cega.w1, cega.w2, cega.w3, cega.w4, cega.w5, cega.w6, cega.w7, cega.w8, cega.w9)#this is the observed values for each of the stages

#can initalize this, and prompt user to input the pathways for the particular tree
cega.stage.key <- list()
count(df) -> cega.stage.key[[1]]
df %>% count(Social) -> cega.stage.key[[2]]
df %>% count(Social, Economic) -> cega.stage.key[[3]]
df %>% count(Social, Economic, Events) -> cega.stage.key[[4]]
df %>% count(Social, Economic, Events, Admission) -> cega.stage.key[[5]]
#define a stage key for each cut in the data
#Q: how does this change for asymmetries?
cega.stage.key[[1]]$stage <- c("cega.w0")
cega.stage.key[[2]]$stage <- c("cega.w1", "cega.w2")
cega.stage.key[[3]]$stage <- c("cega.w3", "cega.w4", "cega.w5", "cega.w6")
cega.stage.key[[4]]$stage <- c("cega.w8", "cega.w7", "cega.w9", "cega.w8", "cega.w7", "cega.w9", "cega.w9", "cega.w8", "cega.w9", "cega.w9", "cega.w8", "cega.w9")#this contains the structure
cega.stage.key[[5]]$stage <- rep("cega.winf", length(cega.stage.key[[5]]$n))
##THIS IS FOR CEG-b
cegb.w0 <- df %>% count(Social)
cegb.w1 <- df %>% filter(Social=="High") %>% count(Economic)
cegb.w2 <- df %>% filter(Social=="Low") %>% count(Economic)
cegb.w3 <- df %>% filter(Social=="High", Economic=="High" |
                           Social == "High", Economic=="Low") %>% count(Events)
cegb.w4 <- df %>% filter(Social=="Low", Economic=="High") %>% count(Events)
cegb.w5 <- df %>% filter(Social=="Low", Economic=="Low") %>% count(Events)
cegb.w6 <- df %>% filter((Social=="High" & Economic=="Low" & Events=="Low") |
                           (Social=="High" & Economic=="High" & Events=="Low")) %>% count(Admission)
cegb.w7 <- df %>% filter((Social=="High" & Economic=="High"  & Events=="Average") |
                           (Social=="High" & Economic=="Low"  & Events=="Average") |
                           (Social=="Low" & Economic=="High"  & Events=="Low")|
                           (Social=="Low" & Economic=="High"  & Events=="Average")|
                           (Social=="Low" & Economic=="Low"  & Events=="Low")) %>% count(Admission)
cegb.w8 <- df %>% filter((Social=="High" & Economic=="High"  & Events=="High") |
                           (Social=="High" & Economic=="Low"  & Events=="High") |
                           (Social=="Low" & Economic=="High"  & Events=="High")|
                           (Social=="Low" & Economic=="Low"  & Events=="Average") |
                           (Social=="Low" & Economic=="Low"  & Events=="High")) %>% count(Admission)
cegb.struct <- list(cegb.w0, cegb.w1, cegb.w2, cegb.w3, cegb.w4, cegb.w5, cegb.w6, cegb.w7, cegb.w8)#this is the observed values for each of the stages

#can initalize this, and prompt user to input the pathways for the particular tree
cegb.stage.key <- list()
count(df) -> cegb.stage.key[[1]]
df %>% count(Social) -> cegb.stage.key[[2]]
df %>% count(Social, Economic) -> cegb.stage.key[[3]]
df %>% count(Social, Economic, Events) -> cegb.stage.key[[4]]
df %>% count(Social, Economic, Events,Admission) -> cegb.stage.key[[5]]
#define a stage key for each cut in the data
#Q: how does this change for asymmetries?
cegb.stage.key[[1]]$stage <- c("cegb.w0")##THIS MUST START AT 0 AND NUM MUST BE LAST CHAR
cegb.stage.key[[2]]$stage <- c("cegb.w1", "cegb.w2")
cegb.stage.key[[3]]$stage <- c("cegb.w3", "cegb.w3", "cegb.w4", "cegb.w5")
cegb.stage.key[[4]]$stage <- c("cegb.w7", "cegb.w8","cegb.w6","cegb.w7",
                               "cegb.w8","cegb.w6","cegb.w7","cegb.w8",
                               "cegb.w7","cegb.w8","cegb.w8","cegb.w7")#this contains the structure
cegb.stage.key[[5]]$stage <-rep("cegb.winf", length(cegb.stage.key[[5]]$n))
cegb.stages <- list("cegb.w0", "cegb.w1", "cegb.w2", "cegb.w3", "cegb.w4", "cegb.w5", "cegb.w6", "cegb.w7", "cegb.w8")


stages=cegb.stages; struct=cegb.struct; stage.key=cegb.stage.key
