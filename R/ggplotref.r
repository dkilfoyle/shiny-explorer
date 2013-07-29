#ggplot ref

# title
+ labs(title = "My Title")

# axes
+ xlab("my label")
+ xlim(0,10)

#legend
+ labs(color="My descript")

# bar plots
ggplot(data, aes(x=myfactor)) + geom_bar(stat="bin")
ggplot(data, aes(x=myfactor or myint, y=mynumeric)) + geom_bar(stat="identity") # use identity if y value specified

# stacked bar plots
ggplot(data, aes(x=factor1, fill=factor1)) + geom_bar()

# dogded
+ geom_bar(position="dodge")

# or faceted
ggplot(data, aes(x=factor1)) + geom_bar() + facet_wrap(~ factor2)

# boxplots
ggplot(data, aes(x=factor1, y=numeric1)) + geom_boxplot(notched=T) # notched gives 95% CI for median

# with jittered poins
+ geom_jitter()

# with coloured boxes
+ geom_boxplot(aes(fill=factor1)) # where factor1 is the x factor1
