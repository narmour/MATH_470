# management_strat is a function to be applied to pop every step
manitoba_model_pop = function(pop,management_strat,t){
    if(t == 0){
        return(c())
    }
    else{
        return(c(pop,manitoba_model_pop(do.call(management_strat,list(pop,rnorm(1,.1814,.0262),rnorm(1,.0851,.0552))),management_strat,t-1)))
    }
}


#creates the histograms for birth rate and death rate
birth_death_histograms = function(){

    birth_rate_yr = c(.1754,.1509,.1786,.1940,.2209,.1979,.1776,.2016,.1357)
    death_rate_yr = c(.0741,.0980,.0702,.2264,.1250,.0299,.0581,.0938,.0748,.0645,.0214)
    pdf("birth_rate_histogram.pdf")
    hist(birth_rate_yr,col="yellow",main="Histogram of birth rates",xlab="Birth Rate",ylab="Occurences")
    dev.off()
    pdf("death_rate_histogram.pdf")
    hist(death_rate_yr,col="blue",main="Histogram of death rates",xlab="Death Rate",ylab="Occurences")
    dev.off()



}

# creates the graph containing a line of the observed data
# followed by boxplots of our stochastic model
observed_vs_boxplots = function(){
    start_data = c(27,  51,  57,  53,  56,  67,  86,  96, 107, 124, 140)#observed population data
    time_frame = 36 # number of years

    boxplot_data = lapply(1:time_frame,function(time){
           unlist(lapply(1:5,function(run){
                tail(manitoba_model_pop(57,manitoba_management,time),1)
            }))
    })
    pdf("boxplots.pdf")
   boxplot(boxplot_data[2:time_frame],ylim=c(0,max(unlist(boxplot_data))),xlab="Year",ylab="Elk",
           border=c('white','white','white','white','white','white','white','white','white','white','white','black',
           'black','black','black','black','black','black','black','black','black','black','black','black','black','black','black','black','black','black','black','black','black','black','black','black' ),
           xaxt="n")
    lines(1:11,start_data,lwd=2,col='blue')         

    axis(1,a=1:time_frame,labels=2001:2036)
    legend("bottomright",c("Observed Population","Modeled Population"),lty=c(1,1),lwd=c(2.5,2.5),col=c("blue","black"))
    dev.off()
}

manitoba_management = function(pop,br,dr){
    return(pop * (1+br) - (pop *dr))
}
