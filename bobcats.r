# if perc = 0, hunt_rate is trated as removing hunt_rate bobcats
# if perc = 1, hunt_rate is trated as percentage of pop to add/subtract
#model_pop = function(start_pop,growth_rate,timespan,hunt_rate=0,perc=0){
#    for (y in 2:timespan){
#        start_pop[[y]] = start_pop[[y-1]] * (1 + growth_rate)
#        if(hunt_rate!=0){
#            if(perc ==1){
#                start_pop[[y]] = start_pop[[y]] +((start_pop[[y-1]] * (hunt_rate/100)))
#            }
#            else{
#                start_pop[[y]] = start_pop[[y]]  + hunt_rate 
#            }
#        }
#    }
#    return(start_pop)
#}

# management_strat is a function to be applied to pop every step
model_pop = function(pop,growth_rate,management_strat,t){
    if(t == 0){
        return(c())
    }
    else{
        return(c(pop,model_pop(do.call(management_strat,list(pop * (1+growth_rate))),growth_rate,management_strat,t-1)))
    }
}

create_population_graphs = function(){
    # given values
    best_rate = 0.01676
    medium_rate = .00549
    worst_rate = -0.045
    start_pop = 100
    timespan = 25 # 25 iterations


    best_pop = model_pop(start_pop,best_rate,no_management,timespan)
    medium_pop = model_pop(start_pop,medium_rate,no_management,timespan)
    worst_pop = model_pop(start_pop,worst_rate,no_management,timespan)
    comb = c(best_pop,medium_pop,worst_pop)

    #plot the data
    plot(1:timespan,best_pop,type="l",col="red",ylim=c(min(comb),max(comb)))
    lines(medium_pop,col="blue")
    lines(worst_pop,col="green")

    legend("bottomleft",c("best pop","medium pop","worst pop"),lty=c(1,1),lwd=c(2,2),col=c("red","blue","green"))

    print(best_pop)
}

best_pop_management_strat_graphs = function(){
    # given values
    best_rate = 0.01676
    start_pop = 100
    timespan = 25 # 25 iterations

    h1 = model_pop(start_pop,best_rate,hunt_1,timespan)
    h5 = model_pop(start_pop,best_rate,hunt_5,timespan)
    h1perc = model_pop(start_pop,best_rate,hunt_1_perc,timespan)
    h5perc = model_pop(start_pop,best_rate,hunt_5_perc,timespan)
    comb = c(h1,h5,h1perc,h5perc)
    
    #plot the data
    plot(1:timespan,h1,type="l",col="blue",ylim=c(min(comb),max(comb)))
    lines(h1perc,col="red")
    lines(h5,col="green")
    lines(h5perc,col="yellow")
    legend("bottomleft",c("hunt 1/yr","hunt 1%/yr","hunt 5/yr","hunt 5%/yr"),lty=c(1,1),lwd=c(2,2),col=c("blue","red","green","yellow"))

}

worst_pop_management_strat_graphs = function(){
    # given values
    worst_rate = -0.045
    start_pop = 100
    timespan = 25 # 25 iterations

    a3 = model_pop(start_pop,worst_rate,add_3,timespan)
    a10 = model_pop(start_pop,worst_rate,add_10,timespan)
    a1perc = model_pop(start_pop,worst_rate,add_1_perc,timespan)
    a5perc = model_pop(start_pop,worst_rate,add_5_perc,timespan)
    comb = c(a3,a10,a1perc,a5perc)
    
    #plot the data
    plot(1:timespan,a3,type="l",col="blue",ylim=c(min(comb),max(comb)))
    lines(a1perc,col="red")
    lines(a10,col="green")
    lines(a5perc,col="yellow")
    legend("bottomleft",c("add 3/yr","add 1%/yr","add 10/yr","add 5%/yr"),lty=c(1,1),lwd=c(2,2),col=c("blue","red","green","yellow"))
}

# MANAGEMENT STRATEGIES
no_management = function(pop){return(pop)}
best_stable_200 = function(pop){return((pop + 40) - (pop * .21676))}
hunt_1 = function(pop){ return(pop-1)}
hunt_5 = function(pop){ return(pop-5)}
hunt_1_perc = function(pop){return(pop - (pop * .01))}
hunt_5_perc = function(pop){return(pop - (pop * .05))}
add_3 = function(pop){return(pop+3)}
add_10 = function(pop){return(pop+10)}
add_1_perc = function(pop){return(pop + (pop *.01))}
add_5_perc = function(pop){return(pop + (pop *.05))}




