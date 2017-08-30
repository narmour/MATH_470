
# if perc = 0, hunt_rate is trated as removing hunt_rate bobcats
# if perc = 1, hunt_rate is trated as percentage of pop to add/subtract
model_pop = function(start_pop,growth_rate,timespan,hunt_rate=0,perc=0){
    for (y in 2:timespan){
        start_pop[[y]] = start_pop[[y-1]] * (1 + growth_rate)
        if(hunt_rate!=0){
            if(perc ==1){
                start_pop[[y]] = start_pop[[y]] +((start_pop[[y-1]] * (hunt_rate/100)))
            }
            else{
                start_pop[[y]] = start_pop[[y]]  + hunt_rate 
            }
        }
    }
    return(start_pop)
}


create_population_graphs = function(){
    # given values
    best_rate = 0.01676
    medium_rate = .00549
    worst_rate = -0.045
    start_pop = 100
    timespan = 25 # 25 iterations


    best_pop = model_pop(start_pop,best_rate,timespan)
    medium_pop = model_pop(start_pop,medium_rate,timespan)
    worst_pop = model_pop(start_pop,worst_rate,timespan)
    comb = c(best_pop,medium_pop,worst_pop)

    #plot the data
    plot(1:25,best_pop,type="l",col="red",ylim=c(min(comb),max(c(comb))))
    lines(medium_pop,col="blue")
    lines(worst_pop,col="green")

    legend("bottomleft",c("best pop","medium pop","worst pop"),lty=c(1,1),lwd=c(2,2),col=c("red","blue","green"))

    print(max(best_pop))
}


create_management_strat_graphs = function(){
    # given values
    best_rate = 0.01676
    medium_rate = .00549
    worst_rate = -0.045
    start_pop = 100
    timespan = 25 # 25 iterations

    hunt_1 = model_pop(start_pop,best_rate,timespan,hunt_rate=-1,perc=0)
    hunt_5 = model_pop(start_pop,best_rate,timespan,hunt_rate=-5,perc=0)
    hunt_1_perc = model_pop(start_pop,best_rate,timespan,hunt_rate=-1,perc=1)
    hunt_5_perc = model_pop(start_pop,best_rate,timespan,hunt_rate=-5,perc=1)
    comb = c(hunt_1,hunt_5,hunt_1_perc,hunt_5_perc)
    
    #plot the data
    plot(1:25,hunt_1,type="l",col="blue",ylim=c(min(comb),max(c(comb))))
    lines(hunt_1_perc,col="red")
    lines(hunt_5,col="green")
    lines(hunt_5_perc,col="yellow")
    legend("bottomleft",c("hunt 1/yr","hunt 1%/yr","hunt 5/yr","hunt 5%/yr"),lty=c(1,1),lwd=c(2,2),col=c("blue","red","green","yellow"))

}


