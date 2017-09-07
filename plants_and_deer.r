model_pop = function(plant_pop,deer_pop,plant_func,deer_func,timespan){
    if(timespan==0){
        return(c())
    }
    else{
        new_plant = do.call(plant_func,list(plant_pop,deer_pop))
        new_deer = do.call(deer_func,list(plant_pop,deer_pop))
        return(c(plant_pop,deer_pop,u_model_pop(new_plant,new_deer,plant_func,deer_func,timespan-1)))


    }
}

model = function(){
    time = 20
    data = matrix(model_pop(3000,100,plant_growth,deer_growth,time),ncol=2,byrow=TRUE)
    plot(1:time,data[,1],type='l',col="green",ylim=c(min(data),max(data)))
    lines(1:time,data[,2],col="brown")
}





deer_growth = function(plant_pop,deer_pop){
    grow_rate = ((1.5/3000)*plant_pop)
    return(deer_pop + (deer_pop *-1.1) + (deer_pop * grow_rate))
}



plant_growth = function(plant_pop,deer_pop){

    logistic_growth_rate = (.8 - ((.8/3000) * plant_pop))

    # DEER CALCULATIONS
    eat_rate = ((1.2/3000) * plant_pop)
    #print(c(eat_rate,deer_pop,plant_pop))

    # plant_pop + plants added from natural growth - plants eaten from deer population
    return(plant_pop + (plant_pop * logistic_growth_rate) - (deer_pop * eat_rate))
}
