

# models the change in plant_pop and deer_pop given a plant_func and deer_func over timespan iterations
model_pop = function(plant_pop,deer_pop,plant_func,deer_func,timespan){
    if(timespan==0){
        return(c())
    }
    else{
        new_plant = do.call(plant_func,list(plant_pop,deer_pop))
        new_deer = do.call(deer_func,list(plant_pop,deer_pop))
        return(c(plant_pop,deer_pop,model_pop(new_plant,new_deer,plant_func,deer_func,timespan-1)))


    }
}

model = function(){
    year = 30
    day = 10950
    #data = matrix(model_pop(3000,100,plant_growth_year,deer_growth_year,year),ncol=2,byrow=TRUE)
    data = matrix(model_pop(3000,100,plant_growth_day,deer_growth_day,day),ncol=2,byrow=TRUE)
    print(data)
    plot(1:year,data[,1],type='l',col="green",ylim=c(min(data),max(data)))
    lines(1:year,data[,2],col="brown")
}



deer_growth_day = function(plant_pop,deer_pop){
    return(deer_pop + deer_pop * ((-1.1/365) +  (((1.5/365)/3000)*plant_pop)))
}
plant_growth_day = function(plant_pop,deer_pop){
    logistic_growth_rate = ((.8/365) - (((.8/365)/3000) * plant_pop))

    # DEER CALCULATIONS
    eat_rate = (((1.2/365)/3000) * plant_pop)

    # plant_pop + plants added from natural growth - plants eaten from deer population
    return(plant_pop + (plant_pop * logistic_growth_rate) - (deer_pop * eat_rate))
}

deer_growth_year = function(plant_pop,deer_pop){
    return(deer_pop + deer_pop * (-1.1 +  ((1.5/3000)*plant_pop)))
}

plant_growth_year = function(plant_pop,deer_pop){
    logistic_growth_rate = (.8 - ((.8/3000) * plant_pop))

    # DEER CALCULATIONS
    eat_rate = ((1.2/3000) * plant_pop)

    # plant_pop + plants added from natural growth - plants eaten from deer population
    return(plant_pop + (plant_pop * logistic_growth_rate) - (deer_pop * eat_rate))
}
