bobcat_model_pop = function(pop,model,t){
    if(t == 0){
        return(c())
    }
    else{
        return(c(pop,bobcat_model_pop(do.call(model,list(pop)),model,t-1)))
    }
}

deterministic_model = function(pop){
    growth_rate = .01676 # the mean growth rate
    return(pop * (1+growth_rate))
}

demo_only_model = function(pop){
    growth_rate = rnorm(1,.01676,.005)
    return(pop *(1+growth_rate))
}

stochastic_model = function(pop){
    growth_rate = rnorm(1,.01676,.005)

    if(sample(1:10,1) ==1){
        growth_rate = growth_rate *.9
    }
    return(pop *(1+growth_rate))
}

gen_graphs = function(){

    #runs = lapply(1:5,bobcat_model_pop(100,stochastic_model,25))
    runs = lapply(1:5,function(arg){bobcat_model_pop(100,stochastic_model,25)})

    plot(runs[[1]],type="l",col="blue",ylim=c(100,max(unlist(runs))),ylab="Population",xlab="Years",main="Stochastic Bobcat Population Model")
    lines(runs[[2]],type="l",col="red")
    lines(runs[[3]],type="l",col="green")
    lines(runs[[4]],type="l",col="brown")
    lines(runs[[5]],type="l",col="orange")
    legend("bottomright",c("Run 1","Run 2","Run 3","Run 4","Run 5"),lty=c(1,1),lwd=c(2.5,2.5),
                          col=c("blue","red","green","brown","orange"))
}
