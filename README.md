This is a package to implement diagnostic monitors for the class of models referred to as Chain Event Graphs. 

### Installation

The package can be installed using: 
```{r}
devtools:::install_github("rachwhatsit/cegmonitors")
```

### Chain Event Graphs 

Chain Event Graphs (CEGs) are generalizations of a BN.  
Prequential Bayesian network monitors from (Lauritzen) are also available to provide direct comparisons.


### Diagnostics 

Model diagnostics determine how well the model is forecasting subsequente observations. cegmonitor adapts the model diagnostics from the BN paper as outlined in Lauritzen. 
 
 
### Monitors 

- Global monitors: check how well the model is performing overall 
- Partition monitors: check how suitable the staging for a particular cut is 
- Parent-child montiors: assess how well the node in quesion is performing given its immediate parents 
- Cut monitors (unconditional): assesses how well the node is modelling the next obseration given the previous evidence 
- Cut monitors (conditional): assesses how well the node is modelling the next outcome conditional on all other evidence including for the case in question 
- Stage monitors: check the suitability of each situation composing the stage in question 


