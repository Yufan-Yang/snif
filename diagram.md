### SNIF Diagram

#### General Idea
SNIF sequentially includes one effect from all the main effects (possibly nonlinear) and all the interaction effects formed between the already selected main effects. SNIF accounts for nonlinear effects by using basis function expansions of the original covariates. And the linear original terms are included to avoid the use of nonlinear basis functions when the true effect is linear.
#### Data Preparetion
Original covariates are expanded to suit the mean function below.

![img](https://latex.codecogs.com/svg.latex?%5Cinline%20%5Cfn_cm%20%5Csmall%20E%28y%7Cx_1%2C%5Cdots%2Cx_p%29%3D%5Calpha&plus;%5CSigma_%7Bj%3D1%7D%5E%7Bp%7DX_j%5Cbeta_j&plus;%5CSigma_%7Bk%3D2%7D%5E%7Bp%7D%5CSigma_%7Bl%3D1%7D%5E%7Bk-1%7DX_%7Bkl%7D%5Cgamma_%7Bkl%7D%20%5C%20%5C%20%5C%20%5C%20%5C%20%5C%20%5C%20%5C%20%5C)![img](https://latex.codecogs.com/svg.latex?%5Cinline%20%5Cfn_cm%20%5Csmall%20X_j%3Anx%20M%5C%20%2C%5Cbeta_j%3AMx1%5C%20%2C%5C%20X_%7Bkl%7D%3AnxM%5E2%5C%20%2C%5Cgamma_%7Bkl%7D%3AM%5E2x1)

#### Psudo Code
```r
###Initialize
maxnv=ncol(df) ### Max number of effects to be included
L=c() ### Index set for linear main effects selected
N=c() ### Index set for nonlinear main effects selected
I=c() ### Index set for interation terms selected
P=1:p ### Index set for all linear main effects
NP=1:np ### Index set for all nonlinear main effects
C=P U NP ### Union set of linear and nonlinear main effects
Path=matrix(nrow=maxnv,ncol=2)
colnames(Path)=c("BIC","effects")

For (i in 1:maxnv){
1. choose one effect s from set C such that max(-BIC(s U L U N U N)) ### U means Union
2. remove s from set C
3. add s to relevant set ### For example, if s is linear main effectes, then add s to set L
4. if s is a main effect, add interaction terms between s and exsiting main effects of set L and set N to set C
5. store BIC score and s U L U N U N in matrix Path

}

Path[whichmin(Path$BIC),"effects"] ### Output the model whcih minimizes BIC
```
