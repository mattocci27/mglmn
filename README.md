[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/mglmn)](http://cran.r-project.org/package=mglmn)
 [![Downloads](http://cranlogs.r-pkg.org/badges/mglmn?color=brightgreen)](http://cran.rstudio.com/package=mglmn)
# mglmn
Tools for univariate and multivariate generalized linear models with model averaging and null model technique (Nakamura et al. 2015).

The package provides functions to estimate the relative importance of predictor variables in univariate and multivariate generalized linear models. The relative importance predictor variables is calculated by summing the Akaike weights of all models in which that predictor variable is included (Burnham & Anderson, 2002). The sum of the Akaike weights indicates the importance of a variable in explaining variation in a given dataset, relative to other predictor variables included in the analysis. The significance of each predictor variable can be assessed by the null model approach.

The key functions in this package are the following.

* `maglm` Model averaging for GLM based on information theory.
* `mamglm` Model averaging for multivariate GLM based on information theory.
* `ses.maglm` Standardized effect size of relative importance values for * model averaging GLM.
* `ses.mamglm` Standardized effect size of relative importance values for model averaging mutlivariate GLM.

##### References
Burnham, K.P. & Anderson, D.R. (2002) Model selection and multi-model inference: a practical information-theoretic approach. Springer Verlag, New York.

Nakamura A., Burwell C.J., Lambkin C.L., Katabuchi M., McDougall A., Raven R.J. and Neldner V.J. (2015), The role of human disturbance in island biogeography of arthropods and plants: an information theoretic approach, Journal of Biogeography, DOI: 10.1111/jbi.12520
