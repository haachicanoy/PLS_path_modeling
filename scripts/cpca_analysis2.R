
library(plspm)
library(FactoMineR)

# ======================================================================= #
# PLS-PM: model 2
# ======================================================================= #

## Elements of model

# Path matrix
CTRL = c(0, 0, 0, 0, 0)
HARD = c(1, 0, 0, 0, 0)
SOFT = c(1, 0, 0, 0, 0)
NETW = c(1, 0, 0, 0, 0)
DIRE = c(1, 1, 1, 1, 0)
dir_path2 = rbind(CTRL, HARD, SOFT, NETW, DIRE)
colnames(dir_path2) = rownames(dir_path2)
rm(list=c("HARD", "SOFT", "NETW", "CTRL", "DIRE"))

# Model visualization
innerplot(dir_path2)

# Define list of indicators
dir_blocks = list(as.numeric(na.omit(match(names(h_data[,-c(1:3)]),names(qmfa_data)))),
                  as.numeric(na.omit(match(names(s_data[,-c(1:3)]),names(qmfa_data)))),
                  as.numeric(na.omit(match(names(n_data[,-c(1:3)]),names(qmfa_data)))),
                  as.numeric(na.omit(match(names(c_data),names(qmfa_data)))),
                  as.numeric(na.omit(match(names(d_data),names(qmfa_data)))))

# Formative mode for all blocks
dir_modes = c("B", "B", "B", "B", "B")

## Run plspm analysis
dir_pls2 <- plspm(Data=qmfa_data1, path_matrix=dir_path2, blocks=dir_blocks,
                  modes=dir_modes, scaled=TRUE, scheme="path")

## Plotting results
innerplot(dir_pls2)
outerplot(dir_pls2)

## Summary
dir_pls2$outer_model
dir_pls2$inner_model
dir_pls2$path_coefs
dir_pls2$scores
dir_pls2$crossloadings
dir_pls2$inner_summary
dir_pls2$effects
dir_pls2$unidim
dir_pls2$gof

## Validate model with bootstrap procedure
dir_plsBoot2 <- plspm(Data=qmfa_data1, path_matrix=dir_path2, blocks=dir_blocks,
                      modes=dir_modes, scaled=TRUE, scheme="path", boot.val=TRUE,
                      maxiter=200)
dir_plsBoot2$boot











