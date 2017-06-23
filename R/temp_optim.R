# adja = read.csv('/home/allgoodguys/Documents/Studying/Lund_PhD/methods/bursts-adja.csv')
# key = as.numeric(adja$noBursts_adja)
#
# myfolder='/home/allgoodguys/Documents/Studying/Lund_PhD/epistles/001_article_ratings/260sounds_wav'
# s = segmentFolder(myfolder)
# trial = as.numeric(s$nBursts)
#
# plot (key, trial)
# cor (key, trial, use = 'pairwise.complete.obs')
#
# #
# #
# # s = segmentFolder(myfolder='/home/allgoodguys/Documents/Studying/Lund_PhD/epistles/004_real-fake/analysis/simon-thomas', savePath = '~/Downloads/temp/')
# # key = c(5, 10, 10, 7,
# #         1, 1, 1, 1,
# #         2, 1, 1, 1,
# #         1, 2, 1, 1,
# #         1, 2, 1, 1,
# #         8, 5, 2, 11,
# #         2, 1, 1, 2)
# #
# # trial = as.numeric(s$nBursts)
# #
# # plot (key, trial)
# # cor (key, trial, use = 'pairwise.complete.obs')
#
#
# defaults = list(
#   shortest_syl = 40,
#   shortest_pause = 50,
#   syl_to_global_mean = 0.9,
#   interburst_min_ms = NULL,
#   interburst_min_scale = 1.2,
#   peak_to_global_max = 0.12,
#   peak_to_trough = 3.2,
#   trough_left = TRUE,
#   trough_right = FALSE,
#   smooth_ms = 27,
#   smooth_overlap = 90
# )
#
# evaluate_params = function(p) {
#   if (sum(p < 0) > 0) return (1)  # we don't want negative parameters
#   params = as.list(p)
#   names(params) = pars_to_optimize
#   s = try(do.call(segmentFolder, c(params, myfolder = myfolder)))
#   if (class(s) == 'try-error') {
#     stop('error in segmentFolder')
#   } else {
#     trial = as.numeric(s$nBursts)
#     return (1 - cor (key, trial, use = 'pairwise.complete.obs'))
#   }
# }
# # p = as.numeric(unlist(pars_to_optimize_defaults)) *
# #   soundgen:::rnorm_bounded(length(pars_to_optimize_defaults), 1, .5, low=0)
# # 1 - evaluate_params(p)
#
#
# ## optimization loop
# pars_to_optimize = c('shortest_syl', 'shortest_pause', 'syl_to_global_mean')
# # pars_to_optimize = c('interburst_min_scale', 'peak_to_global_max', 'peak_to_trough')
# nIter = 10
#
# pars_to_optimize_defaults = defaults[names(defaults) %in% pars_to_optimize]
# optimal_pars = list()
# initTime = Sys.time()
# for (i in 1:nIter) {
#   p_init = as.numeric(unlist(pars_to_optimize_defaults)) *
#     soundgen:::rnorm_bounded(length(pars_to_optimize_defaults), 1, .2, low=0)
#   myOptim = optim(par = p_init,
#                   fn = evaluate_params,
#                   method = 'Nelder-Mead',
#                   control = list(maxit = 50, reltol = .01, trace = 0))
#   my_r = 1 - myOptim$value # the best achievable correlation with these predictors
#   my_pars = myOptim$par # optimal pars
#   optimal_pars[[i]] = c(my_r, my_pars)
#
#   remainTime = difftime(Sys.time(), initTime) / i * (nIter - i)
#   remainTime_formatted = capture.output(print (round(remainTime), format = "%H:%M:%S"))
#   remainTime_stripped = substr(remainTime_formatted, 20, nchar(remainTime_formatted))
#   print(paste0('Done ', i, ' / ', nIter, '; Estimated time left: ', remainTime_stripped))
# }
# res = as.data.frame (sapply (optimal_pars, cbind))
# rownames(res) = c('r', pars_to_optimize)
# res = t(res)
# res
#
#
