#' adja = read.csv('/home/allgoodguys/Documents/Studying/Lund_PhD/methods/bursts-adja.csv')
#' key = as.numeric(adja$noBursts_adja)
#'
#' myfolder='/home/allgoodguys/Documents/Studying/Lund_PhD/epistles/001_article_ratings/260sounds_wav'
#' s = segmentFolder(myfolder)
#' trial = as.numeric(s$nBursts)
#'
#' plot (key, trial)
#' cor (key, trial, use = 'pairwise.complete.obs')
#'
#' #' time1 = proc.time()
#' #' Sys.sleep(1.5)
#' #' time_diff = as.numeric((proc.time() - time1)[3])
#' #' convert_sec_to_hms(time_diff)
#' convert_sec_to_hms = function(time_s) {
#'   hours = time_s %/% 3600
#'   minutes = time_s %/% 60
#'   seconds = round (time_s %% 60, 0)
#'
#'   output = ''
#'   if (hours > 0) output = paste0(output, hours, ' h ')
#'   if (minutes > 0) output = paste0(output, minutes, ' min ')
#'   if (seconds > 0) output = paste0(output, seconds, ' s')
#'
#'   # remove the last space, if any
#'   if (substr(output, nchar(output), nchar(output)) == ' ') {
#'     output = substr(output, 1, nchar(output)-1)
#'   }
#'   return(output)
#' }
#'
#'
#' defaults = list(
#'   shortest_syl = 40,
#'   shortest_pause = 40,
#'   syl_to_global_mean = 0.9,
#'   interburst_min_ms = NULL,
#'   interburst_min_scale = 1,
#'   peak_to_global_max = 0.075,
#'   peak_to_trough = 3,
#'   trough_left = TRUE,
#'   trough_right = FALSE,
#'   smooth_ms = 27,
#'   smooth_overlap = 90
#' )
#'
#'
#'
#' ## optimization loop
#' pars_to_optimize = c('shortest_syl', 'shortest_pause', 'syl_to_global_mean')
#' # pars_to_optimize = c('interburst_min_scale', 'peak_to_global_max', 'peak_to_trough')
#' # pars_to_optimize = c('smooth_ms', 'smooth_overlap')
#' fitness_measure = c('nSyllables', 'nBursts')[2]
#' nIter = 10
#'
#' evaluate_params = function(p) {
#'   if (sum(p < 0) > 0) return (1)  # we don't want negative parameters
#'   params = as.list(p)
#'   names(params) = pars_to_optimize
#'   s = try(do.call(segmentFolder, c(params, myfolder = myfolder)))
#'   if (class(s) == 'try-error') {
#'     stop('error in segmentFolder')
#'   } else {
#'     trial = as.numeric(s[, fitness_measure])
#'     return (1 - cor (key, trial, use = 'pairwise.complete.obs'))
#'   }
#' }
#' # p = as.numeric(unlist(pars_to_optimize_defaults)) *
#' #   rnorm_bounded(length(pars_to_optimize_defaults), 1, .5, low=0)
#' # 1 - evaluate_params(p)
#'
#' pars_to_optimize_defaults = defaults[names(defaults) %in% pars_to_optimize]
#' optimal_pars = list()
#' time_start = proc.time()
#' for (i in 1:nIter) {
#'   p_init = as.numeric(unlist(pars_to_optimize_defaults)) *
#'     rnorm_bounded(length(pars_to_optimize_defaults), 1, .2, low=0)
#'   myOptim = optim(par = p_init,
#'                   fn = evaluate_params,
#'                   method = 'Nelder-Mead',
#'                   control = list(maxit = 50, reltol = .01, trace = 0))
#'   my_r = 1 - myOptim$value # the best achievable correlation with these predictors
#'   my_pars = myOptim$par # optimal pars
#'   optimal_pars[[i]] = c(my_r, my_pars)
#'
#'   time_diff = as.numeric((proc.time() - time_start)[3])
#'   time_left = time_diff / i * (nIter - i)
#'   time_left_hms = convert_sec_to_hms(time_left)
#'   print(paste0('Done ', i, ' / ', nIter, '; Estimated time left: ', time_left_hms))
#' }
#' res = as.data.frame (sapply (optimal_pars, cbind))
#' rownames(res) = c('r', pars_to_optimize)
#' res = t(res)
#' res = res[order(res[, 1], decreasing = TRUE), ]
#' res
#'
#' ## END OF OPTIMIZATION LOOP
#'
#' # examine the results
#' for (c in 2:ncol(res)) {
#'   plot(res[, c], res[, 1], main = colnames(res)[c])
#' }
#'
#' pars = as.list(res[1, 2:4])
#' s = do.call(segmentFolder, c(myfolder, pars))
#' # or: s = do.call(segmentFolder, c(myfolder, list(trough_right = FALSE)))
#' cor(key, as.numeric(s[, fitness_measure]))
#' boxplot(as.numeric(s[, fitness_measure]) ~ as.integer(key), xlab='key')
#' abline(a=0, b=1, col='red')
