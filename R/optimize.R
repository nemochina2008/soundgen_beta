## OPTIMIZE PARAMETERS FOR ACOUSTIC ANALYSIS ##

# myfolder = '/home/allgoodguys/Documents/Studying/Lund_PhD/epistles/001_article_ratings/260sounds_wav'
# key = segment_manual
# myfun = 'segmentFolder'
# pars_to_optimize = c('shortest_syl', 'shortest_pause')
# fitness_measure = 'nBursts'
# pars_bounds = list(low = c(0,0), high=c(Inf,Inf))
#
# pm = read.csv('/home/allgoodguys/Documents/Studying/Lund_PhD/epistles/004_real-fake/analysis/590.csv')
# pitch_manual = pm$pitch_manually
#
# myfolder = '/home/allgoodguys/Downloads/temp'
# files = list.files(myfolder)
# file_n = apply(as.matrix(files), 1, function(x) substr(x, 1, 3))
# key = pitch_manual[as.numeric(file_n)]


res = optimizePars(myfolder = '/home/allgoodguys/Downloads/temp', myfun = 'analyzeFolder', key = key,
                   pars_to_optimize = c('silence', 'entropy_threshold'),
                   fitness_measure = 'pitch_median', pars_bounds = list(low = c(0,0), high=c(1,1)),
                   nIter = 2, otherPars = list(plot = FALSE, verbose = FALSE))

# r    silence entropy_threshold
# V1 0.9396199 0.07935274         0.7982454
# V2 0.9395767 0.07806231         0.9191816

#' Optimize parameters for acoustic analysis
#'
#' This customized wrapper for \code{\link[stat]{optim}} attemps to optimize the
#' parameters of \code{\link{segmentFolder}} or \code{\link{analyzeFolder}} by
#' comparing the results with a manually annotated "key". This optimization
#' function uses a single measurement per audio file (e.g., median pitch or the
#' number of syllables). For other purposes, you may want to adapt the
#' optimization function so that the key specifies the exact timing of
#' syllables, their median length, frame-by-frame pitch values, or any other
#' characteristic that you want to optimize for. The general idea remains the
#' same, however: we want to tune function parameters to fit our type of audio
#' and research priorities. The default settings of \code{\link{segmentFolder}}
#' and \code{\link{analyzeFolder}} have been optimized for human non-linguistic
#' vocalizations.
#'
#' If your sounds are very different from human non-linguistic vocalizations,
#' you may want to change the default values of other arguments to speed up
#' convergence. Again, adapt the code to enforce suitable constraints, depending
#' on your data.
#' @param myfolder path to where the .wav files live
#' @param myfun the function being optimized: either 'segmentFolder' or
#'   'analyzeFolder' (in quotes)
#' @param key a vector containing the "correct" measurement that we are aiming
#'   to reproduce
#' @param pars_to_optimize names of arguments to \code{myfun} that should be
#'   optimized
#' @param pars_bounds a list setting the lower and upper boundaries for possible
#'   values of optimized parameters. For ex., if we optimize \code{smooth_ms}
#'   and \code{smooth_overlap}, reasonable pars_bounds might be list(low = c(5,
#'   0), high = c(500, 95))
#' @param fitness_measure the name of output variable that we are comparing with the key, e.g. 'nBursts' or 'pitch_median'
#' @param nIter repeat the optimization several times to check convergence
#' @param wiggle_init each optimization begins with a random seed, and
#'   \code{wiggle_init} specifies the SD of normal distribution used to generate
#'   random deviation of initial values from the defaults
#' @param control a list of control parameters passed on to
#'   \code{\link[stats]{optim}}. The method used is "Nelder-Mead"
#' @return Returns a matrix with one row per iteration, containing Pearson's
#'   correlation between the key and \code{fitness_measure} in the first column
#'   and the best values of each of the optimized parameters in the remaining
#'   columns.
#' @export
#' @examples
#' \dontrun{
#' # download 260 sounds from Anikin & Persson (2017)
#' # http://cogsci.se/personal/results/
#' # 01_anikin-persson_2016_naturalistics-non-linguistic-vocalizations/260sounds_wav.zip
#' # unzip them into a folder, say '~/Downloads/temp'
#' myfolder = '~/Downloads/temp'  # 260 .wav files live here
#'
#' # Optimization of SEGMENTATION
#' # import manual counts of syllables in 260 sounds from Anikin & Persson (2017) (our "key")
#' key = segment_manual  # a vector of 260 integers
#' # run optimization loop several times with random initial values to check convergence
#' # NB: with 260 sounds and default settings, this might take ~20 min per iteration!
#' res = optimizeSegment(myfolder = myfolder, myfun = 'segmentFolder', key = key,
#'   pars_to_optimize = c('shortest_syl', 'shortest_pause', 'syl_to_global_mean'),
#'   fitness_measure = 'nBursts',
#'   nIter = 2, control = list(maxit = 50, reltol = .01, trace = 0))
#'
#' # examine the results
#' print(res)
#' for (c in 2:ncol(res)) {
#'   plot(res[, c], res[, 1], main = colnames(res)[c])
#' }
#' pars = as.list(res[1, 2:ncol(res)])  # top candidate (best pars)
#' s = do.call(segmentFolder, c(myfolder, pars))  # segment with best pars
#' cor(key, as.numeric(s[, fitness_measure]))
#' boxplot(as.numeric(s[, fitness_measure]) ~ as.integer(key), xlab='key')
#' abline(a=0, b=1, col='red')
#' }
optimizePars = function(myfolder,
                        key,
                        myfun,
                        pars_to_optimize,
                        pars_bounds = NULL,
                        fitness_measure,
                        nIter = 10,
                        wiggle_init = .2,
                        control = list(maxit = 50, reltol = .01, trace = 0),
                        otherPars = list(plot = FALSE, verbose = FALSE)) {
  if (is.null(pars_bounds)) {
    pars_bounds = list(low = rep(-Inf, length(pars_to_optimize)),
                       high = rep(Inf, length(pars_to_optimize)))
  }
  defaults = as.list(args(get(myfun)))

  pars_to_optimize_defaults = defaults[names(defaults) %in% pars_to_optimize]
  optimal_pars = list()
  time_start = proc.time()

  for (i in 1:nIter) {
    # start with randomly wiggled default pars
    p_init = rnorm_bounded(
      length(pars_to_optimize_defaults),
      mean = as.numeric(unlist(pars_to_optimize_defaults)),
      sd = as.numeric(unlist(pars_to_optimize_defaults)) * wiggle_init,
      low = pars_bounds$low, high = pars_bounds$high
    )
    # run Nelder-Mead optimization (other methods don't work)
    myOptim = optim(
      par = p_init,
      fn = evaluatePars,
      myfun = myfun,
      pars_to_optimize = pars_to_optimize,
      pars_bounds = pars_bounds,
      fitness_measure = fitness_measure,
      otherPars = otherPars,
      myfolder = myfolder,
      key = key,
      method = 'Nelder-Mead',
      control = control
    )
    my_r = 1 - myOptim$value # the best achievable correlation with these predictors
    my_pars = myOptim$par # optimal pars
    optimal_pars[[i]] = c(my_r, my_pars)

    time_diff = as.numeric((proc.time() - time_start)[3])
    if (i == nIter) {
      time_total = convert_sec_to_hms(time_diff)
      print(paste0('Completed ', i, ' iterations in ', time_total, '.'))
    } else {
      time_left = time_diff / i * (nIter - i)
      time_left_hms = convert_sec_to_hms(time_left)
      print(paste0('Done ', i, ' / ', nIter, '; Estimated time left: ', time_left_hms))
    }
  }
  res = as.data.frame(sapply(optimal_pars, cbind))
  rownames(res) = c('r', pars_to_optimize)
  res = t(res)
  res = res[order(res[, 1], decreasing = TRUE),]
  return (res)
}


#' Evaluate parameters for optimization
#'
#' Internal soundgen function.
#'
#' Called by \code{\link{optimizePars}}.
#' @param p numeric vector of evaluated values of parameters
#' @inheritParams optimizePars
#' @return Returns 1 - Pearson's correlation between fitness measure and the key
#'   (i.e. 0 is perfect fit, 1 is awful fit).
evaluatePars = function(p,
                        pars_to_optimize,
                        myfun,
                        pars_bounds,
                        fitness_measure,
                        myfolder,
                        key,
                        otherPars) {
  # if the pars go beyond the bounds, don't even evaluate
  if (sum(p < pars_bounds$low) > 0 |
      sum(p > pars_bounds$high) > 0) {
    return(1)
  }
  params = as.list(p)
  names(params) = pars_to_optimize
  s = try(do.call(myfun, c(params, myfolder = myfolder, otherPars)))
  if (class(s) == 'try-error') {
    stop('Error in myfun')
  } else {
    trial = as.numeric(s[, fitness_measure])
    return (1 - cor (key, trial, use = 'pairwise.complete.obs'))
  }
}
