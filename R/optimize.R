## OPTIMIZE PARAMETERS FOR ACOUSTIC ANALYSIS ##

#' Optimize parameters for acoustic analysis
#'
#' This customized wrapper for \code{\link[stats]{optim}} attemps to optimize the
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
#' @param fitness_par the name of output variable that we are comparing with the
#'   key, e.g. 'nBursts' or 'pitch_median'
#' @param fitness_crit the function used to evaluate how well the output of
#'   \code{myfun} fits the key. Defaults to 1 - Pearson's correlation (i.e. 0 is
#'   perfect fit, 1 is awful fit). For pitch, log scale is more meaningful, so a
#'   good fitness criterion is \code{function(x) 1 - cor(log(x), key, use =
#'   'pairwise.complete.obs')}, where \code{key} is already log-transformed.
#' @param nIter repeat the optimization several times to check convergence
#' @param init initial values of optimized parameters (if NULL, the default
#'   values are taken from the definition of \code{myfun})
#' @param wiggle_init each optimization begins with a random seed, and
#'   \code{wiggle_init} specifies the SD of normal distribution used to generate
#'   random deviation of initial values from the defaults
#' @param control a list of control parameters passed on to
#'   \code{\link[stats]{optim}}. The method used is "Nelder-Mead"
#' @param otherPars a list of additional arguments to \code{myfun}
#' @param mygrid a dataframe with one column per parameter to optimize, with
#'   each row specifying the values to try. If not NULL, \code{optimizePars}
#'   simply evaluates each combination of parameter values (see examples).
#' @param verbose if TRUE, reports the values of parameters evaluated and fitness
#' @return Returns a matrix with one row per iteration, containing Pearson's
#'   correlation between the key and \code{fitness_par} in the first column
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
#' res = optimizePars(myfolder = myfolder, myfun = 'segmentFolder', key = key,
#'   pars_to_optimize = c('shortest_syl', 'shortest_pause', 'syl_to_global_mean'),
#'   fitness_par = 'nBursts',
#'   nIter = 2, control = list(maxit = 50, reltol = .01, trace = 0))
#'
#' # examine the results
#' print(res)
#' for (c in 2:ncol(res)) {
#'   plot(res[, c], res[, 1], main = colnames(res)[c])
#' }
#' pars = as.list(res[1, 2:ncol(res)])  # top candidate (best pars)
#' s = do.call(segmentFolder, c(myfolder, pars))  # segment with best pars
#' cor(key, as.numeric(s[, fitness_par]))
#' boxplot(as.numeric(s[, fitness_par]) ~ as.integer(key), xlab='key')
#' abline(a=0, b=1, col='red')
#'
#' # Try a grid with particular parameter values instead of formal optimization
#' res = optimizePars(myfolder = myfolder, myfun = 'segmentFolder', key = segment_manual,
#'   pars_to_optimize = c('shortest_syl', 'shortest_pause'),
#'   fitness_par = 'nBursts',
#'   mygrid = expand.grid(shortest_syl = c(30, 40),
#'                        shortest_pause = c(30, 40, 50)))
#' 1 - res$fit  # correlations with key
#'
#' # Optimization of PITCH TRACKING (takes several hours!)
#' res = optimizePars(myfolder = myfolder,
#'                    myfun = 'analyzeFolder',
#'                    key = log(pitch_manual),  # log-scale better for pitch
#'                    pars_to_optimize = c('voiced_threshold_spec',
#'                                         'specPitchThreshold_nullNA',
#'                                         'pitchSpec_only_peak_weight',
#'                                         'slope_spec'),
#'                    fitness_par = 'pitch_median',
#'                    pars_bounds = list(low = c(0, 0, 0, 0),
#'                                       high = c(1, 1, 1, Inf)),
#'                    nIter = 2,
#'                    otherPars = list(plot = FALSE, verbose = FALSE, step = 50,
#'                                     pitch_methods = c('autocor', 'spec', 'dom')),
#'                    fitness_crit = function(x) {
#'                      1 - cor(log(x), key, use = 'pairwise.complete.obs') *
#'                        (1 - mean(is.na(x) & !is.na(key)))  # penalize failing to detect F0
#'                      })
#'
#' # Manual coding of simple grid optimization for voiced_threshold_cep,
#' # w/o calling optimizePars():
#' out = list()
#' voiced_threshold_cep = c(.3, .45, .6, .8)
#' for (i in 1:length(voiced_threshold_cep)) {
#'   print(i)
#'   out[[i]] = analyzeFolder(myfolder, plot = FALSE, verbose = FALSE, step = 50,
#'                            pitch_methods = 'cep',
#'                            voiced_threshold_cep = voiced_threshold_cep[i])$pitch_median
#'   print(cor(log(out[[i]]), key, use = 'pairwise.complete.obs'))
#'   print(cor(log(out[[i]]), key, use = 'pairwise.complete.obs') *
#'           (1 - mean(is.na(out[[i]]) & !is.na(key))))
#' }
#'
#' trial = log(out[[3]])  # pick the value to explore
#' cor (key, trial, use = 'pairwise.complete.obs')
#' cor (key, trial, use = 'pairwise.complete.obs') * (1 - mean(is.na(trial) & !is.na(key)))
#' plot (key, trial)
#' abline(a=0, b=1, col='red')
#'
#' # checking combinations of pitch tracking methods
#' myfolder = '/home/allgoodguys/Documents/Studying/Lund_PhD/sounds_corpora/00_ut_260_numbered'
#' key = log(pitch_manual)
#' p = c('autocor', 'cep', 'spec', 'dom')
#' pp = c(list(p),
#'        combn(p, 3, simplify = FALSE),
#'        combn(p, 2, simplify = FALSE),
#'        combn(p, 1, simplify = FALSE))
#' out = list()
#' res = data.frame('pars' = sapply(pp, function(x) paste(x, collapse = ',')),
#'                  cor1 = rep(NA, length(pp)),
#'                  cor2 = rep(NA, length(pp)))
#'
#' for (i in 1:length(pp)) {
#'   out[[i]] = analyzeFolder(myfolder, plot = FALSE, verbose = FALSE, step = 50,
#'                            pitch_methods = pp[[i]])$pitch_median
#'   res$cor1[i] = cor(log(out[[i]]), log(pitch_manual), use = 'pairwise.complete.obs')
#'   res$cor2[i] = cor(log(out[[i]]), log(pitch_manual), use = 'pairwise.complete.obs') *
#'     (1 - mean(is.na(out[[i]]) & !is.na(key)))
#'   print(res[i, ])
#' }
#' res[order(res$cor1, decreasing = TRUE), ]  # max correlation regardless of NA
#' res[order(res$cor2, decreasing = TRUE), ]  # max correlation penalized for NA
#' }  # end of dontrun
optimizePars = function(myfolder,
                        key,
                        myfun,
                        pars_to_optimize,
                        pars_bounds = NULL,
                        fitness_par,
                        fitness_crit = function(x) 1 - cor(x, key, use = 'pairwise.complete.obs'),
                        nIter = 10,
                        init = NULL,
                        wiggle_init = .2,
                        control = list(maxit = 50, reltol = .01, trace = 0),
                        otherPars = list(plot = FALSE, verbose = FALSE),
                        mygrid = NULL,
                        verbose = TRUE) {
  if (is.null(pars_bounds)) {
    pars_bounds = list(low = rep(-Inf, length(pars_to_optimize)),
                       high = rep(Inf, length(pars_to_optimize)))
  }
  defaults = as.list(args(get(myfun)))

  ## Option 1: grid optimization (just evaluate the fitness for each combination of pars)
  if (!is.null(mygrid)) {
    if (!identical(colnames(mygrid)[1:length(pars_to_optimize)], pars_to_optimize)) {
      stop('mygrid should be either NULL or a dataframe with one column per parameter')
    }
    mygrid$fit = NA
    for (i in 1:nrow(mygrid)) {
      mygrid$fit[i] = evaluatePars(p = as.numeric(mygrid[i, 1:length(pars_to_optimize)]),
                                   pars_to_optimize = pars_to_optimize,
                                   myfun  = myfun,
                                   key = key,
                                   fitness_par = fitness_par,
                                   fitness_crit = fitness_crit,
                                   myfolder = myfolder,
                                   otherPars = otherPars,
                                   verbose = verbose)
    }
    return(mygrid)
  }

  ## Option 2: use optim() to find the best values of pars
  if (is.null(init)) {
    pars_to_optimize_defaults = defaults[names(defaults) %in% pars_to_optimize]
  } else {
    pars_to_optimize_defaults = init
  }
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
      fitness_par = fitness_par,
      fitness_crit = fitness_crit,
      otherPars = otherPars,
      myfolder = myfolder,
      key = key,
      method = 'Nelder-Mead',
      control = control,
      verbose = verbose
    )
    my_r = 1 - myOptim$value # the best achievable correlation with these predictors
    my_pars = myOptim$par # optimal pars
    optimal_pars[[i]] = c(my_r, my_pars)
    reportTime(i = i, nIter = nIter, time_start = time_start)
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
                        pars_bounds = NULL,
                        fitness_par,
                        fitness_crit = function(x) 1 - cor(x, key, use = 'pairwise.complete.obs'),
                        myfolder,
                        key,
                        otherPars = list(plot = FALSE, verbose = FALSE),
                        verbose = TRUE) {
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
    trial = as.numeric(s[, fitness_par])
    out = fitness_crit(trial)
    if (verbose) {
      print(paste0('Tried pars ', paste(round(p, 3), collapse = ', '), '; fit = ', round(out, 4)))
    }
    return (out)
  }
}
