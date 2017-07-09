myfolder = '/home/allgoodguys/Documents/Studying/Lund_PhD/sounds_corpora/plots_260'
key = log(pitch_manual)
res = optimizePars(myfolder = myfolder,
                   myfun = 'analyzeFolder',
                   key = log(pitch_manual),
                   pars_to_optimize = c('voiced_threshold_spec',
                                        'specPitchThreshold_nullNA',
                                        'pitchSpec_only_peak_weight',
                                        'slope_spec'),
                   fitness_par = 'pitch_median',
                   pars_bounds = list(low = c(0, 0, 0, 0),
                                      high = c(1, 1, 1, Inf)),
                   nIter = 2,
                   otherPars = list(plot = FALSE, verbose = FALSE, step = 50,
                                    pitch_methods = c('autocor', 'spec', 'dom')),
                   fitness_crit = function(x) {
                     1 - cor(log(x), key, use = 'pairwise.complete.obs') * (1 - mean(is.na(x) & !is.na(key)))
                     })


# cep alone: .3 cor .42/.4 ## .45 cor .53/.4 ## .6 cor .7/.36
out = list()
mypar = c(.3, .45, .6, .8)
for (i in 1:length(mypar)) {
  print(i)
  out[[i]] = analyzeFolder(myfolder, plot = FALSE, verbose = FALSE, step = 50,
                           # pitch_methods = 'cep',
                           voiced_threshold_cep = mypar[i])$pitch_median
  print(cor(log(out[[i]]), key, use = 'pairwise.complete.obs'))
  print(cor(log(out[[i]]), key, use = 'pairwise.complete.obs') *
          (1 - mean(is.na(out[[i]]) & !is.na(key))))
}

trial = log(out[[3]])
cor (key, trial, use = 'pairwise.complete.obs')
cor (key, trial, use = 'pairwise.complete.obs') * (1 - mean(is.na(trial) & !is.na(key)))
plot (key, trial)
abline(a=0, b=1, col='red')


myfolder2 = '/home/allgoodguys/Documents/Studying/Lund_PhD/sounds_corpora/plots_260_2'
a2 = analyzeFolder(myfolder2, plot = F, savePath = myfolder2, pitch_methods = 'spec', voiced_threshold_spec = .35, specPitchThreshold_nullNA = .35, pitchSpec_only_peak_weight = .4, slope_spec = .85)
trial2 = log(a2$pitch_median)
cor (key, trial2, use = 'pairwise.complete.obs')
cor (key, trial2, use = 'pairwise.complete.obs') * (1 - mean(is.na(trial2) & !is.na(key)))
plot (key, trial2)
abline(a=0, b=1, col='red')
#
#
# # with all pitch methods
# a_full = analyzeFolder(myfolder, plot = F, voiced_threshold_autocor = .88)
# a_75_full = analyzeFolder(myfolder, plot = F, voiced_threshold_autocor = .75)
#
# trial = log(a_full$pitch_median) # trial = log(a_75_full$pitch_median)
# cor (key, trial, use = 'pairwise.complete.obs')
# plot (key, trial)
# abline(a=0, b=1, col='red')




# cor (key, log(a$pitchAutocor_median), use = 'pairwise.complete.obs')
# cor (key, log(a$pitchCepstrum_median), use = 'pairwise.complete.obs')
# cor (key, log(a$pitchSpec_median), use = 'pairwise.complete.obs')
# cor (key, log(a$dom_median), use = 'pairwise.complete.obs')
#
# plot (log(key), log(a$pitch_median))
# abline(a=0, b=1, col='red')
#
#
#
#
# a = analyzeFolder('/home/allgoodguys/Documents/Studying/Lund_PhD/sounds_corpora/plots_260_subset_silence',
#               savePath = '/home/allgoodguys/Documents/Studying/Lund_PhD/sounds_corpora/plots_260_subset_silence/',
#               prior_mean = HzToSemitones(200), prior_sd = 12)
#
#
# myfolder = '/home/allgoodguys/Documents/Studying/Lund_PhD/sounds_corpora/00_ut_260_numbered'
# pars_to_optimize = c('prior_mean', 'prior_sd')
# mygrid = expand.grid (prior_mean = HzToSemitones(seq(100, 500, length.out = 3)),
#                       prior_sd = c(6, 12))
# mygrid$r = NA
# for (i in 1:nrow(mygrid)) {
#   mygrid$r[i] = evaluatePars(p = as.numeric(mygrid[i, 1:length(pars_to_optimize)]),
#                              pars_to_optimize = pars_to_optimize,
#                              myfun  = 'analyzeFolder',
#                              key = log(pitch_manual),
#                              fitness_par = 'pitch_median',
#                              fitness_crit = function(x) cor(log(x), key, use = 'pairwise.complete.obs'),
#                              myfolder = myfolder,
#                              otherPars = list(plot = FALSE, verbose = FALSE, windowLength = 40, step = 100),
#                              verbose = TRUE)
# }

# a = analyzeFolder(myfolder, verbose=T, windowLength = 40, step = 100, plot=F)
# cor(log(a$pitch_median), key, use='pairwise.complete.obs') # .898
# plot (log(a$pitch_median), key)
# abline(a=0, b=1, col='red')
#
# b = analyzeFolder(myfolder, verbose=T, windowLength = 40, step = 100, plot=F,
#                   prior_mean = HzToSemitones(300), prior_sd = 6)
# cor(log(b$pitch_median), key, use='pairwise.complete.obs') # .911
# plot (log(b$pitch_median), key)
# abline(a=0, b=1, col='red')


# a = analyze(sound, samplingRate = 16000, plot = TRUE,
#             pitch_methods = c('autocor'), voiced_threshold_autocor = .3)
# a = analyze(sound, samplingRate = 16000, plot = TRUE, pitch_methods = c('autocor','spec'), slope_spec = .75)



# checking combinations of pitch tracking methods
# myfolder = '/home/allgoodguys/Documents/Studying/Lund_PhD/sounds_corpora/00_ut_260_numbered'
# key = log(pitch_manual)
# p = c('autocor', 'cep', 'spec', 'dom')
# pp = c(list(p),
#        combn(p, 3, simplify = FALSE),
#        combn(p, 2, simplify = FALSE),
#        combn(p, 1, simplify = FALSE))
# out = list()
# res = data.frame('pars' = sapply(pp, function(x) paste(x, collapse = ',')),
#                  cor1 = rep(NA, length(pp)),
#                  cor2 = rep(NA, length(pp)))
#
# for (i in 1:14) {
#   print(i)
#   out[[i]] = analyzeFolder(myfolder, plot = FALSE, verbose = FALSE, step = 50,
#                            pitch_methods = pp[[i]])$pitch_median
#   res$cor1[i] = cor(log(out[[i]]), key, use = 'pairwise.complete.obs')
#   res$cor2[i] = cor(log(out[[i]]), key, use = 'pairwise.complete.obs') *
#           (1 - mean(is.na(out[[i]]) & !is.na(key)))
#   print(res[i, ])
# }


# 15          autocor 0.9179815 0.7944071
# 16              cep 0.5331139 0.3957346
# 17             spec 0.8229765 0.8229765
# 18              dom 0.9058887 0.9058887
