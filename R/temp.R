# myfolder = '/home/allgoodguys/Documents/Studying/Lund_PhD/sounds_corpora/00_ut_260_numbered'
# key = log(pitch_manual)
# res = optimizePars(myfolder = myfolder, myfun = 'analyzeFolder', key = log(pitch_manual),
#                    pars_to_optimize = c('silence', 'entropy_threshold'),
#                    fitness_par = 'pitch_median', pars_bounds = list(low = c(0,0), high=c(1,1)),
#                    nIter = 1,
#                    otherPars = list(plot = FALSE, verbose = FALSE, step = 50),
#                    fitness_crit = function(x) 1 - cor(log(x), key, use = 'pairwise.complete.obs'))
#
#
# a = analyzeFolder(myfolder = '~/Downloads/temp/new', plot=F, windowLength = 50, step = 50)
# trial = log(a$pitch_median)
# cor (key, trial, use = 'pairwise.complete.obs')
# plot (key, trial)
# abline(a=0, b=1, col='red')
#
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
#
# #    prior_mean  prior_sd     r
# # 1   31.34996        1   0.9185
# # 2   50.36951        1   0.9143
# # 3   59.21309        1   0.8885
# # 1   31.34996        6   0.9170507
# # 2   50.36951        6   0.9107709
# # 3   59.21309        6   0.8948648
# # 4   31.34996       12   0.9139908
# # 5   50.36951       12   0.9144919
# # 6   59.21309       12   0.8960651
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
#
#
# # NB: check reportTime() !!!
# a = segmentFolder(myfolder, verbose=T)

# a = analyze(sound, samplingRate = 16000, plot = TRUE)
# a = analyze(sound, samplingRate = 16000, plot = TRUE,
#             plot_spec_pars = list(xlab = 'Time, ms', colorTheme = 'seewave', contrast = .8),
#             plot_pitchCands_pars = list(cex = 3, col = c('gray70', 'yellow', 'purple', 'maroon')),
#             plot_pitch_pars = list(col = 'black', lty = 3, lwd = 3))
#
# a = analyze(sound, samplingRate = 16000, plot = TRUE,
#             plot_spec_pars = NA)

