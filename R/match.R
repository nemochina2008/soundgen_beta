# ## (temp): FIND SOUNDGEN SETTINGS TO REPRODUCE AN EXISTING SOUND ##
#
# target = eval(parse(text = presets$Chimpanzee$Scream_conflict))
# playme(target, 16000)
# pars = c('sylLen', 'pitchAnchors')
# pars_to_round = c('nSyl')
#
# match = function(target,
#                  samplingRate = NULL,
#                  pars,
#                  init = NULL,
#                  probMutation = .25,
#                  stepVariance = 0.1,
#                  maxIter = 50,
#                  minExpectedDelta = 0.001,
#                  windowLength = 40,
#                  overlap = 50,
#                  step = NULL,
#                  verbose = T,
#                  padWith = NA,
#                  penalizeLengthDif = T) {
#   if (is.null(step)) step = windowLength * (1 - overlap / 100)
#   if (is.character(target)) {
#     targetWave = readWave(target)
#     targetVector = targetWave@left
#   } else if (is.numeric(target)) {
#     targetVector = target
#     if (is.null(samplingRate)) {
#       stop ('Please specify samplingRate, eg 44100')
#     }
#     targetWave = tuneR::Wave(s, samp.rate = samplingRate, bit = 16)
#   }
#
#   targetSpec = t(tuneR::melfcc(
#       targetWave,
#       wintime = windowLength / 1000,
#       hoptime = step / 1000,
#       maxfreq = samplingRate / 2,
#       nbands = 100 * windowLength / 20,
#       spec_out = T
#     )$aspectrum)
#   # nbands=log2(11025/20) * 12 * 2  (~9 octaves from 20 to 10000 Hz, *12 semitones, *2 quartertones
#   targetSpec_log = log01(targetSpec) # range(targetSpec_log)
#
#   # show the audiogram of the target
#   seewave::filled.contour.modif2(
#     x = seq(1, ncol(targetSpec_log) * step,
#             length.out = ncol(targetSpec_log)),
#     y = 1:nrow(targetSpec_log),
#     z = t(targetSpec_log),
#     levels = seq(0, 1, length = 30),
#     color.palette = function(x) gray(seq(1, 0, x))
#   )
#
#   ## initialize
#   # start with default par values
#   parDefault = defaults[pars]
#
#   # analyse the target and update
#   as = segment(target, samplingRate = samplingRate, plot = FALSE)
#   aa = analyze(target, samplingRate = samplingRate, plot = FALSE)
#
#   parDefault$sylLen = mean(as$syllables$dur)
#   p = as.numeric(na.omit(aa$pitch))
#   p = downsample(p, srNew = 5, srOld = 1 / step * 1000)  # downsample F0 measures to 5 Hz
#   parDefault$pitchAnchors = data.frame(time = seq(0, 1, length.out = length(p)),
#                                        value = p)
#
#   # replace defaults with user-provided values, if any
#   if (!is.null(init)) {
#     parDefault[names(init)] = init
#   }
#
#   # calculate distance of initial par values
#   output = list(list(pars = parDefault, distance = NA))
#   parLoop = parDefault
#   candVector = try(do.call(soundgen, parLoop), silent = FALSE)
#   candWave = tuneR::Wave(candVector, samp.rate = samplingRate, bit = 16)
#   # playme(candVector, samplingRate)
#   if (class(candidate_sound) == 'try-error') {
#     stop ('Invalid initial pars')
#   }
#   candSpec = t(tuneR::melfcc(
#     candWave,
#     wintime = windowLength / 1000,
#     hoptime = step / 1000,
#     maxfreq = samplingRate / 2,
#     nbands = 100 * windowLength / 20,
#     spec_out = T
#   )$aspectrum)
#   candSpec_log = log01(candSpec) # range(candSpec_log)
#   # seewave::filled.contour.modif2 (x=1:ncol(candSpec_log), y=1:nrow(candSpec_log), z=t(candSpec_log), levels=seq(0, 1, length=30), color.palette=function(x) gray(seq(from=1, to=0, length=x)))
#   output[[1]]$distance = suppressWarnings(spec_sim(
#     candSpec_log,
#     targetSpec_log,
#     padWith = padWith,
#     penalizeLengthDif = penalizeLengthDif
#   ))
#
#   # iteratively mutate pars and save par values that improve distance
#   i = 1
#   while (i < maxIter) {
#     parMut = parLoop
#     # choose pars to mutate
#     idx_mut = rbinom(n = length(pars),
#                      size = 1,
#                      prob = probMutation)
#     idx_mut_bin = which(idx_mut == 1)
#     parsToMutate = pars[idx_mut_bin]
#     if (length(parsToMutate) == 0) {
#       # need to mutate at least one par
#       parsToMutate = sample(pars, size = 1)
#     }
#
#     # prepare a list of mutated par values to feed to the sound generator
#     for (p in parsToMutate) {
#       if (p %in% c('sylLen')) {  # write a full list!!!
#         # continuous pars
#         l = permittedValues[p, 'low']
#         h = permittedValues[p, 'high']
#         parMut[[p]] = rnorm_bounded(
#           n = 1,
#           mean = as.numeric(parLoop[p]),
#           low = l,
#           high = h,
#           sd = (h - l) * stepVariance,
#           roundToInteger = (p %in% pars_to_round)
#         )
#       } else if (p %in% c('pitchAnchors')) {  # write a full list!!!
#         # anchors
#         parMut[[p]] = wiggleAnchors(df = parLoop[[p]],
#                                   temperature = stepVariance,
#                                   temp_coef = 0.1,
#                                   low = c(0, 0),
#                                   high = c(1, 3500),
#                                   wiggleAllRows = FALSE)
#       }
#     }
#
#     # # special case: breathing type (binary 1/0 pars)
#     # b = which(pars_br %in% parsToMutate)
#     # if (length(b) > 0) {
#     #   mutateMe = rbinom(n = b,
#     #                     size = 1,
#     #                     prob = probMutation) # throw a dice for each of 3 breathing-type pars
#     #   brParsToMutate = pars_br[which(mutateMe == 1)]
#     #   if (length(brParsToMutate) > 0) {
#     #     pars_list[brParsToMutate, 'value_new'] = abs(pars_list[brParsToMutate, 'value_new'] - 1) # switch from 0 to 1 or vice versa with probability = probMutation
#     #   }
#     # }
#
#     # generate a new sound based on the mutated pars and compare it to the reference spectrogram
#     candVector = try(do.call(soundgen, parMut), silent = FALSE)
#     # playme(candVector, samplingRate)
#     if (class(candVector) == 'try-error') {
#       distance_new = -Inf
#       delta = -Inf
#     } else {
#       candWave = tuneR::Wave(candVector, samp.rate = samplingRate, bit = 16)
#       candSpec = t(tuneR::melfcc(candWave,
#           wintime = windowLength / 1000,
#           hoptime = step / 1000,
#           maxfreq = maxfreq,
#           nbands = 100 * windowLength / 20,
#           spec_out = T
#         )$aspectrum)
#       candSpec_log = log01(candSpec) # range(candSpec_log)
#       distance_new = suppressWarnings(spec_sim(
#         candSpec_log,
#         targetSpec_log,
#         padWith = padWith,
#         penalizeLengthDif = penalizeLengthDif
#       ))
#       delta = output[[length(output)]]$distance - distance_new  # want to minimize distance
#     }
#
#     condition = try ((delta > minExpectedDelta))
#     if (class(condition) == 'try-error' | is.na(condition)) {
#       print(parLoop) # debugging
#       break
#     } else if (condition) {
#       i = 1
#       output = c(output, list(list(pars = parMut,
#                                   distance = distance_new)))
#       parLoop = parMut
#       if (verbose) {
#         print (paste('Best distance: ', round(output[[length(output)]]$distance, 4)))
#         playme(candVector, samplingRate)
#       }
#     } else {
#       parMut = parLoop # back to previous step
#     }
#     i = i + 1
#   }
#
#   if (verbose) {
#     if (length(output) == 1) {
#       print ('Failed to improve! Try increasing maxIter.')
#     } else {
#       print ('Improved')
#     }
#   }
#
#   # show the audiogram of the final candidate
#   filled.contour.modif2 (
#     x = seq(
#       1,
#       ncol(candSpec_log) * step,
#       length.out = ncol(candSpec_log)
#     ),
#     y = 1:nrow(candSpec_log),
#     z = t(candSpec_log),
#     levels = seq(0, 1, length = 30),
#     color.palette = function(x)
#       gray(seq(
#         from = 1,
#         to = 0,
#         length = x
#       ))
#   )
#
#   permittedValues_optimization = reset_defaults()
#   pars = pars_list[, 1]
#   names(pars) = rownames(pars_list)
#
#   return (list(history = output, pars = pars))
# }
#
#
#
#
#
#
#
# matchByColumn = function (matrix_short, len, padWith = 0) {
#   # adds columns of zeroes to a matrix (attaching them both left and right),
#   # so that the new number of columns = len
#   col_short = 1:ncol(matrix_short)
#   col_long = matchLengths(col_short, len, padDir = 'central', padWith =
#                             padWith) # pads with zeros/NA etc right and left
#   new = matrix(padWith,
#                nrow = nrow(matrix_short),
#                ncol = length(col_long))
#   colnames(new) = col_long
#   if (is.na(padWith)) {
#     new[, !is.na(colnames(new))] = matrix_short
#   } else {
#     new[, colnames(new) != padWith] = matrix_short
#     # paste the old matrix where it belongs, fill the rest with zeros, NA's or whatever
#   }
#   return (new)
# }
#
# spec_sim = function(spec1,
#                     spec2,
#                     padWith = NA,
#                     penalizeLengthDif = T) {
#   # make sure the number of columns (frames) is equal for comparing the two
#   # spectrograms by padding with zeroes (silence)
#   if (ncol(spec1) < ncol(spec2)) {
#     spec1 = matchByColumn(matrix_short = spec1,
#                           len = ncol(spec2),
#                           padWith = padWith)
#   } else if (ncol(spec1) > ncol(spec2)) {
#     spec2 = matchByColumn(matrix_short = spec2,
#                           len = ncol(spec1),
#                           padWith = padWith)
#   }
#
#   # correlate column by column
#   dist_by_column = data.frame(cor = rep(NA, ncol(spec1)), pixel = NA)
#   for (c in 1:ncol(spec1)) {
#     dist_by_column$cor[c] = cor(spec1[, c], spec2[, c], use = 'na.or.complete')
#     dist_by_column$pixel[c] = mean(abs(spec1[, c] - spec2[, c]))
#   }
#
#   if (penalizeLengthDif) {
#     out = apply(dist_by_column, 2, function(x) {
#       # if two sounds are of different length, distance goes up
#       sum(x, na.rm = TRUE) / length(x)
#     })
#   } else {
#     out = apply(dist_by_column, 2, function(x) {
#       # length difference has no effect on distance (e.g., it just tells us
#       # how well the shorter sound fits the middle part of the longer sound)
#       mean(x, na.rm = T)
#     })
#   }
#
#   return (as.numeric(-2.26 * out['cor'] + 1.21 * out['pixel']) / 3.47) # regression coefs from trying to predict dist_mat in Triplets (don't use interaction, since it's negative in the Triplets model, which leads to non-linear effects)
# }
# # spec_sim(spec_bank[[1]], spec_bank[[2]], padWith=NA, penalizeLengthDif=T)
#
# log01 = function(v) {
#   # takes a numeric vector, returns its log ranging from 0 to 1
#   v = v - min(v) + 1
#   v = v / max(v)
#   v = log(v)
#   v = v - min(v)
#   v = v / max(v)
#   return (v)
# }
#
# downsample = function(s, srNew = 10, srOld = 120){
#   if (!srNew < srOld){
#     return (s)
#   }
#   l = length(s)
#   dur = l / srOld
#   idx = seq(1, l, length.out = round(dur * srNew))
#   return (s[idx])
# }
