#' Morph sounds
#'
#' Takes two formulas for synthesizing two target sounds and produces a number of intermediate forms (morphs), attempting to go from one target sound to the other in a specified number of equal steps.
#' @param formula1,formula2 lists of parameters for calling \code{\link{soundgen}} that produce the two target sounds between which morphing will occur
#' @param nHybrids the length of morphing sequence, including target sounds
#' @param playMorphs if TRUE, the morphing sequence will be played
#' @param savePath if it is the path to an existing directory, morphs will be saved there as individual .wav files (defaults to NA)
#' @param samplingRate sampling rate of output (Hz)
#' @export
#' @return A list of two sublists ('formulas' and 'sounds'), each sublist of length nHybrids. For ex., the formula for the second hybrid is m$formulas[[2]], and the waveform is m$sounds[[2]]
#' @examples
#' # write or copy-paste two formulas from the app soundgen_app(), for example:
#' formula1 = list(sylLen=1190, rolloff=-24, rolloffAdjust_quadratic=20,
#'   rolloffAdjust_quadratic_nHarm=1, rolloff_noise=-13,
#'   subFreq=100, subDep=100,
#'   pitchAnchors=data.frame(time=c(0,.05,.25,.35,1), value=c(240,305,250,260,250)),
#'   noiseAnchors=data.frame(time=c(-50,1214), value=c(-50,-70)),
#'   mouthAnchors=data.frame(time=c(0,0.1,.4,1), value=c(0.18,0.5,.2,.1)),
#'   amplAnchors=data.frame(time=c(0,0.16,1), value=c(19,120,120)),
#'   exactFormants=list(f1=data.frame(time=c(0), freq=c(700),
#'   amp=c(30), width=c(80)), f2=data.frame(time=c(0), freq=c(900), amp=c(30),
#'   width=c(120)), f3=data.frame(time=c(0), freq=c(1500), amp=c(20),
#'   width=c(150))))
#' formula2 = list(sylLen=240, pitchEffects_amount=100,
#'   pitchEffects_intensity=50, attackLen=1, jitterDep=5.1, jitterLen=1,
#'   formantDep=1.3,
#'   extraFormants_stochastic=15, rolloff=-12,
#'   rolloffAdjust_per_octave=-6, rolloffAdjust_per_kHz=-6,
#'   rolloff_noise=-13, temperature=0.01,
#'   pitchAnchors=data.frame(time=c(0,0.17,1), value=c(383,421,358)),
#'   noiseAnchors=data.frame(time=c(0,49,256), value=c(-120,10,-120)),
#'   mouthAnchors=data.frame(time=c(0,0.15,1), value=c(0,0.49,0.2)),
#'   amplAnchors=data.frame(time=c(0,0.26,1), value=c(43,120,43)),
#'   exactFormants=list(f1=data.frame(time=c(0), freq=c(400), amp=c(40),
#'   width=c(120)), f2=data.frame(time=c(0), freq=c(1000), amp=c(40),
#'   width=c(120)), f3=data.frame(time=c(0), freq=c(1500), amp=c(30),
#'   width=c(150))))
#'
#'  m = morph (formula1, formula2, nHybrids=5, playMorphs=TRUE)
#'  # use m$formulas to access formulas for each morph, m$sounds for waveforms
#'  playme(m$sounds[[2]], 16000)
morph = function (formula1,
                  formula2,
                  nHybrids,
                  playMorphs = TRUE,
                  savePath = NA,
                  samplingRate = 16000) {
  # which pars are different from the defaults of soundgen()?
  notDefaultIdx_formula1 = which(apply(matrix(1:length(formula1)), 1, function(x) {
    identical(formula1[[x]], defaults[[names(formula1)[x]]]) == FALSE
    }))
  notDefaultIdx_formula2 = which(apply (matrix(1:length(formula2)), 1, function(x) {
    identical(formula2[[x]], defaults[[names(formula2)[x]]]) == FALSE
    }))
  # these pars have to be morphed:
  notDefaultNames = unique(c(names(formula1)[notDefaultIdx_formula1],
                             names(formula2)[notDefaultIdx_formula2]))
  notDefaultNames1 = names(formula1) [names(formula1) %in% notDefaultNames]
  notDefaultNames2 = names(formula2) [names(formula2) %in% notDefaultNames]

  # set up two formulas that contain the same number of pars, fill in with
  # specified values or, for values that are not specified, with defaults
  f1 = f2 = defaults[notDefaultNames]
  f1[notDefaultNames1] = formula1[match(notDefaultNames1, names(formula1))]
  f2[notDefaultNames2] = formula2[match(notDefaultNames2, names(formula2))]
  f2 = f2[match(names(f1), names(f2))]

  # fill in formant stuff if it is missing for one sound but defined for the other
  if (class(f1$exactFormants) != 'list' &
      class(f2$exactFormants) == 'list') {
    # class list means it's not NA or NULL
    f1$exactFormants = f2$exactFormants
    for (f in 1:length(f1$exactFormants))
      f1$exactFormants[[f]]$amp = 0
  }
  if (class(f1$exactFormants_noise) != 'list' &
      class(f2$exactFormants_noise) == 'list') {
    f1$exactFormants_noise = f2$exactFormants_noise
    for (f in 1:length(f1$exactFormants_noise))
      f1$exactFormants_noise[[f]]$amp = 0
  }
  if (class(f2$exactFormants) != 'list' &
      class(f1$exactFormants) == 'list') {
    f2$exactFormants = f1$exactFormants
    for (f in 1:length(f2$exactFormants))
      f2$exactFormants[[f]]$amp = 0
  }
  if (class(f2$exactFormants_noise) != 'list' &
      class(f1$exactFormants_noise) == 'list') {
    f2$exactFormants_noise = f1$exactFormants_noise
    for (f in 1:length(f2$exactFormants_noise))
      f2$exactFormants_noise[[f]]$amp = 0
  }

  # f1 and f2 are now fully prepared: two target formulas,
  # both of the same length, including the same pars, in the same order
  m = f1
  formulas = rep(list(f1), nHybrids)
  sounds = list()
  for (p in 1:length(f1)) {
    # morph each element of the formula list according to its class
    if (class(f1[[p]]) == 'numeric') {
      m[[p]] = seq(f1[[p]], f2[[p]], length.out = nHybrids)
    } else if (class(f1[[p]]) == 'data.frame') {
      m[[p]] = morphDF(f1[[p]], f2[[p]], nHybrids = nHybrids)
    } else if (class(f1[[p]]) == 'list') {
      m[[p]] = morphList(f1[[p]], f2[[p]], nHybrids = nHybrids)
    }
    # a convoluted way of saving the output of morphFormants() in appropriate
    # slots in the output list
    for (h in 1:nHybrids) {
      formulas[[h]] [[p]] = m[[p]] [[h]]
    }
  }

  for (h in 1:nHybrids) {
    sounds[[h]] = do.call(soundgen, formulas[[h]])
    if (playMorphs) playme(sounds[[h]], samplingRate = samplingRate)
    if (!is.na(savePath)){
      filename = paste0(savePath, 'morph_', h, '.wav')
      seewave::savewav(sounds[[h]], f = samplingRate, filename = filename)
    }
  }
  return (list(formulas = formulas, sounds = sounds))
}
