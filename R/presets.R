###
### D E F A U L T   P A R   V A L U E S   &   P R E S E T S
###

# devtools::use_data(permittedValues, presets, overwrite = TRUE)

# global pars and constants
# samplingRate = 16000 # synthesize at ... points/s # set from UI, defaults to 16000
windowLength_points = 512 # the length of fft window
overlap = 75 # overlap of fft windows
throwaway_dB = -120 # discard harmonics and noise that is quieter than this
speedSound = 35400 # cm/s, in warm air. Stevens (2000) "Acoustic phonetics", p. 138
# amplBoost_openMouth_dB = 0
randomWalk_trendStrength = .5 # try 0 to 1 - the higher, the more likely rw is to get high in the middle and low at the beginning and end (ie max effect amplitude in the middle of a sound)
defaultVowel = 'a'
# pitch_floor = 50
# pitch_ceiling = 3500
formantDrift_per_temp=.3 # the higher, the more formants drift at a given temperature
formantDispersion_per_temp=.2 # the higher, the more unevenly stochastic formants are spaced at a given temperature
pitchDrift_per_temp=.5 # the higher, the further pitch drifts (wiggles)
pitchDriftWiggle_per_temp=.125 # the higher, the faster pitch drifts (wiggles)
pitchAnchorsWiggle_per_temp = .05
noiseAnchorsWiggle_per_temp = .1
amplAnchorsWiggle_per_temp = .1
smoothLinear_factor = 1 # (0 to +Inf): regulates smoothing of formant anchors in function getSpectralEnvelope(). 0 = close to spline; >3 = approches linear extrapolation
rolloff_per_ampl = 12 # as amplitude goes down from max to throwaway_dB, rolloff increases by rolloff_per_ampl dB/octave. The effect is to make loud parts brighter by increasing energy in higher frequencies
# dictionaries for getBinaryRandomWalk() to calculate and load once at the beginning of each session
slope_q1 = -.1
midpoint_q1 = 33
slope_q2 = -.1
midpoint_q2 = 66

noiseThresholds_dict = list(pitchEffects_amount = 0:100, q1=NA, q2=NA)
noiseThresholds_dict$q1 = 100 / (1 + exp(-slope_q1*(noiseThresholds_dict$pitchEffects_amount-midpoint_q1)))
noiseThresholds_dict$q2 = 100 / (1 + exp(-slope_q2*(noiseThresholds_dict$pitchEffects_amount-midpoint_q2)))
# plot (noiseThresholds_dict$pitchEffects_amount, noiseThresholds_dict$q1, type='l', col='red')
# points (noiseThresholds_dict$pitchEffects_amount, noiseThresholds_dict$q2, type='l', col='blue')



#' Defaults and ranges
#'
#' A dataset containing defaults and ranges of key variables in the Shiny app.
#' Adjust as needed.
#'
#' @format A matrix with 58 rows and 4 variables:
#' \describe{
#'   \item{default}{default value}
#'   \item{low}{lowest permitted value}
#'   \item{high}{highest permitted value}
#'   \item{step}{increment for adjustment}
#'   ...
#' }
"permittedValues"
permittedValues = matrix (c(
  # sliderInput's to be reset for each callType
  'repeatBout', 1, 1, 20, 1,
  'nSyl', 1, 1, 10, 1, # default, low, high, step
  'sylLen', 300, 20, 5000, 10, # for one syllable
  'pauseLen', 200, 20, 1000, 10,
  'temperature', .025, 0, 1, .025,
  'maleFemale', 0, -1, 1, 0.1,
  'creakyBreathy', 0, -1, 1, 0.1,
  'pitchEffects_amount', 0, 0, 100, 1,
  'pitchEffects_intensity', 50, 0, 100, 1,
  'jitterDep', 3, 0, 24, 0.1,
  'jitterLen', 1, 1, 100, 1,
  'vibratoFreq', 5, 3, 10, .5,   # Horii, 1989
  'vibratoDep', 0, 0, 3, 0.125,
  'shimmerDep', 0, 0, 100, 1,
  'attackLen', 50, 1, 200, 10,
  'rolloff', -12, -60, 0, 1,
  'rolloffAdjust_per_octave', -12, -30, 10, 1,
  'rolloffAdjust_quadratic', 0, -50, 50, 5,
  'rolloffAdjust_quadratic_nHarm', 3, 1, 20, 1,
  'rolloffAdjust_per_kHz', -6, -20, 0, 1,
  'rolloff_lipRad', 6, 0, 20, 1,
  'formantDep', 1, 0, 5, .1,
  'extraFormants_stochastic', 30, 0, 60, 10,
  'vocalTract', 15.5, 2, 100, .5,
  'subFreq', 100, 10, 1000, 10,
  'subDep', 100, 0, 500, 10,
  'shortestEpoch', 300, 50, 500, 25,
  'trillDep', 0, 0, 1, .1,
  'trillFreq', 30, 10, 100, 5,
  'rolloff_noise', -14, -20, 20, 1,

  # other settings
  'spec_windowLength', 40, 5, 100, 2.5, # default, low, high, step
  'spec_contrast', .2, -1, 1, .05,
  'spec_brightness', 0, -1, 1, .05,
  'mouthOpening', .5, 0, 1, .05,
  'pitch', 100, 25, 3500, 1,  # set per species
  'pitchDeltas', 0, -24, 24, 1,
  'time', 0, 0, 5000, 1,
  'noise_ampl', 0, throwaway_dB, 40, 1
), ncol=5, byrow=TRUE)
temp = permittedValues[,1]
permittedValues = apply (permittedValues[,2:5], 2, as.numeric)
colnames(permittedValues) = c('default', 'low', 'high', 'step')
rownames(permittedValues) = temp


# a list of default values for Shiny app - mostly the same as for
# soundgen(). NB: if defaults change, this has to be updated!!!
defaults = list(
  repeatBout = 1,
  nSyl = 1,
  sylLen = 300,
  pauseLen = 200,
  temperature = 0.025,
  maleFemale = 0,
  creakyBreathy = 0,
  pitchEffects_amount = 0,
  pitchEffects_intensity = 50,
  jitterDep = 3,
  jitterLen = 1,
  vibratoFreq = 5,
  vibratoDep = 0,
  shimmerDep = 0,
  attackLen = 50,
  rolloff = -12,
  rolloffAdjust_per_octave = -12,
  rolloffAdjust_quadratic = 0,
  rolloffAdjust_quadratic_nHarm = 3,
  rolloffAdjust_per_kHz = -6,
  rolloff_lipRad = 6,
  formantDep = 1,
  extraFormants_stochastic = 30,
  vocalTract = 15.5,
  subFreq = 100,
  subDep = 100,
  shortestEpoch = 300,
  trillDep = 0,
  trillFreq = 30,
  rolloff_noise = -12,
  samplingRate = 16000,
  windowLength_points = 2048,
  overlap = 75,
  addSilence = 100,
  pitch_floor = 25,
  pitch_ceiling = 3500,
  pitch_samplingRate = 3500,
  pitchAnchors = list(
    time = c(0, .1, .9, 1),
    value = c(100, 150, 135, 100)
  ),
  pitchAnchors_global = list(time = c(0, 1), value = c(0, 0)),
  noiseAnchors = list(time = c(0, 300), value = c(-120, -120)),
  mouthAnchors = list(time = c(0, 1), value = c(.5, .5)),
  amplAnchors = list(time = c(0, 1), value = c(120, 120)),
  amplAnchors_global = list(time = c(0, 1), value = c(120, 120)),
  exactFormants = list(f1 = list(time = 0, freq = 860,
                                 amp = 30, width = 120),
                       f2 = list(time = 0, freq = 1280,
                                 amp = 40, width = 120),
                       f3 = list(time = 0, freq = 2900,
                                 amp = 25, width = 200)),
  exactFormants_noise = NA
)


# -------------------------------------------------------------
# A library of presets for easy generation of a few nice sounds
# -------------------------------------------------------------
#' Presets
#'
#' A library of presets for easy generation of a few nice sounds.
#'
#' @format A list of length 4.
"presets"
presets = list(
  M1 = list(
    Vowel = 'soundgen()', # these are just the global defaults

    Gasp = 'soundgen(sylLen = 280, pitchAnchors = list(time = c(0, 0.7, 1), value = c(402, 556, 490)), temperature = 0.05, rolloffAdjust_per_octave = -8, rolloffAdjust_per_kHz = -15, exactFormants = list(f1 = list(time = 0, freq = 420, amp = 20, width = 150), f2 = list(time = 0, freq = 1200, amp = 50, width = 250), f3 = list(time = 0, freq = 5000, amp = 10, width = 200), f4 = list(time = 0, freq = 8500, amp = 10, width = 300)), subDep = 0, noiseAnchors = list(time = c(1, 234, 408), value = c(-120, -32, -120)), exactFormants_noise = list(f1 = list(time = 0, freq = 420, amp = 20, width = 150), f2 = list(time = 0, freq = 1200, amp = 50, width = 250), f3 = list(time = 0, freq = 5000, amp = 10, width = 200), f4 = list(time = 0, freq = 8500, amp = 10, width = 300)), rolloff_noise=-5, mouthAnchors = list(time = c(0, 0.3, 1), value = c(0.32, 0.88, 0.42)))',

    Roar = 'soundgen(sylLen = 960, pitchAnchors = list(time = c(0, 0.14, 0.88, 1), value = c(174, 200, 191, 131)), temperature = 0.05, pitchEffects_amount = 70, jitterDep = 0.7, rolloff = -20, rolloffAdjust_per_octave = -3, rolloffAdjust_quadratic = -10, rolloffAdjust_quadratic_nHarm = 13, exactFormants = list(f1 = list(time = 0, freq = 620, amp = 40, width = 80), f2 = list(time = 0, freq = 1000, amp = 50, width = 80), f3 = list(time = 0, freq = 1800, amp = 40, width = 200), f4 = list(time = 0, freq = 2560, amp = 50, width = 300), f5 = list(time = 0, freq = 3400, amp = 35, width = 300)), subDep = 40, noiseAnchors = list(time = c(0, 960), value = c(-120, -120)), exactFormants_noise = list(f1 = list(time = 0, freq = 620, amp = 40, width = 80), f2 = list(time = 0, freq = 1000, amp = 50, width = 80), f3 = list(time = 0, freq = 1800, amp = 40, width = 200), f4 = list(time = 0, freq = 2560, amp = 50, width = 300), f5 = list(time = 0, freq = 3400, amp = 35, width = 300)), mouthAnchors = list(time = c(0, 0.14, 0.88, 1), value = c(0.4, 0.59, 0.61, 0.45)))',

    Moan = 'soundgen(sylLen = 800, pitchAnchors = list(time = c(0, 1), value = c(202, 144)), attackLen = 100, rolloff = -23, rolloffAdjust_per_octave = -6, exactFormants = list(f1 = list(time = 0, freq = 980, amp = 35, width = 120), f2 = list(time = 0, freq = 1400, amp = 35, width = 150), f3 = list(time = 0, freq = 2680, amp = 30, width = 200), f4 = list(time = 0, freq = 3400, amp = 30, width = 200), f5 = list(time = 0, freq = 4150, amp = 30, width = 400)), noiseAnchors = list(time = c(-7, 268, 590, 869), value = c(-67, -45, -24, -46)), exactFormants_noise = list(f1 = list(time = 0, freq = 980, amp = 35, width = 120), f2 = list(time = 0, freq = 1400, amp = 35, width = 150), f3 = list(time = 0, freq = 2680, amp = 30, width = 200), f4 = list(time = 0, freq = 3400, amp = 30, width = 200), f5 = list(time = 0, freq = 4150, amp = 30, width = 400)), rolloff_noise = -10, mouthAnchors = list(time = c(0, 0.3, 1), value = c(0.24, 0.41, 0.19)))',

    Sigh = 'soundgen(sylLen = 50, pitchAnchors = list(time = c(0, 1), value = c(202, 144)), temperature = 0.1, exactFormants = list(f1 = list(time = 0, freq = 800, amp = 30, width = 100), f1.5 = list(time = 0, freq = 1200, amp = 30, width = 100), f2 = list(time = 0, freq = 1500, amp = 30, width = 100), f3 = list(time = 0, freq = 2600, amp = 30, width = 100), f4 = list(time = 0, freq = 4000, amp = 30, width = 100)), noiseAnchors = list(time = c(-20, 104, 756, 1252), value = c(26, 40, 23, -22)))',

    Laugh = 'soundgen(nSyl = 5, sylLen = 120, pauseLen = 120, pitchAnchors = list(time = c(0, 1), value = c(180, 166)), pitchAnchors_global = list(time = c(0, 0.3, 1), value = c(1, 2, 0)), temperature = 0.1, pitchEffects_amount = 100, jitterDep = 0.8, attackLen = 10, vowelString = "ae", subDep = 0, noiseAnchors = list(time = c(39, 56, 167), value = c(-120, -44, -120)), exactFormants_noise = list(f1 = list(time = c(0, 1), freq = c(860, 530), amp = c(30, 30), width = c(120, 50)), f1.4 = list(time = c(0, 1), freq = c(1100, 1100), amp = c(0, -20), width = c(100, 100)), f1.6 = list(time = c(0, 1), freq = c(1400, 1400), amp = c(0, 20), width = c(100, 100)), f2 = list(time = c(0, 1), freq = c(1280, 2400), amp = c(40, 40), width = c(120, 300)), f3 = list(time = c(0, 1), freq = c(2900, 4000), amp = c(25, 30), width = c(200, 300)), f4 = list(time = c(0, 1), freq = c(3800, 3800), amp = c(20, 0), width = c(100, 100))), rolloff_noise = -5, amplAnchors = list(time = c(0, 0.4, 1), value = c(120, 0, 120)), amplAnchors_global = list(time = c(0, 0.55, 1), value = c(120, 110, 20)))',

    Snore = 'soundgen(sylLen = 960, pitchAnchors = list(time = c(0, 0.15, 0.87, 1), value = c(175, 199, 188, 140)), temperature = 0.05, pitchEffects_amount = 67, jitterDep = 0.75, exactFormants = list(f1 = list(time = 0, freq = 560, amp = 30, width = 120), f2 = list(time = 0, freq = 1000, amp = 40, width = 120), f3 = list(time = 0, freq = 1450, amp = 25, width = 200), f4 = list(time = 0, freq = 3800, amp = 20, width = 100)), subDep = 80, shortestEpoch = 200, noiseAnchors = list(time = c(0, 960), value = c(-40, -40)), exactFormants_noise = list(f1 = list(time = 0, freq = 560, amp = 30, width = 120), f2 = list(time = 0, freq = 1000, amp = 40, width = 120), f3 = list(time = 0, freq = 1450, amp = 25, width = 200), f4 = list(time = 0, freq = 3800, amp = 20, width = 100)), rolloff_noise = -12, mouthAnchors = list(time = c(0, 1), value = c(0, 0)), amplAnchors_global = list(time = c(0, 1), value = c(120, 43)))',

    # 'Formants' is a reserved name. The list of presets for every caller should end with
    # a list of 'Formants' presets for each vowel and consonant, otherwise you won't be
    # able to specify formants in a string like 'aui' for this speaker
    Formants = list(
      # vowels
      'a' = list(
        'f1'=list(time=0, freq=860, amp=30, width=120), # amplitude in dB, freq and width in Hz
        'f2'=list(time=0, freq=1280, amp=40, width=120),
        'f3'=list(time=0, freq=2900, amp=25, width=200) # any number of formants may be specified
      ),
      'o' = list(
        'f1'=list(time=0, freq=630, amp=35, width=100),
        'f2'=list(time=0, freq=900, amp=35, width=100),
        'f3'=list(time=0, freq=3000, amp=30, width=200),
        'f4'=list(time=0, freq=3960, amp=30, width=200)
      ),
      'i' = list(
        'f1'=list(time=0, freq=300, amp=25, width=80),
        'f2'=list(time=0, freq=2700, amp=30, width=100),
        'f3'=list(time=0, freq=3400, amp=40, width=350),
        'f4'=list(time=0, freq=4200, amp=40, width=350)
      ),
      'e' = list(
        'f1'=list(time=0, freq=530, amp=30, width=50),
        'f1.4'=list(time=0, freq=1100, amp=-20, width=100), # insert a zero-pole pair between F1 and F2
        'f1.6'=list(time=0, freq=1400, amp=20, width=100),  # insert a zero-pole pair between F1 and F2
        'f2'=list(time=0, freq=2400, amp=40, width=300),
        'f3'=list(time=0, freq=4000, amp=30, width=300)
      ),
      'u' = list(
        'f1'=list(time=0, freq=375, amp=25, width=80),
        'f2'=list(time=0, freq=550, amp=35, width=120),
        'f3'=list(time=0, freq=2100, amp=25, width=300),
        'f4'=list(time=0, freq=4200, amp=45, width=250)
      ),
      '0' = list(  # schwa
        'f1'=list(time=0, freq=640, amp=30, width=100),
        'f2'=list(time=0, freq=1670, amp=30, width=100),
        'f3'=list(time=0, freq=2700, amp=30, width=100),
        'f4'=list(time=0, freq=3880, amp=30, width=100)
      ),
      # consonants
      'h' = list(  # sh
        'rolloff_noise'=-13,
        f1=list(time=0, freq=420, amp=20, width=150),
        f2=list(time=0, freq=1200, amp=50, width=250),
        f3=list(time=0, freq=5000, amp=10, width=200),
        f4=list(time=0, freq=8500, amp=10, width=300)
      ),
      's' = list(
        'rolloff_noise'=0,
        'f1'=list(time=0, freq=5500, amp=20, width=200),
        'f2'=list(time=0, freq=7000, amp=30, width=1000),
        'f3'=list(time=0, freq=9000, amp=30, width=1000)
      ),
      'x' = list(  # sh
        'rolloff_noise'=-9,
        'f1'=list(time=0, freq=1700, amp=15, width=80),
        'f2'=list(time=0, freq=2600, amp=30, width=300),
        'f3'=list(time=0, freq=3400, amp=25, width=200),
        'f4'=list(time=0, freq=4800, amp=10, width=300)
      ),
      'f' = list(
        'rolloff_noise'=-10,
        'f1'=list(time=0, freq=1400, amp=30, width=200),
        'f2'=list(time=0, freq=2000, amp=10, width=80),
        'f3'=list(time=0, freq=2900, amp=20, width=1000)
      ),
      'n' = list( # sNuffle (breathing through the nose)
        'rolloff_noise'=0,
        'f1'=list(time=0, freq=5400, amp=25, width=2000)
      )
    )
  ),

  F1 = list(
    'Vowel' = 'soundgen(sylLen = 500, pitchAnchors = list(time = c(0, 0.6, 1), value = c(340, 370, 340)), exactFormants = list(f1 = list(time = 0, freq = 900, amp = 30, width = 80), f2 = list(time = 0, freq = 1300, amp = 30, width = 160), f3 = list(time = 0, freq = 3300, amp = 25, width = 130), f4 = list(time = 0, freq = 4340, amp = 20, width = 370)))',

    Scream = 'soundgen(sylLen = 1110, pitchAnchors = list(time = c(0, 0.1, 0.85, 1), value = c(900, 1832, 1618, 1200)), temperature = 0.1, pitchEffects_amount = 70, jitterDep = 1, shimmerDep = 10, exactFormants = list(f1 = list(time = 0, freq = 900, amp = 30, width = 80), f2 = list(time = 0, freq = 1300, amp = 30, width = 160), f3 = list(time = 0, freq = 3300, amp = 25, width = 130), f4 = list(time = 0, freq = 4340, amp = 20, width = 370)), subFreq = 400, noiseAnchors = list(time = c(0, 1110), value = c(-120, -120)))',

    Growl = 'soundgen(sylLen = 1100, pitchAnchors = list(time = c(0, 0.1, 0.3, 1), value = c(238, 251, 449, 205)), temperature = 0.1, rolloff = -15, rolloffAdjust_per_octave = -6, rolloffAdjust_quadratic = -20, rolloffAdjust_quadratic_nHarm = 20, exactFormants = list(f1 = list(time = 0, freq = 800, amp = 20, width = 120), f2 = list(time = 0, freq = 2150, amp = 30, width = 300), f3 = list(time = 0, freq = 4250, amp = 30, width = 300), f4 = list(time = 0, freq = 6600, amp = 30, width = 300)), trillDep = 0.6, trillFreq = 60, noiseAnchors = list(time = c(0, 1100), value = c(-120, -120)), exactFormants_noise = list(f1 = list(time = 0, freq = 800, amp = 20, width = 120), f2 = list(time = 0, freq = 2150, amp = 30, width = 300), f3 = list(time = 0, freq = 4250, amp = 30, width = 300), f4 = list(time = 0, freq = 6600, amp = 30, width = 300)), mouthAnchors = list(time = c(0, 0.13, 0.76, 1), value = c(0.4, 0.59, 0.61, 0.45)), amplAnchors_global = list(time = c(0, 0.13, 1), value = c(101, 120, 26)))',

    Moan = 'soundgen(sylLen = 360, pitchAnchors = list(time = c(0, 1), value = c(380, 260)), attackLen = 100, rolloff = -20, rolloffAdjust_per_octave = -6, exactFormants = list(f1 = list(time = c(0, 0.5, 1), freq = c(900, 900, 790), amp = c(30, 30, 30), width = c(80, 80, 100)), f2 = list(time = c(0, 0.5, 1), freq = c(1300, 1300, 1600), amp = c(30, 30, 30), width = c(160, 160, 100)), f3 = list(time = c(0, 0.5, 1), freq = c(3300, 3300, 3100), amp = c(25, 25, 30), width = c(130, 130, 100)), f4 = list(time = c(0, 0.5, 1), freq = c(4340, 4340, 3900), amp = c(20, 20, 30), width = c(370, 370, 100))), noiseAnchors = list(time = c(0, 417, 508), value = c(-63, -29, -120)), exactFormants_noise = list(f1 = list(time = c(0, 0.5, 1), freq = c(900, 900, 790), amp = c(30, 30, 30), width = c(80, 80, 100)), f2 = list(time = c(0, 0.5, 1), freq = c(1300, 1300, 1600), amp = c(30, 30, 30), width = c(160, 160, 100)), f3 = list(time = c(0, 0.5, 1), freq = c(3300, 3300, 3100), amp = c(25, 25, 30), width = c(130, 4340, 3900), amp = c(20, 20, 30), width = c(370, 370, 100))), mouthAnchors = list(time = c(0, 0.14, 1), value = c(0.24, 0.41, 0.19)), amplAnchors_global = list(time = c(0, 1), value = c(120, 18)))',

    Laugh = 'soundgen(nSyl = 3, sylLen = 60, pauseLen = 90, pitchAnchors = list(time = c(0, 1), value = c(368, 284)), temperature = 0.075, attackLen = 10, exactFormants = list(f1 = list(time = 0, freq = 790, amp = 30, width = 100), f2 = list(time = 0, freq = 1600, amp = 30, width = 100), f3 = list(time = 0, freq = 3100, amp = 30, width = 100), f4 = list(time = 0, freq = 3900, amp = 30, width = 100)), noiseAnchors = list(time = c(0, 67, 86, 186), value = c(-45, -47, -89, -120)), exactFormants_noise = list(f1 = list(time = 0, freq = 790, amp = 30, width = 100), f2 = list(time = 0, freq = 1600, amp = 30, width = 100), f3 = list(time = 0, freq = 3100, amp = 30, width = 100), f4 = list(time = 0, freq = 3900, amp = 30, width = 100)), rolloff_noise = -8, amplAnchors_global = list(time = c(0, 1), value = c(120, 20)))',

    Cry = 'soundgen(sylLen = 1600, pitchAnchors = list(time = c(0, 1), value = c(610, 511)), temperature = 0.2, pitchEffects_amount = 15, exactFormants = list(f1 = list(time = 0, freq = 790, amp = 30, width = 100), f2 = list(time = 0, freq = 1600, amp = 30, width = 100), f3 = list(time = 0, freq = 3100, amp = 30, width = 100), f4 = list(time = 0, freq = 3900, amp = 30, width = 100)), subFreq = 125, subDep = 70, noiseAnchors = list(time = c(0, 1600), value = c(-120, -120)), rolloff_noise = 0, mouthAnchors = list(time = c(0, 1), value = c(0, 0)), amplAnchors_global = list(time = c(0, 1), value = c(120, 60)))',

    Formants = list( # reserved name - the list of presets for every caller must end with a list of 'Formants' presets for each vowel and consonant
      # vowels
      'a' = list(
        'f1'=list(time=0, freq=900, amp=30, width=80), # amplitude in dB, freq and width in Hz
        'f2'=list(time=0, freq=1300, amp=30, width=160),
        'f3'=list(time=0, freq=3300, amp=25, width=130),
        'f4'=list(time=0, freq=4340, amp=20, width=370) # any number of formants may be specified
      ),
      'o' = list(
        'f1'=list(time=0, freq=800, amp=30, width=80),
        'f2'=list(time=0, freq=1100, amp=30, width=80),
        'f3'=list(time=0, freq=3560, amp=40, width=200),
        'f4'=list(time=0, freq=5830, amp=50, width=200)
      ),
      'i' = list(
        'f1'=list(time=0, freq=330, amp=30, width=120),
        'f2'=list(time=0, freq=2700, amp=40, width=120),
        'f3'=list(time=0, freq=3580, amp=30, width=200),
        'f4'=list(time=0, freq=4710, amp=30, width=200),
        'f5'=list(time=0, freq=5800, amp=30, width=200)
      ),
      'e' = list(
        'f1'=list(time=0, freq=930, amp=30, width=100),
        'f2'=list(time=0, freq=2470, amp=30, width=100),
        'f3'=list(time=0, freq=3300, amp=25, width=120),
        'f4'=list(time=0, freq=4200, amp=30, width=200)
      ),
      'u' = list(
        'f1'=list(time=0, freq=450, amp=30, width=80),
        'f2'=list(time=0, freq=850, amp=40, width=120),
        'f3'=list(time=0, freq=2900, amp=30, width=200),
        'f4'=list(time=0, freq=4100, amp=30, width=275)
      ),
      '0' = list(  # schwa
        'f1'=list(time=0, freq=790, amp=30, width=100),
        'f2'=list(time=0, freq=1600, amp=30, width=100),
        'f3'=list(time=0, freq=3100, amp=30, width=100),
        'f4'=list(time=0, freq=3900, amp=30, width=100)
      ),
      # consonants
      'h' = list(  # sh
        'rolloff_noise'=-13,
        f1=list(time=0, freq=420, amp=20, width=150),
        f2=list(time=0, freq=1200, amp=50, width=250),
        f3=list(time=0, freq=5000, amp=10, width=200),
        f4=list(time=0, freq=8500, amp=10, width=300)
      ),
      's' = list(
        'rolloff_noise'=0,
        'f1'=list(time=0, freq=5500, amp=20, width=200), # NB: amplitude in dB for consonants
        'f2'=list(time=0, freq=7000, amp=30, width=1000),
        'f3'=list(time=0, freq=9000, amp=30, width=1000)
      ),
      'x' = list(  # sh
        'rolloff_noise'=-9,
        'f1'=list(time=0, freq=1700, amp=15, width=80),
        'f2'=list(time=0, freq=2600, amp=30, width=300),
        'f3'=list(time=0, freq=3400, amp=25, width=200),
        'f4'=list(time=0, freq=4800, amp=10, width=300)
      ),
      'f' = list(
        'rolloff_noise'=-10,
        'f1'=list(time=0, freq=1400, amp=30, width=200),
        'f2'=list(time=0, freq=2000, amp=10, width=80),
        'f3'=list(time=0, freq=2900, amp=20, width=1000)
      ),
      'n' = list( # sNuffle (breathing through the nose)
        'rolloff_noise'=0,
        'f1'=list(time=0, freq=5400, amp=25, width=2000)
      )
    )
  ),

  Chimpanzee = list(
    Bark_alarm = 'soundgen(sylLen = 160, pitchAnchors = list(time = c(0, 1), value = c(232, 185)), pitchEffects_amount = 100, jitterDep = 4.9, attackLen = 61, rolloff = -30, rolloffAdjust_per_octave = 0, exactFormants = list(f1 = list(time = 0, freq = 415, amp = 60, width = 120), f2 = list(time = 0, freq = 1000, amp = 60, width = 120), f3 = list(time = 0, freq = 3000, amp = 20, width = 200), f4 = list(time = 0, freq = 5000, amp = 20, width = 1000)), subFreq = 125, subDep = 90, noiseAnchors = list(time = c(0, 76, 158, 344), value = c(-120, 16, -21, -120)), exactFormants_noise = list(f1 = list(time = 0, freq = 415, amp = 60, width = 120), f2 = list(time = 0, freq = 1000, amp = 60, width = 120), f3 = list(time = 0, freq = 3000, amp = 20, width = 200), f4 = list(time = 0, freq = 5000, amp = 20, width = 1000)), rolloff_noise = -14)',

    Scream_conflict = 'soundgen(sylLen = 740, pitchAnchors = list(time = c(0, 0.3, 0.9, 1), value = c(1200, 1547, 1487, 1154)), temperature = 0.05, pitchEffects_amount = 100, jitterDep = 0.3, rolloff = -6, rolloffAdjust_per_octave = 0, exactFormants = list(f1 = list(time = 0, freq = 800, amp = 30, width = 80), f2 = list(time = 0, freq = 1600, amp = 30, width = 160), f3 = list(time = 0, freq = 3000, amp = 25, width = 130), f4 = list(time = 0, freq = 4340, amp = 20, width = 370)), subFreq = 75, subDep = 130, noiseAnchors = list(time = c(0, 740), value = c(-120, -120)), exactFormants_noise = list(f1 = list(time = 0, freq = 800, amp = 30, width = 80), f2 = list(time = 0, freq = 1600, amp = 30, width = 160), f3 = list(time = 0, freq = 3000, amp = 25, width = 130), f4 = list(time = 0, freq = 4340, amp = 20, width = 370)), rolloff_noise = -12)',

    Grunt_excited = 'soundgen(nSyl = 6, sylLen = 100, pauseLen = 220, pitchAnchors = list(time = c(0, 1), value = c(127, 102)), temperature = 0.05, rolloffAdjust_per_octave = -6, exactFormants = list(f1 = list(time = c(0, 1), freq = c(415, 300), amp = c(60, 60), width = c(120, 120)), f2 = list(time = c(0, 1), freq = c(1000, 500), amp = c(60, 60), width = c(120, 120)), f3 = list(time = c(0, 1), freq = c(2200, 2500), amp = c(20, 20), width = c(200, 200)), f4 = list(time = 0, freq = 3600, amp = 20, width = 200), f5 = list(time = c(0, 1), freq = c(4600, 4200), amp = c(20, 20), width = c(200, 200))), noiseAnchors = list(time = c(0, 83, 205), value = c(-120, -6, -120)), rolloff_noise = -12, amplAnchors_global = list(time = c(0, 0.35, 1), value = c(89, 120, 31)))',  # TODO: CHECK BREATHING!!!

    Hoot_excited = 'soundgen(sylLen = 730, pitchAnchors = list(time = c(0, 0.52, 1), value = c(440, 405, 440)), rolloff = -20, rolloffAdjust_per_octave = 0, exactFormants = list(f1 = list(time = c(0, 0.4, 1), freq = c(300, 500, 400), amp = c(30, 30, 30), width = c(80, 80, 80)), f2 = list(time = c(0, 0.2, 1), freq = c(500, 1000, 700), amp = c(30, 30, 30), width = c(120, 120, 120)), f3 = list(time = 0, freq = 2500, amp = 30, width = 120), f4 = list(time = 0, freq = 4000, amp = 30, width = 200), f5 = list(time = 0, freq = 5580, amp = 30, width = 200)), noiseAnchors = list(time = c(0, 730), value = c(-8, -8)), rolloff_noise = -13)',

    Laugh_playing = 'soundgen(nSyl = 5, sylLen = 100, pauseLen = 150, pitchAnchors = list(time = c(0, 0.5, 1), value = c(134, 144, 117)), rolloff = -22, rolloffAdjust_per_octave = 0, rolloffAdjust_quadratic = -20, rolloffAdjust_quadratic_nHarm = 8, exactFormants = list(f1 = list(time = 0, freq = 300, amp = 30, width = 80), f2 = list(time = 0, freq = 950, amp = 30, width = 120), f3 = list(time = 0, freq = 1600, amp = 40, width = 120), f4 = list(time = 0, freq = 2240, amp = 25, width = 200), f5 = list(time = 0, freq = 2900, amp = 20, width = 100)), noiseAnchors = list(time = c(20, 60, 100), value = c(-120, -19, -120)), exactFormants_noise = list(f1 = list(time = 0, freq = 300, amp = 30, width = 80), f2 = list(time = 0, freq = 950, amp = 30, width = 120), f3 = list(time = 0, freq = 1600, amp = 40, width = 120), f4 = list(time = 0, freq = 2240, amp = 25, width = 200), f5 = list(time = 0, freq = 2900, amp = 20, width = 100)), rolloff_noise = -16)',

    Formants = list( # reserved name - the list of presets for every caller must end with a list of 'Formants' presets for each vowel and consonant
      # ...
    )
  ),

  Misc = list(
    Cat_miaw = 'soundgen(sylLen = 1300, pitchAnchors = list(time = c(0, 0.25, 1), value = c(264, 315, 274)), temperature = 0.05, rolloff = -6, rolloffAdjust_per_octave = -6, rolloffAdjust_quadratic = 20, rolloffAdjust_quadratic_nHarm = 20, exactFormants = list(f1 = list(time = c(0, 0.15, 1), freq = c(1540, 1900, 1020), amp = c(20, 20, 20), width = c(200, 200, 200)), f2 = list(time = c(0, 0.15, 1), freq = c(1800, 2900, 2800), amp = c(20, 20, 20), width = c(200, 200, 200)), f2.5 = list(time = c(0, 0.15, 1), freq = c(2500, 3700, 3400), amp = c(-20, -20, -20), width = c(400, 400, 400)), f3 = list(time = c(0, 0.15, 1), freq = c(3200, 5300, 5200), amp = c(20, 20, 20), width = c(400, 400, 400)), f4 = list(time = c(0, 0.15, 1), freq = c(6000, 6200, 6000), amp = c(20, 20, 20), width = c(400, 400, 400))), vocalTract = 8, noiseAnchors = list(time = c(-44, 1200, 1389, 1594), value = c(-55, -32, 10, -78)), exactFormants_noise = list(f1 = list(time = c(0, 0.15, 1), freq = c(1540, 1900, 1020), amp = c(20, 20, 20), width = c(200, 200, 200)), f2 = list(time = c(0, 0.15, 1), freq = c(1800, 2900, 2800), amp = c(20, 20, 20), width = c(200, 200, 200)), f2.5 = list(time = c(0, 0.15, 1), freq = c(2500, 3700, 3400), amp = c(-20, -20, -20), width = c(400, 400, 400)), f3 = list(time = c(0, 0.15, 1), freq = c(3200, 5300, 5200), amp = c(20, 20, 20), width = c(400, 400, 400)), f4 = list(time = c(0, 0.15, 1), freq = c(6000, 6200, 6000), amp = c(20, 20, 20), width = c(400, 400, 400))), rolloff_noise = -17, mouthAnchors = list(time = c(0, 0.14, 0.84, 1), value = c(0, 0.51, 0.38, 0)), amplAnchors = list(time = c(0, 0.3, 1), value = c(35, 120, 36)))',

    Cow = 'soundgen(sylLen = 1610, pitchAnchors = list(time = c(0, 0.61, 0.85, 1), value = c(71, 104, 200, 197)), temperature = 0.05, pitchEffects_amount = 66, jitterDep = 2, rolloff = -24, rolloffAdjust_per_octave = 0, rolloffAdjust_per_kHz = -10, exactFormants = list(f1 = list(time = c(0, 1), freq = c(300, 300), amp = c(30, 30), width = c(120, 120)), f2 = list(time = 0, freq = 800, amp = 40, width = 120), f3 = list(time = 0, freq = 1100, amp = 40, width = 150)), formantDep = 1.6, vocalTract = 34, subDep = 50, shortestEpoch = 125, noiseAnchors = list(time = c(-55, 1404, 1608, 1846), value = c(-120, -120, 3, -120)), exactFormants_noise = list(f1 = list(time = c(0, 1), freq = c(300, 300), amp = c(30, 30), width = c(120, 120)), f2 = list(time = 0, freq = 800, amp = 40, width = 120), f3 = list(time = 0, freq = 1100, amp = 40, width = 150)), rolloff_noise = -17, mouthAnchors = list(time = c(0, 0.6, 0.83, 1), value = c(0, 0, 0.89, 0.81)))',

    Dog_bark = 'soundgen(repeatBout = 2, sylLen = 140, pauseLen = 100, pitchAnchors = list(time = c(0, 0.52, 1), value = c(559, 785, 557)), temperature = 0.05, pitchEffects_amount = 100, jitterDep = 1, rolloffAdjust_per_octave = -6, exactFormants = list(f1 = list(time = c(0, 1), freq = c(1700, 1700), amp = c(40, 40), width = c(400, 400)), f2 = list(time = 0, freq = 3300, amp = 50, width = 400), f3 = list(time = 0, freq = 7300, amp = 40, width = 1000)), vocalTract = 8, subDep = 60, noiseAnchors = list(time = c(0, 80, 160), value = c(-120, 22, -120)), exactFormants_noise = list(f1 = list(time = c(0, 1), freq = c(1700, 1700), amp = c(40, 40), width = c(400, 400)), f2 = list(time = 0, freq = 3300, amp = 50, width = 400), f3 = list(time = 0, freq = 7300, amp = 40, width = 1000)), rolloff_noise = -13, mouthAnchors = list(time = c(0, 0.5, 1), value = c(0, 0.5, 0)))',

    Duck = 'soundgen(repeatBout = 5, sylLen = 110, pauseLen = 170, pitchAnchors = list(time = c(0, 1), value = c(119, 110)), temperature = 0.1, rolloffAdjust_per_octave = 0, exactFormants = list(f1 = list(time = 0, freq = 1600, amp = 40, width = 250), f2 = list(time = 0, freq = 2100, amp = 40, width = 250), f3 = list(time = 0, freq = 2870, amp = 40, width = 300), f4 = list(time = 0, freq = 5600, amp = 40, width = 300), f5 = list(time = 0, freq = 6400, amp = 40, width = 300), f6 = list(time = 0, freq = 7200, amp = 40, width = 300)), noiseAnchors = list(time = c(0, 110), value = c(0, 0)), exactFormants_noise = list(f1 = list(time = 0, freq = 1600, amp = 40, width = 250), f2 = list(time = 0, freq = 2100, amp = 40, width = 250), f3 = list(time = 0, freq = 2870, amp = 40, width = 300), f4 = list(time = 0, freq = 5600, amp = 40, width = 300), f5 = list(time = 0, freq = 6400, amp = 40, width = 300), f6 = list(time = 0, freq = 7200, amp = 40, width = 300)), mouthAnchors = list(time = c(0, 0.5, 1), value = c(0.3, 0.5, 0.3)))',

    Elephant = 'soundgen(sylLen = 510, pitchAnchors = list(time = c(0, 0.37, 1), value = c(436, 452, 328)), pitchEffects_amount = 50, jitterDep = 0.3, rolloffAdjust_per_octave = -2, exactFormants = NA, vocalTract = 100, subFreq = 75, subDep = 40, shortestEpoch = 50, noiseAnchors = list(time = c(0, 510), value = c(-120, -120)), amplAnchors = list(time = c(0, 0.53, 1), value = c(120, 120, 61)))',

    Seagull = 'soundgen(nSyl = 8, sylLen = 200, pauseLen = 140, pitchAnchors = list(time = c(0, 0.65, 1), value = c(977, 1540, 826)), temperature = 0.05, pitchEffects_amount = 100, jitterDep = 0, rolloff = 0, rolloffAdjust_per_octave = -4, rolloffAdjust_quadratic = 25, rolloffAdjust_quadratic_nHarm = 6, exactFormants = list(f1 = list(time = 0, freq = 1000, amp = 20, width = 250), f2 = list(time = 0, freq = 2200, amp = 30, width = 400), f3 = list(time = 0, freq = 4400, amp = 10, width = 200), f4 = list(time = 0, freq = 5900, amp = 10, width = 300), f5 = list(time = 0, freq = 7200, amp = 10, width = 300)), vocalTract = 15, subFreq = 525, subDep = 220, noiseAnchors = list(time = c(-3, 41, 166, 201), value = c(-37, -120, -120, -45)), exactFormants_noise = list(f1 = list(time = 0, freq = 1000, amp = 20, width = 250), f2 = list(time = 0, freq = 2200, amp = 30, width = 400), f3 = list(time = 0, freq = 4400, amp = 10, width = 200), f4 = list(time = 0, freq = 5900, amp = 10, width = 300), f5 = list(time = 0, freq = 7200, amp = 10, width = 300)), rolloff_noise = -16, samplingRate = 24000)',

    Formants = list( # reserved name - the list of presets for every caller must end with a list of 'Formants' presets for each vowel and consonant
      # ...
    )
  )

  # # empty container for importing user presets with random names
  # Custom = list(
  #
  # )
) # END of presets / dictionaries
