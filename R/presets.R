###
### D E F A U L T   P A R   V A L U E S   &   P R E S E T S
###

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
breathingAnchorsWiggle_per_temp = .1
amplAnchorsWiggle_per_temp = .1
smoothLinear_factor = 1 # (0 to +Inf): regulates smoothing of formant anchors in function getSpectralEnvelope(). 0 = close to spline; >3 = approches linear extrapolation
rolloff_per_ampl = 12 # as amplitude goes down from max to throwaway_dB, rolloff_exp increases by rolloff_per_ampl dB/octave. The effect is to make loud parts brighter by increasing energy in higher frequencies
# dictionaries for getBinaryRandomWalk() to calculate and load once at the beginning of each session
slope_q1 = -.1
midpoint_q1 = 33
slope_q2 = -.1
midpoint_q2 = 66

noiseThresholds_dict = data.frame(noiseAmount = 0:100, q1=NA, q2=NA)
noiseThresholds_dict$q1 = 100 / (1 + exp(-slope_q1*(noiseThresholds_dict$noiseAmount-midpoint_q1)))
noiseThresholds_dict$q2 = 100 / (1 + exp(-slope_q2*(noiseThresholds_dict$noiseAmount-midpoint_q2)))
# plot (noiseThresholds_dict$noiseAmount, noiseThresholds_dict$q1, type='l', col='red')
# points (noiseThresholds_dict$noiseAmount, noiseThresholds_dict$q2, type='l', col='blue')



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
permittedValues = matrix ( c (
  # sliderInput's to be reset for each callType
  'nSyl', 1, 1, 10, 1, # default, low, high, step
  'sylDur_mean', 300, 20, 5000, 10, # for one syllable
  'pauseDur_mean', 100, 20, 1000, 10,
  'temperature', .025, 0, 1, .025,
  'min_epoch_length_ms', 300, 50, 500, 25,
  'maleFemale', 0, -1, 1, 0.1,
  'creakyBreathy', 0, -1, 1, 0.1,
  'attackLen', 50, 1, 200, 10,
  'formantStrength', 1, 0, 5, .1,
  'extraFormants_ampl', 30, 0, 60, 10,
  'noiseAmount',0,0,100,1,
  'noiseIntensity',50,0,100,1,
  'jitterDep', 3, 0, 24, 0.1,
  'jitterLength_ms', 1, 1, 100, 1,
  'shimmerDep', 0, 0, 100, 1,
  'g0', 100, 10, 1000, 10,
  'sideband_width_hz', 100, 0, 500, 10,
  'rolloff_exp', -12, -60, 0, 1,
  'rolloff_exp_delta', -12, -30, 10, 1,
  'adjust_rolloff_per_kHz', -6, -20, 0, 1,
  'quadratic_delta', 0, -50, 50, 5,
  'quadratic_nHarm', 3, 1, 20, 1,
  'rolloff_lip', 6, 0, 20, 1,
  'trill_dep', 0, 0, 1, .1,
  'trill_freq', 30, 10, 100, 5,
  'vibratoFreq', 5, 3, 10, .5,   # Horii, 1989
  'vibratoDep', 0, 0, 3, 0.125,
  'rolloff_breathing', -12, -20, 20, 1,
  'vocalTract_length', 15.5, 2, 100, .5,
  'repeatBout', 1, 1, 20, 1,

  # other settings
  'spec_windowLength', 40, 5, 100, 2.5, # default, low, high, step
  'spec_contrast', .2, -1, 1, .05,
  'spec_brightness', 0, -1, 1, .05,
  'pause_ms', 100, 20, 10000, 10,
  'mouthOpening', .5, 0, 1, .05,
  'pitch', 100, 20, 3500, 1, # set per species
  'pitchDeltas', 0, -24, 24, 1,
  'time', 0, 0, 5000, 1,
  'breathing_dur', 0, -1000, 6000, 25,
  'breathing_ampl', 0, throwaway_dB, 40, 1,
  'f1_freq', 860, 200, 1200, 10,
  'f2_freq', 1280, 600, 3500, 10,
  'f3_freq', 2900, 800, 5000, 25,
  'f4_freq', 3800, 1000, 7000, 25,
  'f5_freq', 3800, 1000, 7000, 25,
  'f6_freq', 3800, 1000, 8000, 25,
  'f7_freq', 3800, 1000, 8000, 25,
  'f8_freq', 3800, 1000, 8000, 25,
  'f9_freq', 3800, 1000, 8000, 25,
  'f10_freq', 3800, 1000, 8000, 25,
  'f1_amp', 30, -60, 60, 5,
  'f2_amp', 40, -60, 60, 5,
  'f3_amp', 25, -60, 60, 5,
  'f4_amp', 20, -60, 60, 5,
  'f1_width', 120, 25, 400, 25,
  'f2_width', 120, 25, 400, 25,
  'f3_width', 200, 25, 400, 25,
  'f4_width', 100, 25, 400, 25
), ncol=5, byrow=TRUE)
temp = permittedValues[,1]
permittedValues = apply (permittedValues[,2:5], 2, as.numeric)
colnames(permittedValues)=c('default','low','high','step')
rownames(permittedValues) = temp

# -------------------------------------------------------------
# a library of presets for easy generation of a few nice sounds
# -------------------------------------------------------------
#' Presets
#'
#' A library of presets for easy generation of a few nice sounds.
#'
#' @format A list of length 4.
"presets"
presets = list(
  M1 = list(
    'Vowel' = c(
      as.list (permittedValues[1 : which(rownames(permittedValues)=='rolloff_breathing'),'default']),
      'vowelString'='a',
      list('pitchAnchors' = data.frame('time'=c(0,.1,.9,1), 'value'=c(100,150,135,100)))
    ), # these are basically just the global defaults

    'Gasp' = c(
      'sylDur_mean' = 280,
      'rolloff_exp_delta' = -8,
      'adjust_rolloff_per_kHz' = -15,
      'temperature' = .05,
      'sideband_width_hz'=0,
      'rolloff_breathing'=-5,
      list('pitchAnchors' = data.frame('time'=c(0,.7,1), 'value'=c(402,556,490))),
      list('breathingAnchors'=data.frame('time'=c(1,234,408), 'value'=c(-120,-32,-120))),
      list('mouthAnchors'=data.frame('time'=c(0,.3,1), 'value'=c(0.32,0.88,0.42))),
      list('exactFormants_unvoiced'=list( # [h] preset
        f1=data.frame( time=c(0), freq=c(420), amp=c(20), width=c(150)),
        f2=data.frame( time=c(0), freq=c(1200), amp=c(50), width=c(250)),
        f3=data.frame( time=c(0), freq=c(5000), amp=c(10), width=c(200)),
        f4=data.frame( time=c(0), freq=c(8500), amp=c(10), width=c(300))))
    ),

    'Roar' = c(
      'sylDur_mean' = 960,
      'noiseAmount'=70,
      'noiseIntensity'=50,
      'rolloff_exp'=-20,
      'rolloff_exp_delta'=-3,
      'adjust_rolloff_per_kHz'=-6,
      'quadratic_delta'=-10,
      'quadratic_nHarm'=13,
      'jitterDep' = 0.7,
      'temperature' = .05,
      'min_epoch_length_ms'=300,
      'g0'=100,
      'sideband_width_hz'=40,
      list('pitchAnchors' = data.frame('time'=c(0,.14,.88,1), 'value'=c(174,200,191,131))),
      list('mouthAnchors' = data.frame('time'=c(0,.14,.88,1), 'value'=c(0.4,0.59,0.61,0.45))),
      list('exactFormants'=list(
        f1=data.frame( time=c(0), freq=c(620), amp=c(40), width=c(80)),
        f2=data.frame( time=c(0), freq=c(1000), amp=c(50), width=c(80)),
        f3=data.frame( time=c(0), freq=c(1800), amp=c(40), width=c(200)),
        f4=data.frame( time=c(0), freq=c(2560), amp=c(50), width=c(300)),
        f5=data.frame( time=c(0), freq=c(3400), amp=c(35), width=c(300))))
    ),

    'Moan' = c(
      'sylDur_mean' = 800,
      'rolloff_exp' = -23,
      'rolloff_exp_delta' = -6,
      'attackLen' = 100,
      'temperature' = .025,
      'rolloff_breathing' = -10,
      list('pitchAnchors' = data.frame('time'=c(0,1), 'value'=c(202,144))),
      list('breathingAnchors' = data.frame('time'=c(-7,268,590,869), 'value'=c(-67,-45,-24,-46))),
      list('mouthAnchors' = data.frame('time'=c(0,.3,1), 'value'=c(0.24,0.41,0.19))),
      list('exactFormants'=list(
        f1=data.frame( time=c(0), freq=c(980), amp=c(35), width=c(120)),
        f2=data.frame( time=c(0), freq=c(1400), amp=c(35), width=c(150)),
        f3=data.frame( time=c(0), freq=c(2680), amp=c(30), width=c(200)),
        f4=data.frame( time=c(0), freq=c(3400), amp=c(30), width=c(200)),
        f5=data.frame( time=c(0), freq=c(4150), amp=c(30), width=c(400))))
    ),

    'Sigh' = c(
      'sylDur_mean' = 50,
      temperature = .1,
      list('pitchAnchors' = data.frame('time'=c(0,1), 'value'=c(202,144))),
      list('breathingAnchors'=data.frame('time'=c(-20,104,756,1252), 'value'=c(26,40,23,-22))),
      list(exactFormants=list(
        f1=data.frame( time=c(0), freq=c(800), amp=c(30), width=c(100)),
        f1.5=data.frame( time=c(0), freq=c(1200), amp=c(30), width=c(100)),
        f2=data.frame( time=c(0), freq=c(1500), amp=c(30), width=c(100)),
        f3=data.frame( time=c(0), freq=c(2600), amp=c(30), width=c(100)),
        f4=data.frame( time=c(0), freq=c(4000), amp=c(30), width=c(100))))
    ),

    'Laugh' = c(
      'nSyl' = 5,
      'sylDur_mean' = 120,
      'pauseDur_mean' = 120,
      'temperature' = 0.1,
      'attackLen' = 10,
      'rolloff_exp'=-12,
      'rolloff_exp_delta'=-12,
      'noiseAmount'=100,
      'jitterDep'=.8,
      'sideband_width_hz' = 0,
      'vowelString' = 'ae',
      'rolloff_breathing'=-5,
      list('pitchAnchors' = data.frame('time'=c(0,1), 'value'=c(180,166))),
      list('pitchAnchors_global' = data.frame('time'=c(0,.3,1), 'value'=c(1,2,0))),
      list('breathingAnchors'=data.frame('time'=c(39,56,167), 'value'=c(-120,-44,-120))),
      list('amplAnchors'=data.frame('time'=c(0,.4,1), 'value'=c(120,0,120))),
      list('amplAnchors_global'=data.frame('time'=c(0,.55,1), 'value'=c(120,110,20))),
      list('exactFormants_unvoiced'=list(
        f1=data.frame( time=c(0), freq=c(420), amp=c(20), width=c(150)),
        f2=data.frame( time=c(0), freq=c(1200), amp=c(50), width=c(250)),
        f3=data.frame( time=c(0), freq=c(5000), amp=c(10), width=c(200)),
        f4=data.frame( time=c(0), freq=c(8500), amp=c(10), width=c(300))))
    ),

    'Snore' = c(
      'sylDur_mean'=960,
      'noiseAmount'=67,
      'jitterDep'=0.75,
      'rolloff_exp'=-12,
      'rolloff_exp_delta'=-12,
      'rolloff_breathing'=-12,
      'temperature'=0.05,
      'min_epoch_length_ms'=200,
      'g0'=100,
      'sideband_width_hz'=80,
      list('pitchAnchors'=data.frame('time'=c(0,.15,.87,1), 'value'=c(175,199,188,140))),
      list('breathingAnchors'=data.frame('time'=c(0,960), 'value'=c(-40,-40))),
      list('mouthAnchors'=data.frame('time'=c(0,1), 'value'=c(0,0))),
      list('amplAnchors_global'=data.frame('time'=c(0,1), 'value'=c(120,43))),
      list('exactFormants_unvoiced'=list(
        f1=data.frame( time=c(0), freq=c(560), amp=c(30), width=c(120)),
        f2=data.frame( time=c(0), freq=c(1000), amp=c(40), width=c(120)),
        f3=data.frame( time=c(0), freq=c(1450), amp=c(25), width=c(200)),
        f4=data.frame( time=c(0), freq=c(3800), amp=c(20), width=c(100))))
    ),

    'Formants' = list( # reserved name - the list of presets for every caller must end with a list of 'Formants' presets for each vowel and consonant
      # vowels
      'a' = list(
        'f1'=data.frame(time=0, freq=860, amp=30, width=120), # amplitude in dB, freq and width in Hz
        'f2'=data.frame(time=0, freq=1280, amp=40, width=120),
        'f3'=data.frame(time=0, freq=2900, amp=25, width=200),
        'f4'=data.frame(time=0, freq=3800, amp=20, width=100) # any number of formants may be specified
      ),
      'o' = list(
        'f1'=data.frame(time=0, freq=630, amp=35, width=100),
        'f2'=data.frame(time=0, freq=900, amp=35, width=100),
        'f3'=data.frame(time=0, freq=3000, amp=30, width=200),
        'f4'=data.frame(time=0, freq=3960, amp=30, width=200)
      ),
      'i' = list(
        'f1'=data.frame(time=0, freq=300, amp=25, width=80),
        'f2'=data.frame(time=0, freq=2700, amp=30, width=100),
        'f3'=data.frame(time=0, freq=3400, amp=40, width=350),
        'f4'=data.frame(time=0, freq=4200, amp=40, width=350)
      ),
      'e' = list(
        'f1'=data.frame(time=0, freq=530, amp=30, width=50),
        'f1.4'=data.frame(time=0, freq=1100, amp=-20, width=100), # insert a zero-pole pair between F1 and F2
        'f1.6'=data.frame(time=0, freq=1400, amp=20, width=100),  # insert a zero-pole pair between F1 and F2
        'f2'=data.frame(time=0, freq=2400, amp=40, width=300),
        'f3'=data.frame(time=0, freq=4000, amp=30, width=300)
      ),
      'u' = list(
        'f1'=data.frame(time=0, freq=375, amp=25, width=80),
        'f2'=data.frame(time=0, freq=550, amp=35, width=120),
        'f3'=data.frame(time=0, freq=2100, amp=25, width=300),
        'f4'=data.frame(time=0, freq=4200, amp=45, width=250)
      ),
      '0' = list(  # schwa
        'f1'=data.frame(time=0, freq=640, amp=30, width=100),
        'f2'=data.frame(time=0, freq=1670, amp=30, width=100),
        'f3'=data.frame(time=0, freq=2700, amp=30, width=100),
        'f4'=data.frame(time=0, freq=3880, amp=30, width=100)
      ),
      # consonants
      'h' = list(  # sh
        'rolloff_breathing'=-13,
        f1=data.frame(time=0, freq=420, amp=20, width=150),
        f2=data.frame(time=0, freq=1200, amp=50, width=250),
        f3=data.frame(time=0, freq=5000, amp=10, width=200),
        f4=data.frame(time=0, freq=8500, amp=10, width=300)
      ),
      's' = list(
        'rolloff_breathing'=0,
        'f1'=data.frame(time=0, freq=5500, amp=20, width=200),
        'f2'=data.frame(time=0, freq=7000, amp=30, width=1000),
        'f3'=data.frame(time=0, freq=9000, amp=30, width=1000)
      ),
      'x' = list(  # sh
        'rolloff_breathing'=-9,
        'f1'=data.frame(time=0, freq=1700, amp=15, width=80),
        'f2'=data.frame(time=0, freq=2600, amp=30, width=300),
        'f3'=data.frame(time=0, freq=3400, amp=25, width=200),
        'f4'=data.frame(time=0, freq=4800, amp=10, width=300)
      ),
      'f' = list(
        'rolloff_breathing'=-10,
        'f1'=data.frame(time=0, freq=1400, amp=30, width=200),
        'f2'=data.frame(time=0, freq=2000, amp=10, width=80),
        'f3'=data.frame(time=0, freq=2900, amp=20, width=1000)
      ),
      'n' = list( # sNuffle (breathing through the nose)
        'rolloff_breathing'=0,
        'f1'=data.frame(time=0, freq=5400, amp=25, width=2000)
      )
    )
  ),

  F1 = list(
    'Vowel' = c(
      'sylDur_mean' = 500,
      'vowelString'='a',
      list('pitchAnchors' = data.frame('time'=c(0,0.558,1), 'value'=c(340,370,340)))
    ),

    'Scream' = c(
      'sylDur_mean' = 1110,
      'attackLen' = 50,
      'jitterDep' = 1,
      'shimmerDep' = 10,
      'temperature' = .1,
      'noiseAmount' = 70,
      'noiseIntensity' = 50,
      'g0'=400,
      'sideband_width_hz'=100,
      list('pitchAnchors' = data.frame('time'=c(0,.1,.85,1), 'value'=c(900,1832,1618,1200)))
    ),

    'Growl' = c(
      'sylDur_mean' = 1100,
      'noiseAmount'=0,
      'rolloff_exp'=-15,
      'rolloff_exp_delta'=-6,
      'quadratic_delta'=-20,
      'quadratic_nHarm'=20,
      'trill_dep'=0.6,
      'trill_freq'=60,
      'temperature'=.1,
      'min_epoch_length_ms'=300,
      list('pitchAnchors' = data.frame('time'=c(0,.1,.3,1), 'value'=c(238,251,449,205))),
      list('amplAnchors_global' = data.frame('time'=c(0,.13,1), 'value'=c(101,120,26))),
      list('mouthAnchors' = data.frame('time'=c(0,.13,.76,1), 'value'=c(0.4,0.59,0.61,0.45))),
      list('exactFormants'=list(
        f1=data.frame( time=c(0), freq=c(800), amp=c(20), width=c(120)),
        f2=data.frame( time=c(0), freq=c(2150), amp=c(30), width=c(300)),
        f3=data.frame( time=c(0), freq=c(4250), amp=c(30), width=c(300)),
        f4=data.frame( time=c(0), freq=c(6600), amp=c(30), width=c(300))))
    ),

    'Moan' = c(
      'sylDur_mean' = 360,
      'rolloff_exp' = -20,
      'rolloff_exp_delta' = -6,
      'attackLen' = 100,
      'temperature' = .025,
      'vowelString' = 'aa0',
      list('pitchAnchors' = data.frame('time'=c(0,1), 'value'=c(380,260))),
      list('breathingAnchors' = data.frame('time'=c(0,417,508), 'value'=c(-63,-29,-120))),
      list('mouthAnchors' = data.frame('time'=c(0,.14,1), 'value'=c(0.24,0.41,0.19))),
      list('amplAnchors_global' = data.frame('time'=c(0,1), 'value'=c(120,18)))
    ),

    'Laugh' = c(
      'nSyl' = 3,
      'sylDur_mean' = 60,
      'pauseDur_mean' = 90,
      'temperature' = 0.075,
      'attackLen' = 10,
      'vowelString' = '0',
      'rolloff_breathing'=-8,
      list('pitchAnchors' = data.frame('time'=c(0,1), 'value'=c(368,284))),
      list('breathingAnchors'=data.frame('time'=c(0,67,86,186), 'value'=c(-45,-47,-89,-120))),
      list('amplAnchors_global'=data.frame('time'=c(0,1), 'value'=c(120,20)))
    ),

    'Cry' = c(
      'sylDur_mean' = 1600,
      'noiseAmount' = 15,
      'rolloff_exp' = -12,
      'rolloff_exp_delta' = -12,
      'temperature' = .2,
      'g0' = 125,
      'sideband_width_hz' = 70,
      'rolloff_breathing' = 0,
      'vowelString'='0',
      list('pitchAnchors'=data.frame('time'=c(0,1), 'value'=c(610,511))),
      list('mouthAnchors'=data.frame('time'=c(0,1), 'value'=c(0,0))),
      list('amplAnchors_global'=data.frame('time'=c(0,1), 'value'=c(120,60)))
    ),

    'Formants' = list( # reserved name - the list of presets for every caller must end with a list of 'Formants' presets for each vowel and consonant
      # vowels
      'a' = list(
        'f1'=data.frame(time=0, freq=900, amp=30, width=80), # amplitude in dB, freq and width in Hz
        'f2'=data.frame(time=0, freq=1300, amp=30, width=160),
        'f3'=data.frame(time=0, freq=3300, amp=25, width=130),
        'f4'=data.frame(time=0, freq=4340, amp=20, width=370) # any number of formants may be specified
      ),
      'o' = list(
        'f1'=data.frame(time=0, freq=800, amp=30, width=80),
        'f2'=data.frame(time=0, freq=1100, amp=30, width=80),
        'f3'=data.frame(time=0, freq=3560, amp=40, width=200),
        'f4'=data.frame(time=0, freq=5830, amp=50, width=200)
      ),
      'i' = list(
        'f1'=data.frame(time=0, freq=330, amp=30, width=120),
        'f2'=data.frame(time=0, freq=2700, amp=40, width=120),
        'f3'=data.frame(time=0, freq=3580, amp=30, width=200),
        'f4'=data.frame(time=0, freq=4710, amp=30, width=200),
        'f5'=data.frame(time=0, freq=5800, amp=30, width=200)
      ),
      'e' = list(
        'f1'=data.frame(time=0, freq=930, amp=30, width=100),
        'f2'=data.frame(time=0, freq=2470, amp=30, width=100),
        'f3'=data.frame(time=0, freq=3300, amp=25, width=120),
        'f4'=data.frame(time=0, freq=4200, amp=30, width=200)
      ),
      'u' = list(
        'f1'=data.frame(time=0, freq=450, amp=30, width=80),
        'f2'=data.frame(time=0, freq=850, amp=40, width=120),
        'f3'=data.frame(time=0, freq=2900, amp=30, width=200),
        'f4'=data.frame(time=0, freq=4100, amp=30, width=275)
      ),
      '0' = list(  # schwa
        'f1'=data.frame(time=0, freq=790, amp=30, width=100),
        'f2'=data.frame(time=0, freq=1600, amp=30, width=100),
        'f3'=data.frame(time=0, freq=3100, amp=30, width=100),
        'f4'=data.frame(time=0, freq=3900, amp=30, width=100)
      ),
      # consonants
      'h' = list(  # sh
        'rolloff_breathing'=-13,
        f1=data.frame(time=0, freq=420, amp=20, width=150),
        f2=data.frame(time=0, freq=1200, amp=50, width=250),
        f3=data.frame(time=0, freq=5000, amp=10, width=200),
        f4=data.frame(time=0, freq=8500, amp=10, width=300)
      ),
      's' = list(
        'rolloff_breathing'=0,
        'f1'=data.frame(time=0, freq=5500, amp=20, width=200), # NB: amplitude in dB for consonants
        'f2'=data.frame(time=0, freq=7000, amp=30, width=1000),
        'f3'=data.frame(time=0, freq=9000, amp=30, width=1000)
      ),
      'x' = list(  # sh
        'rolloff_breathing'=-9,
        'f1'=data.frame(time=0, freq=1700, amp=15, width=80),
        'f2'=data.frame(time=0, freq=2600, amp=30, width=300),
        'f3'=data.frame(time=0, freq=3400, amp=25, width=200),
        'f4'=data.frame(time=0, freq=4800, amp=10, width=300)
      ),
      'f' = list(
        'rolloff_breathing'=-10,
        'f1'=data.frame(time=0, freq=1400, amp=30, width=200),
        'f2'=data.frame(time=0, freq=2000, amp=10, width=80),
        'f3'=data.frame(time=0, freq=2900, amp=20, width=1000)
      ),
      'n' = list( # sNuffle (breathing through the nose)
        'rolloff_breathing'=0,
        'f1'=data.frame(time=0, freq=5400, amp=25, width=2000)
      )
    )
  ),

  Chimpanzee = list(
    'Bark (alarm)' = c(
      'sylDur_mean' = 160,
      'noiseAmount'=100,
      'attackLen'=61,
      'jitterDep'=4.9,
      'rolloff_exp'=-30,
      'rolloff_exp_delta'=0,
      'rolloff_breathing'=-14,
      'temperature'=0.025,
      'g0'=125,
      'sideband_width_hz'=90,
      list('pitchAnchors' = data.frame('time'=c(0,1), 'value'=c(232,185))),
      list('breathingAnchors' = data.frame('time'=c(0,76,158,344), 'value'=c(-120,16,-21,-120))),
      list('exactFormants'=list(
        f1=data.frame( time=c(0), freq=c(415), amp=c(60), width=c(120)),
        f2=data.frame( time=c(0), freq=c(1000), amp=c(60), width=c(120)),
        f3=data.frame( time=c(0), freq=c(3000), amp=c(20), width=c(200)),
        f4=data.frame( time=c(0), freq=c(5000), amp=c(20), width=c(1000))))
    ),

    'Scream (conflict)' = c(
      'sylDur_mean'=740,
      'noiseAmount'=100,
      'jitterDep' =.3,
      'g0'=75,
      'sideband_width_hz'=130,
      'rolloff_exp'=-6,
      'rolloff_exp_delta'=0,
      'temperature' = .05,
      'rolloff_breathing'=-12,
      list('pitchAnchors' = data.frame('time'=c(0,.3,.9,1), 'value'=c(1200,1547,1487,1154))),
      list('exactFormants'=list(
        f1=data.frame( time=c(0), freq=c(800), amp=c(30), width=c(80)),
        f2=data.frame( time=c(0), freq=c(1600), amp=c(30), width=c(160)),
        f3=data.frame( time=c(0), freq=c(3000), amp=c(25), width=c(130)),
        f4=data.frame( time=c(0), freq=c(4340), amp=c(20), width=c(370))))
    ),

    'Grunt (excited)' = c(
      'nSyl'=6,
      'sylDur_mean'=100,
      'pauseDur_mean'=220,
      'jitterDep' = 3,
      'rolloff_exp'=-12,
      'rolloff_exp_delta'=-6,
      'temperature' = .05,
      'rolloff_breathing'=-12,
      list('pitchAnchors' = data.frame('time'=c(0,1), 'value'=c(127,102))),
      list('breathingAnchors'=data.frame('time'=c(0,83,205), 'value'=c(-120,-6,-120))),
      list('amplAnchors_global'=data.frame('time'=c(0,.35,1), 'value'=c(89,120,31))),
      list('exactFormants'=list(
        f1=data.frame( time=c(0,1), freq=c(415,300), amp=c(60,60), width=c(120,120)),
        f2=data.frame( time=c(0,1), freq=c(1000,500), amp=c(60,60), width=c(120,120)),
        f3=data.frame( time=c(0,1), freq=c(2200,2500), amp=c(20,20), width=c(200,200)),
        f4=data.frame( time=c(0), freq=c(3600), amp=c(20), width=c(200)),
        f5=data.frame( time=c(0,1), freq=c(4600,4200), amp=c(20,20), width=c(200,200))))
    ),

    'Hoot (excited)' = c(
      'sylDur_mean' = 730,
      'rolloff_exp'=-20,
      'rolloff_exp_delta'=0,
      'rolloff_breathing'=-13,
      'temperature'=0.025,
      list('pitchAnchors' = data.frame('time'=c(0,.52,1), 'value'=c(440,405,440))),
      list('breathingAnchors' = data.frame('time'=c(0,730), 'value'=c(-8,-8))),
      list('exactFormants'=list(
        f1=data.frame( time=c(0,0.4,1), freq=c(300,500,400), amp=c(30,30,30), width=c(80,80,80)),
        f2=data.frame( time=c(0,0.2,1), freq=c(500,1000,700), amp=c(30,30,30), width=c(120,120,120)),
        f3=data.frame( time=c(0), freq=c(2500), amp=c(30), width=c(120)),
        f4=data.frame( time=c(0), freq=c(4000), amp=c(30), width=c(200)),
        f5=data.frame( time=c(0), freq=c(5580), amp=c(30), width=c(200))))
    ),

    'Laugh (playing)' = c(
      'nSyl' = 5,
      'sylDur_mean' = 100,
      'pauseDur_mean' = 150,
      'rolloff_exp'=-22,
      'rolloff_exp_delta'=0,
      'quadratic_delta'=-20,
      'quadratic_nHarm'=8,
      'temperature' = 0.025,
      'rolloff_breathing'=-16,
      list('pitchAnchors' = data.frame('time'=c(0,.5,1), 'value'=c(134,144,117))),
      list('breathingAnchors'=data.frame('time'=c(20,60,100), 'value'=c(-120,-19,-120))),
      list('exactFormants'=list(
        f1=data.frame( time=c(0), freq=c(300), amp=c(30), width=c(80)),
        f2=data.frame( time=c(0), freq=c(950), amp=c(30), width=c(120)),
        f3=data.frame( time=c(0), freq=c(1600), amp=c(40), width=c(120)),
        f4=data.frame( time=c(0), freq=c(2240), amp=c(25), width=c(200)),
        f5=data.frame( time=c(0), freq=c(2900), amp=c(20), width=c(100)))),
      list('exactFormants_unvoiced'=list(
        f1=data.frame( time=c(0), freq=c(420), amp=c(20), width=c(150)),
        f2=data.frame( time=c(0), freq=c(1200), amp=c(50), width=c(250)),
        f3=data.frame( time=c(0), freq=c(5000), amp=c(10), width=c(200)),
        f4=data.frame( time=c(0), freq=c(8500), amp=c(10), width=c(300))))
    ),

    'Formants' = list( # reserved name - the list of presets for every caller must end with a list of 'Formants' presets for each vowel and consonant
      # ...
    )
  ),

  Misc = list(
    'Cat (miaw)' = c(
      'sylDur_mean' = 1300,
      'rolloff_exp'=-6, 'rolloff_exp_delta'=-6,
      'quadratic_delta'=20,
      'quadratic_nHarm'=20,
      'temperature'=0.05,
      'rolloff_breathing'=-17,
      'vocalTract_length'=8,
      list('pitchAnchors' = data.frame('time'=c(0,0.25,1), 'value'=c(264,315,274))),
      list('breathingAnchors' = data.frame(time=c(-44,1200,1389,1594), value=c(-55,-32,10,-78))),
      list('mouthAnchors' = data.frame(time=c(0,0.14,0.84,1), value=c(0,0.51,0.38,0))),
      list('amplAnchors' = data.frame(time=c(0,0.3,1), value=c(35,120,36))),
      list('exactFormants'=list(
        f1=data.frame( time=c(0,.15,1), freq=c(1540,1900,1020), amp=c(20), width=c(200)),
        f2=data.frame( time=c(0,.15,1), freq=c(1800,2900,2800), amp=c(20), width=c(200)),
        f2.5=data.frame( time=c(0,.15,1), freq=c(2500,3700,3400), amp=c(-20), width=c(400)),
        f3=data.frame( time=c(0,.15,1), freq=c(3200,5300,5200), amp=c(20), width=c(400)),
        f4=data.frame( time=c(0,.15,1), freq=c(6000,6200,6000), amp=c(20), width=c(400))))
    ),

    'Cow' = c(
      'sylDur_mean'=1610,
      'vocalTract_length'=34,
      'noiseAmount'=66,
      'jitterDep'=2,
      'formantStrength'=1.6,
      'rolloff_exp'=-24,
      'rolloff_exp_delta'=0,
      'adjust_rolloff_per_kHz'=-10,
      'rolloff_breathing'=-17,
      'temperature'=0.05,
      'min_epoch_length_ms'=125,
      'g0'=100,
      'sideband_width_hz'=50,
      list('pitchAnchors' = data.frame(time=c(0,0.61,0.85,1), value=c(71,104,200,197))),
      list('breathingAnchors'=data.frame(time=c(-55,1404,1608,1846), value=c(-120,-120,3,-120))),
      list('mouthAnchors'=data.frame(time=c(0,0.6,0.83,1), value=c(0,0,0.89,0.81))),
      list('exactFormants'=list(
        f1=data.frame( time=c(0,1), freq=c(300), amp=c(30), width=c(120)),
        f2=data.frame( time=c(0), freq=c(800), amp=c(40), width=c(120)),
        f3=data.frame( time=c(0), freq=c(1100), amp=c(40), width=c(150))))
    ),

    'Dog (bark)' = c(
      'repeatBout'=2,
      'sylDur_mean'=140,
      'pauseDur_mean'=100,
      'noiseAmount'=100,
      'jitterDep'=1,
      'rolloff_exp'=-12,
      'rolloff_exp_delta'=-6,
      'rolloff_breathing'=-13,
      'temperature'=0.05,
      'min_epoch_length_ms'=300,
      'g0'=100,
      'sideband_width_hz'=60,
      'vocalTract_length'=8,
      list('pitchAnchors' = data.frame('time'=c(0,0.52,1), 'value'=c(559,785,557))),
      list('breathingAnchors'=data.frame(time=c(0,80,160), value=c(-120,22,-120))),
      list('mouthAnchors'=data.frame(time=c(0,0.5,1), value=c(0,0.5,0))),
      list('exactFormants'=list(
        f1=data.frame( time=c(0,1), freq=c(1700), amp=c(40), width=c(400)),
        f2=data.frame( time=c(0), freq=c(3300), amp=c(50), width=c(400)),
        f3=data.frame( time=c(0), freq=c(7300), amp=c(40), width=c(1000))))
    ),

    'Duck' = c(
      'nSyl'=1,
      'sylDur_mean'=110,
      'pauseDur_mean'=170,
      'repeatBout'=5,
      'rolloff_exp'=-12,
      'rolloff_exp_delta'=0,
      'temperature'=0.1,
      list('pitchAnchors' = data.frame('time'=c(0,1), 'value'=c(119,110))),
      list('breathingAnchors' = data.frame('time'=c(0,110), 'value'=c(0,0))),
      list('mouthAnchors'=data.frame('time'=c(0,0.5,1), 'value'=c(0.3,0.5,0.3))),
      list('exactFormants'=list(
        f1=data.frame( time=c(0), freq=c(1600), amp=c(40), width=c(250)),
        f2=data.frame( time=c(0), freq=c(2100), amp=c(40), width=c(250)),
        f3=data.frame( time=c(0), freq=c(2870), amp=c(40), width=c(300)),
        f4=data.frame( time=c(0), freq=c(5600), amp=c(40), width=c(300)),
        f5=data.frame( time=c(0), freq=c(6400), amp=c(40), width=c(300)),
        f6=data.frame( time=c(0), freq=c(7200), amp=c(40), width=c(300))))
    ),

    'Elephant' = c(
      'sylDur_mean'=510,
      'noiseAmount'=50,
      'jitterDep'=0.3,
      'rolloff_exp'=-12,
      'rolloff_exp_delta'=-2,
      'min_epoch_length_ms'=50,
      'g0'=75,
      'sideband_width_hz'=40,
      'vocalTract_length'=100,
      list('pitchAnchors'=data.frame('time'=c(0,0.37,1), 'value'=c(436,452,328))),
      list('amplAnchors'=data.frame('time'=c(0,0.53,1), 'value'=c(120,120,61))),
      list('exactFormants'=NA)
    ),

    # 'Elephant (rumble)' = c(
    #   sylDur_mean=5000,
    #   noiseAmount=75,
    #   jitterDep=1.4,
    #   rolloff_exp=-12,
    #   rolloff_exp_delta=-3,
    #   temperature=0.025,
    #   min_epoch_length_ms=2000,
    #   g0=25,
    #   sideband_width_hz=15,
    #   vocalTract_length=85,
    #   list(pitchAnchors=data.frame(time=c(0,0.53,1), value=c(34,52,34))),
    #   list(mouthAnchors=data.frame(time=c(0,0.26,1), value=c(0,0.57,0.1))),
    #   list(amplAnchors=data.frame(time=c(0,.2,.7,1), value=c(0,120,120,0))),
    #   list(exactFormants=list(
    #     f1=data.frame( time=c(0), freq=c(170), amp=c(50), width=c(50)),
    #     f2=data.frame( time=c(0), freq=c(330), amp=c(40), width=c(50)),
    #     f3=data.frame( time=c(0), freq=c(500), amp=c(40), width=c(50)),
    #     f4=data.frame( time=c(0), freq=c(650), amp=c(40), width=c(50)),
    #     f4=data.frame( time=c(0), freq=c(1050), amp=c(40), width=c(50)),
    #     f4=data.frame( time=c(0), freq=c(1300), amp=c(40), width=c(50))))
    # ),

    'Seagull' = c(
      'nSyl'=8,
      'sylDur_mean'=200,
      'pauseDur_mean'=140,
      'noiseAmount'=100,
      'jitterDep'=0,
      'rolloff_exp'=0,
      'rolloff_exp_delta'=-4,
      'quadratic_delta'=25,
      'quadratic_nHarm'=6,
      'temperature'=0.05,
      'rolloff_breathing'=-16,
      'g0'=525,
      'sideband_width_hz'=220,
      'samplingRate'=24000,
      'vocalTract_length'=15,
      list('pitchAnchors'=data.frame('time'=c(0,0.65,1), 'value'=c(977,1540,826))),
      list('breathingAnchors'=data.frame('time'=c(-3,41,166,201), 'value'=c(-37,-120,-120,-45))),
      list('exactFormants'=list(
        f1=data.frame( time=c(0), freq=c(1000), amp=c(20), width=c(250)),
        f2=data.frame( time=c(0), freq=c(2200), amp=c(30), width=c(400)),
        f3=data.frame( time=c(0), freq=c(4400), amp=c(10), width=c(200)),
        f4=data.frame( time=c(0), freq=c(5900), amp=c(10), width=c(300)),
        f5=data.frame( time=c(0), freq=c(7200), amp=c(10), width=c(300))))
    ),

    'Formants' = list( # reserved name - the list of presets for every caller must end with a list of 'Formants' presets for each vowel and consonant
      # ...
    )

  )
) # END of presets / dictionaries
