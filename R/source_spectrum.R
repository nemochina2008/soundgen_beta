# Functions for controlling the spectrum of generated sounds (rolloff and formants).

#' Control rolloff of harmonics
#'
#' Harmonics are generated as separate sine waves. But we don't want each
#' harmonic to be equally strong, so we normally specify some rolloff function
#' that describes the loss of energy in upper harmonics relative to the
#' fundamental frequency (f0). \code{\link{getRolloff}} provides flexible
#' control over this rolloff function, going beyond simple exponential decay
#' (\code{rolloff_exp}). Use quadratic terms to modify the behavior of a few lower
#' harmonics, \code{rolloff_exp_delta} to adjust the rate of decay per octave, and
#' \code{adjust_rolloff_per_kHz} for rolloff correction depending on f0. Plot the
#' output with different parameter values and see examples below and the
#' vignette to get a feel for how to use \code{\link{getRolloff}} effectively.
#' @param pitch_per_gc a vector of f0 per glottal cycle, Hz
#' @param nHarmonics maximum number of harmonics to generate (very weak
#'   harmonics with amplitude < \code{throwaway_dB} will be discarded)
#' @param rolloff_exp basic rolloff at a constant rate of \code{rolloff_exp}
#'   db/octave (exponential decay)
#' @param rolloff_exp_delta basic rolloff changes from lower to upper harmonics
#'   (regardless of f0) by \code{rolloff_exp_delta} dB/oct. For example, we can
#'   get steeper rolloff in the upper part of the spectrum
#' @param quadratic_delta an optional quadratic term affecting only the first
#'   \code{quadratic_nHarm} harmonics. The middle harmonic of the first
#'   \code{quadratic_nHarm} harmonics is amplified or dampened by
#'   \code{quadratic_delta} dB relative to the basic exponential decay.
#' @param quadratic_nHarm the number of harmonics affected by
#'   \code{quadratic_delta}
#' @param quadratic_ceiling an alternative way of specifying which harmonics are
#'   affected by \code{quadratic_delta}: instead of \code{quadratic_nHarm}, we
#'   can specify \code{quadratic_ceiling} to apply a parabolic boost to all
#'   harmonics up to this frequency (ie \code{quadratic_nHarm} will vary
#'   depending on f0). Defaults to NULL
#' @param adjust_rolloff_per_kHz rolloff changes linearly with f0 by
#'   \code{adjust_rolloff_per_kHz} dB/kHz. For ex., -6 dB/kHz gives a 6 dB
#'   steeper basic rolloff as f0 goes up by 1000 Hz
#' @param baseline_Hz The "neutral" frequency, at which no adjustment of rolloff
#'   takes place regardless of \code{adjust_rolloff_per_kHz}
#' @param throwaway_dB discard harmonics that are weaker than this number (in
#'   dB) to save computational resources
#' @param samplingRate sampling rate (needed to stop at Nyquist frequency and
#'   for plotting purposes)
#' @param plot if TRUE, produces a plot
#' @return Returns a matrix of amplitude multiplication factors for adjusting
#'   the amplitude of harmonics relative fo f0. Each row of output contains one
#'   harmonic, and each column contains one glottal cycle.
#' @export
#' @examples
#' # steady exponential rolloff of -12 dB per octave
#' rolloff = soundgen:::getRolloff(pitch_per_gc=150, rolloff_exp=-12, rolloff_exp_delta=0, plot=TRUE)
#' # the rate of rolloff slows down with each octave
#' rolloff = soundgen:::getRolloff(pitch_per_gc=150, rolloff_exp=-12, rolloff_exp_delta=2, plot=TRUE)
#' # the rate of rolloff increases with each octave
#' rolloff = soundgen:::getRolloff(pitch_per_gc=150, rolloff_exp=-12, rolloff_exp_delta=-2, plot=TRUE)
#'
#' # variable f0
#' # the lower f0, the more harmonics are non-zero
#' rolloff = soundgen:::getRolloff(pitch_per_gc=c(150,800,3000), rolloff_exp_delta=0, plot=TRUE)
#' # without the correction for f0 (adjust_rolloff_per_kHz),
#' # high-pitched sounds have the same rolloff as low-pitched sounds,
#' # producing unnaturally strong high-frequency harmonics
#' rolloff = soundgen:::getRolloff(pitch_per_gc=c(150,800,3000), rolloff_exp_delta=0,
#'   adjust_rolloff_per_kHz=0, plot=TRUE)
#'
#' # parabolic adjustment of lower harmonics
#' rolloff = soundgen:::getRolloff(pitch_per_gc=350, quadratic_delta=0,
#'   quadratic_nHarm=2, samplingRate=16000, plot=TRUE)
#' # quadratic_nHarm=1 affects only f0
#' rolloff = soundgen:::getRolloff(pitch_per_gc=150, quadratic_delta=30,
#'   quadratic_nHarm=1, samplingRate=16000, plot=TRUE)
#' # quadratic_nHarm=2 or 3 affects only h1
#' rolloff = soundgen:::getRolloff(pitch_per_gc=150, quadratic_delta=30,
#'   quadratic_nHarm=2, samplingRate=16000, plot=TRUE)
#' # quadratic_nHarm=4 affects h1 and h2, etc
#' rolloff = soundgen:::getRolloff(pitch_per_gc=150, quadratic_delta=30,
#'   quadratic_nHarm=4, samplingRate=16000, plot=TRUE)
#' # negative quadratic_delta weakens lower harmonics
#' rolloff = soundgen:::getRolloff(pitch_per_gc=150, quadratic_delta=-20,
#'   quadratic_nHarm=7, samplingRate=16000, plot=TRUE)
#' # only harmonics below 2000 Hz are affected
#' rolloff = soundgen:::getRolloff(pitch_per_gc=c(150,600),
#'   quadratic_delta=-20, quadratic_ceiling=2000, samplingRate=16000, plot=TRUE)
getRolloff = function(pitch_per_gc=c(440), nHarmonics=100, rolloff_exp=-12, rolloff_exp_delta=-2, quadratic_delta=0, quadratic_nHarm=2, quadratic_ceiling=NULL, adjust_rolloff_per_kHz=-6, baseline_Hz=200, throwaway_dB=-120, samplingRate=44100, plot=FALSE){
  ## Exponential decay
  deltas = matrix(0, nrow=nHarmonics, ncol=length(pitch_per_gc))
  if (sum(rolloff_exp_delta != 0) > 0){
    for (h in 2:nHarmonics){
      deltas[h,] = rolloff_exp_delta * (pitch_per_gc*h-baseline_Hz)/1000 # rolloff changes by rolloff_exp_delta per octave for each octave above H2
    }
  }
  # plot(deltas[,1])

  rolloff = matrix(0, nrow=nHarmonics, ncol=length(pitch_per_gc))
  for (h in 1:nHarmonics){
    rolloff[h,] = ( (rolloff_exp + adjust_rolloff_per_kHz*(pitch_per_gc-baseline_Hz)/1000) * log2(h)) + deltas[h,] # note that rolloff_exp is here adjusted as a linear function of the difference between current f0 and baseline_Hz
    rolloff[h, which(h*pitch_per_gc >= samplingRate/2)] = -Inf # to avoid aliasing, we discard all harmonics above Nyquist frequency
  }

  ## QUADRATIC term affecting the first quadratic_nHarm harmonics only
  if (quadratic_delta != 0){
    if (!is.null(quadratic_ceiling)) {
      quadratic_nHarm = round(quadratic_ceiling / pitch_per_gc) # vector of length pitch_per_gc specifying the number of harmonics whose amplitude is to be adjusted
    } else {
      quadratic_nHarm = rep(round(quadratic_nHarm), length(pitch_per_gc))
    }
    quadratic_nHarm[quadratic_nHarm==2] = 3 # will have the effect of boosting H1 (2 * F0)
    # parabola ax^2+bx+c
    # 0 at h=1 and at h=quadratic_nHarm; a parabola up/down in between. We have the following constraints on the parabola: f(1)=0; f(quadratic_nHarm)=0; f'((1+quadratic_nHarm)/2)=0; and f((1+quadratic_nHarm)/2)=quadratic_delta.
    ## Solving for a,b,c
    # f'(middle) = 2a*(1+quadratic_nHarm)/2+b = a*(1+quadratic_nHarm)+b = 0, so b = -a*(1+quadratic_nHarm).
    # f(1) = a+b+c = 0, so c = -a+a*(1+quadratic_nHarm) = a*quadratic_nHarm.
    # f(middle)=quadratic_delta. middle is (1+quadratic_nHarm)/2, and f( (1+quadratic_nHarm)/2 ) = a*(1+quadratic_nHarm)^2/4 + b*(1+quadratic_nHarm)/2 + c = (substituting above expressions for b and c) = a*(1+quadratic_nHarm)^2/4 - a*(1+quadratic_nHarm)*(1+quadratic_nHarm)/2 + a*quadratic_nHarm = -a*(1+quadratic_nHarm)^2/4 + a*quadratic_nHarm = -a/4*(1 + quadratic_nHarm^2 + 2*quadratic_nHarm - 4*quadratic_nHarm) = -a/4*(1-quadratic_nHarm)^2. And we want this to equal quadratic_delta. Solving for a, we have a = -4*quadratic_delta/(quadratic_nHarm-1)^2
    a = -4*quadratic_delta/(quadratic_nHarm-1)^2
    b = -a*(1+quadratic_nHarm)
    c = a*quadratic_nHarm
    # # verify:
    # myf = function(s,a,b,c){return(a*s^2+b*s+c)}
    # s = seq(1,quadratic_nHarm[1],by=.5)
    # plot (s, myf(s, a,b,c))
    rolloff[1,which(quadratic_nHarm<3)] = rolloff[1,which(quadratic_nHarm<2)] + quadratic_delta # for a single affected harmonic, just change the amplitude of F0
    for (i in which(quadratic_nHarm >= 3)){ # if at least 2 harmonics are to be adjusted, calculate a parabola
      rowIdx = 1:quadratic_nHarm[i]
      rolloff[rowIdx,i] = rolloff[rowIdx,i] + a[i]*rowIdx^2 + b[i]*rowIdx + c[i]   # plot (rolloff[,1])
    }
  }

  # set values under throwaway_dB to zero
  if (is.numeric(throwaway_dB)){ # if not null and not NA
    rolloff[rolloff<throwaway_dB] = -Inf
  }

  # normalize so the amplitude of F0 is always 0
  rolloff = apply (rolloff, 2, function(x)x-max(x))

  # plotting
  if (plot){
    x_max = samplingRate/2/1000
    if (length(pitch_per_gc)==1 | var(pitch_per_gc)==0){
      idx = which(rolloff[,1] > -Inf)
      plot (idx*pitch_per_gc[1]/1000, rolloff[idx,1], type='b', xlim=c(0,x_max), xlab='Frequency, Hz', ylab='Amplitude, dB', main='Glottal source rolloff')
    } else {
      pitch_min = min(pitch_per_gc)
      pitch_max = max(pitch_per_gc)
      idx_min = which.min(pitch_per_gc)
      idx_max = which.max(pitch_per_gc)
      rows_min = 1 : tail(which(rolloff[,idx_min] > -Inf),1)
      rows_max = 1 : tail(which(rolloff[,idx_max] > -Inf),1)
      freqs_min = rows_min*pitch_min/1000
      freqs_max = rows_max*pitch_max/1000
      rolloff_min = rolloff[rows_min,idx_min]
      rolloff_max = rolloff[rows_max,idx_max]
      plot (freqs_min, rolloff_min, type='b', col='blue', xlim=c(0,x_max), xlab='Frequency, Hz', ylab='Amplitude, dB', main='Glottal source rolloff')
      text (x=x_max, y=-10, labels='Lowest pitch', col='blue', pos=2)
      points (freqs_max, rolloff_max, type='b', col='red')
      text (x=x_max, y=0, labels='Highest pitch', col='red', pos=2)
    }
  }

  # convert from dB to linear amplitude multipliers
  rolloff = 2^(rolloff/10)

  # shorten by discarding harmonics that are 0 throughout the sound
  rolloff = rolloff[which(apply(rolloff,1,sum)>0), ,drop=FALSE]
  rownames(rolloff) = 1:nrow(rolloff) # helpful for adding vocal fry

  return (rolloff)
}


#' Spectral envelope
#'
#' Internal soundgen function.
#'
#' Prepares a spectral envelope for filtering a sound to add formants, lip
#' radiation, and some stochastic component regulated by temperature.
#' exactFormants is specified as a list containing time, frequency, amplitude,
#' and width values for each formant (see examples). NB: each formant is
#' generated as a gamma distribution with mean = freq and SD = width. Formant
#' bandwidths in soundgen are therefore NOT compatible with formant bandwidths
#' used in Klatt synthesizer and other algorithms that rely on FIR instead of
#' FFT.
#' @param nr the number of frequency bins = windowLength_points/2, where
#'   windowLength_points is the size of window for Fourier transform
#' @param nc the number of time steps for Fourier transform
#' @param exactFormants either a character string like "aaui" referring to
#'   default presets for speaker "M1" or a list of formant frequencies,
#'   amplitude, and bandwidth (see ex. below). exactFormants=NA defaults to
#'   schwa. Time stamps for exactFormants and mouthOpening can be specified in
#'   ms, percent of duration, or whatever - the scale doesn't matter, since
#'   duration is determined by length(ampl). See details below.
#' @param formantStrength scale factor of formant amplitude
#' @param rolloff_lip adds this many dB per octave (high-frequency boost) when
#'   the mouth is open
#' @param mouthAnchors specify when the mouth is open. Example: mouthAnchors =
#'   data.frame('time' = seq(0, 1000, length.out = 5), 'ampl'=c(0, .2, 1, .2,
#'   0))
#' @param mouthOpening_threshold count the mouth as open when its opening degree
#'   is >threshold
#' @param amplBoost_openMouth_dB amplify the voice when the mouth is open by
#'   \code{amplBoost_openMouth_dB} dB
#' @param vocalTract_length used for calculating formant dispersion and formant
#'   transitions as the mouth opens and closes (specified in cm)
#' @param temperature regulates the amount of stochasticity in the algorithm. If
#'   temperature == 0, the spectrum is exactly the same every time
#'   \code{getSpectralEnvelope} is called with the same pars. If temperature >
#'   0, input parameters are "wiggled" and extra formants are added above the
#'   highest specified formant
#' @param extraFormants_ampl the amplitude of additional formants added above
#'   the highest specified formant (only if temperature > 0)
#' @param smoothLinear_factor regulates smoothing of formant anchors (0 to +Inf)
#'   as they are upsampled to the number of fft steps \code{nc}. This is
#'   necessary because the input \code{exactFormants} normally contains fewer
#'   sets of formant values than the number of fft steps.
#'   \code{smoothLinear_factor} = 0: close to default spline; >3: approaches
#'   linear extrapolation
#' @param samplingRate sampling rate (Hz)
#' @return Returns a spectral filter (matrix nr x nc, where nr is the number of
#'   frequency bins = windowLength_points/2 and nc is the number of time steps)
#' @examples
#' # [a] with F1-F4 visible
#' image(t(soundgen:::getSpectralEnvelope(nr=512, nc=50,
#'   exactFormants=soundgen:::convertStringToFormants('a'),
#'   temperature=0, samplingRate=16000)))
#' # some "wiggling" of specified formants plus extra formants on top
#' image(t(soundgen:::getSpectralEnvelope(nr=512, nc=50,
#'   exactFormants=soundgen:::convertStringToFormants('a'),
#'   temperature=0.1, extraFormants_ampl=10, samplingRate=16000)))
#' # stronger extra formants
#' image(t(soundgen:::getSpectralEnvelope(nr=512, nc=50,
#'   exactFormants=soundgen:::convertStringToFormants('a'),
#'   temperature=0.1, extraFormants_ampl=30, samplingRate=16000)))
#' # a schwa based on the length of vocal tract = 15.5 cm
#' image(t(soundgen:::getSpectralEnvelope(nr=512, nc=50, exactFormants=NA,
#'   temperature=.1, vocalTract_length=15.5, samplingRate=16000)))
#'
#' # manual specification of formants
#' image(t(soundgen:::getSpectralEnvelope(nr=512, nc=50, samplingRate=16000,
#'   exactFormants=list('f1' = data.frame('time'=0, 'freq'=900, 'amp'=30, 'width'=120),
#'                      'f2' = data.frame('time'=0, 'freq'=1300, 'amp'=30, 'width'=120),
#'                      'f3' = data.frame('time'=0, 'freq'=3200, 'amp'=20, 'width'=200)))))
getSpectralEnvelope = function(nr, nc, exactFormants=NA, formantStrength=1, rolloff_lip=6, mouthAnchors=NA, mouthOpening_threshold=0, amplBoost_openMouth_dB=0, vocalTract_length=NULL, temperature=0, extraFormants_ampl=30, smoothLinear_factor=1, samplingRate=44100){
  if (class(exactFormants)=='character'){
    exactFormants = convertStringToFormants(exactFormants)
  }
  if (is.null(vocalTract_length) & length(exactFormants[[1]])>2) { # if we don't know vocalTract_length, but at least one formant is defined, we guess the length of vocal tract
    formantDispersion = mean(diff(unlist(lapply (exactFormants, function(f)f$freq))))
    vocalTract_length = ifelse (is.numeric(formantDispersion),
                                speedSound/2/formantDispersion,
                                speedSound/4/exactFormants$f1$freq)
  }
  if (length(exactFormants[[1]])<2){ # ie if is.na(exactFormants) or if there's something wrong with it, we fall back on vocalTract_length to make a schwa
    freq=speedSound/4/vocalTract_length
    exactFormants=list(
      'f1' = data.frame('time' = 0, 'freq'=freq, 'amp'=30, 'width'=50+(log2(freq)-5)*20) # freq = 50:5000; a = 50+(log2(freq)-5)*20; plot(freq, a)
    )
  }

  # upsample to the length of fft steps
  nPoints = max(unlist(lapply(exactFormants, nrow)))
  exactFormants_upsampled = lapply(exactFormants, function(f){
    temp = apply(f, 2, function(y) {
      if (nrow(f)>1){
        out = spline(approx(y,n=nPoints+2^smoothLinear_factor,x=f$time)$y, n=nc)$y # just spline produces imprecise, overly smoothed curves. Loess is just too slow for this. So we apply linear extrapolation to formant values first, to get a fairly straight line between anchors, and THEN smooth it out with spline
      } else {
        out = rep(y,nc)
      }
      out
    })
    if (class(temp)=='numeric'){ # if nc==1, we get numeric instead of matrix and need to convert
      temp = t(as.matrix(temp))
    }
    temp
  }) # check that class(exactFormants_upsampled[[1]]) == 'matrix'

  ## Stochastic part (only for temperature > 0)
  if (temperature>0){
    # create a few new, relatively high-frequency "pseudo-formants" moving together with the real formants
    if (is.null(vocalTract_length) && length(exactFormants)>1){
      ff = unlist(lapply(exactFormants, function(x)x$freq[1]))
      formantDispersion = mean(c(ff[1],diff(ff)))
    } else if (!is.null(vocalTract_length)){
      formantDispersion = 2*speedSound/(4*vocalTract_length)
    } else {
      formantDispersion = NA # making sdG also NA, ie extra formants will not be added
    }
    sdG = formantDispersion*temperature*formantDispersion_per_temp
    nFormants = length(exactFormants_upsampled)
    freq_max = max(exactFormants_upsampled[[nFormants]][,'freq'])

    if (!is.na(sdG) && extraFormants_ampl>0){
      while(freq_max<(samplingRate/2-1000)){ # don't add extra formants close to Nyquist to avoid artifacts
        rw = getRandomWalk(len=nc, rw_range=temperature*formantDrift_per_temp, rw_smoothing=0, trend=0) # for nPoints==1, returns one number close to 1
        if (length(rw)>1) rw = rw-mean(rw)+1 # for actual random walks, make sure mean is 1
        temp = data.frame ('time'=exactFormants_upsampled[[1]][,'time'],
                           'freq'=exactFormants_upsampled[[nFormants]][,'freq'] + round(rgamma(1, formantDispersion^2/sdG^2, formantDispersion/sdG^2)*rw))
        temp$amp = round(rgamma(1, (formantStrength/temperature)^2, extraFormants_ampl*formantStrength/(extraFormants_ampl*temperature)^2)*rw) # mean = extraFormants_ampl, sd = extraFormants_ampl*temperature
        temp$width = 50+(log2(temp$freq)-5)*20 # visualize: freq=50:8000; plot(freq, 50+(log2(freq)-5)*20)
        exactFormants_upsampled[[nFormants+1]] = temp
        nFormants = nFormants+1
        freq_max = max(exactFormants_upsampled[[nFormants]]$freq)
      }
    }

    # wiggle both user-specified and stochastically added formants
    for (f in 1:nFormants){
      for (c in 2:4){ # wiggle freq, ampl and bandwidth independently
        rw = getRandomWalk(len=nc, rw_range=temperature*formantDrift_per_temp, rw_smoothing=0.3, trend=rnorm(1)) # for nc==1, returns one number close to 1
        if (length(rw)>1) {rw = rw-mean(rw)+1} # for actual random walks, make sure mean is 1
        exactFormants_upsampled[[f]][,c] = exactFormants_upsampled[[f]][,c] * rw
      }

      #   # Humans only (or specify permittedValues for your animal species): make sure frequencies for each formant are within some reasonable bounds
      #   permitted_range = c(permittedValues[paste0('f',f,'_freq'),'low'], permittedValues[paste0('f',f,'_freq'),'high'])
      #   exactFormants_upsampled[[f]]$freq[exactFormants_upsampled[[f]]$freq < permittedValues[paste0('f',f,'_freq'),'low']] = permittedValues[paste0('f',f,'_freq'),'low']
      #   exactFormants_upsampled[[f]]$freq[exactFormants_upsampled[[f]]$freq > permittedValues[paste0('f',f,'_freq'),'high']] = permittedValues[paste0('f',f,'_freq'),'high']
    } # end of wiggling existing formants
  } # end of if temperature>0

  ## Deterministic part
  # convert formant freqs and widths from Hz to bins
  bin_width = samplingRate/2/nr # Hz
  bin_freqs = seq(bin_width/2, samplingRate/2, length.out=nr) # Hz
  for (f in 1:length(exactFormants_upsampled)){
    exactFormants_upsampled[[f]][,'freq'] = (exactFormants_upsampled[[f]][,'freq']-bin_width/2)/bin_width+1 # frequencies expressed in bin indices (how many bin widths above the central frequency of the first bin)
    exactFormants_upsampled[[f]][,'width'] = exactFormants_upsampled[[f]][,'width'] / bin_width
  }

  # mouth opening
  if (length(mouthAnchors)<1 | sum(is.na(mouthAnchors)) > 0){
    mouthOpening_upsampled = rep(0.5, nc) # defaults to mouth half-open the whole time - sort of hanging loosely agape ;))
    mouthOpen_binary = rep(1,nc)
  } else {
    mouthOpening_upsampled = getSmoothContour(len=nc, anchors=mouthAnchors, ampl_floor=permittedValues['mouthOpening','low'], ampl_ceiling=permittedValues['mouthOpening','high'], plot=FALSE)
    mouthOpening_upsampled[mouthOpening_upsampled<mouthOpening_threshold] = 0
    mouthOpen_binary = ifelse(mouthOpening_upsampled>0, 1, 0)
  }
  # plot(mouthOpening_upsampled, type='l')

  # adjust formants for mouth opening
  if (!is.null(vocalTract_length) && is.finite(vocalTract_length)){ # is.finite() returns F for NaN, NA, ±inf, etc
    adjustment_hz = (mouthOpening_upsampled-0.5)*speedSound/(4*vocalTract_length) # speedSound = 35400 cm/s, speed of sound in warm air. The formula for mouth opening is modified from Moore (2016) "A Real-Time Parametric General-Purpose Mammalian Vocal Synthesiser". mouthOpening=.5 gives no modification (neutral, "default" position)
    adjustment_bins = (adjustment_hz-bin_width/2)/bin_width+1
  } else {
    adjustment_bins = 0
  }
  for (f in 1:length(exactFormants_upsampled)){
    exactFormants_upsampled[[f]][,'freq'] = exactFormants_upsampled[[f]][,'freq'] + adjustment_bins
    exactFormants_upsampled[[f]][,'freq'] [exactFormants_upsampled[[f]][,'freq'] < 1] = 1 # force each formant frequency to be positive
  }

  # nasalize the parts with closed mouth: see Hawkins & Stevens (1985); http://www.cslu.ogi.edu/tutordemos/SpectrogramReading/cse551html/cse551/node35.html
  nasalizedIdx = which(mouthOpen_binary==0) # or specify a separate nasalization contour
  if (length(nasalizedIdx)>0){
    # add a pole
    exactFormants_upsampled$fnp = exactFormants_upsampled$f1
    exactFormants_upsampled$fnp[,'amp'] = 0
    exactFormants_upsampled$fnp[nasalizedIdx,'amp'] = exactFormants_upsampled$f1[nasalizedIdx,'amp'] * 2/3
    exactFormants_upsampled$fnp[nasalizedIdx,'width'] = exactFormants_upsampled$f1[nasalizedIdx,'width'] * 2/3
    exactFormants_upsampled$fnp[nasalizedIdx,'freq'] = ifelse(exactFormants_upsampled$f1[nasalizedIdx,'freq']>550/bin_width, exactFormants_upsampled$f1[nasalizedIdx,'freq']-250/bin_width, exactFormants_upsampled$f1[nasalizedIdx,'freq']+250/bin_width) # 250 Hz below or above F1, depending on whether F1 is above or below 550 Hz
    # add a zero
    exactFormants_upsampled$fnz = exactFormants_upsampled$f1
    exactFormants_upsampled$fnz[,'amp'] = 0
    exactFormants_upsampled$fnz[nasalizedIdx,'amp'] = -exactFormants_upsampled$f1[nasalizedIdx,'amp'] * 2/3
    exactFormants_upsampled$fnz[nasalizedIdx,'freq'] = (exactFormants_upsampled$fnp[nasalizedIdx,'freq']+exactFormants_upsampled$f1[nasalizedIdx,'freq'])/2 # midway between f1 and fnp
    exactFormants_upsampled$fnz[nasalizedIdx,'width'] = exactFormants_upsampled$fnp[nasalizedIdx,'width']
    # modify f1
    exactFormants_upsampled$f1[nasalizedIdx,'amp'] = exactFormants_upsampled$f1[nasalizedIdx,'amp'] * 4/5
    exactFormants_upsampled$f1[nasalizedIdx,'width'] = exactFormants_upsampled$f1[nasalizedIdx,'width'] * 5/4
  }

  # create a "spectrum"-shaped filter matrix
  spectralEnvelope = matrix(0, nrow=nr, ncol=nc)
  for (f in 1:length(exactFormants_upsampled)){
    mg = exactFormants_upsampled[[f]][,'freq'] # mean of gamma distribution (vector of length nc)
    sdg = exactFormants_upsampled[[f]][,'width'] # sd of gamma distribution (vector of length nc)
    shape = mg^2/sdg^2
    rate = mg/sdg^2
    formant = matrix(0, nrow=nr, ncol=nc)
    for (c in 1:nc){
      formant[,c] = dgamma (1:nr, shape[c], rate[c])
      formant[,c] = formant[,c]/max(formant[,c]) * exactFormants_upsampled[[f]][c,'amp'] # plot (formant[,115], type='l')
    }
    spectralEnvelope = spectralEnvelope + formant
  }
  spectralEnvelope = spectralEnvelope*formantStrength # plot(spectralEnvelope[,1], type='l')

  # add lip radiation when the mouth is open
  lip_dB = rolloff_lip*log2(1:nr) # vector of length nr
  # plot (lip_dB)  # lip_dB[1000]-lip_dB[500]; lip_dB[500]-lip_dB[250]; lip_dB[250]-lip_dB[125]
  for (c in 1:nc){
    spectralEnvelope[,c] = (spectralEnvelope[,c] + lip_dB*mouthOpen_binary[c]) * 2^(mouthOpening_upsampled[c]*amplBoost_openMouth_dB/10)
  }
  # image(t(spectralEnvelope))
  # range(spectralEnvelope)

  return (2^(spectralEnvelope/10))
}
