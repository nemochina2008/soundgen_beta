# Functions for generating excitation source: either noise with generateNoise() or harmonics with generateHarmonics()

#' Generate noise
#'
#' Internal soundgen function.
#'
#' Generates white noise of length \code{len} and with spectrum defined by
#' exponential decay \code{rolloff_breathing} and/or a specified filter
#' \code{filter_breathing}. Algorithm: paints a spectrum with desired
#' characteristics, sets phase to zero, and generates a time sequence via
#' inverse FFT. Soundgen generates aspiration noise (breathing) using this
#' function. Noise can then be used as an additional source to be added to the
#' glottal source AFTER the glottal source has been formant-filtered, or BEFORE
#' formant-filtering for glottal breathing noise.
#' @param len length of output
#' @param breathingAnchors a dataframe specifying the amplitude envelope of
#'   output. $time: timing of aspiration noise, ms c(start,finish) relative to
#'   voiced part, eg c(-100,500) means breathing starts 100 ms before the voiced
#'   part and lasts until 500 ms into the voiced part (eg total duration of
#'   breathing = 500 - (-100) = 600 ms). breathingAnchors$ampl: the amount of
#'   aspiration noise at the given time anchors (to be smoothed). throwaway_dB =
#'   no breathing, 0 = as strong as the voiced (harmonic) part
#' @param rolloff_breathing desired spectral slope of white noise (exponential
#'   decay analogous to \code{rolloff_exp} in \code{\link{getRolloff}})
#' @param attackLen duration of fade-in and fade-out at the beginning and end of
#'   output, ms
#' @param windowLength_points fft window length, points
#' @param overlap overlap of fft windows
#' @param filter_breathing (optional): in addition to using rolloff_breathing,
#'   we can provide the exact filter - a vector of length windowLength_points/2
#'   or, if we want moving formants, a matrix with windowLength_points/2 rows
#'   and an arbitrary number of columns
#' @param samplingRate sampling frequency (Hz)
#' @examples
#' # 1 s of white noise
#' samplingRate = 16000
#' noise = soundgen:::generateNoise(len = samplingRate,
#'   rolloff_breathing = 0, samplingRate = samplingRate)
#' # playme (noise, samplingRate = samplingRate)
#' # 1 s of noise with rolloff -6 dB
#' noise = soundgen:::generateNoise(len = samplingRate,
#'   rolloff_breathing = -6, samplingRate = samplingRate)
#'
#' # To create a sibilant [s], specify a single strong, broad formant at ~7 kHz:
#' windowLength_points = 1024
#' filter_breathing = soundgen:::getSpectralEnvelope(nr=windowLength_points/2,
#'   nc=1, samplingRate=samplingRate,
#'   exactFormants=list('f1'=data.frame(time=0, freq=7000, amp=50, width=2000)))
#' noise = soundgen:::generateNoise(len = samplingRate, rolloff_breathing = -12,
#'   samplingRate = samplingRate, filter_breathing=filter_breathing)
#' # plot (filter_breathing, type='l')
#' # playme (noise, samplingRate = samplingRate)
#'
#' # low-frequency, wind-like noise
#' filter_breathing = soundgen:::getSpectralEnvelope(nr=windowLength_points/2,
#'   nc=1, rolloff_lip=0, samplingRate=samplingRate,
#'   exactFormants=list('f1'=data.frame(time=0, freq=150, amp=30, width=90)))
#' noise = soundgen:::generateNoise(len = samplingRate, rolloff_breathing = -12,
#'   samplingRate = samplingRate, filter_breathing=filter_breathing)
generateNoise = function(len, breathingAnchors=data.frame('time'=c(0,300), 'ampl'=c(throwaway_dB,throwaway_dB)), rolloff_breathing=-6, attackLen=10, windowLength_points=1024, samplingRate=44100, overlap=75, filter_breathing=NA){
  # convert anchor amplitudes from dB to linear multipliers
  breathingAnchors$ampl = 2^(breathingAnchors$ampl/10)

  # convert anchors to a smooth contour of breathing amplitudes
  breathingStrength = getSmoothContour(len=len, anchors=breathingAnchors, ampl_floor=permittedValues['breathing_ampl','low'], ampl_ceiling=permittedValues['breathing_ampl','high'], samplingRate=samplingRate, plot=FALSE)   # plot(breathingStrength)
  if (is.na(breathingStrength)){
    return (rep(0,len))
  }

  # set up spectral filter
  step = seq(1, len+windowLength_points, by=windowLength_points-(overlap*windowLength_points/100)) # len+windowLength_points gives us two extra windows, since otherwise the sequence is a bit shorter than needed after i-fft
  nr = windowLength_points/2
  nc = length(step)
  if (is.na(filter_breathing)) {
    filter_breathing = matrix(rep (1, nr), nrow=1)
    filterRowIdx = rep(1, nc)
  } else {
    filterRowIdx = round(seq(1, ncol(filter_breathing), length.out=nc))
  }
  filter_breathing = apply(filter_breathing, 2, function(x) x * 2^(rolloff_breathing/10*log2(1:nr))) # modify the exact filter (if provided) by adding the specified basic exponential rolloff
  # plot(filter_breathing[,1], type='l')

  # instead of synthesizing the time series and then doing fft-ifft, we can simply synthesize spectral noise, convert to complex (setting imaginary=0), and then do inverse FFT just once
  z1 = matrix(as.complex(runif(nr*nc)), nrow=nr, ncol=nc) # set up spectrum
  z1_filtered = apply(matrix(1:ncol(z1)), 1, function(x)z1[,x]*filter_breathing[,filterRowIdx[x]]) # multiply by filter
  breathing = as.numeric (seewave::istft(z1_filtered, f=samplingRate, ovlp=overlap, wl=windowLength_points, output="matrix")) # inverse FFT
  breathing = matchLengths(breathing, len=len) # pad with 0s or shorten to the required length
  breathing = breathing/max(breathing) * breathingStrength # normalize
  breathing = fadeInOut(breathing, do_fadeIn=TRUE, do_fadeOut=TRUE, length_fade=floor(attackLen*samplingRate/1000))  # add attack
  # plot(breathing, type='l')
  # playme(breathing, samplingRate=samplingRate); spectro_denoised(breathing, samplingRate=samplingRate)
  # seewave::meanspec(breathing, f=samplingRate, wl=windowLength_points, dB='max0')
  return (breathing)
}



#' Generate harmonics
#'
#' Internal soundgen function.
#'
#' Returns one continuous, unfiltered, voiced syllable consisting of several
#' sine waves.
#' @param pitch a contour of fundamental frequency (numeric vector). NB: for
#'   computational efficiency, provide the pitch contour at a reduced sampling
#'   rate pitchSamplingRate, eg 3500 points/s. The pitch contour will be
#'   upsampled before synthesis.
#' @inheritParams generateBout
#' @examples
#' pitch=soundgen:::getSmoothContour(len=3500,
#'   anchors=data.frame('time'=c(0,1), 'ampl'=c(200,300)))
#' plot(pitch)
#' sound = soundgen:::generateHarmonics(pitch, samplingRate=16000)
#' # playme(sound, samplingRate=16000) # no formants yet
generateHarmonics = function(pitch, attackLen=50, noiseAmount=0, noiseIntensity=0, jitterDep=0, jitterLength_ms=1, vibratoFreq=100, vibratoDep=0, shimmerDep=0, creakyBreathy=0, rolloff_exp=-18, rolloff_exp_delta=-2, adjust_rolloff_per_kHz=-6, quadratic_delta=0, quadratic_nHarm=3, formantStrength=1, temperature=0, min_epoch_length_ms=300, g0=100, sideband_width_hz=0, rolloff_lip=6, trill_dep=0, trill_freq=30, amplAnchors=NA, overlap=75, windowLength_points=2048, samplingRate=44100, pitch_floor=75, pitch_ceiling=3500, pitchSamplingRate=3500) {
  ## PRE-SYNTHESIS EFFECTS (NB: the order in which effects are added is NOT arbitrary!)
  # vibrato (performed on pitch, not pitch_per_gc!)
  if (vibratoDep>0){
    vibrato = 2^(sin(2*pi*(1:length(pitch))*vibratoFreq/pitchSamplingRate) * vibratoDep/12) # plot(vibrato[], type='l')
    pitch = pitch * vibrato
  } # plot (pitch, type='l')

  # transform f0 per s to f0 per glottal cycle
  gc = getGlottalCycles(pitch, samplingRate=pitchSamplingRate) # our "glottal cycles"
  pitch_per_gc = pitch[gc]
  nGC = length(pitch_per_gc)

  # generate a short amplitude contour to adjust rolloff_exp per glottal cycle
  if (!is.na(amplAnchors) && length(which(amplAnchors$ampl < -throwaway_dB))>0){
    amplContour = getSmoothContour(anchors=amplAnchors, len=nGC, ampl_floor=0, ampl_ceiling=-throwaway_dB, samplingRate=samplingRate) # plot (amplContour, type='l')
    amplContour = amplContour/abs(throwaway_dB) - 1
    rolloff_exp_ampl = amplContour * rolloff_per_ampl
  } else {
    rolloff_exp_ampl = 0
  }

  # get a random walk for intra-syllable variation
  if (temperature > 0){
    rw = getRandomWalk (len=nGC, rw_range=temperature, trend=c(randomWalk_trendStrength,-randomWalk_trendStrength), rw_smoothing=.3) # plot (rw, type='l')
    rw_0_100 = rw-min(rw); rw_0_100 = rw_0_100/max(rw_0_100)*100 # plot (rw_0_100, type='l')
    rw_bin = getBinaryRandomWalk (rw_0_100, noiseAmount=noiseAmount, minLength=ceiling(min_epoch_length_ms/1000*pitch_per_gc)) # minLength is min_epoch_length_ms / period_ms, where period_ms = 1000/pitch_per_gc
    # plot(rw_bin)
    rw = rw - mean(rw) + 1 # change mean(rw) to 1
    vocalFry_on = rw_bin>0 # when is vocal fry on? For ex., rw_bin==1 sets vocal fry on only in regime 1, while rw_bin>0 sets vocal fry on in regimes 1 and 2 (ie together with jitter)
    jitter_on = shimmer_on = rw_bin==2
  } else {
    rw = rep(1, nGC)
    vocalFry_on = jitter_on = shimmer_on = rep(1, nGC)
  }

  # calculate jitter (random variation of F0)
  if (jitterDep>0 & noiseAmount>0){
    ratio = pitch_per_gc * jitterLength_ms / 1000 # the number of gc that make up one jitter period (vector of length nGC)
    idx = 1
    i = 1
    while (i < nGC){
      i = tail(idx,1)+ratio[i]
      idx = c(idx, i)
    }
    idx = round(idx)
    idx = idx[idx <= nGC] # pitch for these gc will be wiggled
    idx = unique(idx)

    jitter = 2^(rnorm(n=length(idx), mean=0, sd=jitterDep/12)*rw[idx]*jitter_on[idx]) # plot(jitter, type='l')
    # jitter_per_gc = approx(jitter, n=nGC, x=idx, method='constant')$y # plot(jitter_per_gc, type='l')
    jitter_per_gc = spline(jitter, n=nGC, x=idx)$y # plot(jitter_per_gc, type='l')
    # jitter = 2^(rnorm(n=length(pitch_per_gc), mean=0, sd=jitterDep/12)*rw*jitter_on) # plot(jitter, type='l')
    pitch_per_gc = pitch_per_gc * jitter_per_gc # plot(pitch_per_gc, type='l')
  }

  # calculate random drift of F0 (essentially the same as jitter but slow, ie with smoothing)
  if (temperature>0) {
    # # illustration of the effects of temperature and number of gc's on the amount of smoothing applied to the random drift of f0
    # library(reshape2)
    # library(plot3D)
    # df = expand.grid(temperature = seq(0,1,length.out=30), n_gc = seq(1,1000,length.out=30))
    # df$rw_smoothing = .9 - df$temperature/8 - 1.2/(1+exp(-.008*(df$n_gc-10)))+.6 # 10 gc is "neutral"
    # out_pred = as.matrix(dcast(df, temperature~n_gc, value.var="rw_smoothing"))
    # rownames(out_pred) = seq(0,1,length.out=30)
    # out_pred = out_pred[,-1]
    # persp3D (as.numeric(rownames(out_pred)), as.numeric(colnames(out_pred)), out_pred, theta=40, phi=50, zlab='rw_smoothing', xlab='Temperature', ylab='# of glottal cycles', colkey=FALSE, ticktype='detailed', cex.axis=0.75)
    drift = getRandomWalk (len=length(pitch_per_gc), rw_range=temperature*pitchDrift_per_temp + length(pitch_per_gc)/1000/12, rw_smoothing=.9 - temperature*pitchDriftWiggle_per_temp - 1.2/(1+exp(-.008*(length(pitch_per_gc)-10)))+.6, method='spline') # we get a separate random walk for this slow drift of intonation. Its smoothness vs wiggleness depends on temperature and duration (in glottal cycles). For ex., temperature*2 means that pitch will vary within one octave if temperature==1
    drift = 2^(drift - mean(drift)) # plot (drift, type='l')
    pitch_per_gc = pitch_per_gc * drift  # plot(pitch_per_gc, type='l')
  }

  # as a result of adding pitch effects, F0 might have dropped to indecent values, so we double-check and flatten
  pitch_per_gc[pitch_per_gc>pitch_ceiling] = pitch_ceiling
  pitch_per_gc[pitch_per_gc<pitch_floor] = pitch_floor

  # prepare the harmonic stack
  nHarmonics=ceiling(samplingRate/2-min(pitch_per_gc))/min(pitch_per_gc) # calculate the number of harmonics to generate (from lowest pitch to nyquist)
  rolloff_source = getRolloff(pitch_per_gc=pitch_per_gc, nHarmonics=nHarmonics, rolloff_exp=(rolloff_exp+rolloff_exp_ampl)*rw^3, rolloff_exp_delta=rolloff_exp_delta*rw^3, adjust_rolloff_per_kHz=adjust_rolloff_per_kHz*rw, quadratic_delta=quadratic_delta, quadratic_nHarm=quadratic_nHarm, samplingRate=samplingRate) # NB: this whole pitch_per_gc trick is purely for computational efficiency - the entire pitch contour can be fed in, but then it takes up to 1 s / s audio
  # image(t(rolloff_source))

  # add shimmer (random variation in amplitude)
  if (shimmerDep>0 & noiseAmount>0){
    shimmer = 2^(rnorm (n=ncol(rolloff_source), mean=0, sd=shimmerDep/100)*rw*shimmer_on) # plot(shimmer, type='l')
    rolloff_source = t(t(rolloff_source)*shimmer) # multiplies the first column of rolloff_source by shimmer[1], the second column by shimmer[2], etc
  }

  # add vocal fry (subharmonics)
  if (sideband_width_hz>0 & noiseAmount>0) {
    vocalFry = getVocalFry (rolloff=rolloff_source, pitch_per_gc=pitch_per_gc, g0=g0*rw^4, sideband_width_hz=sideband_width_hz*rw^4*vocalFry_on, min_epoch_length_ms=min_epoch_length_ms)
    rolloff_source = vocalFry$rolloff # list of matrices
    epochs = vocalFry$epochs # dataframe
  } else { # if we don't need to add vocal fry
    rolloff_source = list(rolloff_source)
    epochs = data.frame ('start'=1, 'end'=length(pitch_per_gc))
  }

  ## WAVEFORM GENERATION
  up = upsample(pitch_per_gc, samplingRate=samplingRate) # upsample pitch contour to full resolution (samplingRate). Uses a more sophisticated but still very fast version of linear interpolation, which takes into account the variable length of glottal cycles
  pitch_upsampled = up$pitch
  gc_upsampled = up$gc
  integr = cumsum(pitch_upsampled)/samplingRate # plot (integr[1:10000], type='l')
  waveform = 0

  # synthesize one epoch at a time and join by cross-fading
  for (e in 1:nrow(epochs)){
    idx_gc_up = gc_upsampled[epochs$start[e] : (epochs$end[e]+1)]
    idx_up = min(idx_gc_up):max(idx_gc_up)
    waveform_epoch = rep(0, length(idx_up))
    integr_epoch = integr[idx_up]
    rolloff_epoch = rolloff_source[[e]] # rolloff_source MUST be a list!

    for (h in 1:nrow(rolloff_epoch)){ # NB: rolloff_source can have fewer harmonics that nHarmonics, since weak harmonics are discarded for computational efficiency. Or it can have a lot more than nHarmonics, if we add vocal fry
      times_f0 = as.numeric(rownames(rolloff_epoch)[h]) # freq of harmonic h as a multiple of f0
      am_upsampled = approx(rolloff_epoch[h,], n=length(idx_up), x=idx_gc_up[-length(idx_gc_up)])$y # plot(am_upsampled, type='l')
      waveform_epoch = waveform_epoch + sin(2*pi*integr_epoch*times_f0) * am_upsampled # the actual waveform synthesis happens in this one line
      # # normalize amplitude to avoid abrupt amplitude jumps as subharmonics regime changes
      # if (e>1){
      #   max_prev = max(tail(ampl, samplingRate*.1)) # over the last ... ms of the previous epoch
      #   max_new = max(head(ampl_epoch, samplingRate*.1)) # over the first ... ms of the new epoch
      #   ampl_epoch = ampl_epoch * max_prev/max_new
      #   RMS_prev = sqrt(mean(tail(ampl, samplingRate*.1)^2)) # over the last ... ms of the previous epoch
      #   RMS_new = sqrt(mean(head(ampl_epoch, samplingRate*.1)^2)) # over the first ... ms of the new epoch
      #   ampl_epoch = ampl_epoch * RMS_prev/RMS_new
      # }
    }
    waveform = crossFade(waveform, waveform_epoch, length_ms=15, samplingRate=samplingRate) # longer length_ms provides for smoother transitions, but it shortens the resulting sound. Short transitions preserve sound duration but may create a click where two epochs are joined
  }
  # sum(is.na(waveform))
  # plot(waveform[], type='l')
  # spectro_denoised (waveform, samplingRate=samplingRate, osc=TRUE); playme(waveform, samplingRate=samplingRate) # meanspec(waveform, f=samplingRate)


  ## POST-SYNTHESIS EFFECTS
  # apply amplitude envelope and normalize to be on the same scale as breathing
  if (!is.na(amplAnchors) && length(which(amplAnchors$ampl < -throwaway_dB))>0){
    amplAnchors$ampl = 2^(amplAnchors$ampl/10) # convert from dB to linear multiplier
    amplEnvelope = getSmoothContour(anchors=amplAnchors, len=length(waveform), ampl_floor=0, samplingRate=samplingRate) # plot (amplEnvelope, type='l')
    waveform = waveform * amplEnvelope
  }
  waveform = waveform/max(waveform)

  # add attack
  if (attackLen>0){
    waveform = fadeInOut(waveform, length_fade=floor(attackLen*samplingRate/1000)) # plot(waveform, type='l')
  }

  # pitch drift is accompanied by amplitude drift
  if (temperature>0){
    drift_upsampled = approx(drift, n=length(waveform), x=gc_upsampled[-length(gc_upsampled)])$y # plot(drift_upsampled, type='l')
  } else {
    drift_upsampled = 1
  }
  waveform = waveform * drift_upsampled
  # playme(waveform, samplingRate=samplingRate); spectro_denoised (waveform, samplingRate=samplingRate)
  return (waveform)
}

