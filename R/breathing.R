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
#' noise = getBreathing(len = samplingRate, rolloff_breathing = 0, samplingRate = samplingRate)
#' # playme (noise, samplingRate = samplingRate)
#' # 1 s of noise with rolloff -6 dB
#' noise = getBreathing(len = samplingRate, rolloff_breathing = -6, samplingRate = samplingRate)
#'
#' # To create a sibilant [s], specify a single strong, broad formant at ~7 kHz:
#' windowLength_points = 1024
#' filter_breathing = getSpectralEnvelope(nr=windowLength_points/2, nc=1, samplingRate=samplingRate,
#'   exactFormants=list('f1'=data.frame(time=0, freq=7000, amp=50, width=2000)))
#' noise = getBreathing(len = samplingRate, rolloff_breathing = -12,
#'   samplingRate = samplingRate, filter_breathing=filter_breathing)
#' # plot (filter_breathing, type='l')
#' # playme (noise, samplingRate = samplingRate)
#'
#' # low-frequency, wind-like noise
#' filter_breathing = getSpectralEnvelope(nr=windowLength_points/2, nc=1,
#'   rolloff_lip=0, samplingRate=samplingRate,
#'   exactFormants=list('f1'=data.frame(time=0, freq=150, amp=30, width=90)))
#' noise = getBreathing(len = samplingRate, rolloff_breathing = -12,
#'   samplingRate = samplingRate, filter_breathing=filter_breathing)
getBreathing = function(len, breathingAnchors=data.frame('time'=c(0,300), 'ampl'=c(throwaway_dB,throwaway_dB)), rolloff_breathing=-6, attackLen=10, windowLength_points=1024, samplingRate=44100, overlap=75, filter_breathing=NA){
  # convert anchor amplitudes from dB to linear multipliers
  breathingAnchors$ampl = 2^(breathingAnchors$ampl/10)

  # convert anchors to a smooth contour of breathing amplitudes
  breathingStrength = getSmoothContour(len=len, anchors=breathingAnchors, ampl_floor=permittedValues['breathing_ampl','low'], ampl_ceiling=permittedValues['breathing_ampl','high'], samplingRate=samplingRate, plot=F)   # plot(breathingStrength)
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
  breathing = fadeInOut(breathing, do_fadeIn=T, do_fadeOut=T, length_fade=floor(attackLen*samplingRate/1000))  # add attack
  # plot(breathing, type='l')
  # playme(breathing, samplingRate=samplingRate); spectro_denoised(breathing, samplingRate=samplingRate)
  # seewave::meanspec(breathing, f=samplingRate, wl=windowLength_points, dB='max0')
  return (breathing)
}
