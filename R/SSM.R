#' Self-similarity matrix
#'
#' Calculates the self-similarity matrix and novelty vector for a sound.
#' @param x path to a .wav file or a vector of amplitudes with specified
#'   samplingRate
#' @param samplingRate sampling rate of \code{x} (only needed if
#'   \code{x} is a numeric vector, rather than a .wav file)
#' @param windowLength length of fft window (ms)
#' @param step fft step (ms). Defaults to \code{windowLength / 4}
#' @param maxfreq highest band edge of mel filters (Hz). Defaults to \code{samplingRate / 2}. See \code{\link[tuneR]{melfcc}}
#' @param nbands number of warped spectral bands to use. Defaults to \code{100 * windowLength / 20} See \code{\link[tuneR]{melfcc}}
#' @param MFCC_to_use mel-frequency cepstral coefficients to use
#' @param input either MFCCs ("cepstrum") or mel-filtered spectrum ("audiogram")
#' @param normalizeInput if TRUE, each fft frame is normalized by max power. NB: do this if using cosine similarity with spectrum (audiogram)
#' @param simil_measure method for comparing frames: either cosine similarity or Pearson's correlation
#' @param plot if TRUE, produces a SSM plot
#' @param returnSSM if TRUE, returns the SSM
#' @param novelty_col the color of novelty contour
#' @param novelty_lwd the linewidth of novelty contour
#' @param kernel_ms size of checkerboard kernel for calculating novelty (the larger, the more global vs. local the novelty)
#' @param kernel_sd SD of checkerboard kernel for calculating novelty
#' @param win window for averaging SSM (reduces its size). Forced to odd: 1 3 5 etc
#' @return Returns
#' @export
#' @examples
#' s = ssm(x)
ssm = function(x,
               samplingRate = NULL,
               windowLength = 40,
               step = NULL,
               maxfreq = NULL,
               nbands = NULL,
               MFCC_to_use = 2:13,
               input = c('cepstrum', 'audiogram')[1],
               normalizeInput = FALSE,
               simil_measure = c('cosine', 'correlation')[1],
               plot = T,
               returnSSM = T,
               novelty_col = 'black',
               novelty_lwd = 3,
               kernel_ms = 200,
               kernel_sd = .5,
               win = 3,
               ...) {
  ## import a sound
  if (class(x) == 'character') {
    sound = tuneR::readWave(x)
    samplingRate = sound@samp.rate
  }  else if (class(x) == 'numeric' & length(x) > 1) {
    if (is.null(samplingRate)) {
      stop ('Please specify samplingRate, eg 44100')
    } else {
      sound = tuneR::Wave(left = x, samp.rate = samplingRate, bit = 16)
      sound = tuneR::normalize(sound, unit = '32')
    }
  }

  ## set pars
  duration = length(sound) / samplingRate
  frame_points = samplingRate * windowLength / 1000
  kernel_size = round(kernel_ms * samplingRate / 1000 / frame_points /
                        2) * 2  # kernel size in frames, guaranteed to be even
  if (is.null(nbands)) {
    nbands = 100 * windowLength / 20
  }
  if (is.null(step)) {
    step = windowLength / 4
  }
  if (is.null(maxfreq)) {
    maxfreq = floor(samplingRate / 2)  # Nyquist
  }

  ## compute mel-filtered spectrum and MFCCs
  mel = tuneR::melfcc(
    sound,
    wintime = windowLength / 1000,
    hoptime = step / 1000,
    maxfreq = maxfreq,
    nbands = nbands,
    spec_out = T,
    numcep = max(MFCC_to_use)
  )
  if (input == 'cepstrum') {
    # the first cepstrum presumably makes no sense with amplitude normalization
    # (?), and it overestimates the similarity of different frames
    target_spec = t(mel$cepstra)[MFCC_to_use, ]
  } else if (input == 'audiogram') {
    target_spec = t(mel$aspectrum)
  }

  ## compute self-similarity matrix
  s = selfsim(
    target_spec,
    normalizeInput = normalizeInput,
    simil_measure = simil_measure,
    win = win
  )
  # s = zeroOne(s^2)  # hist(s)

  ## compute novelty
  novelty = getNovelty(s, kernel_size = kernel_size, kernel_sd = kernel_sd)

  ## plot
  if (plot) {
    spec = zeroOne(log(mel$pspectrum) ^ 2)
    op = par(c('mar', 'xaxt', 'yaxt', 'mfrow')) # save user's original pars
    layout(matrix(c(2, 1), nrow = 2, byrow = TRUE), heights = c(2, 1))
    par(mar = c(5.1, 4.1, 0, 2.1),
        xaxt = 's',
        yaxt = 's')
    # spectrogram
    seewave::filled.contour.modif2 (
      x = seq(0, duration, length.out = nrow(spec)),
      y = seq(
        0,
        (samplingRate / 2) - (samplingRate / windowLength_points),
        length.out = ncol(spec)
      ) / 1000,
      z = spec,
      levels = seq(0, 1, length = 30),
      color.palette = seewave::spectro.colors,
      xlab = 'Time, ms',
      ylab = 'kHz',
      ylim = c(0, maxfreq / 1000)
    )
    # novelty
    lines(
      x = seq(0, duration, length.out = length(novelty)),
      y = novelty / max(novelty) * maxfreq / 1000,
      col = novelty_col,
      type = 'b',
      lwd = novelty_lwd,
      pch = 16
    )
    axis(side = 1, labels = TRUE)
    par(mar = c(0, 4.1, 2.1, 2.1),
        xaxt = 'n',
        yaxt = 's')
    xlab = ''
    # SSM
    timestamps_ssm = seq(0, duration, length.out = nrow(s))
    seewave::filled.contour.modif2 (
      x = timestamps_ssm,
      y = timestamps_ssm,
      z = s,
      levels = seq(0, 1, length = 30),
      color.palette = seewave::spectro.colors,
      xlab = 'Time, ms',
      ylab = 'Time, ms',
      main = 'Self-similarity matrix'
    )
    # restore original pars
    par('mar' = op$mar, 'xaxt' = op$xaxt, 'yaxt' = op$yaxt, 'mfrow' = op$mfrow)
  }

  if (returnSSM) {
    return(list(ssm = s, novelty = novelty))
  }
}



#' Compute self-similarity
#'
#' Internal soundgen function.
#'
#' @param m input matrix such as a spectrogram
#' @inheritParams ssm
#' @return Returns a square self-similarity matrix.
selfsim = function(m,
                   normalizeInput = FALSE,
                   simil_measure = c('cosine', 'correlation')[1],
                   win = 1) {
  if (win %% 2 == 0) {
    win = max(ceiling(win / 2) * 2 - 1, 1)
  } # win must be odd
  if (win > ceiling(ncol(m) / 2)) {
    stop (
      paste(
        'win must be smaller than half the number of frames
        (for this file, this means win <', ceiling(ncol(m) / 2), ')'
      )
    )
  }

  # normalize input by column, if needed
  if (normalizeInput) {
    m = apply(m, 2, zeroOne)
  }

  # calculate windows for averaging self-similarity
  numWins = ceiling(ncol(m) / win)
  winIdx = round (seq(1, ncol(m) - win, length.out = numWins))

  # calculate self-similarity
  out = matrix(NA, nrow = length(winIdx), ncol = length(winIdx))
  rownames(out) = colnames(out) = winIdx
  for (i in 1:length(winIdx)) {
    for (j in 1:length(winIdx)) {
      i_low = winIdx[i]
      i_high = winIdx[i] + win - 1
      j_low = winIdx[j]
      j_high = winIdx[j] + win - 1

      if (simil_measure == 'cosine') {
        # http://stackoverflow.com/questions/6597005/cosine-similarity-between-two-vectors-in-language-r
        out[i, j] = mean (crossprod(m[, i_low:i_high], m[, j_low:j_high]) /
                            sqrt(crossprod(m[, i_low:i_high]) *
                                 crossprod(m[, j_low:j_high])),
                          na.rm = TRUE)
      } else if (simil_measure == 'correlation') {
        out[i, j] = mean (cor(m[, i_low:i_high], m[, j_low:j_high]), na.rm = TRUE)
      }
    }
  }
  out = zeroOne(out)
  return (out)
}


#' Checkerboard kernel
#'
#' Internal soundgen function.
#'
#' Prepares a square matrix \code{size x size} specifying a gaussian kernel for measuring novelty of self-similarity matrices.
#' @param size kernel size (points), prefereably an even number
#' @inheritParams ssm
#' @param plot if TRUE, shows a perspective plot of the kernel
#' @return Returns a square matrix with \code{size} rows and columns.
#' @examples
#' kernel = soundgen:::getCheckerboardKernel(size = 64, kernel_sd = 0.5, plot = TRUE)
#' dim(kernel)
getCheckerboardKernel = function(size,
                                 kernel_mean = 0,
                                 kernel_sd = 0.5,
                                 plot = FALSE) {
  x = seq(-1, 1, length.out = size)

  if (size < 50){
    # faster than mvtnorm::dmvnorm for small kernels
    kernel = matrix(NA, ncol = size, nrow = size)
    for (i in 1:nrow(kernel)) {
      for (j in 1:ncol(kernel)) {
        kernel[i, j] = dnorm(x[i], mean = kernel_mean, sd = kernel_sd) *
          dnorm(x[j], mean = kernel_mean, sd = kernel_sd)
      }
    }
  } else {
    sigma = diag(2) * kernel_sd
    kernel_long = expand.grid(x1 = x, x2 = x)
    kernel_long$dd = mvtnorm::dmvnorm(x = kernel_long,
                                      mean = c(kernel_mean, kernel_mean),
                                      sigma = sigma)
    kernel = reshape2::acast(data = kernel_long, formula = x2 ~ x1, value.var = 'dd')
  }

  # quadrant 0 to 3 o'clock
  kernel[1:(floor(size / 2)), (ceiling(size / 2) + 1):size] =
    -kernel[1:(floor(size / 2)), (ceiling(size / 2) + 1):size]
  # quadrant 6 to 9 o'clock
  kernel[(ceiling(size / 2) + 1):size, 1:(ceiling(size / 2))] =
    -kernel[(ceiling(size / 2) + 1):size, 1:(ceiling(size / 2))]

  kernel = kernel / max(kernel)
  if (plot) {
    persp (
      kernel,
      theta = -20,
      phi = 25,
      zlim = c(-1, 4),
      ticktype = 'detailed'
    )
  }
  return (kernel)
}


#' SSM novelty
#'
#' Internal soundgen function.
#'
#' Calculates novelty in a self-similarity matrix. See El Badawy, D., Marmaroli,
#' P., & Lissek, H. (2013). Audio Novelty-Based Segmentation of Music Concerts.
#' In Acoustics 2013 (No. EPFL-CONF-190844).
#' @param ssm self-similarity matrix, as produced by \code{\link{selfsim}}
#' @param kernel_size the size of gaussian kernel (points)
#' @param kernel_sd the SD of gaussian kernel
#' @return Returns a numeric vector of length \code{nrow(ssm)}
getNovelty = function(ssm, kernel_size, kernel_sd) {
  kernel = getCheckerboardKernel(size = kernel_size, sd = kernel_sd)
  ## pad matrix with size / 2 zeros, so that we can correlate it with the
  #  kernel starting from the very edge
  ssm_padded = matrix(0,
                      nrow = nrow(ssm) + kernel_size,
                      ncol = nrow(ssm) + kernel_size)
  # indices in the padded matrix where we'll paste the original ssm
  idx = c(kernel_size / 2 + 1, nrow(ssm_padded) - kernel_size / 2)
  # paste original. Now we have a padded ssm
  ssm_padded [idx[1]:idx[2], idx[1]:idx[2]] = ssm

  ## get novelty
  novelty = rep(NA, nrow(ssm))
  # for each point on the main diagonal, novelty = correlation between the checkerboard kernel and the ssm. See Badawy, "Audio novelty-based segmentation of music concerts"
  for (i in idx[1]:idx[2]) {
    n = (i - kernel_size / 2):(i + kernel_size / 2 - 1)
    novelty[i - kernel_size / 2] =  cor(as.vector(ssm_padded[n, n]),
                                        as.vector(kernel))
  }
  return (novelty)
}



#
#
# myfolder = '/home/allgoodguys/Documents/Studying/Lund_PhD/epistles/001_article_ratings/260sounds_wav'
# x = paste0(myfolder, '/', list.files(myfolder)[130])
# playme(x)
# ssm (x, windowLength=40, step=NULL, maxfreq=6000, nbands=NULL, MFCC_to_use=2:13, input=c('cepstrum','audiogram')[1], normalizeInput=FALSE, simil_measure=c('cosine','correlation')[1], plot=TRUE, returnSSM=FALSE, novelty_col='black', novelty_lwd=3, kernel_ms=200, kernel_sd=.5, win=3)
#
# ssm (x, windowLength=40, step=NULL, maxfreq=6000, nbands=NULL, MFCC_to_use=2:13, input=c('cepstrum','audiogram')[2], normalizeInput=FALSE, simil_measure=c('cosine','correlation')[1], plot=TRUE, returnSSM=FALSE, novelty_col='black', novelty_lwd=3, kernel_ms=200, kernel_sd=.5, win=1)

