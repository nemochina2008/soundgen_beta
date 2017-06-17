# Functions for generating and modifying smooth contours from a few anchors

#' Smooth contour from anchors
#'
#' Internal soundgen function.
#'
#' Returns a smooth contour based on an arbitrary number of anchors. Used for
#' breathing and mouth opening. Note that pitch contours are treated as a
#' special case: values are log-transformed prior to smoothing, so that with 2
#' anchors we get a linear transition on a log scale (as if we were operating
#' with musical notes rather than frequencies in Hz). Pitch plots have two Y
#' axes: one showing Hz and the other showing musical notation.
#' @param anchors a dataframe of anchors with specified time and amplitude.
#'   \code{achors$time} can be in ms (with len=NULL) or in arbitrary units, eg 0
#'   to 1 (with duration determined by len, which must then be provided in ms).
#'   So anchors$time is assumed to be in ms if len=NULL and relative if len is
#'   specified. \code{anchors$ampl} can be on any scale.
#' @param len the required length of the output contour. If NULL, it will be
#'   calculated based on the maximum time value (in ms) and \code{samplingRate}
#' @param thisIsPitch (boolean) is this a pitch contour? If true, log-transforms
#'   before smoothing and plots in both Hz and musical notation
#' @param method smoothing method: 'spline' or 'loess' (default). Consider
#'   'spline' if you have a lot of anchors and not much patience, since it is
#'   much faster than 'loess', but it may produce weird results when there are
#'   only a few anchors
#' @param ampl_floor,ampl_ceiling lower/upper bounds for the contour
#' @param plot (boolean) produce a plot?
#' @param samplingRate sampling rate used to convert time values to points (Hz)
#' @param voiced,contourLabel graphical pars for plotting breathing contours
#'   (see examples below)
#' @param main,xlim,ylim plotting options
#' @param ... other plotting options
#' @return Numeric vector.
#' @examples
#' a = soundgen:::getSmoothContour (anchors = data.frame(
#'   time = c(50, 137, 300), ampl = c(0.03, 0.78, 0.5)),
#'   voiced = 200, ampl_floor = 0, plot = TRUE, main = '',
#'   samplingRate = 16000) # breathing
#'
#' a = soundgen:::getSmoothContour (anchors = data.frame(
#'   time = c(0, .1), ampl = c(350, 800)),
#'   len = 5500, thisIsPitch = TRUE, plot = TRUE,
#'   samplingRate = 3500) # pitch
#'
#' # a single anchor gives constant pitch
#' a = soundgen:::getSmoothContour (anchors = data.frame(time = 0, ampl = 800),
#'   len = 500, thisIsPitch = TRUE, plot = TRUE, samplingRate = 500)
#'
#' # two anchors give loglinear pitch change
#' a = soundgen:::getSmoothContour (anchors = data.frame(
#'   time = c(0, 1), ampl = c(220, 440)),
#'   len = 500, thisIsPitch = TRUE, plot = TRUE, samplingRate = 500)
getSmoothContour = function(anchors = data.frame(time = c(0, 1), ampl = c(0, 1)),
                            len = NULL,
                            thisIsPitch = FALSE,
                            method = c('spline', 'loess')[2],
                            ampl_floor = NULL,
                            ampl_ceiling = NULL,
                            plot = FALSE,
                            main = '',
                            xlim = NULL,
                            ylim = NULL,
                            samplingRate = 44100,
                            voiced = NULL,
                            contourLabel = NULL,
                            ...) {
  if (!is.null(ampl_floor)) {
    anchors$ampl[anchors$ampl < ampl_floor] = ampl_floor
  }
  if (!is.null(ampl_ceiling)) {
    anchors$ampl[anchors$ampl > ampl_ceiling] = ampl_ceiling
  }
  if (thisIsPitch) {
    anchors$ampl = HzToSemitones(anchors$ampl)
    if (!is.null(ampl_floor))
      ampl_floor = HzToSemitones(ampl_floor)
    if (!is.null(ampl_ceiling))
      ampl_ceiling = HzToSemitones(ampl_ceiling)
  }

  if (is.null(len)) {
    # if len is null, we expect that anchors$time encoded
    # the desired duration, in ms
    duration_ms = max(anchors$time) - min(anchors$time)
    len = floor(duration_ms * samplingRate / 1000)
  } else {
    anchors$time = anchors$time - min(anchors$time)
    anchors$time = anchors$time / max(anchors$time) # strictly 0 to 1
    duration_ms = len / samplingRate * 1000
  }
  if (!is.numeric(duration_ms))
    return (NA)

  time = 1:len
  if (nrow(anchors) < 1) {
    stop ('getSmoothContour() requires at least one anchor')
    # alternatively, smoothContour = rep(0, length(time))
  } else if (nrow(anchors) == 1) {
    # flat
    smoothContour = rep(anchors$ampl[1], len)
  } else if (nrow(anchors) == 2) {
    # linear
    smoothContour = seq(anchors$ampl[1], anchors$ampl[2], length.out = len)
  } else {
    # smooth contour
    if (method == 'spline') {
      smoothContour = spline(anchors$ampl, n = len, x = anchors$time)$y
      # plot(smoothContour, type='l')
    } else if (method == 'loess') {
      anchor_time_points = anchors$time - min(anchors$time)
      anchor_time_points = anchor_time_points / max(anchor_time_points) * len
      anchor_time_points[anchor_time_points == 0] = 1
      anchors_long = as.vector(rep(NA, len))
      anchors_long[anchor_time_points] = anchors$ampl # plot (anchors_long)

      # let's draw a smooth curve through the given anchors
      span = (1 / (1 + exp(duration_ms / 500)) + 0.5) / 1.1 ^ (nrow(anchors) - 3)
      # NB: need to compensate for variable number of points, namely decrease
      # smoothing as the number of points increases, hence the "1.1^..." term
      # duration_ms = 50:9000
      # span = 1 / (1 + exp(duration_ms / 500)) + 0.5
      # plot(duration_ms, span, type = 'l')
      l = suppressWarnings(loess(anchors_long ~ time, span = span))
      # plot (time, anchors_long)
      smoothContour = try (predict(l, time), silent = TRUE)
      # plot(time, smoothContour)

      # for long duration etc, larger span may be needed to avoid error in loess
      while (class(smoothContour) == 'try-error') {
        span = span + 0.1
        l = suppressWarnings(loess(anchors_long ~ time, span = span))
        smoothContour = try (predict(l, time), silent = TRUE)
      }
      # plot (smoothContour, type = 'l')

      while (sum(smoothContour < ampl_floor - 1e-6, na.rm = TRUE) > 0) {
        # in case we get values below ampl_floor, less smoothing should be used
        # NB: -1e-6 avoids floating point problem, otherwise we get
        # weird cases of -120 (float) < -120 (integer)
        span = span / 1.1
        l = suppressWarnings(loess(anchors_long ~ time, span = span))
        smoothContour = try (predict(l, time), silent = TRUE)
      }
    }
    smoothContour[smoothContour < ampl_floor] = ampl_floor
    smoothContour[smoothContour > ampl_ceiling] = ampl_ceiling
  }
  # plot (smoothContour, type='l')

  if (plot) {
    op = par(no.readonly = TRUE)
    idx = seq(1, len, length.out = min(len, 100))
    # for plotting, shorten smoothContour to max 100 points
    # to reduce processing load
    smoothContour_downsampled = smoothContour[idx]

    if (thisIsPitch) {
      # pitch - log-transformed
      if (is.null(ylim) || (
        min(smoothContour_downsampled) < HzToSemitones(ylim[1]) |
        max(smoothContour_downsampled) > HzToSemitones(ylim[2])
      )) {
        ylim = round(c(
          min(smoothContour_downsampled) / 1.1,
          max(smoothContour_downsampled) * 1.1
        ))
      } else {
        ylim = HzToSemitones(ylim)
      }
      lbls_semitones = unique(seq(ylim[1], ylim[2], length.out = 5))
      # unique to remove duplicates, max 5 labels
      lbls_notes = notes_dict$note[lbls_semitones + 1]
      lbls_Hz = round(semitonesToHz(lbls_semitones))

      par(mar = c(5, 4, 4, 3)) # c(bottom, left, top, right)
      plot(time[idx] / samplingRate * 1000, smoothContour_downsampled,
           type = 'l', yaxt = "n", ylab = 'Frequency, Hz', xlab = 'Time, ms',
          main = 'Pitch contour', ylim = ylim, ...)
      # ylim = c(25, max(73, max(pitch_semitones) * 1.1))
      axis(2, at = lbls_semitones, labels = lbls_Hz, las = 1)
      axis(4, at = lbls_semitones, labels = lbls_notes, las = 1)
      points(anchors$time * duration_ms, anchors$ampl, col = 'blue', cex = 3)
    } else {
      # not pitch - not log-transformed
      if (!max(anchors$time) > 1) {
        anchors$time = anchors$time * duration_ms
      } # presuming that len was specified and anchors$time are on a
      # relative scale, we transform to ms for plotting
      par(mar = c(5, 4, 4, 3)) # c(bottom, left, top, right)
      if (is.null(xlim)) {
        xlim = c(min(0, anchors$time[1]), anchors$time[length(anchors$time)])
      }
      if (is.null(ylim)) {
        ylim = c(min(0, min(anchors$ampl[1])), max(0, max(anchors$ampl)))
      }
      x = seq(anchors$time[1],
              anchors$time[length(anchors$time)],
              length.out = length(smoothContour_downsampled))
      plot(x = x, y = smoothContour_downsampled, type = 'l', ylab = 'Amplitude',
            xlab = 'Time, ms', xlim = xlim, ylim = ylim, ...)
      points(anchors$time, anchors$ampl, col = 'blue', cex = 3)
      if (is.numeric(voiced)) {
        lines(x = c(0, voiced), y = c(0, 0), col = 'blue', lwd = 10)
        text(x = voiced / 2, y = abs(ylim[2] - ylim[1]) / 25,
          adj = 0.5, labels = 'voiced part', col = 'blue')
        text(x = anchors$time[nrow(anchors)],
             y = anchors$ampl[nrow(anchors)] - (ylim[2] - ylim[1]) / 25,
            adj = 1, labels = contourLabel, col = 'red')
      }
    }
    on.exit(par(op))  # restore original par
  }
  # NA's may arise if the first anchor time > 0
  smoothContour[is.na(smoothContour)] = 0
  if (thisIsPitch)
    smoothContour = semitonesToHz(smoothContour)
  return (smoothContour)
}


#' Discrete smooth contour from anchors
#'
#' Internal soundgen function.
#'
#' A discrete version of \code{\link{getSmoothContour}} with modified plotting. Intended for plotting variation in parameters across syllables.
#' @param len the number of syllables (equivalently, the length of generated contour)
#' @inheritParams getSmoothContour
#' @param ylim ylim for plotting
#' @return Numeric vector.
#' @examples
#' # for a bout consisting of 10 syllables
#' soundgen:::getDiscreteContour (len = 10, method = 'spline', plot = TRUE,
#'   ylab = 'Semitones', anchors = data.frame(time = c(0, .2, .6, 1),
#'   ampl = c(0, -3, 1, 0)))
getDiscreteContour = function(len,
                              anchors = data.frame(time = c(0, 1), ampl = c(1, 1)),
                              method = c('spline', 'loess')[2],
                              ampl_floor = NULL,
                              ampl_ceiling = NULL,
                              ylim = NULL,
                              plot = FALSE,
                              ...) {
  contour = getSmoothContour(
    len = len,
    anchors = anchors,
    method = method,
    ampl_floor = ampl_floor,
    ampl_ceiling = ampl_ceiling
  )
  if (plot) {
    if (is.null(ylim)) {
      ylim = c(min(min(contour), min(anchors$ampl)),
               max(max(contour), max(anchors$ampl)))
    }
    plot (contour, type = 'b', xlab = 'Syllable', col = 'red', bg = 'red',
          cex = 1, pch = 23, ylim = ylim, ...)
    points (x = anchors$time * (len - 1) + 1, y = anchors$ampl, col = 'blue',
             cex = 3)
  }
  return(contour)
}


#' Randomly modify anchors
#'
#' Internal soundgen function.
#'
#' A helper function for introducing random variation into any anchors (for
#' pitch / breathing / amplitude / ...). NB: if the lower and upper bounds are
#' unreasonable given the scale of df$ampl, \code{wiggleAnchors} will keep
#' running forever!
#' @param df dataframe of anchors, for ex. \code{data.frame(time = c(0, .1, .8,
#'   1), ampl = c(100, 230, 180, 90))}
#' @param temperature,temp_coef regulate the amount of stochasticity
#'   ("wiggling"). Since \code{temperature} is used in several functions,
#'   \code{temp_coef} gives more flexibility by controlling how much temperature
#'   affects this particular aspect, namely random variation in anchors. These
#'   two are multiplied, so \code{temp_coef} of 0.5 halves the effect of
#'   temperature.
#' @param low,high bounds on possible variation. Both \code{low} and \code{high}
#'   should be vectors of length 2: the first element specifies the boundary for
#'   \code{df$time} and the second for \code{df$ampl}. Ex.: low = c(0,1) - low
#'   bound on "time"=0, low bound on "ampl"=1
#' @param wiggleAllRows should the firts and last time anchors be wiggled? (TRUE
#'   for breathing, FALSE for other anchors)
#' @return Modified original dataframe.
#' @examples
#' soundgen:::wiggleAnchors(df = data.frame(
#'   time = c(0, .1, .8, 1), ampl = c(100, 230, 180, 90)),
#'   temperature = .2, temp_coef = .1, low = c(0, 50), high = c(1, 1000),
#'   wiggleAllRows = FALSE) # pitch
#' soundgen:::wiggleAnchors(df = data.frame(
#'   time = c(-100, 100, 600, 900), ampl = c(-120, -80, 0, -120)),
#'   temperature = .1, temp_coef = .5, low = c(-Inf, -120), high = c(+Inf, 30),
#'   wiggleAllRows = TRUE) # breathing
wiggleAnchors = function(df,
                         temperature,
                         temp_coef,
                         low,
                         high,
                         wiggleAllRows = FALSE) {
  if (sum(is.na(df)) > 0) return(NA)
  if (wiggleAllRows) {
    df[, 1] = rnorm_bounded(
      n = nrow(df),
      mean = df[, 1],
      sd = temperature * max(abs(df[, 1])) * temp_coef,
      low = low[1],
      high = high[1],
      roundToInteger = FALSE
    )
  } else {
    # don't wiggle the first and last time values
    idx1 = ifelse(nrow(df) < 3, NA, 2:(nrow(df) - 1))
    if (!is.na(idx1)) {
      temp = df[idx1, 1]
      df[idx1, 1] =  rnorm_bounded(
        n = length(temp),
        mean = temp,
        sd = temperature * max(abs(df[, 1])) * temp_coef,
        low = low[1],
        high = high[1],
        roundToInteger = FALSE
      )
    }
  }
  df[, 2] = rnorm_bounded(
    n = nrow(df),
    mean = df[, 2],
    sd = temperature * abs(high[2] - low[2]) * temp_coef,
    low = low[2],
    high = high[2],
    roundToInteger = FALSE
  )
  return(df)
}
