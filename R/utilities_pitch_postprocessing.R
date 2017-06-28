### UTILITIES FOR POSP-PROCESSING OF PITCH CONTOURS ###

#' Cost of jumps
#'
#' Internal soundgen function.
#'
#' Internal helper function for calculating the cost of transitions between
#' pitch candidates. Needed for postprocessing of pitch contour - finding the
#' optimal pitch contour.
#' @param cand1,cand2 two candidate pitch values
#' @examples
#' a = seq(-3, 3, by = .01)
#' b = 1 / (1 + 10 * exp(3 - 7 * abs(a)))
#' plot(a, b, type = 'l')
costJumps = function(cand1, cand2) {
  return (1 / (1 + 10 * exp(3 - 7 * abs(cand1 - cand2))))
}


#' Find gradient
#'
#' Internal soundgen function.
#'
#' Internal helper function for postprocessing of pitch contour. Returns
#' gradient of internal energy of a snake. See \code{\link{snake}}.
#' @param path numeric vector corresponding to a path through pitch candidates
#' @param interpol the number of points to interpolate beyond each end of the path
#' @return Returns a vector of the same length as input path giving its 4th derivative.
findGrad = function(path, interpol = 3) {
  # interpolate 2 values before the first one and two after the last one based
  # on /interpol/ number of points in case the path is shorter than the
  # specified interpol:
  interpol = ifelse(interpol > length(path), length(path), interpol)
  if (interpol == 1) {
    path = c(rep(path[1], 2), path, rep(path[length(path)], 2))
  } else {
    slopeLeft = summary(lm(path[1:interpol] ~ seq(1, interpol)))$coef[2, 1]
    minus12 = path[1] - c(1, 2) * slopeLeft
    slopeRight = summary(lm(path[(length(path) - interpol + 1):length(path)] ~
                              seq(1, interpol)))$coef[2, 1]
    plus12 = path[length(path)] + c(1, 2) * slopeRight
    path = c (minus12[2], minus12[1], path, plus12[1], plus12[2])
  }

  # take the 4th derivative of the path with interpolated values
  # (so that we get d4f over the entire length of the original path)
  grad = rep(0, length(path))
  for (i in 3:(length(path) - 2)) {  # approximation
    grad[i] = path[i - 2] - 4 * path[i - 1] + 6 * path[i] - 4 * path[i + 1] +
      path[i + 2]
  }
  grad = grad[3:(length(grad) - 2)]
  return (grad)
}


#' Force per path
#'
#' Internal soundgen function.
#'
#' Internal helper function for postprocessing of pitch contour. Returns the
#' total force acting on a snake (sum of internal and external gradients, i.e.
#' of the elastic force trying to straighten the snake [internal] and of the
#' force pushing the snake towards the most certain pitch estimates [external])
#' @param
#' @return Returns ???
#' @examples
#'
forcePerPath = function (pitch,
                         pitchCands,
                         pitchCert,
                         pitchCenterGravity,
                         certWeight) {
  ran = diff(range(pitchCands, na.rm = T))
  # pitchForce = -(pitch_path - pitchCenterGravity) / ran
  pitchForce = pitch # just a quick way to initialize a vector of the right length
  for (i in 1:ncol(pitchCands)) {
    cands = na.omit(pitchCands[, i])
    certs = na.omit(pitchCert[, i])
    deltas = 1 / exp((cands - pitch[i]) ^ 2)
    forces = certs * deltas
    forces = ifelse(cands > pitch[i], forces,-forces)
    pitchForce[i] = sum(forces)
  }
  internalForce = -findGrad(pitch)
  return(certWeight * pitchForce + (1 - certWeight) * internalForce)
}


#' Snake
#'
#' Internal soundgen function.
#'
#' Internal helper function for postprocessing of pitch contour. Wiggles a snake along the gradient of internal + external forces. NB: if the snake is run, the final contour may deviate from the actually measured pitch candidates!
#' @param
#' @return
#' @examples
#'
snake = function (pitch,
                  pitchCands,
                  pitchCert,
                  certWeight,
                  snakeSmoothingStep = 0.05,
                  snakeIterMultiplier = 2,
                  plotSnake = F) {
  ran = diff(range(pitchCands, na.rm = T)) # range of pitch
  maxIter = ran / snakeSmoothingStep * snakeIterMultiplier

  # plot for debugging or esthetic appreciation
  if (plotSnake) {
    plot(
      seq(1, ncol(pitchCands)),
      pitch,
      type = 'n',
      ylim = c(
        range(pitchCands, na.rm = T)[1] - .3 * ran,
        range(pitchCands, na.rm = T)[2] + .3 * ran
      )
    )
    for (r in 1:nrow(pitchCands)) {
      points (seq(1, ncol(pitchCands)),
              pitchCands[r, ],
              cex = as.numeric(pitchCert[r, ]) * 2)
    }
    lines (seq(1, ncol(pitchCands)), pitch)
  }

  # optimization algorithm follows
  for (i in 1:maxIter) {
    force = forcePerPath(pitch,
                         pitchCands,
                         pitchCert,
                         pitchCenterGravity,
                         certWeight)
    pitch = pitch + snakeSmoothingStep * force
    if (plotSnake) {
      lines(seq(1, length(pitch)), pitch,
            type = 'l', col = 'green', lty = 4)
    }
  }

  if (plotSnake) {
    lines(seq(1, length(pitch)), pitch,
          type = 'l', col = 'blue', lwd = 3)
  }
  return (pitch)
}


#' Pathfinder
#'
#' Internal soundgen function.
#'
#' Internal helper function for postprocessing pitch contour. Starts with a
#' reasonable guess and computes the more-or-less optimal pitch contour (not
#' quite the very optimal - too computationally expensive).
#' @param pitchCands
#' @param pitchCert
#' @inheritParams analyze
#' @return Returns a numeric vector representing the best found path through pitch candidates.
pathfinder = function(pitchCands,
                      pitchCert,
                      certWeight = 0.5,
                      interpolWindow = 3,
                      interpolTolerance = 0.05,
                      interpolCert = 0.3,
                      runSnake = T,
                      snakeSmoothingStep = 0.05,
                      snakeIterMultiplier = 2,
                      plotSnake = F) {
  # take log to approximate human perception of pitch differences
  pitchCands[!is.na(pitchCands)] = log2(pitchCands[!is.na(pitchCands)])

  # Interpolation: if a frame has no pitch candidate at all (NA) or no candidate
  # between the most likely candidates for the adjacent frames, add such a
  # candidate with ~low certainty
  for (f in 1:ncol(pitchCands)) {
    # NB: in the loop b/c it has to be done recursively, taking interpolated
    # values into account for further interpolation. Unfortunate but can't be
    # helped
    pitchCenterGravity = apply (as.matrix(1:ncol(pitchCands), nrow = 1), 1, function(x) {
      mean(pitchCands[, x],
           weights = pitchCert[, x] / sum(pitchCert[, x]),
           na.rm = T)
    })
    left = max(1, f - interpolWindow)
    right = min(ncol(pitchCands), f + interpolWindow)
    # median over interpolation window (by default ±2 points)
    med = median(pitchCenterGravity[left:right], na.rm = T)
    sum_pitchCands = sum(
      pitchCands[, f] > (1 - interpolTolerance) * med &
        pitchCands[, f] < (1 + interpolTolerance) * med,
      na.rm = T
    )
    if (sum_pitchCands == 0) {
      # defaults to (0.95 * med, 1.05 * med)
      pitchCands = rbind(pitchCands, rep(NA, ncol(pitchCands)))
      pitchCert = rbind (pitchCert, rep(NA, ncol(pitchCert)))
      pitchCands[nrow(pitchCands), f] =
        median(pitchCenterGravity[left:right], na.rm = TRUE)
      pitchCert[nrow(pitchCert), f] = interpolCert # certainty assigned to interpolated frames
    }
  }

  # order pitch candidates and certainties in each frame from lowest to highest
  # pitch (helpful for further processing)
  o = apply (as.matrix(1:ncol(pitchCands), nrow = 1), 1, function(x) {
    order(pitchCands[, x])
  })
  pitchCands = apply (as.matrix(1:ncol(pitchCands), nrow = 1), 1, function(x) {
    pitchCands[o[, x], x]
  })
  pitchCert = apply (as.matrix(1:ncol(pitchCert), nrow = 1), 1, function(x) {
    pitchCert[o[, x], x]
  })
  # remove rows with all NA's
  pitchCands = pitchCands[rowSums(!is.na(pitchCands)) != 0, ]
  pitchCert = pitchCert[rowSums(!is.na(pitchCert)) != 0, ]

  # special case: only a single pitch candidate for all frames in a syllable (no
  # paths to chose among)
  if (class(pitchCands) == 'numeric') {
    return(2 ^ pitchCands)
  }

  # get the "center of gravity" of pitch candidates in each frame (mean of all
  # pitch candidates weighted by their respective certainties)
  pitchCenterGravity = apply(as.matrix(1:ncol(pitchCands), nrow = 1), 1, function(x) {
    mean(
      pitchCands[, x],
      weights = pitchCert[, x] / sum(pitchCert[, x]),
      na.rm = TRUE
    )
  })

  # start at the beginning of the snake: find the most plausible starting pitch
  # by taking median over the first few frames, weighted by certainty
  p = median (pitchCenterGravity[1:min(5, length(pitchCenterGravity))])
  c = pitchCert[, 1] / abs(pitchCands[, 1] - p) # b/c there may be NA's,
  # and they can't be excluded directly in which.max in the next line
  point_current = pitchCands[which.max(c), 1]
  path = point_current
  costPathForward = 0

  for (i in 2:ncol(pitchCands)) {
    cands = na.omit(pitchCands[, i])
    cost_cert = abs(cands - pitchCenterGravity[i])
    cost_pitchJump = apply (as.matrix(1:length(cands), nrow = 1), 1, function(x) {
      costJumps(point_current, cands[x])
    })
    costs = certWeight * cost_cert + (1 - certWeight) * cost_pitchJump
    path = c(path, pitchCands[which.min(costs), i])
    costPathForward = costPathForward + min(costs)
  }

  # run backwards
  pitchCands_rev = pitchCands[, rev(1:ncol(pitchCands))]
  pitchCert_rev = pitchCert[, rev(1:ncol(pitchCert))]
  pitchCenterGravity_rev = rev(pitchCenterGravity)

  p = median (pitchCenterGravity_rev[1:min(5, length(pitchCenterGravity_rev))])
  c = na.omit (pitchCert_rev[, 1] / abs(pitchCands_rev[, 1] - p)) # b/c there may be NA's,
  # and they can't be excluded directly in which.max in the next line
  point_current = pitchCands_rev[which.max(c), 1]
  path_rev = point_current
  costPathBackward = 0

  for (i in 2:ncol(pitchCands_rev)) {
    cands = na.omit(pitchCands_rev[, i])
    cost_cert = abs(cands - pitchCenterGravity_rev[i])
    cost_pitchJump = apply (as.matrix(1:length(cands), nrow = 1), 1, function(x) {
      costJumps(point_current, cands[x])
    })
    costs = certWeight * cost_cert + (1 - certWeight) * cost_pitchJump
    path_rev = c(path_rev, pitchCands_rev[which.min(costs), i])
    costPathBackward = costPathBackward + min(costs)
  }

  if (costPathForward < costPathBackward) {
    pitch_final = path
  } else {
    pitch_final = rev(path_rev)
  }

  if (runSnake) {
    pitch_final = snake (
      pitch_final,
      pitchCands,
      pitchCert,
      certWeight,
      snakeSmoothingStep,
      snakeIterMultiplier,
      plotSnake
    )
  }

  return(2 ^ pitch_final)
}



#' Median smoothing
#'
#' Internal soundgen function.
#'
#' Internal helper function for smoothing pitch contours or other contours. Only
#' outliers are modified, so it's not like smoothing with a kernel. NB: the
#' expected input is pitch, so deviance is calculated on a log-scale.
#' @param df a dataframe (each column is processed separately, so multiple
#'   contours can be fed into this function at once to speed things up)
#' @param smoothing_ww width of smoothing window (points)
#' @param smoothing_threshold tolerated deviance from moving median (semitones)
#' @return Returns a dataframe of the same dimensions as df.
#' @examples
#' df = data.frame(a = rnorm(100, mean = 100, sd = 20),
#'                 b = rnorm(100, mean = 100, sd = 10))
#' df1 = soundgen:::medianSmoother(df, smoothing_ww = 5, smoothing_threshold = 1)
#' plot(df[, 2], type='b')
#' lines(df1[, 2], type='b', col='blue', pch=3)
medianSmoother = function (df, smoothing_ww, smoothing_threshold) {
  temp = df # to calculate median_over_window for original values
  hw = floor(smoothing_ww / 2) # smooth over ± half the smoothing_ww
  for (i in 1:nrow(df)) {
    window = c (max(i - hw, 1), min(i + hw, nrow(df))) # smoothing window
    median_over_window = apply(as.matrix(temp[(window[1]:window[2]), ]), 2, function(x) {
      median(unlist(x), na.rm = TRUE)  # w/o unlist returns NULL for NA vectors (weird...)
      # NB: use either temp or df, for original or smoothed values to be used
      # for calculating median_over_window
    })
    # difference from median pitch etc over window, in semitones
    deviance = 12 * log2(as.numeric(df[i, ]) / median_over_window)
    cond = which(abs(deviance - 1) > smoothing_threshold)
    df[i, cond] = median_over_window[cond]
  }
  return (df)
}
