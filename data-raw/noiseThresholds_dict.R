# When generating an integer random walk with getIntegerRandomWalk(), we need
# some kind of thresholds for activating different regimes of pitch effects.
# Here we set these thresholds.
slope_q1 = -.1
midpoint_q1 = 33
slope_q2 = -.1
midpoint_q2 = 66

noiseThresholds_dict = list(pitchEffects_amount = 0:100, q1=NA, q2=NA)
noiseThresholds_dict$q1 = 100 / (1 + exp(-slope_q1*(noiseThresholds_dict$pitchEffects_amount-midpoint_q1)))
noiseThresholds_dict$q2 = 100 / (1 + exp(-slope_q2*(noiseThresholds_dict$pitchEffects_amount-midpoint_q2)))
# plot (noiseThresholds_dict$pitchEffects_amount, noiseThresholds_dict$q1, type='l', col='red')
# points (noiseThresholds_dict$pitchEffects_amount, noiseThresholds_dict$q2, type='l', col='blue')

