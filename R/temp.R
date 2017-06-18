# Trying to write a sort of de-pickling function (takes pasted string, parses it and updates the sliders)

a = 'generateBout (nSyl=1, sylDur_mean=300, pauseDur_mean=100, noiseAmount=0, noiseIntensity=50, attackLen=50, jitterDep=3, jitterLength_ms=1, vibratoFreq=5, vibratoDep=0, shimmerDep=0, formantStrength=1, extraFormants_ampl=30, creakyBreathy=0, rolloff_exp=-12, rolloff_exp_delta=-12, adjust_rolloff_per_kHz=-6, quadratic_delta=0, quadratic_nHarm=3, rolloff_lip=6, trill_dep=0, trill_freq=30, rolloff_breathing=-12, temperature=0.025, min_epoch_length_ms=300, g0=100, sideband_width_hz=100, maleFemale=0, samplingRate=16000, windowLength_points=800, overlap=75, pitch_floor=25, pitch_ceiling=3500, pitchSamplingRate=3500, vocalTract_length=15.5, repeatBout=1, pitchAnchors=data.frame(time=c(0,0.1,0.9,1), value=c(100,150,135,100)), pitchAnchors_global=data.frame(time=c(0,1), value=c(0,0)), breathingAnchors=data.frame(time=c(0,300), value=c(-120,-120)), mouthAnchors=data.frame(time=c(0,1), value=c(0.5,0.5)), amplAnchors=data.frame(time=c(0,1), value=c(120,120)), amplAnchors_global=data.frame(time=c(0,1), value=c(120,120)), exactFormants=list(f1 = data.frame(time = c(0), freq=c(860), amp = c(30), width = c(120)), f2 = data.frame(time = c(0), freq=c(1280), amp = c(40), width = c(120)), f3 = data.frame(time = c(0), freq=c(2900), amp = c(25), width = c(200)), f4 = data.frame(time = c(0), freq=c(3800), amp = c(20), width = c(100))) , exactFormants_unvoiced=NA)'

a
n = nchar(a)
b = substr(a, 14, n-1) # remove "generateBout(" and the final ")"
as.list(noquote(b))




