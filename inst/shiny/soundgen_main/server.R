server <- function(input, output, session) {
  ## S E T U P
  myPars <- reactiveValues('myfile'=NULL,
                           'sound'=as.numeric(tuneR::readWave('www/temp.wav')@left), # w/o as.numeric we get integers and spectro_denoised complains
                           'pitchAnchors' = data.frame('time'= c(0,.1,.9,1),
                                                       'ampl'=c(100,150,135,100)),
                           'pitchAnchors_global' = data.frame('time' = c(0,1),
                                                              'ampl' = c(0,0)),
                           'breathingAnchors' = data.frame('time'=c(0,300),
                                                           'ampl'=c(throwaway_dB,throwaway_dB)),
                           'mouthAnchors' = data.frame('time'=c(0,1),
                                                       'ampl'=c(.5,.5)),
                           'amplAnchors' = data.frame('time'=c(0,1),
                                                      'ampl'=c(-throwaway_dB,-throwaway_dB)),
                           'amplAnchors_global' = data.frame('time'=c(0,1),
                                                             'ampl'=c(-throwaway_dB,-throwaway_dB)),
                           'exactFormants' = data.frame(
                             'f1' = data.frame('time'=0, 'freq'=permittedValues['f1_freq','default'], 'amp'=permittedValues['f1_amp','default'], 'width'=permittedValues['f1_width','default']),
                             'f2' = data.frame('time'=0, 'freq'=permittedValues['f2_freq','default'], 'amp'=permittedValues['f1_amp','default'], 'width'=permittedValues['f2_width','default']),
                             'f3' = data.frame('time'=0, 'freq'=permittedValues['f3_freq','default'], 'amp'=permittedValues['f1_amp','default'], 'width'=permittedValues['f3_width','default'])
                           ),
                           'exactFormants_unvoiced' = NA,
                           'updateDur' = FALSE
  )

  durTotal = reactive({
    ifelse(input$nSyl==1, input$sylDur_mean, (input$sylDur_mean*input$nSyl + input$pauseDur_mean*(input$nSyl-1))) # the duration of the entire bout without breathing, calculated as a sum of voiced syllables and pauses
  })

  durSyl_withBreathing = reactive({
    ifelse(!sum(myPars$breathingAnchors$ampl>throwaway_dB)>0, input$sylDur_mean, min(0,myPars$breathingAnchors$time[1])+max(input$sylDur_mean,myPars$breathingAnchors$time[length(myPars$breathingAnchors$time)])) # the duration of a single syllable with breathing
  })



  ## R E S E T T I N G
  sliders_to_reset = c('')

  reset_all = reactive({  # this key function is EXTREMELY bug-prone - careful with what you change! The right order is crucial
    myPars$updateDur = FALSE # to prevent duration-related settings in myPars from being updated by event listener observeEvent(input$sylDur_mean) when a new preset is loaded

    # first reset everything to defaults
    for (v in rownames(permittedValues)[1 : which(rownames(permittedValues)=='repeatBout')]){
      updateSliderInput(session, v, value=permittedValues[v,'default'])
    }

    # ...then load the partial list of presets that are specified (≠ default) for this speaker and call type
    preset = presets[[input$speaker]] [[input$callType]] # a list of reference values for this speaker and call type

    sliders_to_reset = names(preset) [which(names(preset) %in% names(input))]
    for (v in sliders_to_reset){
      try(updateSliderInput(session, v, value=as.numeric(preset[[v]])))
    }

    myPars[['pitchAnchors_global']] = data.frame('time'=c(0,1),'ampl'=c(0,0))
    myPars[['mouthAnchors']] = data.frame('time'=c(0,1),'ampl'=c(0.5,0.5))
    myPars[['amplAnchors_global']] = data.frame('time'=c(0,1),'ampl'=c(-throwaway_dB,-throwaway_dB))
    myPars[['amplAnchors']] = data.frame('time'=c(0,1),'ampl'=c(-throwaway_dB,-throwaway_dB))
    myPars[['breathingAnchors']] = data.frame('time'=c(0,preset$sylDur_mean),'ampl'=c(throwaway_dB,throwaway_dB))

    myPars_to_reset = names(myPars) [which(names(myPars) %in% names(preset))]
    for (v in myPars_to_reset){
      myPars[[v]] = preset[[v]]
    }
    updateSliderInput(session, 'breathingTime', value=range(myPars$breathingAnchors$time))

    # special cases
    if (!is.null(preset$pitchAnchors)){
      updateSliderInput(session, 'pitchRange', value=c( round(min(preset$pitchAnchors$ampl)/2^(1/12),0), round(max(preset$pitchAnchors$ampl)*2^(1/12),0) ) )
    }

    if(!is.null(preset$vowelString)){
      updateTextInput(session, inputId='vowelString', value=preset$vowelString)
      updateVowels()
    } else if (is.null(preset$vowelString) & !is.null(preset$exactFormants)) {
      updateTextInput(session, inputId='vowelString', value='')
      updateTextInput(session, inputId='exactFormants', value=pickle(preset$exactFormants))
      myPars$exactFormants = preset$exactFormants
    } else { # if both are NULL
      updateTextInput(session, inputId='vowelString', value=defaultVowel)
      updateVowels()
    }

    if(!is.null(preset$noiseType)){
      updateSelectInput(session, inputId='noiseType', value=preset$noiseType)
      updateNoise()
    } else if (is.null(preset$noiseType) & !is.null(preset$exactFormants_unvoiced)) {
      updateTextInput(session, inputId='noiseType', value='')
      updateTextInput(session, inputId='exactFormants_unvoiced', value=pickle(preset$exactFormants_unvoiced))
      myPars$exactFormants_unvoiced = preset$exactFormants_unvoiced
    } else { # if both are NULL
      updateTextInput(session, inputId='noiseType', value='b')
      updateNoise()
    }
  })

  observeEvent(input$callType, {
    reset_all()
  })

  observeEvent(input$speaker, {
    reset_all()
    updateSelectInput(session, inputId='callType', choices=head(names(presets[[input$speaker]]),-1)) # update available call types for this speaker specified in presets, except the last call type, which is reserved for formants
  })

  observeEvent(input$exactFormants, {
    if (length(input$exactFormants)>0){
      try({myPars$exactFormants = eval(parse(text=input$exactFormants))}) # overrides vowelString
    }
  })

  observeEvent(input$vowelString, {
    updateVowels()
  })

  updateVowels = reactive({
    if (nchar(input$vowelString)>0) {
      try({converted = convertStringToFormants(input$vowelString, speaker=input$speaker)})
      if (sum(unlist(converted))>0) { # if the converted formant list is not empty
        myPars$exactFormants = converted # (...otherwise don't change myPars$exactFormants to prevent crashing)
      }
      updateTextInput(session, inputId='exactFormants', value=pickle(converted))
    }
  })

  observeEvent(input$exactFormants_unvoiced, {
    if (length(input$exactFormants_unvoiced)>0){
      try({myPars$exactFormants_unvoiced = eval(parse(text=input$exactFormants_unvoiced))}) # overrides chosen consonant
    }
  })

  observeEvent(input$noiseType, {
    updateNoise()
  })

  updateNoise = reactive({
    if (input$noiseType=='b'){ # breathing
      myPars$exactFormants_unvoiced = NA
      updateTextInput(session, inputId='exactFormants_unvoiced', value='NA')
    } else if (nchar(input$noiseType)>0) { # debug - check if this always works!!!
      n = presets[[input$speaker]] [['Formants']][input$noiseType] [[1]]
      myPars$exactFormants_unvoiced = n [2:length(n)]
      updateSliderInput(session, inputId='rolloff_breathing', value=n[['rolloff_breathing']])
      updateTextInput(session, inputId='exactFormants_unvoiced', value=pickle(myPars$exactFormants_unvoiced))
    }
  })

  observeEvent(input$sylDur_mean, { # has to be updated manually, b/c breathingAnchors are the only time anchors expressed in ms rather than 0 to 1 (b/c we don't want to rescale pre-syllable aspiration depending on the syllable duration)
    if (myPars$updateDur==TRUE){ # doesn't run if updateDur==FALSE (set to F in reset_all())
      myPars$breathingAnchors$time[myPars$breathingAnchors$time>0] = round(myPars$breathingAnchors$time[myPars$breathingAnchors$time>0] * (input$nSyl*input$sylDur_mean+input$pauseDur_mean*(input$nSyl-1))/durTotal()) # rescale positive time anchors, but not negative ones (ie the length of pre-syllable aspiration does not vary as the syllable length changes - just doesn't seem to make sense)
      updateSliderInput(session, inputId='breathingTime', value=range(myPars$breathingAnchors$time))
    }
    myPars$updateDur = TRUE # after the first change (resetting), this update should be executed
  })


  ## P I T C H
  output$plot_intonation = renderPlot({
    myPitchContour()
  })

  myPitchContour <- reactive({
    pitch_y_lwr = min ( input$pitchRange[1], min(myPars$pitchAnchors$ampl)/1.1 )
    pitch_y_upr = max ( input$pitchRange[2], max(myPars$pitchAnchors$ampl)*1.1 )
    print(pitch_y_upr)
    getSmoothContour (anchors=myPars$pitchAnchors, len=input$sylDur_mean*permittedValues['pitch','high']/1000, plot=TRUE, ylim=c(pitch_y_lwr,pitch_y_upr), samplingRate=permittedValues['pitch','high'], thisIsPitch=TRUE)
  })

  observeEvent(input$plot_intonation_click, {
    click_x = round( round(input$plot_intonation_click$x)/input$sylDur_mean, 2 )
    click_y = round(semitonesToHz(input$plot_intonation_click$y))
    # if the click is below or above thresholds, move within thresholds
    if (click_y<permittedValues['pitch','low']){click_y=permittedValues['pitch','low']}
    if (click_y>permittedValues['pitch','high']){click_y=permittedValues['pitch','high']}

    closest_point_in_time = which.min(abs(myPars$pitchAnchors$time-click_x))
    delta_x = abs(myPars$pitchAnchors$time[closest_point_in_time] - click_x)
    # if the click is near (within ±5% of the time range) an existing anchor point, we update the pitch of this anchor according to click location (and the time as well, unless it is the first or the last anchor)
    if (delta_x < 0.05){
      myPars$pitchAnchors$ampl[closest_point_in_time] = click_y
      if (closest_point_in_time != 1 && closest_point_in_time != length(myPars$pitchAnchors$time)){
        myPars$pitchAnchors$time[closest_point_in_time] = click_x
      }
    } else { # otherwise, we simply add the new point as another anchor
      myPars[['pitchAnchors']] = data.frame ('time'=c(myPars$pitchAnchors$time, click_x),
                                             'ampl'=c(myPars$pitchAnchors$ampl, click_y)) # convoluted, but otherwise problems with unwanted dataframe-list conversion, etc
    }
    # sort the updated dataframe of pitch anchors to make sure the point are in the right order (otherwise it's hard to keep track of which are the first and last anchors - and we have to, since those cannot be removed)
    idx_order = order(myPars$pitchAnchors$time)
    myPars$pitchAnchors$time = myPars$pitchAnchors$time[idx_order]
    myPars$pitchAnchors$ampl = myPars$pitchAnchors$ampl[idx_order]
  })

  observeEvent(input$plot_intonation_dblclick, {
    ref = as.data.frame(myPars[['pitchAnchors']])
    ref$time = ref$time*input$sylDur_mean
    closestPoint = nearPoints(ref, input$plot_intonation_dblclick, xvar='time', yvar='ampl', threshold=100000, maxpoints=1)
    idx = as.numeric(rownames(closestPoint))
    if (length(idx)>0 && idx!=1 && idx!=length(myPars$pitchAnchors$time)){ # we can remove any anchor except the first and the last (because pitch at start and end of sound has to be defined)
      myPars[['pitchAnchors']] = data.frame('time'=myPars$pitchAnchors$time[-idx],
                                            'ampl'=myPars$pitchAnchors$ampl[-idx])
    }
  })

  observeEvent(input$pitch_flatten, {
    myPars[['pitchAnchors']] = data.frame('time'=c(0,1),
                                          'ampl'=rep(myPars$pitchAnchors$ampl[1],2))  # flat pitch equal to the first pitch anchor
  })

  output$pitch_anchors = renderTable(expr=data.frame('Time, ms'=round(myPars$pitchAnchors$time*input$sylDur_mean,0), 'Pitch, Hz'=round(myPars$pitchAnchors$ampl,0), row.names=1:length(myPars$pitchAnchors$time)), digits=0, align='c', rownames=FALSE)


  ## P I T C H   G L O B A L
  output$plot_intonation_global = renderPlot({
    myPitchContour_global()
  })

  myPitchContour_global <- reactive({
    if (input$nSyl>1){
      getDiscreteContour (anchors=myPars$pitchAnchors_global, len=input$nSyl, method='spline', plot=TRUE, ylab='Pitch delta, semitones', ampl_floor=permittedValues['pitchDeltas','low'], ampl_ceiling=permittedValues['pitchDeltas','high'], ylim=c(permittedValues['pitchDeltas','low'], permittedValues['pitchDeltas','high']))
    } else {
      plot (1:10, 1:10, type='n', xlab='', ylab='', axes=FALSE)
      text (x=5, y=5, labels = 'Need >1 syllable!', adj=.5, col='blue', cex=1)
    }

  })

  observeEvent(input$plot_intonation_click_global, {
    timeRange = input$nSyl-1
    click_x = (input$plot_intonation_click_global$x-1)/timeRange # ranges 0 to 1
    click_y = round(input$plot_intonation_click_global$y, 1)
    # if the click is below or above thresholds, move within thresholds
    if (click_y<permittedValues['pitchDeltas','low']){click_y=permittedValues['pitchDeltas','low']}
    if (click_y>permittedValues['pitchDeltas','high']){click_y=permittedValues['pitchDeltas','high']}

    closest_point_in_time = which.min(abs(myPars$pitchAnchors_global$time-click_x))
    delta_x = abs(myPars$pitchAnchors_global$time[closest_point_in_time] - click_x)
    # if the click is near (within ±20% of the time range) an existing anchor point, we update the pitch of this anchor according to click location (and the time as well, unless it is the first or the last anchor)
    if (delta_x < 0.2){
      myPars$pitchAnchors_global$ampl[closest_point_in_time] = click_y
      if (closest_point_in_time != 1 && closest_point_in_time != length(myPars$pitchAnchors_global$time)){
        myPars$pitchAnchors_global$time[closest_point_in_time] = click_x
      }
    }  else { # otherwise, we simply add the new point as another anchor
      myPars[['pitchAnchors_global']] = data.frame ('time'=c(myPars$pitchAnchors_global$time, click_x),
                                                    'ampl'=c(myPars$pitchAnchors_global$ampl, click_y)) # convoluted, but otherwise problems with unwanted dataframe-list conversion, etc
    }
    # sort the updated dataframe of pitch anchors to make sure the point are in the right order (otherwise it's hard to keep track of which are the first and last anchors - and we have to, since those cannot be removed)
    idx_order = order(myPars$pitchAnchors_global$time)
    myPars$pitchAnchors_global$time = myPars$pitchAnchors_global$time[idx_order]
    myPars$pitchAnchors_global$ampl = myPars$pitchAnchors_global$ampl[idx_order]
  })

  observeEvent(input$plot_intonation_dblclick_global, {
    ref = as.data.frame(myPars[['pitchAnchors_global']])
    ref$time = ref$time*(input$nSyl-1)+1
    closestPoint = nearPoints(ref, input$plot_intonation_dblclick_global, xvar='time', yvar='ampl', threshold=100000, maxpoints=1)
    idx = as.numeric(rownames(closestPoint))
    if (length(idx)>0 && idx!=1 && idx!=length(myPars$pitchAnchors_global$time)){ # we can remove any anchor except the first and the last (because pitch at start and end of sound has to be defined)
      myPars[['pitchAnchors_global']] = data.frame('time'=myPars$pitchAnchors_global$time[-idx],
                                                   'ampl'=myPars$pitchAnchors_global$ampl[-idx])
    }
  })

  observeEvent(input$pitch_flatten_global, {
    myPars[['pitchAnchors_global']] = data.frame('time'=c(0,1),'ampl'=c(0,0))  # flat pitch modulation across syllables
  })

  output$pitch_anchors_global = renderTable(expr=data.frame('Time 0 to 1'=myPars$pitchAnchors_global$time, 'Pitch delta, semitones'=round(myPars$pitchAnchors_global$ampl,0), row.names=1:length(myPars$pitchAnchors_global$time)), digits=2, align='c', rownames=FALSE)


  ## BREATHING
  output$plot_unvoiced = renderPlot({
    myBreathingContour()
  })

  myBreathingContour <- reactive({
    br_xlim_low = min(input$breathingTime[1], 0)
    br_xlim_high = max(input$breathingTime[2], input$sylDur_mean)
    br_ylim_low = permittedValues['breathing_ampl', 'low']
    br_ylim_high = permittedValues['breathing_ampl', 'high']
    nTicks = length(seq(br_ylim_low, br_ylim_high, by=20))-1
    getSmoothContour(anchors=myPars$breathingAnchors, plot=TRUE, xlim=c(br_xlim_low, br_xlim_high), ylim=c(br_ylim_low, br_ylim_high), voiced=input$sylDur_mean, contourLabel='breathing', ampl_floor=br_ylim_low, ampl_ceiling=br_ylim_high, yaxp=c(br_ylim_low, br_ylim_high, nTicks))
  })

  observeEvent(input$plot_unvoiced_click, {
    click_x = round(input$plot_unvoiced_click$x)
    click_y = round(input$plot_unvoiced_click$y)
    # if the click is outside the allowed range of y, re-interpret the click as within the range
    if (click_y<permittedValues['breathing_ampl','low']){click_y=permittedValues['breathing_ampl','low']}
    if (click_y>permittedValues['breathing_ampl','high']){click_y=permittedValues['breathing_ampl','high']}

    closest_point_in_time = which.min(abs(myPars$breathingAnchors$time-click_x))
    delta_x = abs(myPars$breathingAnchors$time[closest_point_in_time] - click_x)
    # if the click is near (within ±5% of the time range) an existing anchor point, we update the ampl of this anchor according to click location and time
    if (delta_x < 0.05*durSyl_withBreathing()){
      myPars$breathingAnchors$ampl[closest_point_in_time] = click_y
      myPars$breathingAnchors$time[closest_point_in_time] = click_x
    } else { # otherwise, we simply add the new point as another anchor
      myPars[['breathingAnchors']] = data.frame ('time'=c(myPars$breathingAnchors$time, click_x),
                                                 'ampl'=c(myPars$breathingAnchors$ampl, click_y)) # convoluted, but otherwise problems with unwanted dataframe-list conversion, etc
    }
    # sort the updated dataframe of pitch anchors to make sure the point are in the right order (otherwise it's hard to keep track of which are the first and last anchors - and we have to, since those cannot be removed)
    idx_order = order(myPars$breathingAnchors$time)
    myPars$breathingAnchors$time = myPars$breathingAnchors$time[idx_order]
    myPars$breathingAnchors$ampl = myPars$breathingAnchors$ampl[idx_order]
  })

  observeEvent(input$plot_unvoiced_dblclick, {
    closestPoint = nearPoints(as.data.frame(myPars[['breathingAnchors']]), input$plot_unvoiced_dblclick, xvar='time', yvar='ampl', threshold=100000, maxpoints=1)
    idx = as.numeric(rownames(closestPoint))
    if (length(idx)>0 && length(myPars$breathingAnchors$time)>2){ # we can remove any anchor, as long as there will be at least two anchors left (to know what breathing duration should be)
      myPars[['breathingAnchors']] = data.frame('time'=myPars$breathingAnchors$time[-idx],
                                                'ampl'=myPars$breathingAnchors$ampl[-idx])
    }
  })

  observeEvent(input$breathing_flatten, {
    myPars[['breathingAnchors']] = data.frame(
      'time'=myPars$breathingAnchors$time[c(1,length(myPars$breathingAnchors$time))],
      'ampl'=rep(myPars$breathingAnchors$ampl[1],2)  # flat pitch equal to the first pitch anchor
    )})

  output$breathing_anchors = renderTable(expr=data.frame('Time, ms'=round(myPars$breathingAnchors$time,0), 'Amplitude, dB'=round(myPars$breathingAnchors$ampl,0), row.names=1:length(myPars$breathingAnchors$time)), digits=0, align='c', rownames=FALSE)


  ## MOUTH OPENING
  output$plot_mouth = renderPlot({
    myMouthOpening()
  })

  myMouthOpening <- reactive({
    getSmoothContour(anchors=myPars$mouthAnchors, len=durSyl_withBreathing()/1000*1000, samplingRate=1000, plot=TRUE, contourLabel='mouth', xlim=c(0,durSyl_withBreathing()), xaxs="i", ylim=c(permittedValues['mouthOpening','low'],permittedValues['mouthOpening','high']), ampl_floor=permittedValues['mouthOpening','low'], ampl_ceiling=permittedValues['mouthOpening','high'])  # # xaxs="i" to enforce exact axis limits, otherwise we exceed the range. OR: xlim=range(myPars$breathingAnchors$time)
  })

  observeEvent(input$plot_mouth_click, {
    click_x = round(round(input$plot_mouth_click$x)/durSyl_withBreathing(),2)
    click_y = round(input$plot_mouth_click$y,2)
    # if the click is outside the allowed range of y, re-interpret the click as within the range
    if (click_y<permittedValues['mouthOpening','low']){click_y=permittedValues['mouthOpening','low']}
    if (click_y>permittedValues['mouthOpening','high']){click_y=permittedValues['mouthOpening','high']}

    closest_point_in_time = which.min(abs(myPars$mouthAnchors$time - click_x))
    delta_x = abs(myPars$mouthAnchors$time[closest_point_in_time] - click_x)
    # if the click is near (within ±5% of the time range) an existing anchor point, we update the pitch of this anchor according to click location and time
    if (delta_x < 0.05){
      myPars$mouthAnchors$ampl[closest_point_in_time] = click_y
      if (closest_point_in_time != 1 & closest_point_in_time != length(myPars$mouthAnchors$time)){
        myPars$mouthAnchors$time[closest_point_in_time] = click_x
      }
    } else { # otherwise, we simply add the new point as another anchor
      myPars[['mouthAnchors']] = data.frame ('time'=c(myPars$mouthAnchors$time, click_x),
                                             'ampl'=c(myPars$mouthAnchors$ampl, click_y)) # convoluted, but otherwise problems with unwanted dataframe-list conversion, etc
    }
    # sort the updated dataframe of pitch anchors to make sure the point are in the right order (otherwise it's hard to keep track of which are the first and last anchors - and we have to, since those cannot be removed)
    idx_order = order(myPars$mouthAnchors$time)
    myPars$mouthAnchors$time = myPars$mouthAnchors$time[idx_order]
    myPars$mouthAnchors$ampl = myPars$mouthAnchors$ampl[idx_order]
  })

  observeEvent(input$plot_mouth_dblclick, {
    ref = as.data.frame(myPars[['mouthAnchors']])
    ref$time = ref$time * durSyl_withBreathing()
    closestPoint = nearPoints(ref, input$plot_mouth_dblclick, xvar='time', yvar='ampl', threshold=100000, maxpoints=1)
    idx = as.numeric(rownames(closestPoint))
    if (length(idx)>0 && idx!=1 && idx!=length(myPars$mouthAnchors$time)){ # we can remove any anchor except the first and the last (because mouth opening at start and end of sound has to be defined)
      myPars[['mouthAnchors']] = data.frame('time'=myPars$mouthAnchors$time[-idx],
                                            'ampl'=myPars$mouthAnchors$ampl[-idx])
    }
  })

  observeEvent(input$mouth_flatten, {
    myPars[['mouthAnchors']] = data.frame('time'=c(0,1),
                                          'ampl'=c(.5,.5))  # default mouth opening
  })

  output$mouth_anchors = renderTable(expr=data.frame('Time, ms'=as.integer(round(myPars$mouthAnchors$time*durSyl_withBreathing())), 'Open'=myPars$mouthAnchors$ampl, row.names=1:length(myPars$mouthAnchors$time)), digits=2, align='c', rownames=FALSE)


  ## AMPLITUDE ENVELOPE LOCAL (PER VOICED SYLLABLE)
  output$plot_ampl_syl = renderPlot({
    amplEnvelope_syl()
  })

  amplEnvelope_syl <- reactive({
    getSmoothContour (anchors=myPars$amplAnchors, plot=TRUE, xaxs="i", xlim=c(0,input$sylDur_mean), ylim=c(0, -throwaway_dB), ampl_floor=0, ampl_ceiling=-throwaway_dB, len=input$sylDur_mean/1000*1000, samplingRate=1000)  # xaxs="i" to enforce exact axis limits, otherwise we exceed the range
  })

  observeEvent(input$plot_ampl_syl_click, {
    click_x = round (round(input$plot_ampl_syl_click$x)/input$sylDur_mean,2)
    click_y = round(input$plot_ampl_syl_click$y)
    # if the click is outside the allowed range of y, re-interpret the click as within the range
    if (click_y<0){click_y=0}
    if (click_y>-throwaway_dB){click_y=-throwaway_dB}

    closest_point_in_time = which.min(abs(myPars$amplAnchors$time - click_x))
    delta_x = abs(myPars$amplAnchors$time[closest_point_in_time] - click_x)
    # if the click is near (within ±5% of the time range) an existing anchor point, we update the anchor according to click location and time
    if (delta_x < 0.05){
      myPars$amplAnchors$ampl[closest_point_in_time] = click_y
      if (closest_point_in_time != 1 & closest_point_in_time != length(myPars$amplAnchors$time)){
        myPars$amplAnchors$time[closest_point_in_time] = click_x
      }
    } else { # otherwise, we simply add the new point as another anchor
      myPars[['amplAnchors']] = data.frame ('time'=c(myPars$amplAnchors$time, click_x),
                                            'ampl'=c(myPars$amplAnchors$ampl, click_y)) # convoluted, but otherwise problems with unwanted dataframe-list conversion, etc
    }
    # sort the updated dataframe of anchors to make sure the point are in the right order (otherwise it's hard to keep track of which are the first and last anchors - and we have to, since those cannot be removed)
    idx_order = order(myPars$amplAnchors$time)
    myPars$amplAnchors$time = myPars$amplAnchors$time[idx_order]
    myPars$amplAnchors$ampl = myPars$amplAnchors$ampl[idx_order]
  })

  observeEvent(input$plot_ampl_syl_dblclick, {
    ref = as.data.frame(myPars[['amplAnchors']])
    ref$time = ref$time*input$sylDur_mean
    closestPoint = nearPoints(ref, input$plot_ampl_syl_dblclick, xvar='time', yvar='ampl', threshold=100000, maxpoints=1)
    idx = as.numeric(rownames(closestPoint))
    if (length(idx)>0 && idx!=1 && idx!=length(myPars$amplAnchors$time)){ # we can remove any anchor except the first and the last (because ampl opening at start and end of sound has to be defined)
      myPars[['amplAnchors']] = data.frame('time'=myPars$amplAnchors$time[-idx],
                                           'ampl'=myPars$amplAnchors$ampl[-idx])
    }
  })

  observeEvent(input$ampl_syl_flatten, {
    myPars[['amplAnchors']] = data.frame('time'=c(0,1),
                                         'ampl'=rep(myPars$amplAnchors$ampl[1],2))  # flat ampl equal to the first ampl anchor
  })

  output$ampl_syl_anchors = renderTable(expr=data.frame('Time, ms'=as.integer(round(myPars$amplAnchors$time*input$sylDur_mean,0)), 'Amplitude'=myPars$amplAnchors$ampl, row.names=1:length(myPars$amplAnchors$time)), digits=0, align='c', rownames=FALSE)


  ## AMPLITUDE ENVELOPE GLOBAL (PER BOUT)
  output$plot_ampl_global = renderPlot({
    amplEnvelope_global()
  })

  amplEnvelope_global <- reactive({
    if (input$nSyl>1){
      getSmoothContour (anchors=myPars$amplAnchors_global, xaxs="i", xlim=c(0,durTotal()), ylim=c(0, -throwaway_dB), ampl_floor=0, ampl_ceiling=-throwaway_dB, len=durTotal()/1000*100, samplingRate=100, plot=TRUE)
    } else {
      plot (1:10, 1:10, type='n', xlab='', ylab='', axes=FALSE)
      text (x=5, y=5, labels = 'Need >1 syllable!', adj=.5, col='blue', cex=1)
    }
  })

  observeEvent(input$plot_ampl_global_click, {
    click_x = round(round(input$plot_ampl_global_click$x)/durTotal(),2)
    click_y = round(input$plot_ampl_global_click$y)
    # if the click is outside the allowed range of y, re-interpret the click as within the range
    if (click_y<0){click_y=0}
    if (click_y>-throwaway_dB){click_y=-throwaway_dB}

    closest_point_in_time = which.min(abs(myPars$amplAnchors_global$time - click_x))
    delta_x = abs(myPars$amplAnchors_global$time[closest_point_in_time] - click_x)
    # if the click is near (within ±5% of the time range) an existing anchor point, we update the pitch of this anchor according to click location and time
    if (delta_x < 0.05){
      myPars$amplAnchors_global$ampl[closest_point_in_time] = click_y
      if (closest_point_in_time != 1 & closest_point_in_time != length(myPars$amplAnchors_global$time)){
        myPars$amplAnchors_global$time[closest_point_in_time] = input$plot_ampl_global_click$x
      }
    } else { # otherwise, we simply add the new point as another anchor
      myPars[['amplAnchors_global']] = data.frame ('time'=c(myPars$amplAnchors_global$time, click_x),
                                                   'ampl'=c(myPars$amplAnchors_global$ampl, click_y)) # convoluted, but otherwise problems with unwanted dataframe-list conversion, etc
    }
    # sort the updated dataframe of anchors to make sure the point are in the right order (otherwise it's hard to keep track of which are the first and last anchors - and we have to, since those cannot be removed)
    idx_order = order(myPars$amplAnchors_global$time)
    myPars$amplAnchors_global$time = myPars$amplAnchors_global$time[idx_order]
    myPars$amplAnchors_global$ampl = myPars$amplAnchors_global$ampl[idx_order]
  })

  observeEvent(input$plot_ampl_global_dblclick, {
    ref = as.data.frame(myPars[['amplAnchors_global']])
    ref$time = ref$time * durTotal()
    closestPoint = nearPoints(ref, input$plot_ampl_global_dblclick, xvar='time', yvar='ampl', threshold=100000, maxpoints=1)
    idx = as.numeric(rownames(closestPoint))
    if (length(idx)>0 && idx!=1 && idx!=length(myPars$amplAnchors_global$time)){ # we can remove any anchor except the first and the last (because ampl opening at start and end of sound has to be defined)
      myPars[['amplAnchors_global']] = data.frame('time'=myPars$amplAnchors_global$time[-idx],
                                                  'ampl'=myPars$amplAnchors_global$ampl[-idx])
    }
  })

  observeEvent(input$ampl_global_flatten, {
    myPars[['amplAnchors_global']] = data.frame('time'=c(0,1),
                                                'ampl'=rep(myPars$amplAnchors_global$ampl[1],2))  # flat ampl equal to the first ampl anchor
  })

  output$ampl_global_anchors = renderTable(expr=data.frame('Time, ms'=as.integer(round(myPars$amplAnchors_global$time*durTotal(),0)), 'Amplitude'=myPars$amplAnchors_global$ampl, row.names=1:length(myPars$amplAnchors_global$time)), digits=0, align='c', rownames=FALSE)


  ## O T H E R    P L O T S
  output$plot_syllables = renderPlot({
    divideIntoSyllables (sylDur_mean=input$sylDur_mean, nSyl=input$nSyl, pauseDur_mean=input$pauseDur_mean, sylDur_min=permittedValues['sylDur_mean','low'], sylDur_max=permittedValues['sylDur_mean','high'], pauseDur_min=permittedValues['pauseDur_mean','low'], pauseDur_max=permittedValues['pauseDur_mean','high'], temperature=input$temperature, plot=TRUE)
  })

  output$plot_variation = renderPlot({
    divideIntoSyllables (sylDur_mean=input$sylDur_mean, nSyl=input$nSyl, pauseDur_mean=input$pauseDur_mean, sylDur_min=permittedValues['sylDur_mean','low'], sylDur_max=permittedValues['sylDur_mean','high'], pauseDur_min=permittedValues['pauseDur_mean','low'], pauseDur_max=permittedValues['pauseDur_mean','high'], temperature=input$temperature, plot=TRUE)
  })

  output$plot_spectrum = renderPlot({
    # meanspec (myPars$sound, f=input$samplingRate, dB='max0', wl=floor(input$spec_windowLength*input$samplingRate/1000/2)*2, flim=c(0,10), main='Spectrum')
    getRolloff(pitch_per_gc=getSmoothContour(myPars$pitchAnchors), rolloff_exp=input$rolloff_exp, rolloff_exp_delta=input$rolloff_exp_delta, quadratic_delta=input$quadratic_delta, quadratic_nHarm=input$quadratic_nHarm, adjust_rolloff_per_kHz=input$adjust_rolloff_per_kHz, baseline_Hz=200, throwaway_dB=throwaway_dB, samplingRate=input$samplingRate, plot=TRUE)
  })

  output$plot_settings = renderPlot({
    meanspec (myPars$sound, f=input$samplingRate, dB='max0', wl=floor(input$spec_windowLength*input$samplingRate/1000/2)*2, flim=c(0,10), main='Spectrum')
  })

  output$plot_consonant = renderPlot({
    meanspec (myPars$sound, f=input$samplingRate, dB='max0', wl=floor(input$spec_windowLength*input$samplingRate/1000/2)*2, flim=c(0,10), main='Spectrum')
  })

  output$plot_exactFormants = renderPlot({
    meanspec (myPars$sound, f=input$samplingRate, dB='max0', wl=floor(input$spec_windowLength*input$samplingRate/1000/2)*2, flim=c(0,10), main='Spectrum')
  })

  output$plot_timbre = renderPlot({
    meanspec (myPars$sound, f=input$samplingRate, dB='max0', wl=floor(input$spec_windowLength*input$samplingRate/1000/2)*2, flim=c(0,10), main='Spectrum')
  })

  output$plot_pitchModulation = renderPlot({
    meanspec (myPars$sound, f=input$samplingRate, dB='max0', wl=floor(input$spec_windowLength*input$samplingRate/1000/2)*2, flim=c(0,10), main='Spectrum')
  })

  output$plot_noise = renderPlot({
    meanspec (myPars$sound, f=input$samplingRate, dB='max0', wl=floor(input$spec_windowLength*input$samplingRate/1000/2)*2, flim=c(0,10), main='Spectrum')
  })

  output$spectrogram <- renderPlot({
    spectro_denoised (myPars$sound, samplingRate=input$samplingRate, wn='gaussian', windowLength=input$spec_windowLength, step=round(input$spec_windowLength/4), osc=TRUE, xlab='Time, ms', ylab='Frequency, kHz', main='Spectrogram', contrast=input$spec_contrast, brightness=input$spec_brightness, colorTheme=input$spec_colorTheme, method=input$spec_method, ylim=c(input$spec_ylim[1], input$spec_ylim[2]))
  })


  ## A U D I O
  # create a string with the call to generateBout() with the par values from the UI
  mycall = reactive({
    paste0('generateBout (nSyl=', input$nSyl,', sylDur_mean=',input$sylDur_mean, ', pauseDur_mean=', input$pauseDur_mean, ', noiseAmount=',input$noiseAmount, ', noiseIntensity=', input$noiseIntensity, ', attackLen=',input$attackLen, ', jitterDep=',input$jitterDep, ', jitterLength_ms=',input$jitterLength_ms, ', vibratoFreq=',input$vibratoFreq, ', vibratoDep=',input$vibratoDep, ', shimmerDep=',input$shimmerDep, ', formantStrength=',input$formantStrength, ', extraFormants_ampl=',input$extraFormants_ampl, ', creakyBreathy=',input$creakyBreathy, ', rolloff_exp=',input$rolloff_exp, ', rolloff_exp_delta=',input$rolloff_exp_delta, ', adjust_rolloff_per_kHz=',input$adjust_rolloff_per_kHz, ', quadratic_delta=',input$quadratic_delta, ', quadratic_nHarm=',input$quadratic_nHarm, ', rolloff_lip=',input$rolloff_lip, ', trill_dep=',input$trill_dep, ', trill_freq=',input$trill_freq, ', rolloff_breathing=',input$rolloff_breathing, ', temperature=',input$temperature, ', min_epoch_length_ms=',input$min_epoch_length_ms, ', g0=',input$g0, ', sideband_width_hz=',input$sideband_width_hz,  ', maleFemale=',input$maleFemale, ', samplingRate=',input$samplingRate, ', windowLength_points=',windowLength_points, ', overlap=',overlap, ', pitch_floor=',input$pitchFloorCeiling[1], ', pitch_ceiling=',input$pitchFloorCeiling[2], ', pitchSamplingRate=',input$pitchSamplingRate, ', vocalTract_length=',input$vocalTract_length, ', repeatBout=',input$repeatBout,
           ', pitchAnchors=data.frame(time=c(',paste0(myPars$pitchAnchors$time,collapse=","),
           '), ampl=c(',paste0(myPars$pitchAnchors$ampl,collapse=","),
           ')), pitchAnchors_global=data.frame(time=c(',paste0(myPars$pitchAnchors_global$time,collapse=","),
           '), ampl=c(',paste0(myPars$pitchAnchors_global$ampl,collapse=","),
           ')), breathingAnchors=data.frame(time=c(', paste0(myPars$breathingAnchors$time,collapse=","),
           '), ampl=c(',paste0(myPars$breathingAnchors$ampl,collapse=","),
           ')), mouthAnchors=data.frame(time=c(',paste0(myPars$mouthAnchors$time,collapse=","),
           '), ampl=c(',paste0(myPars$mouthAnchors$ampl,collapse=","),
           ')), amplAnchors=data.frame(time=c(',paste0(myPars$amplAnchors$time,collapse=","),
           '), ampl=c(',paste0(myPars$amplAnchors$ampl,collapse=","),
           ')), amplAnchors_global=data.frame(time=c(',paste0(myPars$amplAnchors_global$time,collapse=","),
           '), ampl=c(',paste0(myPars$amplAnchors_global$ampl,collapse=","),
           ')), exactFormants=', pickle(myPars$exactFormants),
           ', exactFormants_unvoiced=', pickle(myPars$exactFormants_unvoiced),
           ')')
  })

  observeEvent(mycall(), updateTextInput(session, inputId='mycall', value=mycall())) # show this string to user for copy-pasting if needed

  output$myAudio = renderUI (
    tags$audio(src = "temp.wav", type = "audio/wav", autoplay=NA, controls=NA)
  )

  observeEvent(input$generateAudio, {
    if (!is.null(myPars$myfile)){
      file.remove(paste0('www/',myPars$myfile)) # first remove the previous sound file to avoid cluttering up the www/ folder
    }
    myPars$sound = eval(parse(text=mycall())) # generate audio
    randomID = paste (sample (c(letters, 0:9), 8, replace=TRUE), collapse='')
    myPars$myfile = paste0(randomID, '.wav')
    seewave::savewav (myPars$sound, f=input$samplingRate, filename=paste0('www/',myPars$myfile)) # this is the new sound file. NB: has to be saved in www/ !!!
    output$myAudio = renderUI (
      tags$audio(src = myPars$myfile, type = "audio/wav", autoplay=NA, controls=NA)
    )
  })

  output$saveAudio <- downloadHandler(
    filename = function() as.character(myPars$myfile), # to have '.csv' instead of '.wav'
    content = function(filename) {
      savewav (myPars$sound, f=input$samplingRate, filename=filename)
    }
  )

  # observeEvent(input$saveAudio, {
  #   writeWave (myPars$sound, 'temp.wav')
  # })
}

