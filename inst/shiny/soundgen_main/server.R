server = function(input, output, session) {
  # clean-up of www/ folder: remove all files except temp.wav
  files = list.files('www/')
  files = files[files != 'temp.wav']
  for (f in files){
    file.remove(paste0('www/', f))
  }

  ## S E T U P
  myPars = reactiveValues('myfile' = NULL,
                           'sound' = as.numeric(tuneR::readWave('www/temp.wav')@left),
                            # w/o as.numeric we get integers and spec complains
                           'pitchAnchors' = defaults$pitchAnchors,
                           'pitchAnchors_global' = defaults$pitchAnchors_global,
                           'noiseAnchors' = defaults$noiseAnchors,
                           'mouthAnchors' = defaults$mouthAnchors,
                           'amplAnchors' = defaults$amplAnchors,
                           'amplAnchors_global' = defaults$amplAnchors_global,
                           'exactFormants' = defaults$exactFormants,
                           'exactFormants_noise' = NA,
                           'updateDur' = TRUE,
                           'loaded_presets' = list(),
                           'sylDur_previous' = defaults$sylLen
  )

  durTotal = reactive({
    # the duration of the entire bout without noise,
    # calculated as the sum of voiced syllables and pauses
    ifelse(input$nSyl == 1,
           input$sylLen,
           (input$sylLen * input$nSyl + input$pauseLen * (input$nSyl - 1)))
  })

  durSyl_withBreathing = reactive({ # the duration of a single syllable with noise
    ifelse(!sum(myPars$noiseAnchors$value > throwaway_dB) > 0,
           input$sylLen,
           min(0, myPars$noiseAnchors$time[1]) +
             max(input$sylLen,
                 myPars$noiseAnchors$time[length(myPars$noiseAnchors$time)]))
  })



  ## R E S E T T I N G
  sliders_to_reset = c('')

  # this key function is EXTREMELY bug-prone - careful with what you change! The right order is crucial
  reset_all = reactive({
    # print('running reset_all()')
    myPars$updateDur = FALSE # to prevent duration-related settings in myPars
    # from being updated by event listener observeEvent(input$sylLen)
    # when a new preset is loaded

    # first reset everything to defaults
    for (v in rownames(permittedValues)[1:which(rownames(permittedValues) == 'rolloff_noise')]) {
      updateSliderInput(session, v, value = permittedValues[v,'default'])
    }
    lists_to_default = c('pitchAnchors', 'pitchAnchors_global', 'mouthAnchors',
                         'noiseAnchors', 'amplAnchors', 'amplAnchors_global',
                         'exactFormants', 'exactFormants_noise')
    for (v in lists_to_default) {
      myPars[[v]] = defaults[[v]]
    }

    # ...then load the partial list of presets that are specified (≠ default)
    # for this speaker and call type
    if (length(myPars$loaded_presets) >= 1) {
      # the last user-uploaded preset
      preset = try(myPars$loaded_presets[[length(myPars$loaded_presets)]], silent = TRUE)
    } else {
      # a preset from the library
      preset_text = presets[[input$speaker]] [[input$callType]]
      preset_text = substr(preset_text, 13, nchar(preset_text))  # remove 'soundgen('
      preset_text = paste0('list', preset_text)  # start with 'list('
      preset = try(eval(parse(text = preset_text)), silent = TRUE)
    }
    if (class(preset) == 'list') {
      if(class(preset$exactFormants) == 'character') {
        preset$vowelString = preset$exactFormants  # in case exactFormants = 'aui' etc
      }

      sliders_to_reset = names(preset) [which(names(preset) %in% names(input))]
      for (v in sliders_to_reset) {
        try(updateSliderInput(session, v, value = as.numeric(preset[[v]])))
      }

      myPars_to_reset = names(myPars) [which(names(myPars) %in% names(preset))]
      for (v in myPars_to_reset) {
        myPars[[v]] = preset[[v]]
      }

      if (length(myPars$noiseAnchors) > 1) {
        updateSliderInput(session, 'noiseTime', value = range(myPars$noiseAnchors$time))
      }

      # special cases
      if (!is.null(preset$pitchAnchors)) {
        updateSliderInput(session, 'pitchRange',
                          value = c(round(min(preset$pitchAnchors$value) / 2 ^ (1 / 12), 0),
                                    round(max(preset$pitchAnchors$value) * 2 ^ (1 / 12), 0)))
      }

      if(!is.null(preset$vowelString)) {
        updateTextInput(session, inputId = 'vowelString',
                        value = preset$vowelString)
        updateVowels()
      } else if (is.null(preset$vowelString) & !is.null(preset$exactFormants)) {
        updateTextInput(session, inputId = 'vowelString', value = '')
        updateTextInput(session, inputId = 'exactFormants',
                        value = as.character(call('print', preset$exactFormants)[2]))
        myPars$exactFormants = preset$exactFormants
      } else { # if both are NULL
        updateTextInput(session, inputId = 'vowelString', value = defaultVowel)
        updateVowels()
      }

      if(!is.null(preset$noiseType)) {
        updateSelectInput(session, inputId = 'noiseType',
                          value = preset$noiseType)
        updateNoise()
      } else if (is.null(preset$noiseType) &
                 !is.null(preset$exactFormants_noise)) {
        updateTextInput(session, inputId = 'noiseType', value = '')
        updateTextInput(session, inputId = 'exactFormants_noise',
                        value = as.character(call('print', preset$exactFormants)[2]))
        myPars$exactFormants_noise = preset$exactFormants_noise
      } else { # if both are NULL
        updateTextInput(session, inputId = 'noiseType', value = 'b')
        updateNoise()
      }
    }
  })

  observeEvent(input$callType, {
    myPars$loaded_presets = list()  # remove user-uploaded preset
    reset_all()
  })

  observeEvent(input$speaker, {
    myPars$loaded_presets = list()  # remove user-uploaded preset
    # update available call types for this speaker specified in presets,
    # except the last call type, which is reserved for formants
    updateSelectInput(session, inputId = 'callType',
                      choices = head(names(presets[[input$speaker]]), -1),
                      selected = head(names(presets[[input$speaker]]), 1))
    # NB: this triggers observeEvent(input$callType), and that in turn triggers reset_all()
  })

  observeEvent(input$exactFormants, {
    if (length(input$exactFormants) > 0) {
      try({myPars$exactFormants = eval(parse(text = input$exactFormants))})
      # overrides vowelString
    }
  })

  observeEvent(input$vowelString, {
    updateVowels()
  })

  updateVowels = reactive({
    if (nchar(input$vowelString) > 0) {
      try({converted = convertStringToFormants(input$vowelString,
                                               speaker = input$speaker)})
      if (sum(unlist(converted)) > 0) { # if the converted formant list is not empty
        myPars$exactFormants = converted
        # (...otherwise don't change myPars$exactFormants to prevent crashing)
      }
      updateTextInput(session, inputId = 'exactFormants', value = as.character(call('print', converted)[2]))
    }
  })

  observeEvent(input$exactFormants_noise, {
    if (length(input$exactFormants_noise) > 0) {
      try({myPars$exactFormants_noise =
        eval(parse(text = input$exactFormants_noise))}) # overrides chosen consonant
    }
  })

  observeEvent(input$noiseType, {
    updateNoise()
  })

  updateNoise = reactive({
    if (input$noiseType == 'b') {  # breathing
      myPars$exactFormants_noise = NA
      updateTextInput(session, inputId = 'exactFormants_noise', value = 'NA')
    } else if (nchar(input$noiseType) > 0) {  # TODO - check if this always works!!!
      n = presets[[input$speaker]][['Formants']][input$noiseType] [[1]]
      myPars$exactFormants_noise = n[2:length(n)]
      updateSliderInput(session, inputId = 'rolloff_noise',
                        value = n[['rolloff_noise']])
      updateTextInput(session, inputId = 'exactFormants_noise',
                      value = as.character(call('print', myPars$exactFormants_noise)[2]))
    }
  })

  observeEvent(input$sylLen, {
    # has to be updated manually, b/c noiseAnchors are the only time anchors
    # expressed in ms rather than 0 to 1 (b/c we don't want to rescale
    # pre-syllable aspiration depending on the syllable duration)
    if (myPars$updateDur == TRUE) {
      # doesn't run if updateDur == FALSE (set to F in reset_all())
      scale_coef = input$sylLen / myPars$sylDur_previous
      myPars$noiseAnchors$time[myPars$noiseAnchors$time > 0] =
        round(myPars$noiseAnchors$time[myPars$noiseAnchors$time > 0] * scale_coef)
      # rescale positive time anchors, but not negative ones
      # (ie the length of pre-syllable aspiration does not
      # vary as the syllable length changes - just doesn't seem to make sense)
      updateSliderInput(session, inputId = 'noiseTime',
                        value = range(myPars$noiseAnchors$time))
      myPars$sylDur_previous = input$sylLen  # track the previous value
    }
    myPars$updateDur = TRUE  # execute after the first change (resetting)
  })


  ## P I T C H
  output$plot_intonation = renderPlot({
    myPitchContour()
  })

  myPitchContour = reactive({
    pitch_y_lwr = min (input$pitchRange[1], min(myPars$pitchAnchors$value) / 1.1)
    pitch_y_upr = max (input$pitchRange[2], max(myPars$pitchAnchors$value) * 1.1)
    getSmoothContour (anchors = myPars$pitchAnchors,
                      len = input$sylLen * permittedValues['pitch', 'high'] / 1000,
                      ylim = c(pitch_y_lwr, pitch_y_upr),
                      samplingRate = permittedValues['pitch', 'high'],
                      thisIsPitch = TRUE, plot = TRUE)
  })

  observeEvent(input$plot_intonation_click, {
    click_x = round(input$plot_intonation_click$x / input$sylLen, 2)
    click_y = round(semitonesToHz(input$plot_intonation_click$y))
    # if the click is below or above thresholds, move within thresholds
    if (click_y < permittedValues['pitch', 'low']) {
      click_y = permittedValues['pitch', 'low']
    }
    if (click_y > permittedValues['pitch', 'high']) {
      click_y = permittedValues['pitch', 'high']
    }

    closest_point_in_time = which.min(abs(myPars$pitchAnchors$time - click_x))
    delta_x = abs(myPars$pitchAnchors$time[closest_point_in_time] - click_x)
    # if the click is near (within ±5% of the time range) an existing anchor
    # point, we update the pitch of this anchor according to click location (and
    # the time as well, unless it is the first or the last anchor)
    if (delta_x < 0.05) {
      myPars$pitchAnchors$value[closest_point_in_time] = click_y
      if (closest_point_in_time != 1 &&
          closest_point_in_time != length(myPars$pitchAnchors$time)) {
        myPars$pitchAnchors$time[closest_point_in_time] = click_x
      }
    } else { # otherwise, we simply add the new point as another anchor
      myPars[['pitchAnchors']] = data.frame (
        'time' = c(myPars$pitchAnchors$time, click_x),
        'value' = c(myPars$pitchAnchors$value, click_y)
      ) # convoluted, but otherwise problems with unwanted dataframe-list conversion, etc
    }
    # sort the updated dataframe of pitch anchors to make sure the point are in
    # the right order (otherwise it's hard to keep track of which are the first
    # and last anchors - and we have to, since those cannot be removed)
    idx_order = order(myPars$pitchAnchors$time)
    myPars$pitchAnchors$time = myPars$pitchAnchors$time[idx_order]
    myPars$pitchAnchors$value = myPars$pitchAnchors$value[idx_order]
  })

  observeEvent(input$plot_intonation_dblclick, {
    ref = as.data.frame(myPars[['pitchAnchors']])
    ref$time = ref$time * input$sylLen
    closestPoint = nearPoints(ref, input$plot_intonation_dblclick,
                              xvar = 'time', yvar = 'value',
                              threshold = 100000, maxpoints = 1)
    idx = as.numeric(rownames(closestPoint))
    if (length(idx) > 0 && idx != 1 && idx != length(myPars$pitchAnchors$time)) {
      # we can remove any anchor except the first and the last (because pitch at
      # start and end of sound has to be defined)
      myPars[['pitchAnchors']] = data.frame(
        'time' = myPars$pitchAnchors$time[-idx],
        'value' = myPars$pitchAnchors$value[-idx]
      )
    }
  })

  observeEvent(input$pitch_flatten, {
    # flat pitch equal to the first pitch anchor
    myPars[['pitchAnchors']] = data.frame('time' = c(0,1),
                                          'value' = rep(myPars$pitchAnchors$value[1], 2))
  })

  output$pitch_anchors = renderTable(expr = data.frame(
    'Time, ms' = round(myPars$pitchAnchors$time * input$sylLen, 0),
    'Pitch, Hz' = round(myPars$pitchAnchors$value, 0),
    row.names = 1:length(myPars$pitchAnchors$time)),
    digits = 0, align = 'c', rownames = FALSE)


  ## P I T C H   G L O B A L
  output$plot_intonation_global = renderPlot({
    myPitchContour_global()
  })

  myPitchContour_global <- reactive({
    if (input$nSyl > 1) {
      getDiscreteContour (anchors = myPars$pitchAnchors_global,
                          len = input$nSyl,
                          method = 'spline',
                          plot = TRUE,
                          ylab = 'Pitch delta, semitones',
                          value_floor = permittedValues['pitchDeltas', 'low'],
                          value_ceiling = permittedValues['pitchDeltas', 'high'],
                          ylim = c(permittedValues['pitchDeltas', 'low'],
                                   permittedValues['pitchDeltas', 'high']))
    } else {
      plot (1:10, 1:10, type = 'n', xlab = '', ylab = '', axes = FALSE)
      text (x = 5, y = 5, labels = 'Need >1 syllable!', adj = .5, col = 'blue', cex = 1)
    }

  })

  observeEvent(input$plot_intonation_click_global, {
    timeRange = input$nSyl - 1
    click_x = (input$plot_intonation_click_global$x - 1) / timeRange  # ranges 0 to 1
    click_y = round(input$plot_intonation_click_global$y, 1)
    # if the click is below or above thresholds, move within thresholds
    if (click_y < permittedValues['pitchDeltas', 'low']) {
      click_y = permittedValues['pitchDeltas', 'low']
      }
    if (click_y > permittedValues['pitchDeltas', 'high']) {
      click_y = permittedValues['pitchDeltas', 'high']
      }

    closest_point_in_time = which.min(abs(myPars$pitchAnchors_global$time - click_x))
    delta_x = abs(myPars$pitchAnchors_global$time[closest_point_in_time] - click_x)
    # if the click is near (within ±20% of the time range) an existing anchor
    # point, we update the pitch of this anchor according to click location (and
    # the time as well, unless it is the first or the last anchor)
    if (delta_x < 0.2) {
      myPars$pitchAnchors_global$value[closest_point_in_time] = click_y
      if (closest_point_in_time != 1 &&
          closest_point_in_time != length(myPars$pitchAnchors_global$time)) {
        myPars$pitchAnchors_global$time[closest_point_in_time] = click_x
      }
    }  else { # otherwise, we simply add the new point as another anchor
      myPars[['pitchAnchors_global']] = data.frame (
        'time' = c(myPars$pitchAnchors_global$time, click_x),
        'value' = c(myPars$pitchAnchors_global$value, click_y)
      ) # convoluted, but otherwise problems with unwanted dataframe-list conversion, etc
    }
    # sort the updated dataframe of pitch anchors to make sure the point are in the right order (otherwise it's hard to keep track of which are the first and last anchors - and we have to, since those cannot be removed)
    idx_order = order(myPars$pitchAnchors_global$time)
    myPars$pitchAnchors_global$time = myPars$pitchAnchors_global$time[idx_order]
    myPars$pitchAnchors_global$value = myPars$pitchAnchors_global$value[idx_order]
  })

  observeEvent(input$plot_intonation_dblclick_global, {
    ref = as.data.frame(myPars[['pitchAnchors_global']])
    ref$time = ref$time * (input$nSyl - 1) + 1
    closestPoint = nearPoints(ref, input$plot_intonation_dblclick_global,
                              xvar = 'time', yvar = 'value',
                              threshold = 100000, maxpoints = 1)
    idx = as.numeric(rownames(closestPoint))
    if (length(idx) > 0 && idx != 1 &&
        idx != length(myPars$pitchAnchors_global$time)) {
      # we can remove any anchor except the first and the last (because pitch at
      # start and end of sound has to be defined)
      myPars[['pitchAnchors_global']] = data.frame(
        'time' = myPars$pitchAnchors_global$time[-idx],
        'value' = myPars$pitchAnchors_global$value[-idx]
      )
    }
  })

  observeEvent(input$pitch_flatten_global, {
    # flat pitch modulation across syllables
    myPars[['pitchAnchors_global']] = data.frame('time' = c(0,1),
                                                 'value' = c(0,0))
  })

  output$pitch_anchors_global = renderTable(expr = data.frame(
    'Time 0 to 1' = myPars$pitchAnchors_global$time,
    'Pitch delta, semitones' = round(myPars$pitchAnchors_global$value,0),
    row.names = 1:length(myPars$pitchAnchors_global$time)),
    digits = 2, align = 'c', rownames = FALSE)


  ## BREATHING
  output$plot_unvoiced = renderPlot({
    myBreathingContour()
  })

  myBreathingContour = reactive({
    br_xlim_low = min(input$noiseTime[1], 0)
    br_xlim_high = max(input$noiseTime[2], input$sylLen)
    br_ylim_low = permittedValues['noise_ampl', 'low']
    br_ylim_high = permittedValues['noise_ampl', 'high']
    nTicks = length(seq(br_ylim_low, br_ylim_high, by = 20)) - 1
    getSmoothContour(anchors = myPars$noiseAnchors,
                     xlim = c(br_xlim_low, br_xlim_high),
                     ylim = c(br_ylim_low, br_ylim_high),
                     voiced = input$sylLen,
                     contourLabel = 'noise',
                     value_floor = br_ylim_low,
                     value_ceiling = br_ylim_high,
                     yaxp = c(br_ylim_low, br_ylim_high, nTicks),
                     plot = TRUE)
  })

  observeEvent(input$plot_unvoiced_click, {
    click_x = round(input$plot_unvoiced_click$x)
    click_y = round(input$plot_unvoiced_click$y)
    # if the click is outside the allowed range of y, re-interpret the click as within the range
    if (click_y < permittedValues['noise_ampl', 'low']) {
      click_y = permittedValues['noise_ampl', 'low']
    }
    if (click_y > permittedValues['noise_ampl', 'high']) {
      click_y = permittedValues['noise_ampl', 'high']
    }

    closest_point_in_time = which.min(abs(myPars$noiseAnchors$time - click_x))
    delta_x = abs(myPars$noiseAnchors$time[closest_point_in_time] - click_x)
    # if the click is near (within ±5% of the time range) an existing anchor
    # point, we update the ampl of this anchor according to click location and time
    if (delta_x < 0.05 * durSyl_withBreathing()) {
      myPars$noiseAnchors$value[closest_point_in_time] = click_y
      myPars$noiseAnchors$time[closest_point_in_time] = click_x
    } else { # otherwise, we simply add the new point as another anchor
      myPars[['noiseAnchors']] = data.frame (
        'time' = c(myPars$noiseAnchors$time, click_x),
        'value' = c(myPars$noiseAnchors$value, click_y)
      ) # convoluted, but otherwise problems with unwanted dataframe-list conversion, etc
    }
    # sort the updated dataframe of pitch anchors to make sure the point are in
    # the right order (otherwise it's hard to keep track of which are the first
    # and last anchors - and we have to, since those cannot be removed)
    idx_order = order(myPars$noiseAnchors$time)
    myPars$noiseAnchors$time = myPars$noiseAnchors$time[idx_order]
    myPars$noiseAnchors$value = myPars$noiseAnchors$value[idx_order]
  })

  observeEvent(input$plot_unvoiced_dblclick, {
    closestPoint = nearPoints(as.data.frame(myPars[['noiseAnchors']]),
                              input$plot_unvoiced_dblclick, xvar = 'time',
                              yvar = 'value', threshold = 100000, maxpoints = 1)
    idx = as.numeric(rownames(closestPoint))
    if (length(idx) > 0 && length(myPars$noiseAnchors$time) > 2) {
      # we can remove any anchor, as long as there will be at least two anchors
      # left (to know what noise duration should be)
      myPars[['noiseAnchors']] = data.frame(
        'time' = myPars$noiseAnchors$time[-idx],
        'value' = myPars$noiseAnchors$value[-idx]
      )
    }
  })

  observeEvent(input$noise_flatten, {
    # flat pitch equal to the first pitch anchor
    myPars[['noiseAnchors']] = data.frame(
      'time' = myPars$noiseAnchors$time[c(1,length(myPars$noiseAnchors$time))],
      'value' = rep(myPars$noiseAnchors$value[1],2)
    )})

  output$noise_anchors = renderTable(expr = data.frame(
    'Time, ms' = round(myPars$noiseAnchors$time,0),
    'Amplitude, dB' = round(myPars$noiseAnchors$value,0),
    row.names = 1:length(myPars$noiseAnchors$time)),
    digits = 0, align = 'c', rownames = FALSE)


  ## MOUTH OPENING
  output$plot_mouth = renderPlot({
    myMouthOpening()
  })

  myMouthOpening = reactive({
    getSmoothContour(
      anchors = myPars$mouthAnchors,
      len = durSyl_withBreathing() / 1000 * 1000,
      samplingRate = 1000,
      contourLabel = 'mouth',
      xlim = c(0, durSyl_withBreathing()),
      xaxs = "i",
      ylim = c(permittedValues['mouthOpening', 'low'], permittedValues['mouthOpening', 'high']),
      value_floor = permittedValues['mouthOpening', 'low'],
      value_ceiling = permittedValues['mouthOpening', 'high'],
      plot = TRUE)
    # xaxs = "i" to enforce exact axis limits, otherwise we exceed the range.
    # OR: xlim = range(myPars$noiseAnchors$time)
  })

  observeEvent(input$plot_mouth_click, {
    click_x = round(round(input$plot_mouth_click$x) / durSyl_withBreathing(), 2)
    click_y = round(input$plot_mouth_click$y, 2)
    # if the click is outside the allowed range of y, re-interpret the click
    # as within the range
    if (click_y < permittedValues['mouthOpening', 'low']) {
      click_y = permittedValues['mouthOpening', 'low']
    }
    if (click_y > permittedValues['mouthOpening', 'high']) {
      click_y = permittedValues['mouthOpening', 'high']
    }

    closest_point_in_time = which.min(abs(myPars$mouthAnchors$time - click_x))
    delta_x = abs(myPars$mouthAnchors$time[closest_point_in_time] - click_x)
    # if the click is near (within ±5% of the time range) an existing anchor
    # point, we update the pitch of this anchor according to click location and time
    if (delta_x < 0.05) {
      myPars$mouthAnchors$value[closest_point_in_time] = click_y
      if (closest_point_in_time != 1 &
          closest_point_in_time != length(myPars$mouthAnchors$time)) {
        myPars$mouthAnchors$time[closest_point_in_time] = click_x
      }
    } else { # otherwise, we simply add the new point as another anchor
      myPars[['mouthAnchors']] = data.frame (
        'time' = c(myPars$mouthAnchors$time, click_x),
        'value' = c(myPars$mouthAnchors$value, click_y)
      ) # convoluted, but otherwise problems with unwanted dataframe-list conversion, etc
    }
    # sort the updated dataframe of pitch anchors to make sure the point are in
    # the right order (otherwise it's hard to keep track of which are the first
    # and last anchors - and we have to, since those cannot be removed)
    idx_order = order(myPars$mouthAnchors$time)
    myPars$mouthAnchors$time = myPars$mouthAnchors$time[idx_order]
    myPars$mouthAnchors$value = myPars$mouthAnchors$value[idx_order]
  })

  observeEvent(input$plot_mouth_dblclick, {
    ref = as.data.frame(myPars[['mouthAnchors']])
    ref$time = ref$time * durSyl_withBreathing()
    closestPoint = nearPoints(ref, input$plot_mouth_dblclick, xvar = 'time',
                              yvar = 'value', threshold = 100000, maxpoints = 1)
    idx = as.numeric(rownames(closestPoint))
    # we can remove any anchor except the first and the last (because mouth
    # opening at start and end of sound has to be defined)
    if (length(idx) > 0 && idx != 1 && idx != length(myPars$mouthAnchors$time)) {
      myPars[['mouthAnchors']] = data.frame('time' = myPars$mouthAnchors$time[-idx],
                                            'value' = myPars$mouthAnchors$value[-idx])
    }
  })

  observeEvent(input$mouth_flatten, {
    myPars[['mouthAnchors']] = data.frame('time' = c(0,1),
                                          'value' = c(.5,.5))  # default mouth opening
  })

  output$mouth_anchors = renderTable(expr = data.frame(
    'Time, ms' = as.integer(round(myPars$mouthAnchors$time * durSyl_withBreathing())),
    'Open' = myPars$mouthAnchors$value,
    row.names = 1:length(myPars$mouthAnchors$time)),
    digits = 2, align = 'c', rownames = FALSE)


  ## AMPLITUDE ENVELOPE LOCAL (PER VOICED SYLLABLE)
  output$plot_ampl_syl = renderPlot({
    amplEnvelope_syl()
  })

  amplEnvelope_syl <- reactive({
    getSmoothContour (anchors = myPars$amplAnchors,
                      xaxs = "i",
                      xlim = c(0, input$sylLen),
                      ylim = c(0, -throwaway_dB),
                      value_floor = 0, value_ceiling = -throwaway_dB,
                      len = input$sylLen / 1000 * 1000,
                      samplingRate = 1000, plot = TRUE)
    # xaxs = "i" to enforce exact axis limits, otherwise we exceed the range
  })

  observeEvent(input$plot_ampl_syl_click, {
    click_x = round (round(input$plot_ampl_syl_click$x)/input$sylLen,2)
    click_y = round(input$plot_ampl_syl_click$y)
    # if the click is outside the allowed range of y, re-interpret the click
    # as within the range
    if (click_y < 0) click_y = 0
    if (click_y > -throwaway_dB) click_y = -throwaway_dB

    closest_point_in_time = which.min(abs(myPars$amplAnchors$time - click_x))
    delta_x = abs(myPars$amplAnchors$time[closest_point_in_time] - click_x)
    # if the click is near (within ±5% of the time range) an existing anchor point,
    # we update the anchor according to click location and time
    if (delta_x < 0.05) {
      myPars$amplAnchors$value[closest_point_in_time] = click_y
      if (closest_point_in_time != 1 &
          closest_point_in_time != length(myPars$amplAnchors$time)) {
        myPars$amplAnchors$time[closest_point_in_time] = click_x
      }
    } else {  # otherwise, we simply add the new point as another anchor
      myPars[['amplAnchors']] = data.frame (
        'time' = c(myPars$amplAnchors$time, click_x),
        'value' = c(myPars$amplAnchors$value, click_y)
      ) # convoluted, but otherwise problems with unwanted dataframe-list conversion, etc
    }
    # sort the updated dataframe of anchors to make sure the point are in the
    # right order (otherwise it's hard to keep track of which are the first and
    # last anchors - and we have to, since those cannot be removed)
    idx_order = order(myPars$amplAnchors$time)
    myPars$amplAnchors$time = myPars$amplAnchors$time[idx_order]
    myPars$amplAnchors$value = myPars$amplAnchors$value[idx_order]
  })

  observeEvent(input$plot_ampl_syl_dblclick, {
    ref = as.data.frame(myPars[['amplAnchors']])
    ref$time = ref$time * input$sylLen
    closestPoint = nearPoints(ref, input$plot_ampl_syl_dblclick, xvar = 'time',
                              yvar = 'value', threshold = 100000, maxpoints = 1)
    idx = as.numeric(rownames(closestPoint))
    # we can remove any anchor except the first and the last (because ampl
    # opening at start and end of sound has to be defined)
    if (length(idx) > 0 && idx != 1 && idx != length(myPars$amplAnchors$time)) {
      myPars[['amplAnchors']] = data.frame('time' = myPars$amplAnchors$time[-idx],
                                           'value' = myPars$amplAnchors$value[-idx])
    }
  })

  observeEvent(input$ampl_syl_flatten, {
    # flat ampl equal to the first ampl anchor
    myPars[['amplAnchors']] = data.frame('time' = c(0,1),
                                         'value' = rep(myPars$amplAnchors$value[1], 2))
  })

  output$ampl_syl_anchors = renderTable(expr = data.frame(
    'Time, ms' = as.integer(round(myPars$amplAnchors$time * input$sylLen, 0)),
    'Amplitude' = myPars$amplAnchors$value,
    row.names = 1:length(myPars$amplAnchors$time)),
    digits = 0, align = 'c', rownames = FALSE)


  ## AMPLITUDE ENVELOPE GLOBAL (PER BOUT)
  output$plot_ampl_global = renderPlot({
    amplEnvelope_global()
  })

  amplEnvelope_global = reactive({
    if (input$nSyl > 1) {
      getSmoothContour (anchors = myPars$amplAnchors_global,
                        xaxs = "i", xlim = c(0, durTotal()),
                        ylim = c(0, -throwaway_dB),
                        value_floor = 0,
                        value_ceiling = -throwaway_dB,
                        len = durTotal() / 1000 * 100,
                        samplingRate = 100, plot = TRUE)
    } else {
      plot (1:10, 1:10, type = 'n', xlab = '', ylab = '', axes = FALSE)
      text (x = 5, y = 5, labels = 'Need >1 syllable!', adj = .5, col = 'blue', cex = 1)
    }
  })

  observeEvent(input$plot_ampl_global_click, {
    click_x = round(round(input$plot_ampl_global_click$x) / durTotal(), 2)
    click_y = round(input$plot_ampl_global_click$y)
    # if the click is outside the allowed range of y, re-interpret the click as within the range
    if (click_y < 0) click_y = 0
    if (click_y > -throwaway_dB) click_y = -throwaway_dB

    closest_point_in_time = which.min(abs(myPars$amplAnchors_global$time - click_x))
    delta_x = abs(myPars$amplAnchors_global$time[closest_point_in_time] - click_x)
    # if the click is near (within ±5% of the time range) an existing anchor
    # point, we update the pitch of this anchor according to click location and time
    if (delta_x < 0.05) {
      myPars$amplAnchors_global$value[closest_point_in_time] = click_y
      if (closest_point_in_time != 1 &
          closest_point_in_time != length(myPars$amplAnchors_global$time)) {
        myPars$amplAnchors_global$time[closest_point_in_time] =
          input$plot_ampl_global_click$x
      }
    } else { # otherwise, we simply add the new point as another anchor
      myPars[['amplAnchors_global']] = data.frame (
        'time' = c(myPars$amplAnchors_global$time, click_x),
        'value' = c(myPars$amplAnchors_global$value, click_y)
      ) # convoluted, but otherwise problems with unwanted dataframe-list conversion, etc
    }
    # sort the updated dataframe of anchors to make sure the point are in the
    # right order (otherwise it's hard to keep track of which are the first and
    # last anchors - and we have to, since those cannot be removed)
    idx_order = order(myPars$amplAnchors_global$time)
    myPars$amplAnchors_global$time = myPars$amplAnchors_global$time[idx_order]
    myPars$amplAnchors_global$value = myPars$amplAnchors_global$value[idx_order]
  })

  observeEvent(input$plot_ampl_global_dblclick, {
    ref = as.data.frame(myPars[['amplAnchors_global']])
    ref$time = ref$time * durTotal()
    closestPoint = nearPoints(ref, input$plot_ampl_global_dblclick,  xvar = 'time',
                              yvar = 'value', threshold = 100000, maxpoints = 1)
    idx = as.numeric(rownames(closestPoint))
    # we can remove any anchor except the first and the last (because ampl
    # opening at start and end of sound has to be defined)
    if (length(idx) > 0 && idx != 1 &&
        idx != length(myPars$amplAnchors_global$time)) {
      myPars[['amplAnchors_global']] = data.frame(
        'time' = myPars$amplAnchors_global$time[-idx],
        'value' = myPars$amplAnchors_global$value[-idx]
      )
    }
  })

  observeEvent(input$ampl_global_flatten, {
    # flat ampl equal to the first ampl anchor
    myPars[['amplAnchors_global']] = data.frame('time' = c(0,1),
      'value' = rep(myPars$amplAnchors_global$value[1], 2))
  })

  output$ampl_global_anchors = renderTable(expr = data.frame(
    'Time, ms' = as.integer(round(myPars$amplAnchors_global$time * durTotal(), 0)),
    'Amplitude' = myPars$amplAnchors_global$value,
    row.names = 1:length(myPars$amplAnchors_global$time)),
    digits = 0, align = 'c', rownames = FALSE)


  ## O T H E R    P L O T S
  output$plot_syllables = renderPlot({
    divideIntoSyllables (sylLen = input$sylLen,
                         nSyl = input$nSyl,
                         pauseLen = input$pauseLen,
                         sylDur_min = permittedValues['sylLen', 'low'],
                         sylDur_max = permittedValues['sylLen', 'high'],
                         pauseDur_min = permittedValues['pauseLen', 'low'],
                         pauseDur_max = permittedValues['pauseLen', 'high'],
                         temperature = input$temperature, plot = TRUE)
  })

  output$plot_variation = renderPlot({
    divideIntoSyllables (sylLen = input$sylLen,
                         nSyl = input$nSyl,
                         pauseLen = input$pauseLen,
                         sylDur_min = permittedValues['sylLen', 'low'],
                         sylDur_max = permittedValues['sylLen', 'high'],
                         pauseDur_min = permittedValues['pauseLen', 'low'],
                         pauseDur_max = permittedValues['pauseLen', 'high'],
                         temperature = input$temperature, plot = TRUE)
  })

  output$plot_spectrum = renderPlot({
    # seewave::meanspec(myPars$sound, f = input$samplingRate, dB = 'max0',
    #   wl = floor(input$spec_windowLength*input$samplingRate/1000/2)*2,
    #   flim = c(0,10), main = 'Spectrum')
    getRolloff(pitch_per_gc = getSmoothContour(
      myPars$pitchAnchors),
      rolloff = input$rolloff,
      rolloffAdjust_per_octave = input$rolloffAdjust_per_octave,
      rolloffAdjust_quadratic = input$rolloffAdjust_quadratic,
      rolloffAdjust_quadratic_nHarm = input$rolloffAdjust_quadratic_nHarm,
      rolloffAdjust_per_kHz = input$rolloffAdjust_per_kHz,
      baseline_Hz = 200,
      throwaway_dB = throwaway_dB,
      samplingRate = input$samplingRate,
      plot = TRUE
    )
  })

  output$plot_settings = renderPlot({
    seewave::meanspec(myPars$sound,  f = input$samplingRate, dB = 'max0',
      wl = floor(input$spec_windowLength * input$samplingRate / 1000 / 2) * 2,
      flim = c(0, 10), main = 'Spectrum')
  })

  output$plot_consonant = renderPlot({
    seewave::meanspec(myPars$sound,  f = input$samplingRate, dB = 'max0',
      wl = floor(input$spec_windowLength * input$samplingRate / 1000 / 2) * 2,
      flim = c(0, 10), main = 'Spectrum')
  })

  output$plot_exactFormants = renderPlot({
    getSpectralEnvelope(nr = floor(input$spec_windowLength * input$samplingRate / 1000 / 2),
                        nc = 100,
                        exactFormants = myPars$exactFormants,
                        formantDep = input$formantDep,
                        rolloff_lipRad = input$rolloff_lipRad,
                        mouthAnchors = myPars$mouthAnchors,
                        vocalTract = input$vocalTract,
                        temperature = input$temperature,
                        extraFormants_stochastic = input$extraFormants_stochastic,
                        samplingRate = input$samplingRate,
                        plot = TRUE,
                        dur_ms = durSyl_withBreathing(),
                        xlab = 'Time, ms',
                        ylab = 'Frequency, kHz',
                        colorTheme = input$spec_colorTheme
                       )
  })

  output$plot_timbre = renderPlot({
    seewave::meanspec(myPars$sound,  f = input$samplingRate, dB = 'max0',
      wl = floor(input$spec_windowLength * input$samplingRate / 1000 / 2) * 2,
      flim = c(0, 10), main = 'Spectrum')
  })

  output$plot_pitchModulation = renderPlot({
    seewave::meanspec(myPars$sound,  f = input$samplingRate, dB = 'max0',
      wl = floor(input$spec_windowLength * input$samplingRate / 1000 / 2) * 2,
      flim = c(0, 10), main = 'Spectrum')
  })

  output$plot_noise = renderPlot({
    seewave::meanspec(myPars$sound,  f = input$samplingRate, dB = 'max0',
      wl = floor(input$spec_windowLength * input$samplingRate / 1000 / 2) * 2,
      flim = c(0, 10), main = 'Spectrum')
  })

  output$spectrogram = renderPlot({
    spec(myPars$sound,
         samplingRate = input$samplingRate,
         wn = 'gaussian', windowLength = input$spec_windowLength,
         step = round(input$spec_windowLength / 4),
         osc = TRUE, xlab = 'Time, ms', ylab = 'Frequency, kHz',
         main = 'Spectrogram', contrast = input$spec_contrast,
         brightness = input$spec_brightness,
         colorTheme = input$spec_colorTheme,
         method = input$spec_method,
         ylim = c(input$spec_ylim[1], input$spec_ylim[2]))
  })


  ## A U D I O
  # create a string with the call to soundgen() with the par values from the UI
  mycall = reactive({
    arg_list = list(
      repeatBout = input$repeatBout,
      nSyl = input$nSyl,
      sylLen = input$sylLen,
      pauseLen = input$pauseLen,
      pitchAnchors = myPars$pitchAnchors,
      pitchAnchors_global = myPars$pitchAnchors_global,
      temperature = input$temperature,
      maleFemale = input$maleFemale,
      creakyBreathy = input$creakyBreathy,
      pitchEffects_amount = input$pitchEffects_amount,
      pitchEffects_intensity = input$pitchEffects_intensity,
      jitterDep = input$jitterDep,
      jitterLen = input$jitterLen,
      vibratoFreq = input$vibratoFreq,
      vibratoDep = input$vibratoDep,
      shimmerDep = input$shimmerDep,
      attackLen = input$attackLen,
      rolloff = input$rolloff,
      rolloffAdjust_per_octave = input$rolloffAdjust_per_octave,
      rolloffAdjust_quadratic = input$rolloffAdjust_quadratic,
      rolloffAdjust_quadratic_nHarm = input$rolloffAdjust_quadratic_nHarm,
      rolloffAdjust_per_kHz = input$rolloffAdjust_per_kHz,
      rolloff_lipRad = input$rolloff_lipRad,
      exactFormants = myPars$exactFormants,
      formantDep = input$formantDep,
      extraFormants_stochastic = input$extraFormants_stochastic,
      vocalTract = input$vocalTract,
      subFreq = input$subFreq,
      subDep = input$subDep,
      shortestEpoch = input$shortestEpoch,
      trillDep = input$trillDep,
      trillFreq = input$trillFreq,
      noiseAnchors = myPars$noiseAnchors,
      exactFormants_noise = myPars$exactFormants_noise,
      rolloff_noise = input$rolloff_noise,
      mouthAnchors = myPars$mouthAnchors,
      amplAnchors = myPars$amplAnchors,
      amplAnchors_global = myPars$amplAnchors_global,
      samplingRate = input$samplingRate,
      pitch_floor = input$pitchFloorCeiling[1],
      pitch_ceiling = input$pitchFloorCeiling[2],
      pitch_samplingRate = input$pitch_samplingRate
    )
    # simplify arg_list by removing values that are the same as defaults
    idx_same = apply(matrix(1:length(arg_list)), 1, function(x) {
      temp = all.equal(arg_list[[x]], defaults[[names(arg_list)[x]]], check.attributes = FALSE)
      if (class(temp) == 'character') temp = FALSE
      temp
    })
    not_defaults = which(idx_same != TRUE)
    arg_list = arg_list[not_defaults]
    arg_list
  })

  # show simplified function call as string to user for copy-pasting
  observeEvent(mycall(),
               updateTextInput(session, inputId = 'mycall',
                 value = {
                   temp = as.character(call('print', mycall())[2])
                   paste0('soundgen', substr(temp, 5, nchar(temp)))
                 })
  )

  output$myAudio = renderUI (
    tags$audio(src = "temp.wav", type = "audio/wav", autoplay = NA, controls = NA)
  )

  observeEvent(input$generateAudio, {
    # first remove the previous sound file to avoid cluttering up the www/ folder
    if (!is.null(myPars$myfile)){
      file.remove(paste0('www/', myPars$myfile))
    }
    myPars$sound = do.call('soundgen', mycall()) # eval(parse(text = mycall()))  # generate audio
    randomID = paste (sample (c(letters, 0:9), 8, replace = TRUE), collapse = '')
    myPars$myfile = paste0(randomID, '.wav')
    # this is the new sound file. NB: has to be saved in www/ !!!
    seewave::savewav(myPars$sound, f = input$samplingRate,
                     filename = paste0('www/', myPars$myfile))
    output$myAudio = renderUI(
      tags$audio(src = myPars$myfile, type = "audio/wav", autoplay = NA, controls = NA)
    )
  })

  output$saveAudio = downloadHandler(
    filename = function() as.character(myPars$myfile), # to have '.csv' instead of '.wav'
    content = function(filename) {
      seewave::savewav(myPars$sound, f = input$samplingRate, filename = filename)
    }
  )

  observeEvent(input$import_preset, {
    # replace "soundgen" with "list" and parse
    new_preset_text = substr(input$user_preset, 13, nchar(input$user_preset))
    new_preset_text = paste0('list', new_preset_text)
    new_preset_list = try(eval(parse(text = new_preset_text)), silent = TRUE)

    # create a new preset
    new_presetID = paste (sample (c(letters, 0:9), 8, replace = TRUE), collapse = '')
    myPars$loaded_presets[[new_presetID]] = new_preset_list

    # update sliders
    reset_all()
  })
}
