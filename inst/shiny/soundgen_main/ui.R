ui = fluidPage(
  headerPanel('SoundGen 4.0'),

  fluidRow(
    column (6,
            tabsetPanel("CONTROLS", id='parGroup',
                        navbarMenu ("Main",
                                    tabPanel("Intonation syllable",
                                             sidebarLayout(
                                               sidebarPanel(
                                                 actionButton(inputId = "pitch_flatten", label = "Flatten pitch contour"),
                                                 shinyBS::bsPopover(id='pitch_flatten', title=NULL, content='Revert to a flat intonation contour with pitch equal to the first (left) anchor', placement="right", trigger="hover"),
                                                 sliderInput('pitchRange', 'Plotted pitch range, Hz', value=c(75,150), min=permittedValues['pitch', 'low'], max=permittedValues['pitch', 'high'], step=permittedValues['pitch', 'step']),
                                                 shinyBS::bsPopover(id='pitchRange', title=NULL, content='Set upper / lower limit separately or drag in between the markers to shift both limits simultaneously', placement="right", trigger="hover"),
                                                 tableOutput("pitch_anchors"), width=6
                                               ),
                                               mainPanel(
                                                 plotOutput('plot_intonation', click = "plot_intonation_click", dblclick = dblclickOpts(id = "plot_intonation_dblclick")), width=6
                                               )
                                             )
                                    ),

                                    tabPanel("Intonation global",
                                             sidebarLayout(
                                               sidebarPanel(
                                                 actionButton(inputId = "pitch_flatten_global", label = "Flatten pitch contour"),
                                                 shinyBS::bsPopover(id='pitch_flatten_global', title=NULL, content='No global pitch modulation from syllable to syllable', placement="right", trigger="hover"),
                                                 tableOutput("pitch_anchors_global"), width=6
                                               ),
                                               mainPanel(
                                                 plotOutput('plot_intonation_global', click = "plot_intonation_click_global", dblclick = dblclickOpts(id = "plot_intonation_dblclick_global")), width=6
                                               )
                                             )
                                    ),

                                    tabPanel("Syllables",
                                             sidebarLayout(
                                               sidebarPanel(
                                                 sliderInput('sylDur_mean', 'Syllable length, ms', value=permittedValues['sylDur_mean','default'], min=permittedValues['sylDur_mean', 'low'], max=permittedValues['sylDur_mean', 'high'], step=permittedValues['sylDur_mean','step']),
                                                 shinyBS::bsPopover(id='sylDur_mean', title=NULL, content='Average duration of a continuous VOICED syllable (breathing is added separately and may fill in the pauses)', placement="right", trigger="hover"),
                                                 sliderInput('nSyl', 'Number of syllables', value=permittedValues['nSyl','default'], min=permittedValues['nSyl', 'low'], max=permittedValues['nSyl', 'high'], step=permittedValues['nSyl','step']),
                                                 shinyBS::bsPopover(id='nSyl', title=NULL, content='Each sound consists of one or several syllables separated by pauses', placement="right", trigger="hover"),
                                                 sliderInput('pauseDur_mean', 'Pause, ms', value=permittedValues['pauseDur_mean','default'], min=permittedValues['pauseDur_mean', 'low'], max=permittedValues['pauseDur_mean', 'high'], step=permittedValues['pauseDur_mean','step']),
                                                 shinyBS::bsPopover(id='pauseDur_mean', title=NULL, content='Average pause between syllables', placement="right", trigger="hover"),
                                                 sliderInput('repeatBout', 'Repeat bout # times', value=permittedValues['repeatBout','default'], min=permittedValues['repeatBout', 'low'], max=permittedValues['repeatBout', 'high'], step=permittedValues['repeatBout','step']),
                                                 shinyBS::bsPopover(id='repeatBout', title=NULL, content='Play the whole bout several times with a specified pause', placement="right", trigger="hover"),width=6
                                               ),
                                               mainPanel(
                                                 plotOutput('plot_syllables'), width=6
                                               )
                                             )
                                    ),

                                    tabPanel("Amplitude syllable",
                                             sidebarLayout(
                                               sidebarPanel(
                                                 sliderInput ('attackLen', 'Attack length, ms', value=permittedValues['attackLen','default'], min=permittedValues['attackLen', 'low'], max=permittedValues['attackLen', 'high'], step=permittedValues['attackLen','step']),
                                                 shinyBS::bsPopover(id='attackLen', title=NULL, content='Does the voice start/end abruptly or with a "fade-in/out"?', placement="right", trigger="hover"),
                                                 sliderInput('trill_dep', 'Trill depth', value=permittedValues['trill_dep','default'], min=permittedValues['trill_dep', 'low'], max=permittedValues['trill_dep', 'high'], step=permittedValues['trill_dep','step']),
                                                 shinyBS::bsPopover(id='trill_dep', title=NULL, content='Amplitude fo uvular trill as in [r]', placement="right", trigger="hover"),
                                                 sliderInput('trill_freq', 'Trill frequency, Hz', value=permittedValues['trill_freq','default'], min=permittedValues['trill_freq', 'low'], max=permittedValues['trill_freq', 'high'], step=permittedValues['trill_freq','step']),
                                                 shinyBS::bsPopover(id='trill_freq', title=NULL, content='Frequency of rapid sinusoidal amplitude modulation (trill)', placement="right", trigger="hover"),
                                                 actionButton(inputId = "ampl_syl_flatten", label = "Flatten amplitude envelope"),
                                                 shinyBS::bsPopover(id='ampl_syl_flatten', title=NULL, content='Same amplitude over the entire syllable', placement="right", trigger="hover"),
                                                 tableOutput("ampl_syl_anchors"), width=6
                                               ),
                                               mainPanel(
                                                 plotOutput('plot_ampl_syl', click = "plot_ampl_syl_click", dblclick = dblclickOpts(id = "plot_ampl_syl_dblclick")), width=6
                                               )
                                             )
                                    ),

                                    tabPanel("Amplitude global",
                                             sidebarLayout(
                                               sidebarPanel(
                                                 actionButton(inputId = "ampl_global_flatten", label = "Flatten amplitude envelope"),
                                                 shinyBS::bsPopover(id='ampl_global_flatten', title=NULL, content='Same amplitude over the entire bout', placement="right", trigger="hover"),
                                                 tableOutput("ampl_global_anchors"), width=6
                                               ),
                                               mainPanel(
                                                 plotOutput('plot_ampl_global', click = "plot_ampl_global_click", dblclick = dblclickOpts(id = "plot_ampl_global_dblclick")), width=6
                                               )
                                             )
                                    ),

                                    tabPanel("Variation",
                                             sidebarLayout(
                                               sidebarPanel(
                                                 sliderInput('temperature', 'Temperature', value=permittedValues['temperature','default'], min=permittedValues['temperature', 'low'], max=permittedValues['temperature', 'high'], step=permittedValues['temperature','step']),
                                                 shinyBS::bsPopover(id='temperature', title=NULL, content='Stochasticity within each syllable', placement="right", trigger="hover"),
                                                 sliderInput('extraFormants_ampl', 'Random formants, dB', value=permittedValues['extraFormants_ampl','default'], min=permittedValues['extraFormants_ampl', 'low'], max=permittedValues['extraFormants_ampl', 'high'], step=permittedValues['extraFormants_ampl','step']),
                                                 shinyBS::bsPopover(id='extraFormants_ampl', title=NULL, content='Amplitude of extra formants on top of user-specified ones', placement="right", trigger="hover"), width=6
                                               ),
                                               mainPanel(
                                                 plotOutput('plot_variation'), width=6
                                               )
                                             )
                                    ),

                                    tabPanel("Settings",
                                             sidebarLayout(
                                               sidebarPanel(
                                                 sliderInput('vocalTract_length', 'The length of vocal tract, cm', value=permittedValues['vocalTract_length', 'default'], min=permittedValues['vocalTract_length', 'low'], max=permittedValues['vocalTract_length', 'high'], step=permittedValues['vocalTract_length', 'step']),
                                                 shinyBS::bsPopover(id='vocalTract_length', title=NULL, content='Affects default formant dispersion at temperature>0', placement="right", trigger="hover"),
                                                 numericInput('samplingRate', 'Sampling rate, Hz', value=16000, min=8000, max=44000, step=4000),
                                                 shinyBS::bsPopover(id='samplingRate', title=NULL, content='The number of points per second of audio. Higher = better quality; lower = faster. Can be any integer, not necessarily a power of two.', placement="right", trigger="hover"),
                                                 numericInput('pitchSamplingRate', 'Pitch sampling rate, Hz', value=3500, min=100, max=44000, step=100),
                                                 shinyBS::bsPopover(id='pitchSamplingRate', title=NULL, content='The number of considered F0 values per second of audio. Should be >= pitch_ceiling for best quality', placement="right", trigger="hover"),
                                                 # numericInput('windowLength_points', 'Window length', value=512, min=256, max=2048, step=1),
                                                 # shinyBS::bsPopover(id='samplingRate', title=NULL, content='The length of window for synthesis', placement="right", trigger="hover"),
                                                 sliderInput('pitchFloorCeiling', 'Synthesized pitch range, Hz', value=c(permittedValues['pitch', 'low'],permittedValues['pitch', 'high']), min=25, max=8000, step=25),
                                                 shinyBS::bsPopover(id='pitchFloorCeiling', title=NULL, content='Sets the bounds of fundamental frequency for synthesis', placement="right", trigger="hover"), width=6
                                               ),
                                               mainPanel(
                                                 plotOutput('plot_settings'), width=6
                                               )
                                             )
                                    )
                        ),

                        navbarMenu ("Voiced",
                                    tabPanel("Timbre",
                                             sidebarLayout(
                                               sidebarPanel(
                                                 sliderInput('maleFemale', 'Male-female *hyper*', value=permittedValues['maleFemale','default'], min=permittedValues['maleFemale', 'low'], max=permittedValues['maleFemale', 'high'], step=permittedValues['maleFemale','step']),
                                                 shinyBS::bsPopover(id='maleFemale', title=NULL, content='Adjusts vocal tract length, pitch contour, and formants to imitate larger/smaller body size', placement="right", trigger="hover"),
                                                 sliderInput('creakyBreathy', 'Creaky-breathy *hyper*', value=permittedValues['creakyBreathy','default'], min=permittedValues['creakyBreathy', 'low'], max=permittedValues['creakyBreathy', 'high'], step=permittedValues['creakyBreathy','step']),
                                                 shinyBS::bsPopover(id='creakyBreathy', title=NULL, content='Changes a bunch of parameters to make the VOICED component either constricted (creaky) or breathy', placement="right", trigger="hover"), width=6
                                               ),
                                               mainPanel(
                                                 plotOutput('plot_timbre'), width=6
                                               )
                                             )
                                    ),

                                    tabPanel("Vowel",
                                             sidebarLayout(
                                               sidebarPanel(
                                                 sliderInput('formantStrength', 'Formant prominence *hyper*', value=permittedValues['formantStrength','default'], min=permittedValues['formantStrength', 'low'], max=permittedValues['formantStrength', 'high'], step=permittedValues['formantStrength','step']),
                                                 shinyBS::bsPopover(id='formantStrength', title=NULL, content='Multiply formant amplitudes by ... (>1 = emphasize vowel quality)', placement="right", trigger="hover"),
                                                 textInput('vowelString', label='String of vowel presets *hyper*', value = "a", width = NULL, placeholder ='uaaao'),
                                                 shinyBS::bsPopover(id='vowelString', title=NULL, content="Implemented presets: a, o, i, e, u, 0 (schwa)", placement="right", trigger="hover"),
                                                 shinyBS::bsCollapsePanel("Show & modify formants manually",
                                                                 tags$style(type="text/css", "textarea {width:100%}"), # NB: this little hack ties the width of the following textarea to the width of the panel in which it is embedded; see http://stackoverflow.com/questions/32640875/r-shiny-tie-textarea-width-to-wellpanel-width
                                                                 tags$textarea(id="exactFormants", label='Exact formants', rows=10, cols=20, value=NA, placeholder ="list()")
                                                 ), width=6
                                               ),
                                               mainPanel(
                                                 plotOutput('plot_exactFormants'), width=6
                                               )
                                             )
                                    ),

                                    tabPanel("Noise",
                                             sidebarLayout(
                                               sidebarPanel(
                                                 sliderInput('noiseAmount', 'Noise amount, %', value=permittedValues['noiseAmount','default'], min=permittedValues['noiseAmount', 'low'], max=permittedValues['noiseAmount', 'high'], step=permittedValues['noiseAmount','step']),
                                                 shinyBS::bsPopover(id='noiseAmount', title=NULL, content='Regulates the proportion of sound with noise effects added stepwise as first vocal fry, then jitter+shimmer.', placement="right", trigger="hover"),
                                                 sliderInput('noiseIntensity', 'Noise intensity, % *hyper*', value=permittedValues['noiseIntensity','default'], min=permittedValues['noiseIntensity', 'low'], max=permittedValues['noiseIntensity', 'high'], step=permittedValues['noiseIntensity','step']),
                                                 shinyBS::bsPopover(id='noiseIntensity', title=NULL, content='Regulates the intensity of vocal fry, jitter and shimmer, when these effects are added', placement="right", trigger="hover"),
                                                 sliderInput('min_epoch_length_ms', 'Minimal epoch length', value=permittedValues['min_epoch_length_ms','default'], min=permittedValues['min_epoch_length_ms', 'low'], max=permittedValues['min_epoch_length_ms', 'high'], step=permittedValues['min_epoch_length_ms','step']),
                                                 shinyBS::bsPopover(id='min_epoch_length_ms', title=NULL, content='Change noise regimes no sooner than after ... ms', placement="right", trigger="hover"),
                                                 shinyBS::bsCollapsePanel("Advanced",
                                                                 sliderInput('g0', 'Subharmonic, Hz', value=permittedValues['g0','default'], min=permittedValues['g0', 'low'], max=permittedValues['g0', 'high'], step=permittedValues['g0','step']),
                                                                 shinyBS::bsPopover(id='g0', title=NULL, content='The frequency of subharmonic - a secondary frequency lower than f0 (so-called "vocal fry")', placement="right", trigger="hover"),
                                                                 sliderInput('sideband_width_hz', 'Width of sidebands, Hz', value=permittedValues['sideband_width_hz','default'], min=permittedValues['sideband_width_hz', 'low'], max=permittedValues['sideband_width_hz', 'high'], step=permittedValues['sideband_width_hz','step']),
                                                                 shinyBS::bsPopover(id='sideband_width_hz', title=NULL, content='Regulates the width of subharmonic sidebands, ie the strength of subharmonics depending on their distance from main harmonics', placement="right", trigger="hover"),
                                                                 sliderInput('jitterDep', 'Jitter depth, semitones', value=permittedValues['jitterDep','default'], min=permittedValues['jitterDep', 'low'], max=permittedValues['jitterDep', 'high'], step=permittedValues['jitterDep','step']),
                                                                 shinyBS::bsPopover(id='jitterDep', title=NULL, content='Random variation in pitch between individual glottal cycles. Think "noise", a hoarse voice', placement="right", trigger="hover"),
                                                                 sliderInput('jitterLength_ms', 'Jitter period, ms', value=permittedValues['jitterLength_ms','default'], min=permittedValues['jitterLength_ms', 'low'], max=permittedValues['jitterLength_ms', 'high'], step=permittedValues['jitterLength_ms','step']),
                                                                 shinyBS::bsPopover(id='jitterLength_ms', title=NULL, content='The pitch jumps every ... ms. Low ~ harsh noise, high ~ vibrato', placement="right", trigger="hover"),
                                                                 sliderInput('shimmerDep', 'Shimmer depth, %', value=permittedValues['shimmerDep','default'], min=permittedValues['shimmerDep', 'low'], max=permittedValues['shimmerDep', 'high'], step=permittedValues['shimmerDep','step']),
                                                                 shinyBS::bsPopover(id='shimmerDep', title=NULL, content='Random variation in amplitude between individual glottal cycles. Think "noise" again, but less convincing', placement="right", trigger="hover")
                                                 ), width=6
                                               ),
                                               mainPanel(
                                                 plotOutput('plot_noise'), width=6
                                               )
                                             )
                                    ),

                                    tabPanel("Source spectrum",
                                             sidebarLayout(
                                               sidebarPanel(
                                                 sliderInput('rolloff_exp', 'Source rolloff, dB/octave', value=permittedValues['rolloff_exp','default'], min=permittedValues['rolloff_exp', 'low'], max=permittedValues['rolloff_exp', 'high'], step=permittedValues['rolloff_exp','step']),
                                                 shinyBS::bsPopover(id='rolloff_exp', title=NULL, content='Loss of energy in harmonics relative to fundamental frequency (F0); low values emphasize F0', placement="right", trigger="hover"),
                                                 shinyBS::bsCollapsePanel("Advanced",
                                                                 sliderInput('rolloff_exp_delta', 'Change of rolloff with frequency, dB/octave', value=permittedValues['rolloff_exp_delta','default'], min=permittedValues['rolloff_exp_delta', 'low'], max=permittedValues['rolloff_exp_delta', 'high'], step=permittedValues['rolloff_exp_delta','step']),
                                                                 shinyBS::bsPopover(id='rolloff_exp_delta', title=NULL, content='Negative: rolloff is progressively steeper for higher frequencies', placement="right", trigger="hover"),
                                                                 sliderInput('adjust_rolloff_per_kHz', 'Adjust rolloff per f0,  dB/kHz', value=permittedValues['adjust_rolloff_per_kHz','default'], min=permittedValues['adjust_rolloff_per_kHz', 'low'], max=permittedValues['adjust_rolloff_per_kHz', 'high'], step=permittedValues['adjust_rolloff_per_kHz','step']),
                                                                 shinyBS::bsPopover(id='adjust_rolloff_per_kHz', title=NULL, content='Steeper/gentler basic rolloff as f0 varies', placement="right", trigger="hover"),
                                                                 sliderInput('quadratic_delta', 'Parabolic rolloff adjustment, dB/octave', value=permittedValues['quadratic_delta','default'], min=permittedValues['quadratic_delta', 'low'], max=permittedValues['quadratic_delta', 'high'], step=permittedValues['quadratic_delta','step']),
                                                                 shinyBS::bsPopover(id='quadratic_delta', title=NULL, content='Parabolic boost to the first ... harmonics, dB', placement="right", trigger="hover"),
                                                                 sliderInput('quadratic_nHarm', 'Harmonics boosted', value=permittedValues['quadratic_nHarm','default'], min=permittedValues['quadratic_nHarm', 'low'], max=permittedValues['quadratic_nHarm', 'high'], step=permittedValues['quadratic_nHarm','step']),
                                                                 shinyBS::bsPopover(id='quadratic_nHarm', title=NULL, content='Apply a parabolic boost to ... harmonics. See manual for demo', placement="right", trigger="hover"),
                                                                 sliderInput('rolloff_lip', 'Lip radiation, dB/oct', value=permittedValues['rolloff_lip','default'], min=permittedValues['rolloff_lip', 'low'], max=permittedValues['rolloff_lip', 'high'], step=permittedValues['rolloff_lip','step']),
                                                                 shinyBS::bsPopover(id='rolloff_lip', title=NULL, content='Rolloff due to lip radiation', placement="right", trigger="hover")
                                                 ), width=6
                                               ),
                                               mainPanel(
                                                 plotOutput('plot_spectrum'), width=6
                                               )
                                             )
                                    ),

                                    tabPanel("Mouth opening",
                                             sidebarLayout(
                                               sidebarPanel(
                                                 actionButton(inputId = "mouth_flatten", label = "Flatten mouth opening contour"),
                                                 shinyBS::bsPopover(id='mouth_flatten', title=NULL, content='Revert to a flat mouth opening contour with opening degree equal at the first (left) anchor', placement="right", trigger="hover"),
                                                 tableOutput("mouth_anchors"), width=6
                                               ),
                                               mainPanel(
                                                 plotOutput('plot_mouth', click = "plot_mouth_click", dblclick = dblclickOpts(id = "plot_mouth_dblclick")), width=6
                                               )
                                             )
                                    ),

                                    tabPanel("Vibrato",
                                             sidebarLayout(
                                               sidebarPanel(
                                                 sliderInput('vibratoFreq', 'Vibrato rate, Hz', value=permittedValues['vibratoFreq','default'], min=permittedValues['vibratoFreq', 'low'], max=permittedValues['vibratoFreq', 'high'], step=permittedValues['vibratoFreq','step']),
                                                 shinyBS::bsPopover(id='vibratoFreq', title=NULL, content='How fast and how much the voice vibrates (think opera singers)', placement="right", trigger="hover"),
                                                 sliderInput('vibratoDep', 'Vibrato depth, semitones', value=permittedValues['vibratoDep','default'], min=permittedValues['vibratoDep', 'low'], max=permittedValues['vibratoDep', 'high'], step=permittedValues['vibratoDep','step']),
                                                 shinyBS::bsPopover(id='vibratoDep', title=NULL, content='How fast and how much the voice vibrates (think opera singers)', placement="right", trigger="hover"), width=6
                                               ),
                                               mainPanel(
                                                 plotOutput('plot_pitchModulation'), width=6
                                               )
                                             )
                                    )
                        ),

                        navbarMenu ("Unvoiced",
                                    tabPanel("Noise timing",
                                             sidebarLayout(
                                               sidebarPanel(
                                                 actionButton(inputId = "breathing_flatten", label = "Flatten contour"),
                                                 shinyBS::bsPopover(id='breathing_flatten', title=NULL, content='Revert to a flat contour with amplitude equal to the first (left) anchor', placement="right", trigger="hover"),
                                                 sliderInput('breathingTime', 'Breathing start / end, ms', value=c(0,300), min=permittedValues['breathing_dur', 'low'], max=permittedValues['breathing_dur', 'high'], step=permittedValues['breathing_dur','step']),
                                                 shinyBS::bsPopover(id='breathingTime', title=NULL, content='Timing of respiration noise relative to the voiced component', placement="right", trigger="hover"),
                                                 tableOutput("breathing_anchors"), width=6
                                               ),
                                               mainPanel(
                                                 plotOutput('plot_unvoiced', click = "plot_unvoiced_click", dblclick = dblclickOpts(id = "plot_unvoiced_dblclick")), width=6
                                               )
                                             )
                                    ),

                                    tabPanel("Noise type",
                                             sidebarLayout(
                                               sidebarPanel(
                                                 selectInput(inputId='noiseType', label="Presets", choices=c('Breathing'='b', 'Snuffling'='n', 'h'='h', 'sh'='x','f'='f', 's'='s'), selected='b'),
                                                 shinyBS::bsPopover(id='noiseType', title=NULL, content="Breathing = glottal noise (same formants as for voiced part); snuffling = breathing through the nose; h / s / sh / f = sibilants", placement="right", trigger="hover"),
                                                 sliderInput('rolloff_breathing', 'Noise rolloff, dB/oct', value=permittedValues['rolloff_breathing','default'], min=permittedValues['rolloff_breathing', 'low'], max=permittedValues['rolloff_breathing', 'high'], step=permittedValues['rolloff_breathing','step']),
                                                 shinyBS::bsPopover(id='rolloff_breathing', title=NULL, content='Rolloff of the noise component (affects both breathing and supra-glottal noise)', placement="right", trigger="hover"),
                                                 shinyBS::bsCollapsePanel("Show & modify formants manually",
                                                                 tags$style(type="text/css", "textarea {width:100%}"), # NB: this little hack ties the width of the following textarea to the width of the panel in which it is embedded; see http://stackoverflow.com/questions/32640875/r-shiny-tie-textarea-width-to-wellpanel-width
                                                                 tags$textarea(id="exactFormants_unvoiced", label='Exact formants for unvoiced part', rows=10, cols=20, value="", placeholder ="list()")
                                                 ), width=6
                                               ),
                                               mainPanel(
                                                 plotOutput('plot_consonant'), width=6
                                               )
                                             )
                                    )

                        )
            )
    ),
    column (4,
            fluidRow(
              column(3,
                     actionButton(inputId = "generateAudio", label = "Generate", style="color: blue; background-color: orange;")
              ),
              column(5,
                     uiOutput("myAudio")
              )
            ),
            fluidRow(
              plotOutput('spectrogram')
            ),
            fluidRow(
              shinyBS::bsCollapse(id="spec_controls",
                         shinyBS::bsCollapsePanel("Show spectrogram controls",
                                         sliderInput('spec_ylim', 'Frequency range, kHz', value=c(0,5), min=0, max=22, step=1),
                                         sliderInput('spec_windowLength', 'Window length, ms', value=permittedValues['spec_windowLength','default'], min=permittedValues['spec_windowLength', 'low'], max=permittedValues['spec_windowLength', 'high'], step=permittedValues['spec_windowLength','step']),
                                         shinyBS::bsPopover(id='spec_windowLength', title=NULL, content='Window length for FFT transform (Gaussian)', placement="below", trigger="hover"),
                                         radioButtons(inputId='spec_colorTheme', label="Color scheme", choices=c("Seewave"="seewave", "Heat"="heat.colors", "Black & white"="bw"), selected='bw', inline=TRUE, width=NULL),
                                         radioButtons(inputId='spec_method', label="Method", choices=c("Spectrum"="spectrum", "Spectral derivative"="spectralDerivative"), selected='spectrum', inline=TRUE, width=NULL),
                                         sliderInput('spec_contrast', 'Contrast', value=permittedValues['spec_contrast','default'], min=permittedValues['spec_contrast', 'low'], max=permittedValues['spec_contrast', 'high'], step=permittedValues['spec_contrast','step']),
                                         shinyBS::bsPopover(id='spec_contrast', title=NULL, content='Regulates the contrast of the spectrogram', placement="below", trigger="hover"),
                                         sliderInput('spec_brightness', 'Brightness', value=permittedValues['spec_brightness','default'], min=permittedValues['spec_brightness', 'low'], max=permittedValues['spec_brightness', 'high'], step=permittedValues['spec_brightness','step']),
                                         shinyBS::bsPopover(id='spec_brightness', title=NULL, content='Regulates the brightness of the spectrogram', placement="below", trigger="hover")
                                         # sliderInput('spec_median_smoothing', 'Median smoothing, bins', value=3, min=1, max=9, step=2),
                                         # sliderInput('spec_zpad', 'Zero padding, final windowLength_points', value=0, min=0, max=6144, step=512)
                         )
              )
            )
    ),
    column (2,
            tags$h2('Presets'),
            selectInput(inputId='speaker', label="Speaker", choices=names(presets), selected=names(presets)[1]),
            selectInput(inputId='callType', label="Call", choices=names(presets[[1]]), selected=names(presets[[1]])[1]),
            tags$h2('Export'),
            downloadButton (outputId = "saveAudio", label = "Save audio"),
            tags$br(), tags$br(),
            shinyBS::bsCollapsePanel("Show R code",
                            tags$style(type="text/css", "textarea {width:100%; font-size:50%}"), # NB: this little hack ties the width of the following textarea to the width of the panel in which it is embedded; see http://stackoverflow.com/questions/32640875/r-shiny-tie-textarea-width-to-wellpanel-width
                            tags$textarea(id="mycall", label='Copy-paste function call', rows=10, cols=20, value="", placeholder ="generateBout()")
            )
    )
  ),

  fluidRow (
    column(12,
           HTML('SoundGen 4.0 beta, April 2017. Visit <a href="http://cogsci.se/soundgen.html">project web page</a>. Contact me at andrey.anikin / at / lucs.lu.se. Thank you!')
    )
  )
)
