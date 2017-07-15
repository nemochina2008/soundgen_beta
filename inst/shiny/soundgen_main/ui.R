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
                                                 shinyBS:::bsPopover(id='pitch_flatten', title=NULL, content='Revert to a flat intonation contour with pitch equal to the first (left) anchor', placement="right", trigger="hover"),
                                                 tableOutput("pitch_anchors"),
                                                 width=6),
                                               mainPanel(
                                                 plotOutput('plot_intonation', click = "plot_intonation_click", dblclick = dblclickOpts(id = "plot_intonation_dblclick")),
                                                 sliderInput('pitchRange', 'Plotted pitch range, Hz', value=c(75,150), min=permittedValues['pitch', 'low'], max=permittedValues['pitch', 'high'], step=permittedValues['pitch', 'step']),
                                                 shinyBS:::bsPopover(id='pitchRange', title=NULL, content='Set upper / lower limit separately or drag in between the markers to shift both limits simultaneously', placement="right", trigger="hover"),
                                                 width=6)
                                             )
                                    ),

                                    tabPanel("Intonation global",
                                             sidebarLayout(
                                               sidebarPanel(
                                                 actionButton(inputId = "pitch_flatten_global", label = "Flatten pitch contour"),
                                                 shinyBS:::bsPopover(id='pitch_flatten_global', title=NULL, content='No global pitch modulation from syllable to syllable', placement="right", trigger="hover"),
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
                                                 sliderInput('sylLen', 'Syllable length, ms', value=permittedValues['sylLen','default'], min=permittedValues['sylLen', 'low'], max=permittedValues['sylLen', 'high'], step=permittedValues['sylLen','step']),
                                                 shinyBS:::bsPopover(id='sylLen', title=NULL, content='Average duration of a continuous VOICED syllable (breathing is added separately and may fill in the pauses)', placement="right", trigger="hover"),
                                                 sliderInput('nSyl', 'Number of syllables', value=permittedValues['nSyl','default'], min=permittedValues['nSyl', 'low'], max=permittedValues['nSyl', 'high'], step=permittedValues['nSyl','step']),
                                                 shinyBS:::bsPopover(id='nSyl', title=NULL, content='Each sound consists of one or several syllables separated by pauses', placement="right", trigger="hover"),
                                                 sliderInput('pauseLen', 'Pause, ms', value=permittedValues['pauseLen','default'], min=permittedValues['pauseLen', 'low'], max=permittedValues['pauseLen', 'high'], step=permittedValues['pauseLen','step']),
                                                 shinyBS:::bsPopover(id='pauseLen', title=NULL, content='Average pause between syllables', placement="right", trigger="hover"),
                                                 sliderInput('repeatBout', 'Repeat bout # times', value=permittedValues['repeatBout','default'], min=permittedValues['repeatBout', 'low'], max=permittedValues['repeatBout', 'high'], step=permittedValues['repeatBout','step']),
                                                 shinyBS:::bsPopover(id='repeatBout', title=NULL, content='Play the whole bout several times with a specified pause', placement="right", trigger="hover"),width=6
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
                                                 shinyBS:::bsPopover(id='attackLen', title=NULL, content='Does the voice start/end abruptly or with a "fade-in/out"?', placement="right", trigger="hover"),
                                                 sliderInput('trillDep', 'Trill depth', value=permittedValues['trillDep','default'], min=permittedValues['trillDep', 'low'], max=permittedValues['trillDep', 'high'], step=permittedValues['trillDep','step']),
                                                 shinyBS:::bsPopover(id='trillDep', title=NULL, content='Amplitude fo uvular trill as in [r]', placement="right", trigger="hover"),
                                                 sliderInput('trillFreq', 'Trill frequency, Hz', value=permittedValues['trillFreq','default'], min=permittedValues['trillFreq', 'low'], max=permittedValues['trillFreq', 'high'], step=permittedValues['trillFreq','step']),
                                                 shinyBS:::bsPopover(id='trillFreq', title=NULL, content='Frequency of rapid sinusoidal amplitude modulation (trill)', placement="right", trigger="hover"),
                                                 actionButton(inputId = "ampl_syl_flatten", label = "Flatten amplitude envelope"),
                                                 shinyBS:::bsPopover(id='ampl_syl_flatten', title=NULL, content='Same amplitude over the entire syllable', placement="right", trigger="hover"),
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
                                                 shinyBS:::bsPopover(id='ampl_global_flatten', title=NULL, content='Same amplitude over the entire bout', placement="right", trigger="hover"),
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
                                                 shinyBS:::bsPopover(id='temperature', title=NULL, content='Stochasticity within each syllable', placement="right", trigger="hover"),
                                                 sliderInput('extraFormants_stochastic', 'Random formants, dB', value=permittedValues['extraFormants_stochastic','default'], min=permittedValues['extraFormants_stochastic', 'low'], max=permittedValues['extraFormants_stochastic', 'high'], step=permittedValues['extraFormants_stochastic','step']),
                                                 shinyBS:::bsPopover(id='extraFormants_stochastic', title=NULL, content='Amplitude of extra formants on top of user-specified ones', placement="right", trigger="hover"), width=6
                                               ),
                                               mainPanel(
                                                 plotOutput('plot_variation'), width=6
                                               )
                                             )
                                    ),

                                    tabPanel("Settings",
                                             sidebarLayout(
                                               sidebarPanel(
                                                 sliderInput('vocalTract', 'The length of vocal tract, cm', value=permittedValues['vocalTract', 'default'], min=permittedValues['vocalTract', 'low'], max=permittedValues['vocalTract', 'high'], step=permittedValues['vocalTract', 'step']),
                                                 shinyBS:::bsPopover(id='vocalTract', title=NULL, content='Affects default formant dispersion at temperature>0', placement="right", trigger="hover"),
                                                 numericInput('samplingRate', 'Sampling rate, Hz', value=16000, min=8000, max=44000, step=4000),
                                                 shinyBS:::bsPopover(id='samplingRate', title=NULL, content='The number of points per second of audio. Higher = better quality; lower = faster. Can be any integer, not necessarily a power of two.', placement="right", trigger="hover"),
                                                 numericInput('pitch_samplingRate', 'Pitch sampling rate, Hz', value=3500, min=100, max=44000, step=100),
                                                 shinyBS:::bsPopover(id='pitch_samplingRate', title=NULL, content='The number of considered F0 values per second of audio. Should be >= pitch_ceiling for best quality', placement="right", trigger="hover"),
                                                 numericInput('throwaway_dB', 'Dynamic range, dB', value=-120, min=-200, max=-40, step=10),
                                                 shinyBS:::bsPopover(id='throwaway_dB', title=NULL, content='Discard everything below this amplitude', placement="right", trigger="hover"),
                                                 sliderInput('pitchFloorCeiling', 'Synthesized pitch range, Hz', value=c(permittedValues['pitch', 'low'],permittedValues['pitch', 'high']), min=25, max=8000, step=25),
                                                 shinyBS:::bsPopover(id='pitchFloorCeiling', title=NULL, content='Sets the bounds of fundamental frequency for synthesis', placement="right", trigger="hover"), width=6
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
                                                 shinyBS:::bsPopover(id='maleFemale', title=NULL, content='Adjusts vocal tract length, pitch contour, and formants to imitate larger/smaller body size', placement="right", trigger="hover"),
                                                 sliderInput('creakyBreathy', 'Creaky-breathy *hyper*', value=permittedValues['creakyBreathy','default'], min=permittedValues['creakyBreathy', 'low'], max=permittedValues['creakyBreathy', 'high'], step=permittedValues['creakyBreathy','step']),
                                                 shinyBS:::bsPopover(id='creakyBreathy', title=NULL, content='Changes a bunch of parameters to make the VOICED component either constricted (creaky) or breathy', placement="right", trigger="hover"), width=6
                                               ),
                                               mainPanel(
                                                 plotOutput('plot_timbre'), width=6
                                               )
                                             )
                                    ),

                                    tabPanel("Vowel",
                                             sidebarLayout(
                                               sidebarPanel(
                                                 sliderInput('rolloff_lipRad', 'Lip radiation, dB/oct', value=permittedValues['rolloff_lipRad','default'], min=permittedValues['rolloff_lipRad', 'low'], max=permittedValues['rolloff_lipRad', 'high'], step=permittedValues['rolloff_lipRad','step']),
                                                 shinyBS:::bsPopover(id='rolloff_lipRad', title=NULL, content='Rolloff due to lip radiation', placement="right", trigger="hover"),
                                                 sliderInput('formantDep', 'Formant prominence *hyper*', value=permittedValues['formantDep','default'], min=permittedValues['formantDep', 'low'], max=permittedValues['formantDep', 'high'], step=permittedValues['formantDep','step']),
                                                 shinyBS:::bsPopover(id='formantDep', title=NULL, content='Multiply formant amplitudes by ... (>1 = emphasize vowel quality)', placement="right", trigger="hover"),
                                                 textInput('vowelString', label='String of vowel presets *hyper*', value = "a", width = NULL, placeholder ='uaaao'),
                                                 shinyBS:::bsPopover(id='vowelString', title=NULL, content="Implemented presets: a, o, i, e, u, 0 (schwa)", placement="right", trigger="hover"),
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

                                    tabPanel("Pitch effects",
                                             sidebarLayout(
                                               sidebarPanel(
                                                 sliderInput('pitchEffects_amount', 'Amount of pitch effects, %', value=permittedValues['pitchEffects_amount','default'], min=permittedValues['pitchEffects_amount', 'low'], max=permittedValues['pitchEffects_amount', 'high'], step=permittedValues['pitchEffects_amount','step']),
                                                 shinyBS:::bsPopover(id='pitchEffects_amount', title=NULL, content='Regulates the proportion of sound with pitch effects added stepwise as first vocal fry, then vocal fry + jitter & shimmer.', placement="right", trigger="hover"),
                                                 sliderInput('pitchEffects_intensity', 'Intensity of pitch effects, % *hyper*', value=permittedValues['pitchEffects_intensity','default'], min=permittedValues['pitchEffects_intensity', 'low'], max=permittedValues['pitchEffects_intensity', 'high'], step=permittedValues['pitchEffects_intensity','step']),
                                                 shinyBS:::bsPopover(id='pitchEffects_intensity', title=NULL, content='Regulates the intensity of vocal fry, jitter and shimmer, when these effects are added', placement="right", trigger="hover"),
                                                 sliderInput('shortestEpoch', 'Shortest epoch length', value=permittedValues['shortestEpoch','default'], min=permittedValues['shortestEpoch', 'low'], max=permittedValues['shortestEpoch', 'high'], step=permittedValues['shortestEpoch','step']),
                                                 shinyBS:::bsPopover(id='shortestEpoch', title=NULL, content='Change pitch effects regime no sooner than after ... ms', placement="right", trigger="hover"),
                                                 shinyBS::bsCollapsePanel("Advanced",
                                                                 sliderInput('subFreq', 'Subharmonic frequency, Hz', value=permittedValues['subFreq','default'], min=permittedValues['subFreq', 'low'], max=permittedValues['subFreq', 'high'], step=permittedValues['subFreq','step']),
                                                                 shinyBS:::bsPopover(id='subFreq', title=NULL, content='The frequency of subharmonic - a secondary frequency lower than f0 (so-called "vocal fry")', placement="right", trigger="hover"),
                                                                 sliderInput('subDep', 'Width of sidebands, Hz', value=permittedValues['subDep','default'], min=permittedValues['subDep', 'low'], max=permittedValues['subDep', 'high'], step=permittedValues['subDep','step']),
                                                                 shinyBS:::bsPopover(id='subDep', title=NULL, content='Regulates the width of subharmonic sidebands, ie the strength of subharmonics depending on their distance from main harmonics', placement="right", trigger="hover"),
                                                                 sliderInput('jitterDep', 'Jitter depth, semitones', value=permittedValues['jitterDep','default'], min=permittedValues['jitterDep', 'low'], max=permittedValues['jitterDep', 'high'], step=permittedValues['jitterDep','step']),
                                                                 shinyBS:::bsPopover(id='jitterDep', title=NULL, content='Random variation in pitch between individual glottal cycles. Think "noise", a hoarse voice', placement="right", trigger="hover"),
                                                                 sliderInput('jitterLen', 'Jitter period, ms', value=permittedValues['jitterLen','default'], min=permittedValues['jitterLen', 'low'], max=permittedValues['jitterLen', 'high'], step=permittedValues['jitterLen','step']),
                                                                 shinyBS:::bsPopover(id='jitterLen', title=NULL, content='The pitch jumps every ... ms. Low ~ harsh noise, high ~ vibrato', placement="right", trigger="hover"),
                                                                 sliderInput('shimmerDep', 'Shimmer depth, %', value=permittedValues['shimmerDep','default'], min=permittedValues['shimmerDep', 'low'], max=permittedValues['shimmerDep', 'high'], step=permittedValues['shimmerDep','step']),
                                                                 shinyBS:::bsPopover(id='shimmerDep', title=NULL, content='Random variation in amplitude between individual glottal cycles. Think "noise" again, but less convincing', placement="right", trigger="hover")
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
                                                 sliderInput('rolloff', 'Source rolloff, dB/octave', value=permittedValues['rolloff','default'], min=permittedValues['rolloff', 'low'], max=permittedValues['rolloff', 'high'], step=permittedValues['rolloff','step']),
                                                 shinyBS:::bsPopover(id='rolloff', title=NULL, content='Loss of energy in harmonics relative to fundamental frequency (F0); low values emphasize F0', placement="right", trigger="hover"),
                                                 shinyBS::bsCollapsePanel("Advanced",
                                                                 sliderInput('rolloffAdjust_per_octave', 'Change of rolloff with frequency, dB/octave', value=permittedValues['rolloffAdjust_per_octave','default'], min=permittedValues['rolloffAdjust_per_octave', 'low'], max=permittedValues['rolloffAdjust_per_octave', 'high'], step=permittedValues['rolloffAdjust_per_octave','step']),
                                                                 shinyBS:::bsPopover(id='rolloffAdjust_per_octave', title=NULL, content='Negative: rolloff is progressively steeper for higher frequencies', placement="right", trigger="hover"),
                                                                 sliderInput('rolloffAdjust_per_kHz', 'Adjust rolloff per f0,  dB/kHz', value=permittedValues['rolloffAdjust_per_kHz','default'], min=permittedValues['rolloffAdjust_per_kHz', 'low'], max=permittedValues['rolloffAdjust_per_kHz', 'high'], step=permittedValues['rolloffAdjust_per_kHz','step']),
                                                                 shinyBS:::bsPopover(id='rolloffAdjust_per_kHz', title=NULL, content='Steeper/gentler basic rolloff as f0 varies', placement="right", trigger="hover"),
                                                                 sliderInput('rolloffAdjust_quadratic', 'Parabolic rolloff adjustment, dB/octave', value=permittedValues['rolloffAdjust_quadratic','default'], min=permittedValues['rolloffAdjust_quadratic', 'low'], max=permittedValues['rolloffAdjust_quadratic', 'high'], step=permittedValues['rolloffAdjust_quadratic','step']),
                                                                 shinyBS:::bsPopover(id='rolloffAdjust_quadratic', title=NULL, content='Parabolic boost to the first ... harmonics, dB', placement="right", trigger="hover"),
                                                                 sliderInput('rolloffAdjust_quadratic_nHarm', 'Harmonics boosted', value=permittedValues['rolloffAdjust_quadratic_nHarm','default'], min=permittedValues['rolloffAdjust_quadratic_nHarm', 'low'], max=permittedValues['rolloffAdjust_quadratic_nHarm', 'high'], step=permittedValues['rolloffAdjust_quadratic_nHarm','step']),
                                                                 shinyBS:::bsPopover(id='rolloffAdjust_quadratic_nHarm', title=NULL, content='Apply a parabolic boost to ... harmonics. See manual for demo', placement="right", trigger="hover")
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
                                                 shinyBS:::bsPopover(id='mouth_flatten', title=NULL, content='Revert to a flat mouth opening contour with opening degree equal at the first (left) anchor', placement="right", trigger="hover"),
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
                                                 shinyBS:::bsPopover(id='vibratoFreq', title=NULL, content='How fast and how much the voice vibrates (think opera singers)', placement="right", trigger="hover"),
                                                 sliderInput('vibratoDep', 'Vibrato depth, semitones', value=permittedValues['vibratoDep','default'], min=permittedValues['vibratoDep', 'low'], max=permittedValues['vibratoDep', 'high'], step=permittedValues['vibratoDep','step']),
                                                 shinyBS:::bsPopover(id='vibratoDep', title=NULL, content='How fast and how much the voice vibrates (think opera singers)', placement="right", trigger="hover"), width=6
                                               ),
                                               mainPanel(
                                                 plotOutput('plot_pitchModulation'), width=6
                                               )
                                             )
                                    )
                        ),

                        navbarMenu ("Noise",
                                    tabPanel("Timing",
                                             sidebarLayout(
                                               sidebarPanel(
                                                 actionButton(inputId = "noise_flatten", label = "Flatten contour"),
                                                 shinyBS:::bsPopover(id='noise_flatten', title=NULL, content='Revert to a flat contour with amplitude equal to the first (left) anchor', placement="right", trigger="hover"),
                                                 sliderInput('noiseTime', 'Breathing start / end, ms', value=c(0,300), min=permittedValues['sylLen', 'low'], max=permittedValues['sylLen', 'high'], step=permittedValues['sylLen','step']),
                                                 shinyBS:::bsPopover(id='noiseTime', title=NULL, content='Timing of respiration noise relative to the voiced component', placement="right", trigger="hover"),
                                                 tableOutput("noise_anchors"), width=6
                                               ),
                                               mainPanel(
                                                 plotOutput('plot_unvoiced', click = "plot_unvoiced_click", dblclick = dblclickOpts(id = "plot_unvoiced_dblclick")), width=6
                                               )
                                             )
                                    ),

                                    tabPanel("Type",
                                             sidebarLayout(
                                               sidebarPanel(
                                                 selectInput(inputId='noiseType', label="Presets", choices=c('Breathing'='b', 'Snuffling'='n', 'h'='h', 'sh'='x','f'='f', 's'='s'), selected='b'),
                                                 shinyBS:::bsPopover(id='noiseType', title=NULL, content="Breathing = glottal noise (same formants as for voiced part); snuffling = breathing through the nose; h / s / sh / f = sibilants", placement="right", trigger="hover"),
                                                 sliderInput('rolloff_noise', 'Noise rolloff, dB/oct', value=permittedValues['rolloff_noise','default'], min=permittedValues['rolloff_noise', 'low'], max=permittedValues['rolloff_noise', 'high'], step=permittedValues['rolloff_noise','step']),
                                                 shinyBS:::bsPopover(id='rolloff_noise', title=NULL, content='Rolloff of the noise component (affects both breathing and supra-glottal noise)', placement="right", trigger="hover"),
                                                 shinyBS::bsCollapsePanel("Show & modify formants manually",
                                                                 tags$style(type="text/css", "textarea {width:100%}"), # NB: this little hack ties the width of the following textarea to the width of the panel in which it is embedded; see http://stackoverflow.com/questions/32640875/r-shiny-tie-textarea-width-to-wellpanel-width
                                                                 tags$textarea(id="exactFormants_noise", label='Exact formants for unvoiced part', rows=10, cols=20, value="", placeholder ="list()")
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
                                         shinyBS:::bsPopover(id='spec_windowLength', title=NULL, content='Window length for FFT transform (Gaussian)', placement="below", trigger="hover"),
                                         radioButtons(inputId='spec_colorTheme', label="Color scheme", choices=c("Seewave"="seewave", "Heat"="heat.colors", "Black & white"="bw"), selected='bw', inline=TRUE, width=NULL),
                                         radioButtons(inputId='spec_method', label="Method", choices=c("Spectrum"="spectrum", "Spectral derivative"="spectralDerivative"), selected='spectrum', inline=TRUE, width=NULL),
                                         sliderInput('spec_contrast', 'Contrast', value=permittedValues['spec_contrast','default'], min=permittedValues['spec_contrast', 'low'], max=permittedValues['spec_contrast', 'high'], step=permittedValues['spec_contrast','step']),
                                         shinyBS:::bsPopover(id='spec_contrast', title=NULL, content='Regulates the contrast of the spectrogram', placement="below", trigger="hover"),
                                         sliderInput('spec_brightness', 'Brightness', value=permittedValues['spec_brightness','default'], min=permittedValues['spec_brightness', 'low'], max=permittedValues['spec_brightness', 'high'], step=permittedValues['spec_brightness','step']),
                                         shinyBS:::bsPopover(id='spec_brightness', title=NULL, content='Regulates the brightness of the spectrogram', placement="below", trigger="hover")
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
            shinyBS::bsCollapsePanel("Load new preset",
                                     tags$style(type="text/css", "textarea {width:100%; font-size:50%}"),
                                     tags$textarea(id="user_preset", label='Type in a new preset here', rows=10, cols=20, value="", placeholder ="soundgen(...)"),
                                     actionButton(inputId = "import_preset", label = "Update sliders")
            ),

            tags$h2('Export'),
            downloadButton (outputId = "saveAudio", label = "Save audio"),
            tags$br(), tags$br(),
            shinyBS::bsCollapsePanel("Export R code",
                            tags$style(type="text/css", "textarea {width:100%; font-size:50%}"), # NB: this little hack ties the width of the following textarea to the width of the panel in which it is embedded; see http://stackoverflow.com/questions/32640875/r-shiny-tie-textarea-width-to-wellpanel-width
                            tags$textarea(id="mycall", label='Copy-paste function call', rows=10, cols=20, value="", placeholder ="soundgen()")
            )
    )
  ),

  fluidRow (
    column(12,
           HTML('SoundGen 4.0 beta, April 2017. Visit <a href="http://cogsci.se/soundgen.html">project web page</a>. Contact me at andrey.anikin / at / lucs.lu.se. Thank you!')
    )
  )
)
