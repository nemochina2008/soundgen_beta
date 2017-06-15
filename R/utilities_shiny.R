## Functions that are only needed for the Shiny UI, but not for using soundgen as a command-line tool, i.e. not needed for calling generateBout()

#' Convert Hz to semitones
#'
#' Internal soundgen function.
HzToSemitones = function(h){
  return (sapply(h, function(x)log2(x/16.3516)*12)) # this is also the index of the note name in our dictionary notes_dict, so we can simply look it up :))
}

#' Convert semitones to Hz
#'
#' Internal soundgen function.
semitonesToHz = function(s){
  return (sapply(s, function(x)16.3516*2^(x/12)))
}

#' Convert list of formants to string
#'
#' Internal soundgen function.
#'
#' Helper function that converts a list of exact formants into a human-readable string, which can then be modified and evaluated to reproduce a list of the original structure. Note: dput() and dump() can pickle & reproduce any object, but I can't figure out how to make the interim string human-readable and modifiable
pickle = function(l){
  if (is.null(l) | length(l)<1) {
    return (NA)
  } else if (is.na(l)){
    return (NA)
  }

  temp = try(expr={
    out = 'list(\n'
    for (i in 1:length(l)){
      len = nrow(l[[i]])
      freq = ifelse(len==1 | var(l[[i]]$freq)==0, l[[i]]$freq[1], paste0(l[[i]]$freq,collapse=","))
      amp = ifelse(len==1 | var(l[[i]]$amp)==0, l[[i]]$amp[1], paste0(l[[i]]$amp,collapse=","))
      width = ifelse(len==1 | var(l[[i]]$width)==0, l[[i]]$width[1], paste0(l[[i]]$width,collapse=","))
      out = paste0(out, names(l)[i], '=data.frame( time=c(', paste0(round(l[[i]]$time,2),collapse=","), '), freq=c(', freq, '), amp=c(', amp, '), width=c(', width, ')),\n')
    }
    substr(out, nchar(out)-1, nchar(out)) = ')'  #  replace the last comma with )
  }, silent=T)
  if (class(temp)=='try-error') return(NA)
  return(out)  # to go back to the list structure, use eval(parse(text=out))
}

