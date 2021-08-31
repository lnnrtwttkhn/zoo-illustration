all: stimuli-badges transition-matrix study-procedure graph-structure badges-overview stimuli-screens

stimuli-badges: code/zoo-stimuli-badges.R
	Rscript $<

transition-matrix: code/zoo-transition-matrix.R
	Rscript $<

study-procedure: code/zoo-study-procedure.R
	Rscript $<
	
graph-structure: code/zoo-graph-structure.R
	Rscript $<

badges-overview: code/zoo-stimuli-badges-overview.R
	Rscript $<
	
stimuli-screens: code/zoo-stimuli-screens.R
	Rscript $<