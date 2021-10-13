all: stimuli-badges transition-matrix study-procedure graph-structure badges-overview stimuli-screens task-procedure

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

task-procedure: code/zoo-task-procedure.R
	Rscript $<
	
index.html: index.Rmd
	Rscript -e "rmarkdown::render('$<', output_dir = 'public')"
	
index.md: index.Rmd
	Rscript -e "rmarkdown::render('$<', rmarkdown::md_document(variant = 'gfm'))"