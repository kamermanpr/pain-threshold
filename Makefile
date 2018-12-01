# Create directories if required
$(shell mkdir -p data-cleaned figures outputs outputs/figures/)

# Dummy outputs
DATA_A = 	data-cleaned/SPARS_A.csv \
			data-cleaned/SPARS_A.rds

DATA_B = 	data-cleaned/SPARS_B.csv \
			data-cleaned/SPARS_B.rds

1S = 	outputs/Suppl-01-SPARSA-threshold-width.html \
			outputs/Suppl-01-SPARSA-threshold-width.md

2S = 	outputs/Suppl-02-SPARSB-threshold-width.html \
			outputs/Suppl-02-SPARSB-threshold-width.md

3S = 	outputs/Suppl-03-binomial-analysis.html \
			outputs/Suppl-03-binomial-analysis.md

4S = 	outputs/Suppl-04-stimulus-intensity-change.html \
			outputs/Suppl-04-stimulus-intensity-change.md

.PHONY: all

all: 	$(DATA_A) $(DATA_B) $(1S) $(2S) $(3S) $(4S)

# Clean
clean:
	rm -rfv figures/ outputs/ data-cleaned/

# Generate data
data-cleaned/SPARS_A.csv data-cleaned/SPARS_A.rds: \
clean-SPARSA.R \
data-original/SPARS_A/*.xlsx
	Rscript "$<"

data-cleaned/SPARS_B.csv data-cleaned/SPARS_B.rds: \
clean-SPARSB.R \
data-original/SPARS_B/*.txt
	Rscript "$<"

# Generate html and md outputs
outputs/Suppl-01-SPARSA-threshold-width.html outputs/Suppl-01-SPARSA-threshold-width.md: \
Suppl-01-SPARSA-threshold-width.Rmd \
data-cleaned/SPARS_A.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/Suppl-01-SPARSA-threshold-width outputs/figures/

outputs/Suppl-02-SPARSB-threshold-width.html outputs/Suppl-02-SPARSB-threshold-width.md: \
Suppl-02-SPARSB-threshold-width.Rmd \
data-cleaned/SPARS_B.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/Suppl-02-SPARSB-threshold-width outputs/figures/

outputs/Suppl-03-binomial-analysis.html	outputs/Suppl-03-binomial-analysis.md: \
Suppl-03-binomial-analysis.Rmd \
data-cleaned/SPARS_A.rds \
data-cleaned/SPARS_B.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/Suppl-03-binomial-analysis outputs/figures/

outputs/Suppl-04-stimulus-intensity-change.html outputs/Suppl-04-stimulus-intensity-change.md: \
Suppl-04-stimulus-intensity-change.Rmd \
data-cleaned/SPARS_A.rds \
data-cleaned/SPARS_B.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	mv figures/Suppl-04-stimulus-intensity-change outputs/figures/
