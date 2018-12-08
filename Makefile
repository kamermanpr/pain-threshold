# Create directories if required
$(shell mkdir -p data-cleaned figures outputs outputs/figures/)

# Dummy outputs
DATA_A = 	data-cleaned/SPARS_A.csv \
			data-cleaned/SPARS_A.rds

DATA_B = 	data-cleaned/SPARS_B.csv \
			data-cleaned/SPARS_B.rds

1S = 	outputs/Suppl-01-SPARSA-threshold-width.pdf

2S = 	outputs/Suppl-02-SPARSB-threshold-width.pdf

3S = 	outputs/Suppl-03-binomial-analysis.pdf

4S = 	outputs/Suppl-04-stimulus-intensity-change.pdf

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
outputs/Suppl-01-SPARSA-threshold-width.pdf: \
Suppl-01-SPARSA-threshold-width.Rmd \
data-cleaned/SPARS_A.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	rm -r figures/Suppl-01-SPARSA-threshold-width

outputs/Suppl-02-SPARSB-threshold-width.pdf: \
Suppl-02-SPARSB-threshold-width.Rmd \
data-cleaned/SPARS_B.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	rm -r figures/Suppl-02-SPARSB-threshold-width

outputs/Suppl-03-binomial-analysis.pdf: \
Suppl-03-binomial-analysis.Rmd \
data-cleaned/SPARS_A.rds \
data-cleaned/SPARS_B.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	rm -r figures/Suppl-03-binomial-analysis

outputs/Suppl-04-stimulus-intensity-change.pdf: \
Suppl-04-stimulus-intensity-change.Rmd \
data-cleaned/SPARS_A.rds \
data-cleaned/SPARS_B.rds
	Rscript -e "rmarkdown::render('$<', output_dir = 'outputs/')"
	rm -r figures/Suppl-04-stimulus-intensity-change
