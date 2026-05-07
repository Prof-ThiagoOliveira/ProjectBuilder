setup:
	Rscript scripts/00_start_here.R

install:
	Rscript scripts/install_packages.R

health:
	Rscript -e "source('scripts/_load_project.R'); project_health_check()"
{{ makefile_extra_targets }}
