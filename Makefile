onCPU ?= true
swapdrive ?= /dev/nvme0n1p4
script ?= reports/explore.R

all: cpu-env swap train

train: cpu-env swap
	Rscript ${script}


cpu-env:
	@if (${onCPU} == "true"); then\
	  export CUDA_VISIBLE_DEVICES="-1";\
	  echo "Hided GPU to CUDA";\
	fi

swap:
	sudo swapoff -a
	sudo swapon ${swapdrive}
	@echo "Swap setup to 1.4 GB"
