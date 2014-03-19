SRC         = PaperExample1

# We never run small.out since it's not feasible.
TARGETS			= qcNone.out  \
              qc10.out		\
              qcjh.out		\
              smart.out		\
              qcjhint.out

# feat.out		\

GRAPHS      = data.pdf time-big.pdf time-small.pdf

all : $(TARGETS)

include ../Config.mk

%.pdf : $(TARGETS)
	gnuplot script.gnp
