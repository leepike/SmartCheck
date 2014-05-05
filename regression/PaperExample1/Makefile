SRC         = PaperExample1
FRMT        = jpeg

# We never run small.out since it's not feasible.
TARGETS			= qcNone.out  \
              qc10.out		\
              qcjh.out		\
              smart.out

# feat.out		\

GRAPHS      = data.$(FRMT) time-big.$(FRMT) time-small.$(FRMT)

all : $(TARGETS)
	gnuplot script.gnp

include ../Config.mk
