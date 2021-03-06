ifndef GPI2_HOME
  GPI2_HOME = ../../../..
endif

ifndef CMD_RUN
  CMD_RUN = $(GPI2_HOME)/bin/gaspi_run -m $$PBS_NODEFILE -N
endif

NAME += left_right_double_buffer_funneled
NAME += left_right_double_buffer_multiple
# NAME += left_right_dataflow_halo

CFLAGS += -Wall
CFLAGS += -Wextra
CFLAGS += -Wshadow
CFLAGS += -O3
CFLAGS += -std=gnu99
ifeq "$(NON_MPI_CC)" "gcc"
CFLAGS += -fopenmp
endif
ifeq "$(NON_MPI_CC)" "icc"
CFLAGS += -qopenmp
endif
###############################################################################

INCLUDE_DIR += $(srcdir)/src/include
INCLUDE_DIR += ..
LIBRARY_DIR += $(GPI2_HOME)/src/.libs

LDFLAGS += $(addprefix -L,$(LIBRARY_DIR))

ifeq "$(NON_MPI_CC)" "icc"
  LDFLAGS += -qopenmp-link static
endif

CFLAGS += $(addprefix -I,$(INCLUDE_DIR))

LIB += GPI2
LIB += ibverbs
LIB += m
LIB += pthread

OBJ += ../data
OBJ += ../now
OBJ += queue
OBJ += slice
OBJ += waitsome
OBJ += testsome

###############################################################################

BIN = $(addsuffix .exe, $(NAME))

default: $(BIN)

%.exe: %.o $(addsuffix .o, $(OBJ))
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS) $(addprefix -l, $(LIB))

###############################################################################

%.run: %.exe
	@$(CMD_RUN) $^ | grep ^#

.PHONY: run

run: $(addprefix $(CURDIR)/, $(addsuffix .run, $(NAME)))

###############################################################################

.PHONY: clean objclean

objclean:
	rm -f *.o

clean: objclean
	rm -f $(BIN)
