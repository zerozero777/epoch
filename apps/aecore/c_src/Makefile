.PHONY: deps deps_build clean distclean help show_config

CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)

PROJECT := aec_pow_cuckoo

BUILD_CFG := build.cfg
LIBDIR := cuckoo

ERTS_INCLUDE_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~s/erts-~s/include/\", [code:root_dir(), erlang:system_info(version)]).")

C_SRC_DIR = $(CURDIR)
PRIV_DIR = $(BASEDIR)/priv/
LIB_MEAN16_SO ?= $(CURDIR)/../priv/$(PROJECT)_mean16_nif.so
LIB_LEAN16_SO ?= $(CURDIR)/../priv/$(PROJECT)_lean16_nif.so
LIB_MEAN29_SO ?= $(CURDIR)/../priv/$(PROJECT)_mean29_nif.so
LIB_LEAN29_SO ?= $(CURDIR)/../priv/$(PROJECT)_lean29_nif.so
LIB_SO ?= $(CURDIR)/../priv/$(PROJECT)_nif.so
TESTLIB_SO ?= $(CURDIR)/../priv/siphash_test_nif.so

# System type and C compiler/flags.

UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Darwin)
	CC ?= cc
	CFLAGS ?= -fPIC -O3 -std=c99 -arch x86_64 -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O3 -arch x86_64 -Wall
	LDFLAGS ?= -arch x86_64 -flat_namespace -undefined suppress
	SED_FLAGS ?= -i ''
else ifeq ($(UNAME_SYS), FreeBSD)
	CC = clang
	CXX = clang++
	CFLAGS ?= -fPIC -O3 -std=c99 -Wall -Wmissing-prototypes
	CFLAGS += -D_GCLIBCXX_USE_C99
	CXXFLAGS ?= -O3 -Wall
	CXXFLAGS += -std=c++11 -stdlib=libc++ -D_GCLIBCXX_USE_C99
	SED_FLAGS ?= -i
else ifeq ($(UNAME_SYS), OpenBSD)
	CC = egcc
	CXX = eg++
	CFLAGS ?= -fPIC -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CFLAGS += -D_GCLIBCXX_USE_C99
	CXXFLAGS ?= -O3 -Wall
	CXXFLAGS += -std=c++11 -pthread -finline-functions -D_GCLIBCXX_USE_C99
	SED_FLAGS ?= -i
else ifeq ($(UNAME_SYS), DragonFly)
	CC = c++48
	CXX = c++48
	CFLAGS ?= -fPIC -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CFLAGS += -D_GCLIBCXX_USE_C99
	CXXFLAGS ?= -O3 -finline-functions -Wall
	CXXFLAGS += -D_GCLIBCXX_USE_C99
	SED_FLAGS ?= -i
else ifeq ($(UNAME_SYS), NetBSD)
	CC = c++48
	CXX = c++48
	CFLAGS ?= -fPIC -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CFLAGS += -D_GCLIBCXX_USE_C99
	CXXFLAGS ?= -O3 -finline-functions -Wall
	CXXFLAGS += -D_GCLIBCXX_USE_C99
	SED_FLAGS ?= -i
else ifeq ($(UNAME_SYS), SunOS)
	ifeq ($(shell uname -v | sed 's/_.*$$//'), joyent)
		CC = gcc
		CXX = g++
	else
		CC = c++48
		CXX = c++48
	endif
	CFLAGS ?= -fPIC -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CFLAGS += -D_GCLIBCXX_USE_C99
	CXXFLAGS ?= -O3 -finline-functions -Wall
	CXXFLAGS += -D_GCLIBCXX_USE_C99
	SED_FLAGS ?= -i
else ifeq ($(UNAME_SYS), Linux)
	CC ?= gcc
	CFLAGS ?= -fPIC -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O3 -finline-functions -Wall
	SED_FLAGS ?= -i
endif

ifeq ($(UNAME_SYS), OpenBSD)
	LDLIBS += -lestdc++
else
	LDLIBS += -lstdc++
endif

LDFLAGS += -shared

CXXFLAGS += -g -fPIC -I $(ERTS_INCLUDE_DIR) -I. -I./cuckoo/src -std=c++11 -Wno-format

# Verbosity.

c_verbose_0 = @echo " C     " $(?F);
c_verbose = $(c_verbose_$(V))

cpp_verbose_0 = @echo " CPP   " $(?F);
cpp_verbose = $(cpp_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

CUCKOO_SRCS = $(LIBDIR)/src/blake2b-ref.c
CUCKOO_OBJECTS = $(LIBDIR)/src/blake2b-ref.o

COMPILE_C = $(c_verbose) $(CC) $(CFLAGS) $(CPPFLAGS) -c
COMPILE_CPP = $(cpp_verbose) $(CXX) $(CXXFLAGS) $(CPPFLAGS) -c

ALGORITHM = $(shell cat "$(BUILD_CFG)" 2>/dev/null | awk -F': ' '$$1=="alg"{print $$2}')
GRAPHSIZE = $(shell cat "$(BUILD_CFG)" 2>/dev/null | awk -F': ' '$$1=="graph"{print $$2}')
EDGEBITS = $(shell expr $(GRAPHSIZE) - 1)

SPEC_OBJECTS = aec_pow_cuckoo_$(ALGORITHM)_$(GRAPHSIZE).o aec_pow_cuckoo_nif_$(GRAPHSIZE).o

help: ## This documentation
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

show_config:
	@echo "==== Using algorithm: $(ALGORITHM), graph size: $(GRAPHSIZE) edge bits: $(EDGEBITS) ===="

mean-test: ALGORITHM = mean
mean-test: GRAPHSIZE = 16
mean-test: EDGEBITS = 15
mean-test: deps show_config $(PRIV_DIR) $(LIB_MEAN16_SO) $(TESTLIB_SO)

lean-test: ALGORITHM = lean
lean-test: GRAPHSIZE = 16
lean-test: EDGEBITS = 15
lean-test: deps show_config $(PRIV_DIR) $(LIB_LEAN16_SO)

mean-prod: ALGORITHM = mean
mean-prod: GRAPHSIZE = 29
mean-prod: EDGEBITS = 28
mean-prod: deps show_config $(PRIV_DIR) $(LIB_MEAN29_SO)

lean-prod: ALGORITHM = lean
lean-prod: GRAPHSIZE = 29
lean-prod: EDGEBITS = 28
mean-prod: deps show_config $(PRIV_DIR) $(LIB_LEAN29_SO)

$(LIB_MEAN16_SO): aec_pow_cuckoo_mean16.o aec_pow_cuckoo_nif16.o $(CUCKOO_OBJECTS)
	$(link_verbose) $(CXX) -o $@ $(LDLIBS)  $(LDFLAGS) $^
	ln -sf $@ $(LIB_SO)

$(LIB_LEAN16_SO): aec_pow_cuckoo_lean16.o aec_pow_cuckoo_nif16.o $(CUCKOO_OBJECTS)
	$(link_verbose) $(CXX) -o $@ $(LDLIBS)  $(LDFLAGS) $^
	ln -sf $@ $(LIB_SO)

$(LIB_MEAN29_SO): aec_pow_cuckoo_mean29.o aec_pow_cuckoo_nif29.o $(CUCKOO_OBJECTS)
	$(link_verbose) $(CXX) -o $@ $(LDLIBS)  $(LDFLAGS) $^
	ln -sf $@ $(LIB_SO)

$(LIB_LEAN29_SO): aec_pow_cuckoo_lean29.o aec_pow_cuckoo_nif29.o $(CUCKOO_OBJECTS)
	$(link_verbose) $(CXX) -o $@ $(LDLIBS)  $(LDFLAGS) $^
	ln -sf $@ $(LIB_SO)

$(TESTLIB_SO): aec_siphash_test_nif.o $(CUCKOO_OBJECTS)
	$(link_verbose) $(CXX) -o $@ $(LDLIBS)  $(LDFLAGS) $^

%.o: %.c
	$(COMPILE_C) $(OUTPUT_OPTION) $<

%16.o: %.cpp
	$(COMPILE_CPP) -mavx2 -DXBITS=0 -DNSIPHASH=8 -DEDGEBITS=15 $(OUTPUT_OPTION) $<

%20.o: %.cpp
	$(COMPILE_CPP) -mavx2 -DXBITS=2 -DNSIPHASH=8 -DEDGEBITS=19 $(OUTPUT_OPTION) $<

%25.o: %.cpp
	$(COMPILE_CPP) -mavx2 -DXBITS=5 -DNSIPHASH=8 -DEDGEBITS=24 $(OUTPUT_OPTION) $<

%28.o: %.cpp
	$(COMPILE_CPP) -mavx2 -DXBITS=6 -DNSIPHASH=8 -DEDGEBITS=27 $(OUTPUT_OPTION) $<

%29.o: %.cpp
	$(COMPILE_CPP) -mavx2 -DNSIPHASH=8 -DEDGEBITS=28 $(OUTPUT_OPTION) $<

deps: $(LIBDIR)

$(LIBDIR):
	git clone https://github.com/tromp/cuckoo.git
	#https://github.com/aeternity/cuckoo.git
	( cd $(LIBDIR) && git checkout master; )
	#sed $(SED_FLAGS) 's/printf/aec_printf/g' $(LIBDIR)/src/cuckoo_miner.hpp
	#sed $(SED_FLAGS) 's/.*TWICE_PER_ATOM = .*/int aec_printf(const char *format, ...);&/g' $(LIBDIR)/src/cuckoo_miner.hpp

$(PRIV_DIR):
	mkdir -p $(PRIV_DIR)

$(CUCKOO_SRCS): $(LIBDIR)

$(CUCKOO_OBJECTS): $(CUCKOO_SRCS)

clean: ## Clean all files
	rm -f *.o $(LIB_SO) $(BUILD_CFG)

distclean: clean
	rm -fR $(LIBDIR)
