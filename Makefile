HS_CSPM=$(shell sh which-hs libcspm)
HS_CSPM_DIR=$(shell dirname $(HS_CSPM))
HS_CPEX=dist/build/$(shell ls -1 dist/build | grep .so)
HS_CPEX_DIR=dist/build
HS_RTS=$(shell sh which-hs builtin_rts 'rts_thr[^_]')
HS_RTS_DIR=$(shell dirname $(HS_RTS))
HASKELL=$(HS_CSPM) $(HS_CPEX) $(HS_RTS)

CC=g++
CPPFLAGS=-g -O0
LDFLAGS=-Wl,-rpath,$(HS_CSPM_DIR):$(HS_CPEX_DIR):$(HS_RTS_DIR)
INCLUDES=-I$(HS_CPEX_DIR) -I$(HS_RTS_DIR)/include

all : native.cpp
	$(CC) $(LDFLAGS) $(CPPFLAGS) $(INCLUDES) $(HASKELL) -o test native.cpp
