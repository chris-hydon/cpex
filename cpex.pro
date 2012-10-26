HS_SRC = $${PWD}/haskell
HS_DEST = haskell

HS_DEPS += "builtin_rts rts_thr-" \
           array \
           base \
           containers \
           deepseq \
           ghc-prim \
           integer-gmp \
           libcspm \
           pretty \
           transformers

HS_SOURCES += haskell/Cpex/Transitions.hs \
              haskell/CSPM/Foreign.hs

include(haskell.pri)

QT += core gui

TARGET = cpex
TEMPLATE = app

SOURCES += main.cpp\
           mainwindow.cpp

HEADERS += mainwindow.h

FORMS += mainwindow.ui

LIBS += $${HS_LIBS}

unix:QMAKE_RPATHDIR += $${HS_LIB_DIRS}
