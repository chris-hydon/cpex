HS_SRC = $${PWD}/haskell
HS_DEST = haskell

HS_DEPS += array \
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
           mainwindow.cpp \
    widget/processtree.cpp \
    model/processmodel.cpp \
    model/transition.cpp \
    model/process.cpp \
    cspmsession.cpp

HEADERS += mainwindow.h \
    widget/processtree.h \
    model/processmodel.h \
    model/transition.h \
    model/process.h \
    cspmsession.h

FORMS += mainwindow.ui

LIBS += $${HS_LIBS}

unix:QMAKE_RPATHDIR += $${HS_LIB_DIRS}

OTHER_FILES += \
    haskell/Cpex/Transitions.hs \
    haskell/CSPM/Foreign.hs
