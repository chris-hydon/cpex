HS_SRC = $${PWD}/haskell
HS_DEST = haskell

HS_DEPS += \
    array \
    base \
    containers \
    deepseq \
    ghc-prim \
    integer-gmp \
    libcspm \
    pretty \
    transformers

HS_SOURCES += \
    haskell/Cpex/Transitions.hs \
    haskell/CSPM/Foreign.hs

include(haskell.pri)

QT += core gui

TARGET = cpex
TEMPLATE = app

SOURCES += \
    main.cpp \
    mainwindow.cpp \
    widget/processtree.cpp \
    model/processmodel.cpp \
    model/process.cpp \
    cspmsession.cpp \
    model/event.cpp

HEADERS += \
    mainwindow.h \
    widget/processtree.h \
    model/processmodel.h \
    model/process.h \
    cspmsession.h \
    model/event.h

FORMS += \
    mainwindow.ui

OTHER_FILES += \
    haskell/Cpex/Transitions.hs \
    haskell/CSPM/Foreign.hs
