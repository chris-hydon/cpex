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
    haskell/Cpex/Foreign.hs \
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
    model/event.cpp \
    widget/tracelistwidget.cpp \
    widget/sessiontree.cpp \
    model/sessionmodel.cpp \
    programstate.cpp

HEADERS += \
    mainwindow.h \
    widget/processtree.h \
    model/processmodel.h \
    model/process.h \
    cspmsession.h \
    model/event.h \
    widget/tracelistwidget.h \
    widget/sessiontree.h \
    model/sessionmodel.h \
    programstate.h

FORMS += \
    mainwindow.ui

OTHER_FILES += \
    haskell/Cpex/Transitions.hs \
    haskell/CSPM/Foreign.hs \
    haskell/Cpex/Foreign.hs
