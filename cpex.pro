HS_SRC = $${PWD}/haskell
HS_DEST = haskell

HS_DEPS += \
    array \
    base \
    containers \
    deepseq \
    ghc-prim \
    hashable \
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
    delegate/processitemdelegate.cpp \
    model/event.cpp \
    model/process.cpp \
    model/ptypes.cpp \
    model/sessionmodel.cpp \
    model/transitionmodel.cpp \
    view/displaystring.cpp \
    widget/processtree.cpp \
    widget/sessiontree.cpp \
    widget/tracelistwidget.cpp \
    cspmsession.cpp \
    main.cpp \
    mainwindow.cpp \
    programstate.cpp

HEADERS += \
    delegate/processitemdelegate.h \
    model/event.h \
    model/process.h \
    model/ptypes.h \
    model/sessionmodel.h \
    model/transitionmodel.h \
    view/displaystring.h \
    widget/processtree.h \
    widget/sessiontree.h \
    widget/tracelistwidget.h \
    cspmsession.h \
    mainwindow.h \
    programstate.h

FORMS += \
    mainwindow.ui

OTHER_FILES += \
    haskell/Cpex/Transitions.hs \
    haskell/Cpex/Foreign.hs \
    haskell/CSPM/Foreign.hs
