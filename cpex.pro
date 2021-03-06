HS_SRC = $${PWD}/haskell
HS_DEST = haskell

HS_DEPS += \
    array \
    base \
    containers \
    deepseq \
    ghc-prim \
    hashable \
    hashtables \
    integer-gmp \
    libcspm \
    pretty \
    transformers \
    value-supply

HS_SOURCES += \
    haskell/Cpex/Foreign.hs \
    haskell/Cpex/Transitions.hs \
    haskell/CSPM/Foreign.hs

include(haskell.pri)

QT += core gui

TARGET = cpex
TEMPLATE = app

TRANSLATIONS = cpex.ts

SOURCES += \
    delegate/processitemdelegate.cpp \
    model/event.cpp \
    model/expression.cpp \
    model/inspectmodel.cpp \
    model/process.cpp \
    model/processitem.cpp \
    model/ptypes.cpp \
    model/sessionmodel.cpp \
    model/transitionmodel.cpp \
    view/displaystring.cpp \
    widget/processtree.cpp \
    widget/tab.cpp \
    widget/tracelistwidget.cpp \
    cspmsession.cpp \
    main.cpp \
    mainwindow.cpp \
    programstate.cpp \
    csperror.cpp \
    widget/clickablelabel.cpp \
    widget/errordialog.cpp

HEADERS += \
    delegate/processitemdelegate.h \
    model/event.h \
    model/expression.h \
    model/inspectmodel.h \
    model/process.h \
    model/processitem.h \
    model/ptypes.h \
    model/sessionmodel.h \
    model/transitionmodel.h \
    view/displaystring.h \
    widget/processtree.h \
    widget/tab.h \
    widget/tracelistwidget.h \
    cspmsession.h \
    mainwindow.h \
    programstate.h \
    csperror.h \
    widget/clickablelabel.h \
    widget/errordialog.h

OTHER_FILES += \
    haskell/Cpex/Transitions.hs \
    haskell/Cpex/Foreign.hs \
    haskell/CSPM/Foreign.hs

RESOURCES += \
    cpex.qrc
