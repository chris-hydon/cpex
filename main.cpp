#include <QLocale>
#include <QTranslator>
#include <QtGui/QApplication>
#include "mainwindow.h"
#include "cspmsession.h"
#include "programstate.h"

extern "C"
{
  typedef void * StgFunTable;
  typedef void * StgRegTable;

  #include <HsFFI.h>
  #include <RtsAPI.h>
}

int main(int argc, char *argv[])
{
  RtsConfig conf = defaultRtsConfig;
  conf.rts_opts_enabled = RtsOptsAll;
  hs_init_ghc(&argc, &argv, conf);

  QApplication a(argc, argv);
  QTranslator translator;
  translator.load(QString("cpex_") + QLocale::system().name(), ":/translations");
  a.installTranslator(&translator);
  MainWindow * w = MainWindow::get();
  w->show();

  int ret = a.exec();
  delete w;

  ProgramState::get()->cleanup();
  hs_exit();

  return ret;
}
