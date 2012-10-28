#include <QtGui/QApplication>
#include "mainwindow.h"
#include "cspmsession.h"

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
  MainWindow w;
  w.show();

  int ret = a.exec();

  CSPMSession::getSession()->free();
  hs_exit();

  return ret;
}
