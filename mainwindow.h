#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include "cspmsession.h"

namespace Ui {
class MainWindow;
}

class MainWindow : public QMainWindow
{
  Q_OBJECT

public:
  explicit MainWindow(QWidget * parent = 0);
  ~MainWindow();
  static MainWindow * get();
  static Ui::MainWindow * getUi();

public slots:
  void actionOpen();

signals:
  void fileLoaded(CSPMSession * session);

private:
  static MainWindow * window;
  Ui::MainWindow * ui;
};

#endif // MAINWINDOW_H
