#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>

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

private:
  static MainWindow * window;
  Ui::MainWindow * ui;
};

#endif // MAINWINDOW_H
