#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QtGui/QAction>
#include <QtGui/QGridLayout>
#include <QtGui/QLabel>
#include <QtGui/QMenu>
#include <QtGui/QMenuBar>
#include <QtGui/QSplitter>
#include <QtGui/QStatusBar>
#include <QtGui/QTabWidget>
#include <QtGui/QToolButton>
#include <QtGui/QTreeView>
#include <QtGui/QWidget>

#include <QMainWindow>
#include <QShortcut>
#include "cspmsession.h"

class Tab;

class MainWindow : public QMainWindow
{
  Q_OBJECT

public:
  explicit MainWindow(QWidget * parent = 0);
  ~MainWindow();
  static MainWindow * get();
  Tab * createTab();
  bool setTabExpression(Tab *, const QString &);
  void setCurrentTab(Tab *);
  Tab * currentTab();

public slots:
  void actionOpen();
  void newBlankTab();
  void closeTab(int index = -1);
  void newTabFromExpression(const QString & = QString());
  void setTabFromExpression(const QString & = QString());

signals:
  void fileLoaded(CSPMSession * session);

private:
  static MainWindow * window;

  // UI elements
  QMenuBar * uiMenu;
  QMenu * uiMenuFile;
  QAction * uiMenuFileOpen;
  QStatusBar * uiStatus;
  QWidget * uiCentral;
  QSplitter * uiSplitter;
  QTreeView * uiSessions;
  QTabWidget * uiTabs;
  QToolButton * uiNewTabButton;
};

#endif // MAINWINDOW_H
