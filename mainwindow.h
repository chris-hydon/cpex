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
#include "model/expression.h"
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
  void setTabExpression(Tab *, const Expression &);
  void setCurrentTab(Tab *);
  Tab * currentTab();
  void closeSessionTabs(const CSPMSession *);
  void newSession(const QString &);
  void setCurrentSession(CSPMSession *);

public slots:
  void actionOpen();
  void actionReload();
  void actionReloadAll();
  void actionClose();
  void actionCloseAll();
  void newBlankTab();
  void closeTab(int index = -1);
  void newTabFromExpression(const Expression & = Expression());
  void setTabFromExpression(const Expression & = Expression());

private:
  void _invalidExpressionMessage();

  static MainWindow * window;

  // UI elements
  QMenuBar * uiMenu;
  QMenu * uiMenuSession;
  QAction * uiMenuSessionOpen;
  QAction * uiMenuSessionExit;
  QAction * uiMenuSessionClose;
  QAction * uiMenuSessionReload;
  QAction * uiMenuSessionCloseAll;
  QAction * uiMenuSessionReloadAll;
  QStatusBar * uiStatus;
  QWidget * uiCentral;
  QSplitter * uiSplitter;
  QTreeView * uiSessions;
  QTabWidget * uiTabs;
  QToolButton * uiNewTabButton;
};

#endif // MAINWINDOW_H
