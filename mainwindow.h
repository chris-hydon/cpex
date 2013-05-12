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
#include "widget/clickablelabel.h"
#include "widget/errordialog.h"
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
  void setErrorCount(int);

public slots:
  void actionOpen();
  void actionReload(CSPMSession * = NULL);
  void actionReloadAll();
  void actionClose(CSPMSession * = NULL);
  void actionCloseAll();
  void actionSyncSemantics();
  void actionAsyncSemantics();
  void newBlankTab();
  void closeTab(int index = -1);
  void tabChanged(int);
  void newTabFromExpression(const Expression & = Expression());
  void setTabFromExpression(const Expression & = Expression());
  void showErrorLog();
  void sessionContextMenu(const QPoint &);

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
  QAction * uiMenuSessionErrors;
  QMenu * uiMenuBehaviour;
  QAction * uiMenuBehaviourSync;
  QAction * uiMenuBehaviourAsync;
  QStatusBar * uiStatus;
  ClickableLabel * uiStatusErrors;
  QWidget * uiCentral;
  QSplitter * uiSplitter;
  QTreeView * uiSessions;
  QTabWidget * uiTabs;
  QToolButton * uiNewTabButton;
  ErrorDialog * uiErrors;
};

#endif // MAINWINDOW_H
