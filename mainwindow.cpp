#include "mainwindow.h"

#include <QApplication>
#include <QFileDialog>
#include <QHeaderView>
#include <QMouseEvent>
#include <QTabBar>
#include <QTableWidget>
#include <QTextDocument>
#include "model/sessionmodel.h"
#include "widget/tab.h"
#include "cspmsession.h"
#include "programstate.h"

MainWindow * MainWindow::window = NULL;

MainWindow * MainWindow::get()
{
  if (MainWindow::window == NULL)
  {
    MainWindow::window = new MainWindow();
  }
  return MainWindow::window;
}

class TabBar : public QTabBar
{
public:
  TabBar(QWidget * parent = 0) : QTabBar(parent) {}

protected:
  virtual void mouseReleaseEvent(QMouseEvent * event)
  {
    if (event->button() == Qt::MiddleButton)
    {
      emit tabCloseRequested(tabAt(event->pos()));
    }
  }
};

class TabWidget : public QTabWidget
{
public:
  TabWidget(QWidget * parent = 0) : QTabWidget(parent)
  {
    setTabBar(new TabBar(this));
  }
};

MainWindow::MainWindow(QWidget * parent) : QMainWindow(parent)
{
  resize(900, 500);
  setWindowTitle(tr("CSP Process Explorer"));

  // Main UI elements
  uiCentral = new QWidget(this);
  QGridLayout * grid = new QGridLayout(uiCentral);
  uiSplitter = new QSplitter(uiCentral);
  uiSplitter->setOrientation(Qt::Horizontal);
  grid->addWidget(uiSplitter, 0, 0, 1, 1);
  uiSessions = new QTreeView(uiSplitter);
  uiSessions->setRootIsDecorated(true);
  uiSessions->setHeaderHidden(true);
  uiSessions->setContextMenuPolicy(Qt::CustomContextMenu);
  uiSplitter->addWidget(uiSessions);
  uiTabs = new TabWidget(uiSplitter);
  uiTabs->setDocumentMode(true);
  uiTabs->setElideMode(Qt::ElideRight);
  uiNewTabButton = new QToolButton(uiTabs);
  uiNewTabButton->setAutoRaise(true);
  uiNewTabButton->setIcon(QIcon(":/images/new-tab.png"));
  uiNewTabButton->setShortcut(QKeySequence::AddTab);
  uiTabs->setCornerWidget(uiNewTabButton);
  uiSplitter->addWidget(uiTabs);

  // These are the size ratios of items in the splitter. However, this does not
  // work if the sizes given are too small.
  QList<int> defaultSizes;
  defaultSizes << 200 << 700;
  uiSplitter->setSizes(defaultSizes);

  // Menu bar
  uiMenu = new QMenuBar(this);
  uiMenu->setGeometry(QRect(0, 0, 836, 23));
  uiMenuSession = new QMenu(uiMenu);
  uiMenuSession->setTitle(tr("&Session"));
  uiMenuSessionOpen = new QAction(uiMenuSession);
  uiMenuSessionOpen->setText(tr("&Open"));
  uiMenuSessionOpen->setStatusTip(tr("Open a CSP file to explore."));
  uiMenuSessionReload = new QAction(uiMenuSession);
  uiMenuSessionReload->setStatusTip(tr("Reload the CSP file into the current session."));
  uiMenuSessionReload->setVisible(false);
  uiMenuSessionReloadAll = new QAction(uiMenuSession);
  uiMenuSessionReloadAll->setText(tr("Reload All"));
  uiMenuSessionReloadAll->setStatusTip(tr(
    "Reload all currently open sessions from their respective files."));
  uiMenuSessionClose = new QAction(uiMenuSession);
  uiMenuSessionClose->setStatusTip(tr(
    "Close the current session and remove it from the list."));
  uiMenuSessionClose->setVisible(false);
  uiMenuSessionCloseAll = new QAction(uiMenuSession);
  uiMenuSessionCloseAll->setText(tr("Close All"));
  uiMenuSessionCloseAll->setStatusTip(tr(
    "Close all sessions and remove them from the list, and close all tabs."));
  uiMenuSessionErrors = new QAction(uiMenuSession);
  uiMenuSessionErrors->setText(tr("Error log"));
  uiMenuSessionErrors->setStatusTip(tr("Show a log of all errors encountered."));
  uiMenuSessionExit = new QAction(uiMenuSession);
  uiMenuSessionExit->setText(tr("E&xit"));
  uiMenuSessionExit->setStatusTip(tr("Close the program."));
  uiMenuBehaviour = new QMenu(uiMenu);
  uiMenuBehaviour->setTitle(tr("&Behaviour"));
  QActionGroup * g = new QActionGroup(uiMenuBehaviour);
  uiMenuBehaviourSync = new QAction(g);
  uiMenuBehaviourSync->setText(tr("Synchronous Termination Semantics"));
  uiMenuBehaviourSync->setStatusTip(tr(
    "Use the FDR 2 style behaviour for termination of processes in parallel."));
  uiMenuBehaviourSync->setCheckable(true);
  uiMenuBehaviourSync->setChecked(true);
  uiMenuBehaviourAsync = new QAction(g);
  uiMenuBehaviourAsync->setCheckable(true);
  uiMenuBehaviourAsync->setText(tr("Asynchronous Termination Semantics"));
  uiMenuBehaviourAsync->setStatusTip(tr(
    "Use the Omega style behaviour for termination of processes in parallel."));

  // Menu bar actions.
  uiMenu->addAction(uiMenuSession->menuAction());
  uiMenuSession->addAction(uiMenuSessionOpen);
  uiMenuSession->addAction(uiMenuSessionReload);
  uiMenuSession->addAction(uiMenuSessionReloadAll);
  uiMenuSession->addAction(uiMenuSessionClose);
  uiMenuSession->addAction(uiMenuSessionCloseAll);
  uiMenuSession->addAction(uiMenuSessionErrors);
  uiMenuSession->addAction(uiMenuSessionExit);
  uiMenu->addAction(uiMenuBehaviour->menuAction());
  uiMenuBehaviour->addAction(uiMenuBehaviourSync);
  uiMenuBehaviour->addAction(uiMenuBehaviourAsync);

  // New blank tab. Add this after the menu, since creating a tab requires the
  // presence of the behaviour menu elements.
  newBlankTab();

  // Status bar
  uiStatus = new QStatusBar(this);
  uiStatusErrors = new ClickableLabel(uiStatus);
  uiStatusErrors->setPixmap(QPixmap(":/images/error.png"));
  uiStatusErrors->setVisible(false);
  uiStatus->addPermanentWidget(uiStatusErrors);

  // Error dialog.
  uiErrors = new ErrorDialog(this);

  // Assemble main window.
  setCentralWidget(uiCentral);
  setMenuBar(uiMenu);
  setStatusBar(uiStatus);

  // Model for TreeView uiSessions
  SessionModel * sessModel = new SessionModel(uiSessions);
  uiSessions->setModel(sessModel);

  // Signals and slots
  connect(uiMenuSessionOpen, SIGNAL(triggered()), this, SLOT(actionOpen()));
  connect(uiMenuSessionReload, SIGNAL(triggered()), this, SLOT(actionReload()));
  connect(uiMenuSessionReloadAll, SIGNAL(triggered()),
    this, SLOT(actionReloadAll()));
  connect(uiMenuSessionClose, SIGNAL(triggered()), this, SLOT(actionClose()));
  connect(uiMenuSessionCloseAll, SIGNAL(triggered()), this, SLOT(actionCloseAll()));
  connect(uiMenuSessionExit, SIGNAL(triggered()), this, SLOT(close()));
  connect(uiMenuBehaviourSync, SIGNAL(triggered()),
    this, SLOT(actionSyncSemantics()));
  connect(uiMenuBehaviourAsync, SIGNAL(triggered()),
    this, SLOT(actionAsyncSemantics()));
  connect(
    uiSessions, SIGNAL(activated(const QModelIndex &)),
    sessModel, SLOT(itemActivated(const QModelIndex &))
  );
  connect(
    uiSessions, SIGNAL(customContextMenuRequested(QPoint)),
    this, SLOT(sessionContextMenu(QPoint))
  );
  connect(
    uiTabs, SIGNAL(tabCloseRequested(int)),
    this, SLOT(closeTab(int))
  );
  connect(uiTabs, SIGNAL(currentChanged(int)), this, SLOT(tabChanged(int)));
  connect(uiNewTabButton, SIGNAL(clicked()), this, SLOT(newBlankTab()));
  connect(uiStatusErrors, SIGNAL(clicked()), this, SLOT(showErrorLog()));
  connect(uiMenuSessionErrors, SIGNAL(triggered()), this, SLOT(showErrorLog()));

  // A file may be passed as an argument to load on launch.
  QStringList args = QApplication::arguments();
  if (args.length() > 1)
  {
    newSession(args.last());
  }
}

MainWindow::~MainWindow()
{
}

Tab * MainWindow::createTab()
{
  uiTabs->setTabsClosable(uiTabs->count() != 0);
  Tab * tab = new Tab(uiTabs);
  tab->setBehaviour(Tab::AsynchronousTermination, uiMenuBehaviourAsync->isChecked());
  QShortcut * shiftReturn = new QShortcut(QKeySequence("Shift+Return"), tab->exprBox);
  shiftReturn->setContext(Qt::WidgetShortcut);
  connect(tab->exprBox, SIGNAL(returnPressed()), this, SLOT(setTabFromExpression()));
  connect(shiftReturn, SIGNAL(activated()), this, SLOT(newTabFromExpression()));
  uiTabs->addTab(tab, tr("New Tab"));
  return tab;
}

void MainWindow::newBlankTab()
{
  Tab * blankTab = createTab();
  QLabel * blankTabLabel = new QLabel(tr(
    "Open a CSP file to begin, then double-click an expression\nin the pane on the "
    "left or enter it into the box above."), blankTab);
  blankTabLabel->setAlignment(Qt::AlignCenter);
  blankTab->layout()->addWidget(blankTabLabel);
  uiTabs->setCurrentWidget(blankTab);
}

void MainWindow::actionOpen()
{
  QString file = QFileDialog::getOpenFileName(this, tr("Select file to open"),
    QDir::homePath(), tr("CSP definition files (*.csp);;All files (*.*)"));
  if (file != NULL)
  {
    newSession(file);
  }
}

void MainWindow::actionReload()
{
  QString status;
  CSPMSession * session = ProgramState::currentSession();
  if (session->reload())
  {
    closeSessionTabs(session);
    static_cast<SessionModel *>(uiSessions->model())->reloadSession(session);
    status = tr("Reloaded file: %1").arg(session->fileName());
  }
  else
  {
    status = tr("Error while reloading file: %1").arg(session->fileName());
  }
  uiStatus->showMessage(status, 5000);
}

void MainWindow::actionReloadAll()
{
  QString status;
  QStringList failures;
  foreach (CSPMSession * session, ProgramState::getSessions().values())
  {
    if (session->reload())
    {
      closeSessionTabs(session);
      static_cast<SessionModel *>(uiSessions->model())->reloadSession(session);
    }
    else
    {
      failures << session->displayName();
    }
  }

  if (failures.isEmpty())
  {
    status = tr("Successfully reloaded all open files.");
  }
  else
  {
    status = tr("Error while reloading the following sessions: %1")
      .arg(failures.join(", "));
  }
  uiStatus->showMessage(status, 5000);
}

void MainWindow::actionClose()
{
  CSPMSession * session = ProgramState::currentSession();
  closeSessionTabs(session);
  static_cast<SessionModel *>(uiSessions->model())->removeSession(session);
  ProgramState::deleteSession(session);
  setCurrentSession(NULL);
}

void MainWindow::actionCloseAll()
{
  // Close all tabs.
  while (uiTabs->count() > 0)
  {
    delete uiTabs->widget(0);
  }
  newBlankTab();

  // Remove sessions from the session view pane.
  static_cast<SessionModel *>(uiSessions->model())->removeAllSessions();

  // Delete all sessions internally.
  foreach (CSPMSession * session, ProgramState::getSessions().values())
  {
    ProgramState::deleteSession(session);
  }

  setCurrentSession(NULL);
}

void MainWindow::actionSyncSemantics()
{
  currentTab()->setBehaviour(Tab::AsynchronousTermination, false);
}

void MainWindow::actionAsyncSemantics()
{
  currentTab()->setBehaviour(Tab::AsynchronousTermination, true);
}

void MainWindow::setTabExpression(Tab * tab, const Expression & expr)
{
  tab->setExpression(expr);
  uiTabs->setTabsClosable(true);
  int index = uiTabs->indexOf(tab);
  QString tabText = tab->exprBox->text();
  // Wrap in <p> to enable rich text word wrapping.
  QString toolTipText = QString("<p>") + Qt::escape(tabText) + "</p>";
  uiTabs->setTabToolTip(index, toolTipText);
  // Truncate and elide to stop tabs from getting too wide.
  if (tabText.length() > 25)
  {
    tabText.truncate(20);
    tabText += QChar(0x2026);
  }
  uiTabs->setTabText(index, tabText);
}

void MainWindow::setCurrentTab(Tab * tab)
{
  uiTabs->setCurrentWidget(tab);
}

void MainWindow::closeTab(int index)
{
  if (index == -1)
  {
    index = uiTabs->currentIndex();
  }
  delete uiTabs->widget(index);

  if (uiTabs->count() == 0)
  {
    newBlankTab();
  }
  if (uiTabs->count() == 1 &&
    !static_cast<Tab *>(uiTabs->widget(0))->expression().isValid())
  {
    uiTabs->setTabsClosable(false);
  }
}

void MainWindow::tabChanged(int index)
{
  if (index != -1)
  {
    Tab * tab = static_cast<Tab *>(uiTabs->widget(index));
    if (tab->behaviour(Tab::AsynchronousTermination))
    {
      uiMenuBehaviourAsync->setChecked(true);
    }
    else
    {
      uiMenuBehaviourSync->setChecked(true);
    }
  }
}

Tab * MainWindow::currentTab()
{
  return static_cast<Tab *>(uiTabs->currentWidget());
}

void MainWindow::closeSessionTabs(const CSPMSession * session)
{
  int count = uiTabs->count();
  for (int i = 0; i < count; i++)
  {
    Tab * tab = static_cast<Tab *>(uiTabs->widget(i));
    if (tab->expression().isValid() &&
      tab->expression().process().session() == session)
    {
      // Close the tab and then decrement i and count (since the indices change).
      closeTab(i--);
      count--;
    }
  }
}

void MainWindow::newSession(const QString & file)
{
  QString status;
  CSPMSession * opened = ProgramState::newSession(file);
  if (opened == NULL)
  {
    status = tr("Error while loading file: %1").arg(file);
  }
  else
  {
    status = tr("Loaded file: %1").arg(file);
    static_cast<SessionModel *>(uiSessions->model())->addSession(opened);
    setCurrentSession(opened);
  }
  uiStatus->showMessage(status, 5000);
}

void MainWindow::setCurrentSession(CSPMSession * session)
{
  ProgramState::setCurrentSession(session);
  if (session != NULL)
  {
    uiMenuSessionClose->setText(tr("Close %1").arg(session->displayName()));
    uiMenuSessionClose->setVisible(true);
    uiMenuSessionReload->setText(tr("Reload %1").arg(session->displayName()));
    uiMenuSessionReload->setVisible(true);
  }
  else
  {
    uiMenuSessionClose->setVisible(false);
    uiMenuSessionReload->setVisible(false);
  }
}

void MainWindow::newTabFromExpression(const Expression & expression)
{
  Tab * oldTab = static_cast<Tab *>(uiTabs->currentWidget());
  Expression expr = expression;
  if (!expr.isValid())
  {
    expr = Expression(oldTab->exprBox->text());
    if (!expr.isValid())
    {
      _invalidExpressionMessage();
      return;
    }
  }
  Tab * tab = createTab();
  setTabExpression(tab, expr);
  setCurrentTab(tab);
  oldTab->updateExprBox();
}

void MainWindow::setTabFromExpression(const Expression & expression)
{
  Tab * tab = static_cast<Tab *>(uiTabs->currentWidget());
  Expression expr = expression;
  if (!expr.isValid())
  {
    expr = Expression(tab->exprBox->text());
    if (!expr.isValid())
    {
      _invalidExpressionMessage();
      return;
    }
  }
  setTabExpression(tab, expr);
}

void MainWindow::_invalidExpressionMessage()
{
  uiStatus->showMessage(tr("Invalid expression for the current file."), 5000);
}

void MainWindow::setErrorCount(int count)
{
  if (!count)
  {
    uiStatusErrors->setVisible(false);
  }
  else
  {
    uiStatusErrors->setVisible(true);
    uiStatusErrors->setToolTip(tr("%n error(s)", "", count));
  }
  uiErrors->errorsChanged();
}

void MainWindow::showErrorLog()
{
  uiErrors->show();
}

void MainWindow::sessionContextMenu(const QPoint & pos)
{
  static QString probe = "%1:probe:%2";
  static QString inspect = "%1:inspect:%2";

  // Ensure that we are right-clicking a process with no blanks.
  QModelIndex index = uiSessions->indexAt(pos);
  if (!index.isValid()) return;
  SessionModel * model = static_cast<SessionModel *>(uiSessions->model());
  QString procStr = model->getProcInputString(index);
  QString sessionName = model->getSession(index)->displayName();
  if (procStr == QString() || procStr.contains("_")) return;

  QMenu menu;
  QAction * probeCT = menu.addAction("Probe");
  QAction * probeNT = menu.addAction("Probe (New Tab)");
  QAction * inspectCT = menu.addAction("Inspect");
  QAction * inspectNT = menu.addAction("Inspect (New Tab)");

  QAction * selectedItem = menu.exec(uiSessions->mapToGlobal(pos));
  if (selectedItem == probeCT)
  {
    Expression e(probe.arg(sessionName, procStr));
    setTabFromExpression(e);
  }
  else if (selectedItem == probeNT)
  {
    Expression e(probe.arg(sessionName, procStr));
    newTabFromExpression(e);
  }
  else if (selectedItem == inspectCT)
  {
    Expression e(inspect.arg(sessionName, procStr));
    setTabFromExpression(e);
  }
  else if (selectedItem == inspectNT)
  {
    Expression e(inspect.arg(sessionName, procStr));
    newTabFromExpression(e);
  }
}
