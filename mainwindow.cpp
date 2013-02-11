#include "mainwindow.h"

#include <QApplication>
#include <QFileDialog>
#include <QMouseEvent>
#include <QTabBar>
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
  uiSplitter->addWidget(uiSessions);
  uiTabs = new TabWidget(uiSplitter);
  uiTabs->setDocumentMode(true);
  uiTabs->setElideMode(Qt::ElideRight);
  newBlankTab();
  uiNewTabButton = new QToolButton(uiTabs);
  uiNewTabButton->setAutoRaise(true);
  uiNewTabButton->setIcon(QIcon(QApplication::style()->standardPixmap(
    QStyle::SP_ArrowForward))); // TODO: Placeholder icon.
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
  uiMenuFile = new QMenu(uiMenu);
  uiMenuFile->setTitle(tr("&File"));
  uiMenuFileOpen = new QAction(uiMenuFile);
  uiMenuFileOpen->setText(tr("&Open"));
  // Menu bar actions.
  uiMenu->addAction(uiMenuFile->menuAction());
  uiMenuFile->addAction(uiMenuFileOpen);

  // Status bar
  uiStatus = new QStatusBar(this);

  // Assemble main window.
  setCentralWidget(uiCentral);
  setMenuBar(uiMenu);
  setStatusBar(uiStatus);

  // Model for TreeView uiSessions
  SessionModel * sessModel = new SessionModel(uiSessions);
  uiSessions->setModel(sessModel);

  // Signals and slots
  connect(uiMenuFileOpen, SIGNAL(triggered()), this, SLOT(actionOpen()));
  connect(
    this, SIGNAL(fileLoaded(CSPMSession *)),
    sessModel, SLOT(sessionLoaded(CSPMSession *))
  );
  connect(
    uiSessions, SIGNAL(activated(const QModelIndex &)),
    sessModel, SLOT(itemActivated(const QModelIndex &))
  );
  connect(
    uiTabs, SIGNAL(tabCloseRequested(int)),
    this, SLOT(closeTab(int))
  );
  connect(uiNewTabButton, SIGNAL(clicked()), this, SLOT(newBlankTab()));
}

MainWindow::~MainWindow()
{
}

Tab * MainWindow::createTab()
{
  uiTabs->setTabsClosable(uiTabs->count() != 0);
  Tab * tab = new Tab(uiTabs);
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
  QLabel * blankTabLabel = new QLabel(tr("Open a CSP file to begin, then double-click an expression\nin the pane on the left or enter it into the box above."), blankTab);
  blankTabLabel->setAlignment(Qt::AlignCenter);
  blankTab->layout()->addWidget(blankTabLabel);
}

void MainWindow::actionOpen()
{
  QString file = QFileDialog::getOpenFileName(this, tr("Select file to open"),
    QDir::homePath(), tr("CSP definition files (*.csp);;All files (*.*)"));
  if (file != NULL)
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
      emit fileLoaded(opened);
    }
    uiStatus->showMessage(tr(status.toAscii()), 5000);
  }
}

bool MainWindow::setTabExpression(Tab * tab, const QString & expr)
{
  Expression e(expr);
  if (!e.isValid())
  {
    uiStatus->showMessage(tr("Invalid expression for the current file."), 5000);
    return false;
  }
  else
  {
    tab->setExpression(e);
    uiTabs->setTabsClosable(true);
    // Don't set to expr directly in case expr can be shortened.
    uiTabs->setTabText(uiTabs->indexOf(tab), tab->exprBox->text());
    return true;
  }
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

Tab * MainWindow::currentTab()
{
  return static_cast<Tab *>(uiTabs->currentWidget());
}

void MainWindow::newTabFromExpression(const QString & expression)
{
  Tab * oldTab = static_cast<Tab *>(uiTabs->currentWidget());
  Tab * tab = createTab();
  if (!setTabExpression(tab, expression == QString() ? oldTab->exprBox->text() :
    expression))
  {
    delete tab;
  }
  else
  {
    setCurrentTab(tab);
    oldTab->updateExprBox();
  }
}

void MainWindow::setTabFromExpression(const QString & expression)
{
  Tab * tab = static_cast<Tab *>(uiTabs->currentWidget());
  setTabExpression(tab, expression == QString() ? tab->exprBox->text() : expression);
}
