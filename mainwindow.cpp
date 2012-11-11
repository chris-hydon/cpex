#include "mainwindow.h"
#include "ui_mainwindow.h"

#include <QFileDialog>
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

Ui::MainWindow * MainWindow::getUi()
{
  return MainWindow::get()->ui;
}

MainWindow::MainWindow(QWidget * parent) :
  QMainWindow(parent),
  ui(new Ui::MainWindow)
{
  ui->setupUi(this);

  // These are the size ratios of items in the splitter. However, this does not
  // work if the sizes given are too small.
  QList<int> defaultSizes;
  defaultSizes << 200 << 500 << 200;
  ui->qspSplitter->setSizes(defaultSizes);

  connect(
    this, SIGNAL(fileLoaded(CSPMSession *)),
    ui->qtvSessions->model(), SLOT(sessionLoaded(CSPMSession *))
  );

  // Context menu for explorer.
  ui->qtvExplorer->setContextMenuPolicy(Qt::CustomContextMenu);
  connect(
    ui->qtvExplorer, SIGNAL(customContextMenuRequested(const QPoint &)),
    ui->qtvExplorer, SLOT(showContextMenu(const QPoint &))
  );
}

MainWindow::~MainWindow()
{
  delete ui;
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
      status = "Error while loading file: " + file;
    }
    else
    {
      status = "Loaded file: " + file;
      emit fileLoaded(opened);
    }
    ui->qsbStatus->showMessage(tr(status.toAscii()), 5000);
  }
}
