#include "mainwindow.h"
#include "ui_mainwindow.h"

#include <QFileDialog>
#include "cspmsession.h"

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
  ui->qlsSplitter->setSizes(defaultSizes);
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
    int r = CSPMSession::getSession()->loadFile(file);
    QString status;
    if (!r)
    {
      status = "Error while loading file: " + file;
    }
    else
    {
      status = "Loaded file: " + file;
      ui->qtvSessions->fileLoaded();
    }
    ui->qsbStatus->showMessage(tr(status.toAscii()), 5000);
  }
}
