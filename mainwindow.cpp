#include "mainwindow.h"
#include "ui_mainwindow.h"

#include <QFileDialog>
#include "cspmsession.h"

MainWindow::MainWindow(QWidget *parent) :
  QMainWindow(parent),
  ui(new Ui::MainWindow)
{
  ui->setupUi(this);
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
    }
    ui->qsbStatus->showMessage(tr(status.toAscii()), 5000);
  }
}
