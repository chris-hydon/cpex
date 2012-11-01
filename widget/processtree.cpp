#include "processtree.h"

#include "mainwindow.h"
#include "ui_mainwindow.h"

ProcessTree::ProcessTree(QWidget *parent) :
  QTreeView(parent)
{
  _model = NULL;
}

void ProcessTree::loadInitialState()
{
  if (_model != NULL)
  {
    delete _model;
  }
  _model = new ProcessModel(MainWindow::getUi()->qleExpression->text(), this);
  setModel(_model);
}
