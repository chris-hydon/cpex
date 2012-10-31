#include "processtree.h"

ProcessTree::ProcessTree(QWidget *parent) :
  QTreeView(parent)
{
  _model = NULL;
}

ProcessTree::~ProcessTree()
{
  delete _model;
}

void ProcessTree::loadInitialState()
{
  if (_model != NULL)
  {
    delete _model;
  }
  _model = new ProcessModel("PHILS", this);
  setModel(_model);
}
