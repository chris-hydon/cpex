#include "sessiontree.h"

#include "cspmsession.h"
#include <QStringListModel>

SessionTree::SessionTree(QWidget * parent) : QTreeView(parent)
{
}

void SessionTree::fileLoaded()
{
  QAbstractItemModel * oldModel = model();
  QStringList strings = CSPMSession::getSession()->procCallNames();
  QAbstractItemModel * newModel = new QStringListModel(strings, this);
  setModel(newModel);

  if (oldModel != NULL)
  {
    delete oldModel;
  }
}
