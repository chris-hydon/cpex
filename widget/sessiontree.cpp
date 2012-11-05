#include "sessiontree.h"

#include "model/sessionmodel.h"
#include "mainwindow.h"
#include <QStringListModel>

SessionTree::SessionTree(QWidget * parent) : QTreeView(parent)
{
  SessionModel * model = new SessionModel(this);
  setModel(model);
}
