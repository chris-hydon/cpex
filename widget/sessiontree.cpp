#include "sessiontree.h"

#include "model/sessionmodel.h"
#include "mainwindow.h"
#include <QStringListModel>

SessionTree::SessionTree(QWidget * parent) : QTreeView(parent)
{
  SessionModel * model = new SessionModel(this);
  setModel(model);
  connect(this, SIGNAL(activated(const QModelIndex &)),
    model, SLOT(itemActivated(const QModelIndex &)));
}
