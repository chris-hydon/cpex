#include "processtree.h"

#include "model/transitionmodel.h"
#include "mainwindow.h"
#include "programstate.h"

#include <QApplication>
#include <QClipboard>

ProcessTree::ProcessTree(QWidget *parent) : QTreeView(parent)
{
  _model = NULL;
}

void ProcessTree::showContextMenu(const QPoint & pos)
{
  QMenu menu;
  menu.addAction("Copy");

  QAction * selectedItem = menu.exec(mapToGlobal(pos));
  if (selectedItem)
  {
    QApplication::clipboard()->setText(indexAt(pos).data(Qt::EditRole).toString());
  }
}

void ProcessTree::selectionChanged()
{
  QModelIndex index = selectedIndexes()[0];
  emit itemSelected(index);
}
