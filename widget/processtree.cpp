#include "processtree.h"

#include "mainwindow.h"
#include "programstate.h"

#include <QApplication>
#include <QClipboard>

ProcessTree::ProcessTree(const CSPMSession * session, QWidget * parent) :
  QTreeView(parent), _session(session)
{
  _model = NULL;
}

void ProcessTree::showContextMenu(const QPoint & pos)
{
  QMenu menu;
  QAction * copy = menu.addAction("Copy");
  QAction * probeCT = menu.addAction("Probe");
  QAction * probeNT = menu.addAction("Probe (New Tab)");
  QAction * inspectCT = menu.addAction("Inspect");
  QAction * inspectNT = menu.addAction("Inspect (New Tab)");

  QString data = indexAt(pos).data(Qt::EditRole).toString();

  QAction * selectedItem = menu.exec(mapToGlobal(pos));
  if (selectedItem == copy)
  {
    QApplication::clipboard()->setText(data);
  }
  else if (selectedItem == probeCT)
  {
    MainWindow::get()->setTabFromExpression(_session->displayName() + ":" + data);
  }
  else if (selectedItem == probeNT)
  {
    MainWindow::get()->newTabFromExpression(_session->displayName() + ":" + data);
  }
  else if (selectedItem == inspectCT)
  {
    MainWindow::get()->setTabFromExpression(_session->displayName() + ":inspect:" +
      data);
  }
  else if (selectedItem == inspectNT)
  {
    MainWindow::get()->newTabFromExpression(_session->displayName() + ":inspect:" +
      data);
  }
}

void ProcessTree::selectionChanged()
{
  QModelIndex index = selectedIndexes()[0];
  emit itemSelected(index);
}
