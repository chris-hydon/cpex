#include "processtree.h"

#include "model/processitem.h"
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

  if (!indexAt(pos).isValid()) return;
  Process proc = static_cast<ProcessItem *>(indexAt(pos).internalPointer())->process();

  QAction * selectedItem = menu.exec(mapToGlobal(pos));
  if (selectedItem == copy)
  {
    QApplication::clipboard()->setText(indexAt(pos).data(Qt::EditRole).toString());
  }
  else if (selectedItem == probeCT)
  {
    Expression e(proc, Expression::Probe);
    MainWindow::get()->setTabFromExpression(e);
  }
  else if (selectedItem == probeNT)
  {
    Expression e(proc, Expression::Probe);
    MainWindow::get()->newTabFromExpression(e);
  }
  else if (selectedItem == inspectCT)
  {
    Expression e(proc, Expression::Inspect);
    MainWindow::get()->setTabFromExpression(e);
  }
  else if (selectedItem == inspectNT)
  {
    Expression e(proc, Expression::Inspect);
    MainWindow::get()->newTabFromExpression(e);
  }
}

void ProcessTree::selectionChanged()
{
  QModelIndex index = selectedIndexes()[0];
  emit itemSelected(index);
}
