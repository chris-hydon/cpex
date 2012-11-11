#include "processtree.h"

#include "programstate.h"
#include "mainwindow.h"
#include "ui_mainwindow.h"
#include <QClipboard>

ProcessTree::ProcessTree(QWidget *parent) : QTreeView(parent)
{
  _model = NULL;
}

void ProcessTree::loadInitialState()
{
  Process * p = ProgramState::currentSession()->compileExpression(
    MainWindow::getUi()->qleExpression->text());
  if (p == NULL)
  {
    MainWindow::getUi()->qsbStatus->showMessage(
      tr("Invalid expression for the current file."), 5000);
    return;
  }

  if (_model != NULL)
  {
    delete _model;
  }
  _model = new ProcessModel(p, this);
  setModel(_model);
  connect(
    selectionModel(),
    SIGNAL(selectionChanged(const QItemSelection &, const QItemSelection &)),
    this,
    SLOT(selectionChanged())
  );

  // Horizontal scroll bar. Need to do this after loading the model for some reason.
  header()->setHorizontalScrollMode(QAbstractItemView::ScrollPerPixel);
  header()->setResizeMode(0, QHeaderView::ResizeToContents);
  header()->setStretchLastSection(false);
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
