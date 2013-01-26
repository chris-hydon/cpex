#include "processtree.h"

#include "model/transitionmodel.h"
#include "mainwindow.h"
#include "programstate.h"
#include "ui_mainwindow.h"

#include <QClipboard>

ProcessTree::ProcessTree(QWidget *parent) : QTreeView(parent)
{
  _model = NULL;
}

void ProcessTree::loadInitialState()
{
  Process p = ProgramState::currentSession()->compileExpression(
    MainWindow::getUi()->qleExpression->text());
  if (!p.isValid())
  {
    MainWindow::getUi()->qsbStatus->showMessage(
      tr("Invalid expression for the current file."), 5000);
    return;
  }

  if (_model != NULL)
  {
    delete _model;
  }
  _model = new TransitionModel(p, this);
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

  // Expand the root.
  expand(_model->index(0, 0, rootIndex()));
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
