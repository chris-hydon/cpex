#include "tab.h"

#include <QtGui/QGridLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLayoutItem>
#include <QtGui/QSplitter>

#include "delegate/processitemdelegate.h"
#include "model/process.h"
#include "model/transitionmodel.h"
#include "widget/processtree.h"
#include "widget/tracelistwidget.h"
#include "programstate.h"

Tab::Tab(QWidget * parent) : QWidget(parent)
{
  QLayout * grid = new QGridLayout(this);
  exprBox = new QLineEdit(this);
  grid->addWidget(exprBox);
}

QString Tab::expression() const
{
  return _expression;
}

bool Tab::setExpression(const QString & expr)
{
  Process p = ProgramState::currentSession()->compileExpression(expr);
  if (!p.isValid())
  {
    return false;
  }

  // Index 0 will be the expression box. Remove everything else.
  QLayoutItem * oldContents;
  while ((oldContents = layout()->takeAt(1)) != 0)
  {
    delete oldContents->widget();
    delete oldContents;
  }

  QSplitter * splitter = new QSplitter(this);
  splitter->setOrientation(Qt::Horizontal);
  layout()->addWidget(splitter);

  ProcessTree * tree = new ProcessTree(splitter);
  tree->setHeaderHidden(true);
  TransitionModel * m = new TransitionModel(p, tree);
  tree->setModel(m);

  TraceListWidget * trace = new TraceListWidget(splitter);

  splitter->addWidget(tree);
  splitter->addWidget(trace);

  QList<int> defaultSizes;
  defaultSizes << 500 << 200;
  splitter->setSizes(defaultSizes);

  connect(
    tree->selectionModel(),
    SIGNAL(selectionChanged(const QItemSelection &, const QItemSelection &)),
    tree, SLOT(selectionChanged())
  );
  connect(
    tree, SIGNAL(itemSelected(QModelIndex)),
    trace, SLOT(setTraces(QModelIndex))
  );

  // Horizontal scroll bar. Need to do this after loading the model for some reason.
  tree->header()->setHorizontalScrollMode(QAbstractItemView::ScrollPerPixel);
  tree->header()->setResizeMode(0, QHeaderView::ResizeToContents);
  tree->header()->setStretchLastSection(false);

  // Expand the root.
  tree->expand(m->index(0, 0, tree->rootIndex()));

  // Context menu.
  tree->setContextMenuPolicy(Qt::CustomContextMenu);
  connect(
    tree, SIGNAL(customContextMenuRequested(const QPoint &)),
    tree, SLOT(showContextMenu(const QPoint &))
  );

  // Item delegate.
  tree->setItemDelegate(new ProcessItemDelegate(tree));

  _expression = expr;
  exprBox->setText(expr);
  return true;
}
