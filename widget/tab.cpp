#include "tab.h"

#include <QtGui/QGridLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLayoutItem>
#include <QtGui/QSplitter>

#include "delegate/processitemdelegate.h"
#include "model/expression.h"
#include "model/inspectmodel.h"
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

Expression Tab::expression() const
{
  return _expression;
}

void Tab::setExpression(const Expression & expr)
{
  if (!expr.isValid())
  {
    return;
  }
  _expression = expr;
  updateExprBox();

  // Index 0 will be the expression box. Remove everything else.
  QLayoutItem * oldContents;
  while ((oldContents = layout()->takeAt(1)) != 0)
  {
    delete oldContents->widget();
    delete oldContents;
  }

  switch (expr.mode())
  {
    case Expression::Probe:
      setupProbe(expr);
      break;
    case Expression::Inspect:
      setupInspector(expr);
      break;
  }
}

void Tab::setupProbe(const Expression & expr)
{
  QSplitter * splitter = new QSplitter(this);
  splitter->setOrientation(Qt::Horizontal);
  layout()->addWidget(splitter);

  ProcessTree * tree = new ProcessTree(expr.process().session(), splitter);
  tree->setHeaderHidden(true);
  TransitionModel * m = new TransitionModel(expr.process(), tree);
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
}

void Tab::setupInspector(const Expression & expr)
{
  QSplitter * splitter = new QSplitter(this);
  splitter->setOrientation(Qt::Horizontal);
  layout()->addWidget(splitter);

  ProcessTree * tree = new ProcessTree(expr.process().session(), this);
  tree->setHeaderHidden(true);
  InspectModel * m = new InspectModel(expr.process(), tree);
  tree->setModel(m);
  layout()->addWidget(tree);

  // "Why not" widget.
  QLineEdit * why = new QLineEdit(this);
  why->setPlaceholderText("Type an event here to see why it is/is not offered...");
  layout()->addWidget(why);

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

  // "Why not" widget slots. The tree's icon data changes depending on the contents
  // of the text box.
  connect(why, SIGNAL(textChanged(QString)), m, SLOT(eventTextChanged(QString)));

  // Item delegate.
  tree->setItemDelegate(new ProcessItemDelegate(tree));
}


void Tab::updateExprBox()
{
  exprBox->setText(_expression.text(ProgramState::getSessions().count() > 1,
    _expression.mode() != Expression::Probe));
}
