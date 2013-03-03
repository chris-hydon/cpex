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

  _tree = new ProcessTree(expr.process().session(), splitter);
  _tree->setHeaderHidden(true);
  TransitionModel * m = new TransitionModel(expr.process(), _tree);
  _tree->setModel(m);

  TraceListWidget * trace = new TraceListWidget(splitter);

  splitter->addWidget(_tree);
  splitter->addWidget(trace);

  QList<int> defaultSizes;
  defaultSizes << 500 << 200;
  splitter->setSizes(defaultSizes);

  connect(
    _tree->selectionModel(),
    SIGNAL(selectionChanged(const QItemSelection &, const QItemSelection &)),
    _tree, SLOT(selectionChanged())
  );
  connect(
    _tree, SIGNAL(itemSelected(QModelIndex)),
    trace, SLOT(setTraces(QModelIndex))
  );

  // Horizontal scroll bar. Need to do this after loading the model for some reason.
  _tree->header()->setHorizontalScrollMode(QAbstractItemView::ScrollPerPixel);
  _tree->header()->setResizeMode(0, QHeaderView::ResizeToContents);
  _tree->header()->setStretchLastSection(false);

  // Expand the root.
  _tree->expand(m->index(0, 0, _tree->rootIndex()));

  // Context menu.
  _tree->setContextMenuPolicy(Qt::CustomContextMenu);
  connect(
    _tree, SIGNAL(customContextMenuRequested(const QPoint &)),
    _tree, SLOT(showContextMenu(const QPoint &))
  );

  // Item delegate.
  _tree->setItemDelegate(new ProcessItemDelegate(_tree));
}

void Tab::setupInspector(const Expression & expr)
{
  QSplitter * splitter = new QSplitter(this);
  splitter->setOrientation(Qt::Horizontal);
  layout()->addWidget(splitter);

  _tree = new ProcessTree(expr.process().session(), this);
  _tree->setHeaderHidden(true);
  InspectModel * m = new InspectModel(expr.process(), _tree);
  _tree->setModel(m);
  layout()->addWidget(_tree);

  // "Why not" widget.
  _inspectorWhy = new QLineEdit(this);
  _inspectorWhy->setPlaceholderText(
    tr("Type an event here to see why it is/is not offered..."));
  layout()->addWidget(_inspectorWhy);

  // Horizontal scroll bar. Need to do this after loading the model for some reason.
  _tree->header()->setHorizontalScrollMode(QAbstractItemView::ScrollPerPixel);
  _tree->header()->setResizeMode(0, QHeaderView::ResizeToContents);
  _tree->header()->setStretchLastSection(false);

  // Expand the root.
  _tree->expand(m->index(0, 0, _tree->rootIndex()));

  _inspectorWhyDetails = new QLabel(this);
  _inspectorWhyDetails->setWordWrap(true);
  _inspectorWhyDetails->setVisible(false);
  layout()->addWidget(_inspectorWhyDetails);

  // Context menu.
  _tree->setContextMenuPolicy(Qt::CustomContextMenu);
  connect(
    _tree, SIGNAL(customContextMenuRequested(const QPoint &)),
    _tree, SLOT(showContextMenu(const QPoint &))
  );

  // "Why not" widget slots. The tree's icon data changes depending on the contents
  // of the text box.
  connect(
    _inspectorWhy, SIGNAL(textChanged(QString)),
    m, SLOT(eventTextChanged(QString))
  );
  connect(
    _tree->selectionModel(),
    SIGNAL(selectionChanged(const QItemSelection &, const QItemSelection &)),
    _tree, SLOT(selectionChanged())
  );
  connect(
    _tree, SIGNAL(itemSelected(const QModelIndex &)),
    this, SLOT(displayEventDetails(const QModelIndex &))
  );
  connect(
    m, SIGNAL(eventChanged()),
    this, SLOT(displayEventDetails())
  );
  connect(
    _tree, SIGNAL(expanded(QModelIndex)),
    m, SLOT(refreshData(QModelIndex))
  );

  // Item delegate.
  _tree->setItemDelegate(new ProcessItemDelegate(_tree));
}

void Tab::updateExprBox()
{
  exprBox->setText(_expression.text(ProgramState::getSessions().count() > 1,
    _expression.mode() != Expression::Probe));
}

void Tab::displayEventDetails(const QModelIndex & idx)
{
  QModelIndex index = idx;
  if (!index.isValid())
  {
    QModelIndexList selected = _tree->selectionModel()->selectedIndexes();
    if (selected.count() > 0)
    {
      index = selected[0];
    }
  }

  // No details if there is nothing typed in.
  if (_inspectorWhy->text() == QString())
  {
    _inspectorWhyDetails->setVisible(false);
    return;
  }

  // If there is no process selected, tell the user to select one, otherwise proceed
  // as normal.
  if (!index.isValid())
  {
    _inspectorWhyDetails->setText(tr("Select an item for details."));
  }
  else
  {
    InspectItem * idx = static_cast<InspectItem *>(index.internalPointer());
    Process process = idx->process();
    QList<Event> events = idx->events();
    _inspectorWhyDetails->setText(process.whyEvent(events, !index.parent().isValid()));
  }

  _inspectorWhyDetails->setVisible(true);
}
