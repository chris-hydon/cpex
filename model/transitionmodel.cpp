#include "transitionmodel.h"

#include "cspmsession.h"
#include "programstate.h"

TransitionItem::TransitionItem(const Process & process, const TransitionItem * parent,
  const Event & cause, int index) : process(process), parent(parent),
  cause(cause), index(index)
{
}

TransitionItem::TransitionItem(const Process & process) : parent(NULL), index(0)
{
  _next.append(new TransitionItem(process, this, Event(), 0));
}

TransitionItem::~TransitionItem()
{
  while (!_next.isEmpty())
  {
    delete _next.takeFirst();
  }
}

TransitionItem * TransitionItem::next(int index) const
{
  return _next.at(index);
}

int TransitionItem::count() const
{
  // Lazy-load it here, since count() must always be called before next.
  if (!_loaded && process.isValid())
  {
    QList<QPair<Event, Process> > transitions = process.transitions();
    QPair<Event, Process> pair;
    for (int i = 0; i < transitions.count(); i++)
    {
      pair = transitions.at(i);
      _next.append(new TransitionItem(pair.second, this, pair.first, i));
    }
    _loaded = true;
  }
  return _next.count();
}

TransitionModel::TransitionModel(const Process & rootProcess, QObject * parent) :
  QAbstractItemModel(parent), _rootProcess(new TransitionItem(rootProcess))
{
}

TransitionModel::~TransitionModel()
{
  delete _rootProcess;
}

QModelIndex TransitionModel::index(int row, int column, const QModelIndex & parent) const
{
  if (!hasIndex(row, column, parent))
  {
    return QModelIndex();
  }

  const TransitionItem * parentItem;
  if (!parent.isValid())
  {
    parentItem = _rootProcess;
  }
  else
  {
    parentItem = static_cast<TransitionItem *>(parent.internalPointer());
  }

  return createIndex(row, column, parentItem->next(row));
}

QModelIndex TransitionModel::parent(const QModelIndex & index) const
{
  if (!index.isValid())
  {
    return QModelIndex();
  }

  const TransitionItem * parentItem =
    static_cast<TransitionItem *>(index.internalPointer())->parent;

  if (parentItem == _rootProcess)
  {
    return QModelIndex();
  }

  return createIndex(parentItem->index, 0, (void *) parentItem);
}

int TransitionModel::rowCount(const QModelIndex & parent) const
{
  const TransitionItem * parentItem;
  if (!parent.isValid())
  {
    parentItem = _rootProcess;
  }
  else
  {
    parentItem = static_cast<TransitionItem *>(parent.internalPointer());
  }
  return parentItem->count();
}

int TransitionModel::columnCount(const QModelIndex &) const
{
  return 1;
}

QVariant TransitionModel::data(const QModelIndex & index, int role) const
{
  if ((role != Qt::DisplayRole && role != Qt::EditRole) || !index.isValid())
  {
    return QVariant();
  }

  const TransitionItem * p = static_cast<TransitionItem *>(index.internalPointer());
  QString ptext = (role == Qt::DisplayRole ? p->process.displayText().toString() :
    p->process.fullText());

  // Top level process.
  if (p->cause == Event())
  {
    return ptext;
  }
  else
  {
    return QString("%1: %2").arg(p->cause.displayText(), ptext);
  }
}

bool TransitionModel::hasChildren(const QModelIndex & parent) const
{
  if (!parent.isValid())
  {
    return true;
  }
  else
  {
    return static_cast<TransitionItem *>(parent.internalPointer())->process.transitions().count();
  }
}
