#include "processmodel.h"

#include "cspmsession.h"
#include "programstate.h"

ProcessItem::ProcessItem(const Process * process, const ProcessItem * parent,
  const Event * cause, int index) : process(process), parent(parent),
  cause(cause), index(index)
{
  _next = new QList<ProcessItem *>();
}

ProcessItem::~ProcessItem()
{
  while (!_next->isEmpty())
  {
    delete _next->takeFirst();
  }
  delete _next;
}

ProcessItem * ProcessItem::next(int index) const
{
  return _next->at(index);
}

int ProcessItem::count() const
{
  return _next->count();
}

bool ProcessItem::canFetchMore() const
{
  return process->transitions()->count() > count();
}

void ProcessItem::fetchMore(int toFetch)
{
  int got = count();

  QList<QPair<Event *, Process *> *> * transitions = process->transitions();
  QPair<Event *, Process *> * pair;
  for (int i = got; i < got + toFetch; i++)
  {
    pair = transitions->at(i);
    _next->append(new ProcessItem(pair->second, this, pair->first, i));
  }
}

ProcessModel::ProcessModel(const Process * rootProcess, QObject * parent) :
  QAbstractItemModel(parent), _rootProcess(new ProcessItem(rootProcess))
{
}

ProcessModel::~ProcessModel()
{
  delete _rootProcess->process;
  delete _rootProcess;
}

QModelIndex ProcessModel::index(int row, int column, const QModelIndex & parent) const
{
  if (!hasIndex(row, column, parent))
  {
    return QModelIndex();
  }

  const ProcessItem * parentItem;
  if (!parent.isValid())
  {
    parentItem = _rootProcess;
  }
  else
  {
    parentItem = static_cast<ProcessItem *>(parent.internalPointer());
  }

  return createIndex(row, column, parentItem->next(row));
}

QModelIndex ProcessModel::parent(const QModelIndex & index) const
{
  if (!index.isValid())
  {
    return QModelIndex();
  }

  const ProcessItem * parentItem =
    static_cast<ProcessItem *>(index.internalPointer())->parent;

  if (parentItem == _rootProcess)
  {
    return QModelIndex();
  }

  return createIndex(parentItem->index, 0, (void *) parentItem);
}

int ProcessModel::rowCount(const QModelIndex & parent) const
{
  const ProcessItem * parentItem;
  if (!parent.isValid())
  {
    parentItem = _rootProcess;
  }
  else
  {
    parentItem = static_cast<ProcessItem *>(parent.internalPointer());
  }
  return parentItem->count();
}

int ProcessModel::columnCount(const QModelIndex &) const
{
  return 1;
}

QVariant ProcessModel::data(const QModelIndex & index, int role) const
{
  if (role != Qt::DisplayRole || !index.isValid())
  {
    return QVariant();
  }

  const ProcessItem * p = static_cast<ProcessItem *>(index.internalPointer());
  QString display = QString("%1: %2").arg(p->cause->displayText(), p->process->displayText());
  display.truncate(100);
  return display;
}

bool ProcessModel::canFetchMore(const QModelIndex & parent) const
{
  const ProcessItem * parentItem;
  if (!parent.isValid())
  {
    parentItem = _rootProcess;
  }
  else
  {
    parentItem = static_cast<ProcessItem *>(parent.internalPointer());
  }

  return parentItem->canFetchMore();
}

void ProcessModel::fetchMore(const QModelIndex & parent)
{
  ProcessItem * parentItem;
  if (!parent.isValid())
  {
    parentItem = _rootProcess;
  }
  else
  {
    parentItem = static_cast<ProcessItem *>(parent.internalPointer());
  }

  int got = parentItem->count();
  int toFetch = qMin(10, parentItem->process->transitions()->count() - got);
  beginInsertRows(parent, got, got + toFetch - 1);
  parentItem->fetchMore(toFetch);
  endInsertRows();
}

bool ProcessModel::hasChildren(const QModelIndex & parent) const
{
  const ProcessItem * parentItem;
  if (!parent.isValid())
  {
    parentItem = _rootProcess;
  }
  else
  {
    parentItem = static_cast<ProcessItem *>(parent.internalPointer());
  }
  return parentItem->process->transitions()->count();
}
