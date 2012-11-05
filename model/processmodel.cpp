#include "processmodel.h"

#include "cspmsession.h"
#include "programstate.h"

ProcessModel::ProcessModel(const Process * rootProcess, QObject * parent) :
  QAbstractItemModel(parent), _rootProcess(rootProcess)
{
}

ProcessModel::~ProcessModel()
{
  delete _rootProcess;
}

QModelIndex ProcessModel::index(int row, int column, const QModelIndex & parent) const
{
  if (!hasIndex(row, column, parent))
  {
    return QModelIndex();
  }

  const Process * parentItem;
  if (!parent.isValid())
  {
    parentItem = _rootProcess;
  }
  else
  {
    parentItem = static_cast<const Process *>(parent.internalPointer());
  }

  return createIndex(row, column, parentItem->transitions()->at(row)->second);
}

QModelIndex ProcessModel::parent(const QModelIndex & index) const
{
  if (!index.isValid())
  {
    return QModelIndex();
  }

  const Process * childItem = static_cast<const Process *>(index.internalPointer());
  const Process * parentItem = childItem->parent();

  if (parentItem == _rootProcess)
  {
    return QModelIndex();
  }

  return createIndex(parentItem->parentTransitionIndex(), 0, (void *) parentItem);
}

int ProcessModel::rowCount(const QModelIndex & parent) const
{
  const Process * parentItem;
  if (!parent.isValid())
  {
    parentItem = _rootProcess;
  }
  else
  {
    parentItem = static_cast<const Process *>(parent.internalPointer());
  }
  return parentItem->transitions()->count();
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

  const Process * proc = static_cast<const Process *>(index.internalPointer());
  return QString("%1: %2").arg(proc->causedBy()->displayText(), proc->displayText());
}
