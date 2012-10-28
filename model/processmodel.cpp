#include "processmodel.h"

ProcessModel::ProcessModel(QString rootExpression, QObject * parent) : QAbstractItemModel(parent)
{
}

QModelIndex ProcessModel::index(int row, int column, const QModelIndex & parent) const
{
  return parent;
}

QModelIndex ProcessModel::parent(const QModelIndex & index) const
{
  return index;
}

int ProcessModel::rowCount(const QModelIndex & parent) const
{
  // TODO
  return 0;
}

int ProcessModel::columnCount(const QModelIndex & parent) const
{
  return 1;
}

QVariant ProcessModel::data(const QModelIndex & index, int role) const
{
  return "";
}

// Infinite data structure: must lazily load data as the view requires it.
bool ProcessModel::hasChildren(const QModelIndex & parent) const
{
  return rowCount(parent) > 0 || canFetchMore(parent);
}

bool ProcessModel::canFetchMore(const QModelIndex & parent) const
{
  bool fetched = true;
  if (fetched)
  {
    return false;
  }

  // TODO
  fetched = true;
  return true;
}

void ProcessModel::fetchMore(const QModelIndex & parent)
{
}
