#include "inspectmodel.h"

#include <QIcon>
#include "cspmsession.h"

InspectItem::InspectItem(const Process & process, const InspectItem * parent,
  int index) : ProcessItem(process, parent, index)
{
}

InspectItem::InspectItem(const Process & process) : ProcessItem(Process(), NULL, 0)
{
  _next.append(new InspectItem(process, this, 0));
  _loaded = true;
}

const InspectItem * InspectItem::parent() const
{
  return static_cast<const InspectItem *>(_parentItem());
}

void InspectItem::_load() const
{
  QList<Process> components = process().components(true);
  for (int i = 0; i < components.count(); i++)
  {
    _next.append(new InspectItem(components.at(i), this, i));
  }
  _loaded = true;
}

InspectModel::InspectModel(const Process & rootProcess, QObject * parent) :
  QAbstractItemModel(parent), _rootProcess(new InspectItem(rootProcess))
{
}

InspectModel::~InspectModel()
{
  delete _rootProcess;
}

QModelIndex InspectModel::index(int row, int column, const QModelIndex & parent) const
{
  if (!hasIndex(row, column, parent))
  {
    return QModelIndex();
  }

  const InspectItem * parentItem;
  if (!parent.isValid())
  {
    parentItem = _rootProcess;
  }
  else
  {
    parentItem = static_cast<InspectItem *>(parent.internalPointer());
  }

  return createIndex(row, column, parentItem->next(row));
}

QModelIndex InspectModel::parent(const QModelIndex & index) const
{
  if (!index.isValid())
  {
    return QModelIndex();
  }

  const InspectItem * parentItem =
    static_cast<InspectItem *>(index.internalPointer())->parent();

  if (parentItem == _rootProcess)
  {
    return QModelIndex();
  }

  return createIndex(parentItem->index(), 0, (void *) parentItem);
}

int InspectModel::rowCount(const QModelIndex & parent) const
{
  const InspectItem * parentItem;
  if (!parent.isValid())
  {
    parentItem = _rootProcess;
  }
  else
  {
    parentItem = static_cast<InspectItem *>(parent.internalPointer());
  }
  return parentItem->count();
}

int InspectModel::columnCount(const QModelIndex &) const
{
  return 1;
}

QVariant InspectModel::data(const QModelIndex & index, int role) const
{
  static QIcon tick(":/images/tick.png");
  static QIcon cross(":/images/cross.png");

  if (!index.isValid())
  {
    return QVariant();
  }

  const InspectItem * p = static_cast<InspectItem *>(index.internalPointer());

  switch (role)
  {
    case Qt::DisplayRole:
      return p->process().displayText().toString();
    case Qt::EditRole:
      return p->process().fullText();
    case Qt::ToolTipRole:
    {
      QString tt = p->process().toolTip();
      if (tt == QString()) return QVariant();
      return tt;
    }
    case Qt::DecorationRole:
    {
      if (!_event.isValid())
      {
        return QVariant();
      }
      if (p->process().offersEvent(_event))
      {
        return tick;
      }
      return cross;
    }
    default:
      return QVariant();
  }
}

bool InspectModel::hasChildren(const QModelIndex & parent) const
{
  if (!parent.isValid())
  {
    return true;
  }
  else
  {
    return static_cast<InspectItem *>(parent.internalPointer())->count();
  }
}

void InspectModel::eventTextChanged(const QString & newText)
{
  if (newText == QString())
  {
    _event = Event();
  }
  else
  {
    _event = _rootProcess->next(0)->process().session()->stringToEvent(newText);
  }

  _dataChanged(index(0, 0));
  emit eventChanged(_event);
}

void InspectModel::_dataChanged(const QModelIndex & idx)
{
  InspectItem * item = static_cast<InspectItem *>(idx.internalPointer());
  if (item->isLoaded())
  {
    emit dataChanged(index(0, 0, idx), index(item->count() - 1, 0, idx));
    for (int i = 0; i < item->count(); i++)
    {
      _dataChanged(index(i, 0, idx));
    }
  }
}
