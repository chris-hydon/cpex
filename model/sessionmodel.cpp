#include "sessionmodel.h"

#include "cspmsession.h"
#include "programstate.h"
#include <QStringList>

SessionModel::SessionModel(QObject * parent) : QAbstractItemModel(parent)
{
  _sessions = new QList<const SessionItem *>();
}

SessionModel::~SessionModel()
{
  delete _sessions;
}

QModelIndex SessionModel::index(int row, int column, const QModelIndex & parent) const
{
  if (!hasIndex(row, column, parent))
  {
    return QModelIndex();
  }

  if (!parent.isValid())
  {
    return createIndex(row, column, (void *) _sessions->at(row));
  }

  SessionItem * parentItem = static_cast<SessionItem *>(parent.internalPointer());
  return createIndex(row, column, parentItem->_procs->at(row));
}

QModelIndex SessionModel::parent(const QModelIndex & index) const
{
  if (!index.isValid())
  {
    return QModelIndex();
  }

  const SessionItem * item = static_cast<const SessionItem *>(index.internalPointer());
  if (item->_type == SessionItem::ProcCall)
  {
    int row = _sessions->indexOf(item->_parent);
    return createIndex(row, 0, (void *) item->_parent);
  }
  return QModelIndex();
}

int SessionModel::rowCount(const QModelIndex & parent) const
{
  if (!parent.isValid())
  {
    return _sessions->count();
  }

  const SessionItem * parentItem = static_cast<const SessionItem *>(parent.internalPointer());
  if (parentItem->_type == SessionItem::Session)
  {
    return parentItem->_procs->count();
  }

  return 0;
}

int SessionModel::columnCount(const QModelIndex &) const
{
  return 1;
}

QVariant SessionModel::data(const QModelIndex & index, int role) const
{
  if (role != Qt::DisplayRole || !index.isValid())
  {
    return QVariant();
  }

  const SessionItem * item = static_cast<const SessionItem *>(index.internalPointer());
  if (item->_type == SessionItem::ProcCall)
  {
    return item->_displayStr;
  }
  else
  {
    return item->_session->fileName();
  }
}

void SessionModel::sessionLoaded(const CSPMSession * session)
{
  emit layoutAboutToBeChanged();
  const SessionItem * sessionItem = new SessionItem(session);
  _sessions->append(sessionItem);
  QStringList procs = session->procCallNames();
  QString proc;
  foreach (proc, procs)
  {
    sessionItem->_procs->append(new SessionItem(sessionItem, proc));
  }

  emit layoutChanged();
}
