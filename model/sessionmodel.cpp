#include "sessionmodel.h"

#include "widget/tab.h"
#include "cspmsession.h"
#include "mainwindow.h"
#include "programstate.h"
#include <QFileInfo>
#include <QStringList>

class SessionItem
{
public:
  enum Type
  {
    Session, ProcCall
  };

  SessionItem(CSPMSession * session) : _type(Session), _session(session),
    _parent(NULL)
  {
  }

  SessionItem(SessionItem * parent, const QString & displayStr) :
    _type(ProcCall), _session(NULL), _parent(parent), _displayStr(displayStr)
  {
  }

  ~SessionItem()
  {
    while (!_procs.isEmpty())
    {
      delete _procs.takeFirst();
    }
  }

  QList<SessionItem *> _procs;
  const Type _type;
  CSPMSession * _session;
  SessionItem * _parent;
  const QString _displayStr;
};

SessionModel::SessionModel(QObject * parent) : QAbstractItemModel(parent)
{
}

SessionModel::~SessionModel()
{
  while (!_sessions.isEmpty())
  {
    delete _sessions.takeFirst();
  }
}

QModelIndex SessionModel::index(int row, int column, const QModelIndex & parent) const
{
  if (!hasIndex(row, column, parent))
  {
    return QModelIndex();
  }

  if (!parent.isValid())
  {
    return createIndex(row, column, (void *) _sessions.at(row));
  }

  SessionItem * parentItem = static_cast<SessionItem *>(parent.internalPointer());
  return createIndex(row, column, parentItem->_procs.at(row));
}

QModelIndex SessionModel::parent(const QModelIndex & index) const
{
  if (!index.isValid())
  {
    return QModelIndex();
  }

  SessionItem * item = static_cast<SessionItem *>(index.internalPointer());
  if (item->_type == SessionItem::ProcCall)
  {
    int row = _sessions.indexOf(item->_parent);
    return createIndex(row, 0, (void *) item->_parent);
  }
  return QModelIndex();
}

int SessionModel::rowCount(const QModelIndex & parent) const
{
  if (!parent.isValid())
  {
    return _sessions.count();
  }

  SessionItem * parentItem = static_cast<SessionItem *>(parent.internalPointer());
  if (parentItem->_type == SessionItem::Session)
  {
    return parentItem->_procs.count();
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

  SessionItem * item = static_cast<SessionItem *>(index.internalPointer());
  if (item->_type == SessionItem::ProcCall)
  {
    return item->_displayStr;
  }
  else
  {
    return item->_session->displayName();
  }
}

void SessionModel::addSession(CSPMSession * session)
{
  emit layoutAboutToBeChanged();
  SessionItem * sessionItem = new SessionItem(session);
  _sessions.append(sessionItem);
  QStringList procs = session->procCallNames();
  QString proc;
  foreach (proc, procs)
  {
    sessionItem->_procs.append(new SessionItem(sessionItem, proc));
  }

  emit layoutChanged();
}

void SessionModel::reloadSession(CSPMSession * session)
{
  for (int i = 0; i < _sessions.count(); i++)
  {
    SessionItem * sessionItem = _sessions[i];
    if (sessionItem->_session == session)
    {
      emit layoutAboutToBeChanged();

      while (!sessionItem->_procs.isEmpty())
      {
        delete sessionItem->_procs.takeFirst();
      }

      QStringList procs = session->procCallNames();
      foreach (QString proc, procs)
      {
        sessionItem->_procs.append(new SessionItem(sessionItem, proc));
      }

      emit layoutChanged();
      break;
    }
  }
}

void SessionModel::removeSession(CSPMSession * session)
{
  for (int i = 0; i < _sessions.count(); i++)
  {
    SessionItem * sessionItem = _sessions[i];
    if (sessionItem->_session == session)
    {
      emit layoutAboutToBeChanged();
      _sessions.removeAt(i);
      delete sessionItem;
      emit layoutChanged();
      break;
    }
  }
}

void SessionModel::removeAllSessions()
{
  emit layoutAboutToBeChanged();
  while (!_sessions.isEmpty())
  {
    delete _sessions.takeFirst();
  }
  emit layoutChanged();
}

void SessionModel::itemActivated(const QModelIndex & index)
{
  const SessionItem * item = static_cast<const SessionItem *>(index.internalPointer());
  CSPMSession * session = NULL;
  if (item->_type == SessionItem::ProcCall)
  {
    session = item->_parent->_session;
  }
  else
  {
    session = item->_session;
  }

  if (!(session == ProgramState::currentSession()))
  {
    MainWindow::get()->setCurrentSession(session);
  }

  if (item->_type == SessionItem::ProcCall)
  {
    if (item->_displayStr.contains('_'))
    {
    }
    else
    {
      MainWindow::get()->setTabFromExpression(Expression(item->_displayStr));
    }
  }
}
