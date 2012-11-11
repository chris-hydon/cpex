#include "sessionmodel.h"

#include "cspmsession.h"
#include "mainwindow.h"
#include "programstate.h"
#include "ui_mainwindow.h"
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
    _procs = new QList<SessionItem *>();
  }

  SessionItem(const SessionItem * parent, const QString & displayStr) :
    _type(ProcCall), _session(NULL), _parent(parent), _displayStr(displayStr)
  {
    _procs = NULL;
  }

  ~SessionItem()
  {
    if (_procs != NULL)
    {
      while (!_procs->isEmpty())
      {
        delete _procs->takeFirst();
      }
    }
  }

  QList<SessionItem *> * _procs;
  const Type _type;
  CSPMSession * _session;
  const SessionItem * _parent;
  const QString _displayStr;
};

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
    return QFileInfo(item->_session->fileName()).fileName();
  }
}

void SessionModel::sessionLoaded(CSPMSession * session)
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

  if (!(*session == *ProgramState::currentSession()))
  {
    ProgramState::setCurrentSession(session);
  }

  if (item->_type == SessionItem::ProcCall)
  {
    MainWindow::getUi()->qleExpression->setText(item->_displayStr);
    if (!item->_displayStr.contains('_'))
    {
      MainWindow::getUi()->qtvExplorer->loadInitialState();
    }
  }
}
