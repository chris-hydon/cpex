#ifndef SESSIONMODEL_H
#define SESSIONMODEL_H

#include "cspmsession.h"
#include <QAbstractItemModel>

class SessionModel : public QAbstractItemModel
{
  Q_OBJECT
public:
  SessionModel(QObject * parent = 0);
  ~SessionModel();
  QModelIndex index(int row, int column, const QModelIndex & parent = QModelIndex()) const;
  QModelIndex parent(const QModelIndex & index) const;
  int rowCount(const QModelIndex & parent = QModelIndex()) const;
  int columnCount(const QModelIndex & parent = QModelIndex()) const;
  QVariant data(const QModelIndex & index, int role = Qt::DisplayRole) const;

public slots:
  void sessionLoaded(const CSPMSession * session);

private:
  struct SessionItem
  {
    enum Type
    {
      Session, ProcCall
    };

    SessionItem(const CSPMSession * session) : _type(Session), _session(session),
      _parent(NULL)
    {
      _procs = new QList<SessionItem *>();
      QStringList procs = session->procCallNames();
      QString proc;
      foreach (proc, procs)
      {
        _procs->append(new SessionItem(this, proc));
      }
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
    const CSPMSession * _session;
    const SessionItem * _parent;
    const QString _displayStr;
  };

private:
  QList<const SessionItem *> * _sessions;
};

#endif // SESSIONMODEL_H
