#ifndef SESSIONMODEL_H
#define SESSIONMODEL_H

#include <QAbstractItemModel>
#include "cspmsession.h"
#include "model/expression.h"

class SessionItem;

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

  void addSession(CSPMSession *);
  void reloadSession(CSPMSession *);
  void removeSession(CSPMSession *);
  void removeAllSessions();

  QString getProcInputString(const QModelIndex & index) const;
  CSPMSession * getSession(const QModelIndex & index) const;

public slots:
  void itemActivated(const QModelIndex & index);

signals:
  void expressionActivated(const Expression &);
  void sessionSelected(CSPMSession *);

private:
  QList<SessionItem *> _sessions;
};

#endif // SESSIONMODEL_H
