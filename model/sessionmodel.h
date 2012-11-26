#ifndef SESSIONMODEL_H
#define SESSIONMODEL_H

#include "cspmsession.h"
#include <QAbstractItemModel>

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

public slots:
  void sessionLoaded(CSPMSession * session);
  void itemActivated(const QModelIndex & index);

private:
  QList<const SessionItem *> _sessions;
};

#endif // SESSIONMODEL_H
