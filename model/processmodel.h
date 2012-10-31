#ifndef PROCESSMODEL_H
#define PROCESSMODEL_H

#include "process.h"
#include <QAbstractItemModel>

class ProcessModel : public QAbstractItemModel
{
public:
  ProcessModel(const QString & rootExpression, QObject * parent = 0);
  ~ProcessModel();
  QModelIndex index(int row, int column, const QModelIndex & parent = QModelIndex()) const;
  QModelIndex parent(const QModelIndex & index) const;
  int rowCount(const QModelIndex & parent = QModelIndex()) const;
  int columnCount(const QModelIndex & parent = QModelIndex()) const;
  QVariant data(const QModelIndex & index, int role = Qt::DisplayRole) const;

private:
  const Process * _rootProcess;
};

#endif // PROCESSMODEL_H
