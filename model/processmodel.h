#ifndef PROCESSMODEL_H
#define PROCESSMODEL_H

#include <QAbstractItemModel>

class ProcessModel : public QAbstractItemModel
{
public:
  ProcessModel(QObject * parent = 0);
  QModelIndex index(int row, int column, const QModelIndex & parent = QModelIndex()) const;
  QModelIndex parent(const QModelIndex & index) const;
  int rowCount(const QModelIndex & parent = QModelIndex()) const;
  int columnCount(const QModelIndex & parent = QModelIndex()) const;
  QVariant data(const QModelIndex & index, int role = Qt::DisplayRole) const;

  // Infinite data structure: must lazily load data as the view requires it.
  bool hasChildren(const QModelIndex & parent = QModelIndex()) const;
  bool canFetchMore(const QModelIndex & parent) const;
  void fetchMore(const QModelIndex & parent);
};

#endif // PROCESSMODEL_H
