#ifndef PROCESSTREE_H
#define PROCESSTREE_H

#include "model/processmodel.h"

#include <QTreeView>

class ProcessTree : public QTreeView
{
  Q_OBJECT
public:
  explicit ProcessTree(QWidget * parent = 0);

signals:
  void itemSelected(const QModelIndex & index);

public slots:
  void loadInitialState();
  void showContextMenu(const QPoint &);

protected slots:
  void selectionChanged();

private:
  ProcessModel * _model;
};

#endif // PROCESSTREE_H
