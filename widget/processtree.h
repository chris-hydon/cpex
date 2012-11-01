#ifndef PROCESSTREE_H
#define PROCESSTREE_H

#include "model/processmodel.h"

#include <QTreeView>

class ProcessTree : public QTreeView
{
  Q_OBJECT
public:
  explicit ProcessTree(QWidget *parent = 0);
  
signals:
  
public slots:
  void loadInitialState();

private:
  ProcessModel * _model;
};

#endif // PROCESSTREE_H
