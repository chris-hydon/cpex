#ifndef TAB_H
#define TAB_H

#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QModelIndex>
#include <QString>
#include <QWidget>
#include "model/expression.h"
#include "widget/processtree.h"

class Tab : public QWidget
{
  Q_OBJECT
public:
  explicit Tab(QWidget * parent = 0);
  Expression expression() const;
  void setExpression(const Expression &);
  void updateExprBox();
  QLineEdit * exprBox;

signals:

public slots:
  void displayEventDetails(const QModelIndex & = QModelIndex());

private:
  void setupProbe(const Expression &);
  void setupInspector(const Expression &);

  Q_DISABLE_COPY(Tab)
  Expression _expression;
  ProcessTree * _tree;
  QLabel * _inspectorWhyDetails;
  QLineEdit * _inspectorWhy;
};

#endif // TAB_H
