#ifndef TAB_H
#define TAB_H

#include <QLineEdit>
#include <QString>
#include <QWidget>
#include "model/expression.h"

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

private:
  void setupProbe(const Expression &);
  void setupInspector(const Expression &);

  Q_DISABLE_COPY(Tab)
  Expression _expression;
};

#endif // TAB_H
