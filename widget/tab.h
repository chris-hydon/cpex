#ifndef TAB_H
#define TAB_H

#include <QLineEdit>
#include <QString>
#include <QWidget>

class Tab : public QWidget
{
  Q_OBJECT
public:
  explicit Tab(QWidget * parent = 0);
  QString expression() const;
  bool setExpression(const QString &);
  QLineEdit * exprBox;

signals:

public slots:

private:
  Q_DISABLE_COPY(Tab)
  QString _expression;
};

#endif // TAB_H
