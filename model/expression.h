#ifndef EXPRESSION_H
#define EXPRESSION_H

#include <QSharedData>
#include <QString>
#include "model/process.h"

class ExpressionData;

class Expression
{
public:
  enum Mode { Probe, Inspect };

  Expression();
  Expression(const Expression &);
  Expression(const QString &);
  Expression(Process, Mode);
  ~Expression();
  bool isValid() const;
  Process process() const;
  Mode mode() const;
  QString text(bool, bool) const;
  const Expression & operator =(const Expression &);

private:
  QString modeString() const;

  QSharedDataPointer<ExpressionData> _d;
};

#endif // EXPRESSION_H
