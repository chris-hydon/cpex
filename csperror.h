#ifndef CSPERROR_H
#define CSPERROR_H

#include "cspmsession.h"

class CSPError
{
public:
  enum ErrorType { Error, Warning };

  CSPError(const CSPMSession *, ErrorType, const QString &);
  QString sessionName() const;
  ErrorType type() const;
  QString message() const;

private:
  QString _sessionName;
  ErrorType _type;
  QString _message;
};

#endif // CSPERROR_H
