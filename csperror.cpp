#include "csperror.h"

CSPError::CSPError(const CSPMSession * session, ErrorType type,
  const QString & message) : _sessionName(session->displayName()), _type(type),
  _message(message)
{
}

QString CSPError::sessionName() const
{
  return _sessionName;
}

CSPError::ErrorType CSPError::type() const
{
  return _type;
}

QString CSPError::message() const
{
  return _message;
}
