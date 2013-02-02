#include "expression.h"

#include <QRegExp>
#include "programstate.h"

class ExpressionData : public QSharedData
{
public:
  QString expr;
  Process rootProc;
  const CSPMSession * session;
  Expression::Mode mode;
};

Expression::Expression()
{
}

Expression::Expression(const Expression & other) : _d(other._d)
{
}

Expression::Expression(const QString & expr)
{
  _d = new ExpressionData();

  // Expressions are of the form "[filename:][type:]cspstring" where filename is the
  // name of the opened file to use (default to the current session), type is the
  // expression type, and cspstring is a string of CSP to evaluate. Possible types
  // are:
  // - probe (default)
  // - inspect
  QStringList sessions;
  foreach (CSPMSession * sess, ProgramState::getSessions())
  {
    sessions << sess->displayName();
  }
  QRegExp regex("(?:(" + sessions.join("|") + "):|)(?:(probe|inspect):|)(.*)");

  // This will always return true. More interesting is whether it matched a session
  // and/or type.
  regex.exactMatch(expr);
  QStringList caps = regex.capturedTexts();

  if (caps[1] == QString())
  {
    _d->session = ProgramState::currentSession();
  }
  else
  {
    _d->session = ProgramState::getSessions().value(caps[1]);
  }

  if (caps[2] == "inspect")
  {
    _d->mode = Inspect;
  }
  else
  {
    _d->mode = Probe;
  }

  // Compile the expression. If that fails, make this expression invalid.
  // TODO: Error messages in the status bar.
  _d->rootProc = ProgramState::currentSession()->compileExpression(caps[3]);
  if (!_d->rootProc.isValid())
  {
    _d->expr = QString();
  }
  else
  {
    _d->expr = _d->rootProc.fullText();
  }
}

Expression::Expression(Process proc, Mode mode)
{
  if (proc.isValid())
  {
    _d = new ExpressionData();
    _d->rootProc = proc;
    _d->mode = mode;
    _d->session = proc.session();
    _d->expr = proc.fullText();
  }
}

Expression::~Expression()
{
}

bool Expression::isValid() const
{
  return _d && _d->expr != QString();
}

Process Expression::process() const
{
  return _d->rootProc;
}

Expression::Mode Expression::mode() const
{
  return _d->mode;
}

QString Expression::text(bool withSess, bool withMode) const
{
  QString extra;
  if (withSess) extra += _d->session->displayName() + ":";
  if (withMode) extra += modeString();
  return extra + _d->expr;
}

QString Expression::modeString() const
{
  switch (_d->mode)
  {
    case Inspect:
      return "inspect:";
    default:
      return "probe:";
  }
}

const Expression & Expression::operator =(const Expression & other)
{
  _d = other._d;
  return *this;
}
