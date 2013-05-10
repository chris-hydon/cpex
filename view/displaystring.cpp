#include "displaystring.h"

#include <QRegExp>
#include <QStringList>
#include <QTextDocument>

const QString stylesheet = QString("<style type=\"text/css\">") +
  ".brac0 { color: #c00; }" +
  ".brac1 { color: #00c; }" +
  ".brac2 { color: #0c0; }" +
  ".brac3 { color: #990; }" +
  ".brac4 { color: #909; }" +
  ".brac5 { color: #099; }" +
  "</style>";

DisplayString::DisplayString()
{
}

DisplayString::DisplayString(const DisplayString & other) : _disp(other._disp)
{
}

DisplayString::DisplayString(const QString & raw)
{
  _disp = Qt::escape(raw);

  // Match any CSPM open or close bracket. This is a little unclear here because
  // of all the backslashes, but matches (, [, [|, [[, [+, /+, { and their respective
  // closing brackets. |> also counts as a closing bracket ("[|a|>" is CSPM for
  // exception on events a). [> is also matched, and is a special case: neither
  // opening nor closing.
  QRegExp brackets(
    "(\\(|\\[&gt;|\\[[\\[|+]?|/\\+|\\{|\\}|\\+\\\\|[\\]|+]?\\]|\\)|\\|&gt;)");
  QStringList open;
  open << "(" << "[" << "{" << "[[" << "[|" << "[+" << "/+";
  QString span("<span class=\"brac%1\">%2</span>");
  int i = 0;
  int level = 0;
  while ((i = brackets.indexIn(_disp, i)) != -1)
  {
    QString m = brackets.capturedTexts()[0];
    bool isOpen = open.contains(m);
    if (!isOpen && m != "[&gt;") level--;

    _disp.replace(i, brackets.matchedLength(), span.arg(QString::number(level % 6), m));
    i += 27 + brackets.matchedLength(); // Length of QString span after args are replaced.

    if (isOpen) level++;
  }

  _disp.prepend(stylesheet);
}

QString DisplayString::toString() const
{
  return _disp;
}

bool DisplayString::operator ==(const DisplayString & other)
{
  return _disp == other._disp;
}
