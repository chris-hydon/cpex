#ifndef DISPLAYSTRING_H
#define DISPLAYSTRING_H

#include <QString>

class DisplayString
{
public:
  DisplayString();
  DisplayString(const DisplayString &);
  explicit DisplayString(const QString &);
  QString toString() const;
  bool operator ==(const DisplayString &);

private:
  QString _disp;
};

#endif // DISPLAYSTRING_H
