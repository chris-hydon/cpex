#ifndef EVENT_H
#define EVENT_H

#include <QString>

class Event
{
public:
  Event(void * _hsPtr);
  ~Event();
  QString displayText() const;
  bool operator ==(const Event & other) const;

private:
  void * _hsPtr;
  mutable QString _displayText;
  mutable unsigned char _type;

  void _lazyLoad() const;
};

#endif // EVENT_H
