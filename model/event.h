#ifndef EVENT_H
#define EVENT_H

#include <QString>

class Event
{
public:
  enum Type
  {
    User, Tau, Tick
  };

  Event(void * _hsPtr);
  ~Event();
  QString displayText() const;
  bool operator ==(const Event & other) const;
  Type type() const;

private:
  void * _hsPtr;
  mutable QString _displayText;
  mutable Type _type;

  void _lazyLoad() const;
};

#endif // EVENT_H
