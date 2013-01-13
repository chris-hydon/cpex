#ifndef EVENT_H
#define EVENT_H

#include <QExplicitlySharedDataPointer>
#include <QHash>
#include <QString>

class EventData;

class Event
{
public:
  enum Type
  {
    User, Tau, Tick
  };

  Event();
  Event(const Event & other);
  Event(void * _hsPtr);
  ~Event();
  QString displayText() const;
  Type type() const;
  bool operator ==(const Event & other) const;
  Event & operator =(const Event & other);

private:
  QExplicitlySharedDataPointer<EventData> _d;

  void _lazyLoad() const;
};

inline uint qHash(const Event & e)
{
  return qHash(e.displayText());
}

#endif // EVENT_H
