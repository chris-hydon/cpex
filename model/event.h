#ifndef EVENT_H
#define EVENT_H

#include <QExplicitlySharedDataPointer>
#include <QHash>
#include <QString>

class CSPMSession;
class EventData;

class Event
{
public:
  enum Type
  {
    User, Tau, Tick
  };

  Event();
  Event(const Event &);
  Event(void *);
  ~Event();
  bool isValid() const;
  QString displayText() const;
  Type type() const;
  bool operator ==(const Event &) const;
  Event & operator =(const Event &);

private:
  QExplicitlySharedDataPointer<EventData> _d;

  void _lazyLoad() const;
};

inline uint qHash(const Event & e)
{
  return qHash(e.displayText());
}

#endif // EVENT_H
