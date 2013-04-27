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
  enum ListStyle
  {
    Set, SetOrSingle, Comma, CommaOr, CommaAnd
  };

  Event();
  Event(const Event &);
  ~Event();
  bool isValid() const;
  QString displayText() const;
  Type type() const;
  bool operator ==(const Event &) const;
  Event & operator =(const Event &);

  // Converts a list of events to its string representation in the ListStyle form.
  //
  // Set: CSP set notation, shortened if possible. e.g. if foo is a channel with one
  //      integer parameter ranging between 0 and 2 then {foo.0, foo.1, foo.2} is
  //      shortened to {|foo|}, while {foo.0, foo.1} will be presented as is.
  // SetOrSingle: As Set, but singleton sets are presented without braces.
  // Comma: A comma-separated list.
  // CommaOr: A comma-separated list with "or" separating the last element instead of
  //          a comma.
  // CommaAnd: A comma-separated list with "and" separating the last element instead
  //           of a comma.
  static QString displayEventList(const CSPMSession *, QList<Event>,
    ListStyle = Event::Set);
  static Event create(const CSPMSession *, void *);

private:
  QExplicitlySharedDataPointer<EventData> _d;

  Event(void *);
  void _lazyLoad() const;
};

inline uint qHash(const Event & e)
{
  return qHash(e.displayText());
}

#endif // EVENT_H
