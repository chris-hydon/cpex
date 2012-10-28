#ifndef CSPMSESSION_H
#define CSPMSESSION_H

#include <QString>

class CSPMSession
{
public:
  static CSPMSession * getSession();
  static void free();

  int loadFile(QString fileName);

private:
  static CSPMSession * session;

  CSPMSession();
  ~CSPMSession();
  void * hs_session;
  void * file;
};

#endif // CSPMSESSION_H
