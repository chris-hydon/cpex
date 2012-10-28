#include "cspmsession.h"

#include "haskell/CSPM/Foreign_stub.h"

CSPMSession * CSPMSession::session;

void CSPMSession::free()
{
  if (CSPMSession::session != NULL)
  {
    delete CSPMSession::session;
    CSPMSession::session = NULL;
  }
}

CSPMSession * CSPMSession::getSession()
{
  if (CSPMSession::session == NULL)
  {
    CSPMSession::session = new CSPMSession();
  }

  return CSPMSession::session;
}

CSPMSession::CSPMSession()
{
  hs_session = cspm_session_create();
}

CSPMSession::~CSPMSession()
{
  if (file != NULL)
  {
    cspm_file_free(file);
  }
  cspm_session_free(hs_session);
}

int CSPMSession::loadFile(QString fileName)
{
  if (file != NULL)
  {
    cspm_file_free(file);
  }
  return cspm_session_load_file(hs_session, (void *) fileName.toStdWString().c_str(), &file);
}
