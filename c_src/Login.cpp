#include "Login.h"
#include "librets.h"
#include <iostream>
#include <fstream>

using namespace librets;
using std::string;
using std::ofstream;
using std::ifstream;

Login::Login() 
{

}

RetsSessionPtr Login::OpenSession(string loginUrl,
    string username,
    string password,
    string userAgent,
    string userAgentPassword,
    string useHttpGet,
    string sRetsVersion,
    string useFullMetadata,
    string brokerCode,
    string saveMetadataTimestamp,
    string proxyUrl,
    string proxyPassword,
    string disableStreaming,
    string enableCaching,
    string encoding)
{
    RetsSessionPtr session(new RetsSession(loginUrl));

    /* use HTTP get? */
    bool bUseHttpGet = (useHttpGet=="true")?true:false;

    /* use Full Metadata? */
    bool bUseFullMetadata = (useFullMetadata=="true")?true:false;

    /* enable streaming? */
    bool bDisableStreaming = (disableStreaming=="true")?true:false;

    /* enable caching? */
    bool bEnableCaching = (enableCaching=="true")?true:false;

    /* type of encoding? */
    EncodingType encodingType;
    if ( encoding == "ascii" )
    {
        encodingType = RETS_XML_ISO_ENCODING;
    } else if ( encoding == "utf8")
    {
        encodingType = RETS_XML_UTF8_ENCODING;
    } else {
        encodingType = RETS_XML_DEFAULT_ENCODING;
    }

    /* which one Rets Version? */
    librets::RetsVersion retsVersion;
    if ( sRetsVersion == "1_7" )
    {
       retsVersion = RETS_1_7;
    } else {
       retsVersion = RETS_1_8;
    }

    try
    {
        session->SetUserAgent(userAgent);
        session->UseHttpGet(bUseHttpGet);
        session->SetRetsVersion(retsVersion);
        session->SetIncrementalMetadata(bUseFullMetadata);
        session->SetUserAgentPassword(userAgentPassword);

        unsigned int flags = 0;
        if (bDisableStreaming)
        {
            flags |= RetsSession::MODE_NO_STREAM;
        }
        if (bEnableCaching)
        {
            flags |= RetsSession::MODE_CACHE;
        }
        flags |= RetsSession::MODE_NO_SSL_VERIFY;
        session->SetModeFlags(flags);

        if (!proxyUrl.empty())
        {
            session->SetProxy(proxyUrl,proxyPassword);
        }

        session->SetDefaultEncoding(encodingType);

        session->Login(username, password, brokerCode, saveMetadataTimestamp);
        return session;
    }
    catch (RetsException & e)
    {
        return session;
    }
}
