#include "librets.h"
#include <string>
#include <fstream>

using std::string;

class Login
{
    public:
        Login();

        librets::RetsSessionPtr OpenSession(string loginUrl,
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
             string encoding);
};
