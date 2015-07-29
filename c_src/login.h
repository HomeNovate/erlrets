//#ifndef LIBRETS_EXAMPLES_OPTIONS_H
//#define LIBRETS_EXAMPLES_OPTIONS_H

// Includes
#include "librets.h"
#include <string>
#include <fstream>
#include <iostream>

// Using, namepsaces
using namespace librets;
using std::string;
using std::endl;
using std::ofstream;
using std::ifstream;
using std::map;

/* Local vars */
std::string loginUrl = "http://127.0.0.1/rets/login";
std::string username = "John";
std::string password = "j.doe";
std::string userAgent = RetsSession::DEFAULT_USER_AGENT;
std::string userAgentPassword = "";
librets::UserAgentAuthType uaAuthType;
bool useHttpGet = false;
librets::RetsVersion retsVersion = RETS_1_8;
std::string sRetsVersion = "1.8";
bool useFullMetadata = false;
std::string brokerCode = "";
std::string savedMetadataTimestamp = "";
std::string proxyUrl = "";
std::string proxyPassword = "";
bool disableStreaming = false;
bool enableCaching = false;
std::string mEncoding = "ascii";

bool logging_in(map<string, string>& Options) 
{
    try
    {        
        RetsSessionPtr session(
            new RetsSession(Options.find("login_url")->second));
        session->Login(Options.find("username")->second, 
            Options.find("password")->second);
        return true;
    } 
    catch (RetsException & e) 
    {
        return false;
    } 
}
