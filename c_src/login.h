/*
 *
 * Copyright (C) 2015
 * This is a product for HomeNovate LLC
 * Authors: Jorge Garrido <zgbjgg@gmail.com>
 * All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 */

#include "librets.h"
#include <string>
#include <fstream>
#include <iostream>
#include "erl_rets_util.h"

using namespace librets;
using std::string;
using std::map;

/* Logging in agains RETS server */
bool logging_in(string loginUrl, 
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
    /* use HTTP get? */
    bool bUseHttpGet = string_to_boolean(useHttpGet);
    
    /* use Full Metadata? */
    bool bUseFullMetadata = string_to_boolean(useFullMetadata);

    /* enable streaming? */
    bool bDisableStreaming = string_to_boolean(disableStreaming);

    /* enable caching? */
    bool bEnableCaching = string_to_boolean(enableCaching);

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
        RetsSessionPtr session(new RetsSession(loginUrl));
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
        return true;
    } 
    catch (RetsException & e) 
    {
        return false;
    } 
}

