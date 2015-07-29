%%
%% Copyright (C) 2015
%% This is a product for HomeNovate LLC
%% Authors: Jorge Garrido <zgbjgg@gmail.com>
%% All rights reserved.
%%
%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%%

-define(DRIVER_SO_NAME, "erl_rets").

%% Queries
-define(BUILD_LOGIN_QUERY(Url, Username, Password, Options), 
    [{url_login, Url},
     {username, Username},
     {password, Password} | Options]).
