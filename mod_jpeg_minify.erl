%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014-2019 Marc Worrell
%% @doc Minify JPEG files on upload. Recompresses with a quality depending on the quality of the uploaded file.

%% Copyright 2014-2019 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(mod_jpeg_minify).

-mod_title("JPEG Minify").
-mod_description("Minimize uploaded JPEGs by re-compressing with lower quality.").

-author("Marc Worrell <marc@worrell.nl>").

-export([
    observe_media_upload_preprocess/2
    ]).

-include_lib("zotonic.hrl").

-define(PIX_Q99, 1000).
-define(PIX_Q50, 250000).

observe_media_upload_preprocess(#media_upload_preprocess{mime="image/jpeg", file=File, medium=Medium} = PreProc, _Context) ->
    {width, Width} = proplists:lookup(width, Medium),
    {height, Height} = proplists:lookup(height, Medium),
    {size, Size} = proplists:lookup(size, Medium),
    case jpeg_minify_recompress:minify(File, Width, Height, Size) of
        {ok, NewFile, NewSize} ->
            lager:debug("JPEG minify: from ~p to ~p bytes", [Size, NewSize]),
            PreProc#media_upload_preprocess{
                file=NewFile,
                medium=[
                    {size, NewSize}
                    | proplists:delete(size, Medium) 
                ]
            };
        keep ->
            lager:debug("JPEG minify: keep old file"),
            undefined
    end;
observe_media_upload_preprocess(#media_upload_preprocess{}, _Context) ->
    undefined.

