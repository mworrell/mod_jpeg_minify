%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014 Marc Worrell
%% @doc Minify JPEG files on upload. Recompresses with a quality depending on the quality of the uploaded file.

%% Copyright 2014 Marc Worrell
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
    case minify(File, Width, Height, Size) of
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

%% @doc Try to recompress the JPEG to smaller version
minify(File, Width, Height, Size) when Width > 0, Height > 0 ->
    TmpFile = z_tempfile:new(".jpg"),
    Cmd = lists:flatten([
        "convert ",
        z_utils:os_filename(File), " ",
        " -quality ", integer_to_list(quality(Width * Height)), " ",
        sharpen_small(Width, Height),
        z_utils:os_filename(TmpFile)
    ]),
    _ = os:cmd(Cmd),
    case filelib:is_regular(TmpFile) of
        true ->
            case filelib:file_size(TmpFile) of 
                NewSize when NewSize < Size, NewSize > 0 ->
                    {ok, TmpFile, NewSize};
                _NewSize ->
                    file:delete(TmpFile),
                    keep 
            end;
        false ->
            keep
    end;
minify(_File, _Width, _Height, _Size) ->
    keep.


%% @doc Sharpen small images, this compensates the fuzzyness of the JPEG compression
sharpen_small(Width, Height) when Width < 400, Height < 400 ->
    " -unsharp 0.3x0.7 ";
sharpen_small(_Width, _Height) ->
    "".

% @doc Calculate the quality on a lineair scale between PIX_Q50 and PIX_Q99
quality(Pixels) when Pixels =< ?PIX_Q99 ->
    99;
quality(Pixels) when Pixels >= ?PIX_Q50 ->
    50;
quality(Pixels) ->
    99 - round(50 * (Pixels - ?PIX_Q99) / (?PIX_Q50 - ?PIX_Q99)).

