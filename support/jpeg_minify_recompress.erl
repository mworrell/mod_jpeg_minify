%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019 Marc Worrell
%% @doc Recompresses with a quality depending on the quality of the uploaded file.

%% Copyright 2019 Marc Worrell
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

-module(jpeg_minify_recompress).

-export([
    minify/4,

    minify_all/1
]).

-include_lib("zotonic.hrl").

-define(PIX_Q99, 1000).
-define(PIX_Q50, 250000).

% Only perform minify_all on jpegs largers than this size.
-define(MINIFY_ALL_SIZE, 60000).

%% @doc Try to recompress the JPEG to smaller version
-spec minify( string()|binary(), non_neg_integer(), non_neg_integer(), non_neg_integer() ) ->
        {ok, file:filename(), non_neg_integer()} | keep.
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

%% @doc Minify all existing JPEGs in the database. The JPEGs are minified in-situ, the
%%      existing medium records are updated. This only works if S3 storage is disabled
%%      and the files are on the filesystem.
-spec minify_all( z:context() ) -> {ok, non_neg_integer()} | {error, term()}.
minify_all(Context0) ->
    Context = z_acl:sudo(Context0),
    case z_module_manager:active(mod_filestore, Context) of
        false ->
            % Fetch all JPEGs of considerable size, skip recent ones
            % as they might not yet be stable.
            Ms = z_db:q("
                select id, size, filename, width, height
                from medium
                where mime = 'image/jpeg'
                  and size > $1
                  and created < $2
                ",
                [
                    ?MINIFY_ALL_SIZE,
                    z_datetime:prev_hour( calendar:universal_time() )
                ],
                Context),
            Result = lists:map(
                fun({Id, Size, Filename, Width, Height}) ->
                    ArchivedFile = z_media_archive:abspath(Filename, Context),
                    case minify(ArchivedFile, Width, Height, Size) of
                        {ok, TmpFile, NewSize} ->
                            lager:info("JPEG Minify: replacing ~s (freeing up ~p bytes)",
                                      [Filename, Size - NewSize]),
                            case z_db:q("
                                update medium
                                set size = $2
                                where id = $1",
                                [ Id, NewSize ],
                                Context)
                            of
                                1 ->
                                    % Medium updated, replace the archived file
                                    % Flush the caches after the copy
                                    {ok, _} = file:copy(TmpFile, ArchivedFile),
                                    flush(Id, Context),
                                    file:delete(TmpFile),
                                    Size - NewSize;
                                0 ->
                                    % The medium was deleted whilst we were processing
                                    file:delete(TmpFile),
                                    0
                            end;
                        keep ->
                            0
                    end
                end,
                Ms),
            {ok, lists:sum(Result)};
        true ->
            {error, filestore_active}
    end.

flush(Id, Context) ->
    z_depcache:flush({medium, Id}, Context),
    z_depcache:flush(Id, Context).




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
