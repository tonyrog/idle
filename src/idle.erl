%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Simple IDLE shell for erlang
%%%    start with -noinput
%%% @end
%%% Created :  2 Jan 2022 by Tony Rogvall <tony@rogvall.se>

-module(idle).
-export([start/0]).
-export([expand/1]).

start() ->
    TTY = tty:open(),
    init(TTY),
    loop(TTY, 1, #{}, false, erl_eval:new_bindings()),
    tty:close(TTY),
    halt(0).

init(TTY) ->
    Geom=tty:get_tty_geometry(TTY),
    tty:set_unicode_state(TTY, true),
    tty:csi(TTY, off),
    tty:outputf(TTY, "Erlang ~s IDLE 1.0\n\r", [erlang:system_info(version)]),
    Geom.

loop(TTY, I, Forms, Compile0, Bindings) ->
    case parse(TTY, I, []) of
	eof -> 
	    eof;
	error -> 
	    loop(TTY, I+1, Forms, Compile0, Bindings);
	{form, Form = {function,_Ln,Name,Arity,_Clauses}} ->
	    case lint_function(Form) of
		{ok,Ws} ->
		    display_error(TTY, red, Ws),
		    Forms1 = Forms#{ {Name,Arity} => Form },
		    loop(TTY, I+1, Forms1, true, Bindings);
		{error,Es,Ws} ->
		    display_error(TTY, red, Es),
		    display_error(TTY, magenta, Ws),
		    loop(TTY, I+1, Forms, Compile0, Bindings)
	    end;
	{exprs, Exprs} ->
	    Compile = compile(TTY, Compile0, Forms),
	    %% evaluate exprs in the code environment with Code
	    %% functions called not bifs are assumed to be in the
	    %% 'this' module
	    try erl_eval:expr_list(Exprs,Bindings,{eval,fun eval_this/3}) of
		{Vs,Bindings1} ->
		    tty:outputf(TTY, "~p\r\n", [lists:last(Vs)]),
		    loop(TTY, I+1, Forms, Compile, Bindings1)
	    catch
		error:undef:Stack ->
		    case Stack of 
			[{this,F,As,_}|_] ->
			    %% color?
			    errorf(TTY, "funcion ~s/~w is not defined\n", 
				   [F, length(As)]),
			    loop(TTY, I+1, Forms, Compile, Bindings);
			
			[{M,F,As,_}|_] ->
			    case code:module_status(M) of
				loaded ->
				    errorf(TTY, "funcion ~s:~s/~w is not defined\n", 
						[M, F, length(As)]),
				    loop(TTY, I+1, Forms, Compile, Bindings);
				not_loaded ->
				    
				    errorf(TTY, "module ~s is not loaded/found\n", 
					      [M]),
				    loop(TTY, I+1, Forms, Compile, Bindings)
			    end
		    end;
		error:Reason:Stack ->
		    errorf(TTY, "Error: ~p\r\n", [Reason]),
		    lists:foreach(
		      fun({M,F,A,Info}) ->
			      errorf(TTY, "~s:~w: ~s:~s/~w\r\n", 
				     [proplists:get_value(file,Info,"nofile"),
				      proplists:get_value(line,Info,0),
				      M, F, A])
		      end, Stack),
		    tty:outputf(TTY, "\r\n"),
		    loop(TTY, I+1, Forms, Compile,  Bindings);
		throw:bye ->
		    infof(TTY, "bye bye\r\n", []),
		    bye
	    end
		
    end.

display_error(TTY, Color, [{File,Es}|List]) ->    
    tty:csi(TTY, {fg, Color}),
    lists:foreach(
      fun({Line,Mod,Error}) ->	 
	      tty:outputf(TTY, "~s:~w ~s\n",[File,Line,Mod:format_error(Error)])
      end, Es),
    tty:csi(TTY, {fg, black}),
    display_error(TTY, Color, List);
display_error(_TTY, _Color, []) ->
    ok.

lint_function(Form={function,_Ln,Name,Arity,_Clauses}) ->
    erl_lint:module(
      [{attribute,1,module,this},
       {attribute,1,export,[{Name,Arity}]},
       Form]).

%% maybe compile forms
compile(_TTY, false, _Forms) ->
    false;
compile(TTY, true, Forms) ->
    case compile_forms(TTY, Forms) of
	ok -> false;
	error -> true
    end.

compile_forms(TTY, FormsMap) ->
    Fs = maps:to_list(FormsMap),
    Forms = [Func || {_, Func} <- Fs],
    Exports = [{attribute,1,export,[{Name,Arity}]} ||
		  {{Name,Arity},_Func} <- Fs],
    case compile:forms([{attribute,1,module,this} | Exports] ++ Forms,
		       [return_errors, return_warnings, 
			{error_location,line}]) of
	{ok,this,Bin,Ws} ->
	    display_error(TTY, magenta, Ws),
	    case code:load_binary(this,"nofile",Bin) of
		{module, this} ->
		    ok;
		{error, Reason} ->
		    errorf(TTY, "unable to load forms: ~p\r\n",
			   [Reason]),
		    error
	    end;
	{error,Es,Ws} ->
	    display_error(TTY, red, Es),
	    display_error(TTY, magenta, Ws),
	    error;
	error ->
	    error
    end.

errorf(TTY, Fmt, Args) ->
    tty:csi(TTY, {fg, red}),
    tty:outputf(TTY, Fmt, Args),
    tty:csi(TTY, {fg, black}).

infof(TTY, Fmt, Args) ->
    tty:csi(TTY, {fg, green}),
    tty:outputf(TTY, Fmt, Args),
    tty:csi(TTY, {fg, black}).

eval_this(bye,[],_Bindings) ->
    throw(bye);
eval_this(Func,Args, Bindings) ->
    {Vs,Bindings1} = erl_eval:expr_list(Args,Bindings,{eval,fun eval_this/3}),
    {value,apply(this, Func, Vs),Bindings1}.

parse(TTY, I, Ts) ->
    if Ts =:= [] ->
	    tty:outputf(TTY, "~w> ", [I]);
       true ->
	    tty:outputf(TTY, ">> ", [])
    end,
    case tty:get_line(TTY, ?MODULE) of
	eof ->
	    %% parse Ts. ?
	    eof;
	Line ->
	    tty:output(TTY, "\r\n"),
	    case erl_scan:string(binary_to_list(Line)) of
		{ok, Ts1, _Eol} ->
		    Ts2 = Ts++Ts1,
		    case lists:member({dot,1}, Ts1) of
			true ->
			    case erl_parse:parse_form(Ts2) of
				{ok, Form} ->
				    {form, Form};
				{error, {_Ln,_Mod,_Error}}->
				    case erl_parse:parse_exprs(Ts2) of
					{ok,Exprs} -> {exprs,Exprs};
					{error, {_Ln2,Mod2,Error2}}->
					    errorf(TTY, "~s\r\n", [Mod2:format_error(Error2)]),
					    error
				    end
			    end;
			false ->
			    parse(TTY, I, Ts2)
		    end;
		{error,{_Ln3,Mod3,Error3}} ->
		    errorf(TTY, "~s\r\n", [Mod3:format_error(Error3)]),
		    error
	    end
    end.

%% Tab complete line
-include_lib("kernel/include/eep48.hrl").
-define(RENDERABLE_FORMAT(Format),
        Format =:= ?NATIVE_FORMAT;
        binary_part(Format, 0, 5) =:= <<"text/">>).


expand(RevLine) ->
    case erl_scan:string(lists:reverse(RevLine)) of
	{ok, [{atom, _, M0}], _} ->
	    expand_modules(code:all_loaded(), atom_to_list(M0), []);
	{ok, [{atom, _, Mod},{':',_}], _} ->
	    try apply(Mod, module_info, [exports]) of
		Fs ->
		    {yes, "", [atom_to_list(F) || {F,_} <- Fs]}
	    catch
		error:_ ->
		    {no, "", []}
	    end;
	{ok, [{atom, _, Mod},{':',_},{atom,_,F0}], _} ->
	    %% try expand all functions in module Mod starting with F0
	    try apply(Mod, module_info, [exports]) of
		Fs ->
		    expand_functions(Fs, atom_to_list(F0), [])
	    catch
		error:_ ->
		    {no, "", []}
	    end;
	{ok, [{atom, _, Mod},{':',_},{atom,_,F0},{'(',_}], _} ->
	    %% find documentation
	    try code:get_doc(Mod) of
		{ok, #docs_v1{ format = Format } = Docs} 
		  when ?RENDERABLE_FORMAT(Format) ->		
		    case format_docs(shell_docs:render(Mod,F0,Docs)) of
			{error,_} -> {no, "", []};
			Text -> {yes, "", Text}
		    end;
		_Error ->
		    {no, "", []}
	    catch
		error:_ ->
		    {no, "", []}
	    end;
	_ ->
	    {no, "", []}
    end.


format_docs({error,_} = E) ->
    E;
format_docs(Docs) ->
    Docs.
%%    {match,Lines} = re:run(Docs,"(.+\n|\n)",
%%			   [unicode,global,{capture,all_but_first,binary}]),
%%    Lines.


expand_functions([{Fun,_Arity}|Fs], F0, Acc) ->
    FunStr = atom_to_list(Fun),
    case string:prefix(FunStr, F0) of
	nomatch ->
	    expand_functions(Fs, F0, Acc);
	_Tail ->
	    expand_functions(Fs, F0, [FunStr|Acc])
    end;
expand_functions([], F0, Acc) ->
    expand_match(lists:reverse(Acc), F0, "(").

expand_modules([{Mod,_Path}|Ms], M0, Acc) ->
    ModStr = atom_to_list(Mod),
    case string:prefix(ModStr, M0) of
	nomatch ->
	    expand_modules(Ms, M0, Acc);
	_Tail ->
	    expand_modules(Ms, M0, [ModStr|Acc])
    end;
expand_modules([], M0, Acc) ->
    expand_match(lists:reverse(Acc), M0, ":").

expand_match([], _Match, _Sep) ->
    {no, "", []};
expand_match([OneMatch], Match, Sep) ->
    Insert = remove_prefix(OneMatch, Match),
    {yes, Insert++Sep, []};
expand_match(MultipleMatches=[FirstMatch|RestOfMatches], Match, _Sep) ->
    CommonMatch =
	lists:foldl(
	  fun(String, Prefix) ->
		  common_prefix(Prefix, String)
	  end, FirstMatch, RestOfMatches),
    Insert = remove_prefix(CommonMatch, Match),
    {yes, Insert, MultipleMatches}.

remove_prefix(String, Prefix) ->
    string:prefix(String, Prefix).

%% extract common prefix (utf8?)
common_prefix(String1, String2) ->
    common_prefix(String1, String2, []).

common_prefix([C|Cs], [C|Ds], Acc) ->
    common_prefix(Cs,Ds,[C|Acc]);
common_prefix(_Cs,_Ds,Acc) -> 
    lists:reverse(Acc).
