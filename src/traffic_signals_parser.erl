-module(traffic_signals_parser).
-include_lib("xmerl/include/xmerl.hrl").

-export([show/1]).

show(Filename) -> 
    HookFun = fun(#xmlElement{name=Name, content=[Cont]}, GS) when 
        is_list(Cont) -> 
            {{Name, Cont}, GS}; 
        (#xmlElement{name=Name, attributes=[#xmlAttribute{name=AttribName, value=AttribValue}], content=Cont}, GS) -> 
            {{Name, [{AttribName, AttribValue}], Cont}, GS}; 
        (#xmlElement{name=Name, content=Cont}, GS) -> 
            {{Name, Cont}, GS}; 
        (E, GS) -> 
            {E, GS} 
    end,
    
    AccFun = fun(#xmlText{value=V} = _E, Acc, GS) -> 
        case re:run(V, "^\\s*$") of
            {match, _} -> {Acc, GS}; 
            nomatch -> {[V | Acc], GS} 
        end; 
        (_E,Acc,GS) -> {[_E|Acc], GS} 
    end,
    
    {{'traffic-signals', Signals}, _} = xmerl_scan:file(Filename, [{hook_fun, HookFun}, {acc_fun, AccFun}]),
    Signals.
