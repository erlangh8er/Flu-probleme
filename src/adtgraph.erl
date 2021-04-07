%%%-------------------------------------------------------------------
%%% Created : 30. Sep 2019 08:52
%%%-------------------------------------------------------------------
-module(adtgraph).

%%Graph = {Direction, [Vertex-Liste], [Edge-Liste]}

%%Vertex = {id, [Attribut], ...}
%%Edge = {V1IDVon, V2IDZu, [Attribut], ...}
%%Attribut = {AttrName, Value}

%% API
-export([createG/1, addVertex/2, deleteVertex/2, addEdge/3, deleteEdge/3, setAtE/5, setAtV/4, getValE/4, getValV/3,
  getEdges/1, getTarget/2, getSource/2, getIncident/2, getVertices/1, getAdjacent/2, importG/2, exportG/2, printG/2, printGFF/2]).

%% createG Kapitel 2.
%% /createG(<d|ud>)
%% return Graph
createG(d) ->
  util:setglobalvar(yndirected, directed),
  {d, [], []};
createG(ud) ->
  util:setglobalvar(yndirected, nondirected),
  {ud, [], []};
createG(_) -> nil. % Fehler: Weder gerichtet noch ungerichtet.


%% addVertex Kapitel 3.
%% /addVertex(<graph>,<vertex>)
%% Bedindung in Methoden Signatur: http://erlang.org/doc/reference_manual/functions.html
%% return Graph
addVertex({Direction, VertexList, EdgeList}, VertexID) when is_integer(VertexID), VertexID > 0 ->
  {Direction, [{addV(VertexList, VertexID), []} | VertexList], EdgeList};
addVertex({Direction, VertexList, EdgeList}, _) ->
  {Direction, VertexList, EdgeList}.  % Wird ausgeführt, wenn vertexID nicht natuerlich ist
%% Findet heraus, ob Vertex in VertexList existiert und gibt Vertex zurueck
addV([], VertexID) -> VertexID;
addV([Vertex | Rest], VertexID) when VertexID /= Vertex -> addV(Rest, VertexID);
addV(_, _) -> "".


%% deleteVertex Kapitel 4.
%%  /deleteVertex(<graph>,<vertex>)
%% return Graph
deleteVertex({Direction, VertexList, EdgeList}, VertexID) when is_integer(VertexID), VertexID > 0 ->
  {Direction, dVertex(VertexList, VertexList, VertexID), dVertexEdges(EdgeList, EdgeList, VertexID)}.

%% Loescht Vertex in VertexListe und gibt diese zurueck
dVertex(VertexList, [], _) -> VertexList;
dVertex(VertexList, [{Vertex, Attr} | Rest], VertexID) ->
  if VertexID == Vertex -> remove({VertexID, Attr}, VertexList)
  ; VertexID /= Vertex -> dVertex(VertexList, Rest, VertexID)
  end.
%% Loescht Edges, die von Vertex ein- oder ausgehen aus der List
%% Return EdgeList
dVertexEdges(Edgelist, [], _) -> Edgelist;
dVertexEdges(EdgeList, [{Edge1, Edge2, Attr} | Rest], VertexID) when Edge1 == VertexID ->
  dVertexEdges(remove({VertexID, Edge2, Attr}, EdgeList), Rest, VertexID);
dVertexEdges(EdgeList, [{Edge1, Edge2, Attr} | Rest], VertexID) when Edge2 == VertexID ->
  dVertexEdges(remove({Edge1, VertexID, Attr}, EdgeList), Rest, VertexID);
dVertexEdges(EdgeList, [_ | Rest], VertexID) -> dVertexEdges(EdgeList, Rest, VertexID).

%% addEdge Kapitel 5.
%% /addEdge(<graph>,<vertex>,<vertex>)
%% Vorstellung Aufgabe 1: nutzt addVertex
%% Return Graph
addEdge({Direction, VertexList, EdgeList}, VertexAID, VertexBID) when is_integer(VertexAID), is_integer(VertexBID), VertexBID * VertexAID > 0 ->
  {Direction, addEdgeVList(VertexList, VertexAID, VertexBID), addE(Direction, EdgeList, VertexAID, VertexBID)}.
%% Fuegt Vertex in VertexList ein, wenn VertexAID oder BID nicht existiert
%% Return VertexList
addEdgeVList(VertexList, VertexAID, VertexBID) ->
  [{addV(VertexList, VertexBID), []} | [{addV(VertexList, VertexAID), []} | VertexList]].
%% Wenn der Graph undirected ist, muss geprueft zu dem werden, ob die inverse Edge existiert (Mehrfachkantenvermeidung)
%% Return EdgeList
addE(Direction, EdgeList, VertexAID, VertexBID) when Direction == ud ->
  case is_Edge(EdgeList, VertexAID, VertexBID) == true of
    true -> case is_Edge(EdgeList, VertexBID, VertexAID) == true of
              true -> EdgeList;
              false -> [{VertexAID, VertexBID, []} | EdgeList]
            end;
    false -> case is_Edge(EdgeList, VertexBID, VertexAID) == true of
               true -> EdgeList;
               false -> [{VertexAID, VertexBID, []} | EdgeList]
             end
  end;
addE(Direction, EdgeList, VertexAID, VertexBID) when Direction == d ->
  case is_Edge(EdgeList, VertexAID, VertexBID) == true of
    true -> EdgeList;
    false -> [{VertexAID, VertexBID, []} | EdgeList]
  end.
%% Wenn es Edge in der EdgeListe gibt return true
%% Sonst false
is_Edge([], _, _) -> false;
is_Edge([{Vertex1, Vertex2, _} | _], VertexAID, VertexBID) when {Vertex1, Vertex2} == {VertexAID, VertexBID} -> true;
is_Edge([_ | Rest], VertexAID, VertexBID) -> is_Edge(Rest, VertexAID, VertexBID).

%% deleteEdge Kapitel 6.
%% /deleteEdge(<graph>,<vertex>,<vertex>)
%% Vorstellung Aufgabe 1: nutzt addVertex
%% return Graph
deleteEdge({Direction, VertexList, EdgeList}, VertexAID, VertexBID) when is_integer(VertexAID), is_integer(VertexBID), VertexBID * VertexAID > 0
  -> {Direction, VertexList, deleteEdge_(Direction, EdgeList, EdgeList, VertexAID, VertexBID)};
deleteEdge({Direction, VertexList, EdgeList}, _, _) -> {Direction, VertexList, EdgeList}.
%% Bei ud wird im Gegensatz zu d auch die inverse Kante geloescht
%% Return EdgeList
deleteEdge_(_, EdgeList, [], _, _) -> EdgeList;
deleteEdge_(Direction, EdgeList, [{Edge1, Edge2, Attr} | Rest], VertexAID, VertexBID) when Direction == d ->
  if {Edge1, Edge2} == {VertexAID, VertexBID} ->
    deleteEdge_(Direction, remove({Edge1, Edge2, Attr}, EdgeList), Rest, VertexAID, VertexBID)
  ; true -> deleteEdge_(Direction, EdgeList, Rest, VertexAID, VertexBID)
  end;
%% Wichtig: Aenderung ggü Entwurf: Unterscheidung zw u und ud
deleteEdge_(Direction, EdgeList, [{Edge1, Edge2, Attr} | Rest], VertexAID, VertexBID) when Direction == ud ->
  if {Edge1, Edge2} == {VertexAID, VertexBID} ->
    deleteEdge_(Direction, remove({Edge1, Edge2, Attr}, EdgeList), Rest, VertexAID, VertexBID)
  ;{Edge2, Edge1} == {VertexAID, VertexBID} ->
    deleteEdge_(Direction, remove({Edge1, Edge2, Attr}, EdgeList), Rest, VertexAID, VertexBID)
  ;true -> deleteEdge_(Direction, EdgeList, Rest, VertexAID, VertexBID) %% True soll den Else Fall einleiten
  end.

%% setAtE Kapitel 7.
%%/ setAtE(<graph>,<vertex>,<vertex>,<name>,<value>)
%% return Graph
setAtE({Direction, VertexList, EdgeList}, VertexAID, VertexBID, AttrName, Value)
  when is_integer(VertexAID), is_integer(VertexBID), (VertexBID * VertexAID > 0), is_atom(AttrName) ->
  {Direction, VertexList, setAtE_(EdgeList, EdgeList, VertexAID, VertexBID, AttrName, Value)};
setAtE({Direction, VertexList, EdgeList}, _, _, _, _) -> {Direction, VertexList, EdgeList}.

%%findet Edge und fuegt diese mit neuem Attribut in Edgelist ein
%% return EdgeList
setAtE_(EdgeList, [], _, _, _, _) -> EdgeList;
setAtE_(EdgeList, [{Edge1, Edge2, Attr} | _Rest], VertexAID, VertexBID, AttrName, AttrValue) when {Edge1, Edge2} == {VertexAID, VertexBID} ->
  [{Edge1, Edge2, setAttributes(Attr, Attr, AttrName, AttrValue)} | remove({Edge1, Edge2, Attr}, EdgeList)];
setAtE_(EdgeList, [_ | Rest], VertexAID, VertexBID, AttrName, AttrValue) ->
  setAtE_(EdgeList, Rest, VertexAID, VertexBID, AttrName, AttrValue).
%% Wichtig! Aenderung Entwurf: Wenn attr gleich ist, wird es in Liste ueberschrieben
%% findet Attribute, wenn schon in Liste und fuegt neues in die Liste ein
%% return AttrList
setAttributes(Attr, [], AttrName, AttrValue) ->
  [{AttrName, AttrValue} | Attr];
setAttributes(Attr, [{Name, Value} | _Rest], AttrName, AttrValue) when Name == AttrName ->
  [{AttrName, AttrValue} | remove({Name, Value}, Attr)];
setAttributes(Attr, [_ | Rest], AttrName, AttrValue) -> setAttributes(Attr, Rest, AttrName, AttrValue).

%% setAtV Kapitel 8.
%% /setAtV(<graph>,<vertex>,<name>,<value>)
%% return Graph
%% findet Vertex und fuegt diese mit neuem Attribut in Vertex ein
%% return EdgeList
setAtV({Direction, VertexList, EdgeList}, Vertex, AttrName, Value)
  when is_integer(Vertex), is_integer(Value) ->
  {Direction, setAtV_(VertexList, VertexList, Vertex, AttrName, Value), EdgeList};
setAtV({Direction, VertexList, EdgeList}, _, _, _) -> {Direction, VertexList, EdgeList}.

%% findet Vertex und fuegt diese mit neuem Attribut in VertexList ein
%% return EdgeList
setAtV_(VertexList, [], _, _, _) -> VertexList;
setAtV_(VertexList, [{VertexA, Attr} | _Rest], Vertex, AttrName, AttrValue) when Vertex == VertexA ->
  [{VertexA, setAttributes(Attr, Attr, AttrName, AttrValue)} | remove({VertexA, Attr}, VertexList)];
setAtV_(VertexList, [_ | Rest], Vertex, AttrName, AttrValue) ->
  setAtV_(VertexList, Rest, Vertex, AttrName, AttrValue).

%% getValE Kapitel 9.
%% /getValE(<graph>,<vertex>,<vertex>,<name>)
%% Zusaetzlich geschiet hier eine Unterscheidung zwischen u und ud:
%% Es muss bei ud die inverse Edge auch geprueft werden
%% return Value from Edge
getValE({_Direction, _, []}, _, _, _) -> nil;
getValE({Direction, _, [{Edge1, Edge2, Attr} | _]}, VertexAID, VertexBID, AttrName)
  when ((Direction == ud) and (({Edge1, Edge2} == {VertexAID, VertexBID}) or ({Edge2, Edge1} == {VertexAID, VertexBID}))) ->
  getAttr(Attr, AttrName);
getValE({_Direction, _, [{Edge1, Edge2, Attr} | _]}, VertexAID, VertexBID, AttrName)
  when {Edge1, Edge2} == {VertexAID, VertexBID} ->
  getAttr(Attr, AttrName);
getValE({Direction, VertexList, [_ | Rest]}, VertexAID, VertexBID, AttrName) ->
  getValE({Direction, VertexList, Rest}, VertexAID, VertexBID, AttrName).

%% getValV Kapitel 10.
%% /getValV(<graph>,<vertex>,<name>)
%% return Value from Vertex
getValV({_Direction, [{Vertex, Attr} | _Rest], _}, VertexID, AttrName) when Vertex == VertexID ->
  getAttr(Attr, AttrName);
getValV({Direction, [_ | Rest], EdgeList}, VertexID, AttrName) ->
  getValV({Direction, Rest, EdgeList}, VertexID, AttrName);
getValV(_, _, _) -> nil.

%% getIncident Kapitel 11.
%% /getIncident(<graph>,<vertex>)
%% return List of Vertex
getIncident({_Direction, _VertexList, EdgeList}, VertexID) when is_integer(VertexID), VertexID > 0 ->
  getI(VertexID, EdgeList);
getIncident(_, _) -> nil.

%% Ist die Vertex teil eines Edge-Tupels? VertexB | getI..
getI(VertexList, [{VertexA, VertexB, _} | Tail]) when (VertexList == VertexA) or (VertexList == VertexB) ->
  [VertexA, VertexB | getI(VertexList, Tail)];
getI(VertexList, [_Head | Tail]) -> getI(VertexList, Tail);
%% Existiert nicht - gebe leeres Ergebnis zurueck und Abbruch Bedingung
getI(_, []) -> [].

%% getAdjacent Kapitel 12.
%% /getAdjacent(<graph>,<vertex>)
%% return List of Vertex
getAdjacent({_Direction, _VertexList, EdgeList}, VertexID) when is_integer(VertexID), VertexID > 0 ->
  util:list2set(append(getTargetList(VertexID, EdgeList), getSourceList(VertexID, EdgeList)));
getAdjacent(_, _) -> nil.

%% getTarget Kapitel 13.
%% /getTarget(<graph>,<vertex>)
%% return List of Vertex
getTarget(Graph = {ud, _VertexList, _EdgeList}, VertexID) ->
  getAdjacent(Graph, VertexID);
getTarget({_Direction, _VertexList, EdgeList}, VertexID) when is_integer(VertexID), VertexID > 0 ->
  getTargetList(VertexID, EdgeList);
getTarget(nil, _) -> [];
getTarget(_, _) -> nil.

getTargetList(VertexList, [{VertexList, Target, _AttributList} | Tail]) -> [Target | getTargetList(VertexList, Tail)];
getTargetList(VertexList, [_Head | Tail]) -> getTargetList(VertexList, Tail);
getTargetList(_VertexList, []) -> [].

%% getSource Kapitel 14.
%% /getSource(<graph>,<vertex>)
%% getSource nutz getAdjacent, bei getSource im ungerichteten Graph
%% return List of Vertex
getSource(Graph = {ud, _VertexList, _EdgeList}, VertexID) ->
  getAdjacent(Graph, VertexID);
getSource({_Direction, _VertexList, EdgeList}, VertexID) when is_integer(VertexID), VertexID > 0 ->
  getSourceList(VertexID, EdgeList);
getSource(nil, _) -> [];
getSource(_, _) -> nil.

getSourceList(VertexList, [{Source, VertexList, _AttributList} | Tail]) -> [Source | getSourceList(VertexList, Tail)];
getSourceList(VertexList, [_Head | Tail]) -> getSourceList(VertexList, Tail);
getSourceList(_VertexList, []) -> [].


%% getEdges Kapitel 15.
%% /getEdges(<graph>)
%% return List of Vertex
getEdges({_Direction, _VertexList, EdgeList}) -> getEdgeList(EdgeList);
getEdges(_) -> nil.

getEdgeList([{VertexA, VertexB, _AttrList} | Tail]) -> [VertexA, VertexB | getEdgeList(Tail)];
getEdgeList([]) -> [].

%% getVertices Kapitel 16.
%% /getVertices(<graph>)
%% Aenderung zum Entwurf: Duplikate werden durch util:list2set eleminiert
%% ohne util:list2set: 38720ms
%% mit util:list2set: 333153ms
%% return List of Vertex
getVertices({_Direction, VertexList, _EdgeList}) -> getVertexList(VertexList);
getVertices(_) -> nil.

getVertexList([{Vertex, _AttrList} | Tail]) -> [Vertex | getVertexList(Tail)];
getVertexList([]) -> [].

%% importG Kapitel 17.
%% /importG(<filename>,<d|ud>)
%% Vorstellung Aufgabe 1: nutzt addEdge und setAttribute
%% readliste write list aus util
%% return Graph
importG(Filename, Direction) when is_atom(Filename) -> importG(util:to_String(Filename), Direction);
importG(Filename, Direction) when (Direction == d) or (Direction == ud) ->
  importGList(util:readlist(util:attachEnding(Filename, "graph")), createG(Direction));
importG(_, _) -> nil.

importGList(ok, Graph) -> Graph;
importGList([VertexA, VertexB, Attr | Rest], Graph) ->
  setAtE(addEdge(importGList(Rest, Graph), VertexA, VertexB), VertexA, VertexB, weight, Attr);
importGList([], Graph) -> Graph.

%% exportG Kapitel 18.
%% /exportG(<graph>,<filename>)
%% return file
exportG({_Direction, _VertexList, EdgeList}, Filename) ->
  util:writelist(exportGList(EdgeList), util:attachEnding(Filename, "graph"));
exportG(_, _) -> nil.

exportGList([{V1, V2, AttrList} | Rest]) -> exportGList(Rest) ++ [V1, V2, getAttr(AttrList, weight)];
exportGList([]) -> [].

append([H | T], Tail) ->
  [H | append(T, Tail)];
append([], Tail) ->
  Tail.

%% printG Kapitel 19.
%% /printG(<graph>,<filename>)
%% return dot
printG({ud, _VertexList, EdgeList}, Filename) ->
  util:logging(util:attachEnding(Filename, "dot"), "graph G {\n" ++ printGList(EdgeList, pfeil(ud)) ++ "}");
printG({d, _VertexList, EdgeList}, Filename) ->
  util:logging(util:attachEnding(Filename, "dot"), "digraph G {\n" ++ printGList(EdgeList, pfeil(d)) ++ "}");
printG(_, _) -> nil.


pfeil(d) -> "->";
pfeil(ud) -> "--".

printGList([{V1, V2, AttrList} | Rest], Pfeil) ->
  util:list2string([V1, Pfeil, V2, '[ label =', getAttr(AttrList, weight), ']']) ++ printGList(Rest, Pfeil);
printGList([], _Pfeil) -> "".

%% return Value from Attribut
%% @return AttributValue
getAttr([{Name, AttrValue} | _Rest], Name) -> AttrValue;
getAttr([_Kopf | Rest], Name) -> getAttr(Rest, Name);
getAttr(_, _Name) -> nil.

%% remove elem X from List L
remove(X, L) ->
  [Y || Y <- L, Y /= X].

%% Erweitern Sie die ADT Graph aus Aufgabe 1 um eine Funktion printGFF(<Graph>,<Filename>),
%% die für die Graphen dieser Aufgabe die Graphen in eine dot Datei mit der
%% Kapazität (Attribut weight) und dem berechneten Fluss (Attribut flow) ausgibt.
printGFF({ud, _VertexList, EdgeList}, Filename) ->
  util:logging(util:attachEnding(Filename, "dot"), "graph G {\n" ++ printGFFList(EdgeList, pfeil(ud)) ++ "}");
printGFF({d, _VertexList, EdgeList}, Filename) ->
  util:logging(util:attachEnding(Filename, "dot"), "digraph G {\n" ++ printGFFList(EdgeList, pfeil(d)) ++ "}");
printGFF(_, _) -> nil.

printGFFList([{V1, V2, AttrList} | Rest], Pfeil) ->
  util:list2string([V1, Pfeil, V2, '[ label = (Kapazitaet =', getAttr(AttrList, weight), '|', 'berechneten Fluss=', getAttr(AttrList, flow), ')];']) ++ printGFFList(Rest, Pfeil);
printGFFList([], _Pfeil) -> "".