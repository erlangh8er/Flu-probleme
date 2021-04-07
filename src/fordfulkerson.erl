%%%-------------------------------------------------------------------
%%% Created : 13. Nov 2019 10:00
%%%-------------------------------------------------------------------
-module(fordfulkerson).

%% API
-export([fordfulkerson/3, fordfulkersonT/3,
  is_Vertex/2, initEdges/2, initVertices/2, markVerticeF/3, markVerticeB/3, maximizeFlow/7,
  removeAllMarkAndInspected/2, getAllInspected/2]).

%% Das Ergebnis ist nachvollziehbar (vergrößernde Wege mit dem Delta der Veränderung) in einer
%% Datei VerGrWege.log auszugeben (z.B. 88<-63<-42->62<-43->61<-44<-22 mit Delta 1).
%% Der berechnete Fluss ist unter dem Attributnamen flow an der Kante im Graphen zu speichern.
%%
%% Schnittstelle: fordfulkerson:fordfulkerson(<Filename>,<Quelle>,<Senke>);
%%                fordfulkerson:fordfulkerson(<Graph>,<Quelle>,<Senke>);
%% Rückgabewert: [<Liste der im letzten Lauf inspizierten Ecken>]
%%               sowie ein Bild des resultierenden Graphen als *.dot und *.svg Datei;
fordfulkerson(Filename, Quelle, Senke) when is_atom(Filename) ->
  fordfulkerson(adtgraph:importG(Filename, d), Quelle, Senke);

fordfulkerson(Graph, Quelle, Senke) when is_integer(Quelle), is_integer(Senke) ->
  %% Hole alle Vertices
  Vertices = adtgraph:getVertices(Graph),
  %% Ist die Quelle, Senke eine Vertex?
  case ((is_Vertex(Vertices, Quelle)) and (is_Vertex(Vertices, Senke))) == true of
    true ->
      %% Markiere aktuellen Vertex mit "inspiziert" = false
      VerticeList = initVertices(Graph, Vertices),
      Edges = adtgraph:getEdges(Graph),
      %% Markiere aktuelle Edge mit "flow" = 0
      ModifiedGraph = initEdges(VerticeList, Edges),
      %% Markiere den Quell-Vertex mit "mark" = nil, nil, undefined (nil equals infinitely)
      MarkedStart = adtgraph:setAtV(ModifiedGraph, Quelle, mark, {nil, nil, undefined}),
      %% Umsetzung des Algorithmus
      {ModiefiedStartGraph, VisitedEdges} = iterate(MarkedStart, Vertices, Quelle, Senke, true),
      %% Visualisierung.
      adtgraph:printGFF(ModiefiedStartGraph, fordfulkerson_graph),
      VisitedEdges;
    false -> []
  end.


%% fordfulkerson:fordfulkersonT(<Graph>,<Quelle>,<Senke>);
%% mit obigem Rückgabewert,
%% jedoch ohne weitere Ausgaben (keine log-Dateien oder io-Ausgaben) für die Zeitmessung.
fordfulkersonT(Graph, Quelle, Senke) ->
  %% Hole alle Vertices
  Vertices = adtgraph:getVertices(Graph),
  %% Ist die Quelle, Senke eine Vertex?
  case ((is_Vertex(Vertices, Quelle)) and (is_Vertex(Vertices, Senke))) == true of
    true ->
      %% Markiere aktuellen Vertex mit "inspiziert" = false
      VerticeList = initVertices(Graph, Vertices),
      Edges = adtgraph:getEdges(Graph),
      %% Markiere aktuelle Edge mit "flow" = 0
      EdgeList = initEdges(VerticeList, Edges),
      %% Markiere den Quell-Vertex mit "mark" = nil, nil, undefined (nil equals infinitely)
      MarkedStart = adtgraph:setAtV(EdgeList, Quelle, mark, {nil, nil, undefined}),
      %% Umsetzung des Algorithmus
      {_ModiefiedStartGraph, VisitedEdges} = iterate(MarkedStart, Vertices, Quelle, Senke, false),
      VisitedEdges;
    false -> []
  end.

%% Iteration von Quelle bis Senke mit Markierungen der Vertices
iterate(Graph, Vertices, Quelle, Senke, Logging) ->
  %% Sind alle Vertices inspiziert? Also wenn wir an der Senke angekommen sind.
  case adtgraph:getValV(Graph, Senke, mark) == nil of
    true ->
      NextVertex = getNextVertex(Graph, Vertices),
      case NextVertex == nil of
        true ->
          % Finish
          {Graph, []};
        false ->
          %% Markiere den Vertex mit "inspiziert" = true
          Graph2 = adtgraph:setAtV(Graph, NextVertex, inspiziert, true),
          %Vorwärtskanten
          TargetsForward = adtgraph:getTarget(Graph, NextVertex),
          {Graph3, _BSForward} = markVerticeF(Graph2, NextVertex, TargetsForward),
          %Rückwärtskanten
          SourcesBackward = adtgraph:getSource(Graph, NextVertex),
          {Graph4, _BSBackward} = markVerticeB(Graph3, NextVertex, SourcesBackward),
          iterate(Graph4, Vertices, Quelle, Senke, Logging)
      end;
    false ->
      %% Gehe zu 3.
      Graph2 = maximizeFlow(Graph, nil, Senke, Senke, Quelle, "", Logging),
      InspectedVertices = getAllInspected(Graph2, Vertices),
      %% Entferne die Markierung
      Graph3 = removeAllMarkAndInspected(Graph2, Vertices),
      %% Sonderbehandlung fuer Quell-Vertex
      Graph4 = adtgraph:setAtV(Graph3, Quelle, mark, {nil, nil, undefined}),
      {ReturnGraph, ReturnIV} = iterate(Graph4, Vertices, Quelle, Senke, Logging),
      if
        ReturnIV == [] -> {ReturnGraph, InspectedVertices};
        true -> {ReturnGraph, ReturnIV}
      end
  end.

%% Hole naechsten Vertex, der nicht markiert und inspiziert ist
getNextVertex(_Graph, []) -> nil;
getNextVertex(Graph, [VertexA | Tail]) ->
  Mark = adtgraph:getValV(Graph, VertexA, mark),
  Inspected = adtgraph:getValV(Graph, VertexA, inspiziert),
  case ((Mark =/= nil) and (Inspected == false)) of
    true -> VertexA;
    false -> getNextVertex(Graph, Tail)
  end.


%% ---------------------------Single Point of Control---------------------------------------------
is_Vertex([], _) -> false;
is_Vertex([Vertex1 | _], VertexID) when Vertex1 == VertexID -> true;
is_Vertex([_ | Rest], VertexID) -> is_Vertex(Rest, VertexID).

%% Initialisierung der Edges
initEdges(Graph, []) -> Graph;
%% Markiere jede Kante mit flow = 0
initEdges(Graph, [VertexA, VertexB | Tail]) -> initEdges(adtgraph:setAtE(Graph, VertexA, VertexB, flow, 0), Tail).

%% Initialisierung der Edges
initVertices(Graph, []) -> Graph;
%% Markiere jede Ecke mit inspiziert = false
initVertices(Graph, [Vertex | Tail]) -> initVertices(adtgraph:setAtV(Graph, Vertex, inspiziert, false), Tail).

%% Markiere Ecken in der Vorwaertsrichtung
markVerticeF(InputGraph, VertexA, [Target | Tail]) ->
  %% null safe pruefung
  case adtgraph:getValV(InputGraph, Target, mark) =/= nil of
    true ->
      OutputGraph = InputGraph,
      RandomList = [];
    false ->
      Flow = adtgraph:getValE(InputGraph, VertexA, Target, flow),
      Weight = adtgraph:getValE(InputGraph, VertexA, Target, weight),

      if
      %% Ist der flow kleiner als weight?
        Flow < Weight ->
          {_Weight, _Flow, DeltaA} = adtgraph:getValV(InputGraph, VertexA, mark),
          Delta = kleinere(Weight - Flow, DeltaA),
          %% Markiere Vertex mit + Delta
          OutputGraph = adtgraph:setAtV(InputGraph, Target, mark, {'+', VertexA, Delta}),
          case adtgraph:getValV(OutputGraph, Target, inspiziert) of
            true -> RandomList = [];
            false -> RandomList = [Target]
          end;
        true ->
          OutputGraph = InputGraph,
          RandomList = []
      end
  end,
  {ReturnGraph, ReturnRandomList} = markVerticeF(OutputGraph, VertexA, Tail),
  {ReturnGraph, RandomList ++ ReturnRandomList};
markVerticeF(Graph, _Vertex, []) -> {Graph, []}.

%% Markiere Ecken in der Rueckwaertsrichtung
markVerticeB(InputGraph, VertexA, [Source | Tail]) ->
  %% null safe pruefung
  case adtgraph:getValV(InputGraph, Source, mark) =/= nil of
    true ->
      OutputGraph = InputGraph,
      RandomList = [];
    false ->
      Flow = adtgraph:getValE(InputGraph, Source, VertexA, flow),
      if
        Flow > 0 ->
          {_Weight, _Flow, DeltaA} = adtgraph:getValV(InputGraph, VertexA, mark),
          Delta = kleinere(Flow, DeltaA),
          %% Markiere Vertex mit - Delta
          OutputGraph = adtgraph:setAtV(InputGraph, Source, mark, {'-', VertexA, Delta}),
          case adtgraph:getValV(OutputGraph, Source, inspiziert) of
            true -> RandomList = [];
            false -> RandomList = [Source]
          end;
        true ->
          OutputGraph = InputGraph,
          RandomList = []
      end
  end,
  {ReturnGraph, ReturnRandomList} = markVerticeB(OutputGraph, VertexA, Tail ++ RandomList),
  {ReturnGraph, RandomList ++ ReturnRandomList};
markVerticeB(Graph, _TargetVertex, []) -> {Graph, []}.

maximizeFlow(Graph, Delta, ActualVertice, Senke, Quelle, Log, Logging) ->
  %% Hole Markierung von Vertex
  {Direction, TargetVertice, CurrentDelta} = adtgraph:getValV(Graph, ActualVertice, mark),
  Log2 = util:to_String(ActualVertice) ++ Log,
  if
  %% Ist es die Quell-Vertex?
    ActualVertice =/= Quelle ->
      if
        ActualVertice == Senke ->
          Log3 = Log2 ++ " mit Delta " ++ util:to_String(CurrentDelta),
          Delta2 = CurrentDelta;
        true ->
          Log3 = Log2,
          Delta2 = Delta
      end,
      if
      %% Ist es eine Vorwärts-kante '+'?
        Direction == '+' ->
          % Andere Ecke ist Vorgänger
          Flow = adtgraph:getValE(Graph, TargetVertice, ActualVertice, flow),
          %% Addiere Flow & Delta
          Graph2 = adtgraph:setAtE(Graph, TargetVertice, ActualVertice, flow, add(Flow, Delta2)),
          LogString4 = "->" ++ Log3;
        true ->
          % Andere Ecke ist Nachfolger
          Flow = adtgraph:getValE(Graph, ActualVertice, TargetVertice, flow),
          %% Subtrahiere Flow & Delta
          Graph2 = adtgraph:setAtE(Graph, ActualVertice, TargetVertice, flow, sub(Flow, Delta2)),
          LogString4 = "<-" ++ Log3
      end;
    true ->
      LogString4 = Log2,
      Graph2 = Graph,
      Delta2 = Delta
  end,
  if
  %% Ist es die Quell-Vertex?
    ActualVertice == Quelle ->
      if
        Logging -> util:logging("VerGrWege.log", LogString4 ++ "\n");
        true -> nil
      end,
      Graph2;
    true ->
      maximizeFlow(Graph2, Delta2, TargetVertice, Senke, Quelle, LogString4, Logging)
  end.

%% Entferne alle inspiziert und mark markierungen an den Ecken
removeAllMarkAndInspected(Graph, [VertexA | Tail]) ->
  removeAllMarkAndInspected(adtgraph:setAtV(adtgraph:setAtV(Graph, VertexA, inspiziert, false), VertexA, mark, nil), Tail);
removeAllMarkAndInspected(Graph, []) -> Graph.

%% Hole alle inspizierten Ecken
getAllInspected(Graph, [VertexA | Tail]) ->
  case adtgraph:getValV(Graph, VertexA, inspiziert) of
    true -> [VertexA | getAllInspected(Graph, Tail)];
    false -> getAllInspected(Graph, Tail)
  end;
getAllInspected(_Graph, []) -> [].

%% gebe kleinere zahl zurueck fuer den delta abgleich im algorithmus
kleinere(undefined, V2) -> V2;
kleinere(V1, undefined) -> V1;
kleinere(V1, V2) -> min(V1, V2).

%% Fluss vergroessern
add(nil, _) -> nil;
add(_, nil) -> nil;
add(_, true) -> nil;
add(A, B) -> A + B.

%% Fluss verkleinern
sub(nil, _) -> nil;
sub(_, nil) -> nil;
sub(A, B) -> A - B.