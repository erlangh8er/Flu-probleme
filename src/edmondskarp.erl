%%%-------------------------------------------------------------------
%%% Created : 13. Nov 2019 10:01
%%%-------------------------------------------------------------------
-module(edmondskarp).

%% API
-export([edmondskarp/3, edmondskarpT/3]).

%% wobei soviel Code, wie möglich, von der Implementierung des Ford und Fulkerson Algorithmus verwendet werden soll.
%%
%% Das Ergebnis ist nachvollziehbar (vergrößernde Wege mit dem Delta der Veränderung)
%% in einer Datei VerGrWege.log auszugeben. Der berechnete Fluss ist unter dem
%% Attributnamen flow an der Kante im Graphen zu speichern.
%%
%% Schnittstelle: edmondskarp:edmondskarp(<Filename>,<Quelle>,<Senke>);
%%                edmondskarp:edmondskarp(<Graph>,<Quelle>,<Senke>);
%% Rückgabewert: [<Liste der im letzten Lauf isnpizierten Ecken]
%%               sowie ein Bild des resultierenden Graphen als *.dot und *.svg Datei;
edmondskarp(Filename, Quelle, Senke) when is_atom(Filename) ->
  edmondskarp(adtgraph:importG(Filename, d), Quelle, Senke);

edmondskarp(Graph, Quelle, Senke) when is_integer(Quelle), is_integer(Senke) ->
  %% Hole alle Vertices
  Vertices = adtgraph:getVertices(Graph),
  %% Ist die Quelle, Senke eine Vertex?
  case ((fordfulkerson:is_Vertex(Vertices, Quelle)) and (fordfulkerson:is_Vertex(Vertices, Senke))) == true of
    true ->
      % initialize
      Graph2 = fordfulkerson:initVertices(Graph, Vertices),
      Edges = adtgraph:getEdges(Graph),
      %% Markiere aktuelle Edge mit "flow" = 0
      Graph3 = fordfulkerson:initEdges(Graph2, Edges),
      %% Markiere den Quell-Vertex mit "mark" = nil, nil, undefined (nil equals infinitely)
      Graph4 = adtgraph:setAtV(Graph3, Quelle, mark, {nil, nil, undefined}),
      %% Erstelle Schlange BS
      BS = [Quelle],
      %% Umsetzung des Algorithmus
      {ReturnGraph, VisitedEdges} = inspection(Graph4, Vertices, BS, Quelle, Senke, true),
      %% Visualisierung.
      adtgraph:printGFF(ReturnGraph, edmondskarp_graph),
      VisitedEdges;
    false -> []
  end.

%% edmondskarp:edmondskarpT(<Graph>,<Quelle>,<Senke>);
%% mit obigem Rückgabewert,
%% jedoch ohne weitere Ausgaben (keine log-Dateien oder io-Ausgaben) für die Zeitmessung.
edmondskarpT(Graph, Quelle, Senke) ->
  %% Hole alle Vertices
  Vertices = adtgraph:getVertices(Graph),
  %% Ist die Quelle, Senke eine Vertex?
  case ((fordfulkerson:is_Vertex(Vertices, Quelle)) and (fordfulkerson:is_Vertex(Vertices, Senke))) == true of
    true ->
      % initialize
      Graph2 = fordfulkerson:initVertices(Graph, Vertices),
      Edges = adtgraph:getEdges(Graph),
      %% Markiere aktuelle Edge mit "flow" = 0
      Graph3 = fordfulkerson:initEdges(Graph2, Edges),
      %% Markiere den Quell-Vertex mit "mark" = nil, nil, undefined (nil equals infinitely)
      Graph4 = adtgraph:setAtV(Graph3, Quelle, mark, {nil, nil, undefined}),
      %% Erstelle Schlange BS
      BS = [Quelle],
      %% Umsetzung des Algorithmus
      {_ReturnGraph, VisitedEdges} = inspection(Graph4, Vertices, BS, Quelle, Senke, false),
      VisitedEdges;
    false -> []
  end.

inspection(Graph, Vertices, BS, Quelle, Senke, Logging) ->
  %% Sind alle Vertices inspiziert? Also wenn wir an der Senke angekommen sind.
  case adtgraph:getValV(Graph, Senke, mark) == nil of
    true ->
      case BS == [] of
        true ->
          % Finish wenn Schlange leer ist
          {Graph, []};
        false ->
          [Vertex | BSTail] = BS,
          %% Markiere den Vertex mit "inspiziert" = true
          Graph2 = adtgraph:setAtV(Graph, Vertex, inspiziert, true),
          %Vorwärtskanten
          TargetsForward = adtgraph:getTarget(Graph2, Vertex),
          {Graph3, BSForward} = fordfulkerson:markVerticeF(Graph2, Vertex, TargetsForward),
          %Rückwärtskanten
          SourcesBackward = adtgraph:getSource(Graph2, Vertex),
          {Graph4, BSBackward} = fordfulkerson:markVerticeB(Graph3, Vertex, SourcesBackward),
          ModifiedBS = BSTail ++ BSForward ++ BSBackward,
          inspection(Graph4, Vertices, ModifiedBS, Quelle, Senke, Logging)
      end;
    false ->
      %% Gehe zu 3.
      Graph2 = fordfulkerson:maximizeFlow(Graph, nil, Senke, Senke, Quelle, "", Logging),
      InspectedVertices = fordfulkerson:getAllInspected(Graph2, Vertices),
      %% Entferne die Markierung
      Graph3 = fordfulkerson:removeAllMarkAndInspected(Graph2, Vertices),
      %% Sonderbehandlung fuer Quell-Vertex
      Graph4 = adtgraph:setAtV(Graph3, Quelle, mark, {nil, nil, undefined}),
      {ReturnGraph, ReturnIV} = inspection(Graph4, Vertices, [Quelle], Quelle, Senke, Logging),
      if
        ReturnIV == [] -> {ReturnGraph, InspectedVertices};
        true -> {ReturnGraph, ReturnIV}
      end
  end.