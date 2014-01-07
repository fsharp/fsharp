#indent "off"

open System
open System.Windows.Forms
open Ast
open Utils
open States
open Analysis

[<STAThread>] do ()


let argv = System.Environment.GetCommandLineArgs()

let print_graph (p:Prog) (gv:Microsoft.GLEE.GraphViewerGDI.GViewer list) = if gv=[] then () else
( let g = new Microsoft.GLEE.Drawing.Graph("graph") in
  //
  let rec sub_activity (a:Activity) = List.iter (sub_handler a) a.h
  and sub_handler (a:Activity) ((hs,h):(string*Handler)) =
  ( let prefix = a.s^"."^hs^":"^(type_to_string h.at)^"." in
    let node = g.AddNode(prefix^h.a) :?> Microsoft.GLEE.Drawing.Node in
    node.Attr.Fillcolor <- Microsoft.GLEE.IGraphViewer.Color.Aqua;
    node.Attr.Label <- a.s^"."^hs^":"^(type_to_string h.at);
    let edge = g.AddEdge(prefix^h.a, prefix^h.a) :?> Microsoft.GLEE.Drawing.Edge in
    edge.Attr.Color <- Microsoft.GLEE.IGraphViewer.Color.White; // invisible, just to make sure the node is rendered onscreen
    edge.Attr.Id <- prefix^h.a;
    List.iter (sub_node prefix) h.n;
  )
  and sub_node prefix (n:Node) =
  ( let node = g.AddNode(prefix^n.s) :?> Microsoft.GLEE.Drawing.Node in
    node.Attr.Shape <- Microsoft.GLEE.IGraphViewer.Shape.Box;
    node.Attr.XRad <- 3.0F;
    node.Attr.YRad <- 3.0F;
    ( match n.d with
      | NExpr(e) -> node.Attr.Label <- (expr_to_string e)
      | NCall(peer,verb,q,e) -> ( node.Attr.Label <- "CALL "^peer^"."^verb^"("^(expr_to_string e)^")"; node.Attr.Fillcolor <- Microsoft.GLEE.IGraphViewer.Color.Pink; )
      | NCallreturn(e) -> node.Attr.Label <- (expr_to_string e)
      | NAtomic(ad) -> node.Attr.Label <- (atomic_to_string ad.m)^" "^ad.rw;
      | NSet(idp,tail,e) -> node.Attr.Label <- "SET "^(idpath_to_string idp)^":="^(expr_to_string e)
      | NGet(v,tail) -> node.Attr.Label <- "GET "^v
      | NFirst(fs) -> node.Attr.Label <- "SELECT"
      | NSignal(e) -> (node.Attr.Label <- "SIGNAL("^(expr_to_string e)^")"; node.Attr.Fillcolor <- Microsoft.GLEE.IGraphViewer.Color.LightGreen )
      | NReturn(e) -> (node.Attr.Label <- "RETURN("^(expr_to_string e)^")"; node.Attr.Fillcolor <- Microsoft.GLEE.IGraphViewer.Color.LightGreen )
      | NIf(ifs) -> (node.Attr.Label <- "IF"; List.iter (sub_ifnode prefix n) ifs )
      | NIfType(ifs) -> (node.Attr.Label <- "TYPESWITCH("^(List.head ifs).ifv^")"; List.iter (sub_iftypenode prefix n) ifs )
    );
    List.iter (sub_inwire prefix n) n.inWires
  )
  and sub_ifnode prefix n (id:IfDat) =
  ( let node = g.AddNode(prefix^id.thens) :?> Microsoft.GLEE.Drawing.Node in
    node.Attr.Shape <- Microsoft.GLEE.IGraphViewer.Shape.Box;
    node.Attr.XRad <- 3.0F;
    node.Attr.YRad <- 3.0F;
    node.Attr.Label <- "("^(expr_to_string id.ife)^")";
    let edge = g.AddEdge(prefix^n.s,prefix^id.thens) :?> Microsoft.GLEE.Drawing.Edge in
    edge.Attr.Id <- prefix^n.s^"--"^prefix^id.thens;
  )
  and sub_iftypenode prefix n (id:IfTypeDat) =
  ( let node = g.AddNode(prefix^id.tthens) :?> Microsoft.GLEE.Drawing.Node in
    node.Attr.Shape <- Microsoft.GLEE.IGraphViewer.Shape.Box;
    node.Attr.XRad <- 3.0F;
    node.Attr.YRad <- 3.0F;
    node.Attr.Label <- "["^(typelist_to_string id.ift)^"]";
    let edge = g.AddEdge(prefix^n.s,prefix^id.tthens) :?> Microsoft.GLEE.Drawing.Edge in
    edge.Attr.Id <- prefix^n.s^"--"^prefix^id.tthens;
  )
  and sub_inwire prefix n ((ws,wq):Wire) =
  ( let edge = g.AddEdge(prefix^ws, prefix^n.s) :?> Microsoft.GLEE.Drawing.Edge in
    edge.Attr.Id <- prefix^ws^"--"^prefix^n.s;
    let star = (match wq with | NUnbounded -> "*" | NBounded(1) -> "" | NBounded(n) -> string(n)) in
    edge.Attr.Label <- star^ws;
  )
  in
  //
  List.iter sub_activity p.a;
  List.iter (fun (gv:Microsoft.GLEE.GraphViewerGDI.GViewer) -> gv.Graph <- g ) gv;
)

let update_graph (p:Prog) (ps:ProgState) (gv:Microsoft.GLEE.GraphViewerGDI.GViewer list) (show:bool) = if gv=[] then () else
( let g = (List.head gv).Graph in
  //
  let rec sub_instance ((i,inst):(int*Instance)) =
  ( let cols = [| Microsoft.GLEE.IGraphViewer.Color.Red; Microsoft.GLEE.IGraphViewer.Color.Gold; Microsoft.GLEE.IGraphViewer.Color.Green; Microsoft.GLEE.IGraphViewer.Color.Blue; Microsoft.GLEE.IGraphViewer.Color.Purple |] in
    let col = if show then cols.[i % Array.length(cols)] else Microsoft.GLEE.IGraphViewer.Color.Black in
    let prefix = inst.a^"."^inst.h^":"^(type_to_string inst.ht)^"." in
    List.iter (sub_queue prefix col) inst.q;
  )
  and sub_queue prefix col (wq:WireQueue) =
  ( let name = prefix^wq.src^"--"^prefix^wq.dst in
    let edge = g.EdgeById(name) :?> Microsoft.GLEE.Drawing.Edge in
    edge.Attr.Color <- col;
    edge.Attr.Fontcolor <- col;
    edge.Attr.LineWidth <- if show then 10 else 1;
  )
  and sub_mainqueue (mq:MainQueue) =
  ( let (a:Activity) = activity_in_prog mq.a p in
    let (h:Handler) = handler_in_activity (mq.h,mq.ht) a in
    let prefix = mq.a^"."^mq.h^":"^(type_to_string mq.ht)^"." in
    let node = g.FindNode(prefix^h.a)  :?> Microsoft.GLEE.Drawing.Node in
    node.Attr.Color <- if show then Microsoft.GLEE.IGraphViewer.Color.Red else Microsoft.GLEE.IGraphViewer.Color.Black;
    node.Attr.LineWidth <- if show then 10 else 1;
  )
  in
  //
  List.iter sub_mainqueue ps.mq;
  List.iter sub_instance ps.i;
  List.iter (fun (gv:Microsoft.GLEE.GraphViewerGDI.GViewer) -> gv.Invalidate(); if show then gv.Refresh(); ) gv;
  Application.DoEvents();
)






let amain (fn:string) =
  let fs = new IO.FileStream(fn, IO.FileMode.Open, IO.FileAccess.Read, IO.FileShare.Read) in
  let stream = new IO.StreamReader(fs) in
  let lexbuf = Lexing.from_text_reader System.Text.Encoding.ASCII stream in 
  let prog = 
    try Pars.start Lex.token lexbuf
    with e -> let pos = Lexing.lexbuf_curr_p lexbuf in 
      raise (new System.Exception("Parsing error near line "^string(pos.pos_lnum+1)^" char "^string(pos.pos_cnum-pos.pos_bol)^"\n"^e.ToString()))
    in
  stream.Close();
  fs.Close();
  let prog = Analysis.PostParse(prog) in
  let prog = Analysis.CheckServicePorts(prog) in
  let prog = Analysis.InferTypes(prog) in
  log ("\n\nProgram:\n-------------\n"^(prog_to_string prog)^"\n\n");
  let ps = ref (MakeInitialState prog false) in
  log (progstate_to_string (!ps));
  //
  let form = new Form() in
  form.AutoScaleDimensions <- new Drawing.SizeF(8.0F,16.0F);
  form.AutoScaleMode <- AutoScaleMode.Font;
  form.ClientSize <- new Drawing.Size(676,363);
  form.Text <- argv.[1];
  let numhandlers = List.length (List.concat (List.map (fun (a:Activity)->a.h) prog.a)) in
  let split = new SplitContainer() in
  let gv = if IO.File.Exists("Microsoft.GLEE.dll") then [new Microsoft.GLEE.GraphViewerGDI.GViewer(); new Microsoft.GLEE.GraphViewerGDI.GViewer()]
  else (let _ = MessageBox.Show("Need these DLLs to display the graph:\nMicrosoft.GLEE.dll\nMicrosoft.GLEE.Drawing.dll\nMicrosoft.GLEE.GraphHelper.dll\nMicrosoft.GLEE.GraphViewerGDI.dll\nMicrosoft.GLEE.IGraphViewer.dll\nMicrosoft.GLEE.Splines.dll\nAsml.Tools.Algos.SA.dll\nAsml.Tools.Algos.SimplexMethod.dll\nAsml.Tools.Algos.SimplexMethodOpt.Tableau.dll\n") in []) in
  let gprint (ps:ProgState) (show:bool) : unit = update_graph prog ps gv show in
  let handle f = new EventHandler (fun x y -> try f x y with e -> Console.Beep(); log("EXCEPTION\n"); log (e.ToString()^"\n\n") ) in
  let autostep = ref false in // for whether generating an event does an automatic step-over
  //
  let btnStepInto = new Button() in
  btnStepInto.Anchor <- AnchorStyles.Top ||| AnchorStyles.Left;
  btnStepInto.Location <- new Drawing.Point(12,12);
  btnStepInto.Size <- new Drawing.Size(75,23);
  btnStepInto.TabIndex <- 1;
  btnStepInto.Text <- "Step&Into";
  btnStepInto.UseVisualStyleBackColor <- true;
  btnStepInto.add_Click (handle (fun _ _ -> ps := step prog (!ps) gprint false None ));
  form.Controls.Add(btnStepInto);
  let btnStepOver = new Button() in
  btnStepOver.Anchor <- AnchorStyles.Top ||| AnchorStyles.Left;
  btnStepOver.Location <- new Drawing.Point(93,12);
  btnStepOver.Size <- new Drawing.Size(75,23);
  btnStepOver.TabIndex <- btnStepInto.TabIndex + 1;
  btnStepOver.Text <- "Step&Over";
  btnStepOver.UseVisualStyleBackColor <- true;
  btnStepOver.add_Click (handle (fun _ _ -> ps := step prog (!ps) gprint true None ));
  form.Controls.Add(btnStepOver);
  let btnReach = new Button() in
  btnReach.Anchor <- AnchorStyles.Top ||| AnchorStyles.Left;
  btnReach.Location <- new Drawing.Point(174,12);
  btnReach.Size <- new Drawing.Size(75,23);
  btnReach.TabIndex <- btnStepOver.TabIndex + 1;
  btnReach.Text <- "&Reach";
  btnReach.UseVisualStyleBackColor <- true;
  btnReach.add_Click (handle (fun _ _ -> reachability form prog gprint ));
  form.Controls.Add(btnReach);
  let btnRace = new Button() in
  btnRace.Anchor <- AnchorStyles.Top ||| AnchorStyles.Left;
  btnRace.Location <- new Drawing.Point(255,12);
  btnRace.Size <- new Drawing.Size(75,23);
  btnRace.TabIndex <- btnReach.TabIndex + 1;
  btnRace.Text <- "Ra&ce";
  btnRace.UseVisualStyleBackColor <- true;
  btnRace.add_Click (handle (fun _ _ -> race form prog gprint ));
  form.Controls.Add(btnRace);
  let btnRelease = new Button() in
  btnRelease.Anchor <- AnchorStyles.Top ||| AnchorStyles.Left;
  btnRelease.Location <- new Drawing.Point(336,12);
  btnRelease.Size <- new Drawing.Size(75,23);
  btnRelease.TabIndex <- btnRace.TabIndex + 1;
  btnRelease.Text <- "Re&lease";
  btnRelease.UseVisualStyleBackColor <- true;
  btnRelease.add_Click (handle (fun _ _ -> release form prog gprint ));
  form.Controls.Add(btnRelease);
  let btnCycle = new Button() in
  btnCycle.Anchor <- AnchorStyles.Top ||| AnchorStyles.Left;
  btnCycle.Location <- new Drawing.Point(417,12);
  btnCycle.Size <- new Drawing.Size(75,23);
  btnCycle.TabIndex <- btnRelease.TabIndex + 1;
  btnCycle.Text <- "C&ycle";
  btnCycle.UseVisualStyleBackColor <- true;
  btnCycle.add_Click (handle (fun _ _ -> cycle form prog gprint ));
  form.Controls.Add(btnCycle);
  let btnAutoRun = new CheckBox() in
  btnAutoRun.Anchor <- AnchorStyles.Top ||| AnchorStyles.Right;
  btnAutoRun.AutoSize <- true;
  btnAutoRun.Location <- new Drawing.Point(537,12);
  btnAutoRun.Size <- new Drawing.Size(127,21);
  btnAutoRun.TabIndex <- btnCycle.TabIndex + 1;
  btnAutoRun.Text <- "Events &Autorun";
  btnAutoRun.UseVisualStyleBackColor <- true;
  btnAutoRun.add_CheckedChanged (handle (fun _ _ -> autostep := btnAutoRun.Checked ));
  form.Controls.Add(btnAutoRun);
  //
  let count = ref 0 in
  for ai=0 to List.length(prog.a)-1 do
    let (a:Activity) = List.nth prog.a ai in
    if a.ext then (for ti=0 to List.length(a.signal)-1 do
      let t = List.nth a.signal ti in
      let txt = new TextBox() in
      txt.Anchor <- AnchorStyles.Top ||| AnchorStyles.Left ||| AnchorStyles.Right;
      txt.Location <- new Drawing.Point(12,40+(!count)*28);
      txt.Size <- new Drawing.Size(571,22);
      txt.TabIndex <- btnAutoRun.TabIndex+1+(!count)*2;
      txt.Text <- type_exemplar prog.tp t;
      form.Controls.Add(txt);
      let btn = new Button() in
      btn.Anchor <- AnchorStyles.Top ||| AnchorStyles.Right;
      btn.Location <- new Drawing.Point(589,40+(!count)*28);
      btn.Size <- new Drawing.Size(75,23);
      btn.TabIndex <- 4+(!count)*2+1;
      btn.Text <- a.s;
      btn.UseVisualStyleBackColor <- true;
      btn.add_Click (handle (fun _ _ -> 
         let expr = string_to_expr txt.Text in
         let v = Evaluate prog.ef [] [] Unimportant expr in
         ps := insert prog (!ps) gprint btn.Text v;
         if (!autostep) then ps := step prog (!ps) gprint true None; ));
      form.Controls.Add(btn);
      count := (!count)+1;
    done; )
  done;
  //
  split.Anchor <- AnchorStyles.Top;
  split.Location <- new Drawing.Point(12,40+(!count)*28);
  split.Size <- new Drawing.Size(652,311-(!count)*28);
  split.SplitterDistance <- (if numhandlers>=6 then split.ClientSize.Width/2 else split.ClientSize.Width);
  List.iter (fun (gv:Microsoft.GLEE.GraphViewerGDI.GViewer) ->
    gv.Name <- "gv";
    gv.Dock <- DockStyle.Fill;
    gv.TabIndex <- btnAutoRun.TabIndex+1+(!count)*2;
    gv.NavigationVisible <- true;
    gv.SaveButtonVizible <- false;
    gv.MouseHitDistance <- 0.05;
  ) gv;
  split.Panel1.Controls.Add (List.nth gv 0);
  split.Panel2.Controls.Add (List.nth gv 1);
  form.Controls.Add(split);
  print_graph prog gv;
  //
  Application.Run(form);
  ()


let _ = try
  if argv.Length <> 2 then (log "usage: checkmri filename.viper | /test\n"; exit(0) );
  log "CheckMRI\n----------\n";
  let arg = argv.[1] in
  if arg="-test" || arg="--test" || arg="/test" then Test.tmain()
  else amain(arg)
with e -> log (e.ToString())
//let _ = log "\nPress Return\n"; input_line stdin



// Here are some useful F# references:
//
// http://www.strangelights.com/fsharp/
// http://www.strangelights.com/fsharp/Wiki/default.aspx
// http://research.microsoft.com/projects/ilx/fsharp-manual/default.aspx
// http://www.strangelights.com/fsharp/Doc/index.html
// http://research.microsoft.com/projects/ilx/fsharp-manual/namespaces.html
