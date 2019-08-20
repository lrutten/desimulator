use "collections"
use "random"
use "time"

class Graph
   """
   A simple graph for testing. This graph consists of 3 nodes and 3 edges.
   """

   let nodes: Array[String]
   """
   The list of nodes.
   """
   let edges: Array[(String, String, F64)]
   """
   The list of edges
   """
   
   new create() =>
      """
      Constructor for Graph.
      """
      nodes = ["n1"; "n2"; "n3"]
      //edges = [("n1","n2", F64(3)); ("n2","n3", F64(4)); ("n3","n1", F64(5))]
      edges = [("n1","n2", F64(3)); ("n2","n3", F64(4))]
   
   new create_diepenbeek() =>
      """
      Constructor for Diepenbeek Graph.
      """
      nodes = 
      [
         "k100"; "k102"; "k101"; "k105"; "k103"; "k84"; "k85";   "k106"; "k126"
         "k235"; "k236"
         "k138"; "k141"; "k142"; "k125"; "k122"; "k121"; "k145"; "k108"; "k109"
         "k119"; "k123"; "k68";  "k89";  "k86";  "k113"; "k111"; "k107"; "k110"
         "k130"; "k120"; "k137"; "k112"; "k116"; "k117"; "k129"; "k128"; "k139"
         "k136"
      ]
      edges =
      [
         ("k100","k102", 3.0)
         ("k102","k101", 3.0)
         ("k101","k103", 3.0)
         ("k103","k84",  1.0)
         ("k84", "k85",  5.5)
         ("k85", "k106", 5.5)
         ("k106","k105", 2.0)
         ("k103","k105", 1.0)
         ("k102","k105", 3.5)
         ("k106","k126", 4.5)
         ("k138","k141", 2.0)
         ("k141","k142", 2.0)
         
         ("k100", "k235", 1.4)
         ("k235", "k236", 2.2)
         ("k126", "k236", 1.9)
         ("k138", "k236", 2.6)
         
         ("k138", "k145", 3.0)
         ("k125", "k126", 2.0)
         ("k85",  "k108", 4.5)
         ("k68",  "k108", 2.5)
         ("k68",  "k85",  3.5)
         ("k121", "k145", 1.0)
         ("k122", "k125", 2.5)
         ("k123", "k125", 12.0)
         ("k122", "k123", 8.0)
         ("k121", "k122", 2.0)
         ("k121", "k137", 5.0)
         ("k120", "k137", 2.5)
         ("k120", "k130", 3.5)
         ("k119", "k130", 2.5)
         ("k119", "k123", 2.0)
         ("k108", "k109", 5.0)
         ("k68",  "k89",  2.5)
         ("k86",  "k89",  3.0)
         ("k86",  "k113", 0.5)
         ("k109", "k113", 6.0)
         ("k109", "k119", 1.5)
         ("k111", "k113", 3.5)
         ("k109", "k110", 0.5)
         ("k107", "k110", 2.5)
         ("k107", "k111", 2.0)
         ("k107", "k129", 3.0)
         ("k129", "k130", 3.5)
         ("k128", "k129", 1.5)
         ("k128", "k139", 3.0)
         ("k120", "k139", 2.0)
         ("k136", "k139", 2.5)
         ("k136", "k137", 0.5)
         ("k111", "k112", 3.0)
         ("k112", "k116", 2.0)
         ("k116", "k117", 2.0)
         ("k117", "k128", 1.0)
      ]

class val PathStep
   """
   This class represents a step in a path.
   In this case a step also contains the distance to the neighbour agent.
   """
   let agent: Agent tag
   let distance: F64
   
   new val create(ag: Agent tag, dist: F64) =>
      agent = ag
      distance = dist

class Path
   let steps: Array[PathStep val] val
   
   //new create() =>
   //   steps = []
   new val create(sts: Array[PathStep val] val) =>
      steps = sts
      
   fun first(): PathStep val? =>
      steps(0)?
      
   fun destination(): Agent tag? =>
      let last = steps(steps.size() - 1)?
      last.agent

   fun distance(): F64 =>
      var dist: F64 = 0
      for st in steps.values() do
         dist = dist + st.distance
      end
      dist
   
   fun is_in_path(ag: Agent tag): Bool =>
      for st in steps.values() do
         if (digestof ag) == (digestof st.agent) then
            true
         end
      end
      false

   fun display(_env: Env) =>
      for st in steps.values() do
         _env.out.print("         step " + ((digestof st.agent) % 1000).string() + " " + st.distance.string())
      end
      
class val Port
   """
   Every Agent can haven several ports.
   """
   let step: PathStep val

   new val create(st: PathStep val) =>
      step = st

trait Event
   """
   Every event must implement this trait
   """

class val Timed
   """
   This class is used for keeping all scheduled timers in a list.
   """
   let _time: U64
   let _agent: Agent tag
   let _event: Event val

   new val create(time: U64, agent: Agent tag, event: Event val) =>
      _time = time
      _agent = agent
      _event = event

   
   fun val get_time(): U64 =>
      _time

   fun val get_agent(): Agent tag =>
      _agent

   fun val get_event(): Event val =>
      _event

   fun lt(ti: Timed val): Bool val =>
      _time < ti._time
   
   fun le(ti: Timed val): Bool val =>
      _time <= ti._time
   
   fun gt(ti: Timed val): Bool val =>
      _time > ti._time
   
   fun ge(ti: Timed val): Bool val =>
      _time >= ti._time

   fun eq(ti: Timed val): Bool val =>
      _time == ti._time

   fun ne(ti: Timed val): Bool val =>
      _time != ti._time
   
   fun compare(ti: Timed val): (Less | Greater | Equal) =>
      if _time < ti._time then
         Less
      elseif _time > ti._time then
         Greater
      else
         Equal
      end   


primitive Start is Event
   """
   Every agent receives this event as the first event.
   This can be used as a kind of initialisation
   """
primitive TimerEvent is Event
   """
   An timer event.
   """

class RouteEvent is Event
   let path: Path val

   new val create(pa: Path val) =>
      path = pa

trait Agent
   """
   Every agent implements this trait
   """
   fun sim(): Simulator tag

   be add_neighbour(n: Agent tag, dist: F64)
   be display()
   be receive_event(from: Agent tag, event: Event val)
   be req_routes()
   
actor Node is Agent
   """
   This class implements an example agent with a particular behaviour.
   The agent starts a 3s timer 5 times.
   """
   let _sim: Simulator
   let _name: String
   let _env: Env
   let _neighbours: SetIs[Port val]
   var _counter: U32
   let _routes: Map[USize, Path val]
   let _debug: Bool val = false

   new create(si: Simulator, env: Env, name: String) =>
      _sim = si
      _env = env
      _name = name
      _neighbours = SetIs[Port val].create()
      _counter = 0
      //_routes = Array[Path val]
      _routes = Map[USize, Path val]
      if _debug then _env.out.print("create node " + _name) end

   be add_neighbour(n: Agent tag, dist: F64) =>
      if _debug then _env.out.print("add neighbour " + (digestof this).string() + " " + (digestof n).string() + " " + dist.string()) end
      _neighbours.set(Port(PathStep(n, dist)))

   be display() =>
      _env.out.print("display Node  "  + _name + " " + ((digestof this) % 1000).string())
      _env.out.print("   ports")
      for po in _neighbours.values() do
         _env.out.print("      port "  + ((digestof po.step.agent) % 1000).string() + " " + po.step.distance.string())
      end
      _env.out.print("   routes")
      for ro in _routes.values() do
         _env.out.print("      route ")
         ro.display(_env)
      end

   be req_routes() =>
      let routs = recover Array[Array[(Agent tag, F64 val)]] end

      for ro in _routes.values() do
         let rout = recover Array[(Agent tag, F64 val)] end
         for st in ro.steps.values() do
            rout.push((st.agent, st.distance))
         end
         routs.push(consume rout)
      end
      sim().resp_routes(this, consume routs)
      
      
   be receive_event(from: Agent tag, event: Event val) =>
      if _debug then _env.out.print("Node receive_event") end
      match event
      | Start =>
         if _debug then _env.out.print("Node receive_event start") end
         // add all the neighbours as route
         for po in _neighbours.values() do
            if _debug then _env.out.print("Node start po") end
            let pa: Path val = Path([po.step])
            //_routes.push(pa)
            try
               let dst = pa.destination()?
               if _debug then _env.out.print("Node start dst " + (digestof dst).string()) end
               _routes.insert(digestof dst, pa)
               if _debug then _env.out.print("Node start routes size " + _routes.size().string()) end
            else
               if _debug then _env.out.print("Node start no dst") end
            end
         end
         sim().start_timer(3000, this, TimerEvent)

      | TimerEvent =>
         if _debug then _env.out.print("Node receive_event timer event " + _counter.string()) end
         send_all_routes()
         _counter = _counter + 1
         if _counter < 50 then
            sim().start_timer(3000, this, TimerEvent)
         end

      | let rev: RouteEvent val =>
         if _debug then _env.out.print("Node receive_event route event") end
         if _debug then _env.out.print("   path size " + rev.path.steps.size().string()) end
         if not rev.path.is_in_path(this) then
            if _debug then _env.out.print("Node route is not in path") end
            try
               let dst = rev.path.destination()?
               if not _routes.contains(digestof dst) then
                  if _debug then _env.out.print("Node route is not in routes") end
                  _routes.insert(digestof dst, rev.path)
               else
                  if _debug then _env.out.print("Node route is in routes") end
                  let pa = _routes(digestof dst)?
                  if _debug then _env.out.print("Node route distance new " + rev.path.distance().string()) end
                  if _debug then _env.out.print("Node route distance old " + pa.distance().string()) end
                  if rev.path.distance() < pa.distance() then
                     _routes.insert(digestof dst, rev.path)
                     if _debug then _env.out.print("Node route insert new route") end
                  else
                     if _debug then _env.out.print("Node route no insert new route") end
                  end
               end
            else
               if _debug then _env.out.print("Node route no dst") end
            end
         else
            if _debug then _env.out.print("Node is in path") end
         end
      else
         if _debug then _env.out.print("Node receive_event other event") end
      end
      sim().response(this)

   fun sim(): Simulator tag =>
      _sim

   fun send_all_routes() =>
      if _debug then _env.out.print("Node send all routes") end
      
      // for each port
      for po in _neighbours.values() do
         if _debug then _env.out.print("   port") end
         let neighb = po.step.agent
         if _debug then _env.out.print("      route neighb " + (digestof neighb).string()) end
         if _debug then _env.out.print("      routes size " + _routes.size().string()) end
         for ro in _routes.values() do
            if _debug then _env.out.print("      route") end
            try
               if _debug then _env.out.print("      route to be sent") end
               let fi: PathStep val = ro.first()?
               if (digestof fi.agent) != (digestof po.step.agent) then
                  if _debug then _env.out.print("         send route") end
                  
                  let ro2 = 
                  recover val
                     //let st  = po.step
                     let st = PathStep(this, po.step.distance)
                     var ro3 = [st]
                     ro3.concat(ro.steps.values())
                     ro3
                  end
                  sim().send_event(this, po.step.agent, RouteEvent(Path(ro2)) )
               else
                  if _debug then _env.out.print("         do not send route") end
               end
            else
               if _debug then _env.out.print("         no first") end
            end
         end
      end
   
actor Simulator
   """
   This is the simulator engine.
   """
   let _agents: SetIs[Agent tag]
   let _names: Map[String, (Agent tag | None)]
   let _env: Env
   var _out_counter: U32 = 0
   let _events: List[(Agent tag, Agent tag, Event val)]
   var _current_time: U64
   let _timers: MinHeap[Timed val]
   let _rand: Rand
   let _debug:Bool val = true
   
   
   new create(env: Env) =>
      _env = env
      if _debug then env.out.print("start simulator") end
      _agents = SetIs[Agent tag].create()
      _names = Map[String, (Agent tag|None)].create()
      if _debug then _env.out.print("create size " + _agents.size().string()) end
      _events = List[(Agent tag, Agent tag, Event val)].create()
      _current_time = 0
      _timers = MinHeap[Timed val].create(10)
      _rand = Rand

   be add(agname: String, ag: Agent tag) =>
      if _debug then _env.out.print("add ag " + agname + " " + (digestof ag).string()) end
      _agents.set(ag)
      _names.insert(agname, ag)
      if _debug then _env.out.print("add _agents size " + _agents.size().string()) end
      if _debug then _env.out.print("add _names  size " + _names.size().string()) end

   be make_edge(aga: String, agb: String, dist: F64) =>
      if _debug then _env.out.print("make_edge " + aga + " " + agb + " " + dist.string()) end
      let agea: (Agent tag|None) = _names.get_or_else(aga, None)
      let ageb: (Agent tag|None) = _names.get_or_else(agb, None)
      match (agea, ageb)
      | (let a: Agent tag, let b: Agent tag) => 
         if _debug then _env.out.print("ag a b gevonden " + aga + " " + agb) end
         a.add_neighbour(b, dist)
         b.add_neighbour(a, dist)
      | (None, _) => _env.out.print("ag niet gevonden 1 " + aga)
      | (_, None) => _env.out.print("ag niet gevonden 2 " + aga)
      end
      
   be display() =>
      _env.out.print("display size " + _agents.size().string())
      for ag in _agents.values() do
         _env.out.print("   " + (digestof ag).string())
         ag.display()
      end

   be display_routes() =>
      _env.out.print("display routes")
      for ag in _agents.values() do
         ag.req_routes()
      end

   be start() =>
      if _debug then _env.out.print("Simulator start") end
      for ag in _agents.values() do
         send(ag, ag, Start)
      end
      shift_time()
      
   be response(from: Agent tag) =>
      _out_counter = _out_counter - 1
      if _debug then _env.out.print("Simulator response " + _out_counter.string()) end
      shift_time()
      
   be start_timer(time: U64, agent: Agent tag, event: Event val) =>
      if _debug then _env.out.print("Simulator start timer") end
      let raval = _rand.next() % 20
      _timers.push(Timed(_current_time + time + raval, agent, event))

   be send_event(from: Agent tag, to: Agent tag, ev: Event val) =>
      send(from, to, ev)

   be resp_routes(from: Agent tag, routes: Array[Array[(Agent tag, F64 val)]] iso) =>
      if _debug then _env.out.print("Simulator resp routes") end
      //if _debug then _env.out.print("node " + (digestof from).string()) end
      if _debug then _env.out.print("node " + get_name(from)) end

      recover iso
         let rts: Array[Array[(Agent tag, F64 val)]] ref = consume routes
         for rt in rts.values() do
            if _debug then _env.out.print("   route") end
            for (ag, dist) in rt.values() do
               if _debug then _env.out.print("      step " + get_name(ag) + ", " + dist.string()) end
            end
         end
         consume rts
      end
      
   fun get_name(ag: Agent tag): String =>
      for (nm, age) in _names.pairs() do
         if ag is age then
            return nm
         end
      end
      "???"

   fun ref send(from: Agent tag, to: Agent tag, ev: Event val) =>
      _events.push((from, to, ev))
      shift_time()
      
   fun ref send_now(from: Agent tag, to: Agent tag, ev: Event val) =>
      _out_counter = _out_counter + 1
      if _debug then _env.out.print("Simulator send " + _out_counter.string()) end
      to.receive_event(from, ev)

   fun ref send_all() =>
      while _events.size() > 0 do
         try
            let ev :(Agent tag, Agent tag, Event val) = _events.shift()?
            (let fr: Agent tag, let to: Agent tag, let e: Event val) = ev
            send_now(fr, to, e)
         end
      end

   fun ref shift_time() =>
      send_all()
      if (_events.size() == 0) and (_out_counter == 0) then
         if _debug then _env.out.print("Simulator shift time new time") end
         try
            let ti = _timers.pop()?
            send(ti.get_agent(), ti.get_agent(), ti.get_event())
            _current_time = ti.get_time()
            if _debug then _env.out.print("Simulator shift time current time " + _current_time.string()) end
         end
      end


class Notify is TimerNotify
   let _sim: Simulator

   new iso create(sim: Simulator) =>
      _sim = sim
      
   fun ref apply(timer: Timer, count: U64): Bool =>
      _sim.display_routes()
      true
      
      
actor Main
   """
   Main is the start of the simulation.
   """
   let _env: Env
   let _sim: Simulator
   let _debug: Bool val = true
   
   new create(env: Env) =>
      _env       = env
      var a: U32 = 7
      if _debug then env.out.print("start main " + (digestof this).string()) end
      _sim =  Simulator(env)
      
      let graph: Graph = Graph.create_diepenbeek()
      
      for ndname in graph.nodes.values() do
         add(ndname, Node(_sim, env, ndname))
      end

      for (nda, ndb, dist) in graph.edges.values() do
         if _debug then env.out.print("edge " + nda + " " + ndb) end
         _sim.make_edge(nda, ndb, dist)
      end
      
      
      _sim.start()

      /*
      for i in Range(0, 100) do
         _sim.display()
      end
       */
      //_sim.display()
       
      let timers = Timers
      let timer = Timer(Notify(_sim), 5_000_000_000, 2_000_000_000)
      timers(consume timer)

      
   fun add(ndname: String, nd: Node tag) =>
      _sim.add(ndname, nd)
