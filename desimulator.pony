use "collections"
use "promises"
use "random"

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
      edges = [("n1","n2", F64(3)); ("n2","n3", F64(4)); ("n3","n1", F64(5))]
   

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
   let _routes: Array[Path val]

   new create(si: Simulator, env: Env, name: String) =>
      _sim = si
      _env = env
      _name = name
      _neighbours = SetIs[Port val].create()
      _counter = 0
      _routes = Array[Path val]
      _env.out.print("create node " + _name)

   be add_neighbour(n: Agent tag, dist: F64) =>
      _env.out.print("add neighbour " + (digestof this).string() + " " + (digestof n).string() + " " + dist.string())
      _neighbours.set(Port(PathStep(n, dist)))

   be display() =>
      _env.out.print("display Node  "  + _name)
      for po in _neighbours.values() do
         _env.out.print("   port "  + (digestof po.step.agent).string() + " " + po.step.distance.string())
      end
      
   be receive_event(from: Agent tag, event: Event val) =>
      _env.out.print("Node receive_event")
      match event
      | Start =>
         _env.out.print("Node receive_event start")
         // add all the neighbours as route
         for po in _neighbours.values() do
            let pa: Path val = Path([po.step])
            _routes.push(pa)
         end
         sim().start_timer(3000, this, TimerEvent)
      | TimerEvent =>
         _env.out.print("Node receive_event timer event " + _counter.string())
         send_all_routes()
         _counter = _counter + 1
         if _counter < 5 then
            sim().start_timer(3000, this, TimerEvent)
         end
      | let rev: RouteEvent val =>
         _env.out.print("Node receive_event route event")
         _env.out.print("   path size " + rev.path.steps.size().string())
      else
         _env.out.print("Node receive_event other event")
      end
      sim().response(this)

   fun sim(): Simulator tag =>
      _sim

   fun send_all_routes() =>
      _env.out.print("Node send all routes")
      
      // for each port
      for po in _neighbours.values() do
         _env.out.print("   port")
         let neighb = po.step.agent
         for ro in _routes.values() do
            _env.out.print("      route")
            try
               let fi: PathStep val = ro.first()?
               if (digestof fi.agent) != (digestof po.step.agent) then
                  _env.out.print("         send route")
                  
                  let ro2 = 
                  recover val
                     let st  = po.step
                     var ro3 = [st]
                     ro3.concat(ro.steps.values())
                     ro3
                  end
                  sim().send_event(this, po.step.agent, RouteEvent(Path(ro2)) )
               else
                  _env.out.print("         do not send route")
               end
            else
               _env.out.print("         no first")
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
   
   
   new create(env: Env) =>
      _env = env
      env.out.print("start simulator")
      _agents = SetIs[Agent tag].create()
      _names = Map[String, (Agent tag|None)].create()
      _env.out.print("create size " + _agents.size().string())
      _events = List[(Agent tag, Agent tag, Event val)].create()
      _current_time = 0
      _timers = MinHeap[Timed val].create(10)
      _rand = Rand

   be add(agname: String, ag: Agent tag) =>
      _env.out.print("add ag " + agname + " " + (digestof ag).string())
      _agents.set(ag)
      _names.insert(agname, ag)
      _env.out.print("add _agents size " + _agents.size().string())
      _env.out.print("add _names  size " + _names.size().string())

   be addpr(ag: Agent tag, pr: Promise[U32]) =>
      _env.out.print("add ag " + (digestof ag).string())
      _agents.set(ag)
      _env.out.print("add size " + _agents.size().string())
      pr(77)

   be make_edge(aga: String, agb: String, dist: F64) =>
      _env.out.print("make_edge " + aga + " " + agb + " " + dist.string())
      let agea: (Agent tag|None) = _names.get_or_else(aga, None)
      let ageb: (Agent tag|None) = _names.get_or_else(agb, None)
      match (agea, ageb)
      | (let a: Agent tag, let b: Agent tag) => 
         _env.out.print("ag a b gevonden " + aga + " " + agb)
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

   be start() =>
      _env.out.print("Simulator start")
      for ag in _agents.values() do
         send(ag, ag, Start)
      end
      shift_time()
      
   be response(from: Agent tag) =>
      _out_counter = _out_counter - 1
      _env.out.print("Simulator response " + _out_counter.string())
      shift_time()
      
   be start_timer(time: U64, agent: Agent tag, event: Event val) =>
      _env.out.print("Simulator start timer")
      let raval = _rand.next() % 20
      _timers.push(Timed(_current_time + time + raval, agent, event))

   be send_event(from: Agent tag, to: Agent tag, ev: Event val) =>
      send(from, to, ev)

   fun ref send(from: Agent tag, to: Agent tag, ev: Event val) =>
      _events.push((from, to, ev))
      shift_time()
      
   fun ref send_now(from: Agent tag, to: Agent tag, ev: Event val) =>
      _out_counter = _out_counter + 1
      _env.out.print("Simulator send " + _out_counter.string())
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
         _env.out.print("Simulator shift time new time")
         try
            let ti = _timers.pop()?
            send(ti.get_agent(), ti.get_agent(), ti.get_event())
            _current_time = ti.get_time()
            _env.out.print("Simulator shift time current time " + _current_time.string())
         end
      end

actor Main
   """
   Main is the start of the simulation.
   """
   let _env: Env
   let _sim: Simulator
   
   new create(env: Env) =>
      _env       = env
      var a: U32 = 7
      env.out.print("start main " + (digestof this).string())
      _sim =  Simulator(env)
      
      let graph: Graph = Graph.create()
      
      for ndname in graph.nodes.values() do
         add(ndname, Node(_sim, env, ndname))
      end

      for (nda, ndb, dist) in graph.edges.values() do
         env.out.print("edge " + nda + " " + ndb)
         _sim.make_edge(nda, ndb, dist)
      end
      
      _sim.display()
      
      _sim.start()

   fun add(ndname: String, nd: Node tag) =>
      _sim.add(ndname, nd)

   fun addpr(nd: Node tag) =>
      let pr = Promise[U32]
      _env.out.print("promise " + (digestof pr).string()) 
      _sim.addpr(nd, pr)
      pr.next[None](
      recover 
         {
            (nr: U32)(_env) =>
               _env.out.print("ok " + nr.string() + " " + (digestof this).string()) 
         }
      end)
