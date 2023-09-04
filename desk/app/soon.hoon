/-  s=soon
/+  default-agent, verb, dbug
^-  agent:gall
=>
  |%
  +$  card  card:agent:gall
  +$  edition
    $%  state-0:s
    ==
  --
=|  state-0:s
=*  state  -
=<
  %+  verb  &
  %-  agent:dbug
  |_  =bowl:gall
  +*  this  .
      def   ~(. (default-agent this %.n) bowl)
      cor   ~(. +> [bowl ~])
  ++  on-init  
    ^-  (quip card _this)
    =^  cards  state
      abet:init:cor
    [cards this]
  ::
  ++  on-save  !>(state)
  ++  on-load
    |=  =vase
    =/  old  !<(edition vase)
    |^
    ?-  -.old
      %0  `this(state old)
    ==
    --
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    =^  cards  state
      abet:(poke:cor mark vase)
    [cards this]
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    =^  cards  state
      abet:(watch:cor path)
    [cards this]
  ::
  ++  on-peek   peek:cor
  ::
  ++  on-leave   on-leave:def
  ++  on-fail    on-fail:def
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    =^  cards  state
      abet:(agent:cor wire sign)
    [cards this]
  ++  on-arvo
    |=  [=wire sign=sign-arvo]
    ^-  (quip card _this)
    =^  cards  state
      abet:(arvo:cor wire sign)
    [cards this]
  --
|_  [=bowl:gall cards=(list card)]
++  abet  [(flop cards) state]
++  cor   .
++  emit  |=(=card cor(cards [card cards]))
++  emil  |=(caz=(list card) cor(cards (welp (flop caz) cards)))
++  give  |=(=gift:agent:gall (emit %give gift))
++  init
  ^+  cor
  cor
++  poke
  |=  [=mark =vase]
  ^+  cor 
  ?+    mark  ~|(bad-poke/mark !!)      
      %noun
    =+  !<(act=nudge-action:s vase)
    ?>  from-self
    ?-  -.act
      %create   nu-abet:(nu-create:nu-core scheme.act)
      %update   nu-abet:(nu-update:(nu-abed:nu-core id.act) scheme.act)
      %delete   nu-abet:nu-delete:(nu-abed:nu-core id.act)
      %enable   nu-abet:nu-enable:(nu-abed:nu-core id.act)
      %disable  nu-abet:nu-disable:(nu-abed:nu-core id.act)
      %dismiss  nu-abet:nu-dismiss:(nu-abed:nu-core id.act)
      %snooze   nu-abet:(nu-snooze:(nu-abed:nu-core id.act) next.act)
    ==
  ::
  ==
:: 
++  watch
  |=  =path
  ^+  cor
  cor
  :: ?+    path  ~|(bad-watch-path/path !!)
  ::   ::   [%~.~ %gossip %source ~]
  ::   :: (give %fact ~ directory+!>(directory))
  :: ==
++  agent
  |=  [=(pole knot) =sign:agent:gall]
  ^+  cor
  ?+    pole  ~|(bad-agent-wire/pole !!)
      ~  cor
  ::
  ==
++  arvo
  |=  [=(pole knot) sign=sign-arvo]
  ^+  cor
  ?+    pole  ~|(bad-arvo-take/wire !!)
      [%timers time=@ ~]
    nu-abet:(nu-timer:nu-core (slav %da time.pole))
  ==
++  peek
  |=  =path
  ^-  (unit (unit cage))
  ?+  path  [~ ~]
    [%x %nudges ~]  ``nudges+!>(nudges)
  ==
:: ++  inflate-io
::   =/  nudges  ~(tap by nudges)
::   |-
++  nu-core
  |_  [=id:s =nudge:s gone=_|]
  ++  nu-core  .
  ++  emit  |=(=card nu-core(cor (^emit card)))
  ++  emil  |=(caz=(list card) nu-core(cor (^emil caz)))
  ++  give  |=(=gift:agent:gall nu-core(cor (^give gift)))
  ++  nu-abet
    ::  always clear current timer and regenerate if necessary
    =.  nu-core  nu-cancel-current
    ?:  gone  cor(nudges (~(del by nudges) id))
    =.  nudges  (~(put by nudges) id nudge)
    ?:  =([%inert ~] status.nudge)  cor
    =/  timer=(unit time)
      ?:  ?=(%snoozed -.status.nudge)
        `(add now.bowl (fall next.status.nudge snooze))
      (resolve-timer fires.nudge +.scheme.nudge)
    ?~  timer   cor
    =.  nudges  (~(put by nudges) id nudge(current timer))
    =.  timers  (~(put by timers) u.timer id)
    (set-timer u.timer)
  ::
  ++  nu-abed
    |=  i=id:s
    nu-core(id i, nudge (~(gut by nudges) i *nudge:s))
  ++  nu-area  `wire`/nudge/(scot %uv id)
  ++  nu-create
    |=  =scheme:s
    ^+  nu-core
    :: todo: send to squad
    =.  id  (end [7 1] (shax eny.bowl))
    nu-core(nudge [id [~ 0] [%ready ~] our.bowl ~ 0 scheme])
  ::
  ++  nu-timer
    |=  =time
    ^+  nu-core
    ::  load id and nudge
    =.  nu-core  (nu-abed (~(got by timers) time))
    =*  scheme  +.scheme.nudge
    ::  shouldn't happen, but no-op if we're disabled
    ?:  ?=(%inert -.status.nudge)  nu-core
    ~&  "%soon: {<note.scheme>} {<now.bowl>}"
    ?:  =(%snoozed -.status.nudge)
      ::  if a snooze fire, we don't want to mess with fire tracking
      =.  status.nudge  [%fired ~]
      nu-core
    ::  update status and fire tracking
    =.  status.nudge        [%fired ~]
    =.  last.fires.nudge    `time
    =.  count.fires.nudge   +(count.fires.nudge)
    ::  let abet handle if we have another timer to fire
    nu-core
  ++  nu-update
    |=  =scheme:s
    ::  clear status and fire tracking if schedule changes
    =/  new-schedule=?  !=(schedule.scheme schedule.+.scheme.nudge)
    =?  fires.nudge  new-schedule  *fires:s
    =?  status.nudge  new-schedule  *status:s
    nu-core(scheme.nudge [+(rev.scheme.nudge) scheme])
  ++  nu-delete   nu-core(gone &)
  ++  nu-enable   nu-core(status.nudge [%ready ~])
  ++  nu-disable  nu-core(status.nudge [%inert ~])
  ++  nu-dismiss  nu-core(status.nudge [%dismissed ~])
  ++  nu-snooze
    |=  next=(unit @dr)
    nu-core(status.nudge [%snoozed next])
  ++  nu-cancel-current
    =/  timer  current.nudge
    ?~  timer  nu-core
    ~&  "%soon: resting {<u.timer>}"
    =.  timers  (~(del by timers) u.timer)
    =.  current.nudge  ~
    (emit %pass /timers/(scot %da u.timer) %arvo %b %rest u.timer)
  --
::
++  set-timer
  |=  =time
  ^+  cor
  =/  behns=(set @da)  (~(gas in *(set @da)) .^((list @da) %bx (scry ~ /timers)))
  ?:  (~(has in behns) time)  cor
  (emit %pass /timers/(scot %da time) %arvo %b %wait time)
++  resolve-timer
  |=  [=fires:s scheme:s]
  ^-  (unit time)
  ?~  last.fires  `time.schedule    :: first fire
  ?~  repeat.schedule      ~        :: no repeat, already fired
  =/  next=time  (next-fire fires u.repeat.schedule)
  (should-fire next fires until.u.repeat.schedule)
::  $next-fire: assumes we've already fired once, does not check if we
::  should fire again
++  next-fire
  |=  [=fires:s =repeat:s]
  ?>  ?=(^ last.fires)
  ?-  -.plan.repeat
    %every  (add u.last.fires every.plan.repeat)
    %day    (add u.last.fires (mul per.plan.repeat ~d1))
    %week   (week-next-fire fires repeat)
    %month  (month-next-fire fires repeat)
  ::
      %year  ::  ignores Feb 29th
    =/  date  (yore u.last.fires)
    (year date(y (add per.plan.repeat y.date)))
  ==
++  month-next-fire
  |=  [=fires:s =repeat:s]
  ^-  time
  ?>  ?=(^ last.fires)
  ?>  ?=(%month -.plan.repeat)
  =/  dt=date  (yore u.last.fires)
  ?-  -.on.plan.repeat
      %day
    (year dt(m (add per.plan.repeat m.dt)))
  ::
      %wdom
    =/  next=date  dt(m (add per.plan.repeat m.dt), t [1 0 0 0 ~[0x0]])
    =/  current-month  m.next
    =/  count  0
    =|  last=@da
    |-
    ::  if we reach the end of the month, then we give last
    ?:  (gth m.next current-month)  last
    =/  match  =((wd-num day.on.plan.repeat) (daws:chrono:userlib next))
    ::  if no match go to next day
    ?.  match  $(next next(d.t +(d.t.next)))
    ::  if looking for first and match, then we're done
    ?:  ?|  =(%first week.on.plan.repeat)
            &(=(+(count) 2) =(%second week.on.plan.repeat))
            &(=(+(count) 3) =(%third week.on.plan.repeat))
            &(=(+(count) 4) =(%fourth week.on.plan.repeat))
        ==
      (year next)
    ::  otherwise continue, adding match to last and incrementing count
    $(last (year next), count +(count))
  ==
++  week-next-fire
  |=  [=fires:s =repeat:s]
  ^-  time
  ?>  ?=(^ last.fires)
  ?>  ?=(%week -.plan.repeat)
  ::  last fire day of week
  =/  today   (daws:chrono:userlib (yore u.last.fires))
  ::  repeat duration
  =/  every  (mul per.plan.repeat ~d7)
  =/  days              (sorted-days on.plan.repeat)
  =/  first-day=@ud     +:(head days)
  =/  last-day=@ud      +:(rear days)
  =/  is-last-day       =(today last-day)
  ::  if we're not on last day we can just add the number of days
  ?.  is-last-day
    =/  days-away  (weekday-distance today on.plan.repeat)
    (add u.last.fires days-away)
  ::  otherwise we need to jump to the next fire week
  ::  get beginning of next fire week
  =/  next-begin  (sub (add u.last.fires every) (mul ~d1 last-day))
  ::  add the number of days until the first fire
  (add next-begin (mul ~d1 first-day))
++  should-fire
  |=  [next=time =fires:s =until:s]
  ^-  (unit time)
  ?+  -.until  `next
    %time   ?.((lth next time.until) ~ `next)
    %count  ?.((lth count.fires count.until) ~ `next)
  ==
::
++  from-self  =(our src):bowl
++  sorted-days
  |=  days=(set weekday:s)
  ^-  (list [weekday:s @ud])
  %+  sort
    %+  turn  ~(tap in days)
    |=  =weekday:s 
    [weekday (wd-num weekday)]
  |=  [[* a=@ud] [* b=@ud]]
  (lth a b)
++  weekday-distance
  |=  [weekday=@ud days=(set weekday:s)]
  ^-  @ud
  =/  ord=(list [weekday:s @ud])  ~(tap by wd-map)
  |-
  ?~  ord  0
  =/  day  -.i.ord
  =/  offset  (sub 7 weekday)
  =/  num  (mod (add +.i.ord offset) 7)
  =/  norm  (mod (add weekday offset) 7)
  ?.  (~(has in days) day)  $(ord t.ord)
  ?.  (gth num norm)  $(ord t.ord)
  num
++  midnight
  |=  =time
  ^-  ^time
  =/  =date  (yore time)
  (year date(t [d.t.date 0 0 0 ~[0x0]]))
::
++  wd-num  
  |=  =weekday:s
  (~(got by wd-map) weekday)
++  num-wd
  |=  num=@ud
  ?>  (lth num 7)
  (head (snag num ~(tap by wd-map)))
++  wd-map
  ^-  (map weekday:s @ud)
  %-  ~(gas by *(map weekday:s @ud))
  :~  [%saturday 0]
      [%sunday 1]
      [%monday 2]
      [%tuesday 3]
      [%wednesday 4]
      [%thursday 5]
      [%friday 6]
  ==
::
++  scry
  |=  [agent=(unit dude:gall) =path]
  ^-  ^path
  =/  ap  ?~(agent /$ /[u.agent])
  ;:  weld
    /(scot %p our.bowl)
    ap
    /(scot %da now.bowl)
    path
  ==
--