|%
::  $nudge:  a reminder about something in the future
::
::    $id: a unique identifier for the nudge
::    $fires: when the nudge was last fired, and how many times
::    $status: the status of the nudge
::    $owner: the person who created the nudge
::    $current: the current time used to backreference timers
::    $scheme: the contents and schedule of the nudge
::
+$  nudge
  $:  =id
      =fires
      =status
      owner=ship
      current=(unit time)
      scheme=(rev scheme)
  ==
::  $scheme: the contents and schedule of the nudge
::
::    $schedule: a plan for when to fire the nudge, possibly repeating
::    $note: what the nudge is about
::    $start: when the nudge was created
::    $squad: the people to be nudged, not including self
::    $name: optional name of the nudge
::    $image: optional visual to represent the nudge
::
+$  scheme
  $:  =schedule
      note=cord
      squad=(unit squad)
      name=(unit cord)
      image=(unit cord)
  ==
::
+$  nudge-action
  $%  [%create =scheme]
      [%update =id =scheme]
      [%delete =id]
      [%enable =id]
      [%disable =id]
      [%dismiss =id]
      [%snooze =id next=(unit @dr)]
  ==
::
+$  id      @uv
+$  nudges  (map id nudge)
+$  timers  (map time id)
::  $schedule: a plan for when to fire the nudge, possibly repeating
::
::    $time: when to fire initially, also used to calculate repeats
::    $repeat: how and when to repeat the nudge, until specified 
::
+$  schedule
  $:  =time
      repeat=(unit repeat)
  ==
::
::  $repeat: how and when to repeat the nudge, until specified
::
::    $every: repeat every interval of time
::    $day: repeat every X days at same time every repeat
::    $week: repeat every X weeks on certain weekdays
::    $month: repeat every X months on certain days
::    $year: repeat every X years, on date of initial nudge
::
+$  repeat
  $:  =until
      $=  plan
      $%  [%every every=@dr]
          [%day per=@ud]
          [%week per=@ud on=(set weekday)]
          [%month per=@ud on=month-on]
          [%year per=@ud]
      ==
  ==
::
+$  month-on
  $%  [%day day=@ud]
      [%wdom day=weekday week=?(%first %second %third %fourth %last)]
  ==
::
::  $until: when to stop repeating
+$  until
  $~  [%never ~]
  $%  [%never ~]
      [%time time=@da]
      [%count count=@ud]
  ==
::
+$  fires
  $:  last=(unit time)
      count=@ud
  ==
::  $status: the status of the nudge
::
::    $inert: the nudge is inactive
::    $ready: the nudge is active and ready to fire
::    $fired: the nudge has fired
::    $dismissed: the nudge has been dismissed
::    $snoozed: the nudge has been snoozed for a duration
::
+$  status
  $~  [%ready ~]
  $%  [%inert ~]
      [%ready ~]
      [%fired ~]
      [%dismissed ~]
      [%snoozed next=(unit @dr)]
  ==
::  $squad: the people to be nudged
+$  squad  (set ship)
::  $weekday: a day of the week
+$  weekday
  $~  %sunday
  $?  %sunday
      %monday
      %tuesday
      %wednesday
      %thursday
      %friday
      %saturday
  ==
::
+$  state-0
  $:  %0
      =nudges
      =timers
      snooze=_~m5
  ==
++  rev
  |$  [data]
  [rev=@ud data]
::
++  apply-rev
  |*  [old=(rev) new=(rev)]
  ^+  [changed=& old]
  ?:  (lth rev.old rev.new)
    &+new
  |+old
::
++  next-rev
  |*  [old=(rev) new=*]
  ^+  [changed=& old]
  ?:  =(+.old new)
    |+old
  &+old(rev +(rev.old), + new)
--