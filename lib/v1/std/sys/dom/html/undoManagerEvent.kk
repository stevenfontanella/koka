public module sys/dom/html/undoManagerEvent

// Interface UndoManagerEvent (generated by esidl 0.2.0).

import sys/dom/types

/* safe and unsafe conversion from all types including `any */
external inline maybeUndoManagerEvent  : forall<a> (a) -> maybe<undoManagerEvent>
{ inline "(#1.toString()==='[object UndoManagerEvent]'?std_core.Just(#1):std_core.Nothing)" }
external inline toUndoManagerEvent : forall<a> (a) -> exn undoManagerEvent
{ inline "(#1.toString()==='[object UndoManagerEvent]'?#1:std_core.error('toUndoManagerEvent: cast failed'))" }


// UndoManagerEvent implements UndoManagerEvent (supplemental)
external inline data : (this: undoManagerEvent) -> dom any /* anyGetter */
{ inline "#1.data" }
external inline initUndoManagerEvent : forall<a> (this: undoManagerEvent, typeArg: string, canBubbleArg: bool, cancelableArg: bool, dataArg: a /* anyArg */) -> dom ()
{ inline "#1.initUndoManagerEvent(#2, #3, #4, #5)" }

// UndoManagerEvent extends Event
external inline event : (undoManagerEvent) -> event // upcasting to extended type
{ inline "#1" }
// UndoManagerEvent implements in extension Event (overloading)
external inline type_ : (this: undoManagerEvent) -> dom string
{ inline "#1.type" }
external inline target : (this: undoManagerEvent) -> dom eventTarget
{ inline "#1.target" }
external inline currentTarget : (this: undoManagerEvent) -> dom eventTarget
{ inline "#1.currentTarget" }
external inline eventPhase : (this: undoManagerEvent) -> dom eventPhase
{ inline "#1.eventPhase" }
external inline stopPropagation : (this: undoManagerEvent) -> dom ()
{ inline "#1.stopPropagation()" }

external inline stopImmediatePropagation : (this: undoManagerEvent) -> dom ()
{ inline "#1.stopImmediatePropagation()" }

external inline bubbles : (this: undoManagerEvent) -> dom bool
{ inline "#1.bubbles" }
external inline cancelable : (this: undoManagerEvent) -> dom bool
{ inline "#1.cancelable" }
external inline preventDefault : (this: undoManagerEvent) -> dom ()
{ inline "#1.preventDefault()" }

external inline defaultPrevented : (this: undoManagerEvent) -> dom bool
{ inline "#1.defaultPrevented" }
external inline isTrusted : (this: undoManagerEvent) -> dom bool
{ inline "#1.isTrusted" }
external inline timeStamp : (this: undoManagerEvent) -> dom domTimeStamp
{ inline "#1.timeStamp" }
external inline initEvent : (this: undoManagerEvent, type_: string, bubbles: bool, cancelable: bool) -> dom ()
{ inline "#1.initEvent(#2, #3, #4)" }

