module type K_Max = sig
  type 'a queue

  val init : int -> 'a queue

  val put : 'a -> 'a queue -> 'a queue

  val getmax : 'a queue -> 'a
end

module type Dequeue = sig
  type 'a dequeue

  val empty : 'a dequeue

  val putf : 'a -> 'a dequeue -> 'a dequeue

  val pute : 'a -> 'a dequeue -> 'a dequeue

  val lookf : 'a dequeue -> 'a

  val looke : 'a dequeue -> 'a

  val removef : 'a dequeue -> 'a dequeue

  val removee : 'a dequeue -> 'a dequeue

  exception Empty
end

module Dequeue: Dequeue = struct 

end

module K_Max_Queue : K_Max = struct
  type 'a queue = {q: ('a * int) deque,time:int,k:int}


end
