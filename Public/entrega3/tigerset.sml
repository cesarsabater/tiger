structure tigerset :> tigerset =
struct
 type 'a set = 'a Splayset.set ref
 
 
 exception NoEncontrado
 
 fun newEmpty f = ref (Splayset.empty f)
 
 fun add (s,a) = s := Splayset.add(!s,a)
 
 fun addList (s,l) = s := Splayset.addList(!s,l)
 
 fun delete (s,a) = s := Splayset.delete(!s,a)
 
 fun member (s,a) = Splayset.member(!s,a)
 
 fun isEmpty (s) = Splayset.isEmpty(!s)
 fun notEmpty (s) = not(Splayset.isEmpty(!s))
 
 fun unElem s = let val x = Splayset.find ( fn _ => true) (!s)
   in
      case x of SOME i => i
              | NONE   =>  raise (NoEncontrado)                 
 end
 
 fun app b s = Splayset.app b (!s)
 
 fun fold f e s = Splayset.foldl f e (!s)
 
 fun all p s = Splayset.foldl (fn (x,b) => b andalso (p x)) true (!s)
 
 fun exists p s = Splayset.foldl (fn (x,b) => b orelse (p x)) false (!s)
 
 fun union (a,b) = ref (Splayset.union(!a,!b))
 fun intersection(a,b) = ref (Splayset.intersection(!a,!b))
 fun difference(a,b) = ref (Splayset.difference(!a,!b))
 fun numItems s = Splayset.numItems(!s)
 
end
