var x:int;
var y:int;
fun exp(b:int):int
  { var z:int;
    begin if b=0 then z:= 1
	  else z:= x * exp(b-1);
    return z;
    end
};
  begin
    read x;
    read y;
    print exp(y);
  end
