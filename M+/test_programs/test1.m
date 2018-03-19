var x[2]:int;      
fun f(b:int):real
    { var z:real;
      begin if x[2] = 0 then z:= 1
            else z:= x[1] * 2.0;
      return z;
      end};
begin
    x[0] := 4; 
    read x[1];
    print f(x[0]);
end
