var x[2]:real;      
fun f(b:real):real
    { var z:real;
      begin if x[2] = 0.0 then z:= 1.0
            else z:= x[1] * 2.0;
      return z;
 
     end};
begin
    x[0] := 4.0; 
    read x[1];
    print f(x[0]);
end