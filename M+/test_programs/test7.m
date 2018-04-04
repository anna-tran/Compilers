var x[2][3]:int;
var y[2]:int;
var z[1][1]:int;
begin
    x[1] := y;
    x := z;
    x[1][1] := y[2];
%    x := y;
end
