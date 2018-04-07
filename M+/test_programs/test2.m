/*
 *  Should report error that 2 does not match with x[][]
 */

var x[2]:int;      
fun f(x[][]:int):real
{
    begin
        return 2.0;
    end
};
begin
    read x[0]; 
    x[1] := 3;
    print (x[0]);
    print (f(2));
end
