/*
 *  Should report an error that an integer value cannot be assigned to
 *  a variable expecting a real number.
 */

var x:real;
var y:int;
begin
    x := 2 * 3 + 4;
    y := 2 * (3+4);
    print(x);
end
