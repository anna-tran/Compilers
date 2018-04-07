/* this is a comment */
% this is also a comment
/*
    Should report an error. Cannot assign a value of 2 to an array x without
    given dimensions.
*/


var x[3]:int;
var y[3][2]:int;
begin
    x := 2;
    if (x > 1)
        then print(x)
    else x := 1;

    if (size(x) = size(y))
        then read x
    else read y;
end
