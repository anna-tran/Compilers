/*  Should detect that the wrong type of value is being returned */

var x[6][3]:real;
fun a():real
 { 
    var t:real;
    fun b(t1:real):bool
    {
    var t2:real;
    var b2:bool;
    begin
        t2 := 2.0;
        b2 := b(t2);
        t2 := a();
        print x[6];
        return true;
    end
    };
    var i:int;
    begin 
        if b (t)
        then  i := size(x)*size(x[]) 
        else i:= 3; 
        return t;
    end 
};
var ii:int;
begin

print a();
ii:=ceil(3.1);

end
