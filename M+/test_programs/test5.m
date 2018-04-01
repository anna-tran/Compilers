var x[2]:int;
fun f(i:int):bool {
    var valid:bool;
    var x:bool;
    begin
        if i >= 2 || i =< 0
            then valid := true
        else x := false;
        return valid && x;
    end
};
begin
    read x[0];
    print f(x[0]);
end
