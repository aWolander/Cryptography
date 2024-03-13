(*Performs addition of two points {x1,y1} and {x2,y2} on an elliptic curve y^2=x^3+ax+b. 
The elliptic curve can be specified to be over the field of order p.*)

EllipticAdd[{x1_, y1_}, {x2_, y2_}, {a_, b_}, p_ : 0] := 
 Module[{s, x3, y3},
  If[{x1, y1} == {Infinity, Infinity}, x3 = x2; y3 = y2;,
   If[{x2, y2} == {Infinity, Infinity}, x3 = x1; y3 = y1;,
    If[p == 0,
     If[x1 == x2 && Mod[y1 + y2, p] == 0, x3 = Infinity; 
       y3 = Infinity;,
       If[{x1, y1} == {x2, y2},
        s = Mod[(3*x1^2 + a)*PowerMod[2*y1, -1, p], p]
        ,
        s = Mod[(y2 - y1)*PowerMod[(x2 - x1), -1, p], p]
        ];
       x3 = Mod[s^2 - x1 - x2, p];
       y3 = Mod[s*(x1 - x3) - y1, p]];
     ,
     If[x1 == x2 && y1 == -y2, x3 = Infinity; y3 = Infinity;,
       If[x1 == x2 && y1 == y2,
        s = (3*x1^2 + a)/(2*y1),
        s = (y2 - y1)/(x2 - x1)];
       x3 = s^2 - x1 - x2;
       y3 = -y1 + s*(x1 - x3);];
     ]]];
  {x3, y3}]
