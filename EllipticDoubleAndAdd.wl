(*Implementation of the Double-and-Add algorithm for fast multiplication on an elliptic curves. 
Takes as input a point {a,b}, the scalar n it should be multiplied by and the elliptic curve y^2=x^3+ax+b. 
It optionally takes the input p to specify the field over which the curve exists.
Uses EllipticAdd[] from another file in this depository*)

DoubleAndAdd[n_, P_, {a_, b_}, p_ : 0] := 
 Module[{nbin, r, Q, QList, R, i},
  nbin = Reverse[IntegerDigits[n, 2]];
  r = Length[nbin];
  Q = P;
  QList = {Q};
  For[i = 1, i < r, i++,
   Q = EllipticAdd[Q, Q, {a, b}, p];
   AppendTo[QList, Q];
   ];
  R = {p + 1, p + 1};
  For[i = 1, i <= r, i++,
   If[nbin[[i]] == 1,
     If[R == {p + 1, p + 1},
       R = QList[[i]];,
       R = EllipticAdd[R, QList[[i]], {a, b}, p];
       ];
     ];
   ];
  R]
