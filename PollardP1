(*Attempts to factor an integer using Pollard's p-1 method*)

Pollardp1[n_] := Module[{a, d, j, Found}, 
   a = 2;
   Found = False;
   For[j = 2, j < n, j++,
    a = PowerMod[a, j, n];
    d = GCD[a - 1, n];
    If[1 < d < n,
     Found = True;
     Return[d];
     Break[];]
    ];
   If[Not[Found], Print["A non-trivial factor could not be found."]]
   ];
