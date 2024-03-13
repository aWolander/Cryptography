(*Implementation of the Miller Rabin primality test. 
Takes as input a number n to be tested and the sought after certainty for it to be prime*)

MillerRabin[n_, GoalProb_] := 
  Module[{Composite, q, a, k, T, aTable, j, x, i},
   Composite = False;
   (*Using estimation based on the prime number theorem for number of intergers to check for sought after certainty of primality*)
   T = Floor[Log[Log[n]/(1 - 0.999999)]*(1/Log[4])] + 1;
   If[Mod[n, 2] == 0, Composite = True;, 
    (*I do it like this so that I avoid repeated a's:*)
    aTable = RandomSample[Range[2, 1000*T], T];
    k = 0;
    While[IntegerQ[(n - 1)/(2^(k + 1))], k++];
    q = (n - 1)/(2^k);
    For[j = 1, j <= Length[aTable], j++,
     a = aTable[[j]];
     If[1 < GCD[a, n] < n, Composite = True; 
      Break[];];(*a is a witness*);
     x = PowerMod[a, q, n];
     If[Mod[x, n] == 1, Continue[];];(*Test failed for a*);
     For[i = 0, i < k, i++,
      If[PowerMod[x, 2^i, n] == Mod[-1, n], 
         Break[]];(*Test failed for a*);
       If[i == k - 1, Composite = True;];(*a is a witness*);
      ];
     ];
    ];
   If[Composite == False, 
    Print["I am over ", PercentForm[N[1 - Log[n]/(4^T), 10]], 
     " sure that N is prime"], Print["N is not prime"]]
   ];
