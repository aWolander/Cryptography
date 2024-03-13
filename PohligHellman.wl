(*Attempts to solve the DLP g^x=h (mod p) using the Pohlig-Hellman algorithm. 
Uses MultiplicativeOrder to solve in this implementations, 
though it is easily adapted for other algorithms like Shank's.*)

PohligHellman[g_, p_, h_] :=
  Module[{xTable, PieTable, PrimePowersTable, r, pi, ei, gi, hi, xj, 
    gj, j, i, k, hj, dj, x},
   xTable = {};
   PieTable = {};
   PrimePowersTable = FactorInteger[p - 1];
   r = Length[PrimePowersTable];
   For[k = 1, k <= r, k++,
    AppendTo[PieTable, 
      PrimePowersTable[[k]][[1]]^PrimePowersTable[[k]][[2]]];
    ];
   
   For[i = 1, i <= r, i++,
    pi = PrimePowersTable[[i]][[1]];
    ei = PrimePowersTable[[i]][[2]];
    
    gi = PowerMod[g, (p - 1)/pi^ei, p];
    hi = PowerMod[h, (p - 1)/pi^ei, p];
    
    xj = 0;
    gj = PowerMod[gi, pi^(ei - 1), p];
    For[j = 0, j < ei, j++,
     hj = PowerMod[hi*
        PowerMod[gi, -xj, p],
        pi^(ei - 1 - j), p];
     dj = MultiplicativeOrder[gj, p, hj];
     xj = xj + (pi^j)*dj;
     ];
    AppendTo[xTable, xj];
    ];
   x = ChineseRemainder[xTable, PieTable]
   ];
