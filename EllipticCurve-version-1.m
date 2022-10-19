(* ::Package:: *)

(* ::Input:: *)
(*EE:=EllipticCurve[0,0,1,-79,342]*)
(**)
(*WeierstrassModel:={0,0,0,-27*c4,-54*c6}*)
(**)
(*MinWeierstrassEq:=(y^2+EE[[1]]*x*y+EE[[3]]*y-x^3-EE[[2]]*x^2-EE[[4]]*x-EE[[5]])*)
(**)
(*NonextendedModel:=Expand[(x^3+b2/4*x^2+b4/2*x+b6/4)/.x->(x-b2/12)]*)
(**)
(*b2:=EE[[1]]^2+4EE[[2]]*)
(**)
(*b4:=EE[[1]]*EE[[3]]+2EE[[4]]*)
(**)
(*b6:=EE[[3]]^2+4EE[[5]]*)
(**)
(*b8:=EE[[1]]^2*EE[[5]]-EE[[1]]*EE[[3]]*EE[[4]]+EE[[2]]*EE[[3]]^2+*)
(*4*EE[[2]]*EE[[5]]-EE[[4]]^2*)
(**)
(*c4:=b2^2-24b4*)
(**)
(*c6:=-b2^3+36b2*b4-216b6*)
(**)
(*CurveDiscriminant:=-b2^2*b8+9b2*b4*b6-8b4^3-27b6^2*)
(**)
(*Delta:=CurveDiscriminant*)
(**)
(*JInvariant:=c4^3/Delta*)
(**)
(*BadReductionPrimes:=If[#[[2]]>1,#[[1]],1]&/@FactorInteger[Delta]*)
(**)
(*MultReductionPrimes:=If[#[[2]]==1,#[[1]],1]&/@FactorInteger[Delta]*)
(**)
(*SplitMultReduction[p_]:=(PowerMod[Mod[-c6,p],(p-1)/2,p]==1)*)
(**)
(*SSplitMultReduction:=SplitMultReduction[#]&/@MultReductionPrimes*)
(**)
(*GoodReductionPrimes:=If[#[[2]]==0,#[[1]]]&/@FactorInteger[Delta]*)
(**)
(*AuxRealPeriod[aa4_,aa6_]:=*)
(*Re[(t/.FindRoot[WeierstrassP[t,{-4*aa4,-4*aa6}]==*)
(*Max[Select[x/.NSolve[x^3+aa4*x+aa6==0,x],Im[#]==0&]],{t,0.1}])*2]*)
(**)
(*RealPeriod:=If[Delta>0,2,1]**)
(*AuxRealPeriod[Delete[Take[*)
(*Reverse[CoefficientList[Expand[(x^3+b2/4*x^2+b4/2*x+b6/4)/.x->(x-b2/12)],x]],*)
(*{-2,-1}],0]]*)
(**)
(*PointReflect[{x1_,y1_}]:=*)
(*Flatten[{{x1,y1}[[1]],y/.Solve[{(MinWeierstrassEq/.x->x1)==0,y!=y1},y]}]*)
(**)
(*PointCurveNegAdd[{x1_,y1_},{x2_,y2_}]:=Flatten[{x,y}/.Solve[{MinWeierstrassEq==0,y-y1==(y2-y1)/(x2-x1)*(x-x1),*)
(*x!=x1,x!=x2},{x,y}]]*)
(**)
(*PointNegativeDouble[{x1_,y1_}]:=Flatten[{x,y}/.Solve[{MinWeierstrassEq==0,*)
(*y-y1==-((D[MinWeierstrassEq,x]/D[MinWeierstrassEq,y])/.(x->x1)/.(y->y1))**)
(*(x-x1),x!=x1},{x,y}]]*)
(**)
(*CurveAdd[{x1_,y1_},{x2_,y2_}]:=Which[x1==0,{x2,y2},x2==0,{x1,y1},x1!=x2,*)
(*PointReflect[PointCurveNegAdd[{x1,y1},{x2,y2}]],{x2,y2}=={x1,y1},*)
(*PointReflect[PointNegativeDouble[{x1,y1}]],{x2,y2}==PointReflect[{x1,y1}],{0,0}]*)
(**)
(*CurveMulNum[{x1_,y1_},n_]:=*)
(*Which[n==0,{0,0},n==1,{x1,y1},n>1,CurveAdd[{x1,y1},CurveMulNum[{x1,y1},n-1]],*)
(*True,PointReflect[CurveMulNum[{x1,y1},-n]]]*)
(**)
(*CurveRecursDouble[{x1_,y1_},n_]:=CurveRecursDouble[{x1,y1},n]=*)
(*If[n==0,{x1,y1},CurveAdd[CurveRecursDouble[{x1,y1},n-1],*)
(*CurveRecursDouble[{x1,y1},n-1]]]*)
(**)
(*NEp[p_]:=1+Length[Solve[y^2+EE[[1]]*x y+EE[[3]]*y==x^3+EE[[2]]*x^2+EE[[4]]*x+EE[[5]],*)
(*{x,y},Modulus->p]]*)
(**)
(*ProductL[p_,s_]:=Which[Mod[Delta,p]!=0,1-(p+1-NEp[p])*p^(-s)+p^(1-2s),*)
(*Mod[Delta,p^2]==0,1,Mod[Delta,p]==0&&PowerMod[Mod[-c6,p],(p-1)/2,p]==1,*)
(*1-p^(-s),Mod[Delta,p]==0&&PowerMod[Mod[-c6,p],(p-1)/2,p]==p-1,1+p^(-s)]*)
