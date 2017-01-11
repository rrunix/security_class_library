Alf= "ABCDEFGHIJKLMNOPQRSTUVWXYZ., "




\\*****************************************//
\\.........................................//
\\.........................................//
\\...........MY OWN FUNCTIONS..............//
\\.........................................//
\\*****************************************//

\\First and second

\\Calculate bank digit, d = 00+d, c.
calcularBankDigit(n) = { Mod(-sum(i=0,9,Mod(2,11)^i*n[i+1]),11)}

\\Calculate isbn
isbndc(n) = {sum(i=1,10,Mod(n[i],11)*i)}

\\Calculate the position digit( in n must be 0)
isbnCalcDigit(n,position) = {
	local(isbn);
	isbn=isbndc(n);
	lift(Mod(-isbn/position, 11))
}

\\Calculate number in lostdigit position (starts with 0)
calculateDNIDigit(number,letternumber,lostdigit) = {
	Mod((letternumber+Mod(-number,23))/(10^lostdigit),23)
}

\\Check letter
isCorrectDNI(number,letternumber) = {
	print(Mod(number,23));
	Mod(number,23)==letternumber;
}


\\Print matrix info
printCodeInfo(G,H,module)= {
	local(inf,rows,cols, minDistance, fixed);
	minDistance = calculateMinDistance(H,module);
	fixed = floor((minDistance-1)/2);
	inf=matsize(G);
	rows=inf[2];
	cols=inf[1];
	print("Number of n               ",rows);
	print("Number of k               ",cols);
	print("q                         ",module);
	print("Number of d               ",minDistance);
	print("Number of errors detected ",minDistance-1);
	print("Number of errors fixed    ",fixed);

}

\\Calculate min distance given H matrix
calculateMinDistance(H,module)= {
	local(i,found,colscombinations,temp,actual,j,result,cols,resultlength,z,actuallength,actualsum,actualvalue);
	cols=matsize(H)[2];
	i=0;
	found = 0;
	while( i <cols && found == 0,
		i++;
		result=listcreate(1);
		temp=listcreate(i);
		colscombinations=combinations(cols,i+1,1,result,temp);
		resultlength=length(colscombinations);
		for(j=1, resultlength,
			actual = colscombinations[j];
			actuallength = length(actual);
			actualsum= H[,actual[1]];
			for(z=2, actuallength,
				actualsum = actualsum + H[,actual[z]]);	
			actualsum=Mod(actualsum,module);
			actualsum=lift(actualsum);
			actualvalue = sum(index=1, length(actualsum), actualsum[index]);
			if(actualvalue == 0,
			found = 1)));
	if(found==0,
		print("No linear combinations found.."));
	i
}

\\Combinations of 1..top elements with size 2
combinations(top,tam,actual,result,actuallist)= {
	local(i);
	if(actual >= tam,
			if(length(actuallist)==tam-1,
				listput(result, actuallist, length(result)+1)),
			for(i=actual, top,
				if(setsearch(actuallist,i)==0,
					listput(actuallist,i,actual);
					result = combinations(top,tam, i + 1, result,actuallist),
			   )
			)
	);
	result
}



\\Generate words
words(G)= {
	local(binaryShift, maxNumber,result, rows, cols,bin);
	cols=matsize(G)[2];
	rows=matsize(G)[1];
	binaryShift=2^(rows+1); \\ shift to avoid binary numbers with less digits
	maxNumber= 2^(rows)-1; \\Max number
	result=matrix(maxNumber+1,cols,j,k,0);
	for(index=0, maxNumber,
		bin=binary(binaryShift+index); \\Get binary number + shift
		result[index+1,]=lift(Mod(matrix(1,rows,j,k,bin[k+2])*G,2))[1,]); \\Undo shift and multiply by G
	result
}



\\THIRD
freqcounter(mensaje)={
	local(i,n,L,frec);
	L=length(Alf);n=length(mensaje);frec=vector(L);
	for(i=1,n,
		c=Vec(mensaje)[i];
		frec[letranum(c)+1]++
	);
	for(i=1,L, print("Letter : ",Vec(Alf)[i]," rep : ", frec[i]," numero ",i-1));
	[Vec(Alf);frec]
}

\\Generate RSA Pair
generateRSAPair(plength, qlength,minN) = {
	local(n,p,q,fi,e,d);
	n=1;
	while(minN > log(n)/log(length(Alf)) && floor(minN) != floor(log(n)/log(length(Alf))),
		p=nextprime(random(10^plength));
		q=nextprime(random(10^qlength));
		n=p*q
	);
	print("log(n)/log(length(Alf)) = "log(n)/log(length(Alf)));
	fi=(p-1)*(q-1);
	e=random(fi);
	while(gcd(e,fi) != 1,e=random(fi));
	d=Mod(e^(-1),fi);d=lift(d);
	print("p,q,n,fi,e,d");
	[p,q,p*q,fi,e,d];
}

\\Generate ElGamal Pair
generateElGamalPair(plength,g) = {
	local(p,a);
	p=nextprime(random(10^plength));
	a=nextprime(random(plength));
	print("p,g,a,alfa");
	lift([p,g,a,Mod(g^a,p)])
}

\\Generate ElGamal Pair
generateElGamalTwoPair(plength,g) = {
	local(p,a,b);
	p=nextprime(random(10^plength));
	a=nextprime(random(plength));
	b=nextprime(random(plength));
	print("p,g,a,alfa,b,beta");
	lift([p,g,a,Mod(g^a,p),b,Mod(g^b,p)])
}


\\Invert a matrix
invertMatrix(M, module) = {
	matadjoint(M)*Mod(matdet(M),module)^(-1)
}


\\Factor small numbers
OLF(x)= {
	local(i);
	i=1;
	while(i<x, if(issquare(ceil(sqrt(i*x))^2%x), return (gcd(x,floor(ceil(sqrt(i*x))-sqrt((ceil(sqrt(i*x))^2)%x)))));i++)
}

\\Generate table of syndroms
generateSyndrom(H)={
	\\works only with 1<=weight
	res=[];
	kn=matsize(H);k=kn[1];n=kn[2];
	print("syndrome - leader");
	for(i=0,n,
		v=binary(2^n+2^i);
		w=vecextract(v,concat("2..",n+1));
		syndrome=H*w~;
		print(syndrome~,w);
	);
}


\\afin brute force, apply later afin crack(for filtering with dictionary)
afinbrute(msg) = {
	local(i,j);
	for(i=0,30, for(j=0,30,print(i," ",j,"    ",cifradoafin(msg,i,j))));
}




\\Calculate Ainv
hillSolver(v,w,numAlf)={vinv=lift(matadjoint(v)*Mod(matdet(v),numAlf)^(-1));A=lift(Mod(w*vinv,numAlf));Ainv = lift(matadjoint(A)*Mod(matdet(A),numAlf)^(-1));Ainv}

afinSolver(c,m,c1,m1) = {
	local(Ainv,A,aprima,ab,a,b,mult);
	A=[m,1; m1,1];
	mult=[c;c1];
	Ainv=invertMatrix(A,length(Alf));
	ab = Ainv * [c;c1];
	ab = lift(Mod(ab,length(Alf)));
	print(ab);
	a=ab[1,1];
	b=ab[2,1];
	aprima= lift(Mod(a,length(Alf))^-1);
	[lift(Mod(aprima,length(Alf))), lift(Mod(-aprima+b,length(Alf)))]
}






\\********************************************************//
\\........................................................//
\\........................................................//
\\..................OTHER FUNCTIONS.......................//
\\........................................................//
\\........................................................//
\\........................................................//
\\********************************************************//

\\ Frecuencias de caracteres en un mensaje
contarfrecuencias(mensaje)={
	local(i,n,L,frec);
	L=length(Alf);n=length(mensaje);frec=vector(L);
	for(i=1,n,
		c=Vec(mensaje)[i];
		frec[letranum(c)+1]++
	);
	[Vec(Alf);frec]
}

distanciaHamming (v,m) = {local(distancia); distancia=0; if(length(v) == length(m), for(i=1, length(v),if(v[i]!=m[i], distancia = distancia + 1,print(" "))),print("no tienen la misma longitud")); distancia }



distanciaMinima(C)={local(m,n,i); m=length(C); n=length(C[1]);resultado=n; for(i=1,m-1, for(j=i+1,m,dis=distanciaHamming(C[i],C[j]);print(dis);if(dis<resultado,resultado=dis)));resultado} 



descodificarDistanciaMinima(C,v)={m=length(C);n=length(C[1]);resultado=(n+1);for(i=1,m,dis=distanciaHamming(C[i],v);print(dis);if(dis==resultado, print("Error, empate en la distancia");i=m,if(dis<resultado, resultado=dis; k=C[i])));print("La minima distancia es", resultado);k} 



fermat(n) = {
	local(x,y);
	x=sqrtint(n)+1;
	while(issquare(x^2-n)==0,x=x+1);
	y=sqrtint(x^2-n);
	[x-y,x+y]
}













\\ Funcio'n que codifica nu'meros en letras segu'n el alfabeto
\\ Quiero que empiece en 0
numletra(i)={Vec(Alf)[i+1]}

\\ Funcio'n que codifica letras en nu'meros
\\ la forma ra'pida seri'a letranum(c)=setsearch(Set(Vec(Alf)),c)-1
\\ pero si el alfabeto tiene espacios, puntos, etc. no me sirve
\\ porque Set los ordena a su manera, punto y espacio antes que las letras

letranum(c)={
	local(continuar,i,resultado);
	continuar=1;i=0;resultado=i;
	while(continuar,
		if(Vec(Alf)[i+1]==c,
			resultado=i;continuar=0,
			i++
		)
	);
	resultado
}

\\ Las siguientes funciones aplican lo anterior a todos los caracteres
\\ de una cadena o a todos los nu'meros de una lista o vector

numletras(lista)={
	local(n,i,res);
	n=length(lista);res="";
	for(i=1,n,
		res=concat(res,numletra(lista[i]))
	);
	res
}

letrasnum(cadena)={
	local(n,i,res);
	n=length(cadena);res=[];
	for(i=1,n,
		res=concat(res,letranum(Vec(cadena)[i]))
	);
	res
}


\\ Cifrado afi'n monoalfabe'tico C=a*M+b mod el alfabeto
cifradoafin(mensaje,a,b)={
	local(L,n,res,i,M,C);
	L=length(Alf);n=length(mensaje);
	res=Vec(mensaje);
	for(i=1,n,
		\\no pude usar el contador i, se me cambia
		\\al llamar a letranum (???)
		M=letranum(res[i]);
		C=Mod(M,L)*a+b; C=lift(C);
		res[i]=numletra(C)
	);
	concat(res)
}

\\ Cifrado de Vigenere: letra a letra es simplemente la suma mod L
\\ la dificultad esta' en clonar la palabra clave varias veces
\\ hasta igualar la longitud del mensaje
\\

cifradovigenere(mensaje,clave,flag=1)={
	\\1 para cifrar, -1 para descifrar, por defecto 1
	local(n,k,L,res,i,r,C1,C2,C);
	n=length(mensaje);k=length(clave);L=length(Alf);
	res=Vec(mensaje);
	for(i=1,n,
		C1=letranum(res[i]);
		r=i%k;if(r==0,r=k);
		C2=letranum(Vec(clave)[r]);
		C=Mod(C1,L)+flag*Mod(C2,L); C=lift(C);
		res[i]=numletra(C)
	);
	concat(res)
	
}

\\ Cifrado de Hill: a la funcio'n se le pasa un mensaje y una matriz
\\ valdra' tanto para cifrar como para descifrar.
\\ Conviene que la longitud del mensaje sea mu'ltiplo del taman~o
\\ de bloque, esto es responsabilidad del usuario

cifradohill(mensaje,A)={
	local(L,n,k,q,i,j,res,v,w,numeros);
	n=length(mensaje); L=length(Alf);
	k=length(A); \\taman~o de bloque
	q=n\k; \\deberi'a ser entero
	numeros=letrasnum(mensaje);res="";
	for(i=0,q-1,
		v=vector(k,j,numeros[i*k+j]); v=v~;
		w=A*v;w=Mod(w,L);w=lift(w);
		res=concat(res,numletras(w))
	);
	res
}




\\CIFRADO Y DESCIFRADO RSA


\\ Funcio'n que expresa un nu'mero en base L, la longitud del alfabeto
\\ Hay que pasarle un k, nu'mero de si'mbolos que queremos,
numlista(N,k)={
    \\so'lo hay que ir combinando cocientes y restos sucesivos mod L
    local(lista,L,N1,i);
    lista=[];L=length(Alf);N1=N;
    for(i=1,k-1,
        lista=concat(N1%L,lista);
        N1=N1\L
    );
    lista=concat(N1,lista)
}

\\ Funcio'n anti'doto de la anterior, convierte lista a nu'mero

listanum(lista)={
    \\Aqui' no hace falta pasarle como para'metro la longitud de la lista
    local(k,L,i);
    k=length(lista);L=length(Alf);
    sum(i=0,k-1,lista[k-i]*L^i)
}

cifrarRSA(mensaje,n,e)={
    local(numeros,letras,k,M,C);
    k=length(mensaje);
    numeros=letrasnum(mensaje);
    M=listanum(numeros);
    C=Mod(M,n)^e;C=lift(C);
    letras=numletras(numlista(C,k+1))
}

descifrarRSA(mensaje,n,d)={
    local(numeros,letras,l,M,C);
    l=length(mensaje);
    numeros=letrasnum(mensaje);
    C=listanum(numeros);
    M=Mod(C,n)^d;M=lift(M);
    letras=numletras(numlista(M,l-1))
}





\\ Funciones de cifrado y descrifrado El Gamal


cifrarelgamal(mensaje,p,g,alfa)={
    local(k,r,s,rletras,sletras,numeros,M,kk);
    kk=length(mensaje);
    numeros=letrasnum(mensaje);
    M=listanum(numeros);
    k=random(p);
    r=Mod(g,p)^k;r=lift(r);
    s=M*Mod(alfa,p)^k;s=lift(s);
    rletras=numletras(numlista(r,kk+1));
    sletras=numletras(numlista(s,kk+1));
    [rletras,sletras]
}

descifrarelgamal(RS,p,g,a)={
    local(k,r,s,rletras,sletras,numeros,M,l);
    rletras=RS[1]; sletras=RS[2];l=length(rletras);
    r=listanum(letrasnum(rletras));
    s=listanum(letrasnum(sletras));
    M=Mod(s,p)*Mod(r,p)^(-a); M=lift(M);
    numeros=numlista(M,l-1);
    numletras(numeros)
}











\\ Firma digital
\\ Para firmar mensajes de A a B se usa la clave privada de A
\\ y la pu'blica de B
firmarRSA(mensaje,nA,dA,nB,eB)={
	local(numeros,letras,k,R,M,S);
	k=length(mensaje); 
	numeros=letrasnum(mensaje);
	M=listanum(numeros);
	print(M);
	R=Mod(M,nA)^dA; R=lift(R);
	S=Mod(R,nB)^eB; S=lift(S);
	letras=numletras(numlista(S,k+1))
}

verificarfirmaRSA(mensaje,nA,eA,nB,dB)={
	local(numeros,letras,l,R,M,S);
	l=length(mensaje); 
	numeros=letrasnum(mensaje);
	S=listanum(numeros); S=lift(S);
	R=Mod(S,nB)^dB; R=lift(R);
	M=Mod(R,nA)^eA; M=lift(M);
	print(M);
	letras=numletras(numlista(M,l-1))
}


cifrarelgamalconfirma(mensaje,p,g,a,beta)={
	local(k1,k2,r1,s1,r2,s2,rletras1,sletras1,rletras2,sletras2,numeros,M,k);
	k=length(mensaje);
	numeros=letrasnum(mensaje);
	M=listanum(numeros);
	k1=random(p);
	r1=Mod(g,p)^k1;r1=lift(r1);
	s1=M*Mod(beta,p)^k1;s1=lift(s1);
	rletras1=numletras(numlista(r1,k+1));
	sletras1=numletras(numlista(s1,k+1));
	k2=0; while(gcd(k2,p-1)!=1,k2=random(p-1));
	r2=Mod(g,p)^k2;r2=lift(r2);
	k2inv=Mod(k2,p-1)^(-1);
	s2=Mod(M-a*r2,p-1)*Mod(k2,p-1)^(-1);s2=lift(s2);
	rletras2=numletras(numlista(r2,k+1));
	sletras2=numletras(numlista(s2,k+1));
	[[rletras1,sletras1],[rletras2,sletras2]]
}

descifrarelgamalconfirma(RS,p,g,alfa,b)={
	local(k,r1,s1,r2,s2,rletras1,sletras1,rletras2,sletras2,numeros,M,l,gm1,gm2);
	rletras1=RS[1][1]; sletras1=RS[1][2];
	rletras2=RS[2][1]; sletras2=RS[2][2];
	l=length(rletras1);
	r1=listanum(letrasnum(rletras1));
	s1=listanum(letrasnum(sletras1));
	r2=listanum(letrasnum(rletras2));
	s2=listanum(letrasnum(sletras2));
	M=Mod(s1,p)*Mod(r1,p)^(-b); M=lift(M);
	numeros=numlista(M,l-1);
	print("El bloque descifrado es: ",numletras(numeros));
	gm1=Mod(g,p)^M; gm1=lift(gm1);
	gm2=Mod(alfa,p)^r2*Mod(r2,p)^s2;gm2=lift(gm2);
	if(gm1==gm2,
		print("Firma verificada OK"),
		print("Fallo en la firma")
	)
}





