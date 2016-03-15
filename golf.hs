main=interact f
f s=show$mod(foldl(+)0$map i$r$p(read s::Int)) 10007
p n=if(mod n 2)==0 then 2:p(div n 2)else a n 3
  where a n x
          | (mod n x)==0=x:a (div n x) x
          | n==1=[0]
          | True=0:a n (x+2)
i x=f x 0 1
  where f x a b
          | x>0=f (x-1) b (a+b)
          | True=a
r[]=[]
r(x:xs)=x:r(filter(\y->not(x==y)) xs)
