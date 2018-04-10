

--Ejercicio 1--


esMultiploDeAlguno n lista = any ((==0).(mod n)) lista 
--esMultiplo x y = (mod x y) == 0-- 

------------------------------------------------------------------

--Ejercicio 2--

primeros instMusciales n= take n instMusciales

------------------------------------------------------------------

--Ejercicio 3--

data Alumno = Alumno String [Int]  

nombre (Alumno nombre _ ) = nombre
notas (Alumno _ notas ) = notas

promedio alumno = div (sum (notas alumno)) (length (notas alumno))

aTupla f1 f2 alumno = ( f1 alumno  , f2 alumno)

promediosAlumnos alumnos  =  map (aTupla nombre promedio) alumnos

--Consulta   promediosAlumnos [(Alumno "Juan" [8,6]),(Alumno "Maria" [7,9,4]),(Alumno "Ana" [6,2,4])]
--Respuesta  [("Juan",7),("Maria",6),("Ana",4)]


------------------------------------------------------------------

--Ejercicio 4--
prom notas =  div (sum notas) (length notas)

transformacionConfiltro f1 = f1.(filter (>=6))

promediosSinAplazos planillaDenotas = map (transformacionConfiltro prom ) planillaDenotas

--Consulta  promedioSinAplazos [[8,6],[6,6,4]]
--Respuesta [7,6]
------------------------------------------------------------------

--Ejercicio 5--


aprobo alumno = all (>=6) (notas alumno)

--Consulta  aprobo (Alumno "Manuel" [8,6,2,4])
--Respuesta False
------------------------------------------------------------------

--Ejercicio 6--

--aprobaron alumnos = map nombre (filter aprobo alumnos)
aprobaron = map nombre.(filter aprobo)

--Consulta  aprobaron [(Alumno "Manuel" [8,6,2,4]),(Alumno "Elena" [7,9,6,6]),(Alumno "Ana" [6,2,4,2]),(Alumno "Pedro" [9,6,7,10])]
--Respuesta ["Elena","Pedro"]
------------------------------------------------------------------

--Ejercicio 7--

productos listaProd listaPre = zip listaProd listaPre

--Consulta  productos ["melon","zapallo","palta"] [15,10,12,7]
--Respuesta [("melon",15),("zapallo",10),("palta",12)]
------------------------------------------------------------------

--Ejercicio 8 --

convierteAUno (x,y) = 1

cantidadElementos listaDePares = foldl1 (+) (map convierteAUno listaDePares)

--Consulta cantidadElementos [(8,6),(5,5),(5,6),(7,8)] 
--Respuesta 4
------------------------------------------------------------------

--Ejercicio 9 --




buscaElemento valor empleados = head ( filter ((== valor).snd) empleados)


maxTuplas empleados = maximum (map snd empleados)

masGastador empleados = buscaElemento (maxTuplas empleados) empleados

--Consulta masGastador [("Ana",80),("Pepe",40),("Juan",300),("Maria",120)]
--Respuesta ("Juan",300)
------------------------------------------------------------------

--Ejercicio 10 --

monto empleados = foldl (+) 0 (map snd empleados) 

--Consulta monto [("Ana",80),("Pepe",40),("Juan",300),("Maria",120)]
--Respuesta  540
------------------------------------------------------------------

--buscaElemento 300 [("Ana",80),("Pepe",40),("Juan",300),("Maria",120)]