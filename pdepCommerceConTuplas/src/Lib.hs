{-
take :: Int -> String -> String
drop :: Int -> String -> String
head :: String -> Char
elem :: Char -> String -> Bool
reverse :: String -> String
-}

type Producto = (String, Float)

nombreDeProducto :: Producto -> String
nombreDeProducto unProducto = fst unProducto

precioDeProducto :: Producto -> Float
precioDeProducto unProducto = snd unProducto

--aplicarCostoDeEnvio: Dado un precio y un costo de envío, obtener el precio final una vez sumado el costo de envío.
aplicarCostoDeEnvio :: Producto -> Float -> Float
aplicarCostoDeEnvio unProducto costoEnvio = precioDeProducto unProducto + costoEnvio

--aplicarDescuento: Dado un precio y un descuento, obtener el precio final con el descuento aplicado.
aplicarDescuento :: Producto -> Float -> Float
aplicarDescuento unProducto dto = precioDeProducto unProducto - dto

-- precioTotal: Dado un precio unitario, una cantidad, un descuento y un costo de envío calcular el precio total. Para eso, hay que calcular el precio unitario con descuento y multiplicarlo por la cantidad. ¡No te olvides de agregar el precio del envío!
precioTotal :: Producto -> Float -> Float -> Float -> Float
precioTotal (unNombre, unPrecio) cantidad descuento costoDeEnvio =
  aplicarCostoDeEnvio (unNombre, aplicarDescuento (unNombre, unPrecio) descuento * cantidad) costoDeEnvio

--entregaSencilla: Una entrega es sencilla, si se hace en un día sencillo. Los días sencillos son lo que tienen una cantidad de letras par en el nombre. Ejemplo de un día: “20 de Abril de 2020”.
entregaSencilla :: String -> Bool
entregaSencilla dia = (even . length) dia

--productoDeLujo: Dado el nombre de un producto, saber si es de lujo. Un producto es de lujo cuando contiene una “x” o “z” en su nombre.
productoDeLujo :: Producto -> Bool
productoDeLujo unProducto = (elem 'x' . nombreDeProducto) unProducto || (elem 'z' . nombreDeProducto) unProducto

--productoCodiciado: Dado el nombre de un producto, saber si es un producto codiciado. Un producto es codiciado cuando la cantidad de letras en su nombre es mayor a 10.
productoCodiciado :: Producto -> Bool
productoCodiciado unProducto = (length . nombreDeProducto) unProducto > 10

--descodiciarProducto: Dado el nombre de un producto, generar uno que no sea codiciado. Para esto le vamos a sacar las últimas letras hasta que la cantidad de letras en el nombre quede igual a 10 (ó menor a 10 en productos con nombres cortos)
descodiciarProducto :: Producto -> String
descodiciarProducto unProducto = take 10 . nombreDeProducto $ unProducto

--versionBarata: Dado el nombre de un producto conseguir su versión barata. La misma es el producto descodiciado y con su nombre dado vuelta.
versionBarata :: Producto -> Producto
versionBarata (unNombre, unPrecio) = ((reverse . descodiciarProducto) (unNombre, unPrecio), unPrecio)

--productoXL: Dado un producto, conseguir su versión XL. Esta se consigue agregando ‘XL’ al final del nombre.
productoXL :: Producto -> Producto
productoXL (unNombre, unPrecio) = (unNombre ++ "XL", unPrecio)

--productoCorriente: Dado el nombre de un producto, saber si es un producto corriente. Un producto es corriente si la primera letra de su nombre es una vocal.
productoCorriente :: Producto -> Bool
productoCorriente unProducto = esVocal . head . nombreDeProducto $ unProducto

esVocal :: Char -> Bool
esVocal letra = elem letra "aeiouAEIOU"

--productoDeElite: Un producto es de elite si es de lujo, codiciado y no es un producto corriente.
productoDeElite :: Producto -> Bool
productoDeElite unProducto = productoDeLujo unProducto && productoCodiciado unProducto && (not . productoCorriente) unProducto