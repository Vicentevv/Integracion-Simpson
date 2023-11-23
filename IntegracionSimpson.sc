// 1.-

// Funcion de integracion con el metodo Simpson

def integracion(f: Double => Double,a: Int, b:Int): Double = {

  val tam_interv = b - a

  val _x = (a + b) / 2

  val rf = (tam_interv * (f(a) + (4 * f(_x)) + f(b)))/6
  rf
}

// El tipo de dato que devolverá la función integracion: Es Double
// ¿Cuáles son los parámetros que recibe la función?: Los parametros que recibe la funcion son una funcion, a que es un Entero y b que es un Entero

// 2.-

// Integral 1= ∫(-x^2+8x-12) 3 hasta 5

def Integral1 (x:Double):Double = -(x*x)+(8*x)-12

val resultado1 = integracion(Integral1 , 3, 5)
println("Integral 1: "+resultado1)

// Integral 2 = ∫(3x^2) 0 hasta 2

def Integral2 (x:Double):Double = 3 * x * x

val resultado2 = integracion(Integral2 , 0, 2)
println("Integral 2: "+resultado2)

// Integral 3 = ∫(x+2x^2-x^3+5x^4) -1 hasta 1

def Integral3 (x:Double):Double =  x + 2 * (x * x) - (x * x * x) + 5 * (x * x * x * x)

val resultado3 = integracion(Integral3, -1, 1)
println("Integral 3: "+resultado3)

// Integral 4 =  ∫(2x-1)/(x^2+x) 1 hasta 2

def Integral4 (x:Double):Double = ((2 * x) + 1) / ((x * x) + x)

val resultado4 = integracion(Integral4, 1, 2)
println("Integral 4: "+resultado4)

// Integral 5 = ∫(e^x) 0 hasta 1

def Integral5 (x:Double):Double = Math.exp(x)

val resultado5 = integracion(Integral5, 0, 1)
println("Integral 5: "+resultado5)

// Integral 6 = ∫(1 / (x-1)^1/2) 2 hasta 3

def Integral6 (x:Double):Double = 1/ (x-1)*Math.pow(1,0.5)

val resultado6 = integracion(Integral6, 2, 3)
println("Integral 6: "+resultado6)

// Integral 7 = ∫(1 / (1 + x^2) 0 hasta 1

def Integral7 (x:Double):Double = 1 / (1 + (x*x))

val resultado7 = integracion(Integral7, 0, 1)
println("Integral 7: "+resultado7)

// 3.-

// Calcular el margen de error (valorEsperado - valorObtenido)

def calcularError(valorEsperado: Double, valorObtenido: Double): Double = {
  math.abs(valorEsperado - valorObtenido)
// Utilizamos "math.abs" para obtener el valor absoluto (que el valor de respuesta no sea negativo)
}

val error1 = calcularError(7.33, resultado1)
println("El margen de error en la Integral 1 es de: " + error1)

val error2 = calcularError(8, resultado2)
println("El margen de error en la Integral 2 es de: " + error2)

val error3 = calcularError(3.333, resultado3)
println("El margen de error en la Integral 3 es de: " + error3)

val error4 = calcularError(1.09861, resultado4)
println("El margen de error en la Integral 4 es de: " + error4)

val error5 = calcularError(1.71828, resultado5)
println("El margen de error en la Integral 5 es de: " + error5)

val error6 = calcularError(0.828427, resultado6)
println("El margen de error en la Integral 6 es de: " + error6)

val error7 = calcularError(0.785398, resultado7)
println("El margen de error en la Integral 7 es de: " + error7)