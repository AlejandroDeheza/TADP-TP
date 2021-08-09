
class IdError < StandardError
  def initialize(nombre_clase)
    super("No es posible ejecutar lo solicitado, el objeto de la clase " + nombre_clase + " no posee id.
            Probablemente deba realizar save! antes")
  end
end

class ValidatorError < StandardError
  def initialize(nombre_clase, atributo, errores)
    super("El atributo \"" +atributo.to_s+ "\" de la clase \"" +nombre_clase+ "\" no cumple con: " +errores.join(", "))
  end
end

=begin
class TipoDeDatoError < StandardError
  def initialize(nombre_clase, atributo, tipo)
    super("El atributo \"" + atributo.to_s + "\" de la clase \"" + nombre_clase +
              "\" no corresponde con el tipo declarado. Debe ingresar :" + tipo.name)
  end
end

class NoBlankError < StandardError
  def initialize(nombre_clase, atributo)
    super("El atributo \"" + atributo.to_s + "\" de la clase \"" + nombre_clase + "\" no esta inicializado.")
  end
end

class FromError < StandardError
  def initialize(nombre_clase, atributo, from)
    super("El atributo \"" + atributo.to_s + "\" de la clase \"" + nombre_clase + "\" debe ser mayor que " + from.to_s)
  end
end

class ToError < StandardError
  def initialize(nombre_clase, atributo, to)
    super("El atributo \"" + atributo.to_s + "\" de la clase \"" + nombre_clase + "\" debe ser menor que " + to.to_s)
  end
end

class BlockError < StandardError
  def initialize(nombre_clase, atributo, proc)
    super("El atributo \"" + atributo.to_s + "\" de la clase \"" + nombre_clase + "\" no cumple con: #{proc}")
  end
end
=end
