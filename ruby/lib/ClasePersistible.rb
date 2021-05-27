require_relative 'Util'

module ClasePersistible

  include Util

  # TODO te evitas inicializarlo en todos lados o chequear el nil
  # attr_reader :atributos_persistibles
  def atributos_persistibles
    @atributos_persistibles ||= {}
  end

  attr_accessor :tabla

  def has_one(tipo_atributo, params)
    # TODO "params" adentro tiene ":named" (params te llega como un mapa / Hash de ruby)
    agregar_atributo(tipo_atributo, params)

    # TODO es mejor hacer una abstracción de tus atributos persistibles (una clase)
    # y luego guardarlas en una colección (hash o array) para que puedas delegar la lógica ahí
    # sin tener que acordarte que @atributos_persistibles[:has_many] dice cuales son los que son "many"
    # y que @atributos_persistibles[:un_nombre] contiene un tipo.
    # Si por ejemplo tenés que agregar validaciones, vas a tener que tener la lógica de "if es compuesto"
    # por todos los lugares donde lo necesites usar.
    # (es decir, es más dificil extenderlo / hacer cambios)
    #
    # Podrías guardar algo como AtributoSimple.new(nombre, tipo, config) o
    # AtributoCompuesto.new(nombre, tipo, config)
  end

  def has_many(tipo_atributo, params)
    agregar_atributo(tipo_atributo, params)
    # TODO aca @atributos_persistibles podría ser nil si antes no llamaste al has_one
    #@atributos_persistibles[:has_many] ||= []
    #@atributos_persistibles[:has_many].push(params[:named])

    atributos_persistibles[:has_many] ||= []
    atributos_persistibles[:has_many].push(params[:named])
  end

  def agregar_atributo(tipo_atributo, params)
    attr_accessor params[:named]
    # TODO lo inicializa el getter
    # @atributos_persistibles ||= {}
    atributos_persistibles[params[:named]] = tipo_atributo
  end

  #def definir_getter(named)
  #  send(:define_method, named) do
  #    obj.instance_variable_set("@#{named.to_s}".to_sym, [])
  #  end
  #end

  def analizar_ancestros
    # TODO una forma más simple de hacer un algoritmo similar es hacer algo así:
    # atributos_persistibles.merge(super)
    # y atrapando cuando super no tenga "analizar_ancestros"
    #
    # o sino haciendo algo como:
    # padre = ancestors.find { |a| a.is_a?(ClasePersistible) }
    # if padre != nil
    # atributos_persistibles.merge(padre.atributos_persistibles)
    # else
    # atributos_persistibles
    ancestros = []
    ancestors.each do |ancestro|
      break if ancestro == ORM
      ancestros.push(ancestro) if ancestro.is_a?(ClasePersistible)
    end
    ancestros.delete_at(0)
    # TODO no deberías necesitar agregar atributos de los ancestros
    agregar_atributos_de_ancestros(ancestros) if ancestros.size > 0
    self
  end

  def agregar_atributos_de_ancestros(ancestros)
    ancestros.reverse!
    atr_persistibles_original = @atributos_persistibles.clone
    ancestros.each { |modulo| agregar_atributos_de(modulo.atributos_persistibles) }
    agregar_atributos_de(atr_persistibles_original)
    # TODO aca te sigue persiguiendo el :has_many, si no lo abstraes te va a volver loco
    # agregar nuevos tipos de relaciones
    @atributos_persistibles[:has_many] = @atributos_persistibles[:has_many].uniq if @atributos_persistibles[:has_many]
    self
  end

  def agregar_atributos_de(hash_atributos)
    # TODO no deberías necesitar hacer esto porque tu instancia ya es hija
    # de la que definió el "has_many" o "has_one"
    atr_persistibles_sin_has_many(hash_atributos).each do |nombre, tipo|
      if es_atributo_has_many(hash_atributos, nombre)
        has_many(tipo, named: nombre)
      else
        has_one(tipo, named: nombre)
      end
    end
    self
  end

  def inicializar_tabla
    @tabla = TADB::DB.table(name)
    analizar_ancestros # tambien agrega atributos de clases padre
    self
  end

  def insertar_en_tabla(hash)
    @tabla.insert(hash)
  end

  def borrar_de_tabla(id)
    @tabla.delete(id)
    self
  end

  def hash_atributos_persistidos(id)
    @tabla.entries.each{ |entrada| return entrada if entrada.has_value?(id) }
    nil
  end

  def all_instances
    if @tabla
      all_instances_de_hijos + @tabla.entries.map { |entrada| generar_instancia(entrada) }
    else
      if is_a?(Class)
        []
      else
        all_instances_de_hijos
      end
    end
  end

  def all_instances_de_hijos
    array_aux = []
    modulos_hijos.each { |modulo| array_aux = array_aux + modulo.all_instances }
    array_aux
  end

  def generar_instancia(entrada_de_tabla)
    instancia = self.new
    instancia.send(:id=, entrada_de_tabla[:id])
    instancia.settear_atributos
  end

  #naturalmente falla si el metodo tiene aridad != 0, porque asi esta definido en respond_to_missing?
  def method_missing(mensaje, *args, &bloque)
    # TODO no te conviene usar "respond_to?" (porque matchea con más cosas que solo "find_by_*")
    if respond_to?(mensaje, false)
      all_instances.select { |instancia| instancia.send(sin_find_by_(mensaje)) == args[0] }
    else
      super
    end
  end

  def respond_to_missing?(mensaje, priv = false)
    instancia = self.new
    # TODO podés preguntar por "si empieza con "find_by_" y luego el resto de las condiciones
    # (sino podés estar matcheando otras cosas)
    # [3] pry(main)> :pepe.to_s.gsub("find_by_", "").to_sym
    # => :pepe
    if instancia.respond_to?(sin_find_by_(mensaje), false)
      metodo = instancia.method(sin_find_by_(mensaje))
      metodo.arity == 0 || super
    else
      super
    end
  end

  def sin_find_by_(mensaje)
    mensaje.to_s.gsub("find_by_", "").to_sym
  end

end
