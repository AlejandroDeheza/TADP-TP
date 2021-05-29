require_relative 'Util'

# "CLASE ABSTACTA"

class Atributo

  include Util

  attr_reader :tipo, :nombre  #, :params

  def valor_default
    @params[:default]
  end

  def initialize(tipo, params)
    @nombre = params[:named]
    @tipo = tipo
    @params = params
  end

  def tiene_valor_default(valor)
    valor.nil? && !@params[:default].nil?
  end

  def validar_todo(valor, nombre_clase_error)
    validar_no_blank(valor, nombre_clase_error)
    unless valor.nil?
      validar_tipo(valor, nombre_clase_error)
      validar_block_validate(valor, nombre_clase_error)
      if @tipo == Numeric
        validar_from(valor, nombre_clase_error)
        validar_to(valor, nombre_clase_error)
      end
    end
  end

  def validar_no_blank(valor, nombre_clase_error)
    if (valor.nil? || valor == "") && @params[:no_blank]
      raise NoBlankException.new(nombre_clase_error, @nombre)
    end
  end

  def validar_tipo(valor, nombre_clase_error)
    if @tipo == Boolean
      raise TipoDeDatoException.new(nombre_clase_error, @nombre, @tipo) unless valor.is_a?(Boolean)
    elsif @tipo == Numeric
      raise TipoDeDatoException.new(nombre_clase_error, @nombre, @tipo) unless valor.is_a?(Numeric)
    elsif @tipo == String
      raise TipoDeDatoException.new(nombre_clase_error, @nombre, @tipo) unless valor.is_a?(String)
    else
      if valor.is_a?(InstanciaPersistible)
        valor.validate!
      else
        raise TipoDeDatoException.new(nombre_clase_error, @nombre, @tipo)
      end
    end
  end

  def validar_from(valor, nombre_clase_error)
    if @params[:from] && @params[:from] > valor
      raise FromException.new(nombre_clase_error, @nombre, @params[:from])
    end
  end

  def validar_to(valor, nombre_clase_error)
    if @params[:to] && @params[:to] < valor
      raise ToException.new(nombre_clase_error, @nombre, @params[:to])
    end
  end

  def validar_block_validate(valor, nombre_clase_error)
    if @params[:validate] && !valor.instance_eval(&@params[:validate])
      raise BlockValidateException.new(nombre_clase_error, @nombre, @params[:validate])
    end
  end

  private
  def valor_persistido(instancia)
    instancia.class.hash_atributos_persistidos(instancia.id)[@nombre]
  end

  def setter_generico(instancia, valor_a_settear)
    instancia.send(pasar_a_setter(@nombre), valor_a_settear)
    self
  end

end


# ATRIBUTO SIMPLE <-------------------------------------

class AtributoSimple < Atributo

  def initialize(tipo, params)
    if es_tipo_primitivo(tipo)
      self.extend(SimpleBasico) # se inserta entre la instancia y su clase -> Instancia, SimpleBasico, AtributoSimple
    else
      self.extend(SimpleComplejo)
    end
    super(tipo, params)
  end

end

module SimpleBasico

  def obtener_valor_para_insertar(dato)
    dato
  end

  def settear(instancia)
    setter_generico(instancia, valor_persistido(instancia))
    self
  end

end

module SimpleComplejo

  def obtener_valor_para_insertar(dato)
    dato.save!.id
  end

  def settear(instancia)
    puts "#{instancia}"
    puts "#{@nombre}"
    puts "#{@tipo}"
    setter_generico(instancia, @tipo.find_by_id(valor_persistido(instancia))[0])
    self
  end

end


# ATRIBUTO MULTIPLE <-------------------------------------

class AtributoMultiple < Atributo

  def initialize(tipo, params)
    if es_tipo_primitivo(tipo)
      self.extend(MultipleBasico)
    else
      self.extend(MultipleComplejo)
    end
    super(tipo, params)
  end

  private
  def settear(instancia, &bloque)
    setter_generico(instancia, [])
    array_persistido(instancia).each do |elem|
      setter_generico(instancia, instancia.send(@nombre).push(bloque.call(elem)))
    end
    self
  end

  def array_persistido(instancia)
    if @tipo == Numeric
      valor_persistido(instancia).split(",").map{ |elem| elem.to_i }
    elsif @tipo == Boolean
      valor_persistido(instancia).split(",").map{ |elem| elem == "true" ? true : false }
    else
      valor_persistido(instancia).split(",")
    end
  end

end

module MultipleBasico

  def obtener_valor_para_insertar(dato)
    dato.join(",")
  end

  def settear(instancia)
    super(instancia){ |elem| elem }
  end

end

module MultipleComplejo

  def obtener_valor_para_insertar(dato)
    dato.map{|instancia| instancia.save!.id}.join(",")
  end

  def settear(instancia)
    super(instancia){ |elem| @tipo.find_by_id(elem)[0] }
  end

end