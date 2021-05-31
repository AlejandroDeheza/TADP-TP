require_relative 'Util'

class ValidadorAtributos

  include Util

  def initialize(params, tipo)
    @params = params
    @tipo_atributo = tipo
  end

  def validar(valor, nombre_clase_error)
    validar_no_blank(valor, nombre_clase_error)
    unless valor.nil?
      validar_tipo(valor, nombre_clase_error).validar_block_validate(valor, nombre_clase_error)
      if @tipo_atributo <= Numeric
        validar_from(valor, nombre_clase_error).validar_to(valor, nombre_clase_error)
      end
    end
    self
  end

  def validar_no_blank(valor, nombre_clase_error)
    if (valor.nil? || valor == "") && @params[:no_blank]
      raise NoBlankException.new(nombre_clase_error, @params[:named])
    end
    self
  end

  def validar_tipo(valor, nombre_clase_error)
    if valor.is_a?(InstanciaPersistible)
      valor.validate!
    elsif es_tipo_primitivo(@tipo_atributo) && !(valor.class <= @tipo_atributo) || !es_tipo_primitivo(@tipo_atributo)
      raise TipoDeDatoException.new(nombre_clase_error, @params[:named], @tipo_atributo)
    end
    self
  end

  def validar_from(valor, nombre_clase_error)
    if @params[:from] && @params[:from] > valor
      raise FromException.new(nombre_clase_error, @params[:named], @params[:from])
    end
    self
  end

  def validar_to(valor, nombre_clase_error)
    if @params[:to] && @params[:to] < valor
      raise ToException.new(nombre_clase_error, @params[:named], @params[:to])
    end
    self
  end

  def validar_block_validate(valor, nombre_clase_error)
    if @params[:validate] && !valor.instance_eval(&@params[:validate])
      raise BlockValidateException.new(nombre_clase_error, @params[:named], @params[:validate])
    end
    self
  end

end