require_relative 'Util'

# Se podría simplificar un poco más el modelo si cambias el método "validar" por "call" (termina siendo polimorfico a un proc)

class CompositeValidator
  def initialize(validadores_proc)
    @validadores = validadores_proc
  end

  def call(valor)
    errores = []
    @validadores.each { |v| errores.push( v.call(valor) ) }
    errores.compact
  end
end

class ValidatorsBuilder
  extend Util

  def self.build(tipo_atributo, params)
    validators = []
    validators.push(no_blank) if params[:no_blank]
    validators.push(tipo_de_dato(tipo_atributo))
    validators.push(from(params[:from])) if params[:from]
    validators.push(to(params[:to])) if params[:to]
    validators.push(block_validate(params[:validate])) if params[:validate]
    CompositeValidator.new(validators)
  end

  def self.no_blank
    proc { |v| "NO BLANCK" unless !v.nil? && v != "" }
  end

  def self.tipo_de_dato(tipo_atributo)
    proc do |v|
      if v.is_a?(InstanciaPersistible)
        v.validate!
        nil
      elsif !v.nil? && es_tipo_primitivo(tipo_atributo) && !(v.class <= tipo_atributo) ||
        !v.nil? && !es_tipo_primitivo(tipo_atributo)
        "TIPO DE DATO (Debe ingresar :" + tipo_atributo.name + " )"
      end
    end
  end

  def self.from(from)
    proc { |v| "FROM" unless !v.nil? && v.class <= Numeric && from < v }
  end

  def self.to(to)
    proc { |v| "TO" unless !v.nil? && v.class <= Numeric && to > v }
  end

  def self.block_validate(block)
    proc { |v| "BLOCK VALIDATE" unless !v.nil? && v.instance_eval(&block) }
  end
end
