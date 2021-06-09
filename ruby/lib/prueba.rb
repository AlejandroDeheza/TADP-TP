class Prueba

  def materia
    :tadp
  end
end

#estaba en "ObjetoPersistible"
=begin
  # define metodos y accesors para las clases persistibles
  def self.included(clase)
    clase.singleton_class.send(:attr_reader, :atributos_persistibles)
    clase.singleton_class.send(:attr_accessor, :tabla)
  end
=end

# estaba en "ClasePersistible"
#def definir_getter(named)
#  send(:define_method, named) do
#    obj.instance_variable_set("@#{named.to_s}".to_sym, [])
#  end
#end


# estaba en "ORM" <<<<<<<<<<<<<
#modulo.class_eval do
#  def initialize
#    inicializar_atributos_has_many
#    super
#  end
#end

# Hace lo mismo que arriba
#modulo.send(:define_method, :initialize) do
#  inicializar_has_many
#  super()
#end

# Hace lo mismo que arriba
#modulo.define_singleton_method(:initialize) do
#  inicializar_has_many
#  super()
#end


# estaba en ORM
#modulo.incluye_orm = true
#
=begin
class Module

  attr_accessor :incluye_orm

  def modulos_hijos
    @modulos_hijos ||= []
  end

  def included(modulo)
    if @incluye_orm
      ORM::entregar_dependecias(modulo)
      modulos_hijos.push(modulo)
    end
  end

end

class Class
  def inherited(clase)
    if @incluye_orm
      clase.incluye_orm = true
      modulos_hijos.push(clase)
    end
  end
end
=end


# estaba en AdministradorDeTabla
=begin
def analizar_ancestros
  ancestros = []
  ancestors.each do |ancestro|
    break if ancestro == ORM
    ancestros.push(ancestro) if ancestro.is_a?(EntidadPersistible)
  end
  ancestros.delete_at(0)
  agregar_atributos_de_ancestros(ancestros) if ancestros.size > 0
  self
end

def agregar_atributos_de_ancestros(ancestros)
  ancestros.reverse!
  atr_persistibles_original = atributos_persistibles.clone
  atr_has_many_original = atributos_has_many.clone
  ancestros.each { |modulo| agregar_atributos_de(modulo.atributos_persistibles, modulo.atributos_has_many) }
  agregar_atributos_de(atr_persistibles_original, atr_has_many_original)
  atributos_has_many = self.atributos_has_many.uniq
  self
end

def agregar_atributos_de(hash_atributos, atributos_has_many)
  hash_atributos.each do |nombre, tipo|
    if atributos_has_many.include?(nombre)
      has_many(tipo, named: nombre)
    else
      has_one(tipo, named: nombre)
    end
  end
  self
end
=end


# estaba en ValidadorAtributos
=begin
class ValidadorAtributos

  include Util

  def initialize(params, tipo)
    @params = params
    @tipo_atributo = tipo
  end

  def validar(valor, nombre_clase_error)
    validar_no_blank(valor, nombre_clase_error) if @params[:no_blank]
    unless valor.nil?
      validar_tipo(valor, nombre_clase_error)
      validar_block_validate(valor, nombre_clase_error) if @params[:validate]
      if @tipo_atributo <= Numeric
        validar_from(valor, nombre_clase_error) if @params[:from]
        validar_to(valor, nombre_clase_error) if @params[:to]
      end
    end
    self
  end

  def validar_no_blank(valor, nombre_clase_error)
    if valor.nil? || valor == ""
      raise NoBlankError.new(nombre_clase_error, @params[:named])
    end
    self
  end

  def validar_tipo(valor, nombre_clase_error)
    if valor.is_a?(InstanciaPersistible)
      valor.validate!
    elsif es_tipo_primitivo(@tipo_atributo) && !(valor.class <= @tipo_atributo) || !es_tipo_primitivo(@tipo_atributo)
      raise TipoDeDatoError.new(nombre_clase_error, @params[:named], @tipo_atributo)
    end
    self
  end

  def validar_from(valor, nombre_clase_error)
    if @params[:from] > valor
      raise FromError.new(nombre_clase_error, @params[:named], @params[:from])
    end
    self
  end

  def validar_to(valor, nombre_clase_error)
    if @params[:to] < valor
      raise ToError.new(nombre_clase_error, @params[:named], @params[:to])
    end
    self
  end

  def validar_block_validate(valor, nombre_clase_error)
    unless valor.instance_eval(&@params[:validate])
      raise BlockError.new(nombre_clase_error, @params[:named], @params[:validate])
    end
    self
  end

end
=end


#estaba en ValidadorAtributos
#
=begin
class CompositeValidator
  def initialize(validadores_proc)
    @validadores = validadores_proc
  end

  def call(valor, clase)
    @validadores.each { |v| v.call(valor, clase) }
  end
end

class ValidatorsBuilder
  extend Util

  def self.build(tipo_atributo, params)
    validators = []
    validators.push(no_blank(params[:named])) if params[:no_blank]
    validators.push(validar_tipo(tipo_atributo, params[:named]))
    validators.push(validar_from(params[:from], params[:named])) if params[:from]
    validators.push(validar_to(params[:to], params[:named])) if params[:to]
    validators.push(validar_block_validate(params[:validate], params[:named])) if params[:validate]
    CompositeValidator.new(validators)
  end

  def self.no_blank(nombre_atr)
    proc { |v, clase| raise NoBlankError.new(clase, nombre_atr) unless !v.nil? && v != "" }
  end

  def self.validar_tipo(tipo_atributo, nombre_atr)
    proc do |v, clase|
      if v.is_a?(InstanciaPersistible)
        v.validate!
      elsif !v.nil? && es_tipo_primitivo(tipo_atributo) && !(v.class <= tipo_atributo) ||
        !v.nil? && !es_tipo_primitivo(tipo_atributo)
        raise TipoDeDatoError.new(clase, nombre_atr, tipo_atributo)
      end
    end
  end

  def self.validar_from(from, nombre_atr)
    proc { |v, clase| raise FromError.new(clase, nombre_atr, from) unless !v.nil? && v.class <= Numeric && from < v }
  end

  def self.validar_to(to, nombre_atr)
    proc { |v, clase| raise ToError.new(clase, nombre_atr, to) unless !v.nil? && v.class <= Numeric && to > v }
  end

  def self.validar_block_validate(block, nombre_atr)
    proc { |v, clase| raise BlockError.new(clase, nombre_atr, block) unless !v.nil? && v.instance_eval(&block)}
  end
end
=end


#estaba en Excepciones
