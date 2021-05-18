require 'c_adapter'

module ClasePersistible

    def inherited(modulo)
        @subclases||=[]
        @subclases << modulo
    end

    def subclasses
        @subclases||{}
    end

    def tabla
        @tabla ||= Tabla.new(self)
        @tabla
    end

    #Enunciado
    def has_one(tipo, named:, default: nil, no_blank: false, from: nil, to: nil)
        attr_accessor named
        @atributos_persistibles||={}
        @atributos_persistibles[named] = AtributoHelper.as_atribute(named, tipo)
    end

    #Enunciado
    def all_instances
        tabla.get_all + subclasses.flat_map{|s| s.all_instances}
    end

    def atributos_persistibles
        propios = @atributos_persistibles || {}
        persistibles_heredados.merge(propios)
    end

    private
    def persistibles_heredados
        (superclass == BasicObject)? {} : superclass.atributos_persistibles
    end

    #Enunciado: Find by
    def method_missing(method, *args)
        if trying_to_find?(method)
            property = parse_find_by(method)
            validar_busqueda(property)
            return find_by(property, *args)
        end

        super
    end

    def respond_to_missing?(*args)
        metodo = args.first
        trying_to_find?(metodo) or super
    end

    # Metodos auxiliares ++++++++++++++++++++++++++++++++

    def trying_to_find?(metodo)
        metodo.to_s.start_with?("find_by_")
    end

    def parse_find_by(mensaje)
        mensaje.to_s.delete_prefix("find_by_").to_sym
    end

    def is_property?(property)
        method_defined?(property) and instance_method(property).arity.eql? 0
    end

    def validar_busqueda(property)
        raise PropertyNotFoundException.new(property, self) unless is_property?(property)
    end

    protected
    def find_by(property, value)
        if atributos_persistibles[property].is_a? AtributoSimple
            return tabla.find_by(property, value) + subclasses.flat_map{|c|c.find_by(property, value)}
            #La idea es que si no hace falta instanciar todos los objetos para hacer la consulta, no lo haga.
            #En particular, para el caso del id, de la otra forma si una clase tuviera un atributo de su mismo tipo,
            #al querer recuperarla de la db se va a hacer find_by_id, quedando en bucle.
        else
            return all_instances.select{|i| i.send(property)==value}
        end
    end
end

class Object
    extend ClasePersistible

    has_one String, named: :id

    #Enunciado
    def save!
        tabla.persist(self)
    end

    #Enunciado
    def forget!
        tabla.remove(self)
        self.id= nil
    end

    #Enunciado
    def refresh!
        tabla.recuperar_de_db(self)

        self
    end

    def valores_persistibles
        self.class.atributos_persistibles
            .map do |nombre, atributo|
                [atributo, self.send(nombre)]
            end
    end

    private
    def tabla
        self.class.tabla
    end
end