require 'd_adapter'
module ORM

    module ModuloPersistible

        def self.extended(modulo) #Se usa desde los mixines
            ModuloPersistible.init(modulo)
        end

        def included(modulo) #Aplica a los mixines que incluyen otros persistibles
            @submodulos ||= []
            @submodulos << modulo

            if modulo.is_a? Class
                modulo.include ObjetoPersistible
            end
        end

        def inherited(modulo) #Aplica a las clases que heredan de clases persistibles
            @submodulos << modulo
            ModuloPersistible.init(modulo)
        end

        def self.init(modulo)
            modulo.instance_eval do
                @submodulos             ||= []
                @atributos_persistibles ||= {}
            end
        end

        def tabla
            @tabla ||= Tabla.new_tabla_unica(self)
        end

        #Enunciado
        def has_one(tipo, args)
            has_attribute(tipo, args, false)
        end

        #Enunciado
        def has_many(tipo, args)
            has_attribute(tipo, args, true)
        end

        private
        def has_attribute(tipo, args, many)
            validar_has_args(args)
            atributo = AtributoHelper.as_attribute(args, tipo, self, many)
            attr_accessor atributo.nombre
            @atributos_persistibles[atributo.nombre] = atributo
        end

        def validar_has_args(args)
            parametrosSobrantes = args.keys - [:named, :default, :from, :to, :no_blank, :validate]
            raise HasArgsException.new(parametrosSobrantes) unless parametrosSobrantes.empty?
        end

        public

        #Enunciado
        def all_instances
            tabla.get_all_instances + @submodulos.flat_map{|s| s.all_instances}
        end

        def atributos_persistibles
            persistibles_heredados.merge(persistibles_incluidos).merge(@atributos_persistibles)
        end

        private
        def persistibles_heredados
            (superclass.is_a? ModuloPersistible)? superclass.atributos_persistibles: {}
        end

        def persistibles_incluidos
            modulos_persistibles.flat_map{|m| m.atributos_persistibles}.reduce(Hash.new, :merge)
        end

        def modulos_persistibles
            included_modules.select {|m| m.is_a?(ModuloPersistible)}
        end

        #Enunciado: Find by
        def method_missing(method, *args)
            if trying_to_find?(method)
                property = parse_find_by(method)
                validar_busqueda(property)
                return find_by(property, *args)
            end
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

        def has_property?(property)
            method_defined?(property) and instance_method(property).arity.eql? 0
        end

        def validar_busqueda(property)
            raise PropertyNotFoundException.new(property, self) unless has_property?(property)
        end

        def entrada_de_tabla?(property)
            atributos_persistibles[property].is_a? AtributoSimple
        end

        public
        def find_by(property, value)
            if entrada_de_tabla?(property)
                return tabla.find_by(property, value) + @submodulos.flat_map{|c|c.find_by(property, value)}
            else
                return all_instances.select{|i| i.send(property)==value}
            end
        end
    end

    #************************** Objeto Persistible ************************

    module ObjetoPersistible
        extend ModuloPersistible

        def self.included(modulo)
            modulo.extend ModuloPersistible
            modulo.has_one String, named: :id
        end

        #Enunciado
        def save!
            set_defaults_on_empty
            save_attributes!
            tabla.save(self)
            save_relations!
            self
        end

        #Enunciado
        def forget!
            each_persistible {|p| p.forget_relaciones!}
            tabla.forget(self)
            self.id= nil
        end

        #Enunciado
        def refresh!
            tabla.refresh(self)
            self
        end

        #Enunciado
        def validate!
            each_persistible do
                |atributo|
                valorActual = atributo.get_from(self)
                atributo.validar_instancia(valorActual)
            end
        end

        private
        def save_attributes!
            each_persistible {|atributo| atributo.persistir_de(self)}
        end

        def save_relations!
            each_persistible { |atributo| atributo.persistir_relaciones(self)}
        end

        def set_defaults_on_empty
            each_persistible {|atr| atr.set_default_on_empty(self)}
        end

        def each_persistible(&block)
            self.class.atributos_persistibles.each_value { |atributo| block.call(atributo) }
        end

        def tabla
            self.class.tabla
        end
    end
end