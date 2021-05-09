describe Prueba do
  let(:prueba) { Prueba.new }

  describe '#materia' do
    it 'debería pasar este test' do
      expect(prueba.materia).to be :tadp
    end

    it 'persistir una clase simple' do
        prueba.save!
        expect(prueba.id).to_not be_nil
    end

    it 'persistir una clase que hereda de otra' do
        atributos = Parcialito.atributos_persistibles.map{|n,v| n}

        expect(atributos).to match_array([:nota, :nombreAlumno, :nombreAyudante])
    end
  end
end