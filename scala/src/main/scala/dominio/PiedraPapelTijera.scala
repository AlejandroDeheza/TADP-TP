package dominio

sealed trait ResultadoPPT {
  val leGanaA: ResultadoPPT
  val probabilidadDeObtencion: Double
}
case object Piedra extends ResultadoPPT {
  lazy val leGanaA: ResultadoPPT = Tijera
  lazy val probabilidadDeObtencion: Double = 0.35
}
case object Papel extends ResultadoPPT  {
  lazy val leGanaA: ResultadoPPT = Piedra
  lazy val probabilidadDeObtencion: Double = 0.25
}
case object Tijera extends ResultadoPPT {
  lazy val leGanaA: ResultadoPPT = Papel
  lazy val probabilidadDeObtencion: Double = 0.4
}
