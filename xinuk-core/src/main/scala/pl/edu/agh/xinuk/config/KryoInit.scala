package pl.edu.agh.xinuk.config

import com.esotericsoftware.kryo.Kryo
import pl.edu.agh.xinuk.model._

final class KryoInit {
  def customize(kryo: Kryo): Unit = {
    kryo.register(classOf[Grid.CellArray])
    kryo.register(classOf[Array[GridPart]])
    kryo.register(classOf[Cell.SmellArray])
    kryo.register(classOf[Array[Signal]])
    kryo.register(classOf[Array[BufferCell]])
  }
}
