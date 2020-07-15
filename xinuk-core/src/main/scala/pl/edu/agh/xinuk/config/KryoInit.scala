package pl.edu.agh.xinuk.config

import com.esotericsoftware.kryo.Kryo
import pl.edu.agh.xinuk.model._

final class KryoInit {
  def customize(kryo: Kryo): Unit = {
    kryo.register(classOf[Array[Cell]])
    kryo.register(classOf[SignalMap])
    kryo.register(classOf[Array[Signal]])
  }
}
