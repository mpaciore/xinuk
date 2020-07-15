package pl.edu.agh.xinuk.config

import com.typesafe.config.Config
import net.ceedubs.ficus.FicusInstances
import net.ceedubs.ficus.readers.{ArbitraryTypeReader, ValueReader}
import pl.edu.agh.xinuk.model.Signal

object ValueReaders extends FicusInstances with ArbitraryTypeReader {
  implicit val guiTypeReader: ValueReader[GuiType] =
    new ValueReader[GuiType] {
      override def read(config: Config, path: String): GuiType = GuiType.byName(config.getString(path))
    }
  implicit val signalReader: ValueReader[Signal] =
    new ValueReader[Signal] {
      override def read(config: Config, path: String): Signal = Signal(config.getNumber(path).doubleValue())
    }
}
