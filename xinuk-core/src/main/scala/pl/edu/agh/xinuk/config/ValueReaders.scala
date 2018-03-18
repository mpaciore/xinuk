package pl.edu.agh.xinuk.config

import com.typesafe.config.Config
import net.ceedubs.ficus.FicusInstances
import net.ceedubs.ficus.readers.{ArbitraryTypeReader, ValueReader}
import pl.edu.agh.xinuk.model.{Energy, Signal}

object ValueReaders extends FicusInstances with ArbitraryTypeReader {
  implicit val guiTypeReader: ValueReader[GuiType] =
    (config: Config, path: String) => GuiType.byName(config.getString(path))
  implicit val signalReader: ValueReader[Signal] =
    (config: Config, path: String) => Signal(config.getNumber(path).doubleValue())
  implicit val energyReader: ValueReader[Energy] =
    (config: Config, path: String) => Energy(config.getNumber(path).doubleValue())
}
