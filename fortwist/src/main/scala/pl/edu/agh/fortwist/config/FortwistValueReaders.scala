package pl.edu.agh.fortwist.config

import com.typesafe.config.Config
import net.ceedubs.ficus.FicusInstances
import net.ceedubs.ficus.readers.{ArbitraryTypeReader, ValueReader}
import pl.edu.agh.fortwist.model.Energy

object FortwistValueReaders extends FicusInstances with ArbitraryTypeReader {

  implicit val energyReader: ValueReader[Energy] =
    new ValueReader[Energy] {
      override def read(config: Config, path: String): Energy = Energy(config.getNumber(path).doubleValue())
    }
}
