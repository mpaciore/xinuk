package pl.edu.agh.fortwist.algorithm

import pl.edu.agh.xinuk.algorithm.UpdateTag

object FortwistUpdateTag {

  case object Add extends UpdateTag

  case object Remove extends UpdateTag

  case object Change extends UpdateTag

}
