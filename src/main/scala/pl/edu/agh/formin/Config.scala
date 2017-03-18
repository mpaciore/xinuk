package pl.edu.agh.formin

import pl.edu.agh.formin.model.{Energy, Signal}

/*
FSE - foraminifera start energy; FSE ∈ [0,1] && FSE ∈ R.
FRC - foraminifera reproduction cost; FRC ∈ [0,1] && FRC ∈ R.
FRT - foraminifera reproduction threshold; FRT ∈ [0,1] && FRT ∈ R.
FLAC - foraminifera life activities(vegetation and movement) cost; FLAC ∈ [0,1] && FLAC ∈ R.
ARF - algae reproduction frequency; ARF ∈ N.
AEC - algae energetic capacity; AEC ∈ [0,1] && AEC ∈ R.
SSR - signal speed ratio; SSR ∈ N. Foraminifera speed is 1.
DFF - diffraction factor; DFF ∈ [0,1] && DFF ∈ R.
ATF - attenuation factor; ATF ∈ [0,1] && ATF ∈ R.
SPF - global suppression factor of the signal; SPF ∈ [0,1] && SPF ∈ R.
GS - grid size; GS ∈ N, where map size is GSxGS.
SC - spawn chance, SC ∈ [0,1] && SC ∈ R
FAR - foraminifera-algae ratio; FAR ∈ N.
FSSV - foraminifera start signal value; FSSV ∈ [0,1] && FSSV ∈ R.
ASSV - algae start signal value; ASSV ∈ [0,1] && ASSV ∈ R.
 */

case class Config(
                   foraminiferaStartEnergy: Energy,
                   foraminiferaReproductionCost: Energy,
                   foraminiferaReproductionThreshold: Energy,
                   foraminiferaLifeActivityCost: Energy,
                   algaeReproductionFrequency: Int,
                   algaeEnergeticCapacity: Energy,
                   signalSpeedRatio: Int,
                   //diffractionFactor: Double,
                   attenuationFactor: Double,
                   signalSuppresionFactor: Double,
                   gridSize: Int,
                   spawnChance: Double,
                   foraminiferaAlgaeRatio: Double,
                   foraminiferaInitialSignal: Signal,
                   algaeInitialSignal: Signal
                 )