# Xinuk

**Xinuk** is an implementation of a highly scalable agent-based spatial simulation framework, written in Scala. It uses [sbt](https://www.scala-sbt.org/1.x/docs/) build tool.

## Models and scenarios

This repository contains several subprojects. `xinuk-core` contains the core of the framework, while the remaining subprojects contain implementation of a different simulation model and scenario:
- `mock`: randomly moving agents, an implementation serving as an introduction to the framework 
- `fortwist`: habitat of benthic creatures feeding on the algae growing on the seabed
- `rabbits`: predator-prey scenario of stationary, rapidly growing lettuce and mobile rabbits feeding on the lettuce
- `torch`: simplified scenario of fire emergency evacuation
- `urban`: people navigating urban environment, with optional rules of proximity avoidance

Each implemented scenario follows a similar structure. A Main object (e.g. `MockMain`) can be found in the main project package, along with the `reference.conf` file in the corresponding `resources` directory. These files provide all the necessary tools to run the simulation, e.g. using
```bash
sbt mock/run
```
If necessary, the values can be edited directly in the corresponding `.conf` file or overridden with commandline argument, by adding `-Dpath.in.config.file=newValue`, e.g. `-Dmock.config.mockInitialSignal=2` (see [mock config file](mock/src/main/resources/reference.conf)).

## Distributed execution

The framework supports execution in distributed environment:
- exactly one instance of the framework must be manually elected as supervisor by setting `<model-name>.config.isSupervisor=true`,
- each instance must be provided with its own and supervisor's host by setting `clustering.ip` and `clustering.supervisor.ip` (and must be able to reach each other),
- each instance must be provided with the desired total number of instances by setting `akka.cluster.min-nr-of-members`,
- all other simulation parameters should be identical for all instances.

## Related publications

Bujas, J., Dworak, D., Turek, W., & Byrski, A. (2019).\
High-performance computing framework with desynchronized information propagation for large-scale simulations.

Renc, P., Bielech, M., Pęcak, T., Morawiecki, P., Paciorek, M., Turek, W., Byrski, A., & Wąs, J. (2020).\
HPC large-scale pedestrian simulation based on proxemics rules.

Paciorek, M., Bogacz, A., & Turek, W. (2020).\
Scalable signal-based simulation of autonomous beings in complex environments.

Paciorek, M., Bujas, J., Dworak, D., Turek, W., & Byrski, A. (2021).\
Validation of signal propagation modeling for highly scalable simulations.

Paciorek, M., & Turek, W. (2021).\
Agent-based modeling of social phenomena for high performance distributed simulations.

Paciorek, M., Poklewski-Koziełł, D., Racoń-Leja, K., Byrski, A., Gyurkovich, M., & Turek, W. (2021).\
Microscopic simulation of pedestrian traffic in urban environment under epidemic conditions.
