# InSPEqTor

**InSPEqTor** (*INference of Shortest Paths with EQuivalent abstracT behaviOR*) is a prototype implementation to compute parameter valuations in parametric directed weighted graphs such that shortest paths are preserved.

In other words, given a parametric directed weighted graph and a valuation of the parameters, InSPEqTor synthesizes valuations for the parametric weights such that the shortest paths between any two nodes remain preserved.


## Inputs and outputs

### Inputs
* a parametric directed weighted graphs
* a reference valuation for the parameters

### Outputs
* a set of parameter valuations for which the shortest paths remain preserved between any pair of nodes


## Keywords
* parametric directed weighted graph
* parameter synthesis
* Floyd-Warshall
* shortest paths
* robustness

## Contact
[Étienne André](https://orcid.org/0000-0001-8473-9555)

(Tool developed in 2008-2010)

## Bibliography
* [Une méthode inverse pour les plus courts chemins](http://www.lsv.fr/Publis/PAPERS/PDF/andre-etr09.pdf) (Actes de la 6e École temps-réel (ETR 2009))
* [An inverse method for the synthesis of timing parameters in concurrent systems, Chapter 7.1](https://www.theses.fr/2010DENS0044)
