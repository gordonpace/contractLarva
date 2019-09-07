# contractLarva: Runtime Verification of Solidity Smart Contracts
Gordon J. Pace &lt;gordon.pace@um.edu.mt&gt;

Joshua Ellul &lt;joshua.ellul@um.edu.mt&gt;

Shaun Azzopardi &lt;shaun.azzopardi@um.edu.mt&gt;

## Overview

contractLarva is a runtime verification tool for Solidity contracts. For more details about the tool check out the user manual in the docs folder. 

The code is currently undocumented and not well organised. Hopefully, it will be cleaned up for the next major release.

If you would like to ask any questions, report any bugs or make any feature requests, contact us using one of the emails above.

## Building the tool

To compile contractLarva, you need a recent version of [GHC](https://www.haskell.org/ghc/). The easiest option is to install the [Haskell Platform](https://www.haskell.org/platform/) which comes with all the necessary libraries included (make sure that you choose to install the full platform, not the minimal one). To compile contractLarva, then simply run the following command in the src folder:

> ghc -o contractlarva Main.hs

## Tool usage:

> contractlarva &lt;specification.dae&gt; &lt;input.sol&gt; &lt;output.sol&gt;

## License
This project is licensed under the terms of the [Apache 2.0 license](LICENSE).

----
# FAQ and Common Problems

### When compiling the code I get the error: Could not find module ‘Text.Parsec’.
Make sure you have installed parsec.  Also, see this stackoverflow thread (https://stackoverflow.com/questions/9058914/cant-find-parsec-modules-in-ghci)
>cabal install parsec
