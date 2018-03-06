# pretended-blockchain
![PretendedBlockchainLogo](/images/pretended-blockchain.png)
## Description

A simple pretended blockchain :link: in Haskell, for playing with blockchain concepts :yum:.

## Table of content
* [pretended-blockchain](#pretended-blockchain)
   * [Description](#description)
   * [Table of content](#table-of-content)
   * [In a nutshell](#in-a-nutshell)
      * [About](#about)
      * [How to use](#how-to-use)
         * [Cloning this repo](#cloning-this-repo)
         * [Testing the implementation](#testing-the-implementation)
         * [Generating the documentation](#generating-the-documentation)
         * [Building and executing the project](#building-and-executing-the-project)
   * [Related work](#related-work)
   * [Project structure](#project-structure)
   * [Data types](#data-types)
      * [The blockchain](#the-blockchain)
         * [A block](#a-block)
            * [The header](#the-header)
            * [A transaction](#a-transaction)
   * [Implementation](#implementation)
   * [What's next](#whats-next)
      * [The project](#the-project)
      * [The developments](#the-developments)
   * [References <g-emoji class="g-emoji" alias="book" fallback-src="https://assets-cdn.github.com/images/icons/emoji/unicode/1f4d6.png">📖</g-emoji>](#references-book)
      * [Blockchain resources](#blockchain-resources)
      * [Haskell guides](#haskell-guides)
      * [Tools](#tools)
   * [Contributing](#contributing)


## In a nutshell
### About
This project is a **toy blockchain** built in Haskell. It is as simple as possible, and that has scope for improvement. For instance, instead of having a single transaction in a block, set of transactions will be used in a future version, and a Merkle tree later on.

For now, a *blockchain* is a chain of *blocks*, comprised of a *header* and *transactions*.

In this version, certain limitations have been set, so the project remains simple:
- *Logical times* are used for the timestamp in the header
- A *transaction* is a contract from someone to someone else, involving a certain amount of something.
- The *blockchain* is local.
- The *blockchain* routine exists in the Main, and the user interacts with the blockchain through the terminal.
- No *proof-of-work*, or whatever. No NONCE.

### Usage
The project uses stack. The following commands are useful to understand the project implementation.

#### Cloning this repo
```bash
$ git clone https://github.com/hnrck/pretended-blockchain
$ git checkout tags/0.1.0.0
```

#### Testing the implementation
```bash
$ stack test
```
Please feel free to report problems with your tests.

#### Generating the documentation
```bash
$ stack haddock --open
```
The documentation will be opened in a browser. The documentation of the project is located in the Blockchain section.

#### Building and executing the project
```bash
$ stack build
$ stack exec PretendedBlockchain
```
Once executed, the terminal can be used to add transactions in the blockchain.

## Related work
Multiple Haskell projects on implementing a blockchain exist, especially on GitHub :octocat:. They are all very impressive and much more explained than mine.

Here is a selection:

- [TGOIson/blockchain](https://github.com/TGOlson/blockchain) -- A generic implementation of a blockchain, available on [hackage](https://hackage.haskell.org/package/blockchain).
- [MichaelBurge/haskoin](https://github.com/MichaelBurge/haskoin) -- A POC blockchain in Haskell, with a [detailed article](http://www.michaelburge.us/2017/08/17/rolling-your-own-blockchain.html) on his blog.
- [aviaviavi/legion](https://github.com/aviaviavi/legion) -- Another blockchain written in Haskell, inspired by naivechain.
- [adjoint-io/nanochain](https://github.com/adjoint-io/nanochain) -- A blockchain written in Haskell, with proof-of-work. A [dedicated website](https://www.adjoint.io/) is available.
- [kendricktan/bch](https://github.com/kendricktan/bch/) -- A minimal but very understandable blockchain written in Haskell, with an accessible [article](https://kndrck.co/posts/minimal_blockchain_haskell/).

## Project structure
```bash
.
├── app
│   └── Main.hs
├── ChangeLog.md
├── LICENSE
├── pretended-blockchain.cabal
├── README.md
├── src
│   ├── Blockchain
│   │   ├── Block
│   │   │   ├── Header.hs
│   │   │   └── Transaction.hs
│   │   └── Block.hs
│   └── Blockchain.hs
├── stack.yaml
└── tst
    └── unit
        ├── BlockSpec.hs
        ├── HeaderSpec.hs
        ├── Spec.hs
        └── TransactionSpec.hs


```
Important folders:
- ```app``` - the application sources.
- ```src``` - the sources of the pretended-blockchain library.
- ```tst``` - the tests folders.

Important files:
- ```pretended-blockchain.cabal``` and ```stack.yaml``` - the project configuration files.
- ```README.md``` - this file.
- ```ChangeLog.md``` - list of changes between revisions.
- ```LICENSE``` - the license file.

## Data types
### The blockchain
The blockchain is a simple *chain* of *blocks*. Nevertheless, in the usage, the blockchain is put in a StateT monad.
```haskell
type Blockchain = [Block]
```
![PretendedBlockchain illustration](/images/blockchain.png)

#### A block
The blockchain's block. A block can be printed or tested for equality. The header is computed at creation, and the transaction is not mandatory.

```haskell
data Block = Block
  { header      :: Header
  , transaction :: Maybe Transaction
  } deriving (Show, Eq)
```

##### The header
Containing the previous hash, timestamp, and current hash, computed at block creation.
```haskell
data Header = Header
  { previousHash :: ByteString
  , timestamp    :: !LogicalTime
  , hash         :: ByteString
  } deriving (Show, Eq)
```

##### A transaction
A transaction is a contract from someone to someone else, of a certain amount.
```haskell
data Transaction = Transaction
  { from   :: String
  , to     :: String
  , amount :: Float
  } deriving (Show, Eq)

```

## Implementation
To be done...

## Progression
This project is used as a playground; the following has to be done.

### The project
- [x] publishing this project
- [ ] writing an accessible README
  - [x] introducing the project
  - [x] describing how to use it
  - [x] referencing related work
  - [ ] describing the types and implementation
  - [ ] describing the types and implementation
- [ ] writing an article on my blog

### The developments
- [x] implementing a first version of the blockchain
  - [x] implementing the blockchain
    - [x] implementing blocks
      - [x] implementing blocks headers
      - [x] implementing blocks transactions
- [ ] using transactions instead of transaction
- [ ] using a Merkle tree instead of transactions
- [ ] implementing a RESTful interface
- [ ] adding proof-of-work
- [ ] distributing the blockchain

## References :book:
### Blockchain resources
- [Bitcoin paper](https://bitcoin.org/bitcoin.pdf) -- The famous bitcoin paper. See fig. 'Longest Proof-of-Work Chain' section 8 for a nice blockchain illustration.
- To be completed...

### Haskell guides
- [trying to understand monad transformers state in](https://www.reddit.com/r/haskell/comments/2jdmdz/trying_to_understand_monad_transformers_state_in/) -- A good explanation of the StateT usage, and why State is not sufficient.
- To be completed...

### Tools
- [Stack](https://docs.haskellstack.org/en/stable/README/) -- A cross-platform program for developing Haskell project.
- [HSpec](https://hspec.github.io/) -- A testing framework for Haskell.
- To be completed...

## Contributing
Contributions are welcome. Feel free to fork too :fork_and_knife:.

## Credits
@hnrck Henrick Deschamps [hnrck.io](https://hnrck.io)

## License
This work is under the MIT License. See LICENSE file. :copyright: Henrick Deschamps, 2018.
