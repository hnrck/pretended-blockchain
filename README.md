# pretended-blockchain
![PretendedBlockchainLogo](/images/pretended-blockchain.png)

## Description
A simple pretended blockchain :link: in Haskell, for playing with blockchain concepts :yum:.

## Table of Contents
* [1. In a nutshell](#1-in-a-nutshell)
   * [1.1. About](#11-about)
   * [1.2. Usage](#12-usage)
      * [1.2.1. Cloning this repo](#121-cloning-this-repo)
      * [1.2.2. Testing the implementation](#122-testing-the-implementation)
      * [1.2.3. Generating the documentation](#123-generating-the-documentation)
      * [1.2.4. Building and executing the project](#124-building-and-executing-the-project)
* [2. Related work](#2-related-work)
* [3. Project structure](#3-project-structure)
* [4. Data types](#4-data-types)
   * [4.1. The blockchain](#41-the-blockchain)
   * [4.2. A block](#42-a-block)
   * [4.3. The header](#43-the-header)
   * [4.4. A transaction](#44-a-transaction)
* [5. Implementation](#5-implementation)
* [6. Progression](#6-progression)
   * [6.1. The project](#61-the-project)
   * [6.2. The developments](#62-the-developments)
* [7. References <g-emoji class="g-emoji" alias="book" fallback-src="https://assets-cdn.github.com/images/icons/emoji/unicode/1f4d6.png">📖</g-emoji>](#7-references-book)
   * [7.1. Blockchain resources](#71-blockchain-resources)
   * [7.2. Haskell guides](#72-haskell-guides)
   * [7.3. Tools](#73-tools)
* [8. Contributing](#8-contributing)
* [9. Credits](#9-credits)
* [10. License](#10-license)

## 1. In a nutshell
### 1.1. About
This project is a **toy blockchain** built in Haskell. It is as simple as possible, and has scope for improvement. For instance, instead of having a single transaction in a block, set of transactions will be used in a future version, and a Merkle tree later on.

For now, a *blockchain* is a chain of *blocks*, comprised of a *header* and *transactions*.

In this version, certain limitations have been set, so the project remains simple:
- *Logical times* are used for the timestamp in the header
- A *transaction* is a contract from someone to someone else, involving a certain amount of something.
- The *blockchain* is local.
- The *blockchain* routine exists in the Main, and the user interacts with the blockchain through the terminal.
- No *proof-of-work*, or whatever. No NONCE.

### 1.2. Usage
The project uses stack. The following commands are useful to understand the project implementation.

This project has been tested with Linux and Windows.

#### 1.2.1. Cloning this repo
```bash
$ git clone https://github.com/hnrck/pretended-blockchain
$ git checkout tags/0.1.0.0
```

#### 1.2.2. Testing the implementation
```bash
$ stack test
```
Please feel free to report problems with your tests.

#### 1.2.3. Generating the documentation
```bash
$ stack haddock --open
```
The documentation will be opened in a browser. The documentation of the project is located in the Blockchain section.

#### 1.2.4. Building and executing the project
```bash
$ stack build
$ stack exec PretendedBlockchain
```
Once executed, the terminal can be used to add transactions in the blockchain.

## 2. Related work
Multiple Haskell projects on implementing a blockchain exist, especially on GitHub :octocat:. They are all very impressive and much more explained than mine.

Here is a selection:

- [TGOIson/blockchain](https://github.com/TGOlson/blockchain) -- A generic implementation of a blockchain, available on [hackage](https://hackage.haskell.org/package/blockchain).
- [MichaelBurge/haskoin](https://github.com/MichaelBurge/haskoin) -- A POC blockchain in Haskell, with a [detailed article](http://www.michaelburge.us/2017/08/17/rolling-your-own-blockchain.html) on his blog.
- [aviaviavi/legion](https://github.com/aviaviavi/legion) -- Another blockchain written in Haskell, inspired by naivechain.
- [adjoint-io/nanochain](https://github.com/adjoint-io/nanochain) -- A blockchain written in Haskell, with proof-of-work. A [dedicated website](https://www.adjoint.io/) is available.
- [kendricktan/bch](https://github.com/kendricktan/bch/) -- A minimal but very understandable blockchain written in Haskell, with an accessible [article](https://kndrck.co/posts/minimal_blockchain_haskell/).

## 3. Project structure
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

## 4. Data types
### 4.1. The blockchain
The blockchain is a simple *chain* of *blocks*. Nevertheless, in the usage, the blockchain is put in a StateT monad.
```haskell
type Blockchain = [Block]
```
![PretendedBlockchain illustration](/images/blockchain.png)

### 4.2. A block
The blockchain's block. A block can be printed or tested for equality. The header is computed at creation, and the transaction is not mandatory.

```haskell
data Block = Block
  { header      :: Header
  , transaction :: Maybe Transaction
  } deriving (Show, Eq)
```

### 4.3. The header
Containing the previous hash, timestamp, and current hash, computed at block creation.
```haskell
data Header = Header
  { previousHash :: ByteString
  , timestamp    :: !LogicalTime
  , hash         :: ByteString
  } deriving (Show, Eq)
```

### 4.4. A transaction
A transaction is a contract from someone to someone else, of a certain amount.
```haskell
data Transaction = Transaction
  { from   :: String
  , to     :: String
  , amount :: Float
  } deriving (Show, Eq)

```

## 5. Implementation
documentation to be done...

## 6. Progression
This project is used as a playground; the following has to be done.

### 6.1. The project
- [x] publishing this project
- [ ] writing an accessible README
  - [x] introducing the project
  - [x] describing how to use it
  - [x] referencing related work
  - [ ] describing the types and implementation
- [ ] writing an article on my blog

### 6.2. The developments
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

## 7. References :book:
### 7.1. Blockchain resources
- [Bitcoin paper](https://bitcoin.org/bitcoin.pdf) -- The famous bitcoin paper. See fig. 'Longest Proof-of-Work Chain' section 8 for a nice blockchain illustration.
- To be completed...

### 7.2. Haskell guides
- [trying to understand monad transformers state in](https://www.reddit.com/r/haskell/comments/2jdmdz/trying_to_understand_monad_transformers_state_in/) -- A good explanation of the StateT usage, and why State is not sufficient.
- To be completed...

### 7.3. Tools
- [Stack](https://docs.haskellstack.org/en/stable/README/) -- A cross-platform program for developing Haskell project.
- [HSpec](https://hspec.github.io/) -- A testing framework for Haskell.
- To be completed...

## 8. Contributing
Contributions are welcome. Feel free to fork too :fork_and_knife:.

## 9. Credits
- Henrick Deschamps  ([:octocat: hnrck](https://github.com/hnrck) ) ([:globe_with_meridians: hnrck.io](https://hnrck.io))

## 10. License
This work is under the [MIT License](https://github.com/hnrck/pretended-blockchain/blob/master/LICENSE). :copyright: Henrick Deschamps, 2018.
