# Zar OCaml: formally verified sampling from discrete probability distributions.

See the related [paper](https://arxiv.org/abs/2211.06747) (appeared at
PLDI'23), the main [Zar Github
repository](https://github.com/bagnalla/zar) and similar [Haskell
package](https://github.com/bagnalla/haskellzar), and [blog
post](https://bagnalla.github.io/posts/zar.html).

## Why use Zar?

### Probabilistic Choice

A basic operation in randomized algorithms is *probabilistic choice*:
for some `p ∈ [0,1]`, execute action `a1` with probability `p` or
action `a2` with probability `1-p` (i.e., flip a biased coin to
determine the path of execution). A common method for performing
probabilistic choice is as follows:
```ocaml
if Random.float 1.0 < p then a1 else a2
```

where `p` is a float in the range `[0,1]` and `Random.float 1.0`
produces a random float in the range `[0,1)`. While good enough for
many applications, this approach is not always correct due to float
roundoff error. We can only expect `a1` to be executed with
probability `p + ϵ` for some small error term `ϵ`, which technically
invalidates any correctness guarantee of our overall system that
depends on the correctness of its probabilistic choices.

Zar provides an alternative that is formally proved (in Coq) to
execute `a1` with probability `p` (where `num` and `denom` are integers such
that `p = num / denom`):
```ocaml
let coin_stream = Zar.coin num denom in (* Build coin stream *)
if Zar.first coin_stream (* Look at first flip of the coin *)
  then a1 else a2
```

The expression `Zar.coin num denom` builds a stream of `bool`s
resulting from flipping the coin with bias `p = num / denom`. See
[lib/stream.mli](stream/mli) for the stream type interface. We may
integrate with an existing library in the future (e.g.,
[streaming](https://ocaml.org/p/streaming/0.8.0/doc/index.html))
similar to the [Zar Haskell
package](https://github.com/bagnalla/zar/tree/main/haskell/zar)'s
integration with the
[pipes](https://hackage.haskell.org/package/pipes) library.

Internally, the coin is constructed as a stream transformer of type
`bool stream -> bool stream` that transforms an input source of fair
coin flips into an output stream of biased coin flips. The coin
transformer is applied to a default source of fair coin flips based on
the OCaml Random module. The following code is equivalent:

```ocaml
let coin_transformer = Zar.coin_transformer num denom
let coin_stream = coin_transformer (Zar.bits ())
if Zar.first coin_stream (* Look at first flip of the coin *)
  then a1 else a2
```

The user has the option to apply the coin transformer to their own
source of fair coin flips instead (perhaps one connected to a "true"
source of randomness such as the [NIST randomness
beacon](https://csrc.nist.gov/Projects/interoperable-randomness-beacons/beacon-20)).

### Uniform Sampling

Another common operation is to randomly draw from a finite collection
of values with equal (uniform) probability of each. An old trick for
drawing an integer uniformly from the range `[0, n)` is to generate a
random integer from `[0, RAND_MAX]` and take the modulus wrt. `n`:
```C
k = rand() % n // Assign to k a random integer from [0,n)
// do something with k
```
but this method suffers from modulo bias when `n` is not a power of 2,
causing some values to occur with higher probability than others (see,
e.g., [this
article](https://research.kudelskisecurity.com/2020/07/28/the-definitive-guide-to-modulo-bias-and-how-to-avoid-it/)
for more information on modulo bias). Zar provides a uniform sampler
that is guaranteed for any integer `0 < n` to generate samples from
the range `[0,n)` with probability `1/n` each:
```ocaml
let die = Zar.die n in
let k = Zar.first die in (* k drawn uniformly from [0,n) *)
(* do something with k *)
```

Although the OCaml function `Random.int` is ostensibly free from
modulo bias, our implementation guarantees it by a *formal proof of
correctness* in Coq.

### Finite Distributions

The coin and die samplers are special cases of a more general
construction for finite probability distributions that we provide
here. Given a list of nonnegative integer weights `weights` such that
`0 < weightsᵢ` for some `i` (at least one of the weights is nonzero),
we can draw an integer `k` from the range `[0, |weights|)` with
probability `weightsₖ / ∑ⱼweightsⱼ` (the corresponding weight of `k`
normalized by the sum of all weights):
```ocaml
let findist = Zar.findist weights in
let k = Findist.sample () in
(* do something with k *)
...
```

For example, `Zar.findist [1; 3; 2]` builds a sampler that draws
integers from the set `{0, 1, 2}` with `Pr(0) = 1/6`, `Pr(1) = 1/2`,
and `Pr(2) = 1/3`.

## Trusted Computing Base

The samplers provided by Zar have been implemented and verified in Coq
and extracted to OCaml for execution. Validity of the correctness
proofs is thus dependent on the correctness of Coq's extraction
mechanism, the OCaml compiler and runtime, and a small amount of OCaml
shim code (viewable
[here](https://github.com/bagnalla/ocamlzar/blob/main/lib/core.ml)
and thoroughly tested with QCheck
[here](https://github.com/bagnalla/ocamlzar/blob/main/test/zar.ml)),

## Proofs of Correctness

The samplers are implemented as choice-fix (CF) trees (an intermediate
representation used in the [Zar](https://github.com/bagnalla/zar)
compiler) and compiled to [interaction
trees](https://github.com/DeepSpec/InteractionTrees) that implement
them via reduction to sequences of fair coin flips. See Section 3 of
the [paper](https://arxiv.org/abs/2211.06747) for details and the file
[ocamlzar.v](https://github.com/bagnalla/zar/blob/main/ocamlzar.v) for
their implementations and proofs of correctness.

Correctness is two-fold. For biased coin with bias `p`, we prove:

*
  [coin_itree_correct](https://github.com/bagnalla/zar/blob/release-pldi23/ocamlzar.v#L34):
  the probability of producing `true` according to the formal
  probabilistic semantics of the constructed interaction tree is equal
  to `p`, and

*
  [coin_true_converges](https://github.com/bagnalla/zar/blob/release-pldi23/ocamlzar.v#67):
  when the source of random bits is uniformly distributed, for any
  sequence of coin flips the proportion of `true` samples converges to
  `p` as the number of samples goes to +∞.

The equidistribution result is dependent on uniform distribution of
the Boolean values generated by OCaml's
[`Random.bool`](https://v2.ocaml.org/api/Random.html) function. See
[the paper](https://arxiv.org/abs/2211.06747) for a more detailed
explanation.

Similarly, Theorem
[die_itree_correct](https://github.com/bagnalla/zar/blob/release-pldi23/ocamlzar.v#L83)
proves semantic correctness of the n-sided die, and Corollary
[die_eq_n_converges](https://github.com/bagnalla/zar/blob/release-pldi23/ocamlzar.v#L115)
that for any `m < n` the proportion of samples equal to `m` converges
to `1 / n`.

Theorem
[findist_itree_correct](https://github.com/bagnalla/zar/blob/release-pldi23/ocamlzar.v#L128)
proves semantic correctness of findist samplers, and Corollary
[findist_eq_n_converges](https://github.com/bagnalla/zar/blob/release-pldi23/ocamlzar.v#L166)
that for any weight vector `weights` and integer `0 <= i < |weights|`,
the proportion of samples equal to `i` converges to `weightsᵢ /
∑ⱼweightsⱼ`.

## Usage

See [zar.mli](lib/zar.mli) for the top-level interface.

`Zar.bits ()` produces a stream of uniformly distributed random bits.

`Zar.seed ()` initializes the PRNG for `Zar.bits` (currently just
calls [Random.self_init](https://v2.ocaml.org/api/Random.html)).

### Biased Coin

`Zar.coin_transformer num denom` builds a stream transformer that when
applied to a stream of uniformly distributed random bits generates
`bool` samples with `Pr(True) = num/denom`. Requires `0 <= num <
denom` and `0 < denom`.

`Zar.coin num denom` composes `Zar.coin_transformer num denom` with
the default source of uniformly distributed random bits.

### N-sided Die

`Zar.die_transformer n` builds a stream transformer that when applied
to a stream of uniformly distributed random bits generates `int`
samples with `Pr(m) = 1/n` for integer m where `0 <= m < n` .

`Zar.die n` composes `Zar.die_transformer n` with the default source
of uniformly distributed random bits.

### Finite Distribution

`Zar.findist_transformer weights` builds a stream transformer from
list of nonnegative integer weights `weights` (where `0 < weightsᵢ`
for some `i`) that when applied to a stream of uniformly distributed
random bits generates `int` samples with `Pr(i) = weightsᵢ /
∑ⱼweightsⱼ` for integer `0 <= i <
|weights|`.

`Zar.findist weights` composes `Zar.findist_transformer weights` with
the default source of uniformly distributed random bits.

## Performance and Limitations

The samplers here are optimized for sampling performance at the
expense of build time. Thus, this library not be ideal if your use
case involves frequent rebuilding due to changes in the samplers'
parameters (e.g., the coin's bias or the number of sides of the
die). For example, in our experiments it takes ~0.22s to build a
100000-sided die and 1.83s to build a 500000-sided die, but only
~1.85s and ~2.19s respectively to generate one million samples from
each.

The size of the in-memory representation of a coin with bias `p = num
/ denom` is proportional to `denom` (after bringing the fraction to
reduced form). The size of an `n`-sided die is proportional to `n`,
and the size of a finite distribution to the sum of its weights. The
formal results we provide are partial in the sense that they only
apply to samplers that execute without running out of memory. I.e., we
do not provide any guarantees against stack overflow or out-of-memory
errors when, e.g., `n` is too large.
