# Zar for OCaml: formally verified sampling from discrete probability distributions.

See the related [paper](https://arxiv.org/abs/2211.06747) (appeared at
PLDI'23), [Zar](https://github.com/bagnalla/zar) Coq development, [blog
post](https://bagnalla.github.io/posts/zar.html) and [opam package](https://opam-4.ocaml.org/packages/zar/).

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
draws a uniform random float from the range `[0,1)`. While good enough for
many applications, this approach is not always correct due to floating point
rounding error. We can only expect `a1` to be executed with
probability `p + ϵ` for some small error term `ϵ`, which technically
invalidates any correctness guarantees of our overall system that
depend on the correctness of its probabilistic choices.

Zar provides an alternative that is formally proved (in Coq) to
execute `a1` with probability `p` (where `num` and `denom` are integers such
that `p = num / denom`):
```ocaml
let coin = Zar.coin num denom in (* Build coin sampler. *)
if coin#gen () then a1 else a2
```

The expression `Zar.coin num denom` builds a sampler object that flips
a coin with bias `p = num / denom`. Internally, the coin is
constructed as a stream transformer of type `bool Seq.t -> bool Seq.t`
(see [OCaml's lazy sequence
library](https://v2.ocaml.org/api/Seq.html)) that transforms an input
source of fair coin flips (i.e., uniformly distributed random bits)
into an output stream of biased coin flips. The coin transformer is
applied to a default source of random bits based on the OCaml Random
module, and then wrapped in a stateful `sampler` object that provides
a simplified interface for consuming elements from the stream. The
following code is equivalent:

```ocaml
let bit_stream = Seq.forever Random.bool |> Seq.memoize in
let coin_stream = bit_stream |> Zar.coin_transformer num denom in
let coin = new Zar.sampler coin_stream in
if coin#gen () then a1 else a2
```

You're free to supply your own stream of random bits instead, but
remember that the coin will have the correct output distribution only
when the input stream is uniformly distributed. We also recommend
ensuring that the input stream is *persistent* (see [the Seq module
documentation](https://v2.ocaml.org/api/Seq.html) for discussion of
persistent vs. ephemeral sequences).

### Uniform Sampling

Another common operation is to randomly draw from a finite collection
of values with equal (uniform) probability of each. An old trick for
drawing an integer uniformly from the range `[0, n)` is to generate a
random integer from `[0, RAND_MAX]` and take the modulus wrt. `n`:
```C
k = rand() % n // Assign to k a random integer from [0,n).
// Do something with k.
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
let k = die#gen () in (* Draw k uniformly from [0,n). *)
(* Do something with k. *)
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
let k = findist#gen () in
(* Do something with k. *)
```

For example, `Zar.findist [1; 3; 2]` builds a sampler that draws
integers from the set `{0, 1, 2}` with `Pr(0) = 1/6`, `Pr(1) = 3/6 = 1/2`,
and `Pr(2) = 2/6 = 1/3`.

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
  when the source of random bits is uniformly distributed, the proportion of `true` samples generated by the coin converges to
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

`Zar.self_init ()` initializes the PRNG for `Zar.bits` (currently just
calls [Random.self_init](https://v2.ocaml.org/api/Random.html)).

`Zar.init n` initializes the PRNG for `Zar.bits` with a given seed.

### Biased Coin

`Zar.coin_transformer num denom` builds a stream transformer that when
applied to a stream of uniformly distributed random bits generates
`bool` samples with `Pr(True) = num/denom`. Requires `0 <= num <
denom` and `0 < denom`.

`Zar.coin_stream num denom` composes `Zar.coin_transformer num denom`
with the default source of uniformly distributed random bits.

`Zar.coin num denom` builds a sampler object over the stream produced
by `Zar.coin_stream num denom`.

### N-sided Die

`Zar.die_transformer n` builds a stream transformer that when applied
to a stream of uniformly distributed random bits generates `int`
samples with `Pr(m) = 1/n` for integer m where `0 <= m < n` .

`Zar.die_stream n` composes `Zar.die_transformer n` with the default
source of uniformly distributed random bits.

`Zar.die n` builds a sampler object over the stream produced by
`Zar.die_stream n`.

### Finite Distribution

`Zar.findist_transformer weights` builds a stream transformer from
list of nonnegative integer weights `weights` (where `0 < weightsᵢ`
for some `i`) that when applied to a stream of uniformly distributed
random bits generates `int` samples with `Pr(i) = weightsᵢ /
∑ⱼweightsⱼ` for integer `0 <= i <
|weights|`.

`Zar.findist_stream weights` composes `Zar.findist_transformer
weights` with the default source of uniformly distributed random bits.

`Zar.findist weights` builds a sampler object over the stream produced
by `Zar.findist_stream weights`.

## Performance and Limitations

The samplers here are optimized for sampling performance at the
expense of build time. Thus, this library may not be ideal if your use
case involves frequent rebuilding due to changes in the samplers'
parameters (e.g., the coin's bias or the number of sides of the die).

The size of the in-memory representation of a coin with bias `p = num
/ denom` is proportional to `denom` (after bringing the fraction to
reduced form). The size of an `n`-sided die is proportional to `n`,
and the size of a finite distribution to the sum of its weights. The
formal results we provide are partial in the sense that they only
apply to samplers that execute without running out of memory. I.e., we
do not provide any guarantees against stack overflow or out-of-memory
errors when, e.g., `n` is too large.

## Acknowledgments

Thanks to [mooreryan](https://github.com/mooreryan) for comments and
code contributions.
