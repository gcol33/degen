# Vision

## The core problem

Statistical models are treated as stories with numbers.
Different latent structures are assumed to represent different ecological mechanisms.

But in many cases, those structures collapse to the same object once they touch data.

The field has no systematic way to detect this.

As a result, modelling practice quietly drifts from inference into narrative confirmation.

## The vision

`degen` exists to separate what the data identify from what the modeller assumes.

Not by rejecting models.
Not by selecting better ones.
But by making the boundary between identified and assumed structure explicit.

The goal is not correctness.
It is epistemic clarity.

## What changes if this exists

Right now, the modelling workflow looks like:

1. Specify a model that encodes a causal story
2. Fit it
3. Interpret parameters as if the story were testable

`degen` inserts a missing step:

> 2.5 Ask whether the story is even distinguishable from alternatives

If not, the story must be treated as an assumption, not a result.

## A shift in what "diagnostics" mean

Most diagnostics ask:

- Did it converge?
- Does it predict?
- Are residuals well behaved?

`degen` asks:

- What transformations of this model leave the likelihood unchanged?
- Which parameters live in flat directions of the likelihood?
- Which conceptual distinctions collapse once data are observed?

This is a different axis of model criticism.

## What the package treats as first-class objects

- Equivalence classes of models, not individual fits
- Identified functions of parameters, not raw coefficients
- Non-identifiable contrasts, not just wide intervals

In other words, it works on meaning, not estimation.

## What it does to interpretation

It forces a re-labelling:

From:
> "The model shows that process A dominates process B"

To:
> "Under this set of assumptions, the combined effect of A and B is identified"

That sounds modest.
It is not.

It changes what counts as evidence.

## What it is deliberately not trying to do

- It does not tell users which model to choose
- It does not fix non-identifiability
- It does not recommend priors or constraints

Those are modelling decisions.

`degen` only tells you where the data stop speaking.

## Why this is uncomfortable

Because many published conclusions sit exactly in that silent zone.

Making equivalence visible does not break models.
It breaks overconfident interpretations.

That is why this kind of tool does not emerge naturally from incentive-driven modelling culture.

## The long-term vision

If tools like this existed, then:

- Reviewers could ask "is this distinction identifiable?"
- Authors could explicitly state "this separation is assumed"
- Debates would shift from parameter estimates to model meaning

Over time, this would not reduce modelling.
It would make mechanistic claims rarer, sharper, and harder to fake.

## One sentence

`degen` is about forcing models to say what they actually know, and nothing more.
