# vanilla
## Data sources
- https://wowwiki.fandom.com/wiki/Resistance?oldid=398985
- https://us.forums.blizzard.com/en/wow/t/bug-hit-tables/185675/8
- https://wowwiki.fandom.com/wiki/Attack_table?oldid=396406
- https://wowwiki.fandom.com/wiki/Spell_power_coefficient?oldid=1721745
- https://vanilla-wow.fandom.com/wiki/Item_level

## Things we don't take into account/eventual TODO
- fight longevity -- thus we underestimate int/spirit/mp5, for example.
- healers not supported yet due to their complexity: how do we calc spell composition -- some fights you never use prayer of healing. just calculating hps values is specious so we eschew healing entirely for now.
- expected buffs on bosses
- raid comp determining probabilities (i.e. 4 warlocks means 50% probability of using curse of doom, whereas it's 0% for 2 locks due to curse of elements/shadow requirements)
  - This may be eventually exposed by a `RaidComp` type
- currently rotations are hardcoded, i.e. Warlocks are assumed to use DS/Ruin. Eventually these will be parameterized by talents.

## Shoutouts
- http://web.engr.oregonstate.edu/~erwig/papers/PFP_JFP06.pdf

# Yesod
## Development

Start a development server with:

```
stack exec -- yesod devel
```

As your code changes, your site will be automatically recompiled and redeployed to localhost.

## Tests

```
stack test --flag vanilla:library-only --flag vanilla:dev
```

(Because `yesod devel` passes the `library-only` and `dev` flags, matching those flags means you don't need to recompile between tests and development, and it disables optimization to speed up your test compile times).

## Documentation

* Read the [Yesod Book](https://www.yesodweb.com/book) online for free
* Check [Stackage](http://stackage.org/) for documentation on the packages in your LTS Haskell version, or [search it using Hoogle](https://www.stackage.org/lts/hoogle?q=). Tip: Your LTS version is in your `stack.yaml` file.
* For local documentation, use:
        * `stack haddock --open` to generate Haddock documentation for your dependencies, and open that documentation in a browser
        * `stack hoogle <function, module or type signature>` to generate a Hoogle database and search for your query
* The [Yesod cookbook](https://github.com/yesodweb/yesod-cookbook) has sample code for various needs

## Getting Help

* Ask questions on [Stack Overflow, using the Yesod or Haskell tags](https://stackoverflow.com/questions/tagged/yesod+haskell)
* Ask the [Yesod Google Group](https://groups.google.com/forum/#!forum/yesodweb)
* There are several chatrooms you can ask for help:
        * For IRC, try Freenode#yesod and Freenode#haskell
        * [Functional Programming Slack](https://fpchat-invite.herokuapp.com/), in the #haskell, #haskell-beginners, or #yesod channels.
