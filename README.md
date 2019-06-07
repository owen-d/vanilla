# vanilla
## Data sources
- https://wowwiki.fandom.com/wiki/Resistance?oldid=398985
- https://us.forums.blizzard.com/en/wow/t/bug-hit-tables/185675/8
- https://wowwiki.fandom.com/wiki/Attack_table?oldid=396406
- https://wowwiki.fandom.com/wiki/Spell_power_coefficient?oldid=1721745

## Things we don't take into account/eventual TODO
- fight longevity -- thus we underestimate int/spirit/mp5, for example.
- healers not supported yet due to their complexity: how do we calc spell composition -- some fights you never use prayer of healing. just calculating hps values is specious so we eschew healing entirely for now.
- expected buffs on bosses
- raid comp determining probabilities (i.e. 4 warlocks means 50% probability of using curse of doom, whereas it's 0% for 2 locks due to curse of elements/shadow requirements)
  - This may be eventually exposed by a `RaidComp` type
- currently rotations are hardcoded, i.e. Warlocks are assumed to use DS/Ruin. Eventually these will be parameterized by talents.

## Shoutouts
- http://web.engr.oregonstate.edu/~erwig/papers/PFP_JFP06.pdf
