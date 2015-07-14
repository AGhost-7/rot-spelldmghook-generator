// -----------------------------------------------------------------------------
// Structures
// -----------------------------------------------------------------------------

// Structs are nice for this since we could add more fields which are returned
// by the GetBonusSpellDamage. Then we can roll those values in the while loop
// using the RollBonusSpellDamage. No need to edit hundreds of scripts for a
// minor change.
struct BonusSpellDmg {
  int nBaseDmg;
  int nDiceSize0;
  int nDices0;
};

// -----------------------------------------------------------------------------
// Prototypes
// -----------------------------------------------------------------------------

// Determine the bonus outside of the loop, e.g. +1d6, then roll it in the loop.
struct BonusSpellDmg GetBonusSpellDamage(
    object oCaster,
    // The school of the spell would be really nice to be able to access from
    // this hook since the only way to do so atm is through the 2da files of the
    // game.
    int nSchool,
    // Lets say someone wants to create a new class or give fire genasi a bonus
    // to all their fire damage spells. Once this hook is in place we could
    // simply edit this single function
    int nDamageType,
    // If the spell can hit multiple targets, it should be balanced accordingly.
    int bAoE,
    // For spells which will hit the same target more than once, such as glaive,
    // missile storm, and flame arrow.
    int bMultiHit = FALSE,
    // Persistent AoE spells like incendiary cloud would need to gain less
    // damage since they spread a lot more damage over time than regular AoEs.
    int bPersistent = FALSE,
    // Say, something like +1 per spell level to damage could be an interesting
    // sort of bonus. Used this to override the default function call for epic
    // spells. Otherwise, int GetSpellLevel(int nSpellID) is used.
    int nSpellLvl = -1
    );

// This is the function which takes the structs and finally rolls the damage
// inside of the AoE's loop. I want to pass the target as that way I can check
// for race or alignment bonuses against the target.
int RollBonusSpellDamage(object oTarget, struct BonusSpellDmg dmg);

// -----------------------------------------------------------------------------
// Implementations
// -----------------------------------------------------------------------------

struct BonusSpellDmg GetBonusSpellDamage(
    object oCaster,
    int nSchool,
    int nDamageType,
    int bAoE,
    int bMultiHit = FALSE,
    int bPersistent = FALSE,
    int nSpellLvl = -1
    )
{
  struct BonusSpellDmg dmg;
  // We don't need to implement anything right away.
  return dmg;
}


int RollBonusSpellDamage(object oTarget, struct BonusSpellDmg dmg);
{
  int nDamage = 0;
  if(dmg.nDices0 > 0)
  {
    int i;
    for(i = 0; i < dmg.nDices0; i++)
    {
      nDamage += Random(dmg.nDiceSize0) + 1;
    }
  }

  return nDamage + dmg.nBaseDmg;
}
