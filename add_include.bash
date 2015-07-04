#!/usr/bin/env bash
cd campaigns/Realms*
# This script will automatically add the #include "inc_rot_hooks_spelldmg" to
# every single script in the campaign folder.
for file in `grep -l 'EffectDamage(' *.{NSS,nss}`
do
  #sed -i 's/#include "foobar"/#include "foobar"\n#include "hello_again"/g' test.NSS
  echo "Processing file $file"
  sed -i 's/#include "x2_inc_spellhook"/#include "x2_inc_spellhook"\n#include "inc_rot_hooks_spelldmg"/g' $file
done
