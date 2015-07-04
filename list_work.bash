#!/usr/bin/env bash
# I want to list all files that I have to work on.

cd campaigns/Realms*

atomOpen=false
for args in "$@"; do if [[ $args == "-o" ]]; then atomOpen=true; fi; done

cnt=0
for file in `grep -l '#include "x2_inc_spellhook"' *.{NSS,nss}`
do
  if [[ `grep 'EffectDamage(' $file` != "" ]] #\
    #&& [[ `grep 'GetBonusSpellDamage(' $file` == "" ]]
  then
    cnt=$(($cnt + 1))
    if [ "$atomOpen" = true ]; then
      if [ $cnt -lt 10 ]; then
        echo "Opening: $file"
        atom $file
      else
        break
      fi
    else
      echo $file
    fi
  fi
done
if [ "$atomOpen" = false ]; then
  echo "
  ~~$cnt files to go~~"
fi
