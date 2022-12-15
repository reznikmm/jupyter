#!/bin/bash

# SPDX-FileCopyrightText: 2022 Max Reznik <reznikmm@gmail.com>
#
# SPDX-License-Identifier: MIT

echo "--  SPDX-FileCopyrightText: 2022 Max Reznik <reznikmm@gmail.com>"
echo "--"
echo "--  SPDX-License-Identifier: MIT"
echo "----------------------------------------------------------------"
echo ""
echo "package Embedded.Texts is"
echo "   Text : Embedded_Text := ("
for J in *.ad[sb]; do
  echo '     +"'$J'",'
  sed -e 's/"/~/g' -e 's/.*/     +"\0",/' $J
  echo '     +"END '$J'",'
done
echo '     +"");'
echo "end Embedded.Texts;"