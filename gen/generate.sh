#! /usr/bin/env bash

echo -e "\n"

cat <<EOF
  ' ' '''''''''=#= ' ''''-==*#######***===++* -'==+==**###**--+==============-'
'''-''''''''''-*#'''''''--+*#######*******==*-''-*=*########+--===============+
'''''''''' ''''#* ' '''''-'*####*====+=+======---**==********=-+===============
'-'''''''-''''=#=  '' '''-*#*= -=-'---'-''''-=--+ -' ''''''  -++===============
++=+++=++++--'=#= ''''''+*=  '-' '''  ' '''-*='+=-''  '    '    '========**=*==
============+-*# -+='''=+   -  '''-=''' ''''##=-*  ''''      '    =*===*******=
=============+##=-'-'-+    '   ''=##=''' ''-*#=-='  '=*+    '      =*===****=*=
==============#==#**--     ' ''-+=*=-''''''+*='-+=--'-=#'       ' ' =*==*******
==============-*###-+       '-=**##**======*=-'-++==++==*=          '=***=*****
==============*###++       '--++==+==+===****-=-----==*=+    '       '*********
==============###==         '-+==     ''=###*=''==-=+*##==+'-+      ' =********
=============*##*+'    '    ''--+-   '''=****+   -'  +=*=+-'''         =*******
============*###--      '    ''--+'  '+=****'         '-++-'-   '  ''   *******
*===========###=+ '         '  '-++  =***#=             -  -'     '  '  =******
*===*======####-               ''-++*****-               '---       '''' =*****
**=****===*###='     '   -+++--''--+-***'-               ' ++-      ''''  *****
***=***==*###*' ' '     ''''-++++++--===   ''''-''''-''''''+**+     '-'--'=****
**=*****=##*=         '' '+=====+--'''  '     '  '  '''-''         '-+==-'=****
**=*******='''   ' '  ' ''-+++-''''''    ''        '-+--         '  ''-+-=*****
**=*****=*=+-''''''   '----+++--''''''    ''    ---' '-='         ''-===*******
******=-=*==+''-'' '''-++++====+--'''  '  '    -  '+-+*#=   ' -'+====**********
**=***=====-''''--'''-+==========+---''''''''   ' '-+=*##= ' ''-+=====***======
#* -U *****==-''-'''''-===*****======+++++----''''+=***===  ''' ' '++==*==++-++
#*******==++---'-'''''' '-==*###**===========+----=*##*='  ' ' '  ''''+========
EOF

echo -e "\n"


php gen/Recursive.php > System/Posix/Recursive.hs
php gen/Recursive.ByteString.php > System/Posix/Recursive/ByteString.hs

echo "PHP: I'm your father"
echo "Haskell: NOOOOOOOOOOOOOOO!!!!"
