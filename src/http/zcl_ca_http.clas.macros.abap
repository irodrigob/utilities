*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

DEFINE unescape_amp.
* &1: &. . .
* &2: Integer of the unicode symbol
* &.. within the
  concatenate '*' &1 '*' into l_pattern.                    "#EC NOTEXT
  if r_unescaped cp l_pattern.
    try.
        l_replace = cl_abap_conv_in_ce=>uccpi( &2 ).
      catch cx_root.
        l_replace = '#'.                                    "#EC NOTEXT
    endtry.
    replace all occurrences of &1 in r_unescaped with l_replace.
  endif.
END-OF-DEFINITION.                                          "#EC NOTEXT

DEFINE unescape_hex.

END-OF-DEFINITION.
