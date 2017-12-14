REPORT ZZE_IW1473331HISTOGRAMM.
" Temporary demo values.
DATA MITTELWERT TYPE F VALUE 1.
DATA STANDARDABWEICHUNG TYPE F VALUE 1.
DATA HIGHESTNUMBER TYPE F VALUE 3.
DATA NUMBERCOUNT TYPE F VALUE 1000.
DATA CLASSCOUNT TYPE F VALUE 30.
DATA NORMAL TYPE F VALUE 0.


SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME
                                     TITLE SELECTIO.
SELECTION-SCREEN COMMENT /1(79) CMITTELW.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS PMITTELW LENGTH 15 DEFAULT '1'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN COMMENT /1(79) CSTANDAR.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: PSTANDAR LENGTH 15 DEFAULT '1'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN COMMENT /1(79) CHIGHEST.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: PHIGHEST LENGTH 15 DEFAULT '3'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN COMMENT /1(79) CNUMBERC.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: PNUMBERC LENGTH 15 DEFAULT '2500'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN COMMENT /1(79) CCLASSCO.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: PCLASSCO LENGTH 15 DEFAULT '56'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN COMMENT /1(79) CNORMALL.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: PNORMALL LENGTH 15 DEFAULT '0'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK block1.

AT SELECTION-SCREEN OUTPUT.
  SELECTIO = 'Lognormalverteilung Selektion'.
  CMITTELW = 'Mittelwert (1)'.
  CSTANDAR = 'Standardabweichung (1)'.
  CHIGHEST = 'Größtmögliche Zufallszahl (3)'.
  CNUMBERC = 'Anzahl der Zufallszahlen (umgekippt: 2500, normal: 2000)'.
  CCLASSCO = 'Anzahl der Klassen (umgekippt: 56, normal: 253)'.
  CNORMALL = 'Kippung? (umgekippt: 0, normal: 1)'.

START-OF-SELECTION.

DATA MYREGEX TYPE C LENGTH 64 VALUE '[a-zA-Z\\-_:;,#+*^°!"§$%&/()=?`´]+'.
IF STRLEN( match( val = PMITTELW regex = MYREGEX ) ) > 0 OR STRLEN( match( val = PSTANDAR regex = MYREGEX ) ) > 0 OR STRLEN( match( val = PHIGHEST regex = MYREGEX ) ) > 0 OR STRLEN( match( val = PNUMBERC regex = MYREGEX ) ) > 0
  OR STRLEN( match( val = PCLASSCO regex = MYREGEX ) ) > 0 OR STRLEN( match( val = PNORMALL regex = MYREGEX ) ) > 0.
  WRITE: 'Ungültige Eingabe.'.
ELSE.
  MITTELWERT = CONV F( PMITTELW ).
  STANDARDABWEICHUNG = CONV F( PSTANDAR ).
  HIGHESTNUMBER = CONV F( PHIGHEST ).
  NUMBERCOUNT = CONV F( PNUMBERC ).
  CLASSCOUNT = CONV F( PCLASSCO ).
  NORMAL = CONV F( PNORMALL ).

  IF MITTELWERT > 0 AND STANDARDABWEICHUNG > 0 AND HIGHESTNUMBER > 0 AND NUMBERCOUNT > 0 AND CLASSCOUNT > 0 AND ( NORMAL EQ 1 OR NORMAL EQ 0 ).
    " Values for the normal distribution to achieve the specified values for the lognormal distribution.
    DATA NMITTELWERT TYPE I VALUE 0.
    DATA NSTANDARDABWEICHUNG TYPE I VALUE 0.
    NSTANDARDABWEICHUNG = SQRT( LOG( STANDARDABWEICHUNG * STANDARDABWEICHUNG / MITTELWERT / MITTELWERT + 1 ) ).
    NMITTELWERT = LOG( MITTELWERT ) - NSTANDARDABWEICHUNG ** 4 / 2.

    DATA: COUNTER TYPE I VALUE 1.

    " Ridiculous note to internal tables in ABAP: Index 0 errors, Index count+1 does nothing.
    " Internal table for the random normal numbers by Knuth's algorithm of the Polar method.
    DATA: LOGNORMALRANDOMNUMBERS TYPE F OCCURS 0.
    DATA: CLASSES TYPE I OCCURS 0.
    WHILE COUNTER <= CLASSCOUNT.
      INSERT 0 INTO CLASSES INDEX COUNTER.
      ADD 1 TO COUNTER.
    ENDWHILE.
    " Buffer variables for loops.
    " They are named after Knuth's algorithm.
    DATA: X1 TYPE F VALUE 0.
    DATA: X2 TYPE F VALUE 0.
    DATA: U1 TYPE F VALUE 0.
    DATA: U2 TYPE F VALUE 0.
    DATA: V1 TYPE F VALUE 0.
    DATA: V2 TYPE F VALUE 0.
    DATA: S TYPE F VALUE 0.

    DATA: CURRENT TYPE I VALUE 0.
    DATA: CURRENTF TYPE F VALUE 0.

    " Bart Simpson writes: I will not reuse counter variables in ABAP.
    DATA: COUNTER2 TYPE I VALUE 1.
    " Generate lognormal random values.
    WHILE COUNTER2 <= NUMBERCOUNT.
      CALL FUNCTION 'QF05_RANDOM'
      IMPORTING
        RAN_NUMBER = U1.
      CALL FUNCTION 'QF05_RANDOM'
      IMPORTING
        RAN_NUMBER = U2.
      V1 = 2 * U1 - 1.
      V2 = 2 * U2 - 1.
      S = V1 * V1 + V2 * V2.
      IF S < 1 AND S > 0.
        " Ofcourse,  log returns the natural logarithm.
        X1 = EXP( NMITTELWERT + NSTANDARDABWEICHUNG * V1 * SQRT( -2 * LOG( S ) / S ) ).
        INSERT X1 INTO LOGNORMALRANDOMNUMBERS INDEX COUNTER2.
        " This language is ridiculous.
        ADD 1 TO COUNTER2.
        IF COUNTER2 <= NUMBERCOUNT.
          X2 = EXP( NMITTELWERT + NSTANDARDABWEICHUNG * V2 * SQRT( -2 * LOG( S ) / S ) ).
          INSERT X2 INTO LOGNORMALRANDOMNUMBERS INDEX COUNTER2.
          ADD 1 TO COUNTER2.
        ENDIF.
      ENDIF.
    ENDWHILE.

    DATA KLASSENBREITE TYPE F VALUE 0.
    DATA CLASSINDEX TYPE I VALUE 0.
    DATA MAXCOUNT TYPE I VALUE 0.
    DATA BUFN TYPE N LENGTH 3 VALUE 0.
    KLASSENBREITE = HIGHESTNUMBER.
    KLASSENBREITE = KLASSENBREITE / CLASSCOUNT.
    " For every lognormal random number, get the class index and increment the internal table at the class index.
    LOOP AT LOGNORMALRANDOMNUMBERS INTO CURRENTF.
      CLASSINDEX = ( CURRENTF / KLASSENBREITE ) + 1.
      NEW-LINE.
      IF CLASSINDEX <= CLASSCOUNT.
        ASSIGN CLASSES[ CLASSINDEX ] TO FIELD-SYMBOL(<FS>).
        <FS> = <FS> + 1.
      ENDIF.
    ENDLOOP.
    DATA OUTPUT TYPE STRING VALUE ``.
    DATA FOOTER TYPE STRING VALUE `  0000`.
    CLASSINDEX = 1.
    IF NORMAL EQ 0.
      " Umgekipptes Histogramm.
      DATA BUF TYPE N LENGTH 3 VALUE 0.
      DATA BUFF TYPE P DECIMALS 2 VALUE 0.
      OUTPUT = `             10        20        30        40        50        60        70        80        90        100       110       120       130       140       150       160       170       180`.
      CONCATENATE OUTPUT `       190       200       210       220       230       240` INTO OUTPUT.
      WRITE: OUTPUT.
      NEW-LINE.
      OUTPUT = '   000|-----------------------------------------------------------------------------------------------------------------------------------------------------------------'.
      CONCATENATE OUTPUT `------------------------------------------------------------------------------- Absolute Häufgkeit` INTO OUTPUT.
      WRITE: OUTPUT.
      NEW-LINE.
      LOOP AT CLASSES INTO CURRENT.
        OUTPUT = '      |'.
        IF CLASSINDEX MOD 5 EQ 0.
          BUFF = CLASSINDEX * KLASSENBREITE.
          OUTPUT = |{ CONV STRING( BUFF ) }|.
          CONCATENATE ` ` OUTPUT INTO OUTPUT.
          CONCATENATE OUTPUT '|' INTO OUTPUT.
        ENDIF.
        WHILE CURRENT > 0.
          CONCATENATE OUTPUT '$' INTO OUTPUT.
          CURRENT = CURRENT - 1.
        ENDWHILE.
        WRITE: OUTPUT.
        NEW-LINE.
        ADD 1 TO CLASSINDEX.
      ENDLOOP.
      WRITE: '    Zahl'.
    ELSE.
      " Normales Histogramm.
      DATA BUF2 TYPE N LENGTH 5 VALUE 0.
      LOOP AT CLASSES INTO CURRENT.
        IF CURRENT > MAXCOUNT.
          MAXCOUNT = CURRENT.
        ENDIF.
      ENDLOOP.
      WRITE: 'Absolute Häufigkeit'.
      WHILE MAXCOUNT > 0.
        OUTPUT = '    |'.
        IF MAXCOUNT MOD 5 EQ 0.
          BUFN = MAXCOUNT.
          OUTPUT = |{ CONV STRING( BUFN ) WIDTH = 3 }|.
          CONCATENATE OUTPUT ' |' INTO OUTPUT.
        ENDIF.
        LOOP AT CLASSES INTO CURRENT.
          IF CURRENT >= MAXCOUNT.
            CONCATENATE OUTPUT '$' INTO OUTPUT.
          ELSE.
            CONCATENATE OUTPUT ` ` INTO OUTPUT.
          ENDIF.
        ENDLOOP.
        WRITE OUTPUT.
        NEW-LINE.
        MAXCOUNT = MAXCOUNT - 1.
      ENDWHILE.
      OUTPUT = '000 |------------------------------------------------------------------------------------------------------------'.
      CONCATENATE OUTPUT '---------------------------------------------------------------------------------------------------------------------------------' INTO OUTPUT.
      CONCATENATE OUTPUT '-------------------' INTO OUTPUT.
      WRITE OUTPUT.
      NEW-LINE.
      LOOP AT CLASSES INTO CURRENT.
        IF CLASSINDEX MOD 10 EQ 0.
          CONCATENATE FOOTER `     ` INTO FOOTER.
          BUFF = CLASSINDEX * KLASSENBREITE.
          OUTPUT = |{ CONV STRING( BUFF ) }|.
          CONCATENATE FOOTER OUTPUT INTO FOOTER.
        ENDIF.
          ADD 1 TO CLASSINDEX.
        ENDLOOP.
      WRITE FOOTER.
      WRITE 'Zahl'.
    ENDIF.
  ELSE.
    WRITE: 'Ungültige Eingabe.'.
  ENDIF.
ENDIF.