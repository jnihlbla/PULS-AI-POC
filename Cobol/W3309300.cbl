000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W3309300.                                                 
000400 AUTHOR.        INGVAR SKJELBRED                                          
000500     DATE-WRITTEN.  DEC 1997.                                             
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        SPECILA PROGRAM FÖR RENSNING AV PRIMÄR-REGISTER                  
002000*    SUBPROGRAM:                                                          
002300*                                                                         
002400     EJECT                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     SKIP2                                                                
003100*    --- INFILER:                                                         
003500     SELECT INFIL                        ASSIGN TO W33011D1.              
003600     SKIP2                                                                
003700*    --- UTFILER:                                                         
003900     SELECT W33013                       ASSIGN TO W33011D2.              
004000     SELECT LISTA                        ASSIGN TO W33011D3.              
004400     EJECT                                                                
004500 DATA DIVISION.                                                           
004600     SKIP2                                                                
004700 FILE SECTION.                                                            
004800     SKIP3                                                                
006000 FD  INFIL                                                                
006100     LABEL RECORD   STANDARD                                              
006200     RECORDING      V                                                     
006300     BLOCK CONTAINS 0.                                                    
006400     SKIP2                                                                
006500*    -COPY W330310   -L.                                                  
006700     SKIP2                                                                
006800*    -COPY W330300   -L.                                                  
007000     SKIP2                                                                
007100*    -COPY W330320   -L.                                                  
007300     SKIP2                                                                
007400*    -COPY W330330   -L.                                                  
007600     SKIP2                                                                
007700*    -COPY W330340   -L.                                                  
007900     SKIP2                                                                
008000*    -COPY W330350   -L.                                                  
008200     EJECT                                                                
008300 FD  W33013                                                               
008400     LABEL RECORD   STANDARD                                              
008500     RECORDING      V                                                     
008600     BLOCK CONTAINS 0.                                                    
008700     SKIP2                                                                
008800*01  POST -COPY W330310  -PRE UT310-  -L.                                 
009000     SKIP2                                                                
009100*01  POST -COPY W330300  -PRE UT300-  -L.                                 
009300     SKIP2                                                                
009400*01  POST -COPY W330320  -PRE UT320-  -L.                                 
009600     SKIP2                                                                
009700*01  POST -COPY W330330  -PRE UT330-  -L.                                 
009900     SKIP2                                                                
010000*01  POST -COPY W330340  -PRE UT340-  -L.                                 
010200     SKIP2                                                                
010300*01  POST -COPY W330350  -PRE UT350-  -L.                                 
010500     EJECT                                                                
010600 FD  LISTA                                                                
010700     RECORDING       F                                                    
010800     BLOCK CONTAINS  0.                                                   
010900     SKIP2                                                                
011000 01  LISTAS                      PIC X(121).                              
011100     SKIP3                                                                
011600     EJECT                                                                
011700 WORKING-STORAGE SECTION.                                                 
011800     SKIP2                                                                
011801*    -COPY WY2000W3                                                       
011810     SKIP3                                                                
011900 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W3309300'.            
018300*                                                                         
018400                                                                          
018500 01  FILLER                  PIC X(16)   VALUE 'WS-SEKTION'.              
018600 01  WS-SEKTION                  PIC X(30)   VALUE SPACE.                 
018610 01  FILLER                  PIC X(16)   VALUE 'WS-IMS-SEKTION'.          
018620 01  WS-IMS-SEKTION              PIC X(30)   VALUE SPACE.                 
018630                                                                          
018700 01  DYNAMISKA-SUBPROGRAM.                                                
018800*                                                                         
018900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
019000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
019300     SKIP2                                                                
019400*    --- PARAMETRAR TILL ABEND                                            
019500                                                                          
019600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
019700     SKIP2                                                                
019710 77  W-INFIL-KVPOST-IN           PIC S9(7)   VALUE ZERO COMP-3.           
019720 77  IX                          PIC S9(7)   COMP-3 VALUE ZERO.           
019730 77  IX2                     PIC S9(7)   COMP-3 VALUE ZERO.               
019731 77  ANTAL-300               PIC S9(7)   COMP-3 VALUE ZERO.               
019732 77  SPAR-DAFSGVV-1          PIC  9(6)   VALUE ZERO.                      
019733 77  SPAR-IDDISTR          PIC S9(5)     COMP-3 VALUE ZERO.               
019734 77  SUM-310               PIC S9(15)V99 COMP-3 VALUE ZERO.               
019735 77  SUM-310-RATT          PIC S9(15)V99 COMP-3 VALUE ZERO.               
019736 77  SUM-310-FEL           PIC S9(15)V99 COMP-3 VALUE ZERO.               
019737 77  SUM-320-RATT          PIC S9(15)V99 COMP-3 VALUE ZERO.               
019738 77  SUM-320-FEL           PIC S9(15)V99 COMP-3 VALUE ZERO.               
019739 77  SUM-320               PIC S9(15)V99 COMP-3 VALUE ZERO.               
019740 77  SUM-330               PIC S9(15)V99 COMP-3 VALUE ZERO.               
019741 77  SUM-330-RATT          PIC S9(15)V99 COMP-3 VALUE ZERO.               
019742 77  SUM-330-FEL           PIC S9(15)V99 COMP-3 VALUE ZERO.               
019743 77  SUM-340               PIC S9(15)V99 COMP-3 VALUE ZERO.               
019744 77  SUM-340-RATT          PIC S9(15)V99 COMP-3 VALUE ZERO.               
019745 77  SUM-340-FEL           PIC S9(15)V99 COMP-3 VALUE ZERO.               
019746 77  SUM-350               PIC S9(15)V99 COMP-3 VALUE ZERO.               
019747 77  SUM-350-RATT          PIC S9(15)V99 COMP-3 VALUE ZERO.               
019748 77  SUM-350-FEL           PIC S9(15)V99 COMP-3 VALUE ZERO.               
019749 77  ANTAL-300-FEL           PIC S9(7)   COMP-3 VALUE ZERO.               
019750 77  ANTAL-300-RATT          PIC S9(7)   COMP-3 VALUE ZERO.               
019752 01  WS-IDARTNR            PIC S9(9).                                     
019753 01  WS-DAFSGVV            PIC S9(6).                                     
019754 01  WS-IDDISTR            PIC S9(5).                                     
019755 01  WS-SULEVANT           PIC S9(9).                                     
019756 01  WS-SUARTFSG           PIC S9(9)V9(2).                                
019757     SKIP2                                                                
019758 01  FELTEXT.                                                             
019760     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
019770     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
019771                                                                          
019772 77  DISTRIKT-SW                 PIC X       VALUE 'N'.                   
019773     88  RATT-DISTRIKT                       VALUE 'J'.                   
019780                                                                          
019783                                                                          
019784                                                                          
019785 77  POST-SKRIVEN-SW             PIC X       VALUE 'N'.                   
019786     88  POST-SKRIVEN                        VALUE 'J'.                   
019787                                                                          
019790 77  INFIL-EOF-SW                PIC X       VALUE 'N'.                   
019791     88  END-OF-INFIL                        VALUE 'J'.                   
019792     EJECT                                                                
019793                                                                          
019800*- - - - - - - - - - - - - - - - PARAMETRAR TILL POSTSUM                  
019900*                                                                         
020000*01  -COPY W0005 -PRE  POSTSUM-                                           
020200                                                                          
020300 01  INFIL-AREA-START            PIC X(24)   VALUE                        
020400                                             'INFIL-AREA-START'.          
020500     SKIP2                                                                
020600 01  INFIL-AREA.                                                          
020700     03  INFIL-AREA-0.                                                    
020800         05  INFIL-IDPTYP        PIC X(3).                                
020900         05  FILLER              PIC X(400).                              
021000*   03  FILLER -COPY W330310  -PRE I310-   -RED  INFIL-AREA-0             
021100*   03  FILLER -COPY W330300  -PRE I300-   -RED  INFIL-AREA-0             
021200*   03  FILLER -COPY W330320  -PRE I320-   -RED  INFIL-AREA-0             
021300*   03  FILLER -COPY W330330  -PRE I330-   -RED  INFIL-AREA-0             
021400*   03  FILLER -COPY W330340  -PRE I340-   -RED  INFIL-AREA-0             
021500*   03  FILLER -COPY W330350  -PRE I350-   -RED  INFIL-AREA-0             
021595     EJECT                                                                
021597 01  UTFIL-AREA-START            PIC X(24)   VALUE                        
021598                                             'UTFIL-AREA-START'.          
021599     SKIP2                                                                
021600 01  UTFIL-AREA.                                                          
021601     03  UTFIL-AREA-0.                                                    
021602         05  UTFIL-IDPTYP        PIC X(3).                                
021603         05  FILLER              PIC X(400).                              
021604*   03  FILLER -COPY W330310  -PRE U310-  -RED  UTFIL-AREA-0              
021605*   03  FILLER -COPY W330300  -PRE U300-  -RED  UTFIL-AREA-0              
021606*   03  FILLER -COPY W330320  -PRE U320-  -RED  UTFIL-AREA-0              
021607*   03  FILLER -COPY W330330  -PRE U330-  -RED  UTFIL-AREA-0              
021608*   03  FILLER -COPY W330340  -PRE U340-  -RED  UTFIL-AREA-0              
021609*   03  FILLER -COPY W330350  -PRE U350-  -RED  UTFIL-AREA-0              
028800     EJECT                                                                
028801 01  SPAR-AREA-START           PIC X(24)   VALUE                          
028802                                             'SPAR-AREA-START'.           
028803 01  SPAR-AREA.                                                           
028804     03  SPAR-AREA-0.                                                     
028805         05  SPAR-IDPTYP         PIC X(3).                                
028806         05  FILLER              PIC X(400).                              
028807*   03  FILLER -COPY W330300  -PRE SPAR-   -RED  SPAR-AREA-0              
028808 01  SPAR-AREA-START           PIC X(24)   VALUE                          
028809                                             'SPAR-AREA2-START'.          
028810 01  SPAR-AREA2.                                                          
028811     03  SPAR-AREA2-0.                                                    
028812         05  SPAR-IDPTYP         PIC X(3).                                
028813         05  FILLER              PIC X(400).                              
028814*   03  FILLER -COPY W330310  -PRE SPAR2-   -RED  SPAR-AREA2-0            
028820                                                                          
028830 01  UT-RAD                      PIC X(121)  VALUE SPACE.                 
029500                                                                          
029510     EJECT                                                                
029520 01  W001R1-RUBRIK-S.                                                     
029530*                                                                         
029540     03  W001R1-STYR           PIC X(1)   VALUE '1'.                      
029550     03  FILLER                PIC X(2)   VALUE SPACE.                    
029560     03  FILLER                PIC X(15)                                  
029561                  VALUE 'KREDITERING KOR'.                                
029570     03  FILLER                PIC X(34)                                  
029580                  VALUE 'EA  FÖR DISTRIKT 6117 6122 6121 OC'.             
029590     03  FILLER                PIC X(39)                                  
029591                  VALUE 'H 6124 VECKA 23                    '.            
029592     03  FILLER                PIC X(7)   VALUE '     '.                  
029593     03  FILLER                PIC X(10)  VALUE '          '.             
029594     03  FILLER                PIC X(5)   VALUE SPACE.                    
029595     03  FILLER                PIC X(7)  VALUE 'SIDA  1'.                 
029600     EJECT                                                                
029617 01  W001R2-DELRUBRIK-1.                                                  
029618*                                                                         
029619     03  W001R2-STYR           PIC X(1)   VALUE '0'.                      
029620     03  FILLER                PIC X(4)   VALUE  SPACE.                   
029621     03  FILLER                PIC X(8)  VALUE 'ARTIKEL '.                
029622     03  FILLER                PIC X(2)  VALUE SPACE.                     
029623     03  FILLER                PIC X(5)  VALUE 'VECKA'.                   
029624     03  FILLER                PIC X(3)  VALUE SPACE.                     
029625     03  FILLER                PIC X(8)  VALUE 'DISTRIKT'.                
029626     03  FILLER                PIC X(2)  VALUE SPACE.                     
029627     03  FILLER                PIC X(5)  VALUE 'ANTAL'.                   
029628     03  FILLER                PIC X(7)  VALUE SPACE.                     
029630     03  FILLER                PIC X(5)  VALUE 'SUMMA'.                   
029633     03  FILLER                PIC X(22)  VALUE SPACE.                    
029635     03  FILLER                PIC X(11) VALUE SPACE.                     
029637                                                                          
029638 01  W001R2-DELRUBRIK-2.                                                  
029639*                                                                         
029640     03  W001R2-STYR           PIC X(1)   VALUE '0'.                      
029641     03  FILLER                PIC X(2)   VALUE  SPACE.                   
029642     03  W001R2-IDARTNR        PIC Z(9).                                  
029643     03  FILLER                PIC X(2)  VALUE SPACE.                     
029644     03  W001R2-DAFSGVV        PIC Z(6).                                  
029645     03  FILLER                PIC X(2)  VALUE SPACE.                     
029646     03  W001R2-IDDISTR        PIC Z(5).                                  
029647     03  FILLER                PIC X(2)  VALUE SPACE.                     
029648     03  W001R2-SULEVANT       PIC Z(8)9.                                 
029649     03  FILLER                PIC X(2)  VALUE SPACE.                     
029650     03  W001R2-SUARTFSG       PIC Z(7).99.                               
029651     03  FILLER                PIC X(27)  VALUE SPACE.                    
029652     03  FILLER                PIC X(14) VALUE SPACE.                     
029653                                                                          
029654                                                                          
029655     EJECT                                                                
029660                                                                          
029700 PROCEDURE DIVISION.                                                      
029800     SKIP2                                                                
029900 STYR SECTION.                                                            
030000     PERFORM A-INIT                                                       
030100     PERFORM S01-LAES-INFIL                                               
030400     PERFORM UNTIL END-OF-INFIL                                           
030700         PERFORM B-BEHANDLA                                               
030800         PERFORM S01-LAES-INFIL                                           
037400     END-PERFORM                                                          
037500     PERFORM Z-FINIT                                                      
037600     MOVE ZERO TO RETURN-CODE                                             
037700     GOBACK                                                               
037800     .                                                                    
037900     EJECT                                                                
038000 A-INIT SECTION.                                                          
038010     MOVE 'A-INIT '          TO WS-SEKTION                                
038020*    DISPLAY 'A-INIT '                                                    
038100     SKIP2                                                                
038200     OPEN INPUT INFIL                                                     
038400     SKIP2                                                                
038500     OPEN OUTPUT W33013                                                   
038600                 LISTA                                                    
038610     MOVE W001R1-RUBRIK-S     TO UT-RAD                                   
038620     PERFORM S02-SKRIV-UT-RAD                                             
038700                                                                          
038800     MOVE W001R2-DELRUBRIK-1  TO UT-RAD                                   
038900     PERFORM S02-SKRIV-UT-RAD                                             
041500     .                                                                    
041600     EJECT                                                                
045900 B-BEHANDLA  SECTION.                                                     
045901     MOVE 'B-BEHANDLA '      TO WS-SEKTION                                
045902*    DISPLAY 'B-BEHANDLA '                                                
045910     IF INFIL-IDPTYP = '300'                                              
045920**********************************                                        
045921*****   SPARA UNDAN POSTEN *******                                        
045922**********************************                                        
045923        MOVE I300-W330300 TO SPAR-W330300                                 
045924        MOVE I300-IDARTNR    TO WS-IDARTNR                                
045926        MOVE I300-DAFSGVV    TO SPAR-DAFSGVV-1                            
045927        MOVE I300-DAFSGVV    TO WS-DAFSGVV                                
045928        MOVE 'N'          TO POST-SKRIVEN-SW                              
045929        ADD +1 TO ANTAL-300                                               
045931     END-IF                                                               
045940     IF INFIL-IDPTYP = '310'                                              
045941     AND SPAR-DAFSGVV-1 = 199923                                          
045943        MOVE I310-W330310 TO SPAR2-W330310                                
045944        MOVE I310-IDDISTR TO SPAR-IDDISTR                                 
045945        MOVE I310-IDDISTR TO WS-IDDISTR                                   
045946        IF I310-IDDISTR = 6121                                            
045947        OR I310-IDDISTR = 6122                                            
045948        OR I310-IDDISTR = 6124                                            
045949        OR I310-IDDISTR = 6117                                            
045956           IF POST-SKRIVEN                                                
045957              CONTINUE                                                    
045958           ELSE                                                           
045959              MOVE 'J'          TO POST-SKRIVEN-SW                        
045961              MOVE SPAR-W330300 TO U300-W330300                           
045968              WRITE UT300-POST FROM UTFIL-AREA                            
045970           END-IF                                                         
045971           MOVE SPAR2-W330310  TO I310-W330310                            
045972           PERFORM S03-SKRIV-UTFIL                                        
046023        END-IF                                                            
046039     END-IF                                                               
046040     IF INFIL-IDPTYP = '350'                                              
046041        IF SPAR-IDDISTR = 6121                                            
046042        OR SPAR-IDDISTR = 6122                                            
046043        OR SPAR-IDDISTR = 6124                                            
046044        OR SPAR-IDDISTR = 6117                                            
046049           IF SPAR-DAFSGVV-1 = 199923                                     
046050              PERFORM S03-SKRIV-UTFIL                                     
046052              MOVE I350-SULEVANT-KRE   TO WS-SULEVANT                     
046054              MOVE I350-SUARTFSG-KRE   TO WS-SUARTFSG                     
046056              MOVE WS-IDARTNR          TO W001R2-IDARTNR                  
046057              MOVE WS-DAFSGVV          TO W001R2-DAFSGVV                  
046058              MOVE WS-IDDISTR          TO W001R2-IDDISTR                  
046059              MOVE WS-SULEVANT         TO W001R2-SULEVANT                 
046060              MOVE WS-SUARTFSG         TO W001R2-SUARTFSG                 
046063              MOVE W001R2-DELRUBRIK-2  TO UT-RAD                          
046064              PERFORM S02-SKRIV-UT-RAD                                    
046065           END-IF                                                         
046066        END-IF                                                            
046067     END-IF                                                               
046070                                                                          
046100     SKIP2                                                                
050100     .                                                                    
050200     EJECT                                                                
070400 Z-FINIT SECTION.                                                         
070500     SKIP2                                                                
070700     CLOSE INFIL                                                          
070800           W33013                                                         
070900           LISTA                                                          
071000     DISPLAY 'ANTAL 300 POSTER ' ANTAL-300                                
071100     DISPLAY 'ANTAL 300 FEL POSTER ' ANTAL-300-FEL                        
071200     DISPLAY 'ANTAL 300 RATT POSTER ' ANTAL-300-RATT                      
071300     DISPLAY 'SUM-310 ' SUM-310                                           
071310     DISPLAY 'SUM-320 ' SUM-320                                           
071320     DISPLAY 'SUM-330 ' SUM-330                                           
071330     DISPLAY 'SUM-340 ' SUM-340                                           
071340     DISPLAY 'SUM-350 ' SUM-350                                           
071350     DISPLAY 'SUM-310-RATT ' SUM-310-RATT                                 
071360     DISPLAY 'SUM-320-RATT ' SUM-320-RATT                                 
071370     DISPLAY 'SUM-330-RATT ' SUM-330-RATT                                 
071380     DISPLAY 'SUM-340-RATT ' SUM-340-RATT                                 
071390     DISPLAY 'SUM-350-RATT ' SUM-350-RATT                                 
071400     DISPLAY 'SUM-310-FEL ' SUM-310-FEL                                   
071410     DISPLAY 'SUM-320-FEL ' SUM-320-FEL                                   
071420     DISPLAY 'SUM-330-FEL ' SUM-330-FEL                                   
071430     DISPLAY 'SUM-340-FEL ' SUM-340-FEL                                   
071440     DISPLAY 'SUM-350-FEL ' SUM-350-FEL                                   
073100                                                                          
076900     MOVE 'S' TO POSTSUM-OPKOD                                            
077000     CALL POSTSUM USING POSTSUM-PARM                                      
077100     .                                                                    
077200     EJECT                                                                
077210 S01-LAES-INFIL   SECTION.                                                
077211     MOVE 'S01-LAES-INFIL '  TO WS-SEKTION                                
077212*    DISPLAY 'S01-LAES-INFIL '                                            
077220     SKIP2                                                                
077230     READ INFIL INTO INFIL-AREA                                           
077240     AT END                                                               
077250        SET END-OF-INFIL TO TRUE                                          
077260                                                                          
077270     NOT AT END                                                           
077280        MOVE 'INFIL '     TO POSTSUM-FDNAMN                               
077290        MOVE 'W33011D1'   TO POSTSUM-DDNAMN2                              
077291        MOVE INFIL-IDPTYP TO POSTSUM-TRANSTYP                             
077292        CALL POSTSUM USING POSTSUM-PARM                                   
077293                                                                          
077294        ADD 1 TO W-INFIL-KVPOST-IN                                        
077295*    DISPLAY 'INFIL-IDPTYP ' INFIL-IDPTYP                                 
077296     IF INFIL-IDPTYP = '310'                                              
077298        ADD  I310-SUARTFSG-DO   TO SUM-310                                
077299     END-IF                                                               
077300     IF INFIL-IDPTYP = '320'                                              
077302        ADD I320-SUARTFSG-RAB   TO SUM-320                                
077303     END-IF                                                               
077304     IF INFIL-IDPTYP = '330'                                              
077306        ADD I330-SUARTFSG-SPEC  TO SUM-330                                
077307     END-IF                                                               
077308     IF INFIL-IDPTYP = '340'                                              
077310        ADD I340-SUARTFSG-MAN   TO SUM-340                                
077311     END-IF                                                               
077312     IF INFIL-IDPTYP = '350'                                              
077314        ADD I350-SUARTFSG-KRE   TO SUM-350                                
077315     END-IF                                                               
077316     END-READ                                                             
077317     .                                                                    
077318     EJECT                                                                
077319 S03-SKRIV-UTFIL  SECTION.                                                
077320     MOVE 'S03-SKRIV-UTIFIL '  TO WS-SEKTION                              
077321*    DISPLAY 'S03-SKRIV-UTFIL '                                           
077322                                                                          
077323                                                                          
077350     EVALUATE INFIL-IDPTYP                                                
077410       WHEN '310'                                                         
077500          MOVE I310-W330310 TO U310-W330310                               
077600          WRITE UT310-POST FROM UTFIL-AREA                                
078100       WHEN '320'                                                         
078200          MOVE I320-W330320 TO U320-W330320                               
078300          WRITE UT320-POST FROM UTFIL-AREA                                
078400       WHEN '330'                                                         
078500          MOVE I330-W330330 TO U330-W330330                               
078600          WRITE UT330-POST FROM UTFIL-AREA                                
078700       WHEN '340'                                                         
078800          MOVE I340-W330340 TO U340-W330340                               
078900          WRITE UT340-POST FROM UTFIL-AREA                                
079000       WHEN '350'                                                         
079100          MOVE I350-W330350 TO U350-W330350                               
079200          WRITE UT350-POST FROM UTFIL-AREA                                
083200     END-EVALUATE                                                         
083300                                                                          
083400     IF INFIL-IDPTYP = '310' OR '320' OR '330' OR '340'                   
083500                    OR '350'                                              
083800       MOVE INFIL-IDPTYP TO POSTSUM-TRANSTYP                              
083900       MOVE 'W33013 '    TO POSTSUM-FDNAMN                                
084000       MOVE 'W33011D2'   TO POSTSUM-DDNAMN2                               
084100       CALL POSTSUM USING POSTSUM-PARM                                    
084200     END-IF                                                               
084300     .                                                                    
084400     EJECT                                                                
084500 S02-SKRIV-UT-RAD SECTION.                                                
084600     SKIP2                                                                
084700                                                                          
084800     WRITE LISTAS FROM UT-RAD                                             
084900                                                                          
085000     .                                                                    
085100     EJECT                                                                
