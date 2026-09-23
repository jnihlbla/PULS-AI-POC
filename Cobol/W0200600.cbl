000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W0200600.                                                
000400*AUTHOR.         STEFANO GIOBBI.                                          
000500*DATE-WRITTEN.   95/07/26.                                                
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        PROGRAMMET TAR HAND OM SRS-INFORMATION SOM ÄR NERLÄST            
001000*        DELS FRÅN WDG6 OCH DELS FRÅN WDR6.                               
001100*                                                                         
001200                                                                          
001300 ENVIRONMENT DIVISION.                                                    
001400     SKIP2                                                                
001500 INPUT-OUTPUT SECTION.                                                    
001600                                                                          
001700 FILE-CONTROL.                                                            
001800     SKIP2                                                                
001900*          --- FIL FRÅN W0920800, W092P108 (WDG6)                         
002000     SELECT W092S1                     ASSIGN TO W02006D1.                
002100     SKIP2                                                                
002200*          --- NYA WDR4-RADER FRÅN O/E (WDR6)                             
002300     SELECT W4140I                     ASSIGN TO W02006D2.                
002400     SKIP2                                                                
002500*          --- ANNULLERADE WDR4-RADER FRÅN 4224 (WDR6)                    
002600     SELECT W4140D                     ASSIGN TO W02006D3.                
002700     SKIP2                                                                
002800*          --- NY FIL MOTSVARANDE GAMLA W092Y1                            
002900     SELECT W02008                     ASSIGN TO W02006D4.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W092S1                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01  -COPY WDGZ01      -L.                                                
004000     EJECT                                                                
004100 FD  W4140I                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500*01  POST -COPY W414010  -PRE  W4140I-  -L.                               
004600     EJECT                                                                
004700 FD  W4140D                                                               
004800     RECORDING       F                                                    
004900     BLOCK CONTAINS  0.                                                   
005000                                                                          
005100*01  POST -COPY W414011  -PRE  W414OD-  -L.                               
005200     EJECT                                                                
005300 FD  W02008                                                               
005400     RECORDING       F                                                    
005500     BLOCK CONTAINS  0.                                                   
005600                                                                          
005700*01  POST -COPY W02008   -PRE  W02008-  -L.                               
005800     EJECT                                                                
005900 WORKING-STORAGE SECTION.                                                 
005901                                                                          
005910*    -- CHECKED BY WY2000                                                 
006000*                                                                         
006100 77  IDPGM                       PIC X(8)    VALUE 'W0200600'.            
006200 77  JA                          PIC X       VALUE 'J'.                   
006300 77  NEJ                         PIC X       VALUE 'N'.                   
006310 77  SPEC-FORBI                  PIC X       VALUE 'S'.                   
006400*                                                                         
006500 77  W092S1-EOF-SW               PIC X       VALUE 'N'.                   
006600     88  END-OF-W092S1                       VALUE 'J'.                   
006700 77  W4140I-EOF-SW               PIC X       VALUE 'N'.                   
006800     88  END-OF-W4140I                       VALUE 'J'.                   
006900 77  W4140D-EOF-SW               PIC X       VALUE 'N'.                   
007000     88  END-OF-W4140D                       VALUE 'J'.                   
007100     EJECT                                                                
007200*                                                                         
007300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007400*                                                                         
007500 01  FILLER REDEFINES DAGENS-DATUM.                                       
007600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007900     EJECT                                                                
008000*                                                                         
008100 01  TEST-IDDISTR              PIC 9(5)   COMP-3.                         
008200*01  FILLER  -COPY WWDIST18    -RED TEST-IDDISTR.                         
008300*01  FILLER  -COPY WWDIST19    -RED TEST-IDDISTR.                         
008400*01  FILLER  -COPY WWDIST20    -RED TEST-IDDISTR.                         
008500*01  FILLER  -COPY WWDIST93    -RED TEST-IDDISTR.                         
008600     EJECT                                                                
008700 01  DYNAMISKA-SUBPROGRAM.                                                
008800*                                                                         
008900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009100     SKIP2                                                                
009200*                                                                         
009300*    --- PARAMETRAR TILL ABEND                                            
009400*                                                                         
009500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009700     SKIP2                                                                
009800*                                                                         
009900 01  FELTEXT.                                                             
010000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010200     EJECT                                                                
010300*                                                                         
010400*    --- PARAMETRAR TILL POSTSUM                                          
010500*                                                                         
010600*01  -COPY W0005   -PRE  POSTSUM-                                         
010700     EJECT                                                                
010800 01  INS1-AREA-START             PIC X(24)   VALUE                        
010900                                 'INS1-AREA START  '.                     
011000*01  AREA -COPY WDGZ01     -PRE INS1-                                     
011100     EJECT                                                                
011200                                                                          
011300 01  FILLER                      PIC X(24)   VALUE 'RYE-AREA'.            
011400*01  RYE-AREA   -COPY  WDGZRYE                                            
011500     EJECT                                                                
011600                                                                          
011700 01  FILLER                      PIC X(24)   VALUE 'RYES-AREA'.           
011800*01  RYES-AREA  -COPY  WDGZRYES                                           
011900     EJECT                                                                
012000                                                                          
012100 01  FILLER                      PIC X(24)   VALUE 'RY1-AREA'.            
012200*01  RY1-AREA   -COPY  WDGZRY1                                            
012300     EJECT                                                                
012400                                                                          
012500 01  FILLER                      PIC X(24)   VALUE 'RY1S-AREA'.           
012600*01  RY1S-AREA  -COPY  WDGZRY1S                                           
012700     EJECT                                                                
012800                                                                          
012900 01  FILLER                      PIC X(24)   VALUE 'RY5-AREA'.            
013000*01  RY5-AREA   -COPY  WDGZRY5                                            
013100     EJECT                                                                
013200                                                                          
013300 01  FILLER                      PIC X(24)   VALUE 'RY5S-AREA'.           
013400*01  RY5S-AREA  -COPY  WDGZRY5S                                           
013500     EJECT                                                                
013600                                                                          
013700 01  W4140I-AREA-START           PIC X(24)   VALUE                        
013800                                 'W41401-AREA START'.                     
013900*01  W4140I-AREA  -COPY  W414010  -PRE 40I-                               
014000     EJECT                                                                
014100                                                                          
014200 01  W4140D-AREA-START           PIC X(24)   VALUE                        
014300                                 'W4140D-AREA START'.                     
014400*01  W4140D-AREA  -COPY  W414011                                          
014500     EJECT                                                                
014600                                                                          
014700 01  UT08-AREA-START             PIC X(24)   VALUE                        
014800                                 'UT08-AREA-START  '.                     
014900                                                                          
015000*01  AREA -COPY W02008       -PRE UT08-                                   
015100     EJECT                                                                
015200 PROCEDURE DIVISION.                                                      
015300                                                                          
015400     PERFORM A-INIT                                                       
015500                                                                          
015600     PERFORM B-BEHANDLA-WDG6-DATA                                         
015700     PERFORM C-BEHANDLA-VOR-OE-DATA                                       
015800     PERFORM D-BEHANDLA-ANN-VOR-DATA                                      
015900                                                                          
016000     PERFORM Z-FINIT                                                      
016100                                                                          
016200     MOVE ZERO TO RETURN-CODE                                             
016300     GOBACK                                                               
016400     .                                                                    
016500     EJECT                                                                
016600 A-INIT SECTION.                                                          
016700                                                                          
016800     OPEN   INPUT  W092S1                                                 
016900                   W4140I                                                 
017000                   W4140D                                                 
017100                                                                          
017200     OPEN   OUTPUT W02008                                                 
017300                                                                          
017400     ACCEPT DAGENS-DATUM FROM DATE                                        
017500     MOVE   IDPGM        TO   POSTSUM-PROGNAMN                            
017600     .                                                                    
017700     EJECT                                                                
017800 B-BEHANDLA-WDG6-DATA SECTION.                                            
017900                                                                          
018000     PERFORM S01-LAES-W092S1                                              
018100     PERFORM UNTIL END-OF-W092S1                                          
018200                                                                          
018300       EVALUATE INS1-IDPTYP                                               
018400         WHEN 'RYE'                                                       
018500                  PERFORM BA-BEH-RYE-UTSKRIFT                             
018600         WHEN 'RY1'                                                       
018700                  PERFORM BB-BEH-RY1-PACKRAPPORTERING                     
018800         WHEN 'RY5'                                                       
018900                  PERFORM BC-BEH-RY5-ANNULLATION                          
019000       END-EVALUATE                                                       
019100                                                                          
019200       PERFORM S01-LAES-W092S1                                            
019300     END-PERFORM                                                          
019400     .                                                                    
019500     EJECT                                                                
019600 BA-BEH-RYE-UTSKRIFT SECTION.                                             
019700                                                                          
019800     MOVE INS1-LOGGPOST     TO RYE-AREA                                   
019900     MOVE INS1-SORTPOST     TO RYES-AREA                                  
020000                                                                          
020100     IF  RYE-KDORDBEK = ZERO           OR                                 
020200       ((RYE-KDORDBEK = 80             OR                                 
020300         RYE-KDORDBEK = 90           ) AND                                
020400         RYE-KVRO     = RYE-KVBEART-Q) OR                                 
020500         RYE-KDORDBEK = 92             OR                                 
020600         RYE-KDORDBEK = 93                                                
020700                                                                          
020800       MOVE RYES-IDDISTR    TO  TEST-IDDISTR                              
020900                                                                          
021000       IF RYE-KDORDING        = 3             OR                          
021100         (RYE-TIRODAT         > ZERO          AND                         
021200          RYE-IDKUNDRF-RO NOT = SPACE         AND                         
021300                          NOT = '0000000   ') OR                          
021400          RYE-FLDIRLEV        = JA            OR                          
021500          RYE-FLORDSPE        = JA            OR                          
021600          RYES-FLVORKO        = JA            OR                          
021700         (RYES-FLFORBI        = JA            AND                         
021800          RYES-KDORDKL    NOT = 1           ) OR                          
021810         (RYES-FLFORBI        = SPEC-FORBI    AND                         
021820          RYES-KDORDKL    NOT = 1           ) OR                          
021900          RYES-FLOVRLEV       = JA            OR                          
022000          DIST19-SATS                         OR                          
022100          DIST20-EMBALLAGE                    OR                          
022200          DIST93-BYTESRADIO-C2                OR                          
022300          DIST18-SKROT                                                    
022400         CONTINUE                                                         
022500       ELSE                                                               
022600         MOVE SPACE         TO UT08-AREA                                  
022700                                                                          
022800         MOVE RYE-IDARTNR   TO UT08-IDARTNR                               
022900         MOVE RYE-IDDC      TO UT08-IDDC                                  
023000         MOVE RYE-KDPRODSL  TO UT08-KDPRODSL                              
023100         MOVE RYES-IDDISTR  TO UT08-IDDISTR                               
023200         MOVE RYES-IDKUNDNR TO UT08-IDKUNDNR                              
023300         MOVE RYES-KDORDKL  TO UT08-KDORDKL                               
023400         MOVE +1            TO UT08-REINKORD                              
023500         IF RYE-KVAVBART >= RYE-KVBEART-Q - RYES-KVSLATT                  
023600           MOVE 1           TO UT08-REAVBRAD                              
023700         ELSE                                                             
023800           COMPUTE UT08-REAVBRAD = RYE-KVAVBART / RYE-KVBEART-Q           
023900         END-IF                                                           
024000         MOVE ZERO          TO UT08-REFYSAVV                              
024010                                                                          
024200            PERFORM S11-SKRIV-W02008                                      
024220                                                                          
024300       END-IF                                                             
024400     END-IF                                                               
024500     .                                                                    
024600     EJECT                                                                
024700 BB-BEH-RY1-PACKRAPPORTERING SECTION.                                     
024800                                                                          
024900     MOVE INS1-LOGGPOST    TO RY1-AREA                                    
025000     MOVE INS1-SORTPOST    TO RY1S-AREA                                   
025100                                                                          
025200     MOVE RY1S-IDDISTR     TO TEST-IDDISTR                                
025300                                                                          
025400     IF RY1-KDORDING        = 3             OR                            
025500       (RY1-TIRODAT         > ZERO          AND                           
025600        RY1-IDKUNDRF-RO NOT = SPACE         AND                           
025700                        NOT = '00000     ') OR                            
025800        RY1-FLDIRLEV        = JA            OR                            
025900        RY1S-FLORDSPE       = JA            OR                            
026000        RY1S-FLVORKO        = JA            OR                            
026100       (RY1S-FLFORBI        = JA            AND                           
026200        RY1S-KDORDKL    NOT = 1           ) OR                            
026210       (RY1S-FLFORBI        = SPEC-FORBI    AND                           
026220        RY1S-KDORDKL    NOT = 1           ) OR                            
026300        RY1S-FLOVRLEV       = JA            OR                            
026400        DIST19-SATS                         OR                            
026500        DIST20-EMBALLAGE                    OR                            
026600        DIST93-BYTESRADIO-C2                OR                            
026700        RY1-KVAVART         = ZERO          OR                            
026800        RY1-KVAVBART        = RY1-KVLEVART  OR                            
026900        RY1-KDORDTYP        = 3                                           
027000       CONTINUE                                                           
027100     ELSE                                                                 
027200*------------------------------------------- FYSISK AVVIKELSE M.M         
027300                                                                          
027400       MOVE SPACE          TO UT08-AREA                                   
027500                                                                          
027600       MOVE RY1-IDARTNR    TO UT08-IDARTNR                                
027700       MOVE RY1S-IDDISTR   TO UT08-IDDISTR                                
027800       MOVE RY1S-IDKUNDNR  TO UT08-IDKUNDNR                               
027900       MOVE RY1S-IDDC      TO UT08-IDDC                                   
028000       MOVE RY1S-KDORDKL   TO UT08-KDORDKL                                
028100       MOVE RY1S-KDPRODSL  TO UT08-KDPRODSL                               
028200       IF RY1-KVAVBART < RY1-KVBEART - RY1S-KVSLATT                       
028300***       OM RAD LÅG UNDER SLATTGRÄNS REDAN VID UTSKRIFT                  
028400         COMPUTE UT08-REFYSAVV = (RY1-KVAVBART - RY1-KVLEVART)            
028500                                  / RY1-KVAVBART                          
028600         MOVE ZERO         TO UT08-REINKORD                               
028700         MOVE ZERO         TO UT08-REAVBRAD                               
028701                                                                          
028750         PERFORM S11-SKRIV-W02008                                         
028780                                                                          
028900       ELSE                                                               
029000         IF (RY1-KVAVBART >= RY1-KVBEART - RY1S-KVSLATT) AND              
029100            (RY1-KVLEVART <  RY1-KVBEART - RY1S-KVSLATT)                  
029200           COMPUTE UT08-REFYSAVV = (RY1-KVBEART - RY1-KVLEVART)           
029300                                    / RY1-KVBEART                         
029400           MOVE ZERO       TO UT08-REAVBRAD                               
029500           MOVE ZERO       TO UT08-REINKORD                               
029501                                                                          
029550           PERFORM S11-SKRIV-W02008                                       
029700         END-IF                                                           
029800       END-IF                                                             
029900     END-IF                                                               
030000     .                                                                    
030100     EJECT                                                                
030200 BC-BEH-RY5-ANNULLATION SECTION.                                          
030300                                                                          
030400     MOVE INS1-LOGGPOST TO RY5-AREA                                       
030500     MOVE INS1-SORTPOST TO RY5S-AREA                                      
030600                                                                          
030700     MOVE RY5S-IDDISTR  TO TEST-IDDISTR                                   
030800                                                                          
030900     IF RY5-KDORDING        = 3             OR                            
031000       (RY5-TIRODAT         > ZERO          AND                           
031100        RY5-IDKUNDRF-RO NOT = SPACE         AND                           
031200                        NOT = '00000     ') OR                            
031300        RY5-FLDIRLEV        = JA            OR                            
031400        RY5-FLORDSPE        = JA            OR                            
031500        RY5S-FLVORKO        = JA            OR                            
031600       (RY5S-FLFORBI        = JA            AND                           
031700        RY5-KDORDKL     NOT = 1           ) OR                            
031710       (RY5S-FLFORBI        = SPEC-FORBI    AND                           
031720        RY5-KDORDKL     NOT = 1           ) OR                            
031800        RY5S-FLOVRLEV       = JA            OR                            
031900        DIST19-SATS                         OR                            
032000        DIST20-EMBALLAGE                    OR                            
032100        DIST93-BYTESRADIO-C2                OR                            
032200        DIST18-SKROT                        OR                            
032300        RY5-KVANNANT        = ZERO          OR                            
032400        RY5-KDORDTYP        = 3                                           
032500       CONTINUE                                                           
032600     ELSE                                                                 
032700       PERFORM BCA-BEH-RY5-ANN                                            
032800     END-IF                                                               
032900     .                                                                    
033000     EJECT                                                                
033100 BCA-BEH-RY5-ANN SECTION.                                                 
033200                                                                          
033300     MOVE SPACE           TO UT08-AREA                                    
033400                                                                          
033500     MOVE RY5-IDARTNR     TO UT08-IDARTNR                                 
033600     MOVE RY5-IDDC        TO UT08-IDDC                                    
033700     MOVE RY5-KDORDKL     TO UT08-KDORDKL                                 
033800     MOVE RY5S-IDDISTR    TO UT08-IDDISTR                                 
033900     MOVE RY5S-IDKUNDNR   TO UT08-IDKUNDNR                                
034000     MOVE RY5S-KDPRODSL   TO UT08-KDPRODSL                                
034100     MOVE -1              TO UT08-REINKORD                                
034200     MOVE ZERO            TO UT08-REFYSAVV                                
034300     IF RY5-KVAVBART >= RY5-KVBEART - RY5S-KVSLATT                        
034400***    OM RAD LÅG INOM SLATT FÖRE ANNULLATION                             
034500       MOVE -1            TO UT08-REAVBRAD                                
034600     ELSE                                                                 
034700***    OM RAD HADE BRIST UTANFÖR SLATT FÖRE ANNULLATION                   
034800       COMPUTE UT08-REAVBRAD = (RY5-KVAVBART / RY5-KVBEART) * -1          
034900     END-IF                                                               
035000                                                                          
035100     PERFORM S11-SKRIV-W02008                                             
035200     MOVE    SPACE        TO UT08-AREA                                    
035300                                                                          
035400     IF RY5-KVANNANT < RY5-KVAVBART                                       
035500***     ENDAST TRANS VID DELANNULLATION                                   
035600                                                                          
035700       MOVE RY5-IDARTNR   TO UT08-IDARTNR                                 
035800       MOVE RY5-IDDC      TO UT08-IDDC                                    
035900       MOVE RY5-KDORDKL   TO UT08-KDORDKL                                 
036000       MOVE RY5S-IDDISTR  TO UT08-IDDISTR                                 
036100       MOVE RY5S-IDKUNDNR TO UT08-IDKUNDNR                                
036200       MOVE RY5S-KDPRODSL TO UT08-KDPRODSL                                
036300       MOVE +1            TO UT08-REINKORD                                
036400       MOVE ZERO          TO UT08-REFYSAVV                                
036500       IF RY5-KVAVBART - RY5-KVANNANT >=                                  
036600          RY5-KVBEART  - RY5S-KVSLATT                                     
036700***       RAD LIGGER INOM SLATT ÄVEN EFTER DELANNULLERING                 
036800         MOVE +1          TO UT08-REAVBRAD                                
036900       ELSE                                                               
037000         COMPUTE UT08-REAVBRAD = (RY5-KVBEART - RY5-KVANNANT   -          
037100                                 (RY5-KVBEART - RY5-KVAVBART)) /          
037200                                 (RY5-KVBEART - RY5-KVANNANT)             
037300         ON SIZE ERROR                                                    
037400           MOVE ZERO      TO UT08-REAVBRAD                                
037500         END-COMPUTE                                                      
037600       END-IF                                                             
037700                                                                          
037800       PERFORM S11-SKRIV-W02008                                           
037900     END-IF                                                               
038000     .                                                                    
038100     EJECT                                                                
038200 C-BEHANDLA-VOR-OE-DATA SECTION.                                          
038300                                                                          
038400     PERFORM S02-LAES-W4140I                                              
038500     PERFORM UNTIL END-OF-W4140I                                          
038600                                                                          
038700       PERFORM CA-EV-SKAPA-SRSINFO                                        
038800       PERFORM S02-LAES-W4140I                                            
038900                                                                          
039000     END-PERFORM                                                          
039100     .                                                                    
039200     EJECT                                                                
039300 CA-EV-SKAPA-SRSINFO SECTION.                                             
039400                                                                          
039500     MOVE 40I-IDDISTR     TO  TEST-IDDISTR                                
039600                                                                          
039700     IF 40I-KDORDING        = 3             OR                            
039800        40I-FLDIRLEV        = JA            OR                            
039900        40I-FLORDSPE        = JA            OR                            
040000        40I-FLOVRLEV        = JA            OR                            
040100        DIST19-SATS                         OR                            
040200        DIST20-EMBALLAGE                    OR                            
040300        DIST93-BYTESRADIO-C2                OR                            
040400        DIST18-SKROT                                                      
040500       CONTINUE                                                           
040600     ELSE                                                                 
040700       MOVE SPACE         TO UT08-AREA                                    
040800                                                                          
040900       MOVE 40I-IDARTNR   TO UT08-IDARTNR                                 
041000       MOVE 40I-IDDC      TO UT08-IDDC                                    
041100       MOVE 40I-KDPRODSL  TO UT08-KDPRODSL                                
041200       MOVE 40I-IDDISTR   TO UT08-IDDISTR                                 
041300       MOVE 40I-IDKUNDNR  TO UT08-IDKUNDNR                                
041400       MOVE 40I-KDORDKL   TO UT08-KDORDKL                                 
041500       MOVE +1            TO UT08-REINKORD                                
041600       MOVE +0            TO UT08-REAVBRAD                                
041700                             UT08-REFYSAVV                                
041800                                                                          
041900       PERFORM S11-SKRIV-W02008                                           
042000     END-IF                                                               
042100     .                                                                    
042200     EJECT                                                                
042300 D-BEHANDLA-ANN-VOR-DATA SECTION.                                         
042400                                                                          
042500     PERFORM S03-LAES-W4140D                                              
042600     PERFORM UNTIL END-OF-W4140D                                          
042700                                                                          
042800       PERFORM DA-EV-SKAPA-SRSINFO                                        
042900       PERFORM S03-LAES-W4140D                                            
043000                                                                          
043100     END-PERFORM                                                          
043200     .                                                                    
043300     EJECT                                                                
043400 DA-EV-SKAPA-SRSINFO SECTION.                                             
043500                                                                          
043600     MOVE ANNVOR-IDDISTR TO TEST-IDDISTR                                  
043700                                                                          
043800     IF ANNVOR-FLDIRLEV     = JA            OR                            
043900        DIST19-SATS                         OR                            
044000        DIST20-EMBALLAGE                    OR                            
044100        DIST93-BYTESRADIO-C2                OR                            
044200        DIST18-SKROT                        OR                            
044300        ANNVOR-KVANNANT     = ZERO                                        
044400       CONTINUE                                                           
044500     ELSE                                                                 
044600       PERFORM DAA-BEH-PT-ANNVOR                                          
044700     END-IF                                                               
044800     .                                                                    
044900     EJECT                                                                
045000 DAA-BEH-PT-ANNVOR SECTION.                                               
045100                                                                          
045200     MOVE SPACE           TO UT08-AREA                                    
045300                                                                          
045400     MOVE ANNVOR-IDARTNR  TO UT08-IDARTNR                                 
045500     MOVE ANNVOR-IDDC     TO UT08-IDDC                                    
045600     MOVE +0              TO UT08-KDORDKL                                 
045700     MOVE ANNVOR-IDDISTR  TO UT08-IDDISTR                                 
045800     MOVE ANNVOR-IDKUNDNR TO UT08-IDKUNDNR                                
045900     MOVE ANNVOR-KDPRODSL TO UT08-KDPRODSL                                
046000     MOVE -1              TO UT08-REINKORD                                
046100     MOVE ZERO            TO UT08-REFYSAVV                                
046200                                                                          
046300     COMPUTE UT08-REAVBRAD = (ANNVOR-KVAVBART /                           
046400                              ANNVOR-KVBEART-Q) * -1                      
046500                                                                          
046600     PERFORM S11-SKRIV-W02008                                             
046700     MOVE    SPACE        TO UT08-AREA                                    
046800                                                                          
046900     IF ANNVOR-KVANNANT = ANNVOR-KVBEART-Q                                
047000       CONTINUE                                                           
047100***     ANNULLATION AV HEL RAD                                            
047200     ELSE                                                                 
047300***     ENDAST TRANS VID DELANNULLATION                                   
047400                                                                          
047500       MOVE ANNVOR-IDARTNR  TO UT08-IDARTNR                               
047600       MOVE ANNVOR-IDDC     TO UT08-IDDC                                  
047700       MOVE +0              TO UT08-KDORDKL                               
047800       MOVE ANNVOR-IDDISTR  TO UT08-IDDISTR                               
047900       MOVE ANNVOR-IDKUNDNR TO UT08-IDKUNDNR                              
048000       MOVE ANNVOR-KDPRODSL TO UT08-KDPRODSL                              
048100       MOVE +1              TO UT08-REINKORD                              
048200       MOVE ZERO            TO UT08-REFYSAVV                              
048300                                                                          
048400       COMPUTE UT08-REAVBRAD =  ANNVOR-KVAVBART  /                        
048500                               (ANNVOR-KVBEART-Q -                        
048600                                ANNVOR-KVANNANT)                          
048700         ON SIZE ERROR                                                    
048800           MOVE ZERO        TO UT08-REAVBRAD                              
048900       END-COMPUTE                                                        
049000                                                                          
049100       PERFORM S11-SKRIV-W02008                                           
049200     END-IF                                                               
049300     .                                                                    
049400     EJECT                                                                
049500 Z-FINIT SECTION.                                                         
049600                                                                          
049700     CLOSE W092S1                                                         
049800           W02008                                                         
049900                                                                          
050000     MOVE 'S' TO POSTSUM-OPKOD                                            
050100     CALL POSTSUM USING POSTSUM-PARM                                      
050200     .                                                                    
050300     EJECT                                                                
050400 S01-LAES-W092S1 SECTION.                                                 
050500                                                                          
050600     READ W092S1 INTO INS1-AREA                                           
050700     AT END                                                               
050800        SET END-OF-W092S1 TO TRUE                                         
050900                                                                          
051000     NOT AT END                                                           
051100        MOVE 'W092S1'     TO    POSTSUM-FDNAMN                            
051200        MOVE 'W02006D1'   TO    POSTSUM-DDNAMN2                           
051300        MOVE  INS1-IDPTYP TO    POSTSUM-TRANSTYP                          
051400        CALL  POSTSUM     USING POSTSUM-PARM                              
051500     END-READ                                                             
051600     .                                                                    
051700     EJECT                                                                
051800 S02-LAES-W4140I SECTION.                                                 
051900                                                                          
052000     READ W4140I INTO 40I-W4140I-AREA                                     
052100     AT END                                                               
052200        SET END-OF-W4140I TO TRUE                                         
052300                                                                          
052400     NOT AT END                                                           
052500        MOVE 'W4140I'     TO    POSTSUM-FDNAMN                            
052600        MOVE 'W02006D2'   TO    POSTSUM-DDNAMN2                           
052700        MOVE 'VOR O/E'    TO    POSTSUM-TRANSTYP                          
052800        CALL  POSTSUM     USING POSTSUM-PARM                              
052900     END-READ                                                             
053000     .                                                                    
053100     EJECT                                                                
053200 S03-LAES-W4140D  SECTION.                                                
053300                                                                          
053400     READ W4140D INTO W4140D-AREA                                         
053500     AT END                                                               
053600        SET END-OF-W4140D TO TRUE                                         
053700                                                                          
053800     NOT AT END                                                           
053900        MOVE 'W4140D'     TO    POSTSUM-FDNAMN                            
054000        MOVE 'W02006D3'   TO    POSTSUM-DDNAMN2                           
054100        MOVE 'VOR ANN'    TO    POSTSUM-TRANSTYP                          
054200        CALL  POSTSUM     USING POSTSUM-PARM                              
054300     END-READ                                                             
054400     .                                                                    
054500     EJECT                                                                
054600 S11-SKRIV-W02008 SECTION.                                                
054700                                                                          
054800     WRITE W02008-POST    FROM  UT08-AREA                                 
054900                                                                          
055000     MOVE  'UT08'         TO    POSTSUM-TRANSTYP                          
055100     MOVE  'W02008'       TO    POSTSUM-FDNAMN                            
055200     MOVE  'W02006D4'     TO    POSTSUM-DDNAMN2                           
055300     CALL   POSTSUM       USING POSTSUM-PARM                              
055400     .                                                                    
055500     EJECT                                                                
