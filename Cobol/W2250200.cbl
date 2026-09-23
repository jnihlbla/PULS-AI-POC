000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2250200.                                                
000400*AUTHOR.         STEFANO GIOBBI (PAH).                                    
000500*DATE-WRITTEN.   95/07/26.                                                
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        PROGRAMMET TAR HAND OM SRS-INFORMATION SOM ÄR NERLÄST            
001000*        DELS FRÅN WDG6 OCH DELS FRÅN WDR6.                               
001100*                                                                         
001110*        THE PROGRAM READS     WDK6                                       
001130*                                                                         
001200                                                                          
001300 ENVIRONMENT DIVISION.                                                    
001400     SKIP2                                                                
001500 INPUT-OUTPUT SECTION.                                                    
001600                                                                          
001700 FILE-CONTROL.                                                            
001800     SKIP2                                                                
001900*          --- FIL FRÅN W0920800, W092P108 (WDG6)                         
002000     SELECT W092S1                     ASSIGN TO W22502D1.                
002100     SKIP2                                                                
002200*          --- NYA WDR4-RADER FRÅN O/E (WDR6)                             
002300     SELECT W4140I                     ASSIGN TO W22502D2.                
002400                                                                          
002500*          --- ANNULLERADE WDR4-RADER FRÅN 4224 (WDR6)                    
002600     SELECT W4140D                     ASSIGN TO W22502D3.                
002700                                                                          
002800*          --- NY FIL MOTSVARANDE GAMLA W092Y1  TILL W020XX               
002900     SELECT W22508                     ASSIGN TO W22502D4.                
003000                                                                          
003100*          --- NY FIL MOTSVARANDE GAMLA W092Z1  TILL W22506               
003200     SELECT W22503                     ASSIGN TO W22502D5.                
003300                                                                          
003400*          --- NY FIL MOTSVARANDE GAMLA W092Z1  TILL W092P109             
003500     SELECT W22502                     ASSIGN TO W22502D6.                
003600                                                                          
003700*          --- KONSTANTMEDLEM MED TABELL DISTRIKT-MARKNADSKOD             
003800     SELECT W020TB                     ASSIGN TO W22502D7.                
003900                                                                          
004000*          --- SECURE LISTS TO PURCHASE PLANNING, DAGSERV                 
004100     SELECT W2250201                   ASSIGN TO W22502D8.                
004200     EJECT                                                                
004300 DATA DIVISION.                                                           
004400     SKIP3                                                                
004500 FILE SECTION.                                                            
004600     SKIP3                                                                
004700 FD  W092S1                                                               
004800     RECORDING       F                                                    
004900     BLOCK CONTAINS  0.                                                   
005000                                                                          
005100*01  -COPY WDGZ01      -L.                                                
005200     EJECT                                                                
005300 FD  W4140I                                                               
005400     RECORDING       F                                                    
005500     BLOCK CONTAINS  0.                                                   
005600                                                                          
005700*01  POST -COPY W414010  -PRE  W4140I-  -L.                               
005800     EJECT                                                                
005900 FD  W4140D                                                               
006000     RECORDING       F                                                    
006100     BLOCK CONTAINS  0.                                                   
006200                                                                          
006300*01  POST -COPY W414011  -PRE  W414OD-  -L.                               
006400     EJECT                                                                
006500 FD  W22508                                                               
006600     RECORDING       F                                                    
006700     BLOCK CONTAINS  0.                                                   
006800                                                                          
006900*01  POST -COPY W02008   -PRE  W22508-  -L.                               
007000     EJECT                                                                
007100 FD  W22503                                                               
007200     RECORDING       F                                                    
007300     BLOCK CONTAINS  0.                                                   
007400                                                                          
007500*01  POST -COPY W225P232 -PRE  W22503-  -L.                               
007600     EJECT                                                                
007700 FD  W22502                                                               
007800     RECORDING       F                                                    
007900     BLOCK CONTAINS  0.                                                   
008000                                                                          
008100*01  POST -COPY W02008   -PRE  W22502-  -L.                               
008200     EJECT                                                                
008300 FD  W020TB                                                               
008400     RECORDING       F                                                    
008500     BLOCK CONTAINS  0.                                                   
008600                                                                          
008700 01  TAB-POST                    PIC X(80).                               
009000     EJECT                                                                
009100 FD  W2250201                                                             
009200     RECORDING       F                                                    
009300     BLOCK CONTAINS  0.                                                   
009400                                                                          
009500*01  POST -COPY W2250201 -PRE  W2250201- -L.                              
009600     EJECT                                                                
009700 WORKING-STORAGE SECTION.                                                 
009800                                                                          
009900*    -- CHECKED BY WY2000                                                 
010000*                                                                         
010100 77  IDPGM                       PIC X(8)    VALUE 'W2250200'.            
010110 77  CURRENT-SECTION             PIC X(32)   VALUE SPACE.                 
010120 77  CURRENT-IMS-SECTION         PIC X(32)   VALUE SPACE.                 
010200 77  JA                          PIC X       VALUE 'J'.                   
010300 77  NEJ                         PIC X       VALUE 'N'.                   
010400 77  SPEC-FORBI                  PIC X       VALUE 'S'.                   
010410 77  W-IDANSK                    PIC 9(03)   VALUE ZERO.                  
010500*                                                                         
010600 77  W092S1-EOF-SW               PIC X       VALUE 'N'.                   
010700     88  END-OF-W092S1                       VALUE 'J'.                   
010800 77  W4140I-EOF-SW               PIC X       VALUE 'N'.                   
010900     88  END-OF-W4140I                       VALUE 'J'.                   
011000 77  W4140D-EOF-SW               PIC X       VALUE 'N'.                   
011100     88  END-OF-W4140D                       VALUE 'J'.                   
011200 77  W020TB-EOF-SW               PIC X       VALUE 'N'.                   
011300     88  END-OF-W020TB                       VALUE 'J'.                   
011400                                                                          
011500 01  WS-DISTRIKT-KOLL            PIC S9(5)   COMP-3.                      
011600     88  DISTR-UNDANTAG          VALUE  1258                              
011700                                        1283                              
011800                                        1478                              
011900                                        1678                              
012000                                        1822                              
012100                                        2078                              
012200                                        2178                              
012300                                        2278                              
012400                                        2334                              
012500                                        2374                              
012600                                        2378                              
012700                                        1378.                             
012800 01  WS-KDORDKL-KOLL             PIC S9(1)   COMP-3.                      
012900     88  KDORDKL-0-1-2           VALUE  0 1 2.                            
013000     SKIP2                                                                
013100 01  IX-TAB                      PIC S9(3) COMP-3.                        
013200 01  IX-TAB-MAX                  PIC S9(3) COMP-3.                        
013300                                                                          
013400 01  TAB-KDMARK.                                                          
013500     03  FILLER     OCCURS 300.                                           
013600         05  TAB-IDDISTR-START   PIC S9(5) COMP-3.                        
013700         05  TAB-IDDISTR-SLUT    PIC S9(5) COMP-3.                        
013800         05  TAB-KDMARK-SERV     PIC S9(3) COMP-3.                        
013900     EJECT                                                                
014000*                                                                         
014100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
014200*                                                                         
014300 01  FILLER REDEFINES DAGENS-DATUM.                                       
014400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
014500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
014600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
014700     EJECT                                                                
014800*                                                                         
014900 01  TEST-IDDISTR              PIC 9(5)   COMP-3.                         
015000*01  FILLER  -COPY WWDIST18    -RED TEST-IDDISTR.                         
015100*01  FILLER  -COPY WWDIST19    -RED TEST-IDDISTR.                         
015200*01  FILLER  -COPY WWDIST20    -RED TEST-IDDISTR.                         
015300*01  FILLER  -COPY WWDIST93    -RED TEST-IDDISTR.                         
015400     EJECT                                                                
015500*      --- VALID IDDC CODES                                               
015600*                                                                         
015700*01    -COPY WWDC99                                                       
015800       EJECT                                                              
015900 01  DYNAMISKA-SUBPROGRAM.                                                
016000*                                                                         
016100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
016200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
016210     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016220     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016300     SKIP2                                                                
016310 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016320     SKIP3                                                                
016330 01  KEYS-TILL-DLI.                                                       
016340     03  W-IDARTNR-X.                                                     
016350         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
016351                                                                          
016360*    --- STATUS-KOD FRÅN IMS                                              
016370 01  STATUS-WS                   PIC XX.                                  
016380     88  SEGMENT-FOUND                       VALUE '  '.                  
016390     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
016391     88  SEGMENT-MISSING                     VALUE 'GE'.                  
016392     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
016393     88  IMS-NOT-OK                          VALUE 'XD'.                  
016394     SKIP2                                                                
016395 01  GOOD-STATUSCODES.                                                    
016396     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016397     SKIP3                                                                
016398 01  SSA1                        PIC X(64).                               
016399 01  SSA2                        PIC X(64).                               
016401     EJECT                                                                
016402*    --- IMS FUNCTION CODES                                               
016403*01  -COPY W0003                                                          
016404     EJECT                                                                
016405*    ---  DLI INPUT-OUTPUT AREA                                           
016406                                                                          
016407 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
016408 01  DLI-IO-WDK611.                                                       
016409*    03  -COPY WDK611                                                     
016410     EJECT                                                                
016430*                                                                         
016500*    --- PARAMETRAR TILL ABEND                                            
016600*                                                                         
016700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
016800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
016900     SKIP2                                                                
017000*                                                                         
017410 01  ERROR-TEXT.                                                          
017420     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
017430     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
017500*                                                                         
017600*    --- PARAMETRAR TILL POSTSUM                                          
017700*                                                                         
017800*01  -COPY W0005   -PRE  POSTSUM-                                         
017900     EJECT                                                                
018000 01  INS1-AREA-START             PIC X(24)   VALUE                        
018100                                 'INS1-AREA START  '.                     
018200*01  AREA -COPY WDGZ01     -PRE INS1-                                     
018300     EJECT                                                                
018400                                                                          
018500 01  FILLER                      PIC X(24)   VALUE 'RYE-AREA'.            
018600*01  RYE-AREA   -COPY  WDGZRYE                                            
018700     EJECT                                                                
018800                                                                          
018900 01  FILLER                      PIC X(24)   VALUE 'RYES-AREA'.           
019000*01  RYES-AREA  -COPY  WDGZRYES                                           
019100     EJECT                                                                
019200                                                                          
019300 01  FILLER                      PIC X(24)   VALUE 'RY1-AREA'.            
019400*01  RY1-AREA   -COPY  WDGZRY1                                            
019500     EJECT                                                                
019600                                                                          
019700 01  FILLER                      PIC X(24)   VALUE 'RY1S-AREA'.           
019800*01  RY1S-AREA  -COPY  WDGZRY1S                                           
019900     EJECT                                                                
020000                                                                          
020100 01  FILLER                      PIC X(24)   VALUE 'RY5-AREA'.            
020200*01  RY5-AREA   -COPY  WDGZRY5                                            
020300     EJECT                                                                
020400                                                                          
020500 01  FILLER                      PIC X(24)   VALUE 'RY5S-AREA'.           
020600*01  RY5S-AREA  -COPY  WDGZRY5S                                           
020700     EJECT                                                                
020800                                                                          
020900 01  W4140I-AREA-START           PIC X(24)   VALUE                        
021000                                 'W41401-AREA START'.                     
021100*01  W4140I-AREA  -COPY  W414010  -PRE 40I-                               
021200     EJECT                                                                
021300                                                                          
021400 01  W4140D-AREA-START           PIC X(24)   VALUE                        
021500                                 'W4140D-AREA START'.                     
021600*01  W4140D-AREA  -COPY  W414011                                          
021700     EJECT                                                                
021800                                                                          
021900 01  UT08-AREA-START             PIC X(24)   VALUE                        
022000                                 'UT08-AREA-START  '.                     
022100                                                                          
022200*01  AREA -COPY W02008       -PRE UT08-                                   
022300     EJECT                                                                
022400                                                                          
022500 01  UT03-AREA-START             PIC X(24)   VALUE                        
022600                                 'UT03-AREA-START  '.                     
022700                                                                          
022800*01  AREA -COPY W225P232     -PRE UT03-                                   
022900     EJECT                                                                
023000                                                                          
023010 01  UT09-AREA-START        PIC X(24)   VALUE                             
023020                                 'UT09-AREA-START  '.                     
023030                                                                          
023040*01  AREA -COPY W2250201     -PRE UT09-                                   
023050     EJECT                                                                
023060                                                                          
023100 01  INTB-AREA-START             PIC X(24)   VALUE                        
023200                                 'INTB-AREA-START  '.                     
023300                                                                          
023400 01  INTB-AREA.                                                           
023500     03  INTB-IDDISTR-START      PIC 9(4).                                
023600     03  FILLER                  PIC X.                                   
023700     03  INTB-IDDISTR-SLUT       PIC 9(4).                                
023800     03  FILLER                  PIC X.                                   
023900     03  INTB-KDMARK-SERV        PIC 9(3).                                
024000     03  FILLER                  PIC X(67).                               
024100     EJECT                                                                
024110 LINKAGE SECTION.                                                         
024120                                                                          
024150*01  -COPY W0008  -PRE WDK6-                                              
024160     05  FILLER                  PIC X.                                   
024170                                                                          
024192 PROCEDURE DIVISION  USING WDK6-PCB.                                      
024193 MAIN SECTION.                                                            
024194     ENTRY 'DLITCBL' USING WDK6-PCB.                                      
024300                                                                          
024400     PERFORM A-INIT                                                       
024500                                                                          
024600     PERFORM B-BEHANDLA-WDG6-DATA                                         
024700     PERFORM C-BEHANDLA-VOR-OE-DATA                                       
024800     PERFORM D-BEHANDLA-ANN-VOR-DATA                                      
024900                                                                          
025000     PERFORM Z-FINIT                                                      
025100                                                                          
025200     MOVE ZERO TO RETURN-CODE                                             
025300     GOBACK                                                               
025400     .                                                                    
025500     EJECT                                                                
025600 A-INIT SECTION.                                                          
025700                                                                          
025800     OPEN   INPUT  W092S1                                                 
025900                   W4140I                                                 
026000                   W4140D                                                 
026100                   W020TB                                                 
026200                                                                          
026300     OPEN   OUTPUT W22508                                                 
026400                   W22503                                                 
026500                   W22502                                                 
026600                   W2250201                                               
026700                                                                          
026800     ACCEPT DAGENS-DATUM FROM DATE                                        
026900     MOVE   IDPGM               TO   POSTSUM-PROGNAMN                     
027000     SKIP2                                                                
027100     MOVE +1                    TO IX-TAB                                 
027200     PERFORM S04-LAES-W020TB                                              
027300     PERFORM UNTIL END-OF-W020TB                                          
027400        MOVE INTB-IDDISTR-START TO TAB-IDDISTR-START (IX-TAB)             
027500        MOVE INTB-IDDISTR-SLUT  TO TAB-IDDISTR-SLUT  (IX-TAB)             
027600        MOVE INTB-KDMARK-SERV   TO TAB-KDMARK-SERV   (IX-TAB)             
027700        ADD +1                  TO IX-TAB                                 
027800        PERFORM S04-LAES-W020TB                                           
027900     END-PERFORM                                                          
028000     SUBTRACT 1               FROM IX-TAB                                 
028100     MOVE IX-TAB                TO IX-TAB-MAX                             
028200     .                                                                    
028300     EJECT                                                                
028400 B-BEHANDLA-WDG6-DATA SECTION.                                            
028500                                                                          
028600     PERFORM S01-LAES-W092S1                                              
028700     PERFORM UNTIL END-OF-W092S1                                          
028800                                                                          
028900       EVALUATE INS1-IDPTYP                                               
029000         WHEN 'RYE'                                                       
029100                  PERFORM BA-BEH-RYE-UTSKRIFT                             
029200         WHEN 'RY1'                                                       
029300                  PERFORM BB-BEH-RY1-PACKRAPPORTERING                     
029400         WHEN 'RY5'                                                       
029500                  PERFORM BC-BEH-RY5-ANNULLATION                          
029600       END-EVALUATE                                                       
029700                                                                          
029800       PERFORM S01-LAES-W092S1                                            
029900     END-PERFORM                                                          
030000     .                                                                    
030100     EJECT                                                                
030200 BA-BEH-RYE-UTSKRIFT SECTION.                                             
030300                                                                          
030400     MOVE INS1-LOGGPOST     TO RYE-AREA                                   
030500     MOVE INS1-SORTPOST     TO RYES-AREA                                  
030600                                                                          
030700     IF  RYE-KDORDBEK = ZERO           OR                                 
030800       ((RYE-KDORDBEK = 80             OR                                 
030900         RYE-KDORDBEK = 90           ) AND                                
031000         RYE-KVRO     = RYE-KVBEART-Q) OR                                 
031100         RYE-KDORDBEK = 92             OR                                 
031200         RYE-KDORDBEK = 93                                                
031300                                                                          
031400       MOVE RYES-IDDISTR    TO  TEST-IDDISTR                              
031500       MOVE RYE-IDDC        TO  WS-IDDC                                   
031600                                                                          
031700       IF RYE-KDORDING        = 3             OR                          
031800         (RYE-TIRODAT         > ZERO          AND                         
031900          RYE-IDKUNDRF-RO NOT = SPACE         AND                         
032000                          NOT = '0000000   ') OR                          
032100          RYE-FLDIRLEV        = JA            OR                          
032200          GOOD-DDC                            OR                          
032300          RYE-FLORDSPE        = JA            OR                          
032400          RYES-FLVORKO        = JA            OR                          
032500         (RYES-FLFORBI        = JA            AND                         
032600          RYES-KDORDKL    NOT = 1           ) OR                          
032700         (RYES-FLFORBI        = SPEC-FORBI    AND                         
032800          RYES-KDORDKL    NOT = 1           ) OR                          
032900          RYES-FLOVRLEV       = JA            OR                          
033000          DIST19-SATS                         OR                          
033100          DIST20-EMBALLAGE                    OR                          
033200          DIST93-BYTESRADIO-C2                OR                          
033300          DIST18-SKROT                                                    
033400         CONTINUE                                                         
033500       ELSE                                                               
033600         MOVE SPACE         TO UT08-AREA      UT03-AREA                   
033700                                                                          
033800         MOVE RYE-IDARTNR   TO UT08-IDARTNR   UT03-IDARTNR                
033900         MOVE RYE-IDDC      TO UT08-IDDC                                  
034000         MOVE RYE-KDPRODSL  TO UT08-KDPRODSL                              
034100         MOVE RYES-IDDISTR  TO UT08-IDDISTR   UT03-IDDISTR                
034200         MOVE RYES-IDKUNDNR TO UT08-IDKUNDNR                              
034300         MOVE RYES-KDORDKL  TO UT08-KDORDKL   UT03-KDORDKL                
034400         MOVE +1            TO UT08-REINKORD  UT03-REINKORD               
034500         IF RYE-KVAVBART >= RYE-KVBEART-Q - RYES-KVSLATT                  
034600           MOVE 1           TO UT08-REAVBRAD                              
034700           MOVE ZERO        TO UT03-RERORAD                               
034800         ELSE                                                             
034900           COMPUTE UT08-REAVBRAD = RYE-KVAVBART / RYE-KVBEART-Q           
035000           COMPUTE UT03-RERORAD  =                                        
035100                 (RYE-KVBEART-Q - RYE-KVAVBART) / RYE-KVBEART-Q           
035200         END-IF                                                           
035300         MOVE ZERO          TO UT08-REFYSAVV  UT03-REFYSAVV               
035400         MOVE UT03-RERORAD  TO UT08-RERORAD                               
035500         MOVE RYE-KVBEART-Q TO UT08-KVBEART                               
035600                                                                          
035700            PERFORM S11-SKRIV-W22508                                      
035800                                                                          
035810            PERFORM S13-SKRIV-W2250201                                    
035820                                                                          
035900         MOVE '232'         TO UT03-IDPTYP                                
036000         MOVE UT08-REAVBRAD TO UT03-REAVBRAD                              
036100         MOVE RYE-IDDC(1:1) TO UT03-KDCLAGER                              
036200                                                                          
036300         MOVE RYE-IDDC     TO WS-IDDC                                     
036400         IF GOOD-DC                                                       
036500            PERFORM S12-SKRIV-W22503                                      
036600         END-IF                                                           
036700       END-IF                                                             
036800     END-IF                                                               
036900     .                                                                    
037000     EJECT                                                                
037100 BB-BEH-RY1-PACKRAPPORTERING SECTION.                                     
037200                                                                          
037300     MOVE INS1-LOGGPOST    TO RY1-AREA                                    
037400     MOVE INS1-SORTPOST    TO RY1S-AREA                                   
037500                                                                          
037600     MOVE RY1S-IDDISTR     TO TEST-IDDISTR                                
037700     MOVE RY1S-IDDC        TO WS-IDDC                                     
037800                                                                          
037900     IF RY1-KDORDING        = 3             OR                            
038000       (RY1-TIRODAT         > ZERO          AND                           
038100        RY1-IDKUNDRF-RO NOT = SPACE         AND                           
038200                        NOT = '00000     ') OR                            
038300        RY1-FLDIRLEV        = JA            OR                            
038400        GOOD-DDC                            OR                            
038500        RY1S-FLORDSPE       = JA            OR                            
038600        RY1S-FLVORKO        = JA            OR                            
038700       (RY1S-FLFORBI        = JA            AND                           
038800        RY1S-KDORDKL    NOT = 1           ) OR                            
038900       (RY1S-FLFORBI        = SPEC-FORBI    AND                           
039000        RY1S-KDORDKL    NOT = 1           ) OR                            
039100        RY1S-FLOVRLEV       = JA            OR                            
039200        DIST19-SATS                         OR                            
039300        DIST20-EMBALLAGE                    OR                            
039400        DIST93-BYTESRADIO-C2                OR                            
039500        RY1-KVAVART         = ZERO          OR                            
039600        RY1-KVAVBART        = RY1-KVLEVART  OR                            
039700        RY1-KDORDTYP        = 3                                           
039800       CONTINUE                                                           
039900     ELSE                                                                 
040000*------------------------------------------- FYSISK AVVIKELSE M.M         
040100                                                                          
040200       MOVE SPACE          TO UT08-AREA      UT03-AREA                    
040300                                                                          
040400       MOVE RY1-IDARTNR    TO UT08-IDARTNR   UT03-IDARTNR                 
040500       MOVE RY1S-IDDISTR   TO UT08-IDDISTR   UT03-IDDISTR                 
040600       MOVE RY1S-IDKUNDNR  TO UT08-IDKUNDNR                               
040700       MOVE RY1S-IDDC      TO UT08-IDDC                                   
040800       MOVE RY1S-KDORDKL   TO UT08-KDORDKL   UT03-KDORDKL                 
040900       MOVE RY1S-KDPRODSL  TO UT08-KDPRODSL                               
041000       IF RY1-KVAVBART < RY1-KVBEART - RY1S-KVSLATT                       
041100***       OM RAD LÅG UNDER SLATTGRÄNS REDAN VID UTSKRIFT                  
041200         COMPUTE UT08-REFYSAVV = (RY1-KVAVBART - RY1-KVLEVART)            
041300                                  / RY1-KVAVBART                          
041400                                                                          
041500         PERFORM BBA-SKRIV-08-03                                          
041600                                                                          
041700       ELSE                                                               
041800         IF (RY1-KVAVBART >= RY1-KVBEART - RY1S-KVSLATT) AND              
041900            (RY1-KVLEVART <  RY1-KVBEART - RY1S-KVSLATT)                  
042000           COMPUTE UT08-REFYSAVV = (RY1-KVBEART - RY1-KVLEVART)           
042100                                    / RY1-KVBEART                         
042200                                                                          
042300           PERFORM BBA-SKRIV-08-03                                        
042400                                                                          
042500         END-IF                                                           
042600       END-IF                                                             
042700     END-IF                                                               
042800     .                                                                    
042900     EJECT                                                                
043000 BBA-SKRIV-08-03        SECTION.                                          
043100                                                                          
043200         MOVE ZERO         TO UT08-REINKORD  UT03-REINKORD                
043300         MOVE ZERO         TO UT08-REAVBRAD  UT03-REAVBRAD                
043400         IF RY1-FLRESTN      =  NEJ                                       
043500            MOVE ZERO        TO UT03-RERORAD                              
043600         ELSE                                                             
043700            COMPUTE UT03-RERORAD =                                        
043800                                RY1-KVAVART / RY1-KVBEART                 
043900         END-IF                                                           
044000         MOVE UT03-RERORAD TO UT08-RERORAD                                
044100         MOVE RY1-KVBEART  TO UT08-KVBEART                                
044200                                                                          
044300         PERFORM S11-SKRIV-W22508                                         
044400                                                                          
044500         MOVE '232'          TO UT03-IDPTYP                               
044600         MOVE UT08-REFYSAVV  TO UT03-REFYSAVV                             
044700         MOVE RY1S-IDDC(1:1) TO UT03-KDCLAGER                             
044800                                                                          
044900         MOVE RY1S-IDDC      TO WS-IDDC                                   
045000         IF GOOD-DC                                                       
045100            PERFORM S12-SKRIV-W22503                                      
045200         END-IF                                                           
045300     .                                                                    
045400     EJECT                                                                
045500 BC-BEH-RY5-ANNULLATION SECTION.                                          
045600                                                                          
045700     MOVE INS1-LOGGPOST TO RY5-AREA                                       
045800     MOVE INS1-SORTPOST TO RY5S-AREA                                      
045900                                                                          
046000     MOVE RY5S-IDDISTR  TO TEST-IDDISTR                                   
046100     MOVE RY5-IDDC      TO WS-IDDC                                        
046200                                                                          
046300     IF RY5-KDORDING        = 3             OR                            
046400       (RY5-TIRODAT         > ZERO          AND                           
046500        RY5-IDKUNDRF-RO NOT = SPACE         AND                           
046600                        NOT = '00000     ') OR                            
046700        RY5-FLDIRLEV        = JA            OR                            
046800        GOOD-DDC                            OR                            
046900        RY5-FLORDSPE        = JA            OR                            
047000        RY5S-FLVORKO        = JA            OR                            
047100       (RY5S-FLFORBI        = JA            AND                           
047200        RY5-KDORDKL     NOT = 1           ) OR                            
047300       (RY5S-FLFORBI        = SPEC-FORBI    AND                           
047400        RY5-KDORDKL     NOT = 1           ) OR                            
047500        RY5S-FLOVRLEV       = JA            OR                            
047600        DIST19-SATS                         OR                            
047700        DIST20-EMBALLAGE                    OR                            
047800        DIST93-BYTESRADIO-C2                OR                            
047900        DIST18-SKROT                        OR                            
048000        RY5-KVANNANT        = ZERO          OR                            
048100        RY5-KDORDTYP        = 3                                           
048200       CONTINUE                                                           
048300     ELSE                                                                 
048400       PERFORM BCA-BEH-RY5-ANN                                            
048500     END-IF                                                               
048600     .                                                                    
048700     EJECT                                                                
048800 BCA-BEH-RY5-ANN SECTION.                                                 
048900                                                                          
049000     MOVE SPACE           TO UT08-AREA      UT03-AREA                     
049100                                                                          
049200     MOVE RY5-IDARTNR     TO UT08-IDARTNR   UT03-IDARTNR                  
049300     MOVE RY5-IDDC        TO UT08-IDDC                                    
049400     MOVE RY5-KDORDKL     TO UT08-KDORDKL   UT03-KDORDKL                  
049500     MOVE RY5S-IDDISTR    TO UT08-IDDISTR   UT03-IDDISTR                  
049600     MOVE RY5S-IDKUNDNR   TO UT08-IDKUNDNR                                
049700     MOVE RY5S-KDPRODSL   TO UT08-KDPRODSL                                
049800     MOVE -1              TO UT08-REINKORD  UT03-REINKORD                 
049900     MOVE ZERO            TO UT08-REFYSAVV  UT03-REFYSAVV                 
050000     IF RY5-KVAVBART >= RY5-KVBEART - RY5S-KVSLATT                        
050100***    OM RAD LÅG INOM SLATT FÖRE ANNULLATION                             
050200       MOVE -1            TO UT08-REAVBRAD                                
050300       MOVE ZERO          TO UT03-RERORAD                                 
050400     ELSE                                                                 
050500***    OM RAD HADE BRIST UTANFÖR SLATT FÖRE ANNULLATION                   
050600       COMPUTE UT08-REAVBRAD = (RY5-KVAVBART / RY5-KVBEART) * -1          
050700       COMPUTE UT03-RERORAD  =                                            
050800               ((RY5-KVBEART - RY5-KVAVBART) / RY5-KVBEART) * -1          
050900     END-IF                                                               
051000     MOVE UT03-RERORAD    TO UT08-RERORAD                                 
051100     MOVE RY5-KVBEART     TO UT08-KVBEART                                 
051200                                                                          
051300     PERFORM S11-SKRIV-W22508                                             
051320                                                                          
051500     MOVE '232'          TO UT03-IDPTYP                                   
051600     MOVE RY5-IDDC (1:1) TO UT03-KDCLAGER                                 
051700     MOVE UT08-REAVBRAD  TO UT03-REAVBRAD                                 
051800                                                                          
051900     MOVE RY5-IDDC      TO WS-IDDC                                        
052000     IF GOOD-DC                                                           
052100        PERFORM S12-SKRIV-W22503                                          
052200     END-IF                                                               
052300                                                                          
052400     MOVE    SPACE        TO UT08-AREA       UT03-AREA                    
052500                                                                          
052600     IF RY5-KVANNANT < RY5-KVAVBART                                       
052700***     ENDAST TRANS VID DELANNULLATION                                   
052800                                                                          
052900       MOVE RY5-IDARTNR   TO UT08-IDARTNR    UT03-IDARTNR                 
053000       MOVE RY5-IDDC      TO UT08-IDDC                                    
053100       MOVE RY5-KDORDKL   TO UT08-KDORDKL    UT03-KDORDKL                 
053200       MOVE RY5S-IDDISTR  TO UT08-IDDISTR    UT03-IDDISTR                 
053300       MOVE RY5S-IDKUNDNR TO UT08-IDKUNDNR                                
053400       MOVE RY5S-KDPRODSL TO UT08-KDPRODSL                                
053500       MOVE +1            TO UT08-REINKORD   UT03-REINKORD                
053600       MOVE ZERO          TO UT08-REFYSAVV   UT03-REFYSAVV                
053700       IF RY5-KVAVBART - RY5-KVANNANT >=                                  
053800          RY5-KVBEART  - RY5S-KVSLATT                                     
053900***       RAD LIGGER INOM SLATT ÄVEN EFTER DELANNULLERING                 
054000         MOVE +1          TO UT08-REAVBRAD                                
054100         MOVE ZERO        TO UT03-RERORAD                                 
054200       ELSE                                                               
054300         COMPUTE UT08-REAVBRAD = (RY5-KVBEART - RY5-KVANNANT   -          
054400                                 (RY5-KVBEART - RY5-KVAVBART)) /          
054500                                 (RY5-KVBEART - RY5-KVANNANT)             
054600         ON SIZE ERROR                                                    
054700           MOVE ZERO      TO UT08-REAVBRAD                                
054800         END-COMPUTE                                                      
054900         COMPUTE UT03-RERORAD  =                                          
055000                                 (RY5-KVBEART - RY5-KVAVBART)  /          
055100                                 (RY5-KVBEART - RY5-KVANNANT)             
055200         ON SIZE ERROR                                                    
055300           MOVE ZERO      TO UT03-RERORAD                                 
055400         END-COMPUTE                                                      
055500       END-IF                                                             
055600       MOVE UT03-RERORAD  TO UT08-RERORAD                                 
055700       MOVE RY5-KVBEART   TO UT08-KVBEART                                 
055800                                                                          
055900       PERFORM S11-SKRIV-W22508                                           
056000                                                                          
056010       PERFORM S13-SKRIV-W2250201                                         
056020                                                                          
056100       MOVE '232'          TO UT03-IDPTYP                                 
056200       MOVE RY5-IDDC (1:1) TO UT03-KDCLAGER                               
056300       MOVE UT08-REAVBRAD  TO UT03-REAVBRAD                               
056400                                                                          
056500       MOVE RY5-IDDC       TO WS-IDDC                                     
056600       IF GOOD-DC                                                         
056700         PERFORM S12-SKRIV-W22503                                         
056800       END-IF                                                             
056900     END-IF                                                               
057000     .                                                                    
057100     EJECT                                                                
057200 C-BEHANDLA-VOR-OE-DATA SECTION.                                          
057300                                                                          
057400     PERFORM S02-LAES-W4140I                                              
057500     PERFORM UNTIL END-OF-W4140I                                          
057600                                                                          
057700       PERFORM CA-EV-SKAPA-SRSINFO                                        
057800       PERFORM S02-LAES-W4140I                                            
057900                                                                          
058000     END-PERFORM                                                          
058100     .                                                                    
058200     EJECT                                                                
058300 CA-EV-SKAPA-SRSINFO SECTION.                                             
058400                                                                          
058500     MOVE 40I-IDDISTR     TO  TEST-IDDISTR                                
058600     MOVE 40I-IDDC        TO  WS-IDDC                                     
058700                                                                          
058800     IF 40I-KDORDING        = 3             OR                            
058900        40I-FLDIRLEV        = JA            OR                            
059000        GOOD-DDC                            OR                            
059100        40I-FLORDSPE        = JA            OR                            
059200        40I-FLOVRLEV        = JA            OR                            
059300        DIST19-SATS                         OR                            
059400        DIST20-EMBALLAGE                    OR                            
059500        DIST93-BYTESRADIO-C2                OR                            
059600        DIST18-SKROT                                                      
059700       CONTINUE                                                           
059800     ELSE                                                                 
059900       MOVE SPACE         TO UT08-AREA      UT03-AREA                     
060000                                                                          
060100       MOVE 40I-IDARTNR   TO UT08-IDARTNR   UT03-IDARTNR                  
060200       MOVE 40I-IDDC      TO UT08-IDDC                                    
060300       MOVE 40I-KDPRODSL  TO UT08-KDPRODSL                                
060400       MOVE 40I-IDDISTR   TO UT08-IDDISTR   UT03-IDDISTR                  
060500       MOVE 40I-IDKUNDNR  TO UT08-IDKUNDNR                                
060600       MOVE 40I-KDORDKL   TO UT08-KDORDKL   UT03-KDORDKL                  
060700       MOVE +1            TO UT08-REINKORD  UT03-REINKORD                 
060800       MOVE +0            TO UT08-REAVBRAD  UT03-REAVBRAD                 
060900                             UT08-REFYSAVV  UT03-REFYSAVV                 
061000       MOVE ZERO          TO UT08-RERORAD                                 
061100       MOVE ZERO          TO UT08-KVBEART                                 
061200                                                                          
061300       PERFORM S11-SKRIV-W22508                                           
061400                                                                          
061410       PERFORM S13-SKRIV-W2250201                                         
061420                                                                          
061500       MOVE '232'         TO UT03-IDPTYP                                  
061600       MOVE 40I-IDDC(1:1) TO UT03-KDCLAGER                                
061700       MOVE ZERO          TO UT03-RERORAD                                 
061800                                                                          
061900       MOVE 40I-IDDC      TO WS-IDDC                                      
062000       IF GOOD-DC                                                         
062100          PERFORM S12-SKRIV-W22503                                        
062200       END-IF                                                             
062300     END-IF                                                               
062400     .                                                                    
062500     EJECT                                                                
062600 D-BEHANDLA-ANN-VOR-DATA SECTION.                                         
062700                                                                          
062800     PERFORM S03-LAES-W4140D                                              
062900     PERFORM UNTIL END-OF-W4140D                                          
063000                                                                          
063100       PERFORM DA-EV-SKAPA-SRSINFO                                        
063200       PERFORM S03-LAES-W4140D                                            
063300                                                                          
063400     END-PERFORM                                                          
063500     .                                                                    
063600     EJECT                                                                
063700 DA-EV-SKAPA-SRSINFO SECTION.                                             
063800                                                                          
063900     MOVE ANNVOR-IDDISTR TO TEST-IDDISTR                                  
064000     MOVE ANNVOR-IDDC    TO WS-IDDC                                       
064100                                                                          
064200     IF ANNVOR-FLDIRLEV     = JA            OR                            
064300        GOOD-DDC                            OR                            
064400        DIST19-SATS                         OR                            
064500        DIST20-EMBALLAGE                    OR                            
064600        DIST93-BYTESRADIO-C2                OR                            
064700        DIST18-SKROT                        OR                            
064800        ANNVOR-KVANNANT     = ZERO                                        
064900       CONTINUE                                                           
065000     ELSE                                                                 
065100       PERFORM DAA-BEH-PT-ANNVOR                                          
065200     END-IF                                                               
065300     .                                                                    
065400     EJECT                                                                
065500 DAA-BEH-PT-ANNVOR SECTION.                                               
065600                                                                          
065700     MOVE SPACE           TO UT08-AREA       UT03-AREA                    
065800                                                                          
065900     MOVE ANNVOR-IDARTNR  TO UT08-IDARTNR    UT03-IDARTNR                 
066000     MOVE ANNVOR-IDDC     TO UT08-IDDC                                    
066100     MOVE +0              TO UT08-KDORDKL    UT03-KDORDKL                 
066200     MOVE ANNVOR-IDDISTR  TO UT08-IDDISTR    UT03-IDDISTR                 
066300     MOVE ANNVOR-IDKUNDNR TO UT08-IDKUNDNR                                
066400     MOVE ANNVOR-KDPRODSL TO UT08-KDPRODSL                                
066500     MOVE -1              TO UT08-REINKORD   UT03-REINKORD                
066600     MOVE ZERO            TO UT08-REFYSAVV   UT03-REFYSAVV                
066700                                                                          
066800     COMPUTE UT08-REAVBRAD = (ANNVOR-KVAVBART /                           
066900                              ANNVOR-KVBEART-Q) * -1                      
067000     IF (ANNVOR-KVBEART-Q - ANNVOR-KVANNANT) > ZERO                       
067100        COMPUTE UT03-RERORAD =                                            
067200                (ANNVOR-KVBEART-Q  - ANNVOR-KVAVBART) /                   
067300                (ANNVOR-KVBEART-Q  - ANNVOR-KVANNANT)                     
067400                ON SIZE ERROR MOVE ZERO TO UT03-RERORAD                   
067500        END-COMPUTE                                                       
067600     ELSE                                                                 
067700        MOVE ZERO            TO UT03-RERORAD                              
067800     END-IF                                                               
067900     MOVE UT03-RERORAD       TO UT08-RERORAD                              
068000     MOVE ANNVOR-KVBEART-Q   TO UT08-KVBEART                              
068100                                                                          
068200     PERFORM S11-SKRIV-W22508                                             
068300                                                                          
068400     MOVE '232'              TO UT03-IDPTYP                               
068500     MOVE ANNVOR-IDDC(1:1)   TO UT03-KDCLAGER                             
068600     MOVE UT08-REAVBRAD      TO UT03-REAVBRAD                             
068700                                                                          
068800     MOVE ANNVOR-IDDC        TO WS-IDDC                                   
068900     IF GOOD-DC                                                           
069000        PERFORM S12-SKRIV-W22503                                          
069100     END-IF                                                               
069200                                                                          
069300     MOVE    SPACE        TO UT08-AREA        UT03-AREA                   
069400                                                                          
069500     IF ANNVOR-KVANNANT = ANNVOR-KVBEART-Q                                
069600       CONTINUE                                                           
069700***     ANNULLATION AV HEL RAD                                            
069800     ELSE                                                                 
069900***     ENDAST TRANS VID DELANNULLATION                                   
070000                                                                          
070100       MOVE ANNVOR-IDARTNR  TO UT08-IDARTNR   UT03-IDARTNR                
070200       MOVE ANNVOR-IDDC     TO UT08-IDDC                                  
070300       MOVE +0              TO UT08-KDORDKL   UT03-KDORDKL                
070400       MOVE ANNVOR-IDDISTR  TO UT08-IDDISTR   UT03-IDDISTR                
070500       MOVE ANNVOR-IDKUNDNR TO UT08-IDKUNDNR                              
070600       MOVE ANNVOR-KDPRODSL TO UT08-KDPRODSL                              
070700       MOVE +1              TO UT08-REINKORD  UT03-REINKORD               
070800       MOVE ZERO            TO UT08-REFYSAVV  UT03-REFYSAVV               
070900                                                                          
071000       COMPUTE UT08-REAVBRAD =  ANNVOR-KVAVBART  /                        
071100                               (ANNVOR-KVBEART-Q -                        
071200                                ANNVOR-KVANNANT)                          
071300         ON SIZE ERROR                                                    
071400           MOVE ZERO        TO UT08-REAVBRAD                              
071500       END-COMPUTE                                                        
071600       IF (ANNVOR-KVBEART-Q - ANNVOR-KVANNANT) > ZERO                     
071700          COMPUTE UT03-RERORAD =                                          
071800                  (ANNVOR-KVBEART-Q  - ANNVOR-KVAVBART) /                 
071900                  (ANNVOR-KVBEART-Q  - ANNVOR-KVANNANT)                   
072000                  ON SIZE ERROR MOVE ZERO TO UT03-RERORAD                 
072100          END-COMPUTE                                                     
072200       ELSE                                                               
072300          MOVE ZERO          TO UT03-RERORAD                              
072400       END-IF                                                             
072500       MOVE UT03-RERORAD     TO UT08-RERORAD                              
072600       MOVE ANNVOR-KVBEART-Q TO UT08-KVBEART                              
072700                                                                          
072800       PERFORM S11-SKRIV-W22508                                           
072900                                                                          
072910       PERFORM S13-SKRIV-W2250201                                         
072920                                                                          
073000       MOVE '232'              TO UT03-IDPTYP                             
073100       MOVE ANNVOR-IDDC(1:1)   TO UT03-KDCLAGER                           
073200       MOVE UT08-REAVBRAD      TO UT03-REAVBRAD                           
073300                                                                          
073400       MOVE ANNVOR-IDDC        TO WS-IDDC                                 
073500       IF GOOD-DC                                                         
073600          PERFORM S12-SKRIV-W22503                                        
073700       END-IF                                                             
073800     END-IF                                                               
073900     .                                                                    
074000     EJECT                                                                
074100 Z-FINIT SECTION.                                                         
074200                                                                          
074300     CLOSE W092S1                                                         
074400           W22508                                                         
074500           W22503                                                         
074600           W22502                                                         
074700           W020TB                                                         
074710           W2250201                                                       
074800                                                                          
074900     MOVE 'S' TO POSTSUM-OPKOD                                            
075000     CALL POSTSUM USING POSTSUM-PARM                                      
075100     .                                                                    
075200     EJECT                                                                
075300 S01-LAES-W092S1 SECTION.                                                 
075400                                                                          
075500     READ W092S1 INTO INS1-AREA                                           
075600     AT END                                                               
075700        SET END-OF-W092S1 TO TRUE                                         
075800                                                                          
075900     NOT AT END                                                           
076000        MOVE 'W092S1'     TO    POSTSUM-FDNAMN                            
076100        MOVE 'W22502D1'   TO    POSTSUM-DDNAMN2                           
076200        MOVE  INS1-IDPTYP TO    POSTSUM-TRANSTYP                          
076300        CALL  POSTSUM     USING POSTSUM-PARM                              
076400     END-READ                                                             
076500     .                                                                    
076600     EJECT                                                                
076700 S02-LAES-W4140I SECTION.                                                 
076800                                                                          
076900     READ W4140I INTO 40I-W4140I-AREA                                     
077000     AT END                                                               
077100        SET END-OF-W4140I TO TRUE                                         
077200                                                                          
077300     NOT AT END                                                           
077400        MOVE 'W4140I'     TO    POSTSUM-FDNAMN                            
077500        MOVE 'W22502D2'   TO    POSTSUM-DDNAMN2                           
077600        MOVE 'VOR O/E'    TO    POSTSUM-TRANSTYP                          
077700        CALL  POSTSUM     USING POSTSUM-PARM                              
077800     END-READ                                                             
077900     .                                                                    
078000     EJECT                                                                
078100 S03-LAES-W4140D  SECTION.                                                
078200                                                                          
078300     READ W4140D INTO W4140D-AREA                                         
078400     AT END                                                               
078500        SET END-OF-W4140D TO TRUE                                         
078600                                                                          
078700     NOT AT END                                                           
078800        MOVE 'W4140D'     TO    POSTSUM-FDNAMN                            
078900        MOVE 'W22502D3'   TO    POSTSUM-DDNAMN2                           
079000        MOVE 'VOR ANN'    TO    POSTSUM-TRANSTYP                          
079100        CALL  POSTSUM     USING POSTSUM-PARM                              
079200     END-READ                                                             
079300     .                                                                    
079400     EJECT                                                                
079500 S04-LAES-W020TB SECTION.                                                 
079600                                                                          
079700     READ W020TB INTO INTB-AREA                                           
079800     AT END                                                               
079900        SET END-OF-W020TB TO TRUE                                         
080000                                                                          
080100     NOT AT END                                                           
080200        MOVE 'W020TB'     TO    POSTSUM-FDNAMN                            
080300        MOVE 'CONSTANT'   TO    POSTSUM-DDNAMN2                           
080400        MOVE 'TAB'        TO    POSTSUM-TRANSTYP                          
080500        CALL  POSTSUM     USING POSTSUM-PARM                              
080600     END-READ                                                             
080700     .                                                                    
080800     EJECT                                                                
080900 S11-SKRIV-W22508 SECTION.                                                
081000                                                                          
081100     MOVE +1              TO IX-TAB                                       
081200     MOVE ZERO            TO UT08-KDMARK-SERV                             
081300     PERFORM UNTIL IX-TAB > IX-TAB-MAX  OR                                
081400        UT08-KDMARK-SERV > ZERO                                           
081500        IF UT08-IDDISTR >= TAB-IDDISTR-START (IX-TAB) AND                 
081600           UT08-IDDISTR <= TAB-IDDISTR-SLUT  (IX-TAB)                     
081700           MOVE TAB-KDMARK-SERV (IX-TAB) TO UT08-KDMARK-SERV              
081800        END-IF                                                            
081900        ADD +1            TO IX-TAB                                       
082000     END-PERFORM                                                          
082100                                                                          
082200                                                                          
082300     WRITE W22508-POST    FROM  UT08-AREA                                 
082400                                                                          
082500     MOVE  'UT08'         TO    POSTSUM-TRANSTYP                          
082600     MOVE  'W22508'       TO    POSTSUM-FDNAMN                            
082700     MOVE  'W22502D4'     TO    POSTSUM-DDNAMN2                           
082800     CALL   POSTSUM       USING POSTSUM-PARM                              
082900                                                                          
083000*    SKRIV EN EGEN FIL TILL TORSTEN                                       
083100        WRITE W22502-POST    FROM  UT08-AREA                              
083200                                                                          
083300        MOVE  'UT08'         TO    POSTSUM-TRANSTYP                       
083400        MOVE  'W22502'       TO    POSTSUM-FDNAMN                         
083500        MOVE  'W22502D6'     TO    POSTSUM-DDNAMN2                        
083600        CALL   POSTSUM       USING POSTSUM-PARM                           
083700     .                                                                    
083800     EJECT                                                                
083900 S12-SKRIV-W22503 SECTION.                                                
084000                                                                          
084100     MOVE UT03-IDDISTR TO WS-DISTRIKT-KOLL                                
084200     MOVE UT03-KDORDKL TO WS-KDORDKL-KOLL                                 
084300                                                                          
084400     IF DISTR-UNDANTAG AND KDORDKL-0-1-2                                  
084500        CONTINUE                                                          
084600     ELSE                                                                 
084700        WRITE W22503-POST    FROM  UT03-AREA                              
084800                                                                          
084900        MOVE  'UT03'         TO    POSTSUM-TRANSTYP                       
085000        MOVE  'W22503'       TO    POSTSUM-FDNAMN                         
085100        MOVE  'W22502D5'     TO    POSTSUM-DDNAMN2                        
085200        CALL   POSTSUM       USING POSTSUM-PARM                           
085300     END-IF                                                               
085400     .                                                                    
085500 S13-SKRIV-W2250201 SECTION.                                              
085600                                                                          
085640     MOVE UT08-IDDC         TO WS-IDDC                                    
085700     IF CDC-SE                                                            
085701        MOVE UT08-REINKORD  TO UT09-REINKORD                              
085702        MOVE UT08-REAVBRAD  TO UT09-REAVBRAD                              
085706                                                                          
085710        MOVE UT08-IDARTNR   TO W-IDARTNR                                  
085800        PERFORM IMS-GU-WDK611                                             
085810        IF SEGMENT-FOUND                                                  
085820           MOVE CLAG-IDANSK TO W-IDANSK                                   
085822           MOVE 0           TO W-IDANSK(3:1)                              
085823           MOVE W-IDANSK    TO UT09-IDANSK                                
085830        ELSE                                                              
085840           MOVE +0          TO UT09-IDANSK                                
085850        END-IF                                                            
086200                                                                          
086900        WRITE W2250201-POST FROM UT09-AREA                                
087000                                                                          
087100        MOVE  'UT09'        TO POSTSUM-TRANSTYP                           
087200        MOVE  'W22404'      TO POSTSUM-FDNAMN                             
087300        MOVE  'W22502D8'    TO POSTSUM-DDNAMN2                            
087400        CALL   POSTSUM   USING POSTSUM-PARM                               
087500     END-IF                                                               
088300     .                                                                    
088400     EJECT                                                                
088492 IMS-GU-WDK611 SECTION.                                                   
088493     MOVE 'IMS-GU-WDK611   ' TO CURRENT-IMS-SECTION                       
088494                                                                          
088495     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
088496          DELIMITED BY SIZE INTO SSA1                                     
088497     MOVE 'WDK611  (KDSEGKEY =1)' TO SSA2                                 
088498     MOVE '  GE'                  TO GOOD-STATUSCODES                     
088499     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
088500     MOVE WDK6-STATUS-CODE        TO STATUS-WS                            
088501     PERFORM IMS-STATUSCHECK                                              
088510     .                                                                    
088600     EJECT                                                                
090500 IMS-STATUSCHECK SECTION.                                                 
090600     SKIP2                                                                
090700     SET STATUS-IX TO 1                                                   
090800     SEARCH GOOD-STATUS                                                   
090900       AT END                                                             
091000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
091100           DELIMITED BY SIZE INTO ERROR-TEXT                              
091200         DISPLAY ERROR-TEXT                                               
091300         CALL FELLOG                                                      
091400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
091500         CONTINUE                                                         
091600     END-SEARCH                                                           
091700     .                                                                    
