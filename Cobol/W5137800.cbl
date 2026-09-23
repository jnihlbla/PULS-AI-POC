000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W5137800.                                                
000400*AUTHOR.         KARL JOHAN HANSSON.                                      
000500*DATE-WRITTEN.   96/03/07.                                                
000600*DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PROGRAMMET LÄSER FIL MED ARTIKLAR KVAR ATT INVENTERA             
001000*        OCH MATCHAR MOT PARAMETERPOST FRÅN BILD 5305.                    
001100*        FÖR VARJE ARTIKEL SKAPAS ETT 02-SEGMENT PÅ WDH1.                 
001200*        OM 01-SEGMENT SAKNAS FÖR ARTIKELN LÄGGS DETTA UPP FÖRST.         
001300*        KVITTENS AV KÖRNINGEN GÖRS PÅ LISTA TILL VISSA END-USER.         
001400*        1 LISTA TILL END-USER (DC 21 23 24 25 26 91-ET)                  
001500*        2 DISPLAY I SYSOUT                                               
001600*        ENDAST LISTOR PÅ DC 21, 23, 24, 25, 26 OCH 91-ET                 
001700*                                                                         
001800*        PROGRAMMET UPPDATERAR WLINVA (WDH1)                              
001900*                                                                         
002000*    ABENDKODER:                                                          
002100*        U0016 -  . . . .                                                 
002200*        U1000 -  . . . .                                                 
002300*                                                                         
002400     EJECT                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000*          --- SYSIN FRÅN JCL                                             
003100      SELECT INDATA          ASSIGN TO SYSIN.                             
003200                                                                          
003300*          --- ARTIKLAR KVAR ATT INVENTERA                                
003400     SELECT W51372           ASSIGN TO W51378D1.                          
003500                                                                          
003600*          --- KVITTENSLISTA SDC-21                                       
003700     SELECT W51378-021       ASSIGN TO W51378D3.                          
003800                                                                          
003900*          --- KVITTENSLISTA SDC-23                                       
004000     SELECT W51378-023       ASSIGN TO W51378D4.                          
004100                                                                          
004200*          --- KVITTENSLISTA SDC-24                                       
004300     SELECT W51378-024       ASSIGN TO W51378D5.                          
004400                                                                          
004500*          --- KVITTENSLISTA SDC-25                                       
004600     SELECT W51378-025       ASSIGN TO W51378D6.                          
004700                                                                          
004800*          --- KVITTENSLISTA SDC-26                                       
004900     SELECT W51378-026       ASSIGN TO W51378D7.                          
005000                                                                          
005100*    EXCHANGE TERMINAL       DC-91                                        
005200     SELECT W51378-091       ASSIGN TO W51378D8.                          
005300     EJECT                                                                
005400 DATA DIVISION.                                                           
005500 FILE SECTION.                                                            
005600     SKIP3                                                                
005700 FD INDATA                                                                
005800     LABEL RECORD STANDARD                                                
005900     RECORDING  F                                                         
006000     BLOCK CONTAINS 0.                                                    
006100 01  INPOST                  PIC X(80).                                   
006200                                                                          
006300 FD  W51372                                                               
006400     RECORDING       F                                                    
006500     BLOCK CONTAINS  0.                                                   
006600*01  -COPY W51371 -L.                                                     
006700                                                                          
006800 FD  W51378-021                                                           
006900     LABEL RECORD   STANDARD                                              
007000     RECORDING      F                                                     
007100     BLOCK CONTAINS 0.                                                    
007200 01  W51378-021-L       PIC X(121).                                       
007300 FD  W51378-023                                                           
007400     LABEL RECORD   STANDARD                                              
007500     RECORDING      F                                                     
007600     BLOCK CONTAINS 0.                                                    
007700 01  W51378-023-L       PIC X(121).                                       
007800 FD  W51378-024                                                           
007900     LABEL RECORD   STANDARD                                              
008000     RECORDING      F                                                     
008100     BLOCK CONTAINS 0.                                                    
008200 01  W51378-024-L       PIC X(121).                                       
008300 FD  W51378-025                                                           
008400     LABEL RECORD   STANDARD                                              
008500     RECORDING      F                                                     
008600     BLOCK CONTAINS 0.                                                    
008700 01  W51378-025-L       PIC X(121).                                       
008800                                                                          
008900 FD  W51378-026                                                           
009000     LABEL RECORD   STANDARD                                              
009100     RECORDING      F                                                     
009200     BLOCK CONTAINS 0.                                                    
009300 01  W51378-026-L       PIC X(121).                                       
009400 FD  W51378-091                                                           
009500     LABEL RECORD   STANDARD                                              
009600     RECORDING      F                                                     
009700     BLOCK CONTAINS 0.                                                    
009800 01  W51378-091-L       PIC X(121).                                       
009900     EJECT                                                                
010000 WORKING-STORAGE SECTION.                                                 
010100                                                                          
010200 77  IDPGM                       PIC X(8)    VALUE 'W5137800'.            
010300 77  JA                          PIC X       VALUE 'J'.                   
010400 77  NEJ                         PIC X       VALUE 'N'.                   
010500 77  INDATA-EOF                  PIC X       VALUE 'N'.                   
010600 77  W51372-EOF                  PIC X       VALUE 'N'.                   
010700 77  POST-SKALL-MED-SW           PIC X       VALUE 'N'.                   
010800 77  W-IN-DC                     PIC X(2)    VALUE '  '.                  
010900 77  W-PRM-DC                    PIC X(2)    VALUE '  '.                  
010910 01  WS-KDVVKL                   PIC 9(1).                                
011000     EJECT                                                                
011100 01  FILLER                      PIC X(10)   VALUE 'INAREA'.              
011200 01  INAREA.                                                              
011300   03    PRM-IDDC.                                                        
011400     05  PRM-IDDC-1            PIC X(2).                                  
011500     05  PRM-IDDC-2            PIC 9(2).                                  
011600   03    FILLER                PIC X.                                     
011700   03    PRM-KVINVBEG          PIC 9(3).                                  
011800   03    FILLER                PIC X.                                     
011900   03    PRM-KDVVKL            PIC 9(1).                                  
012000   03    FILLER                PIC X.                                     
012100   03    PRM-ADLAGOMR          PIC 9(2).                                  
012200   03    FILLER                PIC X.                                     
012300   03    PRM-ADGANG            PIC 9(2).                                  
012400   03    FILLER                PIC X.                                     
012500   03    PRM-ADPLATS           PIC 9(5).                                  
012600   03    FILLER                PIC X.                                     
012700   03    PRM-KDPRODSL          PIC 9(2).                                  
012800   03    FILLER                PIC X.                                     
012900   03    PRM-IDFKNGRP          PIC 9(4).                                  
013000   03    FILLER                PIC X(50).                                 
013500     EJECT                                                                
013600                                                                          
013700 01  R1-HEADER.                                                           
013800      03 FILLER              PIC X(4)       VALUE SPACE.                  
013900      03 FILLER              PIC X(18)      VALUE 'VCCS       '.          
014000      03 R1-RAPPID           PIC X(11).                                   
014100      03 FILLER              PIC X(30)      VALUE                         
014200                             'CONFIRMATION OF PIC 5305      '.            
014300      03 FILLER              PIC X(25)      VALUE SPACE.                  
014400      03 FILLER              PIC X(5)       VALUE 'DATE '.                
014500      03 R1-DATUM            PIC 9(6).                                    
014600      03 FILLER              PIC X(8)       VALUE '   TIME '.             
014700      03 R1-KLOCKA           PIC 9(8).                                    
014800      03 FILLER              PIC X(6)       VALUE SPACE.                  
014900                                                                          
015000 01  L1-DETRAD.                                                           
015100      03 FILLER              PIC X(4)   VALUE SPACE.                      
015200      03 L1-TEXT             PIC X(38).                                   
015300      03 FILLER              PIC X(2)   VALUE SPACE.                      
015400      03 L1-VAERDE           PIC 9(5).                                    
015500      03 FILLER              PIC X(70)  VALUE SPACE.                      
015600                                                                          
015700 01  L2-DETRAD.                                                           
015800      03 FILLER              PIC X(6)   VALUE '    DC'.                   
015900      03 FILLER              PIC X(34)  VALUE SPACE.                      
016000      03 FILLER              PIC X(7)   VALUE '=      '.                  
016100      03 L2-DC               PIC X(2).                                    
016200      03 FILLER              PIC X(70)  VALUE SPACE.                      
016300                                                                          
016400     EJECT                                                                
016500 01  ANT-INS.                                                             
016600     03  ANT-INS-H101            PIC  9(5)   VALUE ZERO.                  
016700     03  ANT-INS-H111            PIC  9(5)   VALUE ZERO.                  
016800     03  ANT-INS-H121            PIC  9(5)   VALUE ZERO.                  
016900                                                                          
017000 01  FELTEXT.                                                             
017100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
017200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
017300                                                                          
017400 01  WS-INV-DAREGDAT-AREA.                                                
017500     03  WS-INV-DAREGDAT     PIC 9(9) VALUE ZERO.                         
017600     03  FILLER REDEFINES WS-INV-DAREGDAT.                                
017700       05  WS-INV-NOLL         PIC 9(1).                                  
017800       05  WS-INV-SEKEL        PIC 9(2).                                  
017900       05  WS-INV-AAMMDD       PIC 9(6).                                  
018000                                                                          
018100 01  WS-TISEGKEYAREA.                                                     
018200     03  WS-TIAAAAMMDDL      PIC 9(9) VALUE ZERO.                         
018300     03  FILLER REDEFINES WS-TIAAAAMMDDL.                                 
018400         05  WS-AAR          PIC 9(2).                                    
018500         05  WS-TIAAMMDD     PIC 9(6).                                    
018600         05  WS-LOPNR        PIC 9(1).                                    
018700     03  WS-TISEGKEY         PIC S9(9)  VALUE ZERO COMP-3.                
018800                                                                          
018900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
019000 01  DAGENS-KLOCKA               PIC 9(8)    VALUE ZERO.                  
019100                                                                          
019200 01  CHKP-VAR.                                                            
019300   03  CHKP-MSG-IO-AREA-LENGTH   PIC S9(9)   VALUE +32 COMP SYNC.         
019400   03  CHKP-MSG-IO-AREA          PIC X(32)   VALUE SPACE.                 
019500   03  CHKP-AREA-LENGTH          PIC S9(9)   VALUE +32 COMP SYNC.         
019600   03  CHKP-AREA                 PIC X(32)   VALUE SPACE.                 
019700   03  CHKP-ANT                  PIC S9(3)   VALUE +0.                    
019800   03  CHKP-MAX                  PIC S9(3)   VALUE +50.                   
019900                                                                          
020000 01  DYNAMISKA-SUBPROGRAM.                                                
020100*                                                                         
020200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
020300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
020400     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
020500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
020600                                                                          
020700     EJECT                                                                
020800*    --- VALID IDDC CODES                                                 
020900*                                                                         
021000*01  -COPY WWDC99                                                         
021100*    --- PARAMETRAR TILL POSTSUM                                          
021200*                                                                         
021300*01  -COPY W0005   -PRE  POSTSUM-                                         
021400     EJECT                                                                
021500 01  FILLER                      PIC X(24)   VALUE 'IN-AREA'.             
021600                                                                          
021700*01  AREA -COPY W51371     -PRE IN-                                       
021800*                                                                         
021900     EJECT                                                                
022000 01  FILLER                    PIC X(16) VALUE 'IMS-WS'.                  
022100                                                                          
022200 01  W-WDH101.                                                            
022300     03  W-IDARTNR-X.                                                     
022400         05  W-IDARTNR         PIC S9(9) VALUE ZERO       COMP-3.         
022500     SKIP2                                                                
022600 01  W-WDH111KY-X.                                                        
022700     03  W-IDDC               PIC X(2).                                   
022800     03  W-KDINVKAT           PIC S9(3).                                  
022900     03  W-TISEGKEY           PIC S9(9).                                  
023000     03  W-DAREGDAT-SORT      PIC  9(8).                                  
023100 01  W-WDH111KY-MIN.                                                      
023200     03  IDDC-SEARCH-MIN       PIC X(2).                                  
023300     03  INVKAT-SEARCH-MIN     PIC S9(3) VALUE +01        COMP-3.         
023400     03  TISEGKEY-SEARCH-MIN   PIC S9(9) VALUE ZERO       COMP-3.         
023500     03  DAREGDAT-SORT-MIN     PIC  9(8) VALUE ZERO.                      
023600                                                                          
023700 01  W-WDH111KY-MAX.                                                      
023800     03  IDDC-SEARCH-MAX       PIC X(2).                                  
023900     03  INVKAT-SEARCH-MAX     PIC S9(3) VALUE +61        COMP-3.         
024000     03  TISEGKEY-SEARCH-MAX   PIC S9(9) VALUE +999999999 COMP-3.         
024100     03  DAREGDAT-SORT-MAX     PIC  9(8) VALUE 99999999.                  
024200                                                                          
024300*    --- STATUS-KOD FRÅN IMS                                              
024400 01  STATUS-WS                   PIC XX.                                  
024500     88  SEGMENT-FINNS                       VALUE '  '.                  
024600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
024700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
024800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
024900     88  IMS-EJ-OK                           VALUE 'XD'.                  
025000     SKIP2                                                                
025100 01  GODK-STATUSKODER.                                                    
025200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
025300     SKIP3                                                                
025400 01  SSA1                        PIC X(128).                              
025500 01  SSA2                        PIC X(128).                              
025600 01  SSA3                        PIC X(128).                              
025700     EJECT                                                                
025800*    --- IMS FUNKTIONSKODER                                               
025900*01  -COPY W0003                                                          
026000     EJECT                                                                
026100*    ---  DLI INPUT-OUTPUT AREA                                           
026200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
026300     SKIP3                                                                
026400 01  DLI-IO-AREA-H101.                                                    
026500*    03  -COPY WDH101   -PRE INVA-                                        
026600     EJECT                                                                
026700 01  DLI-IO-AREA-H111.                                                    
026800*    03  -COPY WDH111   -PRE INVA-                                        
026900     EJECT                                                                
027000 01  DLI-IO-AREA-H121.                                                    
027100*    03  -COPY WDH121   -PRE INVA-                                        
027200     EJECT                                                                
027300 LINKAGE SECTION.                                                         
027400                                                                          
027500*01  -COPY W0009  -PRE MSG-                                               
027600                                                                          
027700*01  -COPY W0008  -PRE INVA-                                              
027800     05  FILLER                  PIC X.                                   
027900     EJECT                                                                
028000 PROCEDURE DIVISION  USING MSG-PCB INVA-PCB.                              
028100 MAIN SECTION.                                                            
028200     ENTRY 'DLITCBL' USING MSG-PCB INVA-PCB.                              
028300                                                                          
028400     PERFORM A-INIT                                                       
028500                                                                          
028600     PERFORM S01-LAES-W51372                                              
028700     PERFORM UNTIL W51372-EOF  = JA   OR                                  
028800                   PRM-KVINVBEG = ZERO                                    
028900       IF PRM-IDDC-2 = IN-IDDC                                            
029000         IF CHKP-ANT > CHKP-MAX                                           
029100           PERFORM X-TAG-CHECKPOINT                                       
029200         END-IF                                                           
029300         PERFORM B-SKAPA-INVKOEART-PAA-WDH1                               
029400         IF SEGMENT-FINNS AND POST-SKALL-MED-SW = JA                      
029500           SUBTRACT 1 FROM PRM-KVINVBEG                                   
029600         END-IF                                                           
029700       END-IF                                                             
029800       PERFORM S01-LAES-W51372                                            
029900     END-PERFORM                                                          
030000                                                                          
030100     PERFORM Z-FINIT                                                      
030200                                                                          
030300     MOVE ZERO TO RETURN-CODE                                             
030400     GOBACK                                                               
030500     .                                                                    
030600     EJECT                                                                
030700 A-INIT SECTION.                                                          
030800                                                                          
030900     PERFORM IMS-RESTART                                                  
031000                                                                          
031100     OPEN INPUT INDATA                                                    
031200     READ INDATA NEXT RECORD INTO INAREA                                  
031300       AT END MOVE JA               TO INDATA-EOF                         
031400     END-READ                                                             
031500                                                                          
031600     UNSTRING INAREA DELIMITED BY SPACE INTO PRM-IDDC                     
031700                                             PRM-KVINVBEG                 
031800                                             PRM-KDVVKL                   
031900                                             PRM-ADLAGOMR                 
032000                                             PRM-ADGANG                   
032100                                             PRM-ADPLATS                  
032200                                             PRM-KDPRODSL                 
032300                                             PRM-IDFKNGRP                 
032400                                                                          
032900     CLOSE INDATA                                                         
033000                                                                          
033100     DISPLAY 'PRM-IDDC******=' PRM-IDDC-2                                 
033200     DISPLAY 'PRM-KVINVBEG**=' PRM-KVINVBEG                               
033300     DISPLAY 'PRM-KDVVKL****=' PRM-KDVVKL                                 
033400     DISPLAY 'PRM-ADLAGOMR**=' PRM-ADLAGOMR                               
033500     DISPLAY 'PRM-ADGANG****=' PRM-ADGANG                                 
033600     DISPLAY 'PRM-ADPLATS***=' PRM-ADPLATS                                
033700     DISPLAY 'PRM-KDPRODSL**=' PRM-KDPRODSL                               
033800     DISPLAY 'PRM-IDFKNGRP**=' PRM-IDFKNGRP                               
034000                                                                          
034400     MOVE PRM-IDDC-2       TO W-PRM-DC                                    
034700                              WS-IDDC                                     
034800                                                                          
034900     OPEN INPUT  W51372                                                   
035000                                                                          
035100     IF SDC-NL                                                            
035500        OPEN OUTPUT W51378-021                                            
035700     ELSE                                                                 
036100         IF SDC-ES                                                        
036200           OPEN OUTPUT W51378-024                                         
036300         ELSE                                                             
036400           IF SDC-IT                                                      
036500             OPEN OUTPUT W51378-025                                       
036600           ELSE                                                           
036700             IF SDC-AT                                                    
036800               OPEN OUTPUT W51378-026                                     
036900             ELSE                                                         
036901               IF SDC-NL-ET                                               
036902                  OPEN OUTPUT W51378-091                                  
036903               END-IF                                                     
036904             END-IF                                                       
037000           END-IF                                                         
037100         END-IF                                                           
037300     END-IF                                                               
037400                                                                          
037500     ACCEPT DAGENS-DATUM  FROM DATE                                       
037600     ACCEPT DAGENS-KLOCKA FROM TIME                                       
037700                                                                          
037800     MOVE DAGENS-DATUM   TO WS-TIAAMMDD                                   
037900                            WS-INV-AAMMDD                                 
038000     MOVE 20             TO WS-INV-SEKEL                                  
038100     MOVE DAGENS-DATUM   TO R1-DATUM                                      
038200     MOVE DAGENS-KLOCKA  TO R1-KLOCKA                                     
038300     PERFORM AA-SKRIV-KVITTENS                                            
038400                                                                          
038500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
038600     .                                                                    
038700     EJECT                                                                
038800 AA-SKRIV-KVITTENS SECTION.                                               
038900                                                                          
039000     EVALUATE TRUE                                                        
039100     WHEN SDC-NL                                                          
041900       MOVE 'W51378-021 '                          TO R1-RAPPID           
042000       WRITE W51378-021-L FROM R1-HEADER AFTER ADVANCING PAGE             
042100       MOVE PRM-IDDC-2                               TO L2-DC             
042200       WRITE W51378-021-L FROM L2-DETRAD AFTER ADVANCING 2 LINES          
042300       MOVE 'NO. OF STOCKTAKING                  = ' TO L1-TEXT           
042400       MOVE PRM-KVINVBEG                             TO L1-VAERDE         
042500       WRITE W51378-021-L FROM L1-DETRAD AFTER ADVANCING 1 LINES          
042600       MOVE 'VOLUME VALUE CLASS                  = ' TO L1-TEXT           
042700       MOVE PRM-KDVVKL                               TO L1-VAERDE         
042800       WRITE W51378-021-L FROM L1-DETRAD AFTER ADVANCING 1 LINES          
042900       MOVE 'AREA                                = ' TO L1-TEXT           
043000       MOVE PRM-ADLAGOMR                           TO L1-VAERDE           
043100       WRITE W51378-021-L FROM L1-DETRAD AFTER ADVANCING 1 LINES          
043200       MOVE 'AISLE                               = ' TO L1-TEXT           
043300       MOVE PRM-ADGANG                               TO L1-VAERDE         
043400       WRITE W51378-021-L FROM L1-DETRAD AFTER ADVANCING 1 LINES          
043500       MOVE 'LOCATION                            = ' TO L1-TEXT           
043600       MOVE PRM-ADPLATS                              TO L1-VAERDE         
043700       WRITE W51378-021-L FROM L1-DETRAD AFTER ADVANCING 1 LINES          
043800       MOVE 'PRODUCT GROUP PART                  = ' TO L1-TEXT           
043900       MOVE PRM-KDPRODSL                             TO L1-VAERDE         
044000       WRITE W51378-021-L FROM L1-DETRAD AFTER ADVANCING 1 LINES          
044100       MOVE 'FUNCTION GROUP                      = ' TO L1-TEXT           
044200       MOVE PRM-IDFKNGRP                             TO L1-VAERDE         
044300       WRITE W51378-021-L FROM L1-DETRAD AFTER ADVANCING 1 LINES          
047100                                                                          
047200     WHEN SDC-ES                                                          
047300       MOVE 'W51378-024 '                          TO R1-RAPPID           
047400       WRITE W51378-024-L FROM R1-HEADER AFTER ADVANCING PAGE             
047500       MOVE PRM-IDDC-2                               TO L2-DC             
047600       WRITE W51378-024-L FROM L2-DETRAD AFTER ADVANCING 2 LINES          
047700       MOVE 'NO. OF STOCKTAKING                  = ' TO L1-TEXT           
047800       MOVE PRM-KVINVBEG                             TO L1-VAERDE         
047900       WRITE W51378-024-L FROM L1-DETRAD AFTER ADVANCING 1 LINES          
048000       MOVE 'VOLUME VALUE CLASS                  = ' TO L1-TEXT           
048100       MOVE PRM-KDVVKL                               TO L1-VAERDE         
048200       WRITE W51378-024-L FROM L1-DETRAD AFTER ADVANCING 1 LINES          
048300       MOVE 'AREA                                = ' TO L1-TEXT           
048400       MOVE PRM-ADLAGOMR                           TO L1-VAERDE           
048500       WRITE W51378-024-L FROM L1-DETRAD AFTER ADVANCING 1 LINES          
048600       MOVE 'AISLE                               = ' TO L1-TEXT           
048700       MOVE PRM-ADGANG                               TO L1-VAERDE         
048800       WRITE W51378-024-L FROM L1-DETRAD AFTER ADVANCING 1 LINES          
048900       MOVE 'LOCATION                            = ' TO L1-TEXT           
049000       MOVE PRM-ADPLATS                              TO L1-VAERDE         
049100       WRITE W51378-024-L FROM L1-DETRAD AFTER ADVANCING 1 LINES          
049200       MOVE 'PRODUCT GROUP PART                  = ' TO L1-TEXT           
049300       MOVE PRM-KDPRODSL                             TO L1-VAERDE         
049400       WRITE W51378-024-L FROM L1-DETRAD AFTER ADVANCING 1 LINES          
049500       MOVE 'FUNCTION GROUP                      = ' TO L1-TEXT           
049600       MOVE PRM-IDFKNGRP                             TO L1-VAERDE         
049700       WRITE W51378-024-L FROM L1-DETRAD AFTER ADVANCING 1 LINES          
049800     WHEN SDC-IT                                                          
049900       MOVE 'W51378-025 '                          TO R1-RAPPID           
050000       WRITE W51378-025-L FROM R1-HEADER AFTER ADVANCING PAGE             
050100       MOVE PRM-IDDC-2                               TO L2-DC             
050200       WRITE W51378-025-L FROM L2-DETRAD AFTER ADVANCING 2 LINES          
050300       MOVE 'NO. OF STOCKTAKING                  = ' TO L1-TEXT           
050400       MOVE PRM-KVINVBEG                             TO L1-VAERDE         
050500       WRITE W51378-025-L FROM L1-DETRAD AFTER ADVANCING 1 LINES          
050600       MOVE 'VOLUME VALUE CLASS                  = ' TO L1-TEXT           
050700       MOVE PRM-KDVVKL                               TO L1-VAERDE         
050800       WRITE W51378-025-L FROM L1-DETRAD AFTER ADVANCING 1 LINES          
050900       MOVE 'AREA                                = ' TO L1-TEXT           
051000       MOVE PRM-ADLAGOMR                           TO L1-VAERDE           
051100       WRITE W51378-025-L FROM L1-DETRAD AFTER ADVANCING 1 LINES          
051200       MOVE 'AISLE                               = ' TO L1-TEXT           
051300       MOVE PRM-ADGANG                               TO L1-VAERDE         
051400       WRITE W51378-025-L FROM L1-DETRAD AFTER ADVANCING 1 LINES          
051500       MOVE 'LOCATION                            = ' TO L1-TEXT           
051600       MOVE PRM-ADPLATS                              TO L1-VAERDE         
051700       WRITE W51378-025-L FROM L1-DETRAD AFTER ADVANCING 1 LINES          
051800       MOVE 'PRODUCT GROUP PART                  = ' TO L1-TEXT           
051900       MOVE PRM-KDPRODSL                             TO L1-VAERDE         
052000       WRITE W51378-025-L FROM L1-DETRAD AFTER ADVANCING 1 LINES          
052100       MOVE 'FUNCTION GROUP                      = ' TO L1-TEXT           
052200       MOVE PRM-IDFKNGRP                             TO L1-VAERDE         
052300       WRITE W51378-025-L FROM L1-DETRAD AFTER ADVANCING 1 LINES          
052400     WHEN SDC-AT                                                          
052500       MOVE 'W51378-026 '                          TO R1-RAPPID           
052600       WRITE W51378-026-L FROM R1-HEADER AFTER ADVANCING PAGE             
052700       MOVE PRM-IDDC-2                               TO L2-DC             
052800       WRITE W51378-026-L FROM L2-DETRAD AFTER ADVANCING 2 LINES          
052900       MOVE 'NO. OF STOCKTAKING                  = ' TO L1-TEXT           
053000       MOVE PRM-KVINVBEG                             TO L1-VAERDE         
053100       WRITE W51378-026-L FROM L1-DETRAD AFTER ADVANCING 1 LINES          
053200       MOVE 'VOLUME VALUE CLASS                  = ' TO L1-TEXT           
053300       MOVE PRM-KDVVKL                               TO L1-VAERDE         
053400       WRITE W51378-026-L FROM L1-DETRAD AFTER ADVANCING 1 LINES          
053500       MOVE 'AREA                                = ' TO L1-TEXT           
053600       MOVE PRM-ADLAGOMR                           TO L1-VAERDE           
053700       WRITE W51378-026-L FROM L1-DETRAD AFTER ADVANCING 1 LINES          
053800       MOVE 'AISLE                               = ' TO L1-TEXT           
053900       MOVE PRM-ADGANG                               TO L1-VAERDE         
054000       WRITE W51378-026-L FROM L1-DETRAD AFTER ADVANCING 1 LINES          
054100       MOVE 'LOCATION                            = ' TO L1-TEXT           
054200       MOVE PRM-ADPLATS                              TO L1-VAERDE         
054300       WRITE W51378-026-L FROM L1-DETRAD AFTER ADVANCING 1 LINES          
054400       MOVE 'PRODUCT GROUP PART                  = ' TO L1-TEXT           
054500       MOVE PRM-KDPRODSL                             TO L1-VAERDE         
054600       WRITE W51378-026-L FROM L1-DETRAD AFTER ADVANCING 1 LINES          
054700       MOVE 'FUNCTION GROUP                      = ' TO L1-TEXT           
054800       MOVE PRM-IDFKNGRP                             TO L1-VAERDE         
054900       WRITE W51378-026-L FROM L1-DETRAD AFTER ADVANCING 1 LINES          
054901     WHEN SDC-NL-ET                                                       
054902       MOVE 'W51378-091'                             TO R1-RAPPID         
054903       WRITE W51378-091-L FROM R1-HEADER AFTER ADVANCING PAGE             
054904       MOVE PRM-IDDC-2                                TO L2-DC            
054905       WRITE W51378-091-L FROM L2-DETRAD AFTER ADVANCING 2 LINES          
054906       MOVE 'NO. OF STOCKTAKING                  = ' TO L1-TEXT           
054907       MOVE PRM-KVINVBEG                             TO L1-VAERDE         
054908       WRITE W51378-091-L FROM L1-DETRAD AFTER ADVANCING 1 LINES          
054909       MOVE 'VOLUME VALUE CLASS                  = ' TO L1-TEXT           
054910       MOVE PRM-KDVVKL                               TO L1-VAERDE         
054911       WRITE W51378-091-L FROM L1-DETRAD AFTER ADVANCING 1 LINES          
054912       MOVE 'AREA                                = ' TO L1-TEXT           
054913       MOVE PRM-ADLAGOMR                           TO L1-VAERDE           
054914       WRITE W51378-091-L FROM L1-DETRAD AFTER ADVANCING 1 LINES          
054915       MOVE 'AISLE                               = ' TO L1-TEXT           
054916       MOVE PRM-ADGANG                               TO L1-VAERDE         
054917       WRITE W51378-091-L FROM L1-DETRAD AFTER ADVANCING 1 LINES          
054918       MOVE 'LOCATION                            = ' TO L1-TEXT           
054919       MOVE PRM-ADPLATS                              TO L1-VAERDE         
054920       WRITE W51378-091-L FROM L1-DETRAD AFTER ADVANCING 1 LINES          
054921       MOVE 'PRODUCT GROUP PART                  = ' TO L1-TEXT           
054922       MOVE PRM-KDPRODSL                             TO L1-VAERDE         
054923       WRITE W51378-091-L FROM L1-DETRAD AFTER ADVANCING 1 LINES          
054924       MOVE 'FUNCTION GROUP                      = ' TO L1-TEXT           
054925       MOVE PRM-IDFKNGRP                             TO L1-VAERDE         
054926       WRITE W51378-091-L FROM L1-DETRAD AFTER ADVANCING 1 LINES          
055000     END-EVALUATE                                                         
055100     .                                                                    
055200     EJECT                                                                
055300 B-SKAPA-INVKOEART-PAA-WDH1 SECTION.                                      
055400                                                                          
055500     MOVE NEJ             TO POST-SKALL-MED-SW                            
055600     PERFORM BA-SELEKTERA-INVENTERING                                     
055700                                                                          
055800     IF POST-SKALL-MED-SW = JA                                            
055900       MOVE IN-IDARTNR    TO W-IDARTNR                                    
056000       PERFORM IMS-GET-WDH101                                             
056100       IF SEGMENT-SAKNAS                                                  
056200         MOVE IN-IDARTNR  TO INVA-ART-IDARTNR                             
056300         PERFORM IMS-ISRT-WDH101                                          
056400         ADD +1           TO CHKP-ANT                                     
056500         ADD +1           TO ANT-INS-H101                                 
056600       END-IF                                                             
056700                                                                          
056800       MOVE IN-IDDC       TO INVA-INV-IDDC                                
056900                             W-IDDC                                       
057000       MOVE +9            TO INVA-INV-KDINVKAT                            
057100                             W-KDINVKAT                                   
057200                                                                          
057300       MOVE IN-ADLAGOMR   TO INVA-INV-ADLAGOMR                            
057400       MOVE IN-ADGANG     TO INVA-INV-ADGANG                              
057500       MOVE IN-ADPLATS    TO INVA-INV-ADPLATS                             
057600       MOVE IN-IDFKNGRP   TO INVA-INV-IDFKNGRP                            
057700       MOVE +2            TO INVA-INV-KDINVPRIO                           
057800       MOVE IN-KDPRODSL   TO INVA-INV-KDPRODSL                            
057900       MOVE ZERO          TO INVA-INV-KDPSLLOC                            
058000       MOVE IN-KDVVKL     TO INVA-INV-KDVVKL                              
058100       MOVE +0            TO INVA-INV-KVJUSTKV                            
058200       MOVE SPACE         TO INVA-INV-TEINVANM                            
058300       MOVE NEJ           TO INVA-INV-FLINVBEH                            
058400                             INVA-INV-FLINVSKR                            
058500                             INVA-INV-FLINV2B                             
058600                             INVA-INV-FLINV2C                             
058700                             INVA-INV-FLINV2D                             
058800                             INVA-INV-FLINV3E                             
058900                             INVA-INV-FLINV4N                             
059000                             INVA-INV-FLINV4P                             
059100                             INVA-INV-FLINV4R                             
059200                             INVA-INV-FLINV85                             
059300       MOVE ZERO          TO INVA-INV-KDINVKAT-OLD                        
059400       MOVE SPACE         TO INVA-INV-FILLER1                             
059500                             INVA-INV-FILLER2                             
059600                                                                          
059700       MOVE 20             TO WS-AAR                                      
059800       MOVE 0              TO WS-LOPNR                                    
059900       MOVE WS-TIAAAAMMDDL TO INVA-INV-TISEGKEY                           
060000                              W-TISEGKEY                                  
060100       MOVE ZERO           TO INVA-INV-IDPRTOMG                           
060200                              INVA-INV-IDLOPNR                            
060300                              INVA-INV-KVAKS-OLD                          
060400                              INVA-INV-KVEFRS-OLD                         
060500                              INVA-INV-KVLS-OLD                           
060600       MOVE WS-INV-DAREGDAT TO INVA-INV-DAREGDAT-CRE                      
060700***    COMPUTE INVA-INV-DAREGDAT-SORT =                                   
060800***      99999999 - WS-INV-DAREGDAT                                       
060900**  FÖR ATT POST SKALL KOMMA SIST PÅ BASEN ANVÄNDS 9999 SOM ÅR            
061000       MOVE 99999999        TO  INVA-INV-DAREGDAT-SORT                    
061100                                                                          
061200       MOVE WS-INV-DAREGDAT TO INVA-INV-DAREGDAT                          
061300       MOVE ZERO           TO INVA-INV-DAREGDAT-PR1                       
061400                              INVA-INV-DAREGDAT-PR2                       
061500                              INVA-INV-DAREGDAT-PR3                       
061600                                                                          
061700       PERFORM IMS-ISRT-WDH111                                            
061800       IF NOT SEGMENT-FINNS-REDAN                                         
061900         ADD +1          TO CHKP-ANT                                      
062000         ADD +1          TO ANT-INS-H111                                  
062100       END-IF                                                             
062200***    IF SEGMENT-FINNS                                                   
062300*** INSERT PÅ WDH21 SEGMENTET ***                                         
062400         MOVE 'W5137800'         TO INVA-INVL-IDUSER                      
062500         MOVE '0'                TO INVA-INVL-KDSEGKEY                    
062600         PERFORM IMS-INSERT-WDH121                                        
062700         IF NOT SEGMENT-FINNS-REDAN                                       
062800           ADD +1          TO ANT-INS-H121                                
062900         END-IF                                                           
063000         MOVE SPACE              TO INVA-INVL-IDUSER                      
063100         MOVE '1'                TO INVA-INVL-KDSEGKEY                    
063200         PERFORM IMS-INSERT-WDH121                                        
063300         IF NOT SEGMENT-FINNS-REDAN                                       
063400           ADD +1          TO ANT-INS-H121                                
063500         END-IF                                                           
063600         MOVE SPACE              TO INVA-INVL-IDUSER                      
063700         MOVE '2'                TO INVA-INVL-KDSEGKEY                    
063800         PERFORM IMS-INSERT-WDH121                                        
063900         IF NOT SEGMENT-FINNS-REDAN                                       
064000           ADD +1          TO ANT-INS-H121                                
064100         END-IF                                                           
064200         MOVE SPACE              TO INVA-INVL-IDUSER                      
064300         MOVE '3'                TO INVA-INVL-KDSEGKEY                    
064400         PERFORM IMS-INSERT-WDH121                                        
064500         IF NOT SEGMENT-FINNS-REDAN                                       
064600           ADD +1          TO ANT-INS-H121                                
064700         END-IF                                                           
064800**** SLUT PÅ INSERT PÅ WDH121 SEGMENT                                     
064900****   END-IF                                                             
065000       MOVE '  '  TO STATUS-WS                                            
065100       IF NOT SEGMENT-FINNS-REDAN                                         
065200         ADD +1          TO CHKP-ANT                                      
065300       END-IF                                                             
065400                                                                          
065500     END-IF                                                               
065600     .                                                                    
065700     EJECT                                                                
065800 BA-SELEKTERA-INVENTERING SECTION.                                        
065900                                                                          
066600     MOVE PRM-IDDC-2       TO W-IN-DC                                     
066610     IF PRM-IDDC-2 = '11'                                                 
066621       MOVE IN-KDVVKL TO WS-KDVVKL                                        
066630     ELSE                                                                 
066635         MOVE ZERO TO WS-KDVVKL                                           
066650     END-IF                                                               
066800                                                                          
066900     IF PRM-IDDC-2      = IN-IDDC               AND                       
067000        W-PRM-DC        = W-IN-DC               AND                       
067100       (PRM-KDVVKL      = IN-KDVVKL      OR                               
067200        PRM-KDVVKL      = WS-KDVVKL)            AND                       
067210*       PRM-KDVVKL      = IN-KDVVKL             AND                       
067300       (PRM-ADLAGOMR    = IN-ADLAGOMR    OR                               
067400       (PRM-ADLAGOMR    = ZERO AND                                        
067500        PRM-ADGANG      = ZERO AND                                        
067600        PRM-ADPLATS     = ZERO))                AND                       
067700       (PRM-ADGANG      = IN-ADGANG      OR                               
067800       (PRM-ADGANG      = ZERO AND                                        
067900        PRM-ADPLATS     = ZERO))                AND                       
068000       (PRM-ADPLATS     = IN-ADPLATS     OR                               
068100        PRM-ADPLATS     = ZERO)                 AND                       
068200       (PRM-KDPRODSL    = IN-KDPRODSL    OR                               
068300        PRM-KDPRODSL    = ZERO)                 AND                       
068400       (PRM-IDFKNGRP    = IN-IDFKNGRP    OR                               
068500        PRM-IDFKNGRP    = ZERO)                                           
068600       MOVE JA              TO POST-SKALL-MED-SW                          
068700       MOVE IN-IDARTNR      TO W-IDARTNR                                  
068800       PERFORM IMS-GET-WDH101                                             
068900       IF SEGMENT-FINNS                                                   
069000         MOVE IN-IDDC       TO IDDC-SEARCH-MIN                            
069100                               IDDC-SEARCH-MAX                            
069200         PERFORM IMS-GET-WDH111                                           
069300         IF SEGMENT-FINNS                                                 
069400           MOVE NEJ         TO POST-SKALL-MED-SW                          
069500         END-IF                                                           
069600       END-IF                                                             
069700     END-IF                                                               
069800     .                                                                    
069900     EJECT                                                                
070000 Z-FINIT SECTION.                                                         
070100                                                                          
070200     IF CDC-SE                                                            
070300       MOVE 'NO. OF PARTS               INSERTED = ' TO L1-TEXT           
070400       MOVE ANT-INS-H101                             TO L1-VAERDE         
070500       MOVE 'NO. OF DC/STOCKTAKING CAT. INSERTED = ' TO L1-TEXT           
070600       MOVE ANT-INS-H111                             TO L1-VAERDE         
070700     ELSE                                                                 
070800     IF SDC-NL                                                            
071700       MOVE 'NO. OF PARTS               INSERTED = ' TO L1-TEXT           
071800       MOVE ANT-INS-H101                             TO L1-VAERDE         
071900       WRITE W51378-021-L  FROM L1-DETRAD AFTER ADVANCING 2 LINE          
072000       MOVE 'NO. OF DC/STOCKTAKING CAT. INSERTED = ' TO L1-TEXT           
072100       MOVE ANT-INS-H111                             TO L1-VAERDE         
072200       WRITE W51378-021-L  FROM L1-DETRAD AFTER ADVANCING 1 LINE          
072400     ELSE                                                                 
073300     IF SDC-ES                                                            
073400       MOVE 'NO. OF PARTS               INSERTED = ' TO L1-TEXT           
073500       MOVE ANT-INS-H101                             TO L1-VAERDE         
073600       WRITE W51378-024-L  FROM L1-DETRAD AFTER ADVANCING 2 LINES         
073700       MOVE 'NO. OF DC/STOCKTAKING CAT. INSERTED = ' TO L1-TEXT           
073800       MOVE ANT-INS-H111                             TO L1-VAERDE         
073900       WRITE W51378-024-L  FROM L1-DETRAD AFTER ADVANCING 1 LINES         
074000     ELSE                                                                 
074100     IF SDC-IT                                                            
074200       MOVE 'NO. OF PARTS               INSERTED = ' TO L1-TEXT           
074300       MOVE ANT-INS-H101                             TO L1-VAERDE         
074400       WRITE W51378-025-L  FROM L1-DETRAD AFTER ADVANCING 2 LINES         
074500       MOVE 'NO. OF DC/STOCKTAKING CAT. INSERTED = ' TO L1-TEXT           
074600       MOVE ANT-INS-H111                             TO L1-VAERDE         
074700       WRITE W51378-025-L  FROM L1-DETRAD AFTER ADVANCING 1 LINES         
074800     ELSE                                                                 
074900     IF SDC-AT                                                            
075000       MOVE 'NO. OF PARTS               INSERTED = ' TO L1-TEXT           
075100       MOVE ANT-INS-H101                             TO L1-VAERDE         
075200       WRITE W51378-026-L  FROM L1-DETRAD AFTER ADVANCING 2 LINES         
075300       MOVE 'NO. OF DC/STOCKTAKING CAT. INSERTED = ' TO L1-TEXT           
075400       MOVE ANT-INS-H111                             TO L1-VAERDE         
075500       WRITE W51378-026-L  FROM L1-DETRAD AFTER ADVANCING 1 LINES         
075501     ELSE                                                                 
075502     IF SDC-NL-ET                                                         
075503       MOVE 'NO. OF PARTS               INSERTED = ' TO L1-TEXT           
075504       MOVE ANT-INS-H101                             TO L1-VAERDE         
075505       WRITE W51378-091-L  FROM L1-DETRAD AFTER ADVANCING 2 LINE          
075506       MOVE 'NO. OF DC/STOCKTAKING CAT. INSERTED = ' TO L1-TEXT           
075507       MOVE ANT-INS-H111                             TO L1-VAERDE         
075508       WRITE W51378-091-L  FROM L1-DETRAD AFTER ADVANCING 1 LINE          
075700     END-IF                                                               
075800     END-IF                                                               
075900     END-IF                                                               
076100     END-IF                                                               
076200     END-IF                                                               
076210     END-IF                                                               
076300                                                                          
076400     CLOSE W51372                                                         
076500     IF SDC-NL                                                            
076900       CLOSE W51378-021                                                   
077100     ELSE                                                                 
077500     IF SDC-ES                                                            
077600       CLOSE W51378-024                                                   
077700     ELSE                                                                 
077800     IF SDC-IT                                                            
077900       CLOSE W51378-025                                                   
078000     ELSE                                                                 
078100     IF SDC-AT                                                            
078200       CLOSE W51378-026                                                   
078201     ELSE                                                                 
078202     IF SDC-NL-ET                                                         
078203       CLOSE W51378-091                                                   
078300     END-IF                                                               
078400     END-IF                                                               
078500     END-IF                                                               
078700     END-IF                                                               
078710     END-IF                                                               
078800                                                                          
078900     MOVE 'S' TO POSTSUM-OPKOD                                            
079000     CALL POSTSUM USING POSTSUM-PARM                                      
079100                                                                          
079200     DISPLAY 'ANTAL UPPLAGDA ARTIKELRÖTTER: '  ANT-INS-H101               
079300     DISPLAY 'ANTAL UPPLAGDA H111-SEGMENT : '  ANT-INS-H111               
079400     DISPLAY 'ANTAL UPPLAGDA H121-SEGMENT : '  ANT-INS-H121               
079500     .                                                                    
079600     EJECT                                                                
079700 S01-LAES-W51372  SECTION.                                                
079800                                                                          
079900     READ W51372 INTO IN-W51371                                           
080000     AT END                                                               
080100        MOVE JA          TO W51372-EOF                                    
080200     NOT AT END                                                           
080300        MOVE 'IN  '      TO POSTSUM-TRANSTYP                              
080400        MOVE 'W51372'    TO POSTSUM-FDNAMN                                
080500        MOVE 'W51378D1'  TO POSTSUM-DDNAMN2                               
080600        CALL POSTSUM USING POSTSUM-PARM                                   
080700     END-READ                                                             
080800     .                                                                    
080900     EJECT                                                                
081000 X-TAG-CHECKPOINT   SECTION.                                              
081100                                                                          
081200     PERFORM IMS-CHECKPOINT                                               
081300     MOVE ZERO TO CHKP-ANT                                                
081400     .                                                                    
081500     EJECT                                                                
081600* --- IMS SEKTIONER ---                                                   
081700     SKIP3                                                                
081800 IMS-GET-WDH101 SECTION.                                                  
081900                                                                          
082000     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
082100          DELIMITED BY SIZE INTO SSA1                                     
082200     MOVE '  GE' TO GODK-STATUSKODER                                      
082300     CALL CBLTDLI USING GU INVA-PCB DLI-IO-AREA-H101 SSA1                 
082400     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
082500     PERFORM IMS-STATUSKONTROLL                                           
082600     .                                                                    
082700     SKIP3                                                                
082800 IMS-GET-WDH111 SECTION.                                                  
082900                                                                          
083000     STRING  'WDH111  (WDH111KY>=' W-WDH111KY-MIN                         
083100                     '&WDH111KY<=' W-WDH111KY-MAX ')'                     
083200              DELIMITED BY SIZE INTO SSA1                                 
083300     MOVE '  GE' TO GODK-STATUSKODER                                      
083400     CALL CBLTDLI USING GHNP INVA-PCB DLI-IO-AREA-H111 SSA1               
083500     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
083600     PERFORM IMS-STATUSKONTROLL                                           
083700     .                                                                    
083800     EJECT                                                                
083900 IMS-ISRT-WDH101 SECTION.                                                 
084000                                                                          
084100     MOVE 'WDH101   ' TO SSA1                                             
084200     MOVE '  ' TO GODK-STATUSKODER                                        
084300     CALL CBLTDLI USING ISRT INVA-PCB DLI-IO-AREA-H101 SSA1               
084400     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
084500     PERFORM IMS-STATUSKONTROLL                                           
084600     .                                                                    
084700     SKIP3                                                                
084800 IMS-ISRT-WDH111 SECTION.                                                 
084900                                                                          
085000     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
085100          DELIMITED BY SIZE INTO SSA1                                     
085200     MOVE 'WDH111 ' TO SSA2                                               
085300     MOVE '  II' TO GODK-STATUSKODER                                      
085400     CALL CBLTDLI USING ISRT INVA-PCB DLI-IO-AREA-H111 SSA1 SSA2          
085500     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
085600     PERFORM IMS-STATUSKONTROLL                                           
085700     .                                                                    
085800     SKIP3                                                                
085900 IMS-INSERT-WDH121 SECTION.                                               
086000                                                                          
086100     MOVE 'WDH121 ' TO SSA1                                               
086200     MOVE '  II' TO GODK-STATUSKODER                                      
086300     CALL CBLTDLI USING ISRT INVA-PCB                                     
086400     DLI-IO-AREA-H121 SSA1                                                
086500                                                                          
086600     MOVE INVA-STATUS-CODE TO STATUS-WS                                   
086700     PERFORM IMS-STATUSKONTROLL                                           
086800     .                                                                    
086900     SKIP3                                                                
087000 IMS-RESTART SECTION.                                                     
087100                                                                          
087200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
087300     MOVE '  ' TO GODK-STATUSKODER                                        
087400     CALL CBLTDLI USING XRST MSG-PCB                                      
087500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
087600                        CHKP-AREA-LENGTH CHKP-AREA                        
087700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
087800     PERFORM IMS-STATUSKONTROLL                                           
087900     .                                                                    
088000     SKIP3                                                                
088100 IMS-CHECKPOINT SECTION.                                                  
088200                                                                          
088300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
088400     MOVE '  XD' TO GODK-STATUSKODER                                      
088500     CALL CBLTDLI USING CHKP MSG-PCB                                      
088600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
088700                        CHKP-AREA-LENGTH CHKP-AREA                        
088800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
088900     PERFORM IMS-STATUSKONTROLL                                           
089000                                                                          
089100     IF IMS-EJ-OK                                                         
089200       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
089300       DISPLAY FELTEXT                                                    
089400       CALL FELLOG                                                        
089500     END-IF                                                               
089600     .                                                                    
089700     SKIP3                                                                
089800 IMS-STATUSKONTROLL SECTION.                                              
089900                                                                          
090000     SET STATUS-IX TO 1                                                   
090100     SEARCH GODK-STATUS                                                   
090200       AT END                                                             
090300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
090400           DELIMITED BY SIZE INTO FELTEXT                                 
090500         DISPLAY FELTEXT                                                  
090600         CALL FELLOG                                                      
090700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
090800         CONTINUE                                                         
090900     END-SEARCH                                                           
091000     .                                                                    
