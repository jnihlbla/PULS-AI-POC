000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W2612300.                                                
000400 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000500 DATE-WRITTEN.   11/11/01.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        FRAMSTÄLLER UNDERLAG FÖR MAIL INFÖR SKROTNING                    
001100*                                                                         
001200*        PROGRAMMET LÄSER      WDK6                                       
001300*        PROGRAMMET LÄSER      WDL8                                       
001400*                                                                         
001500*        THE PROGRAM READS   TABLE TP1KAMP                                
001600*        THE PROGRAM READS   TABLE TP1ARTK                                
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- ARTIKLAR INFÖR  SKROT                                      
002700     SELECT W26123                     ASSIGN TO W26123D1.                
002800*          --- ARTIKLAR LISTA FÖR MAIL .                                  
002900     SELECT W26124                     ASSIGN TO W26123D2.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W26123                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01  -COPY W26123   -L.                                                   
004000     SKIP3                                                                
004100 FD  W26124                                                               
004200     RECORDING       V                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500*01  POST -COPY W26124R -PRE  UT-  -L.                                    
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004900 77  IDPGM                       PIC X(8)    VALUE 'W2612300'.            
005000 77  JA                          PIC X       VALUE 'J'.                   
005100 77  NEJ                         PIC X       VALUE 'N'.                   
005200 77  IX                          PIC 9(2)    VALUE ZERO.                  
005300 77  WS-IX                       PIC 9(2)    VALUE ZERO.                  
005400     SKIP2                                                                
005500 01  FELTEXT.                                                             
005600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005800 77  WS-SPAR-BEANST-GODK         PIC X(25)   VALUE SPACE.                 
005900 77  W26123-EOF-SW               PIC X       VALUE 'N'.                   
006000     88  END-OF-W26123                       VALUE 'J'.                   
006100 77  SW-K611-OK                  PIC X       VALUE 'N'.                   
006200 77  SW-KAMPANJ                  PIC X       VALUE 'N'.                   
006300 77  SW-SATS-OK                  PIC X       VALUE 'N'.                   
006400     EJECT                                                                
006500 01  SWITCHAR.                                                            
006600     03  INGAR-SATS-SW           PIC X.                                   
006700         88  INGAR-I-SATS    VALUE 'J'.                                   
006800*                                                                         
006900     EJECT                                                                
007000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007100 01  FILLER REDEFINES DAGENS-DATUM.                                       
007200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007500                                                                          
007600 77  DAGENS-DATUM-Y2K            PIC 9(8)  VALUE ZERO.                    
007700     EJECT                                                                
007800 01  DYNAMISKA-SUBPROGRAM.                                                
007900*                                                                         
008000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
008400     SKIP3                                                                
008500 01  ARB.                                                                 
008600     03  IDARTNR-WS              PIC S9(9)   VALUE ZERO COMP-3.           
008700     03  WS-FLERS                PIC X(1)    VALUE SPACE.                 
008800                                                                          
008900     03  WS-SUARTSTD             PIC 9(7)V9(2) VALUE ZERO.                
009000     03  WS-TIAAAA               PIC 9(4)    VALUE ZERO.                  
009100     03  FILLER  REDEFINES  WS-TIAAAA.                                    
009200         05  WS-TISEKEL          PIC 9(2).                                
009300         05  WS-TIAA-VECKA       PIC 9(2).                                
009400     03  WS-TIAAAA-1             PIC 9(4)    VALUE ZERO.                  
009500     03  WS-TIVV                 PIC S9(3)   VALUE ZERO  COMP-3.          
009600     03  WS-ANTAL-OI             PIC S9(7)   VALUE ZERO COMP-3.           
009700     03  WS-ANTAL-RAD            PIC S9(7)   VALUE ZERO COMP-3.           
009800                                                                          
009900 01  W-DATUM.                                                             
010000     05  W-DATUM-DATE    PIC X(6).                                        
010100     EJECT                                                                
      *                                                                         
006400 01  IX-AR                       PIC 9(9)    COMP-3 VALUE ZERO.           
006500 01  IX-PER                      PIC 9(9)    COMP-3 VALUE ZERO.           
      *                                                                         
009700 01  FILLER                      PIC X(04)   VALUE 'WS'.                  
009800 01  WS.                                                                  
009900  05 WS-DAGENS-AAAAMMDD          PIC 9(8)    VALUE ZERO.                  
010000  05 FILLER                      PIC X(08)   VALUE 'WS-AAPP'.             
010100  05 WS-AAPP                     PIC 9(4)    VALUE ZERO.                  
010200  05 FILLER REDEFINES            WS-AAPP.                                 
010300   10 WS-AA                      PIC 9(2).                                
010400   10 WS-PP                      PIC 9(2).                                
010500*                                                                         
010600  05 WS-VAL                      PIC X(1)    VALUE SPACE.                 
010700  05 WS-AARTAL                   PIC 9(4)    VALUE ZERO.                  
010800  05 WS-PER                      PIC 9(2)    VALUE ZERO.                  
010900  05 WS-KVOI-RED                 PIC 9(7)    VALUE ZERO.                  
011000  05 WS-TOTAL-KVOI               PIC 9(7)    VALUE ZERO.                  
011100  05 WS-KVOI-PER         OCCURS 3 TIMES.                                  
011200   10 WS-KVOI-TOT-TAB            PIC 9(7)    VALUE ZERO.                  
011300   10 WS-KVOI-TAB        OCCURS 12 TIMES                                  
011400                                 PIC 9(7)    VALUE ZERO.                  
011500*                                                                         
011600  05 WS-VV                       PIC  9(2)   VALUE ZERO.                  
011700  05 FILLER                      PIC  X(16)  VALUE 'WS-TABELL'.           
011800  05  WS-TABELL    OCCURS 12.                                             
011900   10 WS-FORSTA-V                PIC  9(2)   VALUE ZERO.                  
012000   10 WS-SISTA-V                 PIC  9(2)   VALUE ZERO.                  
012100   10 WS-KVVIPER                 PIC 9       VALUE ZERO.                  
012200   10 WS-KVOI                    PIC S9(7)   VALUE ZERO COMP-3.           
012300*                                                                         
012400  05 WS-TESTFAELT.                                                        
012500   10 WS-FORSTA-TF               PIC  9(2)   VALUE ZERO.                  
012600   10 FILLER                     PIC  X      VALUE SPACE.                 
012700   10 WS-SISTA-TF                PIC  9(2)   VALUE ZERO.                  
012800   10 FILLER                     PIC  X      VALUE SPACE.                 
012900   10 WS-KVOI-TF                 PIC  9(7)   VALUE ZERO.                  
013000*                                                                         
009150 01  WS-VKART-DISP               PIC 9(7)    VALUE ZERO.                  
009151*                                                                         
009152 01  WS-UT-VKART.                                                         
009160   03  WS-VKART-HELTAL           PIC X(3)    VALUE ZERO.                  
009170   03  WS-VKART-PUNKT            PIC X(1)    VALUE ','.                   
009180   03  WS-VKART-DECIMAL          PIC X(3)    VALUE ZERO.                  
009200*                                                                         
008500 01  WS-PRARTSTD-DISP            PIC 9(7)V9(2) VALUE ZERO.                
008510*                                                                         
008520 01  WS-UT-PRARTSTD.                                                      
008600   03  WS-PRARTSTD-HELTAL        PIC X(6)    VALUE ZERO.                  
008700   03  WS-PRARTSTD-PUNKT         PIC X(1)    VALUE ','.                   
008800   03  WS-PRARTSTD-DECIMAL       PIC X(2)    VALUE ZERO.                  
009000*                                                                         
009010 01  WS-PRARTBTO-MARK-DISP       PIC 9(7)V9(2) VALUE ZERO.                
009020*                                                                         
009100 01  WS-UT-PRARTBTO-MARK.                                                 
009110   03  WS-PRARTBTO-MARK-HELTAL   PIC X(6)    VALUE ZERO.                  
009120   03  WS-PRARTBTO-MARK-PUNKT    PIC X(1)    VALUE ','.                   
009130   03  WS-PRARTBTO-MARK-DECIMAL  PIC X(2)    VALUE ZERO.                  
009140*                                                                         
010200                                                                          
010300*01  -COPY WWPRODSL                                                       
010400                                                                          
010500*- - - - - - - - - - - - - -  PARAMETRAR TILL WDATKONV                    
010600                                                                          
010700*01  -COPY WDATAREA                                                       
010800     EJECT                                                                
010900*    --- PARAMETRAR TILL POSTSUM                                          
011000*                                                                         
011100*01  -COPY W0005   -PRE  POSTSUM-                                         
011200     EJECT                                                                
011300 01  IN-AREA-START               PIC X(24)   VALUE                        
011400                                             'IN-AREA-START'.             
011500     SKIP2                                                                
011600                                                                          
011700*01  AREA -COPY W26123     -PRE IN-                                       
011800*                                                                         
011900     SKIP2                                                                
012000                                                                          
012100*01  AREA -COPY W26124     -PRE UT-                                       
012200     SKIP2                                                                
012300                                                                          
012400*01  -COPY W26124R                                                        
012500     EJECT                                                                
012600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012700     SKIP3                                                                
012800 01  NYCKLAR-TILL-DLI.                                                    
012900     03  W-IDARTNR-X.                                                     
013000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
013100     03  W-KDSEGKEY-X.                                                    
013200         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
013300     03  W-IDLEVNR-X.                                                     
013400         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
013500                                                                          
013600     03  W-TIAAAA-X.                                                      
013700         05  W-TIAAAA            PIC 9(4).                                
013800                                                                          
013900     EJECT                                                                
020500******* NYCKLAR TILL WDC1   *********                                     
020600     03  W-WDC101KY-X.                                                    
020700         05  W-IDARTNR-111       PIC S9(9)   VALUE ZERO COMP-3.           
020800         05  W-IDMARKBO-111      PIC X       VALUE SPACE.                 
020900                                                                          
014000*    --- STATUS-KOD FRÅN IMS                                              
014100 01  STATUS-WS                   PIC XX.                                  
014200     88  SEGMENT-FINNS                       VALUE '  '.                  
014300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
014400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014500     88  SEGMENT-SLUT                        VALUE 'GB'.                  
014600     88  IMS-EJ-OK                           VALUE 'XD'.                  
014700     SKIP2                                                                
014800 01  GODK-STATUSKODER.                                                    
014900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015000     SKIP3                                                                
015100 01  SSA1                        PIC X(400).                              
015200 01  SSA2                        PIC X(64).                               
015300 01  SSA3                        PIC X(64).                               
015400 01  SSA4                        PIC X(64).                               
015500     EJECT                                                                
015600*    --- IMS FUNKTIONSKODER                                               
015700*01  -COPY W0003                                                          
015800     EJECT                                                                
015900*    ---  DLI INPUT-OUTPUT AREA                                           
016000                                                                          
016100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
016200 01  DLI-IO-WDK601.                                                       
016300*    03  -COPY WDK601                                                     
016400     EJECT                                                                
016500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
016600 01  DLI-IO-WDK611.                                                       
016700*    03  -COPY WDK611                                                     
016800     EJECT                                                                
016900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL801'.                      
017000 01  DLI-IO-WDL801.                                                       
017100*    03  -COPY WDL801                                                     
017200     EJECT                                                                
017300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL811'.                      
017400 01  DLI-IO-WDL811.                                                       
017500*    03  -COPY WDL811                                                     
017600                                                                          
017600     EJECT                                                                
017600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDJ101'.                      
017600 01  DLI-IO-WDJ101.                                                       
017600*  03    -COPY WDJ101                                                     
017700     EJECT                                                                
017600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC101'.                      
017600 01  DLI-IO-WDC101.                                                       
017600*  03    -COPY WDC101                                                     
017700     EJECT                                                                
017600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK627'.                      
017600 01  DLI-IO-WDK627.                                                       
017600*  03    -COPY WDK627                                                     
017700     EJECT                                                                
017800 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
017900       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
018000                                                                          
018100 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
018200 01  DB2-WS.                                                              
018300     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
018400         88  CURSOR-OK                      VALUE 000.                    
018500         88  LINES-FOUND                    VALUE 000.                    
018600         88  LINES-MISSING                  VALUE 100.                    
018700         88  RESOURCE-WRONG                 VALUE 904.                    
018800     03  GOOD-SQLCODECODES.                                               
018900         05  GOOD-SQLCODE OCCURS 5                                        
019000             INDEXED BY SQLCODE-IX PIC 9(3).                              
019100                                                                          
019200 01  WS.                                                                  
019300     03 WS-SECTION               PIC X(24)   VALUE SPACE.                 
019400     03 FILLER                   PIC X(16)   VALUE                        
019500                                             'WS-DB2-SEKTION'.            
019600     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
019700     EJECT                                                                
019800 01  FILLER                      PIC X(16)  VALUE 'TP1KAMP-AREA'.         
019900                                                                          
020000*01  -COPY TP1KAMP -PRE TP1KAMP-                                          
020100     EJECT                                                                
020200 01  FILLER                      PIC X(16)  VALUE 'TP1ARTK-AREA'.         
020300                                                                          
020400*01  -COPY TP1ARTK -PRE TP1ARTK-                                          
020500     EJECT                                                                
020600     EXEC SQL INCLUDE TP1KAMP END-EXEC.                                   
020700     EJECT                                                                
020800     EXEC SQL INCLUDE TP1ARTK END-EXEC.                                   
020900     EJECT                                                                
021000 LINKAGE SECTION.                                                         
021100                                                                          
021200*01  -COPY W0009   -PRE MSG-                                              
021300                                                                          
021400*01  -COPY W0008  -PRE WDK6-                                              
021500     05  FILLER                  PIC X.                                   
021600                                                                          
021700*01  -COPY W0008  -PRE WDL8-                                              
021800     05  FILLER                  PIC X.                                   
021800                                                                          
021600*01  -COPY W0008  -PRE WDJ1-                                              
021700     05  FILLER                  PIC X.                                   
021900     EJECT                                                                
021600*01  -COPY W0008  -PRE WDC1-                                              
021700     05  FILLER                  PIC X.                                   
021900     EJECT                                                                
022000 PROCEDURE DIVISION  USING                                                
022100           WDK6-PCB                                                       
022200           WDL8-PCB                                                       
022200           WDJ1-PCB                                                       
022200           WDC1-PCB.                                                      
022300 MAIN SECTION.                                                            
022400     ENTRY 'DLITCBL' USING                                                
022500           WDK6-PCB                                                       
022600           WDL8-PCB                                                       
022600           WDJ1-PCB                                                       
022600           WDC1-PCB.                                                      
022700                                                                          
022800     SKIP2                                                                
022900     PERFORM A-INIT                                                       
023000     PERFORM S01-LAES-W26123                                              
023100     PERFORM UNTIL END-OF-W26123                                          
023200       MOVE IN-IDARTNR       TO W-IDARTNR                                 
023300                                IDARTNR-WS                                
023400       PERFORM IMS-GET-WDK601                                             
023500       IF SEGMENT-FINNS                                                   
023600          IF ART-FLIART = JA                                              
023700             MOVE ART-FLIART TO INGAR-SATS-SW                             
023800          ELSE                                                            
023900             MOVE NEJ        TO INGAR-SATS-SW                             
024000          END-IF                                                          
024100          MOVE ART-FLERS     TO WS-FLERS                                  
024200          PERFORM B-LAES-SKROTINFO                                        
024300                                                                          
024400          IF  SW-K611-OK = JA   AND                                       
024500              CLAG-TISKROT-AUTO < DAGENS-DATUM                            
024600**************AND CLAG-FLSKROT-BEORD NOT = JA                             
024700              PERFORM D-SKAPA-FIL                                         
024800          END-IF                                                          
024900       END-IF                                                             
025000       PERFORM S01-LAES-W26123                                            
025100     END-PERFORM                                                          
025200                                                                          
025300                                                                          
025400     PERFORM Z-FINIT                                                      
025500                                                                          
025600     MOVE ZERO TO RETURN-CODE                                             
025700     GOBACK                                                               
025800     .                                                                    
025900     EJECT                                                                
026000 A-INIT SECTION.                                                          
026100     SKIP2                                                                
026200                                                                          
026300                                                                          
026400     OPEN INPUT W26123                                                    
026500         OUTPUT W26124                                                    
026600                                                                          
026700                                                                          
026800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
026900                                                                          
027000     ACCEPT W-DATUM-DATE FROM DATE                                        
027100     ACCEPT DAGENS-DATUM FROM DATE                                        
027200                                                                          
027300     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM-Y2K                 
027400                                                                          
027500     INITIALIZE GOOD-SQLCODECODES                                         
027600                                                                          
027700     MOVE 'IDAG' TO DAT-KDDATFORM                                         
027800     CALL WDATKONV USING DAT-KDDATFORM,                                   
027900                         DAT-I-TIDATUM,                                   
028000                         DAT-O-TIDATUM,                                   
028100                         DAT-KDSVAR                                       
028200                                                                          
028300     IF DAT-KDSVAR-FEL                                                    
028400        DISPLAY '****  FEL I WDATKONV  *******'                           
028500        CALL FELLOG                                                       
028600     END-IF                                                               
028700                                                                          
028800     MOVE DAT-TIVV       TO WS-TIVV                                       
028900     MOVE DAT-TISEKEL    TO WS-TISEKEL                                    
029000     MOVE DAT-TIAA-VECKA TO WS-TIAA-VECKA                                 
029100     MOVE WS-TIAAAA      TO WS-TIAAAA-1                                   
029200     SUBTRACT +1       FROM WS-TIAAAA-1                                   
029300                                                                          
029400     .                                                                    
029500     EJECT                                                                
029600 B-LAES-SKROTINFO SECTION.                                                
029700                                                                          
029800     PERFORM IMS-GET-WDK611                                               
029900     IF  SEGMENT-FINNS       AND                                          
030000         CLAG-TISKROT-AUTO < DAGENS-DATUM                                 
030100*********AND CLAG-FLSKROT-BEORD NOT = JA                                  
030200        MOVE JA               TO SW-K611-OK                               
030300                                                                          
030400        IF INGAR-SATS-SW = JA                                             
030500           PERFORM BA-LAES-SATS-OI                                        
030600        END-IF                                                            
030500        PERFORM BB-GET-PART-INFO                                          
030500        PERFORM BC-GET-PRARTBTO                                           
030500        PERFORM BD-GET-FLSATART                                           
030500        PERFORM BE-GET-KVOI                                               
030700     ELSE                                                                 
030800        MOVE NEJ              TO SW-K611-OK                               
030900     END-IF                                                               
031000     .                                                                    
031100     EJECT                                                                
031200 BA-LAES-SATS-OI SECTION.                                                 
031300                                                                          
031400*    EFTERSOM INGÅENDE SATSARTIKLAR INTE FINNS MED FRÅN S&T               
031500*    SÅ ANVÄNDER VI ORDERINGÅNG I STÄLLET FÖR FÖRSÄLJNING                 
031600*    FÖR DESSA (ANTAL SÅLDA SISTA RULLANDE ÅR < 150)                      
031700                                                                          
031800     MOVE ZERO            TO WS-ANTAL-OI                                  
031900     MOVE IN-IDARTNR      TO W-IDARTNR                                    
032000     MOVE WS-TIAAAA       TO W-TIAAAA                                     
032100     PERFORM IMS-GU-WDL811                                                
032200     IF SEGMENT-FINNS                                                     
032300        MOVE +1           TO WS-IX                                        
032400        PERFORM UNTIL WS-IX >= WS-TIVV                                    
032500           ADD AAR-KVOI-SATS (WS-IX) TO WS-ANTAL-OI                       
032600           ADD +1         TO WS-IX                                        
032700        END-PERFORM                                                       
032800     END-IF                                                               
032900     MOVE WS-TIAAAA-1     TO W-TIAAAA                                     
033000     PERFORM IMS-GU-WDL811                                                
033100     IF SEGMENT-FINNS                                                     
033200        MOVE WS-TIVV      TO WS-IX                                        
033300        PERFORM UNTIL WS-IX >  53                                         
033400           ADD AAR-KVOI-SATS (WS-IX) TO WS-ANTAL-OI                       
033500           ADD +1         TO WS-IX                                        
033600        END-PERFORM                                                       
033700     END-IF                                                               
033800     IF (IN-SULEVANT-RAAR + WS-ANTAL-OI) > 150                            
033900*       EJ AKTUELL FÖR SKROT                                              
034000        MOVE NEJ          TO SW-K611-OK                                   
034100     END-IF                                                               
034200     .                                                                    
034300     EJECT                                                                
 34300                                                                          
045400 BB-GET-PART-INFO SECTION.                                                
045600                                                                          
045700     PERFORM IMS-GU-WDK601                                                
046000      MOVE ART-KDSORT              TO UT-KDSORT                           
046300*     IF SEGMENT-FINNS                                                    
046400*       MOVE ART-KDPRODSL          TO WS-KDPRODSL                         
046500*     ELSE                                                                
046600*       MOVE 00                    TO WS-KDPRODSL                         
046700*     END-IF                                                              
046800                                                                          
046900      PERFORM IMS-GNP-WDK611                                              
047000      IF SEGMENT-FINNS                                                    
047200        MOVE CLAG-IDINK            TO UT-IDINK                            
047200        MOVE CLAG-KDARTURS         TO UT-KDARTURS                         
047410        MOVE CLAG-PRARTSTD         TO WS-PRARTSTD-DISP                    
047412        MOVE WS-PRARTSTD-DISP(2:6) TO WS-PRARTSTD-HELTAL                  
047413        MOVE WS-PRARTSTD-DISP(8:2) TO WS-PRARTSTD-DECIMAL                 
047420        MOVE WS-UT-PRARTSTD        TO UT-PRARTSTD                         
047600        MOVE CLAG-VKART            TO WS-VKART-DISP                       
047601        MOVE WS-VKART-DISP(2:3)    TO WS-VKART-HELTAL                     
047602        MOVE WS-VKART-DISP(5:3)    TO WS-VKART-DECIMAL                    
047604        MOVE WS-UT-VKART           TO UT-VKART                            
047900        MOVE CLAG-KDFARLIG         TO UT-KDFARLIG                         
048100        PERFORM IMS-GNP-WDK627                                            
048200        IF SEGMENT-FINNS                                                  
048400          MOVE SKROT-KVSKROT       TO UT-KVSKROT                          
048500          MOVE SKROT-DASKROT       TO UT-DASKROT                          
048600        END-IF                                                            
048700      END-IF                                                              
048800     .                                                                    
048900     EJECT                                                                
049000                                                                          
050300 BC-GET-PRARTBTO SECTION.                                                 
050500                                                                          
051200     MOVE 'B'                    TO W-IDMARKBO-111                        
051500     PERFORM IMS-GU-WDC101                                                
051600     IF SEGMENT-FINNS                                                     
051803       MOVE ART-PRARTBTO-MARK    TO WS-PRARTBTO-MARK-DISP                 
051810       MOVE WS-PRARTBTO-MARK-DISP(2:7) TO WS-PRARTBTO-MARK-HELTAL         
051811       MOVE WS-PRARTBTO-MARK-DISP(8:2) TO WS-PRARTBTO-MARK-DECIMAL        
051820       MOVE WS-UT-PRARTBTO-MARK  TO UT-PRARTBTO-MARK                      
051910     ELSE                                                                 
052000       MOVE ZERO                 TO UT-PRARTBTO-MARK                      
052100     END-IF                                                               
052200     .                                                                    
052300     EJECT                                                                
052400                                                                          
 34300 BD-GET-FLSATART SECTION.                                                 
 34300                                                                          
 34300     PERFORM IMS-GU-WDJ101                                                
 34300     IF SEGMENT-FINNS                                                     
 34300       MOVE JA            TO UT-FLSATART                                  
 34300     ELSE                                                                 
 34300       MOVE NEJ           TO UT-FLSATART                                  
 34300     END-IF                                                               
 34300     .                                                                    
 34300     EJECT                                                                
 34300                                                                          
 34300 BE-GET-KVOI SECTION.                                                     
052800                                                                          
052800*GET WEEK IN PERIOD                                                       
054000     MOVE WS-DAGENS-AAAAMMDD (1:4) TO W-TIAAAA                            
054100     MOVE IN-IDARTNR               TO W-IDARTNR                           
054300                                                                          
054400     PERFORM IMS-GU-WDL811                                                
054600     IF SEGMENT-FINNS                                                     
054800       MOVE +3                 TO IX-AR                                   
054900       MOVE +1                 TO IX-PER                                  
055000       MOVE ZERO               TO WS-TOTAL-KVOI                           
055100                                                                          
055200       PERFORM UNTIL IX-PER    =  WS-PER                                  
055300         MOVE ZERO             TO WS-KVOI (IX-PER)                        
055400         MOVE WS-FORSTA-V(IX-PER)                                         
055500                               TO WS-VV                                   
055600         PERFORM UNTIL WS-VV   >  WS-SISTA-V(IX-PER)                      
055700*          WS-VAL = 'TOTAL'                                               
055800           ADD AAR-KVOI-PROG(WS-VV) TO WS-KVOI(IX-PER)                    
055900           ADD AAR-KVOI-NDC(WS-VV)  TO WS-KVOI(IX-PER)                    
056000           ADD AAR-KVOI-SDC(WS-VV)  TO WS-KVOI(IX-PER)                    
056100           ADD AAR-KVOI-SATS(WS-VV) TO WS-KVOI(IX-PER)                    
056200           ADD AAR-KVOI-DIV(WS-VV)  TO WS-KVOI(IX-PER)                    
056300                                                                          
056400           ADD +1              TO WS-VV                                   
056500         END-PERFORM                                                      
056600         COMPUTE WS-KVOI-RED ROUNDED =                                    
056700                 WS-KVOI (IX-PER)                                         
056800                            / WS-KVVIPER (IX-PER) * +4.33                 
056900                                                                          
057000         ADD WS-KVOI-RED       TO WS-KVOI-TAB (IX-AR, IX-PER)             
057100                                  WS-TOTAL-KVOI                           
057400         ADD +1                TO IX-PER                                  
057500       END-PERFORM                                                        
057600                                                                          
057700       ADD WS-TOTAL-KVOI       TO WS-KVOI-TOT-TAB (IX-AR)                 
058000     END-IF                                                               
058100                                                                          
058200     MOVE IN-IDARTNR           TO W-IDARTNR                               
058300     MOVE +2                   TO IX-AR                                   
058400     PERFORM UNTIL IX-AR < +1                                             
058500                                                                          
058600*  POST HISTORY FOR PREVIOUS YEAR AND EARLIER                             
058700                                                                          
058800       SUBTRACT 1              FROM W-TIAAAA                              
058900       PERFORM IMS-GU-WDL811                                              
059000                                                                          
059100       IF SEGMENT-FINNS                                                   
059200         MOVE 1                TO IX-PER                                  
059300         MOVE ZERO             TO WS-TOTAL-KVOI                           
059400         PERFORM UNTIL IX-PER > 12                                        
059500                                                                          
059600           MOVE ZERO           TO WS-KVOI (IX-PER)                        
059700           MOVE WS-FORSTA-V (IX-PER)                                      
059800                               TO WS-VV                                   
059900           PERFORM UNTIL WS-VV > WS-SISTA-V (IX-PER)                      
060000*            WS-VAL = 'TOTAL'                                             
060100             ADD AAR-KVOI-PROG(WS-VV) TO WS-KVOI(IX-PER)                  
060200             ADD AAR-KVOI-NDC(WS-VV)  TO WS-KVOI(IX-PER)                  
060300             ADD AAR-KVOI-SDC(WS-VV)  TO WS-KVOI(IX-PER)                  
060400             ADD AAR-KVOI-SATS(WS-VV) TO WS-KVOI(IX-PER)                  
060500             ADD AAR-KVOI-DIV(WS-VV)  TO WS-KVOI(IX-PER)                  
060600                                                                          
060700             ADD 1             TO WS-VV                                   
060800           END-PERFORM                                                    
060900                                                                          
061000           COMPUTE WS-KVOI-RED ROUNDED =                                  
061100              WS-KVOI (IX-PER) * +4.33                                    
061200                 / WS-KVVIPER (IX-PER)                                    
061300                                                                          
061400           ADD WS-KVOI-RED     TO WS-TOTAL-KVOI                           
061800           ADD +1              TO IX-PER                                  
061900         END-PERFORM                                                      
062000                                                                          
062100         ADD WS-TOTAL-KVOI     TO WS-KVOI-TOT-TAB (IX-AR)                 
062400       END-IF                                                             
062500                                                                          
062600       SUBTRACT +1             FROM IX-AR                                 
062700     END-PERFORM                                                          
062800                                                                          
062900     MOVE +1                 TO IX-AR                                     
063000                                                                          
063100     PERFORM UNTIL IX-AR > +2                                             
063200                                                                          
063300       EVALUATE IX-AR                                                     
063600       WHEN +1                                                            
063700         MOVE WS-KVOI-TOT-TAB (+1) TO UT-KVOI-TOT-ONE-YEAR-AGO            
063800       WHEN +2                                                            
063900         MOVE WS-KVOI-TOT-TAB (+2) TO UT-KVOI-TOT-CURRENT-YEAR            
064000       END-EVALUATE                                                       
064100                                                                          
064200       ADD +1                TO IX-AR                                     
064300     END-PERFORM                                                          
064400     .                                                                    
034300     EJECT                                                                
034400 D-SKAPA-FIL SECTION.                                                     
034500                                                                          
034600     MOVE IDARTNR-WS          TO W-IDARTNR                                
034700                                                                          
034800                                                                          
034900     MOVE ART-KDPRODSL        TO TEST-KDPRODSL                            
035000     IF KDPRODSL-VCBV       OR                                            
035100        KDPRODSL-BIMA       OR                                            
035200        KDPRODSL-LOCAL      OR                                            
035300        KDPRODSL-ACC        OR                                            
035400        KDPRODSL-WHEELS     OR                                            
035500       (IN-IDFKNGRP > 8845 AND IN-IDFKNGRP < 8849) OR                     
035600       (CLAG-REDIRLEV = 1)                                                
035700        MOVE NEJ              TO UT-FLCLART                               
035800     ELSE                                                                 
035900********TILL CLASSIC  KDPRODSL = 11  14,17                                
036000        MOVE JA               TO UT-FLCLART                               
036100     END-IF                                                               
036200********************************************                              
036300                                                                          
036400     MOVE NEJ                 TO SW-KAMPANJ                               
036500     PERFORM DEC-KOLLA-KAMPANJ-DB2                                        
036600     IF SW-KAMPANJ = JA                                                   
036700        MOVE JA               TO UT-FLKAMP                                
036800     ELSE                                                                 
036900        MOVE NEJ              TO UT-FLKAMP                                
037000     END-IF                                                               
037100                                                                          
037200                                                                          
037300***  IF ?                                                                 
037400        MOVE IN-IDANSK        TO UT-IDANSK                                
037500        MOVE IN-IDARTNR       TO UT-IDARTNR                               
037600        MOVE IN-IDLEVNR       TO UT-IDLEVNR                               
037700        MOVE IN-IDFKNGRP      TO UT-IDFKNGRP                              
037800        MOVE IN-KDPRODSL      TO UT-KDPRODSL                              
037900        MOVE IN-TIFINLV       TO UT-TIFINLV                               
038000        MOVE IN-TIURPROD      TO UT-TIURPROD                              
038100        MOVE IN-KVLS          TO UT-KVLS                                  
038300        MOVE IN-KDERS         TO UT-KDERS                                 
038400        MOVE IN-BEART         TO UT-BEART                                 
038500                                                                          
038600        PERFORM S11-SKRIV-W26124                                          
038700***  END-IF                                                               
038800                                                                          
038900     .                                                                    
039000     EJECT                                                                
039100 DEC-KOLLA-KAMPANJ-DB2 SECTION.                                           
039200                                                                          
039300     PERFORM DB2-DCL-OPN-TP1ARTK-CRS                                      
039400                                                                          
039500     MOVE SQLCODE TO SQLCODE-WS                                           
039600     IF SQLCODE-WS = ZERO                                                 
039700***     READ TP1ARTK AND TP1KAMP                                          
039800        PERFORM DB2-FETCH-TP1ARTK-CRS                                     
039900        IF LINES-FOUND                                                    
040000           MOVE JA   TO SW-KAMPANJ                                        
040100        ELSE                                                              
040200           CONTINUE                                                       
040300        END-IF                                                            
040400     END-IF                                                               
040500                                                                          
040600     PERFORM DB2-CLOSE-TP1ARTK-CRS                                        
040700     .                                                                    
040800     EJECT                                                                
040900 Z-FINIT SECTION.                                                         
041000                                                                          
041100                                                                          
041200     CLOSE W26123                                                         
041300           W26124                                                         
041400     SKIP2                                                                
041500     MOVE 'S'        TO POSTSUM-OPKOD                                     
041600     CALL POSTSUM USING POSTSUM-PARM                                      
041700     .                                                                    
041800     EJECT                                                                
041900 S01-LAES-W26123  SECTION.                                                
042000     SKIP2                                                                
042100     READ W26123 INTO IN-AREA                                             
042200     AT END                                                               
042300        SET END-OF-W26123 TO TRUE                                         
042400                                                                          
042500     NOT AT END                                                           
042600        MOVE 'W26123'   TO POSTSUM-FDNAMN                                 
042700        MOVE 'W26123D1' TO POSTSUM-DDNAMN2                                
042800        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
042900        CALL POSTSUM USING POSTSUM-PARM                                   
043000                                                                          
043100     END-READ                                                             
043200     .                                                                    
043300     EJECT                                                                
043400 S11-SKRIV-W26124 SECTION.                                                
043500                                                                          
043600     IF WS-ANTAL-RAD = ZERO                                               
043700        WRITE UT-POST FROM W26124-RUBRIK                                  
043800        MOVE +1      TO WS-ANTAL-RAD                                      
043900     END-IF                                                               
044000                                                                          
044100     WRITE UT-POST FROM UT-AREA                                           
044200                                                                          
044300     MOVE 'UT'       TO POSTSUM-TRANSTYP                                  
044400     MOVE 'W26124'   TO POSTSUM-FDNAMN                                    
044500     MOVE 'W26123D2' TO POSTSUM-DDNAMN2                                   
044600     CALL POSTSUM USING POSTSUM-PARM                                      
044700     ADD +1          TO WS-ANTAL-RAD                                      
044800     .                                                                    
044900     EJECT                                                                
045000* --- IMS SEKTIONER ---                                                   
045100                                                                          
045200 IMS-GET-WDK601 SECTION.                                                  
045300                                                                          
045400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
045500          DELIMITED BY SIZE INTO SSA1                                     
045600     MOVE '  GE' TO GODK-STATUSKODER                                      
045700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
045800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
045900     PERFORM IMS-STATUSKONTROLL                                           
046000     .                                                                    
046100     EJECT                                                                
046200 IMS-GET-WDK611 SECTION.                                                  
046300                                                                          
046400     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
046500          DELIMITED BY SIZE INTO SSA1                                     
046600     MOVE '  GE' TO GODK-STATUSKODER                                      
046700     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK611 SSA1                  
046800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
046900     PERFORM IMS-STATUSKONTROLL                                           
047000     .                                                                    
047100     SKIP3                                                                
047200 IMS-GU-WDL811 SECTION.                                                   
047300                                                                          
047400     STRING 'WDL801  (IDARTNR  =' W-IDARTNR-X ')'                         
047500          DELIMITED BY SIZE INTO SSA1                                     
047600     STRING 'WDL811  (TIAAAA   =' W-TIAAAA-X ')'                          
047700          DELIMITED BY SIZE INTO SSA2                                     
047800     MOVE '  GE' TO GODK-STATUSKODER                                      
047900     CALL CBLTDLI USING GU WDL8-PCB DLI-IO-WDL811 SSA1 SSA2               
048000     MOVE WDL8-STATUS-CODE TO STATUS-WS                                   
048100     PERFORM IMS-STATUSKONTROLL                                           
048200     .                                                                    
048300     EJECT                                                                
048400 IMS-STATUSKONTROLL SECTION.                                              
048500     SKIP2                                                                
048600     SET STATUS-IX TO 1                                                   
048700     SEARCH GODK-STATUS                                                   
048800       AT END                                                             
048900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
049000           DELIMITED BY SIZE INTO FELTEXT-STR                             
049100         DISPLAY FELTEXT-STR                                              
049200         CALL FELLOG                                                      
049300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
049400         CONTINUE                                                         
049500     END-SEARCH                                                           
049600     .                                                                    
049700     EJECT                                                                
049700                                                                          
049700 IMS-GU-WDJ101   SECTION.                                                 
049700                                                                          
049700     STRING 'WDJ101  (IDARTNR  =' W-IDARTNR-X ')'                         
049700          DELIMITED BY SIZE INTO SSA1                                     
049700     MOVE '  GE'  TO GODK-STATUSKODER                                     
049700     CALL CBLTDLI USING GU WDJ1-PCB DLI-IO-WDJ101 SSA1                    
049700     MOVE WDJ1-STATUS-CODE TO STATUS-WS                                   
049700     PERFORM IMS-STATUSKONTROLL                                           
049700     .                                                                    
049700     EJECT                                                                
049700                                                                          
094700 IMS-GU-WDC101 SECTION.                                                   
094800     STRING 'WDC101  (WDC101KY =' W-WDC101KY-X ')'                        
094900          DELIMITED BY SIZE INTO SSA1                                     
095000     MOVE '  GE' TO GODK-STATUSKODER                                      
095100     CALL CBLTDLI USING GU WDC1-PCB DLI-IO-WDC101 SSA1                    
095200     MOVE WDC1-STATUS-CODE TO STATUS-WS                                   
095300     PERFORM IMS-STATUSKONTROLL                                           
095400     .                                                                    
095500     EJECT                                                                
049700                                                                          
089900 IMS-GU-WDK601 SECTION.                                                   
090200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
090300          DELIMITED BY SIZE INTO SSA1                                     
090400     MOVE '  GE' TO GODK-STATUSKODER                                      
090500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
090600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
090700     PERFORM IMS-STATUSKONTROLL                                           
090800     .                                                                    
090900     EJECT                                                                
049700                                                                          
091000 IMS-GNP-WDK611 SECTION.                                                  
091300     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
091400          DELIMITED BY SIZE INTO SSA1                                     
091500     MOVE '  GE' TO GODK-STATUSKODER                                      
091600     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
091700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
091800     PERFORM IMS-STATUSKONTROLL                                           
091900     .                                                                    
092000     EJECT                                                                
049700                                                                          
092100 IMS-GNP-WDK627 SECTION.                                                  
092200*                      LÄSNING AV SKROTNINGSSEGMENT WDK627                
092600     MOVE   'WDK627   '          TO SSA1                                  
092700     MOVE   '  GE'               TO GODK-STATUSKODER                      
092900     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK627 SSA1                   
093100     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
093200     PERFORM IMS-STATUSKONTROLL                                           
091900     .                                                                    
093400     EJECT                                                                
095600                                                                          
049800 DB2-DCL-OPN-TP1ARTK-CRS  SECTION.                                        
049900     MOVE 'DB2-DCL-OPN-TP1ARTK   ' TO  WS-DB2-SEKTION                     
050000                                                                          
050100     MOVE 000100  TO GOOD-SQLCODECODES                                    
050200                                                                          
050300     EXEC SQL                                                             
050400         DECLARE TP1ARTK-CRS CURSOR FOR                                   
050500           SELECT  A.IDKAMP                                               
050600                  ,A.IDARTNR                                              
050700                  ,B.TISTADAT_KAMP                                        
050800                  ,B.TISTODAT_KAMP                                        
050900                  ,B.KDKAMP                                               
051000                  ,B.IDKAMP_GRP                                           
051100                                                                          
051200           FROM    TP1ARTK A                                              
051300                  ,TP1KAMP B                                              
051400                                                                          
051500           WHERE   A.IDARTNR = :W-IDARTNR                                 
051600               AND A.IDKAMP  =  B.IDKAMP                                  
051700                                                                          
051800           ORDER BY B.IDKAMP                                              
051900     END-EXEC                                                             
052000                                                                          
052100     MOVE 000100  TO GOOD-SQLCODECODES                                    
052200     EXEC SQL OPEN TP1ARTK-CRS END-EXEC                                   
052300     .                                                                    
052400     SKIP3                                                                
052500 DB2-FETCH-TP1ARTK-CRS  SECTION.                                          
052600     MOVE 'DB2-FETCH-TP1ARTK   ' TO  WS-DB2-SEKTION                       
052700     SKIP2                                                                
052800     MOVE 000100  TO GOOD-SQLCODECODES                                    
052900     EXEC SQL                                                             
053000         FETCH TP1ARTK-CRS INTO                                           
053100                    :TP1KAMP-IDKAMP                                       
053200                   ,:TP1ARTK-IDARTNR                                      
053300                   ,:TP1KAMP-TISTADAT-KAMP                                
053400                   ,:TP1KAMP-TISTODAT-KAMP                                
053500                   ,:TP1KAMP-KDKAMP                                       
053600                   ,:TP1KAMP-IDKAMP-GRP                                   
053700     END-EXEC                                                             
053800                                                                          
053900     MOVE SQLCODE TO SQLCODE-WS                                           
054000     PERFORM DB2-STATUS-CHECK                                             
054100     .                                                                    
054200     SKIP3                                                                
054300 DB2-CLOSE-TP1ARTK-CRS  SECTION.                                          
054400     MOVE 'DB2-CLOSE-TP1ARTK   ' TO  WS-DB2-SEKTION                       
054500                                                                          
054600     EXEC SQL CLOSE TP1ARTK-CRS END-EXEC                                  
054700     .                                                                    
054800     EJECT                                                                
054900 DB2-STATUS-CHECK  SECTION.                                               
055000                                                                          
055100     SET SQLCODE-IX TO 1                                                  
055200     SEARCH GOOD-SQLCODE                                                  
055300       AT END                                                             
055400          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
055500          DELIMITED BY SIZE INTO FELTEXT-STR                              
055600          DISPLAY FELTEXT-STR                                             
055700          CALL FELLOG                                                     
055800       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
055900     END-SEARCH                                                           
056000     .                                                                    
