000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4183B00.                                                
000400 AUTHOR.         OLSSON SUSANNE.                                          
000500 DATE-WRITTEN.   08/08/19.                                                
000600 DATE-COMPILED.                                                           
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        PROGRAMMET LÄSER IN DAGENS RETURER KOD 72,98 OCH R-O-R           
001000*        SOM SKALL FAKTURERAS FÖR HANDLING COST.OMVANDLAR DISTRIKT        
001100*        OCH KUND TILL PARMA-ID. SKAPAR EN UT-FIL MED DELETE-DAT.         
001200*        OCH EN FIL MED DE RADER SOM SKALL FAKTURERAS EFTER ATT           
001300*        MAN LÄST TABELL TP8SRET OCH SETT ATT PARMA-ID OCH KOD            
001400*        FINNS.DENNA FIL ANVÄNDS VID LOAD UTILLITY AV TP8LRET.            
001500*                                                                         
001600*        INFILER: W418.W418D2.W418AT (3 GEN)                              
001700*                 W418.W418D4.W418AV (+0)                                 
001800*                                                                         
001900*        BMP                                                              
002000*                                                                         
002100*        PROGRAMMET LÄSER      TABELL TP8TRET                             
002200*        PROGRAMMET LÄSER      WDB2                                       
002300*                                                                         
002400*    E'TRACKER: 880053  DATED 2008-08                                     
002500*                                                                         
002600*    ABENDKODER:                                                          
002700*        U0016 -  . . . .                                                 
002800*        U1000 -  . . . .                                                 
002900*                                                                         
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     SKIP2                                                                
003400 INPUT-OUTPUT SECTION.                                                    
003500                                                                          
003600 FILE-CONTROL.                                                            
003700     SKIP2                                                                
003800*          --- HANDLING FEE RADER KOD 72,98 OCH RETUR-AV-RETUR            
003900     SELECT INFIL                      ASSIGN TO W4183BD1.                
004000     SKIP2                                                                
004100*          --- UTFILER                                                    
004200*                                                                         
004300*          --- FIL MED DELETE-DATUM                                       
004400     SELECT W418AW                     ASSIGN TO W4183BD2.                
004500     SKIP2                                                                
004600*          --- RADER SOM SKALL LADDA TABELL TP8LRET                       
004700     SELECT W418AX                     ASSIGN TO W4183BD3.                
004800     EJECT                                                                
004900*          --- RADER SOM SKALL MED MAILSKICK TILL KUND                    
005000     SELECT W418AY                     ASSIGN TO W4183BD4.                
005100     EJECT                                                                
005200 DATA DIVISION.                                                           
005300     SKIP3                                                                
005400 FILE SECTION.                                                            
005500     SKIP3                                                                
005600 FD  INFIL                                                                
005700     RECORDING       F                                                    
005800     BLOCK CONTAINS  0.                                                   
005900                                                                          
006000*01  -COPY W418FEE       -L.                                              
006100     SKIP3                                                                
006200                                                                          
006300 FD  W418AW                                                               
006400     RECORDING       F                                                    
006500     BLOCK CONTAINS  0.                                                   
006600                                                                          
006700 01  UT1-POST     PIC X(80).                                              
006800     SKIP3                                                                
006900                                                                          
007000 FD  W418AX                                                               
007100     RECORDING       F                                                    
007200     BLOCK CONTAINS  0.                                                   
007300                                                                          
007400*01  POST -COPY TP8LRET  -PRE UT2-    -L.                                 
007500     EJECT                                                                
007600                                                                          
007700 FD  W418AY                                                               
007800     RECORDING       V                                                    
007900     BLOCK CONTAINS  0.                                                   
008000                                                                          
008100 01  UT3-POST     PIC X(83).                                              
008200     EJECT                                                                
008300 WORKING-STORAGE SECTION.                                                 
008400                                                                          
008500 77  IDPGM                       PIC X(8)    VALUE 'W4183B00'.            
008600 77  JA                          PIC X       VALUE 'J'.                   
008700 77  NEJ                         PIC X       VALUE 'N'.                   
008800 77  WS-DAREGDAT                 PIC X(8)    VALUE SPACE.                 
008900 77  SPAR-IDREF                  PIC X(15)   VALUE SPACE.                 
009000                                                                          
009100 77  INFIL-EOF-SW                PIC X       VALUE 'N'.                   
009200     88  END-OF-INFIL                        VALUE 'J'.                   
009300                                                                          
009400 77  SKRIV-W418AX-SW             PIC X       VALUE 'J'.                   
009500     88  SKRIV-W418AX-OK                     VALUE 'J'.                   
009600     88  SKRIV-W418AX-NOT-OK                 VALUE 'N'.                   
009700                                                                          
009800 77  SKRIV-W418AY-SW             PIC X       VALUE 'J'.                   
009900     88  SKRIV-W418AY-OK                     VALUE 'J'.                   
010000     88  SKRIV-W418AY-NOT-OK                 VALUE 'N'.                   
010100                                                                          
010110 01  W-KDKUNDKAT                 PIC 9(2)    VALUE 0.                     
010200     EJECT                                                                
010300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
010400 01  FILLER REDEFINES DAGENS-DATUM.                                       
010500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
010600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
010700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
010800     EJECT                                                                
010900 01  DYNAMISKA-SUBPROGRAM.                                                
011000*                                                                         
011100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
011200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
011500     03  W009CIA                 PIC X(8)    VALUE 'W009CIA'.             
011600     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
011700     SKIP2                                                                
011800*    --- PARAMETRAR TILL ABEND                                            
011900                                                                          
012000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
012100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
012200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
012300 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
012400     SKIP2                                                                
012500 01  FELTEXT.                                                             
012600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
012700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
012800     EJECT                                                                
012900*    --- PARAMETRAR TILL POSTSUM                                          
013000*                                                                         
013100*01  -COPY W0005   -PRE  POSTSUM-                                         
013200     EJECT                                                                
013300                                                                          
013400*    --- PARAMETRAR TILL WZ20DAYS                                         
013500*    -COPY WZ20DAYS                                                       
013600     EJECT                                                                
013700                                                                          
013800*    --- PARAMETRAR TILL W009CIA                                          
013900*01  -COPY W009CIA                                                        
014000     EJECT                                                                
014100 01  IN-AREA-START              PIC X(24)   VALUE                         
014200                                 'IN-AREA-START  '.                       
014300     SKIP2                                                                
014400                                                                          
014500*01  AREA -COPY W418FEE     -PRE IN-                                      
014600     EJECT                                                                
014700 01  UT1-AREA-START              PIC X(24)   VALUE                        
014800                                 'UT1-AREA-START  '.                      
014900     SKIP2                                                                
015000 01  FILLER                      PIC X(16) VALUE 'UT1-AREA'.              
015100 01  UT1-AREA.                                                            
015200     03 WS-FILLER1               PIC X(8)  VALUE SPACE.                   
015300     03 WS-DAEXDAT               PIC X(4) VALUE " < '".                   
015400     03 WS-DELDATUM              PIC X(8)  VALUE SPACE.                   
015500     03 WS-FILLER2               PIC X(60) VALUE "')".                    
015600     EJECT                                                                
015700                                                                          
015800 01  UT2-AREA-START              PIC X(24)   VALUE                        
015900                                 'UT2-AREA-START  '.                      
016000     SKIP2                                                                
016100*01  AREA -COPY TP8LRET     -PRE UT2-                                     
016200     EJECT                                                                
016300                                                                          
016400 01  UT3-AREA-START              PIC X(24)   VALUE                        
016500                                 'UT3-AREA-START  '.                      
016600     SKIP2                                                                
016700 01  FILLER                      PIC X(16) VALUE 'UT3-AREA'.              
016800 01  UT3-AREA.                                                            
016900     03 UT3-IDPARTNR             PIC X(9)  VALUE SPACE.                   
017000     03 FILLER                   PIC X(1)  VALUE ';'.                     
017100     03 UT3-IDFTG                PIC X(2)  VALUE SPACE.                   
017200     03 FILLER                   PIC X(1)  VALUE ';'.                     
017300     03 UT3-KDANMORS             PIC X(2)  VALUE SPACE.                   
017400     03 FILLER                   PIC X(1)  VALUE ';'.                     
017500     03 UT3-IDDISTR              PIC 9(4)  VALUE ZERO.                    
017600     03 FILLER                   PIC X(1)  VALUE ';'.                     
017700     03 UT3-IDKUNDNR             PIC 9(6)  VALUE ZERO.                    
017800     03 FILLER                   PIC X(1)  VALUE ';'.                     
017900     03 UT3-IDRAPPNR             PIC 9(7)  VALUE ZERO.                    
018000     03 FILLER                   PIC X(1)  VALUE ';'.                     
018100     03 UT3-IDRAPP               PIC X(10) VALUE SPACE.                   
018200     03 FILLER                   PIC X(1)  VALUE ';'.                     
018300     03 UT3-BETEXT               PIC X(35) VALUE SPACE.                   
018400     03 FILLER                   PIC X(1)  VALUE ';'.                     
018500     EJECT                                                                
018600                                                                          
018700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018800*                                                                         
018900     EJECT                                                                
019000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
019100     SKIP3                                                                
019200 01  NYCKLAR-TILL-DLI.                                                    
019300     03  W-IDGMT-X.                                                       
019400         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
019500         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
019600                                                                          
019700     03  W-IDGMT-MIN-X.                                                   
019800         05  W-IDDISTR-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
019900         05  W-IDKUNDNR-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
020000                                                                          
020100     03  W-IDGMT-MAX-X.                                                   
020200         05  W-IDDISTR-MAX       PIC S9(5)   VALUE ZERO COMP-3.           
020300         05  W-IDKUNDNR-MAX      PIC S9(7)   VALUE ZERO COMP-3.           
020400                                                                          
020500     03  W-IDPARTNR-X.                                                    
020600         05 W-IDPARTNR           PIC X(9)    VALUE SPACE.                 
020700         05 W-IDFTG              PIC X(2)    VALUE SPACE.                 
020800                                                                          
020900     03  W-KDANMORS-X.                                                    
021000         05 W-KDANMORS           PIC X(2)    VALUE SPACE.                 
021100                                                                          
021200     SKIP2                                                                
021300*    --- STATUS-KOD FRÅN IMS                                              
021400 01  STATUS-WS                   PIC XX.                                  
021500     88  SEGMENT-FINNS                       VALUE '  '.                  
021600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
021700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
021800     SKIP2                                                                
021900 01  GODK-STATUSKODER.                                                    
022000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
022100     SKIP3                                                                
022200 01  SSA1                        PIC X(64).                               
022300 01  SSA2                        PIC X(64).                               
022400     EJECT                                                                
022500*    --- IMS FUNKTIONSKODER                                               
022600*01  -COPY W0003                                                          
022700     EJECT                                                                
022800 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
022900       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
023000                                                                          
023100 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
023200 01  DB2-WS.                                                              
023300     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
023400         88  CURSOR-OK                       VALUE 000.                   
023500         88  RADER-FINNS                     VALUE 000.                   
023600         88  RADER-SAKNAS                    VALUE 100.                   
023700         88  ATKOMST-FEL                     VALUE 904.                   
023800                                                                          
023900     03  GODK-SQLCODEKODER.                                               
024000         05  GODK-SQLCODE OCCURS 5                                        
024100             INDEXED BY SQLCODE-IX PIC 9(3).                              
024200*                                                                         
024300*                                                                         
024400*    ---  DB2 HOST-COPYTEXTER                                             
024500     EJECT                                                                
024600 01  FILLER                      PIC X(16)  VALUE 'TP8TRET-AREA'.         
024700*01  -COPY TP8TRET -PRE TRET-                                             
024800 01  FILLER                      PIC X(16)   VALUE 'TP8TRET-DCL '.        
024900        EXEC SQL INCLUDE TP8TRET  END-EXEC.                               
025000                                                                          
025100     EJECT                                                                
025200*    ---  DLI INPUT-OUTPUT AREA                                           
025300                                                                          
025400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
025500 01  DLI-IO-WDB201.                                                       
025600*    03  -COPY WDB201                                                     
025700     EJECT                                                                
025800 LINKAGE SECTION.                                                         
025900                                                                          
026000*01  -COPY W0009  -PRE MSG-                                               
026100     EJECT                                                                
026200                                                                          
026300*01  -COPY W0008  -PRE WDB2-                                              
026400     05  FILLER                  PIC X.                                   
026500     EJECT                                                                
026600                                                                          
026700 PROCEDURE DIVISION  USING MSG-PCB WDB2-PCB.                              
026800 MAIN SECTION.                                                            
026900     ENTRY 'DLITCBL' USING MSG-PCB WDB2-PCB.                              
027000     SKIP2                                                                
027100                                                                          
027200     PERFORM A-INIT                                                       
027300     PERFORM S01-LAES-INFIL                                               
027400     PERFORM UNTIL END-OF-INFIL                                           
027500                                                                          
027600       PERFORM B-BEHANDLA-POSTER                                          
027700                                                                          
027800       PERFORM S01-LAES-INFIL                                             
027900     END-PERFORM                                                          
028000                                                                          
028100     PERFORM C-SKAPA-DELETE-DATE                                          
028200                                                                          
028300     PERFORM Z-FINIT                                                      
028400                                                                          
028500     MOVE ZERO TO RETURN-CODE                                             
028600     GOBACK                                                               
028700     .                                                                    
028800     EJECT                                                                
028900                                                                          
029000 A-INIT SECTION.                                                          
029100     OPEN INPUT  INFIL                                                    
029200                                                                          
029300     OPEN OUTPUT W418AW                                                   
029400                 W418AX                                                   
029500                 W418AY                                                   
029600     SKIP2                                                                
029700     ACCEPT DAGENS-DATUM  FROM DATE                                       
029800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
029900                                                                          
030000     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DAREGDAT                       
030100                                                                          
030200     MOVE LOW-VALUE         TO W-IDGMT-MIN-X                              
030300     MOVE HIGH-VALUE        TO W-IDGMT-MAX-X                              
030400                                                                          
030500     INITIALIZE GODK-SQLCODEKODER                                         
030600     .                                                                    
030700     EJECT                                                                
030800                                                                          
030900 B-BEHANDLA-POSTER SECTION.                                               
031000     MOVE JA  TO  SKRIV-W418AX-SW                                         
031100                                                                          
031200     MOVE IN-RP-IDDISTR    TO W-IDDISTR                                   
031300     MOVE IN-RP-IDKUNDNR   TO W-IDKUNDNR                                  
031400                                                                          
031410     MOVE ZEROES           TO W-KDKUNDKAT                                 
031500     PERFORM IMS-GU-WDB201                                                
031600     IF SEGMENT-SAKNAS                                                    
031700       PERFORM IMS-GU-WDB201-MIN-MAX                                      
031800     END-IF                                                               
031900                                                                          
032000     IF SEGMENT-FINNS                                                     
032001       MOVE GMT-KDKUNDKAT                       TO W-KDKUNDKAT            
032002*** ECOM - NO HANDLING FEE                                                
032100       IF IN-RP-IDPTYP = 'ROR'                                            
032200         MOVE 'RR'                          TO UT2-KDANMORS               
032300                                               W-KDANMORS                 
032400                                                                          
032500         MOVE SPACE                         TO UT2-IDREF                  
032600         MOVE IN-RP-IDRAPP                  TO UT2-IDREF                  
032700                                                                          
032800         MOVE IN-RP-DARFSDAT                TO UT2-DAREFDAT               
032900         MOVE 'HANDL.FEE RETURN-O-RETURN'   TO UT2-BEART                  
033000         MOVE IN-RP-KVRADER-RET             TO UT2-KVLEVART               
033100                                               UT2-KVBEART                
033200         PERFORM BA-FLYTTA-DATA                                           
033300       ELSE                                                               
033400         MOVE SPACE                         TO SPAR-IDREF                 
033500                                                                          
033600         MOVE 'VO'                          TO CIA-IDARTPRE-IN            
033700         MOVE IN-RP-IDRAPPNR                TO CIA-IDARTBET-IN            
033800         CALL W009CIA USING                    CIA-W009CIA                
033900         MOVE CIA-IDARTBET-UT               TO SPAR-IDREF                 
034000                                                                          
034100         IF IN-RP-KVRADER-72 > ZERO                                       
034200           MOVE '72'                        TO UT2-KDANMORS               
034300                                               W-KDANMORS                 
034400                                                                          
034500           MOVE SPACE                       TO UT2-IDREF                  
034600           MOVE SPAR-IDREF                  TO UT2-IDREF                  
034700                                                                          
034800           MOVE IN-RP-DARETANK              TO UT2-DAREFDAT               
034900           MOVE 'HANDLING FEE FOR CODE 72 ' TO UT2-BEART                  
035000           MOVE IN-RP-KVRADER-72            TO UT2-KVLEVART               
035100                                               UT2-KVBEART                
035200                                                                          
035300           PERFORM BA-FLYTTA-DATA                                         
035400         END-IF                                                           
035500         IF IN-RP-KVRADER-98 > ZERO                                       
035600           MOVE '98'                        TO UT2-KDANMORS               
035700                                               W-KDANMORS                 
035800           MOVE SPACE                       TO UT2-IDREF                  
035900           MOVE SPAR-IDREF                  TO UT2-IDREF                  
036000                                                                          
036100           MOVE IN-RP-DARETANK              TO UT2-DAREFDAT               
036200           MOVE 'HANDLING FEE FOR CODE 98 ' TO UT2-BEART                  
036300           MOVE IN-RP-KVRADER-98            TO UT2-KVLEVART               
036400                                               UT2-KVBEART                
036500                                                                          
036600           PERFORM BA-FLYTTA-DATA                                         
036700         END-IF                                                           
036800       END-IF                                                             
036900     ELSE                                                                 
037000*--- FINNS INGET PARMANUMMER TILL DISTRIKTET                              
037100       MOVE '?????'           TO UT3-IDPARTNR                             
037200       MOVE '??'              TO UT3-IDFTG                                
037300       MOVE '??'              TO UT3-KDANMORS                             
037400       MOVE IN-RP-IDDISTR     TO UT3-IDDISTR                              
037500       MOVE IN-RP-IDKUNDNR    TO UT3-IDKUNDNR                             
037600       MOVE IN-RP-IDRAPPNR    TO UT3-IDRAPPNR                             
037700       MOVE IN-RP-IDRAPP      TO UT3-IDRAPP                               
037800       MOVE 'NO PARMA.NO FOR DISTRICT NUMBER' TO UT3-BETEXT               
037900       PERFORM  S13-SKRIV-W418AY                                          
038000     END-IF                                                               
038100     .                                                                    
038200     EJECT                                                                
038300                                                                          
038400 BA-FLYTTA-DATA  SECTION.                                                 
038500     MOVE GMT-IDPARTNR               TO UT2-IDPARTNR                      
038600                                        W-IDPARTNR                        
038700                                                                          
038800     MOVE '57'                       TO UT2-IDFTG                         
038900                                        W-IDFTG                           
039000                                                                          
039100     MOVE SPACE                      TO UT2-IDEXCUST-1                    
039200                                                                          
039300     MOVE 'VO'                       TO CIA-IDARTPRE-IN                   
039400     MOVE IN-RP-IDDISTR              TO CIA-IDARTBET-IN                   
039500     CALL W009CIA USING                 CIA-W009CIA                       
039600     MOVE CIA-IDARTBET-UT            TO UT2-IDEXCUST-1                    
039700                                                                          
039800     MOVE SPACE                      TO UT2-IDEXCUST-2                    
039900                                                                          
040000     IF IN-RP-IDKUNDNR = ZERO                                             
040100       MOVE '0'                      TO UT2-IDEXCUST-2                    
040200     ELSE                                                                 
040300       MOVE 'VO'                     TO CIA-IDARTPRE-IN                   
040400       MOVE IN-RP-IDKUNDNR           TO CIA-IDARTBET-IN                   
040500       CALL W009CIA USING               CIA-W009CIA                       
040600       MOVE CIA-IDARTBET-UT          TO UT2-IDEXCUST-2                    
040700     END-IF                                                               
040800                                                                          
040900     MOVE FUNCTION CURRENT-DATE(1:8) TO UT2-DAREGDAT                      
041000     MOVE 'SE'                       TO UT2-IDLANDX3-SEND                 
041100                                                                          
041200     PERFORM DB2-SELECT-TP8TRET-TAB                                       
041300     IF RADER-FINNS                                                       
043000        PERFORM BAA-MOVE-PRICE-UT2                                        
043100     ELSE                                                                 
043110        MOVE 0                       TO W-IDKUNDNR                        
043120        PERFORM DB2-SELECT-TP8TRET-TAB                                    
043121        MOVE IN-RP-IDKUNDNR          TO W-IDKUNDNR                        
043130        IF RADER-FINNS                                                    
043140           PERFORM BAA-MOVE-PRICE-UT2                                     
043150        ELSE                                                              
043200**** KOMBINATIONEN PARMANUMMER OCH KDANMORS EJ UPPLAGD                    
043300          MOVE NEJ                      TO  SKRIV-W418AX-SW               
043400          MOVE W-IDPARTNR               TO UT3-IDPARTNR                   
043500          MOVE W-IDFTG                  TO UT3-IDFTG                      
043600          MOVE IN-RP-IDDISTR            TO UT3-IDDISTR                    
043700          MOVE IN-RP-IDKUNDNR           TO UT3-IDKUNDNR                   
043800          MOVE IN-RP-IDRAPPNR           TO UT3-IDRAPPNR                   
043900          MOVE IN-RP-IDRAPP             TO UT3-IDRAPP                     
044000          MOVE W-KDANMORS               TO UT3-KDANMORS                   
044110          MOVE 'COMBO DIST/CUST/CODE MISSING' TO UT3-BETEXT               
044200          PERFORM  S13-SKRIV-W418AY                                       
044210        END-IF                                                            
044300     END-IF                                                               
044400                                                                          
044500     MOVE 'SE'                       TO UT2-IDLANDX3-SEND                 
044600     MOVE SPACE                      TO UT2-IDOPTION-1                    
044700                                        UT2-IDOPTION-2                    
044800                                        UT2-IDOPTION-3                    
044900     MOVE IN-RP-IDDC                 TO UT2-IDDC                          
045000     MOVE SPACE                      TO UT2-IDUSER-1                      
045100                                        UT2-IDUSER-2                      
045101     MOVE 'W4K'                      TO UT2-IDSYSTEM-SEND                 
045102                                        UT2-IDSYSTEM-REC                  
045110     IF W-KDKUNDKAT = 18                                                  
045120        MOVE 'ECOM'                  TO UT2-IDSYSTEM-SEND                 
045140     END-IF                                                               
045150                                                                          
045400     MOVE '00000000'                 TO UT2-DAUPPDAT                      
045500                                        UT2-DADELDAT                      
045600                                        UT2-DAFAKT                        
045700     MOVE +0                         TO UT2-IDFINDOC                      
045800     MOVE 'W '                       TO UT2-KDRAPPSTA                     
045900                                                                          
046000     IF SKRIV-W418AX-SW = JA                                              
046100       PERFORM  S12-SKRIV-W418AX                                          
046200     END-IF                                                               
046300                                                                          
046400     PERFORM DB2-SELECT-TP8TRET-TAB                                       
046500     IF RADER-FINNS                                                       
046600       CONTINUE                                                           
046700     ELSE                                                                 
046710       MOVE 0                 TO W-IDKUNDNR                               
046720       PERFORM DB2-SELECT-TP8TRET-TAB                                     
046721       MOVE IN-RP-IDKUNDNR    TO W-IDKUNDNR                               
046730       IF RADER-FINNS                                                     
046740         CONTINUE                                                         
046750       ELSE                                                               
046800**** MINIMIVÄRDE SAKNAS I TABELLEN TP8FRET                                
046900         MOVE W-IDPARTNR        TO UT3-IDPARTNR                           
047000         MOVE W-IDFTG           TO UT3-IDFTG                              
047100         MOVE IN-RP-IDDISTR     TO UT3-IDDISTR                            
047200         MOVE IN-RP-IDKUNDNR    TO UT3-IDKUNDNR                           
047300         MOVE IN-RP-IDRAPPNR    TO UT3-IDRAPPNR                           
047400         MOVE IN-RP-IDRAPP      TO UT3-IDRAPP                             
047500         MOVE W-KDANMORS        TO UT3-KDANMORS                           
047601         MOVE 'MISSING MIN VALUE FOR DIST/CUST/CODE'                      
047610                                 TO UT3-BETEXT                            
047700         PERFORM  S13-SKRIV-W418AY                                        
047710       END-IF                                                             
047800     END-IF                                                               
047900     .                                                                    
048000     EJECT                                                                
048100                                                                          
048110 BAA-MOVE-PRICE-UT2 SECTION.                                              
048111     MOVE TRET-PRARTNTO            TO UT2-PRARTNTO                        
048112                                      UT2-PRARTBTO                        
048113     MOVE 'SEK'                    TO UT2-KDVALISO                        
048114                                                                          
048115     IF GMT-FLLDCKND = JA                                                 
048116       IF TRET-FLINVLDC = NEJ                                             
048117         MOVE NEJ                  TO SKRIV-W418AX-SW                     
048118       ELSE                                                               
048119         MOVE TRET-PRARTNTO-LDC    TO UT2-PRARTNTO                        
048120                                      UT2-PRARTBTO                        
048121       END-IF                                                             
048122     END-IF                                                               
048123                                                                          
048124     IF TRET-FLINVFEE = NEJ                                               
048125       MOVE NEJ                    TO  SKRIV-W418AX-SW                    
048126     END-IF                                                               
048127     .                                                                    
048130     EJECT                                                                
048140                                                                          
048200 C-SKAPA-DELETE-DATE SECTION.                                             
048300*- TAR FRAM DELETE-DATUM SOM ÄR 6 MÅNADER TIDIGARE ÄN DAGENS DATUM        
048400     MOVE WS-DAREGDAT             TO DAYS-TIDATE2                         
048500     MOVE 'YYYYMMDD'              TO DAYS-KDDATFMT2                       
048600     MOVE 183                     TO DAYS-KVDAYS                          
048700     MOVE SPACE                   TO DAYS-TIDATE1                         
048800     MOVE 'YYYYMMDD'              TO DAYS-KDDATFMT1                       
048900                                                                          
049000     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
049100                                                                          
049200     IF DAYS-KDRC = ZERO                                                  
049300       MOVE DAYS-TIDATE1(1:8)     TO WS-DELDATUM                          
049400     ELSE                                                                 
049500       DISPLAY ' ERROR IN WZ20DAYS ' DAYS-KDRC                            
049600     END-IF                                                               
049700                                                                          
049800     PERFORM S11-SKRIV-W418AW                                             
049900     .                                                                    
050000     EJECT                                                                
050100                                                                          
050200 Z-FINIT SECTION.                                                         
050300     CLOSE INFIL                                                          
050400           W418AW                                                         
050500           W418AX                                                         
050600           W418AY                                                         
050700     SKIP2                                                                
050800     MOVE 'S' TO POSTSUM-OPKOD                                            
050900     CALL POSTSUM USING POSTSUM-PARM                                      
051000     .                                                                    
051100     EJECT                                                                
051200                                                                          
051300 S01-LAES-INFIL   SECTION.                                                
051400     READ INFIL INTO IN-AREA                                              
051500     AT END                                                               
051600        MOVE HIGH-VALUE TO IN-AREA                                        
051700        SET END-OF-INFIL TO TRUE                                          
051800                                                                          
051900     NOT AT END                                                           
052000        MOVE 'INFIL' TO POSTSUM-FDNAMN                                    
052100        MOVE 'W4183BD1' TO POSTSUM-DDNAMN2                                
052200*       -- ÄNDRA TILL MOVE SPACE OM FILEN SAKNAR POSTTYP                  
052300*       -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES            
052400        MOVE IN-RP-IDPTYP TO POSTSUM-TRANSTYP                             
052500        CALL POSTSUM USING POSTSUM-PARM                                   
052600     END-READ                                                             
052700     .                                                                    
052800     EJECT                                                                
052900                                                                          
053000 S11-SKRIV-W418AW SECTION.                                                
053100     WRITE UT1-POST FROM UT1-AREA                                         
053200                                                                          
053300     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
053400     MOVE 'W418AW' TO POSTSUM-FDNAMN                                      
053500     MOVE 'W4183BD2' TO POSTSUM-DDNAMN2                                   
053600     CALL POSTSUM USING POSTSUM-PARM                                      
053700     .                                                                    
053800     EJECT                                                                
053900                                                                          
054000 S12-SKRIV-W418AX SECTION.                                                
054100     WRITE UT2-POST FROM UT2-AREA                                         
054200                                                                          
054300     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
054400     MOVE 'W418AX' TO POSTSUM-FDNAMN                                      
054500     MOVE 'W4183BD3' TO POSTSUM-DDNAMN2                                   
054600     CALL POSTSUM USING POSTSUM-PARM                                      
054700     .                                                                    
054800     EJECT                                                                
054900                                                                          
055000 S13-SKRIV-W418AY SECTION.                                                
055100     WRITE UT3-POST FROM UT3-AREA                                         
055200                                                                          
055300     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
055400     MOVE 'W418AY' TO POSTSUM-FDNAMN                                      
055500     MOVE 'W4183BD4' TO POSTSUM-DDNAMN2                                   
055600     CALL POSTSUM USING POSTSUM-PARM                                      
055700     .                                                                    
055800     EJECT                                                                
055900                                                                          
056000 S99-ABEND SECTION.                                                       
056100     SKIP2                                                                
056200     MOVE 'S' TO POSTSUM-OPKOD                                            
056300     CALL POSTSUM USING POSTSUM-PARM                                      
056400     CALL ABEND USING RKOD-ABEND                                          
056500     .                                                                    
056600     EJECT                                                                
056700* --- IMS SEKTIONER ---                                                   
056800                                                                          
056900     EJECT                                                                
057000                                                                          
057100 IMS-GU-WDB201 SECTION.                                                   
057200     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
057300          DELIMITED BY SIZE INTO SSA1                                     
057400     MOVE '  GE' TO GODK-STATUSKODER                                      
057500     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
057600     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
057700     PERFORM IMS-STATUSKONTROLL                                           
057800     .                                                                    
057900     EJECT                                                                
058000                                                                          
058100 IMS-GU-WDB201-MIN-MAX SECTION.                                           
058200     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
058300                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
058400          DELIMITED BY SIZE INTO SSA1                                     
058500     MOVE '  GE' TO GODK-STATUSKODER                                      
058600     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
058700     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
058800     PERFORM IMS-STATUSKONTROLL                                           
058900     .                                                                    
059000     EJECT                                                                
059100                                                                          
059200 IMS-STATUSKONTROLL SECTION.                                              
059300     SET STATUS-IX TO 1                                                   
059400     SEARCH GODK-STATUS                                                   
059500       AT END                                                             
059600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
059700           DELIMITED BY SIZE INTO FELTEXT                                 
059800         DISPLAY FELTEXT                                                  
059900         CALL FELLOG                                                      
060000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
060100         CONTINUE                                                         
060200     END-SEARCH                                                           
060300     .                                                                    
060400     EJECT                                                                
060500                                                                          
063110 DB2-SELECT-TP8TRET-TAB  SECTION.                                         
063120     EXEC SQL                                                             
063130         SELECT  PRARTNTO                                                 
063140                ,PRARTNTO_LDC                                             
063150                ,FLINVFEE                                                 
063160                ,FLINVLDC                                                 
063170                                                                          
063180         INTO   :TRET-PRARTNTO                                            
063190               ,:TRET-PRARTNTO-LDC                                        
063191               ,:TRET-FLINVFEE                                            
063192               ,:TRET-FLINVLDC                                            
063193                                                                          
063194         FROM    TP8TRET                                                  
063195                                                                          
063196         WHERE   IDDISTR  = :W-IDDISTR                                    
063197         AND     IDKUNDNR = :W-IDKUNDNR                                   
063198         AND     KDANMORS = :W-KDANMORS                                   
063199                                                                          
063200     END-EXEC                                                             
063201                                                                          
063202     MOVE 000100  TO GODK-SQLCODEKODER                                    
063203     MOVE SQLCODE TO SQLCODE-WS                                           
063204     PERFORM DB2-STATUS-KONTROLL                                          
063205     .                                                                    
063206     EJECT                                                                
063207                                                                          
063210 DB2-STATUS-KONTROLL  SECTION.                                            
063300     SET SQLCODE-IX TO 1                                                  
063400     SEARCH GODK-SQLCODE                                                  
063500       AT END                                                             
063600          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
063700          DELIMITED BY SIZE INTO FELTEXT                                  
063800          CALL ABEND USING RKOD-ABEND-DB2                                 
063900       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
064000     END-SEARCH                                                           
064100     .                                                                    
