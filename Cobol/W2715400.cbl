000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W2715400.                                                
000400 AUTHOR.         STEFAN KIHLBERG.                                         
000500 DATE-WRITTEN.   95/02/06.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*                                                                         
001000*        SKAPAR LISTFIL FÖR DE ARTIKLAR MED RANSONERINGSFAKTOR            
001100*        MINDRE ÄN 0.7, SOM HAR SÅ MYCKET DISPONIBELT PÅ DE OLIKA         
001200*        SDC'ERNA ATT ARTIKLAR KAN TAS HEM TILL CDC FÖR ATT TÄCKA         
001300*        CDC-BEHOV FRAM TILL DISPONIBELDATUM                              
001400*                                                                         
001500*                                                                         
001600*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001700*        PROGRAMMET LÄSER      WLARTM (WDK9)                              
001800*        PROGRAMMET LÄSER      WLARTS (WDK7)                              
001900*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
002000*                                                                         
002100*    ABENDKODER:                                                          
002200*        U0016 -  . . . .                                                 
002300*        U1000 -  . . . .                                                 
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200     SKIP2                                                                
003300*          --- GRUNDFIL WDK9                                              
003400     SELECT W01168                     ASSIGN TO W27154D1.                
003500     SKIP2                                                                
003600*          --- ARTIKLAR MED BRIST I CDC, SENASTE 5 DAGAR                  
003700     SELECT W27154-I                   ASSIGN TO W27154D2.                
003800     SKIP2                                                                
003900*          --- ARTIKLAR PÅ_DAGENS LISTFIL                                 
004000     SELECT W27154-U                   ASSIGN TO W27154D3.                
004100     SKIP2                                                                
004200*          --- LISFIL ARTIKLAR MED BRIST I CDC OCH TILLGÅNG I SDC         
004300     SELECT W27155                     ASSIGN TO W27154D4.                
004400     EJECT                                                                
004500 DATA DIVISION.                                                           
004600     SKIP2                                                                
004700 FILE SECTION.                                                            
004800     SKIP3                                                                
004900 FD  W01168                                                               
005000     RECORDING       F                                                    
005100     BLOCK CONTAINS  0.                                                   
005200*01  -COPY W01168      -L.                                                
005300                                                                          
005400                                                                          
005500 FD  W27154-I                                                             
005600     RECORDING       F                                                    
005700     BLOCK CONTAINS  0.                                                   
005800*01  POST -COPY W27154  -PRE IN54-    -L.                                 
005900                                                                          
006000                                                                          
006100 FD  W27154-U                                                             
006200     RECORDING       F                                                    
006300     BLOCK CONTAINS  0.                                                   
006400*01  POST -COPY W27154   -PRE UT54-   -L.                                 
006500                                                                          
006600                                                                          
006700 FD  W27155                                                               
006800     RECORDING       F                                                    
006900     BLOCK CONTAINS  0.                                                   
007000*01  POST -COPY W27155   -PRE W27155-  -L.                                
007100                                                                          
007200                                                                          
007300     EJECT                                                                
007400 WORKING-STORAGE SECTION.                                                 
007500                                                                          
007600*    -COPY WY2000W3                                                       
007700     SKIP3                                                                
007800*    -COPY WY2000W1                                                       
007900     SKIP3                                                                
008000*    -COPY WWPRODSL                                                       
008100     SKIP3                                                                
008200 77  IDPGM                       PIC X(8)    VALUE 'W2715400'.            
008300 77  JA                          PIC X       VALUE 'J'.                   
008400 77  NEJ                         PIC X       VALUE 'N'.                   
008500                                                                          
008600 77  W01168-EOF-SW               PIC X       VALUE 'N'.                   
008700     88  END-OF-W01168                       VALUE 'J'.                   
008800                                                                          
008900 77  W27154-EOF-SW               PIC X       VALUE 'N'.                   
009000     88  END-OF-W27154                       VALUE 'J'.                   
009100                                                                          
009200 77  ARTIKEL-OK-SW               PIC X       VALUE 'N'.                   
009300     88  ARTIKEL-OK                          VALUE 'J'.                   
009400                                                                          
009500 77  BRIST-CDC-SW                PIC X       VALUE 'N'.                   
009600     88  BRIST-CDC                           VALUE 'J'.                   
009700                                                                          
009800 77  OVERSKOTT-SDC-SW            PIC X       VALUE 'N'.                   
009900     88  OVERSKOTT-SDC                       VALUE 'J'.                   
010000                                                                          
010100     EJECT                                                                
010200                                                                          
010300 01  ARBETSAREOR.                                                         
010400                                                                          
010500     03 WS-IDARTNR              PIC S9(9)      VALUE ZERO COMP-3.         
010600                                                                          
010700     03 WS-ANTAL-DAGAR          PIC S9(9)      VALUE ZERO COMP-3.         
010800     03 WS-SUTPO-PB-DAG-FIRST   PIC S9(6)V9(2) VALUE ZERO COMP-3.         
010900     03 WS-SUTPO-PB-SUM         PIC S9(6)V9(2) VALUE ZERO COMP-3.         
011000     03 WS-SUTPO-EJPB-DAG-FIRST PIC S9(6)V9(2) VALUE ZERO COMP-3.         
011100     03 WS-SUTPO-EJPB-SUM       PIC S9(6)V9(2) VALUE ZERO COMP-3.         
011200     03 WS-SUTPO-PB-DAG-LAST    PIC S9(6)V9(2) VALUE ZERO COMP-3.         
011300     03 WS-SUTPO-EJPB-DAG-LAST  PIC S9(6)V9(2) VALUE ZERO COMP-3.         
011400     03 WS-SUTPO-PB-VECKA-LAST  PIC S9(6)V9(2) VALUE ZERO COMP-3.         
011500     03 WS-SUTPO-EJPB-VECKA-LAST PIC S9(6)V9(2) VALUE ZERO COMP-3.        
011600     03 WS-TILLGANGAR-CDC       PIC S9(7)      VALUE ZERO COMP-3.         
011700     03 WS-BEHOV-CDC            PIC S9(6)V9(2) VALUE ZERO COMP-3.         
011800     03 WS-KVBEHOV-RETUR-CDC    PIC S9(7)      VALUE ZERO COMP-3.         
011900     03 WS-KVPB-DAG-CDC         PIC S9(6)V9(2) VALUE ZERO COMP-3.         
012000     03 WS-KVPB-3DAG-CDC        PIC S9(6)V9(2) VALUE ZERO COMP-3.         
012100     03 WS-KVPB-DAG3-CDC        PIC S9(6)V9(2) VALUE ZERO COMP-3.         
012200     03 WS-KVBEHOV-T-INLEV-SDC  PIC S9(6)V9(2) VALUE ZERO COMP-3.         
012300     03 WS-DAGAR-T-INLEV        PIC S9(3)      VALUE ZERO COMP-3.         
012400     03 WS-DAGENS-DATUM-PLUS-2V PIC S9(6)      VALUE ZERO COMP-3.         
012500     03 WS-REST-SDC             PIC S9(6)V9(2) VALUE ZERO COMP-3.         
012600     03 WS-DISPONIBELT-SDC      PIC S9(7)      VALUE ZERO COMP-3.         
012700     03 WS-REST-SDC-TOT         PIC S9(6)V9(2) VALUE ZERO COMP-3.         
012800     03 WS-RETUR-SUM-SDC        PIC S9(6)V9(2) VALUE ZERO COMP-3.         
012900     03 WS-KVPB-SEP-DAG-CDC     PIC S9(6)V9(2) VALUE ZERO COMP-3.         
013000     03 WS-KVPB-SATS-DAG-CDC    PIC S9(6)V9(2) VALUE ZERO COMP-3.         
013100     03 WS-KVPB-DAG-SDC         PIC S9(6)V9(2) VALUE ZERO COMP-3.         
013200     03 WS-DD-PLUS-TVA-AR       PIC 9(6)       VALUE ZERO.                
013300                                                                          
013400    03  WS-TIDISPIN-AAAAVVD         PIC 9(7)    VALUE ZERO.               
013500    03  FILLER REDEFINES WS-TIDISPIN-AAAAVVD.                             
013600      05  WS-TIDISPIN-AAAAVV        PIC 9(6).                             
013700      05  WS-TIDISPIN-DAG           PIC 9(1).                             
013800    03  FILLER REDEFINES WS-TIDISPIN-AAAAVVD.                             
013900      05  WS-TIDISPIN-SEKEL         PIC 9(2).                             
014000      05  WS-TIDISPIN-AAVVD         PIC 9(5).                             
014100      05  FILLER REDEFINES WS-TIDISPIN-AAVVD.                             
014200        07  WS-TIDISPIN-AAVV        PIC 9(4).                             
014300        07  WS-TIDISPIN-D           PIC 9(1).                             
014400                                                                          
014500    03  WS-VECKA-PLUS2-AAVVD        PIC 9(5)    VALUE ZERO.               
014600    03  FILLER REDEFINES WS-VECKA-PLUS2-AAVVD.                            
014700        05  WS-VECKA-PLUS2-AAVV     PIC 9(4).                             
014800        05  WS-VECKA-PLUS2-D        PIC 9(1).                             
014900                                                                          
015000     03  TAB-IX                  PIC S9(9)  VALUE ZERO COMP-3.            
015100     03  FIL-IX                  PIC S9(9)  VALUE ZERO COMP-3.            
015200     03  IX                      PIC S9(9)  VALUE ZERO COMP-3.            
015300                                                                          
015400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
015500 01  FILLER REDEFINES DAGENS-DATUM.                                       
015600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
015700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
015800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
015900                                                                          
016000 01  DAGENS-VECKA                PIC 9(7)    VALUE ZERO.                  
016100 01  FILLER REDEFINES DAGENS-VECKA.                                       
016200     03  DAGENS-VECKA-AAAAVV     PIC 9(6).                                
016300     03  DAGENS-VECKA-DAG        PIC 9(1).                                
016400 01  FILLER REDEFINES DAGENS-VECKA.                                       
016500     03  DAGENS-VECKA-SEKEL      PIC 9(2).                                
016600     03  DAGENS-VECKA-AAVVD      PIC 9(5).                                
016700     03  FILLER REDEFINES DAGENS-VECKA-AAVVD.                             
016800       05  DAGENS-VECKA-AAVV     PIC 9(4).                                
016900       05  DAGENS-VECKA-D        PIC 9(1).                                
017000                                                                          
017100     EJECT                                                                
017200*      --- VALID IDDC CODES                                               
017300*                                                                         
017400*01    -COPY WWDC99                                                       
017500*01    -COPY WWDCKONS                                                     
017600       EJECT                                                              
017700******************************************************************        
017800*      TABELLER                                                           
017900******************************************************************        
018000                                                                          
018100                                                                          
018200 01  TABENTRY-PARM.                                                       
018300                                                                          
018400     03  STEGLANGD                 PIC S9(9) COMP.                        
018500     03  POST-ANTAL                PIC S9(9) COMP.                        
018600     03  NYCKELLANGD               PIC S9(9) COMP.                        
018700                                                                          
018800 01  TAB-MAX                     PIC S9(9) COMP  VALUE ZERO.              
018900                                                                          
019000 01  RETURANTALTABELL.                                                    
019100     03  RETURANTAL OCCURS 5.                                             
019200        05  TAB-IDDC                  PIC  X(2).                          
019300        05  TAB-ANTAL                 PIC S9(7)    COMP-3.                
019400                                                                          
019500 01  DYNAMISKA-SUBPROGRAM.                                                
019600*                                                                         
019700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
019800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
019900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
020000     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
020100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
020200     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
020300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
020400     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
020500     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR '.            
020600     SKIP2                                                                
020700*    --- PARAMETRAR TILL ABEND                                            
020800                                                                          
020900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
021000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
021100     SKIP2                                                                
021200 01  FELTEXT.                                                             
021300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
021400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
021500     EJECT                                                                
021600*    --- PARAMETRAR TILL DATKORT                                          
021700*                                                                         
021800 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W27154'.              
021900     SKIP2                                                                
022000 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
022100     SKIP2                                                                
022200*01  -COPY WDATKORT                                                       
022300     EJECT                                                                
022400*    --- PARAMETRAR TILL POSTSUM                                          
022500*                                                                         
022600*01  -COPY W0005   -PRE  POSTSUM-                                         
022700     EJECT                                                                
022800*    --- PARAMETRAR TILL WORKDAY                                          
022900*01  -COPY WORKAREA                                                       
023000     EJECT                                                                
023100*01  -COPY WDATAREA                                                       
023200     EJECT                                                                
023300* VARIABLER TILL SUBPROGRAM W009VADD                                      
023400 01  VADD-DATUM-AAVV                  PIC S9(5)  COMP-3.                  
023500 01  VADD-ANTAL                       PIC S9(3)  COMP-3.                  
023600     EJECT                                                                
023700 01  W01168-AREA-START           PIC X(24)   VALUE                        
023800                                 'W01168-AREA-START  '.                   
023900     SKIP2                                                                
024000                                                                          
024100*01  AREA -COPY W01168     -PRE W01168-                                   
024200     EJECT                                                                
024300 01  W27154-AREA-START           PIC X(24)   VALUE                        
024400                                 'IN54-AREA-START  '.                     
024500     SKIP2                                                                
024600                                                                          
024700*01  AREA -COPY W27154     -PRE IN54-                                     
024800     EJECT                                                                
024900 01  W27155-AREA-START           PIC X(24)   VALUE                        
025000                                 'W27155-AREA-START  '.                   
025100     SKIP2                                                                
025200                                                                          
025300*01  AREA -COPY W27155     -PRE W27155-                                   
025400     EJECT                                                                
025500 01  W27154-AREA-START           PIC X(24)   VALUE                        
025600                                 'UT54-AREA-START  '.                     
025700     SKIP2                                                                
025800                                                                          
025900*01  AREA -COPY W27154     -PRE UT54-                                     
026000     EJECT                                                                
026100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
026200*                                                                         
026300     EJECT                                                                
026400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
026500     SKIP3                                                                
026600 01  NYCKLAR-TILL-DLI.                                                    
026700     03  W-IDARTNR-X.                                                     
026800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
026900     03  W-KDSEGKEY-X.                                                    
027000         05  W-KDSEGKEY          PIC  X(1)   VALUE '1'.                   
027100     03  W-DABEHOV-X.                                                     
027200         05  W-DABEHOV           PIC 9(6)   VALUE ZERO.                   
027300     03  W-IDDC-X.                                                        
027400         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
027500     03  W-IDSKYLT-X.                                                     
027600        05 W-IDSKYLT             PIC X(3)   VALUE 'GB '.                  
027700     SKIP2                                                                
027800*    --- STATUS-KOD FRÅN IMS                                              
027900 01  STATUS-WS                   PIC XX.                                  
028000     88  SEGMENT-FINNS                       VALUE '  '.                  
028100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
028200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
028300     SKIP2                                                                
028400 01  GODK-STATUSKODER.                                                    
028500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
028600     SKIP3                                                                
028700 01  SSA1                        PIC X(64).                               
028800 01  SSA2                        PIC X(64).                               
028900     EJECT                                                                
029000*    --- IMS FUNKTIONSKODER                                               
029100*01  -COPY W0003                                                          
029200     EJECT                                                                
029300******************************************************************        
029400*          DLI INPUT - OUTPUT AREA                                        
029500******************************************************************        
029600                                                                          
029700 01  FILLER                    PIC X(16) VALUE 'DLI-IO-AREA-01'.          
029800     SKIP3                                                                
029900 01  DLI-IO-AREA-01.                                                      
030000     03  IO-AREA-01               PIC X(150) VALUE SPACE.                 
030100     03  WLARTC01 REDEFINES IO-AREA-01.                                   
030200*        05  -COPY WDK601                                                 
030300     SKIP3                                                                
030400                                                                          
030500 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA-11'.        
030600     SKIP3                                                                
030700 01  DLI-IO-AREA-11.                                                      
030800     03  IO-AREA-11               PIC X(900) VALUE SPACE.                 
030900     03  WLARTC11 REDEFINES IO-AREA-11.                                   
031000*        05  -COPY WDK611                                                 
031100     EJECT                                                                
031200                                                                          
031300 01  DLI-IO-AREA-2.                                                       
031400     03  IO-AREA-2               PIC X(200) VALUE SPACE.                  
031500                                                                          
031600*    03 WLARTM01  -COPY WDK901  -PRE WDK9- -RED IO-AREA-2.                
031700                                                                          
031800*    03 WLARTM11  -COPY WDK911             -RED IO-AREA-2.                
031900                                                                          
032000 01  DLI-IO-AREA-3.                                                       
032100     03  IO-AREA-3               PIC X(300) VALUE SPACE.                  
032200                                                                          
032300*    03  WLARTS01 -COPY WDK701             -RED IO-AREA-3.                
032400                                                                          
032500*    03  WLARTS11 -COPY WDK711             -RED IO-AREA-3.                
032600                                                                          
032700 01  DLI-IO-AREA-4.                                                       
032800*    03  WLBENA11 -COPY WDD311                                            
032900                                                                          
033000 LINKAGE SECTION.                                                         
033100                                                                          
033200     EJECT                                                                
033300*01  -COPY W0008  -PRE ARTC-                                              
033400     05  FILLER                  PIC X.                                   
033500     EJECT                                                                
033600*01  -COPY W0008  -PRE ARTM-                                              
033700     05  FILLER                  PIC X.                                   
033800     EJECT                                                                
033900*01  -COPY W0008  -PRE ARTS-                                              
034000     05  FILLER                  PIC X.                                   
034100     EJECT                                                                
034200*01  -COPY W0008  -PRE BENA-                                              
034300     05  FILLER                  PIC X.                                   
034400     EJECT                                                                
034500 PROCEDURE DIVISION  USING ARTC-PCB ARTM-PCB                              
034600     ARTS-PCB BENA-PCB.                                                   
034700     ENTRY 'DLITCBL' USING ARTC-PCB ARTM-PCB                              
034800     ARTS-PCB BENA-PCB.                                                   
034900                                                                          
035000                                                                          
035100     PERFORM A-INIT                                                       
035200     PERFORM S01-LAES-W01168                                              
035300     PERFORM S02-LAES-W27154                                              
035400     PERFORM UNTIL END-OF-W01168                                          
035500                                                                          
035600        IF W01168-ART-RERF-ART > 0.01 AND < 0.7                           
035700           MOVE W01168-ART-IDARTNR     TO WS-IDARTNR                      
035800           PERFORM B-VALJA-ARTIKEL                                        
035900           IF ARTIKEL-OK                                                  
036000              PERFORM C-NOLLSTALL                                         
036100              PERFORM D-BER-BEHOV-CDC                                     
036200              IF BRIST-CDC                                                
036300                 PERFORM E-BER-DISPONIBELT-SDC                            
036400                 IF OVERSKOTT-SDC                                         
036500                    PERFORM F-SKAPA-LISTPOST                              
036600                    PERFORM S11-SKRIV-W27154                              
036700                    PERFORM S12-SKRIV-W27155                              
036800                 END-IF                                                   
036900              END-IF                                                      
037000           END-IF                                                         
037100        END-IF                                                            
037200        PERFORM S01-LAES-W01168                                           
037300     END-PERFORM                                                          
037400     PERFORM Z-FINIT                                                      
037500                                                                          
037600     MOVE ZERO TO RETURN-CODE                                             
037700     GOBACK                                                               
037800     .                                                                    
037900     EJECT                                                                
038000                                                                          
038100                                                                          
038200 A-INIT SECTION.                                                          
038300                                                                          
038400     OPEN INPUT  W01168                                                   
038500                 W27154-I                                                 
038600                                                                          
038700     OPEN OUTPUT W27155                                                   
038800                 W27154-U                                                 
038900                                                                          
039000     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
039100     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
039200     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
039300     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
039400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
039500                                                                          
039600     MOVE 'AAMMDD'              TO DAT-KDDATFORM                          
039700     MOVE DAGENS-DATUM          TO DAT-I-TIDATUM                          
039800     CALL WDATKONV              USING DAT-KDDATFORM                       
039900                                      DAT-I-TIDATUM                       
040000                                      DAT-O-TIDATUM                       
040100                                      DAT-KDSVAR                          
040200                                                                          
040300     IF DAT-KDSVAR-OK                                                     
040400        MOVE DAT-TIAAVVD        TO DAGENS-VECKA-AAVVD                     
040500        MOVE DAT-TISEKEL        TO DAGENS-VECKA-SEKEL                     
040600                                                                          
040700        MOVE DAGENS-VECKA-D     TO WS-VECKA-PLUS2-D                       
040800        MOVE DAGENS-VECKA-AAVV  TO VADD-DATUM-AAVV                        
040900        MOVE +2                 TO VADD-ANTAL                             
041000        CALL W009VADD           USING VADD-DATUM-AAVV                     
041100                                      VADD-ANTAL                          
041200        MOVE VADD-DATUM-AAVV    TO WS-VECKA-PLUS2-AAVV                    
041300                                                                          
041400        MOVE WS-VECKA-PLUS2-AAVVD  TO DAT-I-TIDATUM                       
041500        MOVE 'AAVVD'               TO DAT-KDDATFORM                       
041600        CALL WDATKONV              USING DAT-KDDATFORM                    
041700                                         DAT-I-TIDATUM                    
041800                                         DAT-O-TIDATUM                    
041900                                         DAT-KDSVAR                       
042000        IF DAT-KDSVAR-OK                                                  
042100           MOVE DAT-TIAAMMDD TO WS-DAGENS-DATUM-PLUS-2V                   
042200        ELSE                                                              
042300           MOVE 'FEL FRÅN WDATKONV I A-INIT SECTION W27154'               
042400                                   TO  FELTEXT-STR                        
042500                                                                          
042600           DISPLAY FELTEXT                                                
042700           PERFORM S99-ABEND                                              
042800        END-IF                                                            
042900     ELSE                                                                 
043000        MOVE 'FEL FRÅN WDATKONV I A-INIT SECTION W27154'                  
043100                                   TO  FELTEXT-STR                        
043200        DISPLAY FELTEXT                                                   
043300        PERFORM S99-ABEND                                                 
043400     END-IF                                                               
043500                                                                          
043600*    FIX FÖR ATT KLARA NÄR TIDISPIN ÄR LÄNRE BORT ÄN WORKDAY-             
043700*    KALENDERN                                                            
043800*    SKALL TAS BORT NÄR RICKARDS DAGKONV KOMMER                           
043900     COMPUTE WS-DD-PLUS-TVA-AR = DAGENS-DATUM + 20000                     
044000     .                                                                    
044100     EJECT                                                                
044200                                                                          
044300                                                                          
044400 B-VALJA-ARTIKEL SECTION.                                                 
044500                                                                          
044600     MOVE W01168-ART-IDARTNR TO W-IDARTNR                                 
044700     PERFORM IMS-GET-WDK6-ART                                             
044800     IF SEGMENT-FINNS                                                     
044900        IF ART-KDERS-UTG = +0                                             
045000           MOVE ART-KDPRODSL     TO TEST-KDPRODSL                         
045100           IF KDPRODSL-VOLVO-BIMA                                         
045200              PERFORM IMS-GET-CLAG                                        
045300              IF CLAG-KVPB-SEP < 72                                       
045400                 MOVE CLAG-TIDISPIN           TO TMP1-YYMMDD              
045500                 MOVE WS-DAGENS-DATUM-PLUS-2V TO TMP2-YYMMDD              
045600                 PERFORM WY2000P1                                         
045700                 IF TMP1-YYMMDD > TMP2-YYMMDD                             
045800                    IF CLAG-KVAKS-PAV = +0                                
045900                       PERFORM BA-KOLLA-TIDIGARE-UTSKRIFT                 
046000                    END-IF                                                
046100                 END-IF                                                   
046200              END-IF                                                      
046300           END-IF                                                         
046400        END-IF                                                            
046500     END-IF                                                               
046600                                                                          
046700     .                                                                    
046800     EJECT                                                                
046900                                                                          
047000                                                                          
047100 BA-KOLLA-TIDIGARE-UTSKRIFT SECTION.                                      
047200                                                                          
047300     IF IN54-IDARTNR = W01168-ART-IDARTNR                                 
047400        IF IN54-TIDISPIN NOT = CLAG-TIDISPIN                              
047500           MOVE JA      TO ARTIKEL-OK-SW                                  
047600        END-IF                                                            
047700        PERFORM UNTIL IN54-IDARTNR > W01168-ART-IDARTNR                   
047800           PERFORM S02-LAES-W27154                                        
047900        END-PERFORM                                                       
048000     ELSE                                                                 
048100        IF IN54-IDARTNR > W01168-ART-IDARTNR                              
048200*          SAKNAS PÅ IN54-FIL                                             
048300           MOVE JA      TO ARTIKEL-OK-SW                                  
048400        ELSE                                                              
048500           IF IN54-IDARTNR < W01168-ART-IDARTNR                           
048600              PERFORM UNTIL IN54-IDARTNR > W01168-ART-IDARTNR             
048700                 IF IN54-IDARTNR = W01168-ART-IDARTNR                     
048800                    IF IN54-TIDISPIN NOT = CLAG-TIDISPIN                  
048900                       MOVE JA      TO ARTIKEL-OK-SW                      
049000                    END-IF                                                
049100                 END-IF                                                   
049200                 PERFORM S02-LAES-W27154                                  
049300              END-PERFORM                                                 
049400           END-IF                                                         
049500        END-IF                                                            
049600     END-IF                                                               
049700                                                                          
049800     .                                                                    
049900     EJECT                                                                
050000                                                                          
050100                                                                          
050200 C-NOLLSTALL SECTION.                                                     
050300                                                                          
050400     MOVE NEJ TO  ARTIKEL-OK-SW                                           
050500                  BRIST-CDC-SW                                            
050600                  OVERSKOTT-SDC-SW                                        
050700                                                                          
050800     MOVE ZERO TO WS-IDARTNR                                              
050900                                                                          
051000                  WS-ANTAL-DAGAR                                          
051100                  WS-SUTPO-PB-DAG-FIRST                                   
051200                  WS-SUTPO-PB-SUM                                         
051300                  WS-SUTPO-EJPB-DAG-FIRST                                 
051400                  WS-SUTPO-EJPB-SUM                                       
051500                  WS-SUTPO-PB-DAG-LAST                                    
051600                  WS-SUTPO-EJPB-DAG-LAST                                  
051700                  WS-SUTPO-PB-VECKA-LAST                                  
051800                  WS-SUTPO-EJPB-VECKA-LAST                                
051900                  WS-TILLGANGAR-CDC                                       
052000                  WS-BEHOV-CDC                                            
052100                  WS-KVBEHOV-RETUR-CDC                                    
052200                  WS-KVPB-DAG-CDC                                         
052300                  WS-KVPB-3DAG-CDC                                        
052400                  WS-KVPB-DAG3-CDC                                        
052500                  WS-KVBEHOV-T-INLEV-SDC                                  
052600                  WS-DAGAR-T-INLEV                                        
052700                  WS-KVPB-SEP-DAG-CDC                                     
052800                  WS-KVPB-SATS-DAG-CDC                                    
052900                                                                          
053000                  WS-TIDISPIN-AAAAVVD                                     
053100                                                                          
053200                                                                          
053300     MOVE +1 TO TAB-IX                                                    
053400     PERFORM UNTIL TAB-IX > +5                                            
053500        MOVE ZERO TO TAB-IDDC (TAB-IX)                                    
053600                     TAB-ANTAL(TAB-IX)                                    
053700        ADD +1 TO TAB-IX                                                  
053800     END-PERFORM                                                          
053900                                                                          
054000     .                                                                    
054100     EJECT                                                                
054200                                                                          
054300                                                                          
054400 D-BER-BEHOV-CDC SECTION.                                                 
054500                                                                          
054600     PERFORM S31-BER-KVPB-DAG-CDC                                         
054700     PERFORM S32-BER-DAGAR-TILL-INLEV                                     
054800     PERFORM S33-BER-TPO-BEHOV-DAG-CDC                                    
054900     PERFORM S34-BER-PER-BEHOV-DAG-CDC                                    
055000     PERFORM S35-BER-BRIST                                                
055100     IF WS-KVBEHOV-RETUR-CDC > +0                                         
055200        IF (WS-KVBEHOV-RETUR-CDC >= WS-KVPB-3DAG-CDC) OR                  
055300                 (WS-KVBEHOV-RETUR-CDC >= +10)                            
055400           MOVE JA TO BRIST-CDC-SW                                        
055500        END-IF                                                            
055600     END-IF                                                               
055700     .                                                                    
055800     EJECT                                                                
055900                                                                          
056000                                                                          
056100 E-BER-DISPONIBELT-SDC SECTION.                                           
056200                                                                          
056300     PERFORM IMS-GET-SART                                                 
056400     IF SEGMENT-FINNS                                                     
056500        PERFORM IMS-GET-SLAG                                              
056600        IF SEGMENT-FINNS                                                  
056700           MOVE +1      TO TAB-IX                                         
056800           PERFORM UNTIL SEGMENT-SAKNAS                                   
056900              MOVE SLAG-IDDC TO WS-IDDC                                   
057000              IF SDC                                                      
057100                IF SLAG-IDDC-REF(1:1) = '7'                               
057200                 CONTINUE                                                 
057300                ELSE                                                      
057400                 PERFORM S36-NOLLSTALL-SDC                                
057500                 PERFORM S37-BER-DISP-SDC                                 
057600                 PERFORM S38-BER-BEHOV-SDC                                
057700                 PERFORM S39-BER-SDC-REST                                 
057800                 IF WS-REST-SDC > +0                                      
057900                    IF WS-REST-SDC > WS-KVPB-DAG3-CDC                     
058000                       MOVE SLAG-IDDC     TO TAB-IDDC(TAB-IX)             
058100                       MOVE WS-REST-SDC   TO TAB-ANTAL(TAB-IX)            
058200                       COMPUTE WS-REST-SDC-TOT =                          
058300                               WS-REST-SDC-TOT + WS-REST-SDC              
058400                       ADD +1             TO TAB-IX                       
058500                    END-IF                                                
058600                 END-IF                                                   
058700                END-IF                                                    
058800              END-IF                                                      
058900              PERFORM IMS-GET-SLAG                                        
059000           END-PERFORM                                                    
059100           MOVE TAB-IX  TO TAB-MAX                                        
059200           COMPUTE TAB-MAX = TAB-MAX - 1                                  
059300           IF TAB-MAX > 0                                                 
059400*             TAB-MAX BLIR 0 OM ARTIKEL MED PRODSL < 90                   
059500*             BARA FINNS PÅ NDC, DENNA KOLL FÖR ATT FÖRHINDRA             
059600*             ATT DESSA KOMMER UT PÅ LISTAN TEST I PROD SK 13/6           
059700              IF WS-REST-SDC-TOT > +0                                     
059800                 IF WS-REST-SDC-TOT < WS-KVPB-3DAG-CDC                    
059900                    CONTINUE                                              
060000                 ELSE                                                     
060100                    MOVE JA TO OVERSKOTT-SDC-SW                           
060200                 END-IF                                                   
060300              END-IF                                                      
060400           END-IF                                                         
060500        END-IF                                                            
060600     END-IF                                                               
060700     .                                                                    
060800     EJECT                                                                
060900                                                                          
061000                                                                          
061100 F-SKAPA-LISTPOST SECTION.                                                
061200                                                                          
061300     PERFORM S40-SORTERA-TABELL                                           
061400     PERFORM S41-FLYTTA-CDC-UPPGIFTER                                     
061500                                                                          
061600     MOVE +1 TO FIL-IX                                                    
061700     PERFORM UNTIL FIL-IX > 5                                             
061800        MOVE SPACE               TO W27155-IDDC  (FIL-IX)                 
061900        MOVE ZERO                TO W27155-KVANTAL-SDC (FIL-IX)           
062000        ADD +1                   TO FIL-IX                                
062100     END-PERFORM                                                          
062200                                                                          
062300     MOVE TAB-MAX     TO TAB-IX                                           
062400                         IX                                               
062500     MOVE +1          TO FIL-IX                                           
062600                                                                          
062700     PERFORM UNTIL IX < +1                                                
062800        COMPUTE WS-RETUR-SUM-SDC =                                        
062900                WS-RETUR-SUM-SDC + TAB-ANTAL (TAB-IX)                     
063000        MOVE TAB-IDDC  (TAB-IX) TO W27155-IDDC  (FIL-IX)                  
063100        MOVE TAB-ANTAL (TAB-IX) TO W27155-KVANTAL-SDC(FIL-IX)             
063200        COMPUTE TAB-IX = TAB-IX - 1                                       
063300        COMPUTE     IX = IX - 1                                           
063400        ADD +1                      TO FIL-IX                             
063500     END-PERFORM                                                          
063600                                                                          
063700     MOVE W01168-ART-IDARTNR       TO UT54-IDARTNR                        
063800     MOVE CLAG-TIDISPIN            TO UT54-TIDISPIN                       
063900     .                                                                    
064000     EJECT                                                                
064100                                                                          
064200                                                                          
064300 Z-FINIT SECTION.                                                         
064400     CLOSE W01168                                                         
064500           W27154-I                                                       
064600           W27154-U                                                       
064700           W27155                                                         
064800     SKIP2                                                                
064900     MOVE 'S' TO POSTSUM-OPKOD                                            
065000     CALL POSTSUM USING POSTSUM-PARM                                      
065100     .                                                                    
065200     EJECT                                                                
065300                                                                          
065400                                                                          
065500 S01-LAES-W01168  SECTION.                                                
065600     READ W01168 INTO W01168-AREA                                         
065700     AT END                                                               
065800        SET END-OF-W01168 TO TRUE                                         
065900                                                                          
066000     NOT AT END                                                           
066100        MOVE 'W01168' TO POSTSUM-FDNAMN                                   
066200        MOVE 'W27154D1' TO POSTSUM-DDNAMN2                                
066300        CALL POSTSUM USING POSTSUM-PARM                                   
066400     END-READ                                                             
066500     .                                                                    
066600     EJECT                                                                
066700                                                                          
066800                                                                          
066900 S02-LAES-W27154  SECTION.                                                
067000     READ W27154-I INTO IN54-AREA                                         
067100     AT END                                                               
067200        MOVE +999999999 TO IN54-IDARTNR                                   
067300        SET END-OF-W27154 TO TRUE                                         
067400                                                                          
067500     NOT AT END                                                           
067600        MOVE 'W27154-I' TO POSTSUM-FDNAMN                                 
067700        MOVE 'W27154D2' TO POSTSUM-DDNAMN2                                
067800        CALL POSTSUM USING POSTSUM-PARM                                   
067900     END-READ                                                             
068000     .                                                                    
068100     EJECT                                                                
068200                                                                          
068300                                                                          
068400 S11-SKRIV-W27154 SECTION.                                                
068500                                                                          
068600     WRITE UT54-POST FROM UT54-AREA                                       
068700                                                                          
068800     MOVE 'W27154' TO POSTSUM-FDNAMN                                      
068900     MOVE 'W27154D3' TO POSTSUM-DDNAMN2                                   
069000     CALL POSTSUM USING POSTSUM-PARM                                      
069100     .                                                                    
069200     EJECT                                                                
069300                                                                          
069400                                                                          
069500 S12-SKRIV-W27155 SECTION.                                                
069600                                                                          
069700     WRITE W27155-POST FROM W27155-AREA                                   
069800                                                                          
069900     MOVE 'W27155' TO POSTSUM-FDNAMN                                      
070000     MOVE 'W27154D4' TO POSTSUM-DDNAMN2                                   
070100     CALL POSTSUM USING POSTSUM-PARM                                      
070200     .                                                                    
070300     EJECT                                                                
070400                                                                          
070500                                                                          
070600 S31-BER-KVPB-DAG-CDC SECTION.                                            
070700                                                                          
070800*** VECKOBEHOV ??                                                         
070900                                                                          
071000     COMPUTE WS-KVPB-DAG-CDC ROUNDED =                                    
071100                  (CLAG-KVPB-SEP *                                        
071200                  (1 - CLAG-REDIRLEV) / 4.33) +                           
071300                  (CLAG-KVPB-SATS / 4.33)     +                           
071400                  (CLAG-KVPB-TPO / 4.33)                                  
071500                                                                          
071600*** 3 VECKOBEHOV ??                                                       
071700                                                                          
071800     COMPUTE WS-KVPB-3DAG-CDC = 3 * WS-KVPB-DAG-CDC                       
071900                                                                          
072000*** 1/3 VECKOBEHOV ??                                                     
072100                                                                          
072200     COMPUTE WS-KVPB-DAG3-CDC = WS-KVPB-DAG-CDC / 3                       
072300     .                                                                    
072400     EJECT                                                                
072500                                                                          
072600                                                                          
072700 S32-BER-DAGAR-TILL-INLEV SECTION.                                        
072800                                                                          
072900     MOVE CLAG-TIDISPIN       TO TMP1-YYMMDD                              
073000     MOVE WS-DD-PLUS-TVA-AR   TO TMP2-YYMMDD                              
073100     PERFORM WY2000P1                                                     
073200     IF TMP1-YYMMDD > TMP2-YYMMDD                                         
073300        MOVE 400 TO WS-DAGAR-T-INLEV                                      
073400     ELSE                                                                 
073500        MOVE DAGENS-DATUM        TO WORK-TIAAMMDD-FOM                     
073600        MOVE CLAG-TIDISPIN       TO WORK-TIAAMMDD-TOM                     
073700        MOVE 001                 TO WORK-KDCALL                           
073800        MOVE WC-CDC-SE           TO WORK-IDDC                             
073900        CALL WORKDAY             USING WORK-KDCALL                        
074000                                       WORK-DATE-AREA                     
074100                                       WORK-KDSVAR                        
074200        IF WORK-KDSVAR-OK                                                 
074300           MOVE WORK-KVWORKD    TO WS-DAGAR-T-INLEV                       
074400        ELSE                                                              
074500           MOVE 'FEL FRÅN WORKDAY I S32 SECTION I W27154' TO              
074600                                       FELTEXT-STR                        
074700           DISPLAY FELTEXT                                                
074800           PERFORM S99-ABEND                                              
074900        END-IF                                                            
075000     END-IF                                                               
075100                                                                          
075200                                                                          
075300     MOVE CLAG-TIDISPIN         TO DAT-I-TIDATUM                          
075400     MOVE 'AAMMDD'              TO DAT-KDDATFORM                          
075500     CALL WDATKONV              USING DAT-KDDATFORM                       
075600                                      DAT-I-TIDATUM                       
075700                                      DAT-O-TIDATUM                       
075800                                      DAT-KDSVAR                          
075900     IF DAT-KDSVAR-OK                                                     
076000        MOVE DAT-TIAAVVD-GRP    TO WS-TIDISPIN-AAVVD                      
076100        MOVE DAT-TISEKEL        TO WS-TIDISPIN-SEKEL                      
076200     ELSE                                                                 
076300        MOVE 'FEL FRÅN WDATKONV I S32 SECTION I W27154' TO                
076400                                    FELTEXT-STR                           
076500        DISPLAY FELTEXT                                                   
076600        PERFORM S99-ABEND                                                 
076700     END-IF                                                               
076800                                                                          
076900     .                                                                    
077000     EJECT                                                                
077100                                                                          
077200                                                                          
077300 S33-BER-TPO-BEHOV-DAG-CDC SECTION.                                       
077400                                                                          
077500*    ANTAL PB/EJPB I INNEV VECKA                                          
077600                                                                          
077700     PERFORM IMS-GET-WDK9-ART                                             
077800     IF SEGMENT-FINNS                                                     
077900        PERFORM IMS-GET-ANT                                               
078000        IF SEGMENT-SAKNAS                                                 
078100           CONTINUE                                                       
078200        ELSE                                                              
078300           PERFORM UNTIL SEGMENT-SAKNAS OR                                
078400                   ANT-DABEHOV > WS-TIDISPIN-AAAAVV                       
078500              IF ANT-DABEHOV >= DAGENS-VECKA-AAAAVV                       
078600                 IF ANT-DABEHOV = DAGENS-VECKA-AAAAVV                     
078700                    COMPUTE WS-ANTAL-DAGAR = (6 - DAGENS-VECKA-D)         
078800                    IF WS-ANTAL-DAGAR = ZERO                              
078900                       MOVE +1 TO WS-ANTAL-DAGAR                          
079000                    END-IF                                                
079100                    COMPUTE WS-SUTPO-PB-DAG-FIRST =                       
079200                            ANT-SUTPO-PB / 5                              
079300                    COMPUTE WS-SUTPO-PB-SUM =                             
079400                       WS-SUTPO-PB-DAG-FIRST * WS-ANTAL-DAGAR             
079500                    COMPUTE WS-SUTPO-EJPB-DAG-FIRST =                     
079600                            ANT-SUTPO-EJPB / 5                            
079700                    COMPUTE WS-SUTPO-EJPB-SUM =                           
079800                        WS-SUTPO-EJPB-DAG-FIRST * WS-ANTAL-DAGAR          
079900                                                                          
080000                                                                          
080100                 ELSE                                                     
080200*  * --->           ANTAL I MELLANLIGGANDE VECKOR                         
080300                    IF ANT-DABEHOV < WS-TIDISPIN-AAAAVV                   
080400                       COMPUTE WS-SUTPO-PB-SUM   =                        
080500                            WS-SUTPO-PB-SUM   + ANT-SUTPO-PB              
080600                       COMPUTE WS-SUTPO-EJPB-SUM =                        
080700                            WS-SUTPO-EJPB-SUM + ANT-SUTPO-EJPB            
080800                    ELSE                                                  
080900*  * ---->             ANTAL I SISTA VECKAN                               
081000                       IF ANT-DABEHOV = WS-TIDISPIN-AAAAVV                
081100                          COMPUTE WS-SUTPO-PB-DAG-LAST =                  
081200                                  ANT-SUTPO-PB / 5                        
081300                          COMPUTE WS-SUTPO-PB-VECKA-LAST =                
081400                                  WS-SUTPO-PB-DAG-LAST *                  
081500                                 (WS-TIDISPIN-D - 1)                      
081600                                                                          
081700                          COMPUTE WS-SUTPO-EJPB-DAG-LAST =                
081800                                  ANT-SUTPO-EJPB / 5                      
081900                          COMPUTE WS-SUTPO-EJPB-VECKA-LAST =              
082000                                  WS-SUTPO-EJPB-DAG-LAST *                
082100                                 (WS-TIDISPIN-D - 1)                      
082200                          COMPUTE WS-SUTPO-PB-SUM   =                     
082300                                  WS-SUTPO-PB-SUM   +                     
082400                                       WS-SUTPO-PB-VECKA-LAST             
082500                          COMPUTE WS-SUTPO-EJPB-SUM =                     
082600                                  WS-SUTPO-EJPB-SUM +                     
082700                                       WS-SUTPO-EJPB-VECKA-LAST           
082800                       END-IF                                             
082900                    END-IF                                                
083000                 END-IF                                                   
083100              END-IF                                                      
083200              PERFORM IMS-GET-ANT                                         
083300           END-PERFORM                                                    
083400        END-IF                                                            
083500     END-IF                                                               
083600                                                                          
083700     .                                                                    
083800     EJECT                                                                
083900                                                                          
084000                                                                          
084100 S34-BER-PER-BEHOV-DAG-CDC SECTION.                                       
084200                                                                          
084300     COMPUTE WS-KVPB-SEP-DAG-CDC ROUNDED =                                
084400            (CLAG-KVPB-SEP / 4.33) / 5                                    
084500                                                                          
084600     COMPUTE WS-KVPB-SATS-DAG-CDC ROUNDED =                               
084700            (CLAG-KVPB-SATS / 4.33) / 5                                   
084800                                                                          
084900     .                                                                    
085000     EJECT                                                                
085100                                                                          
085200                                                                          
085300 S35-BER-BRIST SECTION.                                                   
085400                                                                          
085500     COMPUTE WS-TILLGANGAR-CDC =                                          
085600                     CLAG-KVLS          -                                 
085700                     CLAG-KVSPANT       -                                 
085800                     CLAG-KVUTRS        -                                 
085900                     CLAG-KVRESS        -                                 
086000                     W01168-ART-KVPREAVB-VOR -                            
086100                     W01168-ART-KVPREAVB-DAG                              
086200                                                                          
086300     COMPUTE WS-BEHOV-CDC =                                               
086400                     WS-KVPB-SEP-DAG-CDC  * WS-DAGAR-T-INLEV +            
086500                     WS-KVPB-SATS-DAG-CDC * WS-DAGAR-T-INLEV +            
086600                     WS-SUTPO-PB-SUM        +                             
086700                     WS-SUTPO-EJPB-SUM      +                             
086800                     W01168-ART-KVOKS-DAG   +                             
086900                     W01168-ART-KVOKS-BULK  -                             
087000                     W01168-ART-KVPREAVB-DAG                              
087100                                                                          
087200     COMPUTE WS-KVBEHOV-RETUR-CDC ROUNDED =                               
087300                      WS-BEHOV-CDC - WS-TILLGANGAR-CDC                    
087400     .                                                                    
087500     EJECT                                                                
087600                                                                          
087700                                                                          
087800 S36-NOLLSTALL-SDC SECTION.                                               
087900                                                                          
088000     MOVE ZERO TO WS-DISPONIBELT-SDC                                      
088100                  WS-KVPB-DAG-SDC                                         
088200                  WS-KVBEHOV-T-INLEV-SDC                                  
088300                  WS-REST-SDC                                             
088400                  WS-REST-SDC-TOT                                         
088500                  WS-RETUR-SUM-SDC                                        
088600     .                                                                    
088700     EJECT                                                                
088800                                                                          
088900                                                                          
089000 S37-BER-DISP-SDC SECTION.                                                
089100                                                                          
089200     COMPUTE WS-DISPONIBELT-SDC = SLAG-KVLS  -                            
089300                                  SLAG-KVOKS-DAG -                        
089400                                  SLAG-KVOKS-BULK                         
089500     .                                                                    
089600     EJECT                                                                
089700                                                                          
089800                                                                          
089900 S38-BER-BEHOV-SDC SECTION.                                               
090000                                                                          
090100     COMPUTE WS-KVPB-DAG-SDC =                                            
090200                SLAG-KVPB-REF / 4.33                                      
090300     COMPUTE WS-KVBEHOV-T-INLEV-SDC =                                     
090400             WS-KVPB-DAG-SDC * (WS-DAGAR-T-INLEV - 2)                     
090500     .                                                                    
090600     EJECT                                                                
090700                                                                          
090800                                                                          
090900 S39-BER-SDC-REST SECTION.                                                
091000                                                                          
091100     COMPUTE WS-REST-SDC =                                                
091200             WS-DISPONIBELT-SDC - WS-KVBEHOV-T-INLEV-SDC                  
091300     .                                                                    
091400     EJECT                                                                
091500                                                                          
091600                                                                          
091700 S40-SORTERA-TABELL SECTION.                                              
091800                                                                          
091900     MOVE +6                         TO STEGLANGD                         
092000     MOVE TAB-MAX                    TO POST-ANTAL                        
092100     MOVE +4                         TO NYCKELLANGD                       
092200     CALL WINTSOR USING RETURANTALTABELL STEGLANGD                        
092300                        POST-ANTAL                                        
092400           TAB-ANTAL(1) NYCKELLANGD                                       
092500                                                                          
092600                                                                          
092700     MOVE +1 TO TAB-IX                                                    
092800     PERFORM UNTIL TAB-IX > +5                                            
092900        ADD +1 TO TAB-IX                                                  
093000     END-PERFORM                                                          
093100                                                                          
093200     .                                                                    
093300     EJECT                                                                
093400                                                                          
093500                                                                          
093600 S41-FLYTTA-CDC-UPPGIFTER SECTION.                                        
093700                                                                          
093800     MOVE W01168-ART-IDARTNR       TO W27155-IDARTNR                      
093900     MOVE W01168-ART-RERF-ART      TO W27155-RERF-ART                     
094000     MOVE CLAG-KVLS                TO W27155-KVLS                         
094100     MOVE CLAG-KDERS               TO W27155-KDERS                        
094200     MOVE WS-KVBEHOV-RETUR-CDC     TO W27155-RETURBEHOV-CDC               
094300                                                                          
094400     PERFORM IMS-GET-BENA11-BSEQ                                          
094500     IF SEGMENT-FINNS                                                     
094600        MOVE TEXT-BEART TO W27155-BEART-ENG                               
094700     ELSE                                                                 
094800        MOVE 'BENÄMNING SAKNAS     ' TO W27155-BEART-ENG                  
094900     END-IF                                                               
095000     .                                                                    
095100     EJECT                                                                
095200                                                                          
095300                                                                          
095400 S99-ABEND SECTION.                                                       
095500                                                                          
095600     SKIP2                                                                
095700     MOVE 'S' TO POSTSUM-OPKOD                                            
095800     CALL POSTSUM USING POSTSUM-PARM                                      
095900     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
096000     .                                                                    
096100     EJECT                                                                
096200                                                                          
096300                                                                          
096400* --- IMS SEKTIONER --   &&&                                              
096500     SKIP3                                                                
096600                                                                          
096700                                                                          
096800 IMS-GET-WDK6-ART SECTION.                                                
096900                                                                          
097000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
097100          DELIMITED BY SIZE INTO SSA1                                     
097200     MOVE '  GE' TO GODK-STATUSKODER                                      
097300     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-01 SSA1                   
097400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
097500     PERFORM IMS-STATUSKONTROLL                                           
097600     .                                                                    
097700     EJECT                                                                
097800                                                                          
097900                                                                          
098000 IMS-GET-CLAG SECTION.                                                    
098100                                                                          
098200     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
098300          DELIMITED BY SIZE INTO SSA1                                     
098400     MOVE '  GE' TO GODK-STATUSKODER                                      
098500     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-11 SSA1                  
098600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
098700     PERFORM IMS-STATUSKONTROLL                                           
098800     .                                                                    
098900     EJECT                                                                
099000                                                                          
099100                                                                          
099200 IMS-GET-WDK9-ART SECTION.                                                
099300                                                                          
099400     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
099500          DELIMITED BY SIZE INTO SSA1                                     
099600     MOVE '  GE' TO GODK-STATUSKODER                                      
099700     CALL CBLTDLI USING GU ARTM-PCB DLI-IO-AREA-2 SSA1                    
099800     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
099900     PERFORM IMS-STATUSKONTROLL                                           
100000     .                                                                    
100100     EJECT                                                                
100200                                                                          
100300                                                                          
100400 IMS-GET-ANT SECTION.                                                     
100500                                                                          
100600     MOVE 'WLARTM11 ' TO SSA1                                             
100700     MOVE '  GE' TO GODK-STATUSKODER                                      
100800     CALL CBLTDLI USING GNP ARTM-PCB DLI-IO-AREA-2 SSA1                   
100900     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
101000     PERFORM IMS-STATUSKONTROLL                                           
101100     .                                                                    
101200     EJECT                                                                
101300                                                                          
101400                                                                          
101500 IMS-GET-SART SECTION.                                                    
101600                                                                          
101700     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
101800          DELIMITED BY SIZE INTO SSA1                                     
101900     MOVE '  GE' TO GODK-STATUSKODER                                      
102000     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA-3 SSA1                    
102100     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
102200     PERFORM IMS-STATUSKONTROLL                                           
102300     .                                                                    
102400     EJECT                                                                
102500                                                                          
102600                                                                          
102700 IMS-GET-SLAG SECTION.                                                    
102800                                                                          
102900     MOVE 'WLARTS11 ' TO SSA1                                             
103000     MOVE '  GE' TO GODK-STATUSKODER                                      
103100     CALL CBLTDLI USING GNP ARTS-PCB DLI-IO-AREA-3 SSA1                   
103200     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
103300     PERFORM IMS-STATUSKONTROLL                                           
103400     .                                                                    
103500     EJECT                                                                
103600                                                                          
103700                                                                          
103800 IMS-GET-BENA11-BSEQ  SECTION.                                            
103900     SKIP3                                                                
104000     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
104100            DELIMITED BY SIZE INTO SSA1                                   
104200     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
104300            DELIMITED BY SIZE INTO SSA2                                   
104400     MOVE '  GE' TO GODK-STATUSKODER                                      
104500     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-4 SSA1 SSA2               
104600     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
104700     PERFORM IMS-STATUSKONTROLL                                           
104800     .                                                                    
104900     EJECT                                                                
105000                                                                          
105100                                                                          
105200 IMS-STATUSKONTROLL SECTION.                                              
105300                                                                          
105400     SET STATUS-IX TO 1                                                   
105500     SEARCH GODK-STATUS                                                   
105600       AT END                                                             
105700         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
105800         DISPLAY FELTEXT                                                  
105900         CALL FELLOG                                                      
106000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
106100         CONTINUE                                                         
106200     END-SEARCH                                                           
106300     .                                                                    
106400     EJECT                                                                
106500*    -COPY WY2000P1                                                       
