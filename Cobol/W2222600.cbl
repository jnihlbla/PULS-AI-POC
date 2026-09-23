000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2222600.                                                
000400*AUTHOR.         STEFAN ANDREASSON.                                       
000500*DATE-WRITTEN.   99/12/07.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        PROGRAMMET BERÄKNAR NY SÄSONG                                    
001200*                                                                         
001300*        PROGRAMMET LÄSER      WDK6                                       
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  SVAR FRÅN WDATKONV EJ OK                                
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800                                                                          
002900     SELECT W22226                     ASSIGN TO W22226D1.                
003000*          --- UT-FIL                                                     
003100                                                                          
003200     SELECT W22226A                    ASSIGN TO W22226D2.                
003300*          --- UT-FIL                                                     
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
003900                                                                          
004000                                                                          
004100 FD  W22226                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500*01  POST -COPY W22226  -PRE UT1-   -L.                                   
004600                                                                          
004700                                                                          
004800 FD  W22226A                                                              
004900     RECORDING       F                                                    
005000     BLOCK CONTAINS  0.                                                   
005100                                                                          
005200*01  POST -COPY W22226  -PRE UT2-   -L.                                   
005300                                                                          
005400     EJECT                                                                
005500 WORKING-STORAGE SECTION.                                                 
005600     SKIP2                                                                
005700*    -COPY WY2000W1                                                       
005800     SKIP2                                                                
005900*    -COPY WY2000W3                                                       
006000     SKIP3                                                                
006100*    -COPY WY2000W2                                                       
006200     SKIP3                                                                
006300 77  IDPGM                       PIC X(8)    VALUE 'W2222600'.            
006400 77  JA                          PIC X       VALUE 'J'.                   
006500 77  NEJ                         PIC X       VALUE 'N'.                   
006600 77  AKTIV                       PIC X       VALUE 'A'.                   
006700                                                                          
006800*    --- INDEX SAMT MAX-INDEX                                             
006900 77  FILLER                      PIC X(16)   VALUE 'INDEX'.               
007000 77  INDX                        PIC 9(2)    VALUE ZERO.                  
007100 77  IX                          PIC 9(3)    VALUE ZERO.                  
007200 77  DC-INDX                     PIC 9(2)    VALUE ZERO.                  
007300 77  PER-INDX                    PIC 9(2)    VALUE ZERO.                  
007400 77  TREND-INDX                  PIC 9(2)    VALUE ZERO.                  
007500 77  MAX-FSGFAKT                 PIC 9(2)    VALUE 11.                    
007600                                                                          
007700 77  VECKO-INDX                  PIC 9(2)    VALUE ZERO.                  
007800                                                                          
007900 01  TAB-RADIX                   PIC 9(2)    VALUE ZERO.                  
008000 77  MAX-TAB-RADIX               PIC 9(2)    VALUE ZERO.                  
008100                                                                          
008200 77  VV-INDX                     PIC 9(2)    VALUE ZERO.                  
008300 77  MAX-VV-INDX                 PIC 9(2)    VALUE 53.                    
008400                                                                          
008500*    --- SWITCHAR                                                         
008600 01  FILLER                      PIC X(16)   VALUE 'SWITCHAR'.            
008700                                                                          
008800 01  DC-POST.                                                             
008900     03  DC-PARAMETER        PIC X(4).                                    
009000         88 SAMTLIGA-DC      VALUE 'ALLA'.                                
009100         88 SAMTLIGA-SDC     VALUE 'EURO'.                                
009200         88 SAMTLIGA-NDC     VALUE 'AMER'.                                
009300         88 ENSTAKA-DC       VALUE 'DC21' 'DC22' 'DC23'                   
009400                                   'DC24' 'DC25' 'DC26'                   
009500                                   'DC3A'                                 
009600                                   'DC41' 'DC42' 'DC43' 'DC51'.           
009700                                                                          
009800     03  FILLER               PIC X(76).                                  
009900                                                                          
010000                                                                          
010100*      --- VALID IDDC CODES                                               
010200*                                                                         
010300*01    -COPY WWDC99                                                       
010400                                                                          
010500 01 DC-PARAMETER-DELAR.                                                   
010600     03 FILLER                PIC X(2)  VALUE SPACE.                      
010700     03 DC-PARAMETER-LAGER    PIC X(2)  VALUE SPACE.                      
010800                                                                          
010900 01  TREND-SW                    PIC X(5)    VALUE SPACE.                 
011000     88  INGEN-TREND                         VALUE 'INGEN'.               
011100     88  SVAG-TREND                          VALUE 'SVAG '.               
011200     88  STARK-TREND                         VALUE 'STARK'.               
011300                                                                          
011400 01  INDX-SW                     PIC X       VALUE 'N'.                   
011500     88  INDX-HITTAT                         VALUE 'J'.                   
011600                                                                          
011700 01  ARTIKEL-SW                  PIC X       VALUE 'N'.                   
011800     88  ARTIKEL-SKALL-FORAENDRAS            VALUE 'J'.                   
011900                                                                          
012000 01  MANUELL-PROGNOS-SW          PIC X       VALUE 'N'.                   
012100     88  MANUELL-PROGNOS-SATT                VALUE 'J'.                   
012200                                                                          
012300*    --- ARBETSFÄLT                                                       
012400 01  FILLER                      PIC X(16)   VALUE 'ARBETSFÄLT'.          
012500 01  ARBETSFAELT.                                                         
012600     03  PERIODTABELL            OCCURS 13.                               
012700         05 TABELL-TIAARP        PIC  9(4)    VALUE ZERO.                 
012800         05 TABELL-FORSTA-TIAAVV PIC  9(2)    VALUE ZERO.                 
012900         05 TABELL-SISTA-TIAAVV  PIC  9(2)    VALUE ZERO.                 
013000         05 TABELL-KVOI        PIC S9(9)V9(1)  VALUE ZERO COMP-3.         
013100                                                                          
013200     03  SLUT-VV                 PIC 9(2)    VALUE ZERO.                  
013300     03  START-VV                PIC 9(2)    VALUE ZERO.                  
013400                                                                          
013500     03  WS-NOLL                 PIC 9(4)    VALUE ZERO.                  
013600     03  WS-TIAAVV               PIC 9(4)    VALUE ZERO.                  
013700     03  FILLER REDEFINES WS-TIAAVV.                                      
013800         05 WS-TIAA              PIC 9(2).                                
013900         05 WS-TIVV              PIC 9(2).                                
014000     03  WS-IDFKNGRP             PIC S9(5)    VALUE ZERO COMP-3.          
014100                                                                          
014200     03  FOREG-TIAARP            PIC  9(4)   VALUE ZERO.                  
014300     03  FILLER REDEFINES FOREG-TIAARP.                                   
014400         05 FOREG-TIAA           PIC  9(2).                               
014500         05 FOREG-TIRP           PIC  9(2).                               
014600                                                                          
014700     03 DAGENS-TISSSSMMDD        PIC 9(8)       VALUE ZERO.               
014800     03 DAGENS-TISSSSMMDD-GRP    REDEFINES DAGENS-TISSSSMMDD.             
014900       05 DAGENS-TISS            PIC 9(2).                                
015000       05 DAGENS-TISSMMDD        PIC 9(6).                                
015100                                                                          
015200     03  DAGENS-TIAARP           PIC  9(4)   VALUE ZERO.                  
015300     03  FILLER REDEFINES DAGENS-TIAARP.                                  
015400         05 DAGENS-TIAA          PIC  9(2).                               
015500         05 DAGENS-TIRP          PIC  9(2).                               
015600                                                                          
015700     03  NAESTA-TIAARP           PIC  9(4)   VALUE ZERO.                  
015800     03  FILLER REDEFINES NAESTA-TIAARP.                                  
015900         05 NAESTA-TIAA          PIC  9(2).                               
016000         05 NAESTA-TIRP          PIC  9(2).                               
016100                                                                          
016200     03  SEASON-TIAARP           PIC  9(4)   VALUE ZERO.                  
016300     03  FILLER REDEFINES SEASON-TIAARP.                                  
016400         05 SEASON-TIAA          PIC  9(2).                               
016500         05 SEASON-TIRP          PIC  9(2).                               
016600                                                                          
016700     03  DAGENS-TIAAVVD          PIC  9(5)   VALUE ZERO.                  
016800     03  FILLER REDEFINES DAGENS-TIAAVVD.                                 
016900         05 DAGENS-TIAAVVD-AA    PIC  9(2).                               
017000         05 DAGENS-TIAAVVD-VV    PIC  9(2).                               
017100         05 DAGENS-TIAAVVD-D     PIC  9(1).                               
017200                                                                          
017300     03  DAGENS-TIAAVV-GRP       PIC  9(4)   VALUE ZERO.                  
017400                                                                          
017500                                                                          
017600     03  DAGENS-TIAAVVD-LAST-YEAR        PIC  9(5) VALUE ZERO.            
017700     03  FILLER REDEFINES DAGENS-TIAAVVD-LAST-YEAR.                       
017800         05 DAGENS-TIAAVVD-LAST-YEAR-AA  PIC 9(2).                        
017900         05 DAGENS-TIAAVVD-LAST-YEAR-VV  PIC 9(2).                        
018000         05 DAGENS-TIAAVVD-LAST-YEAR-D   PIC 9(1).                        
018100                                                                          
018200     03  DAGENS-TIVV             PIC  9(2)   VALUE ZERO.                  
018300                                                                          
018400     03  WS-ANTAL-VECKOR         PIC  9(2)      VALUE ZERO.               
018500     03  WS-VECKO-IO             PIC S9(9)V9 VALUE ZERO COMP-3.           
018600     03  WS-TEST-TIPBDAT         PIC S9(7)   VALUE ZERO COMP-3.           
018700     03  WS-TIPBDAT-TIAARP       PIC  9(4)      VALUE ZERO.               
018800     03  WS-FORSTA-TIAAVV        PIC  9(4)      VALUE ZERO.               
018900     03  WS-SISTA-TIAAVV         PIC  9(4)      VALUE ZERO.               
019000     03  WS-DAT-TIAAVV           PIC  9(4)      VALUE ZERO.               
019100     03  WS-ONORM-OI-GRAENS-PB   PIC S9(9)V9(1) VALUE ZERO COMP-3.        
019200     03  WS-KVOI-SEASON          PIC S9(9)V9(1) VALUE ZERO COMP-3.        
019300     03  WS-KVOI-TOT             PIC S9(11)V9(2)                          
019400                                                VALUE ZERO COMP-3.        
019500     03  WS-NY-KVPB-SEP          PIC S9(6)V9(2) VALUE ZERO COMP-3.        
019600     03  NY-KVPB-SEP             PIC S9(6)V9(1) VALUE ZERO COMP-3.        
019700     03  WS-PREL-KVPB-SEP        PIC S9(6)V9(2) VALUE ZERO COMP-3.        
019800     03  WS-MEDEL-KVPB-SEP       PIC S9(6)V9(2) VALUE ZERO COMP-3.        
019900     03  WS-ANTAL-FAKTORER-STOERRE-NOLL PIC S9(7)          COMP-3.        
020000     03  WS-KVOTEN               PIC S9(5)V9(2) VALUE ZERO COMP-3.        
020100     03  WS-TIFINLV              PIC S9(5)V     VALUE ZERO COMP-3.        
020200     03  WS-VECKA-I-AKT-PERIOD   PIC  9(2)      VALUE ZERO.               
020300                                                                          
020400     03  WS-DC-PROGFAKT-VKA.                                              
020500       05 WS-PROGFAKT-DC  OCCURS 11.                                      
020600        07 WS-PROGFAKT-PER    OCCURS 13.                                  
020700           09 WS-DC-PERIODTREND-VKA.                                      
020800               11 WS-DC-NORMAL-VKA       PIC 9V9(4).                      
020900               11 WS-DC-SVAG-TREND-VKA   PIC 9V9(4).                      
021000               11 WS-DC-STARK-TREND-VKA  PIC 9V9(4).                      
021100           09 FILLER REDEFINES WS-DC-PERIODTREND-VKA.                     
021200               11 WS-DC-TRENDFAKT-VKA    PIC 9V9(4) OCCURS 3.             
021300     03  WS-DC-PROGFAKT-MAX      PIC 9(2)       VALUE 11.                 
021400                                                                          
021500     EJECT                                                                
021600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
021700 01  FILLER REDEFINES DAGENS-DATUM.                                       
021800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
021900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
022000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
022100     EJECT                                                                
022200                                                                          
022300* INFO OM KÖRTYP(DAG) FRPN CONSTANTMEDLEM VALD AV JCL'EN                  
022400* INFON KOMMER SOM FIL D1                                                 
022500                                                                          
022600 01  W271TYP-POST.                                                        
022700     03  TYP-PARAMETER        PIC X(4).                                   
022800         88 DAY-KORNING       VALUE 'DAY '.                               
022900         88 WEEK-KORNING      VALUE 'WEEK'.                               
023000         88 ACC-KORNING       VALUE 'ACC '.                               
023100     03  FILLER               PIC X(76).                                  
023200                                                                          
023300 01  DYNAMISKA-SUBPROGRAM.                                                
023400*                                                                         
023500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
023600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
023700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
023800     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
023900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
024000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
024100     03  W222SEAS                PIC X(8)    VALUE 'W222SEAS'.            
024200     SKIP2                                                                
024300*    --- PARAMETRAR TILL ABEND                                            
024400                                                                          
024500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
024600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
024700     SKIP2                                                                
024800 01  FELTEXT.                                                             
024900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
025000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
025100     EJECT                                                                
025200*    --- PARAMETRAR TILL DATKORT                                          
025300*                                                                         
025400 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W22226'.              
025500     SKIP2                                                                
025600 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
025700     SKIP2                                                                
025800*01  -COPY WDATKORT                                                       
025900     EJECT                                                                
026000*    --- PARAMETRAR TILL POSTSUM                                          
026100*                                                                         
026200*01  -COPY W0005   -PRE  POSTSUM-                                         
026300     EJECT                                                                
026400*    --- PARAMETRAR TILL WDATKONV                                         
026500*                                                                         
026600*01  -COPY WDATAREA                                                       
026700     EJECT                                                                
026800*    --- PARAMETRAR TILL SUBPROGRAM W222SEAS                              
026900*                                                                         
027000 01  FILLER                      PIC X(16)   VALUE 'W222SEAS'.            
027100     SKIP3                                                                
027200*01 -COPY W222SEAS                                                        
027300     EJECT                                                                
027400 01  UT-AREA1-START              PIC X(24)   VALUE                        
027500                                 'UT-AREA1-START '.                       
027600     SKIP2                                                                
027700                                                                          
027800*01  AREA -COPY W22226     -PRE UT1-                                      
027900     EJECT                                                                
028000 01  UT-AREA2-START              PIC X(24)   VALUE                        
028100                                 'UT-AREA2-START '.                       
028200                                                                          
028300*01  AREA -COPY W22226     -PRE UT2-                                      
028400     EJECT                                                                
028500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
028600                                                                          
028700     SKIP3                                                                
028800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
028900     SKIP3                                                                
029000 01  NYCKLAR-TILL-DLI.                                                    
029100     03  W-IDARTNR-X.                                                     
029200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
029300                                                                          
029400                                                                          
029500     03  W-KDSEGKEY-X.                                                    
029600         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
029700                                                                          
029800     03  W-TIAAAA-X.                                                      
029900         05  W-TIAAAA            PIC 9(4)   VALUE ZERO.                   
030000                                                                          
030100     03  W-IDDC-B6-X.                                                     
030200         05  W-IDDC-B6       PIC X(2)   VALUE '11'.                       
030300                                                                          
030400*                                                                         
030500     SKIP2                                                                
030600*    --- STATUS-KOD FRÅN IMS                                              
030700 01  STATUS-WS                   PIC XX.                                  
030800     88  SEGMENT-FINNS                       VALUE '  '.                  
030900     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
031000     SKIP2                                                                
031100 01  GODK-STATUSKODER.                                                    
031200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
031300     SKIP3                                                                
031400 01  SSA1                        PIC X(64).                               
031500 01  SSA2                        PIC X(64).                               
031600 01  SSA3                        PIC X(64).                               
031700     EJECT                                                                
031800*    --- IMS FUNKTIONSKODER                                               
031900*01  -COPY W0003                                                          
032000     EJECT                                                                
032100*    ---  DLI INPUT-OUTPUT AREA                                           
032200                                                                          
033400 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA-B6'.        
033500 01  DLI-IO-AREA-B601.                                                    
033600*    03  -COPY WDB601                                                     
033700                                                                          
032300 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA-K6'.        
032400     SKIP3                                                                
032500 01  DLI-IO-AREA-K6.                                                      
032600     03  IO-AREA-K6              PIC X(900)  VALUE SPACE.                 
032700     SKIP3                                                                
032800     03  K601 REDEFINES IO-AREA-K6.                                       
032900*        05  -COPY WDK601                                                 
033000     SKIP3                                                                
033100     03  K611 REDEFINES IO-AREA-K6.                                       
033200*        05  -COPY WDK611                                                 
033300     EJECT                                                                
033800     EJECT                                                                
033900                                                                          
034000 LINKAGE SECTION.                                                         
034100                                                                          
034200     EJECT                                                                
034600*01  -COPY W0008  -PRE WDB6-                                              
034700     05  FILLER                  PIC X.                                   
034800     EJECT                                                                
034300*01  -COPY W0008  -PRE WDK6-                                              
034400     05  WDK6-KEY-FB-AREA-IDARTNR       PIC S9(9) COMP-3.                 
034500     EJECT                                                                
034900*01  -COPY W0008  -PRE WDL8-                                              
035000     05  FILLER                  PIC X.                                   
035100     EJECT                                                                
035200 PROCEDURE DIVISION  USING WDB6-PCB WDK6-PCB WDL8-PCB.                    
035300     ENTRY 'DLITCBL' USING WDB6-PCB WDK6-PCB WDL8-PCB.                    
035400                                                                          
035500     PERFORM A-INIT                                                       
035600     PERFORM IMS-GU-WDB601                                                
035700     IF DCS-FLSEASBER = JA                                                
035800                                                                          
035900         PERFORM IMS-GN-WDK6                                              
036000         PERFORM UNTIL SEGMENT-SAKNAS                                     
036100           EVALUATE WDK6-SEG-NAME-FB                                      
036200             WHEN 'WDK601  '                                              
036300               MOVE WDK6-KEY-FB-AREA-IDARTNR                              
036400                             TO W-IDARTNR                                 
036500                  MOVE ART-TIFINLV                                        
036600                             TO WS-TIFINLV                                
036700                  MOVE ART-IDFKNGRP                                       
036800                             TO WS-IDFKNGRP                               
036900             WHEN 'WDK611  '                                              
037000                IF  CLAG-KDERS < 10                                       
037100                    PERFORM C-BEHANDLA-ARTIKEL                            
037200                END-IF                                                    
037300                                                                          
037400           END-EVALUATE                                                   
037500          PERFORM IMS-GN-WDK6                                             
037600       END-PERFORM                                                        
043800                                                                          
037800     END-IF                                                               
037900     PERFORM Z-FINIT                                                      
038000                                                                          
038100     MOVE ZERO TO RETURN-CODE                                             
038200     GOBACK                                                               
038300     .                                                                    
038400     EJECT                                                                
038500                                                                          
038600                                                                          
038700 A-INIT SECTION.                                                          
038800                                                                          
038900     OPEN OUTPUT W22226                                                   
039000                 W22226A                                                  
039100                                                                          
039200     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
039300     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
039400     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
039500     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
039600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
039700                                                                          
039800     MOVE 'AAMMDD'     TO DAT-KDDATFORM                                   
039900     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
040000                                                                          
040100     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
040200                         DAT-O-TIDATUM DAT-KDSVAR                         
040300                                                                          
040400     IF DAT-KDSVAR-OK                                                     
040500        MOVE DAT-TIAARP      TO DAGENS-TIAARP                             
040600                                FOREG-TIAARP                              
040700                                NAESTA-TIAARP                             
040800        MOVE DAT-TIVV        TO DAGENS-TIVV                               
040900        MOVE DAT-TIAAVV-GRP  TO DAGENS-TIAAVV-GRP                         
041000        MOVE DAT-TIAAVVD     TO DAGENS-TIAAVVD                            
041100        MOVE DAT-TISEKEL     TO DAGENS-TISS                               
041200        MOVE DAT-TIAAMMDD    TO DAGENS-TISSMMDD                           
041300     ELSE                                                                 
041400       MOVE 'SVAR 1 FRÅN WDATKONV I A SECTION EJ OK'                      
041500                         TO FELTEXT-STR                                   
041600       DISPLAY FELTEXT                                                    
041700       PERFORM S99-ABEND                                                  
041800     END-IF                                                               
041900     .                                                                    
042000     EJECT                                                                
042100                                                                          
042200                                                                          
042300 C-BEHANDLA-ARTIKEL SECTION.                                              
042400                                                                          
042500     MOVE W-IDARTNR          TO SEAS-IDARTNR                              
042600     MOVE SPACE              TO SEAS-KDSVAR                               
042700     MOVE JA                 TO SEAS-FLKVARTAL                            
042800     MOVE WS-IDFKNGRP        TO SEAS-IDFKNGRP                             
042900                                                                          
043000     CALL W222SEAS USING SEAS-W222SEAS WDL8-PCB                           
043100                                                                          
043200     IF SEAS-KDSVAR = SPACE                                               
043300                                                                          
043400        IF  SEAS-SEASON-ARTIKEL = JA                                      
043500        AND SEAS-ANT-HIST-AR > 1                                          
043600*----- SÄSONG GÄLLER                                                      
043700*-----                                                                    
043800*----- SÅ SMÅNINGOM BÖR NOG SÄSONGSARTIKLAR MED ETT                       
043900*----- ÅRS HISTORIK SKRIVAS PÅ EN EGEN FIL SÅ ATT                         
044000*----- MAN KAN TA UT EN LISTA FÖR MANUELL BEDÖMNING                       
044100*----- AV DESSA                                                           
044200                                                                          
044300           MOVE 1            TO IX                                        
044400           PERFORM UNTIL IX > 12                                          
044500             DIVIDE SEAS-RESEASON (IX)                                    
044600                             BY 100                                       
044700                             GIVING UT1-RESEASON (IX)                     
044800             ADD 1           TO IX                                        
044900           END-PERFORM                                                    
045000        ELSE                                                              
045100*----- OSÄKERHETEN ÄR FÖR STOR                                            
045200*----- SÄSONG GÄLLER EJ                                                   
045300                                                                          
045400           MOVE 1            TO IX                                        
045500           PERFORM UNTIL IX > 12                                          
045600             MOVE 1.00       TO UT1-RESEASON (IX)                         
045700             ADD 1           TO IX                                        
045800           END-PERFORM                                                    
045900                                                                          
046000        END-IF                                                            
046100                                                                          
046200*----- SKRIV POST PÅ ARTIKELFIL                                           
046300        MOVE SEAS-IDARTNR    TO UT1-IDARTNR                               
046400        MOVE SEAS-OSAKERHET  TO UT1-OSAKERHET                             
046500        MOVE SEAS-ANT-HIST-AR                                             
046600                             TO UT1-ANT-HIST-AR                           
046700        MOVE SEAS-SEASON-ARTIKEL                                          
046800                             TO UT1-SEASON-ARTIKEL                        
046900        PERFORM S01-SKRIV-W22226                                          
047000                                                                          
047100        IF SEAS-ANT-HIST-AR = 1                                           
047200        AND NOT (SEAS-RESEASON (1) = 1.00                                 
047300        AND      SEAS-RESEASON (2) = 1.00                                 
047400        AND      SEAS-RESEASON (3) = 1.00                                 
047500        AND      SEAS-RESEASON (4) = 1.00                                 
047600        AND      SEAS-RESEASON (5) = 1.00                                 
047700        AND      SEAS-RESEASON (6) = 1.00                                 
047800        AND      SEAS-RESEASON (7) = 1.00                                 
047900        AND      SEAS-RESEASON (8) = 1.00                                 
048000        AND      SEAS-RESEASON (9) = 1.00                                 
048100        AND      SEAS-RESEASON (10) = 1.00                                
048200        AND      SEAS-RESEASON (11) = 1.00                                
048300        AND      SEAS-RESEASON (12) = 1.00)                               
048400           PERFORM CA-ETT-ARS-HISTORIK                                    
048500        END-IF                                                            
048600     END-IF                                                               
048700     .                                                                    
048800     EJECT                                                                
048900                                                                          
049000                                                                          
049100 CA-ETT-ARS-HISTORIK SECTION.                                             
049200                                                                          
049300*-----                                                                    
049400*----- SÄSONGSARTIKLAR MED ETT                                            
049500*----- ÅRS HISTORIK SKRIVS PÅ EN EGEN FIL SÅ ATT                          
049600*----- MAN KAN TA UT EN LISTA FÖR MANUELL BEDÖMNING                       
049700*----- AV DESSA                                                           
049800*-----                                                                    
049900                                                                          
050000     MOVE 1                  TO IX                                        
050100     PERFORM UNTIL IX > 12                                                
050200       DIVIDE SEAS-RESEASON (IX)                                          
050300                          BY 100                                          
050400                          GIVING UT2-RESEASON (IX)                        
050500       ADD 1                 TO IX                                        
050600     END-PERFORM                                                          
050700                                                                          
050800*----- SKRIV POST FÖR MANUELL BEDÖMNING                                   
050900     MOVE SEAS-IDARTNR       TO UT2-IDARTNR                               
051000     MOVE SEAS-OSAKERHET     TO UT2-OSAKERHET                             
051100     MOVE SEAS-ANT-HIST-AR                                                
051200                             TO UT2-ANT-HIST-AR                           
051300     MOVE SEAS-SEASON-ARTIKEL                                             
051400                             TO UT2-SEASON-ARTIKEL                        
051500     PERFORM S02-SKRIV-W22226A                                            
051600     .                                                                    
051700     EJECT                                                                
051800                                                                          
051900                                                                          
052000 Z-FINIT SECTION.                                                         
052100                                                                          
052200     CLOSE W22226                                                         
052300           W22226A                                                        
052400                                                                          
052500     MOVE 'S' TO POSTSUM-OPKOD                                            
052600     CALL POSTSUM USING POSTSUM-PARM                                      
052700     .                                                                    
052800     EJECT                                                                
052900                                                                          
053000 S01-SKRIV-W22226 SECTION.                                                
053100                                                                          
053200     WRITE UT1-POST FROM UT1-AREA                                         
053300                                                                          
053400     MOVE 'W22226'   TO POSTSUM-FDNAMN                                    
053500     MOVE 'W22226D1' TO POSTSUM-DDNAMN2                                   
053600     CALL POSTSUM USING POSTSUM-PARM                                      
053700     .                                                                    
053800     EJECT                                                                
053900                                                                          
054000 S02-SKRIV-W22226A SECTION.                                               
054100                                                                          
054200     WRITE UT2-POST FROM UT2-AREA                                         
054300                                                                          
054400     MOVE 'W22226A'  TO POSTSUM-FDNAMN                                    
054500     MOVE 'W22226D2' TO POSTSUM-DDNAMN2                                   
054600     CALL POSTSUM USING POSTSUM-PARM                                      
054700     .                                                                    
054800     EJECT                                                                
054900 S99-ABEND SECTION.                                                       
055000                                                                          
055100     MOVE 'S' TO POSTSUM-OPKOD                                            
055200     CALL POSTSUM USING POSTSUM-PARM                                      
055300     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
055400     .                                                                    
055500     EJECT                                                                
055600* --- IMS SEKTIONER ---                                                   
055700     SKIP3                                                                
055800     EJECT                                                                
055900 IMS-GN-WDK6 SECTION.                                                     
056000                                                                          
056100     CALL CBLTDLI USING GN WDK6-PCB DLI-IO-AREA-K6                        
056200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
056300     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
056400     PERFORM IMS-STATUSKONTROLL                                           
056500     .                                                                    
056600     EJECT                                                                
056700 IMS-GU-WDB601    SECTION.                                                
056800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
056900          DELIMITED BY SIZE INTO SSA1                                     
057000     MOVE '  ' TO GODK-STATUSKODER                                        
057100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
057200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
057300     PERFORM IMS-STATUSKONTROLL                                           
057400     .                                                                    
057600     EJECT                                                                
057700 IMS-STATUSKONTROLL SECTION.                                              
057800                                                                          
057900     SET STATUS-IX TO 1                                                   
058000     SEARCH GODK-STATUS                                                   
058100       AT END                                                             
058200         STRING 'OTILLÅTEN RETURKOD FRÅN IMS: ' STATUS-WS                 
058300           DELIMITED BY SIZE INTO FELTEXT-STR                             
058400         DISPLAY FELTEXT                                                  
058500         CALL FELLOG                                                      
058600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
058700         CONTINUE                                                         
058800     END-SEARCH                                                           
058900     .                                                                    
059000     EJECT                                                                
059100*    -COPY WY2000P1                                                       
059200     EJECT                                                                
059300*    -COPY WY2000P2                                                       
059400     EJECT                                                                
059500*    -COPY WY2000Q3                                                       
