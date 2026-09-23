000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2222300.                                                
000400*AUTHOR.         STEFAN ÅSGÅRDEN.                                         
000500*DATE-WRITTEN.   JANUARI 2005.                                            
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000810*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET KONTROLLERAR OM KVPB-HIST SKA UPPDATERAS              
001100*        PGA ATT PUBLICERINGSVECKA HAR PASSERAT ETT ÅR                    
001200*        ELLER ATT TIPBLOCK HAR PASSERATS                                 
001300*        DVS OM KVPB-SEP HAR VARIT LÅST (HÖJBAR MEN EJ SÄNKBAR)           
001400*                                                                         
001500*        ÄNDRING MARS 2007.TEST OM PUB.VECKA PASSERAD BORTTAGEN.          
002800*                                                                         
002900*        PROGRAMMET LÄSER      WDK6                                       
003100*                                                                         
003200*    ABENDKODER:                                                          
003300*        U0016 -  SVAR FRÅN WDATKONV EJ OK                                
003500*        U1000 -  . . . .                                                 
003600*                                                                         
003700                                                                          
003800     SKIP3                                                                
003900 ENVIRONMENT DIVISION.                                                    
004000     SKIP2                                                                
004100 INPUT-OUTPUT SECTION.                                                    
004200                                                                          
004300 FILE-CONTROL.                                                            
004400     SKIP2                                                                
005200                                                                          
005300     SELECT W22223                     ASSIGN TO W22223D1.                
005400*          --- UT-FIL                                                     
005500                                                                          
005800     EJECT                                                                
005900 DATA DIVISION.                                                           
006000     SKIP3                                                                
006100 FILE SECTION.                                                            
006200     SKIP3                                                                
006300                                                                          
007900                                                                          
008000 FD  W22223                                                               
008100     RECORDING       F                                                    
008200     BLOCK CONTAINS  0.                                                   
008300                                                                          
008400*01  POST -COPY W22223  -PRE UT-   -L.                                    
008410                                                                          
008411                                                                          
008500                                                                          
009200     EJECT                                                                
009300 WORKING-STORAGE SECTION.                                                 
009400     SKIP2                                                                
009500*    -COPY WY2000W1                                                       
009600     SKIP2                                                                
009700*    -COPY WY2000W3                                                       
009800     SKIP3                                                                
009900*    -COPY WY2000W2                                                       
010000     SKIP3                                                                
010100 77  IDPGM                       PIC X(8)    VALUE 'W2222300'.            
010200 77  JA                          PIC X       VALUE 'J'.                   
010300 77  NEJ                         PIC X       VALUE 'N'.                   
010400 77  AKTIV                       PIC X       VALUE 'A'.                   
010500                                                                          
010600*    --- INDEX SAMT MAX-INDEX                                             
010700 77  FILLER                      PIC X(16)   VALUE 'INDEX'.               
010800 77  INDX                        PIC 9(2)    VALUE ZERO.                  
010810 77  IX                          PIC 9(3)    VALUE ZERO.                  
010900 77  DC-INDX                     PIC 9(2)    VALUE ZERO.                  
010910 77  PER-INDX                    PIC 9(2)    VALUE ZERO.                  
010920 77  TREND-INDX                  PIC 9(2)    VALUE ZERO.                  
011000 77  MAX-FSGFAKT                 PIC 9(2)    VALUE 11.                    
011100                                                                          
011200 77  VECKO-INDX                  PIC 9(2)    VALUE ZERO.                  
011300                                                                          
011400 01  TAB-RADIX                   PIC 9(2)    VALUE ZERO.                  
011500 77  MAX-TAB-RADIX               PIC 9(2)    VALUE ZERO.                  
011600                                                                          
011700 77  VV-INDX                     PIC 9(2)    VALUE ZERO.                  
011800 77  MAX-VV-INDX                 PIC 9(2)    VALUE 53.                    
011900                                                                          
012000*    --- SWITCHAR                                                         
012100 01  FILLER                      PIC X(16)   VALUE 'SWITCHAR'.            
012200                                                                          
012300 01  DC-POST.                                                             
012400     03  DC-PARAMETER        PIC X(4).                                    
012500         88 SAMTLIGA-DC      VALUE 'ALLA'.                                
012600         88 SAMTLIGA-SDC     VALUE 'EURO'.                                
012700         88 SAMTLIGA-NDC     VALUE 'AMER'.                                
012800         88 ENSTAKA-DC       VALUE 'DC21' 'DC22' 'DC23'                   
012900                                   'DC24' 'DC25' 'DC26'                   
012910                                   'DC3A'                                 
013000                                   'DC41' 'DC42' 'DC43' 'DC51'.           
013100                                                                          
013200     03  FILLER               PIC X(76).                                  
013300                                                                          
013400                                                                          
013500*      --- VALID IDDC CODES                                               
013600*                                                                         
013700*01    -COPY WWDC99                                                       
015700                                                                          
015800 01 DC-PARAMETER-DELAR.                                                   
015900     03 FILLER                PIC X(2)  VALUE SPACE.                      
016000     03 DC-PARAMETER-LAGER    PIC X(2)  VALUE SPACE.                      
016100                                                                          
016200 01  TREND-SW                    PIC X(5)    VALUE SPACE.                 
016300     88  INGEN-TREND                         VALUE 'INGEN'.               
016400     88  SVAG-TREND                          VALUE 'SVAG '.               
016500     88  STARK-TREND                         VALUE 'STARK'.               
016600                                                                          
016700 01  INDX-SW                     PIC X       VALUE 'N'.                   
016800     88  INDX-HITTAT                         VALUE 'J'.                   
016900                                                                          
017000 01  ARTIKEL-SW                  PIC X       VALUE 'N'.                   
017100     88  ARTIKEL-SKALL-FORAENDRAS            VALUE 'J'.                   
017200                                                                          
017300 01  MANUELL-PROGNOS-SW          PIC X       VALUE 'N'.                   
017400     88  MANUELL-PROGNOS-SATT                VALUE 'J'.                   
017500                                                                          
017600*    --- ARBETSFÄLT                                                       
017700 01  FILLER                      PIC X(16)   VALUE 'ARBETSFÄLT'.          
017800 01  ARBETSFAELT.                                                         
017900     03  PERIODTABELL            OCCURS 13.                               
018000         05 TABELL-TIAARP        PIC  9(4)    VALUE ZERO.                 
018100         05 TABELL-FORSTA-TIAAVV PIC  9(2)    VALUE ZERO.                 
018200         05 TABELL-SISTA-TIAAVV  PIC  9(2)    VALUE ZERO.                 
018300         05 TABELL-KVOI        PIC S9(9)V9(1)  VALUE ZERO COMP-3.         
018400                                                                          
018500     03  SLUT-VV                 PIC 9(2)    VALUE ZERO.                  
018600     03  START-VV                PIC 9(2)    VALUE ZERO.                  
018700                                                                          
018800     03  WS-NOLL                 PIC 9(4)    VALUE ZERO.                  
018810     03  WS-TIAAVV               PIC 9(4)    VALUE ZERO.                  
018900     03  FILLER REDEFINES WS-TIAAVV.                                      
019000         05 WS-TIAA              PIC 9(2).                                
019100         05 WS-TIVV              PIC 9(2).                                
019110     03  WS-IDFKNGRP             PIC S9(5)    VALUE ZERO COMP-3.          
019120     03  WS2-TIFINLV             PIC 9(5).                                
019130     03  FILLER REDEFINES WS2-TIFINLV.                                    
019140         05  WS2-TIFINLV-AAVV    PIC 9(4).                                
019150         05  FILLER              PIC 9(1).                                
019160     03  WS3-TIFINLV             PIC 9(6).                                
019170     03  WS-TIPBLOCK-AAVVD       PIC 9(5)    VALUE ZERO.                  
019200                                                                          
019300     03  FOREG-TIAARP            PIC  9(4)   VALUE ZERO.                  
019400     03  FILLER REDEFINES FOREG-TIAARP.                                   
019500         05 FOREG-TIAA           PIC  9(2).                               
019600         05 FOREG-TIRP           PIC  9(2).                               
019610                                                                          
019620     03 DAGENS-TISSSSMMDD        PIC 9(8)       VALUE ZERO.               
019630     03 DAGENS-TISSSSMMDD-GRP    REDEFINES DAGENS-TISSSSMMDD.             
019640       05 DAGENS-TISS            PIC 9(2).                                
019650       05 DAGENS-TISSMMDD        PIC 9(6).                                
019700                                                                          
019800     03  DAGENS-TIAARP           PIC  9(4)   VALUE ZERO.                  
019900     03  FILLER REDEFINES DAGENS-TIAARP.                                  
020000         05 DAGENS-TIAA          PIC  9(2).                               
020100         05 DAGENS-TIRP          PIC  9(2).                               
020200                                                                          
020300     03  NAESTA-TIAARP           PIC  9(4)   VALUE ZERO.                  
020400     03  FILLER REDEFINES NAESTA-TIAARP.                                  
020500         05 NAESTA-TIAA          PIC  9(2).                               
020600         05 NAESTA-TIRP          PIC  9(2).                               
020700                                                                          
020800     03  SEASON-TIAARP           PIC  9(4)   VALUE ZERO.                  
020900     03  FILLER REDEFINES SEASON-TIAARP.                                  
021000         05 SEASON-TIAA          PIC  9(2).                               
021100         05 SEASON-TIRP          PIC  9(2).                               
021200                                                                          
021300     03  DAGENS-TIAAVVD          PIC  9(5)   VALUE ZERO.                  
021400     03  FILLER REDEFINES DAGENS-TIAAVVD.                                 
021500         05 DAGENS-TIAAVVD-AA    PIC  9(2).                               
021600         05 DAGENS-TIAAVVD-VV    PIC  9(2).                               
021700         05 DAGENS-TIAAVVD-D     PIC  9(1).                               
021800                                                                          
021900     03  DAGENS-TIAAVV-GRP       PIC  9(4)   VALUE ZERO.                  
022000                                                                          
022100                                                                          
022200     03  DAGENS-TIAAVVD-LAST-YEAR        PIC  9(5) VALUE ZERO.            
022300     03  FILLER REDEFINES DAGENS-TIAAVVD-LAST-YEAR.                       
022400         05 DAGENS-TIAAVVD-LAST-YEAR-AA  PIC 9(2).                        
022500         05 DAGENS-TIAAVVD-LAST-YEAR-VV  PIC 9(2).                        
022600         05 DAGENS-TIAAVVD-LAST-YEAR-D   PIC 9(1).                        
022700                                                                          
022800     03  DAGENS-TIVV             PIC  9(2)   VALUE ZERO.                  
022900                                                                          
023000     03  WS-ANTAL-VECKOR         PIC  9(2)      VALUE ZERO.               
023100     03  WS-VECKO-IO             PIC S9(9)V9 VALUE ZERO COMP-3.           
023200     03  WS-TEST-TIPBDAT         PIC S9(7)   VALUE ZERO COMP-3.           
023300     03  WS-TIPBDAT-TIAARP       PIC  9(4)      VALUE ZERO.               
023400     03  WS-FORSTA-TIAAVV        PIC  9(4)      VALUE ZERO.               
023500     03  WS-SISTA-TIAAVV         PIC  9(4)      VALUE ZERO.               
023600     03  WS-DAT-TIAAVV           PIC  9(4)      VALUE ZERO.               
023700     03  WS-ONORM-OI-GRAENS-PB   PIC S9(9)V9(1) VALUE ZERO COMP-3.        
023800     03  WS-KVOI-SEASON          PIC S9(9)V9(1) VALUE ZERO COMP-3.        
023900     03  WS-KVOI-TOT             PIC S9(11)V9(2)                          
023910                                                VALUE ZERO COMP-3.        
024000     03  WS-NY-KVPB-SEP          PIC S9(6)V9(2) VALUE ZERO COMP-3.        
024100     03  NY-KVPB-SEP             PIC S9(6)V9(1) VALUE ZERO COMP-3.        
024200     03  WS-PREL-KVPB-SEP        PIC S9(6)V9(2) VALUE ZERO COMP-3.        
024300     03  WS-MEDEL-KVPB-SEP       PIC S9(6)V9(2) VALUE ZERO COMP-3.        
024400     03  WS-ANTAL-FAKTORER-STOERRE-NOLL PIC S9(7)          COMP-3.        
024500     03  WS-KVOTEN               PIC S9(5)V9(2) VALUE ZERO COMP-3.        
024600     03  WS-TIFINLV              PIC S9(5)V     VALUE ZERO COMP-3.        
024610     03  WS-VECKA-I-AKT-PERIOD   PIC  9(2)      VALUE ZERO.               
024700                                                                          
024701     03  WS-DC-PROGFAKT-VKA.                                              
024702       05 WS-PROGFAKT-DC  OCCURS 11.                                      
024703        07 WS-PROGFAKT-PER    OCCURS 13.                                  
024704           09 WS-DC-PERIODTREND-VKA.                                      
024705               11 WS-DC-NORMAL-VKA       PIC 9V9(4).                      
024706               11 WS-DC-SVAG-TREND-VKA   PIC 9V9(4).                      
024707               11 WS-DC-STARK-TREND-VKA  PIC 9V9(4).                      
024708           09 FILLER REDEFINES WS-DC-PERIODTREND-VKA.                     
024709               11 WS-DC-TRENDFAKT-VKA    PIC 9V9(4) OCCURS 3.             
024710     03  WS-DC-PROGFAKT-MAX      PIC 9(2)       VALUE 11.                 
024750                                                                          
024800     EJECT                                                                
024900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
025000 01  FILLER REDEFINES DAGENS-DATUM.                                       
025100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
025200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
025300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
025400     EJECT                                                                
025500                                                                          
025600* INFO OM KÖRTYP(DAG) FRPN CONSTANTMEDLEM VALD AV JCL'EN                  
025700* INFON KOMMER SOM FIL D1                                                 
025800                                                                          
025900 01  W271TYP-POST.                                                        
026000     03  TYP-PARAMETER        PIC X(4).                                   
026100         88 DAY-KORNING       VALUE 'DAY '.                               
026200         88 WEEK-KORNING      VALUE 'WEEK'.                               
026300         88 ACC-KORNING       VALUE 'ACC '.                               
026400     03  FILLER               PIC X(76).                                  
026500                                                                          
026600 01  DYNAMISKA-SUBPROGRAM.                                                
026700*                                                                         
026800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
026900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
027100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
027200     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
027300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
027400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
027401     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
027500     SKIP2                                                                
027600*    --- PARAMETRAR TILL ABEND                                            
027700                                                                          
027800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
027900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
028000     SKIP2                                                                
028100 01  FELTEXT.                                                             
028200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
028300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
028400     EJECT                                                                
028500*    --- PARAMETRAR TILL DATKORT                                          
028600*                                                                         
028700 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W22223'.              
028800     SKIP2                                                                
028900 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
029000     SKIP2                                                                
029100*01  -COPY WDATKORT                                                       
029200     EJECT                                                                
029300*    --- PARAMETRAR TILL POSTSUM                                          
029400*                                                                         
029500*01  -COPY W0005   -PRE  POSTSUM-                                         
029600     EJECT                                                                
029700*    --- PARAMETRAR TILL WDATKONV                                         
029800*                                                                         
029900*01  -COPY WDATAREA                                                       
029950     EJECT                                                                
029960*    --- PARAMETRAR TILL W009VADD                                         
029970*                                                                         
029980 01  W009VADD-AREA.                                                       
029990     03  DATUM-AAVV              PIC S9(5) COMP-3.                        
029991     03  ANTAL                   PIC S9(3) COMP-3.                        
029992*                                                                         
030800     EJECT                                                                
030900 01  UT-AREA1-START              PIC X(24)   VALUE                        
031000                                 'UT-AREA1-START '.                       
031100     SKIP2                                                                
031200                                                                          
031300*01  AREA -COPY W22223     -PRE UT-                                       
032000     EJECT                                                                
032100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
032200                                                                          
032300     SKIP3                                                                
032400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
032500     SKIP3                                                                
032600 01  NYCKLAR-TILL-DLI.                                                    
032700     03  W-IDARTNR-X.                                                     
032800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
032900                                                                          
033000     03  W-IDDC-X.                                                        
033100         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
033200                                                                          
033300     03  W-KDSEGKEY-X.                                                    
033400         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
033401                                                                          
033410     03  W-TIAAAA-X.                                                      
033420         05  W-TIAAAA            PIC 9(4)   VALUE ZERO.                   
033500                                                                          
033600*                                                                         
033700     SKIP2                                                                
033800*    --- STATUS-KOD FRÅN IMS                                              
033900 01  STATUS-WS                   PIC XX.                                  
034000     88  SEGMENT-FINNS                       VALUE '  '.                  
034100     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
034200     SKIP2                                                                
034300 01  GODK-STATUSKODER.                                                    
034400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
034500     SKIP3                                                                
034600 01  SSA1                        PIC X(64).                               
034700 01  SSA2                        PIC X(64).                               
034710 01  SSA3                        PIC X(64).                               
034800     EJECT                                                                
034900*    --- IMS FUNKTIONSKODER                                               
035000*01  -COPY W0003                                                          
035100     EJECT                                                                
035200*    ---  DLI INPUT-OUTPUT AREA                                           
035210                                                                          
035220 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA-K6'.        
035230     SKIP3                                                                
035240 01  DLI-IO-AREA-K6.                                                      
035250     03  IO-AREA-K6              PIC X(900)  VALUE SPACE.                 
035260     SKIP3                                                                
035270     03  K601 REDEFINES IO-AREA-K6.                                       
035280*        05  -COPY WDK601                                                 
035290     SKIP3                                                                
035291     03  K611 REDEFINES IO-AREA-K6.                                       
035292*        05  -COPY WDK611                                                 
035293     EJECT                                                                
036110                                                                          
038500     EJECT                                                                
038600                                                                          
038700 LINKAGE SECTION.                                                         
038800                                                                          
038900     EJECT                                                                
039000*01  -COPY W0008  -PRE WDK6-                                              
039100     05  WDK6-KEY-FB-AREA-IDARTNR       PIC S9(9) COMP-3.                 
039800     EJECT                                                                
039900 PROCEDURE DIVISION  USING WDK6-PCB.                                      
040000     ENTRY 'DLITCBL' USING WDK6-PCB.                                      
040100                                                                          
040200     PERFORM A-INIT                                                       
040300     PERFORM IMS-GN-WDK6                                                  
040400     PERFORM UNTIL SEGMENT-SAKNAS                                         
040500       EVALUATE WDK6-SEG-NAME-FB                                          
040600         WHEN 'WDK601  '                                                  
040700           MOVE WDK6-KEY-FB-AREA-IDARTNR                                  
040701                             TO W-IDARTNR                                 
040720              MOVE ART-TIFINLV                                            
040730                             TO WS-TIFINLV                                
040740              MOVE ART-IDFKNGRP                                           
040750                             TO WS-IDFKNGRP                               
040800         WHEN 'WDK611  '                                                  
040900                                                                          
040910              IF  CLAG-KDERS < 10                                         
040911              AND CLAG-FLMPB = JA                                         
040920              AND WS-TIFINLV > ZERO                                       
040930                  PERFORM C-BEHANDLA-ARTIKEL                              
040960              END-IF                                                      
043800                                                                          
043900       END-EVALUATE                                                       
044000       PERFORM IMS-GN-WDK6                                                
044100     END-PERFORM                                                          
044200                                                                          
044300     PERFORM Z-FINIT                                                      
044400                                                                          
044500     MOVE ZERO TO RETURN-CODE                                             
044600     GOBACK                                                               
044700     .                                                                    
044800     EJECT                                                                
044900                                                                          
045000                                                                          
049400 A-INIT SECTION.                                                          
049500                                                                          
049800     OPEN OUTPUT W22223                                                   
050000                                                                          
050100     ACCEPT DAGENS-DATUM FROM DATE                                        
051100                                                                          
051200     MOVE 'AAMMDD'     TO DAT-KDDATFORM                                   
051300     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
051400                                                                          
051500     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
051600                         DAT-O-TIDATUM DAT-KDSVAR                         
051700                                                                          
051800     IF DAT-KDSVAR-OK                                                     
051900        MOVE DAT-TIAARP      TO DAGENS-TIAARP                             
052000                                FOREG-TIAARP                              
052100                                NAESTA-TIAARP                             
052200        MOVE DAT-TIVV        TO DAGENS-TIVV                               
052300        MOVE DAT-TIAAVV-GRP  TO DAGENS-TIAAVV-GRP                         
052400        MOVE DAT-TIAAVVD     TO DAGENS-TIAAVVD                            
052410        MOVE DAT-TISEKEL     TO DAGENS-TISS                               
052420        MOVE DAT-TIAAMMDD    TO DAGENS-TISSMMDD                           
052500     ELSE                                                                 
052600       MOVE 'SVAR 1 FRÅN WDATKONV I A SECTION EJ OK'                      
052700                         TO FELTEXT-STR                                   
052800       DISPLAY FELTEXT                                                    
052900       PERFORM S99-ABEND                                                  
053000     END-IF                                                               
054500     .                                                                    
054600     EJECT                                                                
074800                                                                          
074900                                                                          
075000 C-BEHANDLA-ARTIKEL SECTION.                                              
075100                                                                          
075105     IF CLAG-TIPBLOCK > ZERO                                              
075106       PERFORM S05-KNV-TIPBLOCK                                           
075107     ELSE                                                                 
075108       MOVE ZERO             TO WS-TIPBLOCK-AAVVD                         
075109     END-IF                                                               
075110                                                                          
075111     MOVE WS-TIPBLOCK-AAVVD  TO TMP1-YYWWD                                
075112     MOVE DAGENS-TIAAVVD     TO TMP2-YYWWD                                
075113     MOVE CLAG-TIPBDAT       TO TMP3-YYWWD                                
075114     PERFORM WY2000Q2                                                     
075115*                                                                         
075116*    KONTROLL OM TIPBLOCK HAR PASSERAT DAGENS-DATUM :                     
075117*             TMP1-YYWWD                                                  
075118*                                                                         
075121*                                                                         
075122     IF ((TMP1-YYWWD < TMP2-YYWWD                                         
075123     AND  TMP1-YYWWD > TMP3-YYWWD)                                        
075124     AND  CLAG-TIPBLOCK > ZERO)                                           
075127       MOVE W-IDARTNR        TO UT-IDARTNR                                
075128       MOVE CLAG-KVPB-SEP    TO UT-KVPB-HIST                              
075129       IF (TMP1-YYWWD < TMP2-YYWWD                                        
075130       AND TMP1-YYWWD > TMP3-YYWWD)                                       
075131       AND CLAG-TIPBLOCK > ZERO                                           
075132*                                                                         
075133*    TIPBLOCK HAR PASSERAT DAGENS-DATUM                                   
075134*                                                                         
075135         MOVE WS-TIPBLOCK-AAVVD                                           
075136                             TO UT-TIPBDAT                                
075145         PERFORM S01-SKRIV-W22223                                         
075147        END-IF                                                            
075150     END-IF                                                               
081200     .                                                                    
081300     EJECT                                                                
098645                                                                          
098646                                                                          
098650 Z-FINIT SECTION.                                                         
098700                                                                          
099000     CLOSE W22223                                                         
099200                                                                          
099300     MOVE 'S' TO POSTSUM-OPKOD                                            
099400     CALL POSTSUM USING POSTSUM-PARM                                      
099500     .                                                                    
099600     EJECT                                                                
130800                                                                          
130900 S01-SKRIV-W22223 SECTION.                                                
131000                                                                          
131100     WRITE UT-POST FROM UT-AREA                                           
131200                                                                          
131300     MOVE 'W22223'   TO POSTSUM-FDNAMN                                    
131400     MOVE 'W22223D1' TO POSTSUM-DDNAMN2                                   
131500     CALL POSTSUM USING POSTSUM-PARM                                      
131600     .                                                                    
133300     EJECT                                                                
133330 S05-KNV-TIPBLOCK SECTION.                                                
133337                                                                          
133338*    RÄKNA OM TIPBLOCK TILL ÅÅVVD                                         
133339     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
133340     MOVE CLAG-TIPBLOCK      TO DAT-I-TIDATUM                             
133342                                                                          
133343     CALL WDATKONV USING DAT-KDDATFORM                                    
133344                         DAT-I-TIDATUM                                    
133345                         DAT-O-TIDATUM                                    
133346                         DAT-KDSVAR                                       
133347                                                                          
133348     IF DAT-KDSVAR-OK                                                     
133349       MOVE DAT-TIAAVVD      TO WS-TIPBLOCK-AAVVD                         
133350     ELSE                                                                 
133351       MOVE 'SVAR WDATKONV   I S05 SECTION'                               
133352                             TO FELTEXT-STR                               
133353       DISPLAY FELTEXT                                                    
133354       PERFORM S99-ABEND                                                  
133355     END-IF                                                               
133356     .                                                                    
133357     EJECT                                                                
133360                                                                          
133400 S99-ABEND SECTION.                                                       
133500                                                                          
133600     MOVE 'S' TO POSTSUM-OPKOD                                            
133700     CALL POSTSUM USING POSTSUM-PARM                                      
133800     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
133900     .                                                                    
134000     EJECT                                                                
134100* --- IMS SEKTIONER ---                                                   
134200     SKIP3                                                                
134300     EJECT                                                                
136400 IMS-GN-WDK6 SECTION.                                                     
136500                                                                          
136600     CALL CBLTDLI USING GN WDK6-PCB DLI-IO-AREA-K6                        
136700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
136800     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
136900     PERFORM IMS-STATUSKONTROLL                                           
137000     .                                                                    
137100     EJECT                                                                
138320     EJECT                                                                
138400 IMS-STATUSKONTROLL SECTION.                                              
138500                                                                          
138600     SET STATUS-IX TO 1                                                   
138700     SEARCH GODK-STATUS                                                   
138800       AT END                                                             
138900         STRING 'OTILLÅTEN RETURKOD FRÅN IMS: ' STATUS-WS                 
139000           DELIMITED BY SIZE INTO FELTEXT-STR                             
139100         DISPLAY FELTEXT                                                  
139200         CALL FELLOG                                                      
139300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
139400         CONTINUE                                                         
139500     END-SEARCH                                                           
139600     .                                                                    
139700     EJECT                                                                
139800*    -COPY WY2000P1                                                       
139900     EJECT                                                                
140000*    -COPY WY2000P2                                                       
140100     EJECT                                                                
140200*    -COPY WY2000Q2                                                       
140300     EJECT                                                                
140400*    -COPY WY2000Q3                                                       
