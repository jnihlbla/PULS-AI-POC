000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2223500.                                                
000400*AUTHOR.         STEFAN ANDREASSON.                                       
000500*DATE-WRITTEN.   JANUARI 2000.                                            
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        PROGRAMMET LARMAR VID STORA ELLER SMÅ UTTAG                      
001200*                                                                         
001300*                                                                         
001400*        PROGRAMMET LÄSER      WDK6                                       
001500*                              WDL8                                       
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*        U0016 -  SVAR FRÅN WDATKONV EJ OK                                
001900*              -  "ÖVERSÄTTNING" AV LAGER TILL DC-INDX SAKNAS             
002000*        U1000 -  . . . .                                                 
002100*                                                                         
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000*          --- UPPGIFT OM DAG, VECKO- ELLER PERIODKÖRNING                 
003100                                                                          
003200     SELECT W22235                     ASSIGN TO W22235D1.                
003300*          --- UT-FIL                                                     
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
003900                                                                          
004000                                                                          
004100 FD  W22235                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500*01  POST -COPY W22235  -PRE UT-    -L.                                   
004600                                                                          
004700     EJECT                                                                
004800 WORKING-STORAGE SECTION.                                                 
004900     SKIP2                                                                
005000*    -COPY WY2000W1                                                       
005100     SKIP2                                                                
005200*    -COPY WY2000W3                                                       
005300     SKIP3                                                                
005400*    -COPY WY2000W2                                                       
005500     SKIP3                                                                
005600 77  IDPGM                       PIC X(8)    VALUE 'W2223500'.            
005700 77  JA                          PIC X       VALUE 'J'.                   
005800 77  NEJ                         PIC X       VALUE 'N'.                   
005900 77  AKTIV                       PIC X       VALUE 'A'.                   
006000                                                                          
006100*    --- INDEX SAMT MAX-INDEX                                             
006200 77  FILLER                      PIC X(16)   VALUE 'INDEX'.               
006300 77  INDX                        PIC 9(2)    VALUE ZERO.                  
006400 77  DC-INDX                     PIC 9(2)    VALUE ZERO.                  
006500 77  PER-INDX                    PIC 9(2)    VALUE ZERO.                  
006600 77  TREND-INDX                  PIC 9(2)    VALUE ZERO.                  
006700 77  MAX-FSGFAKT                 PIC 9(2)    VALUE 7.                     
006800                                                                          
006900 77  VECKO-INDX                  PIC 9(2)    VALUE ZERO.                  
007000                                                                          
007100 01  PER-IX                      PIC 9(2)    VALUE ZERO.                  
007200 01  LARM-IX                     PIC 9(2)    VALUE ZERO.                  
007300 01  TAB-RADIX                   PIC 9(2)    VALUE ZERO.                  
007400 77  MAX-TAB-RADIX               PIC 9(2)    VALUE 12.                    
007500                                                                          
007600 77  VV-INDX                     PIC 9(2)    VALUE ZERO.                  
007700 77  MAX-VV-INDX                 PIC 9(2)    VALUE 53.                    
007800                                                                          
007900*    --- SWITCHAR                                                         
008000 01  FILLER                      PIC X(16)   VALUE 'SWITCHAR'.            
008100                                                                          
008200 01  DC-POST.                                                             
008300     03  DC-PARAMETER        PIC X(4).                                    
008400         88 SAMTLIGA-DC      VALUE 'ALLA'.                                
008500         88 SAMTLIGA-SDC     VALUE 'EURO'.                                
008600         88 SAMTLIGA-NDC     VALUE 'AMER'.                                
008700         88 ENSTAKA-DC       VALUE 'DC21' 'DC22' 'DC23'                   
008800                                   'DC24' 'DC25' 'DC26'                   
008900                                   'DC3A'                                 
009000                                   'DC41' 'DC42' 'DC43' 'DC51'.           
009100                                                                          
009200     03  FILLER               PIC X(76).                                  
009300                                                                          
009400                                                                          
009500*      --- VALID IDDC CODES                                               
009600*                                                                         
009700*01    -COPY WWDC99                                                       
009800                                                                          
009900*                                                                         
010000*01    -COPY WWPRODSL                                                     
010100                                                                          
010200 01 DC-PARAMETER-DELAR.                                                   
010300     03 FILLER                PIC X(2)  VALUE SPACE.                      
010400     03 DC-PARAMETER-LAGER    PIC X(2)  VALUE SPACE.                      
010500                                                                          
010600 01  TREND-SW                    PIC X(5)    VALUE SPACE.                 
010700     88  INGEN-TREND                         VALUE 'INGEN'.               
010800     88  SVAG-TREND                          VALUE 'SVAG '.               
010900     88  STARK-TREND                         VALUE 'STARK'.               
011000                                                                          
011100 01  INDX-SW                     PIC X       VALUE 'N'.                   
011200     88  INDX-HITTAT                         VALUE 'J'.                   
011300                                                                          
011400 01  ARTIKEL-SW                  PIC X       VALUE 'N'.                   
011500     88  ARTIKEL-SKALL-FORAENDRAS            VALUE 'J'.                   
011600                                                                          
011700 01  MANUELL-PROGNOS-SW          PIC X       VALUE 'N'.                   
011800     88  MANUELL-PROGNOS-SATT                VALUE 'J'.                   
011900                                                                          
012000*    --- ARBETSFÄLT                                                       
012100 01  FILLER                      PIC X(16)   VALUE 'ARBETSFÄLT'.          
012200 01  ARBETSFAELT.                                                         
012300     03  PERIODTABELL            OCCURS 13.                               
012400         05 TABELL-TIAARP        PIC  9(4)    VALUE ZERO.                 
012500         05 TABELL-FORSTA-TIAAVV PIC  9(2)    VALUE ZERO.                 
012600         05 TABELL-SISTA-TIAAVV  PIC  9(2)    VALUE ZERO.                 
012700         05 TABELL-KVOI        PIC S9(9)V9(1)  VALUE ZERO COMP-3.         
012800                                                                          
012900     03  SLUT-VV                 PIC 9(2)    VALUE ZERO.                  
013000     03  START-VV                PIC 9(2)    VALUE ZERO.                  
013100                                                                          
013200     03  WS-NOLL                 PIC 9(4)    VALUE ZERO.                  
013300     03  WS-LEDTID               PIC 9(5)    VALUE ZERO.                  
013400     03  WS-LEDTIDSBEHOV         PIC 9(9)    VALUE ZERO.                  
013500     03  WS-SALDO                PIC S9(9)   VALUE ZERO.                  
013600     03  WS-VECKO-PB             PIC 9(5)    VALUE ZERO.                  
013700     03  WS-KDSORT               PIC X(2)    VALUE SPACE.                 
013800     03  WS-ANTAL-C-BEH          PIC 9(9)    VALUE ZERO.                  
013900     03  WS-TIAAVV               PIC 9(4)    VALUE ZERO.                  
014000     03  FILLER REDEFINES WS-TIAAVV.                                      
014100         05 WS-TIAA              PIC 9(2).                                
014200         05 WS-TIVV              PIC 9(2).                                
014300                                                                          
014400     03  FOREG-TIAARP            PIC  9(4)   VALUE ZERO.                  
014500     03  FILLER REDEFINES FOREG-TIAARP.                                   
014600         05 FOREG-TIAA           PIC  9(2).                               
014700         05 FOREG-TIRP           PIC  9(2).                               
014800                                                                          
014900     03 DAGENS-TISSSSMMDD        PIC 9(8)       VALUE ZERO.               
015000     03 DAGENS-TISSSSMMDD-GRP    REDEFINES DAGENS-TISSSSMMDD.             
015100       05 DAGENS-TISS            PIC 9(2).                                
015200       05 DAGENS-TISSMMDD        PIC 9(6).                                
015300                                                                          
015400     03  DAGENS-TIAARP           PIC  9(4)   VALUE ZERO.                  
015500     03  FILLER REDEFINES DAGENS-TIAARP.                                  
015600         05 DAGENS-TIAA          PIC  9(2).                               
015700         05 DAGENS-TIRP          PIC  9(2).                               
015800                                                                          
015900     03  NAESTA-TIAARP           PIC  9(4)   VALUE ZERO.                  
016000     03  FILLER REDEFINES NAESTA-TIAARP.                                  
016100         05 NAESTA-TIAA          PIC  9(2).                               
016200         05 NAESTA-TIRP          PIC  9(2).                               
016300                                                                          
016400     03  SEASON-TIAARP           PIC  9(4)   VALUE ZERO.                  
016500     03  FILLER REDEFINES SEASON-TIAARP.                                  
016600         05 SEASON-TIAA          PIC  9(2).                               
016700         05 SEASON-TIRP          PIC  9(2).                               
016800                                                                          
016900     03  DAGENS-TIAAVVD          PIC  9(5)   VALUE ZERO.                  
017000     03  FILLER REDEFINES DAGENS-TIAAVVD.                                 
017100         05 DAGENS-TIAAVVD-AA    PIC  9(2).                               
017200         05 DAGENS-TIAAVVD-VV    PIC  9(2).                               
017300         05 DAGENS-TIAAVVD-D     PIC  9(1).                               
017400                                                                          
017500     03  DAGENS-TIAAVV-GRP       PIC  9(4)   VALUE ZERO.                  
017600                                                                          
017700                                                                          
017800     03  DAGENS-TIAAVVD-LAST-YEAR        PIC  9(5) VALUE ZERO.            
017900     03  FILLER REDEFINES DAGENS-TIAAVVD-LAST-YEAR.                       
018000         05 DAGENS-TIAAVVD-LAST-YEAR-AA  PIC 9(2).                        
018100         05 DAGENS-TIAAVVD-LAST-YEAR-VV  PIC 9(2).                        
018200         05 DAGENS-TIAAVVD-LAST-YEAR-D   PIC 9(1).                        
018300                                                                          
018400     03  DAGENS-TIVV             PIC  9(2)   VALUE ZERO.                  
018500                                                                          
018600     03  WS-ANTAL-VECKOR         PIC  9(2)      VALUE ZERO.               
018700     03  WS-VECKO-IO             PIC S9(9)V9 VALUE ZERO COMP-3.           
018800     03  WS-TEST-TIPBDAT         PIC S9(7)   VALUE ZERO COMP-3.           
018900     03  WS-TIPBDAT-TIAARP       PIC  9(4)      VALUE ZERO.               
019000     03  WS-FORSTA-TIAAVV        PIC  9(4)      VALUE ZERO.               
019100     03  WS-SISTA-TIAAVV         PIC  9(4)      VALUE ZERO.               
019200     03  WS-DAT-TIAAVV           PIC  9(4)      VALUE ZERO.               
019300     03  WS-ONORM-OI-GRAENS-PB   PIC S9(9)V9(1) VALUE ZERO COMP-3.        
019400     03  WS-KVOI-SEASON          PIC S9(9)V9(1) VALUE ZERO COMP-3.        
019500     03  WS-KVOI-TOT             PIC S9(11)V9(2)                          
019600                                                VALUE ZERO COMP-3.        
019700     03  WS-NY-KVPB-SEP          PIC S9(6)V9(2) VALUE ZERO COMP-3.        
019800     03  NY-KVPB-SEP             PIC S9(6)V9(1) VALUE ZERO COMP-3.        
019900     03  WS-PREL-KVPB-SEP        PIC S9(6)V9(2) VALUE ZERO COMP-3.        
020000     03  WS-MEDEL-KVPB-SEP       PIC S9(6)V9(2) VALUE ZERO COMP-3.        
020100     03  WS-ANTAL-FAKTORER-STOERRE-NOLL PIC S9(7)          COMP-3.        
020200     03  WS-KVOTEN               PIC S9(5)V9(2) VALUE ZERO COMP-3.        
020300     03  WS-TIFINLV              PIC S9(5)V     VALUE ZERO COMP-3.        
020400     03  WS-VECKA-I-AKT-PERIOD   PIC  9(2)      VALUE ZERO.               
020500     03  WS-IDLEVNR              PIC  X(5)      VALUE SPACE.              
020600     03  WS-GRAENS-2V-HOEG       PIC S9(7)V9(2) VALUE ZERO.               
020700     03  WS-GRAENS-4V-HOEG       PIC S9(7)V9(2) VALUE ZERO.               
020800     03  WS-GRAENS-4V-LAAG       PIC S9(7)V9(2) VALUE ZERO.               
020900     03  WS-KVPB-2V              PIC S9(7)V9(2) VALUE ZERO.               
021000     03  WS-KVPB-4V              PIC S9(7)V9(2) VALUE ZERO.               
021100     03  WS-KVPB-SEP-VECKA       PIC S9(7)V9(2) VALUE ZERO.               
021200     03  WS-OI-TOT-2V            PIC S9(9)V9    VALUE ZERO.               
021300     03  WS-OI-TOT-4V            PIC S9(9)V9    VALUE ZERO.               
021400     03  WS-LARM-A               PIC X          VALUE SPACE.              
021500     03  WS-LARM-B               PIC X          VALUE SPACE.              
021600     03  WS-LARM-C               PIC X          VALUE SPACE.              
021700     03  WS-LARM-D               PIC X          VALUE SPACE.              
021800                                                                          
021900     03  WS-LARMVECKOR.                                                   
022000        05 FILLER             OCCURS 4.                                   
022100           07 WS-KVOI-PROG               PIC S9(7)  VALUE ZERO.           
022200           07 WS-RESEASON                PIC 9V9(2) VALUE ZERO.           
022300           07 WS-TIRP                    PIC 9(2)   VALUE ZERO.           
022400                                                                          
022500     EJECT                                                                
022600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
022700 01  FILLER REDEFINES DAGENS-DATUM.                                       
022800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
022900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
023000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
023100     EJECT                                                                
023200                                                                          
023300 01      TYP-PARAMETER        PIC X(4).                                   
023400         88 DAY-KORNING       VALUE 'DAY '.                               
023500         88 WEEK-KORNING      VALUE 'WEEK'.                               
023600         88 ACC-KORNING       VALUE 'ACC '.                               
023700                                                                          
023800 01  DYNAMISKA-SUBPROGRAM.                                                
023900*                                                                         
024000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
024100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
024200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
024300     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
024400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
024500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
024600     SKIP2                                                                
024700*    --- PARAMETRAR TILL ABEND                                            
024800                                                                          
024900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
025000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
025100     SKIP2                                                                
025200 01  FELTEXT.                                                             
025300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
025400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
025500     EJECT                                                                
025600*    --- PARAMETRAR TILL DATKORT                                          
025700*                                                                         
025800 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W22235'.              
025900     SKIP2                                                                
026000 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
026100     SKIP2                                                                
026200*01  -COPY WDATKORT                                                       
026300     EJECT                                                                
026400*    --- PARAMETRAR TILL POSTSUM                                          
026500*                                                                         
026600*01  -COPY W0005   -PRE  POSTSUM-                                         
026700     EJECT                                                                
026800*    --- PARAMETRAR TILL WDATKONV                                         
026900*                                                                         
027000*01  -COPY WDATAREA                                                       
027100     EJECT                                                                
027200*    --- TABELL MED LARMFAKTORER                                          
027300*                                                                         
027400*01  -COPY W222LARM                                                       
027500     EJECT                                                                
027600 01  UT-AREA-START              PIC X(24)   VALUE                         
027700                                 'UT-AREA-START  '.                       
027800     SKIP2                                                                
027900                                                                          
028000*01  AREA -COPY W22235     -PRE UT-                                       
028100     EJECT                                                                
028200 01  UT-AREA2-START             PIC X(24)   VALUE                         
028300                                 'UT-AREA2-START '.                       
028400     SKIP2                                                                
028500                                                                          
028600*01  AREA -COPY W22228     -PRE UT2-                                      
028700     EJECT                                                                
028800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
028900                                                                          
029000     SKIP3                                                                
029100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
029200     SKIP3                                                                
029300 01  NYCKLAR-TILL-DLI.                                                    
029400     03  W-IDARTNR-X.                                                     
029500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
029600                                                                          
029700     03  W-IDDC-X.                                                        
029800         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
029900                                                                          
030000     03  W-KDSEGKEY-X.                                                    
030100         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
030200                                                                          
030300     03  W-TIAAAA-X.                                                      
030400         05  W-TIAAAA            PIC 9(4)   VALUE ZERO.                   
030500                                                                          
030600*                                                                         
030700     SKIP2                                                                
030800*    --- STATUS-KOD FRÅN IMS                                              
030900 01  STATUS-WS                   PIC XX.                                  
031000     88  SEGMENT-FINNS                       VALUE '  '.                  
031100     88  SEGMENT-SAKNAS                      VALUE 'GA'                   
031200                                                   'GB'                   
031300                                                   'GE'.                  
031400     SKIP2                                                                
031500 01  GODK-STATUSKODER.                                                    
031600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
031700     SKIP3                                                                
031800 01  SSA1                        PIC X(64).                               
031900 01  SSA2                        PIC X(64).                               
032000 01  SSA3                        PIC X(64).                               
032100     EJECT                                                                
032200*    --- IMS FUNKTIONSKODER                                               
032300*01  -COPY W0003                                                          
032400     EJECT                                                                
032500*    ---  DLI INPUT-OUTPUT AREA                                           
032600                                                                          
032700 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA-K6'.        
032800     SKIP3                                                                
032900 01  DLI-IO-AREA-K6.                                                      
033000     03  IO-AREA-K6              PIC X(900)  VALUE SPACE.                 
033100     SKIP3                                                                
033200     03  K601 REDEFINES IO-AREA-K6.                                       
033300*        05  -COPY WDK601                                                 
033400     SKIP3                                                                
033500     03  K611 REDEFINES IO-AREA-K6.                                       
033600*        05  -COPY WDK611                                                 
033700     EJECT                                                                
033800                                                                          
033900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK626'.                      
034000 01  DLI-IO-WDK626.                                                       
034100*    03  -COPY WDK626                                                     
034200     EJECT                                                                
034300                                                                          
034400 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDL801'.             
034500     SKIP3                                                                
034600 01  DLI-IO-AREA-WDL801.                                                  
034700*        05  -COPY WDL801                                                 
034800     EJECT                                                                
034900 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDL811'.             
035000     SKIP3                                                                
035100 01  DLI-IO-AREA-WDL811.                                                  
035200*        05  -COPY WDL811                                                 
035300     EJECT                                                                
035400                                                                          
035500 LINKAGE SECTION.                                                         
035600                                                                          
035700     EJECT                                                                
035800*01  -COPY W0008  -PRE SB-WDK6-                                           
035900     05  WDK6-KEY-FB-AREA-IDARTNR       PIC S9(9) COMP-3.                 
036000     EJECT                                                                
036100*01  -COPY W0008  -PRE WDK6-                                              
036200     05  FILLER                  PIC X.                                   
036300     EJECT                                                                
036400*01  -COPY W0008  -PRE WDL8-                                              
036500     05  FILLER                  PIC X.                                   
036600     EJECT                                                                
036700 PROCEDURE DIVISION  USING SB-WDK6-PCB WDK6-PCB WDL8-PCB.                 
036800     ENTRY 'DLITCBL' USING SB-WDK6-PCB WDK6-PCB WDL8-PCB.                 
036900                                                                          
037000*        OBS DETTA KORT ÄR FÖR BTS-TEST ****************                  
037100*    MOVE 'CBLTDLI'     TO CBLTDLI                                        
037200     PERFORM A-INIT                                                       
037300     PERFORM IMS-GN-WDK6                                                  
037400     PERFORM UNTIL SEGMENT-SAKNAS                                         
037500       EVALUATE SB-WDK6-SEG-NAME-FB                                       
037600         WHEN 'WDK601  '                                                  
037700           MOVE WDK6-KEY-FB-AREA-IDARTNR                                  
037800                             TO W-IDARTNR                                 
037900              MOVE ART-TIFINLV                                            
038000                             TO WS-TIFINLV                                
038100              MOVE ART-IDLEVNR                                            
038200                             TO WS-IDLEVNR                                
038300              MOVE ART-KDPRODSL                                           
038400                             TO TEST-KDPRODSL                             
038500              MOVE ART-KDSORT                                             
038600                             TO WS-KDSORT                                 
038700         WHEN 'WDK611  '                                                  
038800                                                                          
038900              MOVE WS-TIFINLV                                             
039000                             TO TMP1-YYWWD                                
039100              MOVE DAGENS-TIAAVVD                                         
039200                             TO TMP2-YYWWD                                
039300              PERFORM WY2000P2                                            
039400              IF TMP1-YYWWD < TMP2-YYWWD                                  
039500              AND (CLAG-KDERS < 20                                        
039600              AND NOT (CLAG-KDERS = 09                                    
039700              OR       CLAG-KDERS = 19))                                  
039800              AND CLAG-KVPB-SEP > 3.0                                     
039900              AND CLAG-REDIRLEV < 1.00                                    
040000              AND CLAG-DAPBPLAN < DAGENS-TISSSSMMDD                       
040100              AND CLAG-FLOREGPB = NEJ                                     
040200              AND WS-KDSORT NOT = 'SW'                                    
040300              AND NOT KDPRODSL-LOCAL                                      
040400              AND NOT (WS-IDLEVNR = '6492 '                               
040500              OR       WS-IDLEVNR = 'BFZZA')                              
040600*                ADD 1       TO WS-ANTAL-C-BEH                            
040700                 PERFORM C-BEHANDLA-ARTIKEL                               
040800              END-IF                                                      
040900                                                                          
041000       END-EVALUATE                                                       
041100       PERFORM IMS-GN-WDK6                                                
041200     END-PERFORM                                                          
041300                                                                          
041400     PERFORM Z-FINIT                                                      
041500                                                                          
041600     MOVE ZERO TO RETURN-CODE                                             
041700     GOBACK                                                               
041800     .                                                                    
041900     EJECT                                                                
042000                                                                          
042100                                                                          
042200 A-INIT SECTION.                                                          
042300                                                                          
042400     OPEN OUTPUT W22235                                                   
042500                                                                          
042600     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
042700     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
042800     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
042900     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
043000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
043100                                                                          
043200     MOVE 'AAMMDD'     TO DAT-KDDATFORM                                   
043300     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
043400                                                                          
043500     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
043600                         DAT-O-TIDATUM DAT-KDSVAR                         
043700                                                                          
043800     IF DAT-KDSVAR-OK                                                     
043900        MOVE DAT-TIAARP      TO DAGENS-TIAARP                             
044000                                FOREG-TIAARP                              
044100                                NAESTA-TIAARP                             
044200        MOVE DAT-TIVV        TO DAGENS-TIVV                               
044300        MOVE DAT-TIAAVV-GRP  TO DAGENS-TIAAVV-GRP                         
044400        MOVE DAT-TIAAVVD     TO DAGENS-TIAAVVD                            
044500        MOVE DAT-TISEKEL     TO DAGENS-TISS                               
044600        MOVE DAT-TIAAMMDD    TO DAGENS-TISSMMDD                           
044700     ELSE                                                                 
044800       MOVE 'SVAR 1 FRÅN WDATKONV I A SECTION EJ OK'                      
044900                         TO FELTEXT-STR                                   
045000       DISPLAY FELTEXT                                                    
045100       PERFORM S99-ABEND                                                  
045200     END-IF                                                               
045300                                                                          
045400                                                                          
045500*    DAGENS DATUM ETT ÅR TILLBAKA                                         
045600                                                                          
045700     MOVE DAGENS-TIAAVVD   TO DAGENS-TIAAVVD-LAST-YEAR                    
045800     IF DAGENS-TIAAVVD-LAST-YEAR-AA = 00                                  
045900       MOVE 99      TO DAGENS-TIAAVVD-LAST-YEAR-AA                        
046000     ELSE                                                                 
046100       SUBTRACT +1  FROM DAGENS-TIAAVVD-LAST-YEAR-AA                      
046200     END-IF                                                               
046300                                                                          
046400     PERFORM AA-INITERA-PERIODTABELL                                      
046500     .                                                                    
046600     EJECT                                                                
046700 AA-INITERA-PERIODTABELL SECTION.                                         
046800*--------------------------------------------------------------*          
046900* HÄR INITIERAS PERIODTABELLEN. INDX 1 MOTSVARAR INNEVARANDE   *          
047000* PERIOD, INDX 2 PERIODEN INNAN OSV.                           *          
047100* PERIODERNA ÄR REDOVISNINGSPERIODER. TABELLEN INITIERAS MED   *          
047200* PER.NR + START/SLUTVECKA FÖR ATT KUNNA UTFÖRA SUMMERINGAR    *          
047300*--------------------------------------------------------------*          
047400                                                                          
047500     MOVE 1 TO TAB-RADIX                                                  
047600     PERFORM AAA-TA-HAND-OM-INNEV-PERIOD                                  
047700     ADD +1 TO TAB-RADIX                                                  
047800     PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                              
047900*---TA REDA PÅ FÖREGÅENDE PERIOD                                          
048000       COMPUTE FOREG-TIRP = FOREG-TIRP - 1                                
048100       IF FOREG-TIRP = +0                                                 
048200         IF FOREG-TIAA = 00                                               
048300           MOVE 99      TO   FOREG-TIAA                                   
048400         ELSE                                                             
048500           SUBTRACT  +1 FROM FOREG-TIAA                                   
048600         END-IF                                                           
048700         MOVE +12     TO   FOREG-TIRP                                     
048800       END-IF                                                             
048900       MOVE FOREG-TIAARP TO TABELL-TIAARP(TAB-RADIX)                      
049000                                                                          
049100*---BEHANDLAD PERIODS START-VECKA - 1 = FÖREGÅENDE PER. SLUT-VECKA        
049200      IF START-VV > +1                                                    
049300        COMPUTE SLUT-VV = START-VV - +1                                   
049400        MOVE SLUT-VV TO TABELL-SISTA-TIAAVV(TAB-RADIX)                    
049500      ELSE                                                                
049600        MOVE FOREG-TIAA      TO WS-TIAA                                   
049700        PERFORM AAB-KOLLA-ANTAL-VECKOR                                    
049800        MOVE SLUT-VV TO TABELL-SISTA-TIAAVV(TAB-RADIX)                    
049900      END-IF                                                              
050000                                                                          
050100*--- TA REDA PÅ START-VECKA                                               
050200                                                                          
050300       MOVE FOREG-TIAARP TO DAT-I-TIDATUM                                 
050400       MOVE 'AARP'       TO DAT-KDDATFORM                                 
050500                                                                          
050600       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
050700                           DAT-O-TIDATUM DAT-KDSVAR                       
050800                                                                          
050900       IF DAT-KDSVAR-OK                                                   
051000*--- START-VECKA ÄR ALLTID VECKA 1 PÅ NYTT ÅR                             
051100         IF DAT-TIVV = +52 OR +53                                         
051200           MOVE +1 TO START-VV                                            
051300                      TABELL-FORSTA-TIAAVV(TAB-RADIX)                     
051400         ELSE                                                             
051500           MOVE DAT-TIVV   TO START-VV                                    
051600                              TABELL-FORSTA-TIAAVV(TAB-RADIX)             
051700         END-IF                                                           
051800       ELSE                                                               
051900         MOVE 'SVAR 2 FRÅN WDATKONV EJ OK' TO FELTEXT-STR                 
052000         DISPLAY FELTEXT                                                  
052100         PERFORM S99-ABEND                                                
052200       END-IF                                                             
052300       MOVE DAT-TIAARP TO TABELL-TIAARP(TAB-RADIX)                        
052400       MOVE ZERO       TO TABELL-KVOI(TAB-RADIX)                          
052500       ADD +1 TO TAB-RADIX                                                
052600     END-PERFORM                                                          
052700     .                                                                    
052800     EJECT                                                                
052900 AAA-TA-HAND-OM-INNEV-PERIOD SECTION.                                     
053000                                                                          
053100*--- KOLLA START-VECKA                                                    
053200                                                                          
053300     MOVE 'AARP'        TO DAT-KDDATFORM                                  
053400     MOVE DAGENS-TIAARP TO DAT-I-TIDATUM                                  
053500                                                                          
053600     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
053700                         DAT-O-TIDATUM DAT-KDSVAR                         
053800                                                                          
053900     IF DAT-KDSVAR-OK                                                     
054000*--- START-VECKA ÄR ALLTID VECKA 1 PÅ NYTT ÅR                             
054100       IF DAT-TIVV = +52 OR +53                                           
054200         MOVE +1 TO START-VV                                              
054300                                                                          
054400         IF DAGENS-TIVV = 52 OR 53                                        
054500           MOVE 1            TO WS-VECKA-I-AKT-PERIOD                     
054600         END-IF                                                           
054700         IF DAGENS-TIVV = 1                                               
054800           MOVE 2            TO WS-VECKA-I-AKT-PERIOD                     
054900         END-IF                                                           
055000         IF DAGENS-TIVV = 2                                               
055100           MOVE 3            TO WS-VECKA-I-AKT-PERIOD                     
055200         END-IF                                                           
055300         IF DAGENS-TIVV = 3                                               
055400           MOVE 4            TO WS-VECKA-I-AKT-PERIOD                     
055500         END-IF                                                           
055600         IF DAGENS-TIVV = 4                                               
055700           MOVE 5            TO WS-VECKA-I-AKT-PERIOD                     
055800         END-IF                                                           
055900       ELSE                                                               
056000         MOVE DAT-TIVV   TO START-VV                                      
056100                                                                          
056200         COMPUTE WS-VECKA-I-AKT-PERIOD =                                  
056300            DAGENS-TIVV - START-VV + 1                                    
056400       END-IF                                                             
056500     ELSE                                                                 
056600       MOVE 'SVAR 3 FRÅN WDATKONV EJ OK' TO FELTEXT-STR                   
056700       DISPLAY FELTEXT                                                    
056800       PERFORM S99-ABEND                                                  
056900     END-IF                                                               
057000                                                                          
057100*--- KOLLA SLUT-VECKA, NÄSTA PERIODS START-VECKA - 1                      
057200                                                                          
057300     COMPUTE NAESTA-TIRP = DAGENS-TIRP + 1                                
057400     IF NAESTA-TIRP = +13                                                 
057500       ADD  +1 TO NAESTA-TIAA                                             
057600       MOVE +1 TO NAESTA-TIRP                                             
057700     END-IF                                                               
057800                                                                          
057900     MOVE 'AARP'        TO DAT-KDDATFORM                                  
058000     MOVE NAESTA-TIAARP TO DAT-I-TIDATUM                                  
058100                                                                          
058200     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
058300                         DAT-O-TIDATUM DAT-KDSVAR                         
058400                                                                          
058500     IF DAT-KDSVAR-OK                                                     
058600       MOVE DAT-TIVV   TO SLUT-VV                                         
058700     ELSE                                                                 
058800       MOVE 'SVAR 4 FRÅN WDATKONV EJ OK' TO FELTEXT-STR                   
058900       DISPLAY FELTEXT                                                    
059000       PERFORM S99-ABEND                                                  
059100     END-IF                                                               
059200                                                                          
059300     IF SLUT-VV = +1                                                      
059400       MOVE DAGENS-TIAA  TO WS-TIAA                                       
059500       PERFORM AAB-KOLLA-ANTAL-VECKOR                                     
059600     ELSE                                                                 
059700       COMPUTE SLUT-VV = SLUT-VV - +1                                     
059800     END-IF                                                               
059900                                                                          
060000     MOVE DAGENS-TIAARP TO TABELL-TIAARP(TAB-RADIX)                       
060100     MOVE START-VV      TO TABELL-FORSTA-TIAAVV(TAB-RADIX)                
060200     MOVE SLUT-VV       TO TABELL-SISTA-TIAAVV(TAB-RADIX)                 
060300     MOVE ZERO          TO TABELL-KVOI(TAB-RADIX)                         
060400                                                                          
060500     .                                                                    
060600     EJECT                                                                
060700 AAB-KOLLA-ANTAL-VECKOR SECTION.                                          
060800                                                                          
060900* --- TAG REDA PÅ OM DET ÄR 52 ELLER 53 VECKOR PÅ ÅRET                    
061000                                                                          
061100     MOVE 53        TO WS-TIVV                                            
061200     MOVE WS-TIAAVV TO DAT-I-TIDATUM                                      
061300     MOVE 'AAVV  '  TO DAT-KDDATFORM                                      
061400     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
061500                         DAT-O-TIDATUM DAT-KDSVAR                         
061600     IF DAT-KDSVAR-OK                                                     
061700       MOVE 53 TO SLUT-VV                                                 
061800     ELSE                                                                 
061900       MOVE 52 TO SLUT-VV                                                 
062000     END-IF                                                               
062100     .                                                                    
062200     EJECT                                                                
062300                                                                          
062400                                                                          
062500 C-BEHANDLA-ARTIKEL SECTION.                                              
062600                                                                          
062700     PERFORM IMS-GU-K626                                                  
062800     IF SEGMENT-SAKNAS                                                    
062900        MOVE ZERO            TO JUST-DAMANSEA                             
063000                                JUST-DASPSEA                              
063100        MOVE 1.0             TO JUST-RESEASON (1)                         
063200                                JUST-RESEASON (2)                         
063300                                JUST-RESEASON (3)                         
063400                                JUST-RESEASON (4)                         
063500                                JUST-RESEASON (5)                         
063600                                JUST-RESEASON (6)                         
063700                                JUST-RESEASON (7)                         
063800                                JUST-RESEASON (8)                         
063900                                JUST-RESEASON (9)                         
064000                                JUST-RESEASON (10)                        
064100                                JUST-RESEASON (11)                        
064200                                JUST-RESEASON (12)                        
064300     END-IF                                                               
064400     MOVE NEJ TO ARTIKEL-SW                                               
064500                                                                          
064600     PERFORM CA-UPPDATERA-ARBETSTABELL                                    
064700                                                                          
064800     PERFORM CB-HAEMTA-GRAENSVAERDEN                                      
064900                                                                          
065000     PERFORM CC-HAEMTA-ORDERINGANG                                        
065100                                                                          
065200     PERFORM CD-KOLLA-LARM                                                
065300*                                                                         
065400                                                                          
065500     IF WS-LARM-A = JA                                                    
065600     OR WS-LARM-B = JA                                                    
065700     OR WS-LARM-C = JA                                                    
065800     OR WS-LARM-D = JA                                                    
065900                                                                          
066000*-----                                                                    
066100*----- SKRIV POST PÅ LARMFIL                                              
066200*-----                                                                    
066300       MOVE CLAG-IDANSK                TO UT-IDANSK                       
066400       MOVE W-IDARTNR                  TO UT-IDARTNR                      
066500       MOVE DAGENS-TISSSSMMDD (1:4)    TO UT-TIAAAA                       
066600       MOVE DAGENS-TIVV                TO UT-TIVV                         
066700       MOVE CLAG-KDVVKL                TO UT-KDVVKL                       
066800       MOVE CLAG-KVPB-SEP              TO UT-KVPB-SEP                     
066900       MOVE WS-KVPB-2V                 TO UT-KVPB-2V                      
067000       MOVE WS-KVPB-4V                 TO UT-KVPB-4V                      
067100       MOVE WS-OI-TOT-2V               TO UT-OI-TOT-2V                    
067200       MOVE WS-OI-TOT-4V               TO UT-OI-TOT-4V                    
067300       MOVE WS-LARM-A                  TO UT-LARM-A                       
067400       MOVE WS-LARM-B                  TO UT-LARM-B                       
067500       MOVE WS-LARM-C                  TO UT-LARM-C                       
067600       MOVE WS-LARM-D                  TO UT-LARM-D                       
067700       COMPUTE UT-RVPROURS = CLAG-RVPROURS + 1                            
067800       PERFORM S01-SKRIV-W22235                                           
067900                                                                          
068000                                                                          
068100     ELSE                                                                 
068200       IF CLAG-RVPROURS > ZERO                                            
068300*-----                                                                    
068400*----- SKRIV POST PÅ LARMFIL, CLAG-RVPROURS SKALL NOLLAS I W22236         
068500*-----                                                                    
068600         MOVE W-IDARTNR                TO UT-IDARTNR                      
068700         MOVE ZERO                     TO UT-RVPROURS                     
068800                                          UT-KDVVKL                       
068900                                          UT-KVPB-SEP                     
069000                                          UT-KVPB-2V                      
069100                                          UT-KVPB-4V                      
069200                                          UT-OI-TOT-2V                    
069300                                          UT-OI-TOT-4V                    
069400                                          UT-IDANSK                       
069500                                          UT-TIAAAA                       
069600                                          UT-TIVV                         
069700         MOVE NEJ                      TO UT-LARM-A                       
069800                                          UT-LARM-B                       
069900                                          UT-LARM-C                       
070000                                          UT-LARM-D                       
070100         PERFORM S01-SKRIV-W22235                                         
070200       END-IF                                                             
070300     END-IF                                                               
070400     .                                                                    
070500     EJECT                                                                
070600 CA-UPPDATERA-ARBETSTABELL SECTION.                                       
070700                                                                          
070800*---------------------------------------------------------------*         
070900* HÄR LÄGGS SENASTE 4 VECKOR IN I EN ARBETSTABELL               *         
071000* EV. NEGATIV OI ÄNDRAS TILL NOLL.                              *         
071100*---------------------------------------------------------------*         
071200                                                                          
071300     MOVE ZERO               TO WS-KVOI-PROG (1)                          
071400                                WS-KVOI-PROG (2)                          
071500                                WS-KVOI-PROG (3)                          
071600                                WS-KVOI-PROG (4)                          
071700     MOVE 1.00               TO WS-RESEASON (1)                           
071800                                WS-RESEASON (2)                           
071900                                WS-RESEASON (3)                           
072000                                WS-RESEASON (4)                           
072100     MOVE 1                  TO WS-ANTAL-VECKOR                           
072200     MOVE +1                 TO TAB-RADIX                                 
072300     MOVE DAGENS-TIVV        TO VECKO-INDX                                
072400                                                                          
072500     MOVE DAGENS-TISSSSMMDD (1:4)                                         
072600                             TO W-TIAAAA                                  
072700     PERFORM IMS-GU-L811                                                  
072800                                                                          
072900*--- LÄS IN DE SENASTE 4 VECKORNAS ORDERINGÅNG I ARBETSTABELLEN           
073000                                                                          
073100     PERFORM UNTIL WS-ANTAL-VECKOR > 4                                    
073200     OR SEGMENT-SAKNAS                                                    
073300       MOVE TABELL-TIAARP (TAB-RADIX) (3:2)                               
073400                             TO WS-TIRP (WS-ANTAL-VECKOR)                 
073500       MOVE WS-TIRP (WS-ANTAL-VECKOR)                                     
073600                             TO PER-IX                                    
073700       PERFORM UNTIL VECKO-INDX < TABELL-FORSTA-TIAAVV(TAB-RADIX)         
073800       OR WS-ANTAL-VECKOR > 4                                             
073900         MOVE JUST-RESEASON (PER-IX)                                      
074000                             TO WS-RESEASON (WS-ANTAL-VECKOR)             
074100         IF AAR-KVOI-PROG(VECKO-INDX) > ZERO                              
074200           MOVE AAR-KVOI-PROG(VECKO-INDX)                                 
074300                             TO WS-KVOI-PROG (WS-ANTAL-VECKOR)            
074400         END-IF                                                           
074500         SUBTRACT +1 FROM VECKO-INDX                                      
074600         ADD 1               TO WS-ANTAL-VECKOR                           
074700       END-PERFORM                                                        
074800       ADD +1 TO TAB-RADIX                                                
074900       IF VECKO-INDX = ZERO                                               
075000          SUBTRACT 1         FROM W-TIAAAA                                
075100          PERFORM IMS-GU-L811                                             
075200          MOVE TABELL-SISTA-TIAAVV(TAB-RADIX)                             
075300                             TO VECKO-INDX                                
075400       END-IF                                                             
075500                                                                          
075600     END-PERFORM                                                          
075700     .                                                                    
075800     EJECT                                                                
075900 CB-HAEMTA-GRAENSVAERDEN SECTION.                                         
076000*---------------------------------------------------------------*         
076100*                                                               *         
076200* HÄR HÄMTAS FAKTORER BEROENDE PÅ VOLYMVÄRDESKLASS              *         
076300*                                                               *         
076400*---------------------------------------------------------------*         
076500                                                                          
076600     COMPUTE WS-KVPB-SEP-VECKA ROUNDED = CLAG-KVPB-SEP / 4.33             
076700     MOVE 1                  TO LARM-IX                                   
076800     PERFORM UNTIL LARM-IX > LARM-MAX-IX                                  
076900                                                                          
077000       IF CLAG-KDVVKL = LARM-KDVVKL (LARM-IX)                             
077100          COMPUTE WS-GRAENS-2V-HOEG ROUNDED =                             
077200               (((WS-KVPB-SEP-VECKA * WS-RESEASON (1))                    
077300               + (WS-KVPB-SEP-VECKA * WS-RESEASON (2)))                   
077400               * 4.33 / 2 * LARM-2V-HOEG (LARM-IX))                       
077500                                                                          
077600          COMPUTE WS-GRAENS-4V-HOEG ROUNDED =                             
077700               (((WS-KVPB-SEP-VECKA * WS-RESEASON (1))                    
077800               + (WS-KVPB-SEP-VECKA * WS-RESEASON (2))                    
077900               + (WS-KVPB-SEP-VECKA * WS-RESEASON (3))                    
078000               + (WS-KVPB-SEP-VECKA * WS-RESEASON (4)))                   
078100               * 4.33 / 4 * LARM-4V-HOEG (LARM-IX))                       
078200                                                                          
078300          COMPUTE WS-GRAENS-4V-LAAG ROUNDED =                             
078400               (((WS-KVPB-SEP-VECKA * WS-RESEASON (1))                    
078500               + (WS-KVPB-SEP-VECKA * WS-RESEASON (2))                    
078600               + (WS-KVPB-SEP-VECKA * WS-RESEASON (3))                    
078700               + (WS-KVPB-SEP-VECKA * WS-RESEASON (4)))                   
078800               * 4.33 / 4 * LARM-4V-LAAG (LARM-IX))                       
078900                                                                          
079000          COMPUTE WS-KVPB-2V ROUNDED =                                    
079100                ((WS-KVPB-SEP-VECKA * WS-RESEASON (1))                    
079200               + (WS-KVPB-SEP-VECKA * WS-RESEASON (2)))                   
079300               * 4.33 / 2                                                 
079400                                                                          
079500          COMPUTE WS-KVPB-4V ROUNDED =                                    
079600                ((WS-KVPB-SEP-VECKA * WS-RESEASON (1))                    
079700               + (WS-KVPB-SEP-VECKA * WS-RESEASON (2))                    
079800               + (WS-KVPB-SEP-VECKA * WS-RESEASON (3))                    
079900               + (WS-KVPB-SEP-VECKA * WS-RESEASON (4)))                   
080000               * 4.33 / 4                                                 
080100                                                                          
080200          MOVE LARM-MAX-IX   TO LARM-IX                                   
080300       END-IF                                                             
080400       ADD 1                 TO LARM-IX                                   
080500     END-PERFORM                                                          
080600     .                                                                    
080700     EJECT                                                                
080800                                                                          
080900                                                                          
081000 CC-HAEMTA-ORDERINGANG SECTION.                                           
081100*---------------------------------------------------------------*         
081200*                                                               *         
081300* HÄR HÄMTAS SENASTE 2 RESP. 4 VECKORS ORDERINGÅNG              *         
081400*                                                               *         
081500*---------------------------------------------------------------*         
081600                                                                          
081700     COMPUTE WS-OI-TOT-2V = WS-KVOI-PROG (1)                              
081800                          + WS-KVOI-PROG (2)                              
081900                                                                          
082000     COMPUTE WS-OI-TOT-4V = WS-KVOI-PROG (1)                              
082100                          + WS-KVOI-PROG (2)                              
082200                          + WS-KVOI-PROG (3)                              
082300                          + WS-KVOI-PROG (4)                              
082400     .                                                                    
082500     EJECT                                                                
082600                                                                          
082700                                                                          
082800 CD-KOLLA-LARM SECTION.                                                   
082900*---------------------------------------------------------------*         
083000*                                                               *         
083100* SKALL DET LARMAS ?                                            *         
083200*                                                               *         
083300*---------------------------------------------------------------*         
083400                                                                          
083500     MOVE NEJ                TO WS-LARM-A                                 
083600                                WS-LARM-B                                 
083700                                WS-LARM-C                                 
083800                                WS-LARM-D                                 
083900                                                                          
084000     COMPUTE WS-LEDTID = CLAG-KVVECKOR-LT + 4                             
084100     COMPUTE WS-VECKO-PB ROUNDED = CLAG-KVPB-SEP / 4.33                   
084200     COMPUTE WS-LEDTIDSBEHOV ROUNDED =                                    
084300             WS-LEDTID * WS-VECKO-PB                                      
084400*    COMPUTE WS-SALDO = CLAG-KVLS - CLAG-KVRESS - ARTM-KVOKS              
084500     COMPUTE WS-SALDO = CLAG-KVLS - CLAG-KVRESS                           
084600     IF (CLAG-KVPB-SEP > ZERO                                             
084700     OR WS-OI-TOT-2V > ZERO                                               
084800     OR WS-OI-TOT-4V > ZERO)                                              
084900        IF (CLAG-KVPB-SEP < 3.0                                           
085000        AND WS-SALDO > WS-LEDTIDSBEHOV)                                   
085100           CONTINUE                                                       
085200        ELSE                                                              
085300                                                                          
085400          IF WS-OI-TOT-2V > WS-GRAENS-2V-HOEG                             
085500          AND WS-OI-TOT-4V > WS-GRAENS-4V-HOEG                            
085600              MOVE JA        TO WS-LARM-A                                 
085700          END-IF                                                          
085800                                                                          
085900          IF WS-OI-TOT-2V > WS-GRAENS-2V-HOEG                             
086000              MOVE JA        TO WS-LARM-B                                 
086100          END-IF                                                          
086200                                                                          
086300          IF WS-OI-TOT-4V > WS-GRAENS-4V-HOEG                             
086400              MOVE JA        TO WS-LARM-C                                 
086500          END-IF                                                          
086600                                                                          
086700*         IF WS-OI-TOT-4V < WS-GRAENS-4V-LAAG                             
086800*             MOVE JA        TO WS-LARM-D                                 
086900*         END-IF                                                          
087000        END-IF                                                            
087100     END-IF                                                               
087200     .                                                                    
087300     EJECT                                                                
087400                                                                          
087500                                                                          
087600                                                                          
087700                                                                          
087800 Z-FINIT SECTION.                                                         
087900                                                                          
088000     CLOSE W22235                                                         
088100                                                                          
088200     MOVE 'S' TO POSTSUM-OPKOD                                            
088300     CALL POSTSUM USING POSTSUM-PARM                                      
088400     .                                                                    
088500     EJECT                                                                
088600 S01-SKRIV-W22235 SECTION.                                                
088700                                                                          
088800     WRITE UT-POST FROM UT-AREA                                           
088900                                                                          
089000     MOVE 'W22235'   TO POSTSUM-FDNAMN                                    
089100     MOVE 'W22235D3' TO POSTSUM-DDNAMN2                                   
089200     CALL POSTSUM USING POSTSUM-PARM                                      
089300     .                                                                    
089400     SKIP3                                                                
089500                                                                          
089600 S99-ABEND SECTION.                                                       
089700                                                                          
089800     MOVE 'S' TO POSTSUM-OPKOD                                            
089900     CALL POSTSUM USING POSTSUM-PARM                                      
090000     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
090100     .                                                                    
090200     EJECT                                                                
090300* --- IMS SEKTIONER ---                                                   
090400     SKIP3                                                                
090500     EJECT                                                                
090600 IMS-GN-WDK6 SECTION.                                                     
090700                                                                          
090800     CALL CBLTDLI USING GN SB-WDK6-PCB DLI-IO-AREA-K6                     
090900     MOVE SB-WDK6-STATUS-CODE TO STATUS-WS                                
091000     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
091100     PERFORM IMS-STATUSKONTROLL                                           
091200     .                                                                    
091300     EJECT                                                                
091400 IMS-GU-K626 SECTION.                                                     
091500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
091600          DELIMITED BY SIZE INTO SSA1                                     
091700     MOVE 'WDK611  (KDSEGKEY =1)' TO SSA2                                 
091800     MOVE 'WDK626   ' TO SSA3                                             
091900     MOVE '  GE' TO GODK-STATUSKODER                                      
092000     CALL CBLTDLI USING GU                                                
092100                      WDK6-PCB DLI-IO-WDK626 SSA1 SSA2 SSA3               
092200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
092300     PERFORM IMS-STATUSKONTROLL                                           
092400     .                                                                    
092500     EJECT                                                                
092600                                                                          
092700 IMS-GU-L811  SECTION.                                                    
092800                                                                          
092900     STRING 'WDL801  (IDARTNR  =' W-IDARTNR-X ')'                         
093000          DELIMITED BY SIZE INTO SSA1                                     
093100     STRING 'WDL811  (TIAAAA   =' W-TIAAAA-X ')'                          
093200          DELIMITED BY SIZE INTO SSA2                                     
093300     MOVE '  GE' TO GODK-STATUSKODER                                      
093400     CALL CBLTDLI USING GU WDL8-PCB                                       
093500                                 DLI-IO-AREA-WDL811 SSA1 SSA2             
093600     MOVE WDL8-STATUS-CODE TO STATUS-WS                                   
093700     PERFORM IMS-STATUSKONTROLL                                           
093800     .                                                                    
093900     EJECT                                                                
094000 IMS-STATUSKONTROLL SECTION.                                              
094100                                                                          
094200     SET STATUS-IX TO 1                                                   
094300     SEARCH GODK-STATUS                                                   
094400       AT END                                                             
094500         STRING 'OTILLÅTEN RETURKOD FRÅN IMS: ' STATUS-WS                 
094600           DELIMITED BY SIZE INTO FELTEXT-STR                             
094700         DISPLAY FELTEXT                                                  
094800         CALL FELLOG                                                      
094900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
095000         CONTINUE                                                         
095100     END-SEARCH                                                           
095200     .                                                                    
095300     EJECT                                                                
095400*    -COPY WY2000P1                                                       
095500     EJECT                                                                
095600*    -COPY WY2000P2                                                       
095700     EJECT                                                                
095800*    -COPY WY2000Q3                                                       
