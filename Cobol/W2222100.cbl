000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2222100.                                                
000400*AUTHOR.         STEFAN ANDREASSON.                                       
000500*DATE-WRITTEN.   SEPTEMBER 1999.                                          
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        PROGRAMMET BERÄKNAR NY PROGNOS FÖR ALLA AKTIVA                   
001200*        ARTIKLAR                                                         
001300*                                                                         
001400*                                                                         
001500*        PROGRAMMET LÄSER      WDK6                                       
001600*                              WDL8                                       
001700*                              WDD3                                       
001800*                                                                         
001900*    ABENDKODER:                                                          
002000*        U0016 -  SVAR FRÅN WDATKONV EJ OK                                
002100*              -  "ÖVERSÄTTNING" AV LAGER TILL DC-INDX SAKNAS             
002200*        U1000 -  . . . .                                                 
002300*                                                                         
002400*                                                                         
002500***------------------------------------------------------------           
002600*                                                                         
002700*    2016-03-16  E'TRACKER 10243132 CHINA EXPORT PROJECT                  
002800*                VID KDERS > 20 SÄTTES KVPB-SEP = 0.0                     
002900*                                                                         
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     SKIP2                                                                
003400 INPUT-OUTPUT SECTION.                                                    
003500                                                                          
003600 FILE-CONTROL.                                                            
003700     SKIP2                                                                
003800*          --- UPPGIFT OM DAG, VECKO- ELLER PERIODKÖRNING                 
003900                                                                          
004000     SELECT W22221                     ASSIGN TO W22221D1.                
004100*          --- UT-FIL                                                     
004200                                                                          
004300     SELECT W22221B                    ASSIGN TO W22221D2.                
004400*          --- UT3-FIL, LARM PROGNOSER ÄR NOLL                            
004500                                                                          
004600     SELECT W22221C                    ASSIGN TO W22221D3.                
004700*          --- UT4-FIL, DISPONIBELDATUM                                   
004800                                                                          
004900     SELECT W22221D                    ASSIGN TO W22221D4.                
005000*          --- UT5-FIL, PROGNOSFÖRÄNDRING SATSARTIKLAR                    
005100                                                                          
005200     SELECT W22221E                    ASSIGN TO W22221D5.                
005300*          --- UT6-FIL, FORECAST ONORMAL CHANGE                           
005400     EJECT                                                                
005500 DATA DIVISION.                                                           
005600     SKIP3                                                                
005700 FILE SECTION.                                                            
005800     SKIP3                                                                
005900                                                                          
006000                                                                          
006100 FD  W22221                                                               
006200     RECORDING       F                                                    
006300     BLOCK CONTAINS  0.                                                   
006400                                                                          
006500*01  POST -COPY W22221  -PRE UT-    -L.                                   
006600                                                                          
006700 FD  W22221B                                                              
006800     RECORDING       F                                                    
006900     BLOCK CONTAINS  0.                                                   
007000                                                                          
007100*01  POST -COPY W22221L -PRE UT3-    -L.                                  
007200                                                                          
007300 FD  W22221C                                                              
007400     RECORDING       F                                                    
007500     BLOCK CONTAINS  0.                                                   
007600                                                                          
007700*01  POST -COPY W21801  -PRE UT4-    -L.                                  
007800                                                                          
007900 FD  W22221D                                                              
008000     RECORDING       F                                                    
008100     BLOCK CONTAINS  0.                                                   
008200                                                                          
008300*01  POST -COPY WDG32202 -PRE UT5-    -L.                                 
008400                                                                          
008500 FD  W22221E                                                              
008600     RECORDING       F                                                    
008700     BLOCK CONTAINS  0.                                                   
008800                                                                          
008900*01  POST -COPY W22221E  -PRE UT6-    -L.                                 
009000                                                                          
009100     EJECT                                                                
009200 WORKING-STORAGE SECTION.                                                 
009300     SKIP2                                                                
009400*    -COPY WY2000W1                                                       
009500     SKIP2                                                                
009600*    -COPY WY2000W3                                                       
009700     SKIP3                                                                
009800*    -COPY WY2000W2                                                       
009900     SKIP3                                                                
010000 77  IDPGM                       PIC X(8)    VALUE 'W2222100'.            
010100 77  JA                          PIC X       VALUE 'J'.                   
010200 77  NEJ                         PIC X       VALUE 'N'.                   
010300 77  AKTIV                       PIC X       VALUE 'A'.                   
010400 77  CURRENT-SECTION             PIC X(30)   VALUE SPACE.                 
010500                                                                          
010600*    --- INDEX SAMT MAX-INDEX                                             
010700 77  FILLER                      PIC X(16)   VALUE 'INDEX'.               
010800 77  INDX                        PIC 9(2)    VALUE ZERO.                  
010900 77  DC-INDX                     PIC 9(2)    VALUE ZERO.                  
011000 77  IX-TAB                      PIC 9(2)    VALUE ZERO.                  
011100 77  PER-INDX                    PIC 9(2)    VALUE ZERO.                  
011200 77  TREND-INDX                  PIC 9(2)    VALUE ZERO.                  
011300 77  MAX-FSGFAKT                 PIC 9(2)    VALUE 7.                     
011400                                                                          
011500 77  VECKO-INDX                  PIC 9(2)    VALUE ZERO.                  
011600                                                                          
011700 01  TAB-RADIX                   PIC 9(2)    VALUE ZERO.                  
011800 77  MAX-TAB-RADIX               PIC 9(2)    VALUE ZERO.                  
011900                                                                          
012000 77  VV-INDX                     PIC 9(2)    VALUE ZERO.                  
012100 77  MAX-VV-INDX                 PIC 9(2)    VALUE 53.                    
012200 77  WS-IDDC-SE                  PIC X(2)    VALUE '11'.                  
012300                                                                          
012400*    --- SWITCHAR                                                         
012500 01  FILLER                      PIC X(16)   VALUE 'SWITCHAR'.            
012600                                                                          
012700 01  DC-POST.                                                             
012800     03  DC-PARAMETER        PIC X(4).                                    
012900         88 SAMTLIGA-DC      VALUE 'ALLA'.                                
013000         88 SAMTLIGA-SDC     VALUE 'EURO'.                                
013100         88 SAMTLIGA-NDC     VALUE 'AMER'.                                
013200         88 ENSTAKA-DC       VALUE 'DC21' 'DC22' 'DC23'                   
013300                                   'DC24' 'DC25' 'DC26'                   
013400                                   'DC3A'                                 
013500                                   'DC41' 'DC42' 'DC43' 'DC51'.           
013600                                                                          
013700     03  FILLER               PIC X(76).                                  
013800                                                                          
013900                                                                          
014000*      --- VALID IDDC CODES                                               
014100*                                                                         
014200*01    -COPY WWDC99                                                       
014300                                                                          
014400 01 DC-PARAMETER-DELAR.                                                   
014500     03 FILLER                PIC X(2)  VALUE SPACE.                      
014600     03 DC-PARAMETER-LAGER    PIC X(2)  VALUE SPACE.                      
014700                                                                          
014800 01  TREND-SW                    PIC X(5)    VALUE SPACE.                 
014900     88  INGEN-TREND                         VALUE 'INGEN'.               
015000     88  SVAG-TREND                          VALUE 'SVAG '.               
015100     88  STARK-TREND                         VALUE 'STARK'.               
015200                                                                          
015300 01  INDX-SW                     PIC X       VALUE 'N'.                   
015400     88  INDX-HITTAT                         VALUE 'J'.                   
015500                                                                          
015600 01  ARTIKEL-SW                  PIC X       VALUE 'N'.                   
015700     88  ARTIKEL-SKALL-FORAENDRAS            VALUE 'J'.                   
015800                                                                          
015900 01  MANUELL-PROGNOS-SW          PIC X       VALUE 'N'.                   
016000     88  MANUELL-PROGNOS-SATT                VALUE 'J'.                   
016100                                                                          
016200*    --- ARBETSFÄLT                                                       
016300 01  FILLER                      PIC X(16)   VALUE 'ARBETSFÄLT'.          
016400 01  ARBETSFAELT.                                                         
016500     03  PERIODTABELL            OCCURS 13.                               
016600         05 TABELL-TIAARP        PIC  9(4)    VALUE ZERO.                 
016700         05 TABELL-FORSTA-TIAAVV PIC  9(2)    VALUE ZERO.                 
016800         05 TABELL-SISTA-TIAAVV  PIC  9(2)    VALUE ZERO.                 
016900         05 TABELL-KVOI        PIC S9(9)V9(1)  VALUE ZERO COMP-3.         
017000                                                                          
017100     03  SLUT-VV                 PIC 9(2)    VALUE ZERO.                  
017200     03  START-VV                PIC 9(2)    VALUE ZERO.                  
017300                                                                          
017400     03  WS-NOLL                 PIC 9(4)    VALUE ZERO.                  
017500     03  WS-TIAAVV               PIC 9(4)    VALUE ZERO.                  
017600     03  FILLER REDEFINES WS-TIAAVV.                                      
017700         05 WS-TIAA              PIC 9(2).                                
017800         05 WS-TIVV              PIC 9(2).                                
017900                                                                          
018000     03  FOREG-TIAARP            PIC  9(4)   VALUE ZERO.                  
018100     03  FILLER REDEFINES FOREG-TIAARP.                                   
018200         05 FOREG-TIAA           PIC  9(2).                               
018300         05 FOREG-TIRP           PIC  9(2).                               
018400                                                                          
018500     03 DAGENS-TISSSSMMDD        PIC 9(8)       VALUE ZERO.               
018600     03 DAGENS-TISSSSMMDD-GRP    REDEFINES DAGENS-TISSSSMMDD.             
018700       05 DAGENS-TISS            PIC 9(2).                                
018800       05 DAGENS-TISSMMDD        PIC 9(6).                                
018900                                                                          
019000     03  DAGENS-TIAARP           PIC  9(4)   VALUE ZERO.                  
019100     03  FILLER REDEFINES DAGENS-TIAARP.                                  
019200         05 DAGENS-TIAA          PIC  9(2).                               
019300         05 DAGENS-TIRP          PIC  9(2).                               
019400                                                                          
019500     03  NAESTA-TIAARP           PIC  9(4)   VALUE ZERO.                  
019600     03  FILLER REDEFINES NAESTA-TIAARP.                                  
019700         05 NAESTA-TIAA          PIC  9(2).                               
019800         05 NAESTA-TIRP          PIC  9(2).                               
019900                                                                          
020000     03  SEASON-TIAARP           PIC  9(4)   VALUE ZERO.                  
020100     03  FILLER REDEFINES SEASON-TIAARP.                                  
020200         05 SEASON-TIAA          PIC  9(2).                               
020300         05 SEASON-TIRP          PIC  9(2).                               
020400                                                                          
020500     03  DAGENS-TIAAVVD          PIC  9(5)   VALUE ZERO.                  
020600     03  FILLER REDEFINES DAGENS-TIAAVVD.                                 
020700         05 DAGENS-TIAAVVD-AA    PIC  9(2).                               
020800         05 DAGENS-TIAAVVD-VV    PIC  9(2).                               
020900         05 DAGENS-TIAAVVD-D     PIC  9(1).                               
021000                                                                          
021100     03  DAGENS-TIAAVV-GRP       PIC  9(4)   VALUE ZERO.                  
021200                                                                          
021300                                                                          
021400     03  DAGENS-TIAAVVD-LAST-YEAR        PIC  9(5) VALUE ZERO.            
021500     03  FILLER REDEFINES DAGENS-TIAAVVD-LAST-YEAR.                       
021600         05 DAGENS-TIAAVVD-LAST-YEAR-AA  PIC 9(2).                        
021700         05 DAGENS-TIAAVVD-LAST-YEAR-VV  PIC 9(2).                        
021800         05 DAGENS-TIAAVVD-LAST-YEAR-D   PIC 9(1).                        
021900                                                                          
022000     03  DAGENS-TIVV             PIC  9(2)   VALUE ZERO.                  
022100                                                                          
022200     03  WS-ANTAL-VECKOR         PIC S9(3)   VALUE ZERO COMP-3.           
022300     03  WS-VECKO-IO             PIC S9(9)V9 VALUE ZERO COMP-3.           
022400     03  FILLER                  PIC X(16)                                
022500                                     VALUE 'WS-TEST-TIPBDAT'.             
022600     03  WS-TEST-TIPBDAT         PIC  9(5)   VALUE ZERO.                  
022700     03  WS-TIPBDAT-TIAARP       PIC  9(4)      VALUE ZERO.               
022800     03  WS-FORSTA-TIAAVV        PIC  9(4)      VALUE ZERO.               
022900     03  WS-SISTA-TIAAVV         PIC  9(4)      VALUE ZERO.               
023000     03  WS-DAT-TIAAVV           PIC  9(4)      VALUE ZERO.               
023100     03  WS-ONORM-OI-GRAENS-PB   PIC S9(9)V9(1) VALUE ZERO COMP-3.        
023200     03  WS-KVOI-SEASON          PIC S9(9)V9(1) VALUE ZERO COMP-3.        
023300     03  WS-KVOI-TOT             PIC S9(11)V9(2)                          
023400                                                VALUE ZERO COMP-3.        
023500     03  WS-NY-KVPB-SEP          PIC S9(6)V9(2) VALUE ZERO COMP-3.        
023600     03  NY-KVPB-SEP             PIC S9(6)V9(1) VALUE ZERO COMP-3.        
023700     03  WS-PREL-KVPB-SEP        PIC S9(6)V9(2) VALUE ZERO COMP-3.        
023800     03  WS-MEDEL-KVPB-SEP       PIC S9(6)V9(2) VALUE ZERO COMP-3.        
023900     03  WS-ANTAL-FAKTORER-STOERRE-NOLL PIC S9(7)          COMP-3.        
024000     03  WS-KVOTEN               PIC S9(5)V9(2) VALUE ZERO COMP-3.        
024100     03  WS-TIFINLV              PIC S9(5)V     VALUE ZERO COMP-3.        
024200     03  WS-VECKA-I-AKT-PERIOD   PIC  9(2)      VALUE ZERO.               
024300     03  WS-IDLEVNR              PIC  X(5).                               
024400     03  WS-KVVIPER              PIC  9(2)      VALUE ZERO.               
024500     03  WS-TIFINLV-AAVV         PIC S9(5)           COMP-3.              
024600     03  WS-TIFINLV-AAVV-PLUS-4  PIC S9(5)           COMP-3.              
024700                                                                          
024800   03 WS-PROG-PERIOD                OCCURS 12.                            
024900      05 PROG-PERIODTREND.                                                
025000          07 PROG-NORMAL              PIC 9V9(2).                         
025100          07 PROG-SVAG-TREND          PIC 9V9(2).                         
025200          07 PROG-STARK-TREND         PIC 9V9(2).                         
025300      05 FILLER REDEFINES PROG-PERIODTREND.                               
025400          07 PROG-TRENDFAKT           PIC 9V9(2) OCCURS 3.                
025500     EJECT                                                                
025600                                                                          
025700     03  WS-PROGFAKT-VKA.                                                 
025800        07 WS-PROGFAKT-PER    OCCURS 13.                                  
025900           09 WS-PERIODTREND-VKA.                                         
026000               11 WS-NORMAL-VKA          PIC 9V9(4).                      
026100               11 WS-SVAG-TREND-VKA      PIC 9V9(4).                      
026200               11 WS-STARK-TREND-VKA     PIC 9V9(4).                      
026300           09 FILLER REDEFINES WS-PERIODTREND-VKA.                        
026400               11 WS-TRENDFAKT-VKA       PIC 9V9(4) OCCURS 3.             
026500                                                                          
026600     03  WS2-TIFINLV                 PIC 9(5).                            
026700     03  FILLER REDEFINES WS2-TIFINLV.                                    
026800         05  WS2-TIFINLV-AAVV        PIC 9(4).                            
026900         05  FILLER                  PIC 9(1).                            
027000     03  WS3-TIFINLV                 PIC 9(6).                            
027100                                                                          
027200     03  WS-IDANSK                   PIC 9(3) VALUE ZERO.                 
027300     03  FILLER REDEFINES WS-IDANSK.                                      
027400         05 WS-IDANSK-1-2            PIC 9(2).                            
027500         05 WS-IDANSK3               PIC 9(1).                            
027600                                                                          
027700 01  LOCK-SW                  PIC X       VALUE 'N'.                      
027800     88  PROGNOS-AER-LAAST                VALUE 'J'.                      
027900                                                                          
028000     EJECT                                                                
028100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
028200 01  FILLER REDEFINES DAGENS-DATUM.                                       
028300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
028400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
028500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
028600     EJECT                                                                
028700                                                                          
028800 01      TYP-PARAMETER        PIC X(4).                                   
028900         88 DAY-KORNING       VALUE 'DAY '.                               
029000         88 WEEK-KORNING      VALUE 'WEEK'.                               
029100         88 ACC-KORNING       VALUE 'ACC '.                               
029200                                                                          
029300 01  DYNAMISKA-SUBPROGRAM.                                                
029400*                                                                         
029500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
029600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
029700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
029800     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
029900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
030000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
030100     03  W009VADD            PIC X(8)    VALUE 'W009VADD'.                
030200     SKIP2                                                                
030300*    --- PARAMETRAR TILL ABEND                                            
030400                                                                          
030500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
030600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
030700     SKIP2                                                                
030800 01  FELTEXT.                                                             
030900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
031000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
031100     EJECT                                                                
031200*    --- PARAMETRAR TILL DATKORT                                          
031300*                                                                         
031400 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W22221'.              
031500     SKIP2                                                                
031600 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
031700     SKIP2                                                                
031800*01  -COPY WDATKORT                                                       
031900     EJECT                                                                
032000*    --- PARAMETRAR TILL POSTSUM                                          
032100*                                                                         
032200*01  -COPY W0005   -PRE  POSTSUM-                                         
032300     EJECT                                                                
032400*    --- PARAMETRAR TILL WDATKONV                                         
032500*                                                                         
032600*01  -COPY WDATAREA                                                       
032700     EJECT                                                                
032800*    --- PARAMETRAR TILL W009VADD                                         
032900*                                                                         
033000 01  W009VADD-AREA.                                                       
033100     03  DATUM-AAVV              PIC S9(5) COMP-3.                        
033200     03  ANTAL                   PIC S9(3) COMP-3.                        
033300*                                                                         
033400     EJECT                                                                
033500*    --- TABELL MED FAKTORER FÖR BERÄKNING AV ONORMAL OI                  
033600*                                                                         
033700*01  -COPY W222FSG    -PRE  FSG-                                          
033800     EJECT                                                                
033900 01  UT-AREA-START              PIC X(24)   VALUE                         
034000                                 'UT-AREA-START  '.                       
034100     SKIP2                                                                
034200                                                                          
034300*01  AREA -COPY W22221     -PRE UT-                                       
034400                                                                          
034500     EJECT                                                                
034600 01  UT-AREA3-START             PIC X(24)   VALUE                         
034700                                 'UT-AREA3-START '.                       
034800     SKIP2                                                                
034900                                                                          
035000*01  AREA -COPY W22221L    -PRE UT3-                                      
035100                                                                          
035200                                                                          
035300     EJECT                                                                
035400 01  UT-AREA4-START             PIC X(24)   VALUE                         
035500                                 'UT-AREA4-START '.                       
035600     SKIP2                                                                
035700                                                                          
035800*                                                                         
035900*01  AREA  -COPY W21801     -PRE UT4-                                     
036000     EJECT                                                                
036100 01  UT-AREA5-START             PIC X(24)   VALUE                         
036200                                 'UT-AREA5-START '.                       
036300     SKIP2                                                                
036400                                                                          
036500*                                                                         
036600*01  AREA  -COPY WDG32202   -PRE UT5-                                     
036700     SKIP2                                                                
036800     EJECT                                                                
036900 01  UT-AREA6-START             PIC X(24)   VALUE                         
037000                                 'UT-AREA6-START '.                       
037100     SKIP2                                                                
037200                                                                          
037300*                                                                         
037400*01  AREA  -COPY W22221E    -PRE UT6-                                     
037500     SKIP2                                                                
037600     EJECT                                                                
037700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
037800                                                                          
037900     SKIP3                                                                
038000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
038100     SKIP3                                                                
038200 01  NYCKLAR-TILL-DLI.                                                    
038300     03  W-IDARTNR-X.                                                     
038400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
038500                                                                          
038600     03  W-IDDC-X.                                                        
038700         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
038800                                                                          
038900     03  W-KDSEGKEY-X.                                                    
039000         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
039100                                                                          
039200     03  W-TIAAAA-X.                                                      
039300         05  W-TIAAAA            PIC 9(4)   VALUE ZERO.                   
039400                                                                          
039500     03  W-IDSKYLT-X.                                                     
039600         05  W-IDSKYLT           PIC X(3)   VALUE 'USA'.                  
039700                                                                          
039800     03  W-KDPROGOI-X.                                                    
039900         05  W-KDPROGOI          PIC X       VALUE 'S'.                   
040000*                                                                         
040100*                                                                         
040200     SKIP2                                                                
040300*    --- STATUS-KOD FRÅN IMS                                              
040400 01  STATUS-WS                   PIC XX.                                  
040500     88  SEGMENT-FINNS                       VALUE '  '.                  
040600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
040700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
040800     SKIP2                                                                
040900 01  GODK-STATUSKODER.                                                    
041000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
041100     SKIP3                                                                
041200 01  SSA1                        PIC X(64).                               
041300 01  SSA2                        PIC X(64).                               
041400 01  SSA3                        PIC X(64).                               
041500     EJECT                                                                
041600*    --- IMS FUNKTIONSKODER                                               
041700*01  -COPY W0003                                                          
041800     EJECT                                                                
041900*    ---  DLI INPUT-OUTPUT AREA                                           
042000                                                                          
042100 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA-K6'.        
042200     SKIP3                                                                
042300 01  DLI-IO-AREA-K6.                                                      
042400     03  IO-AREA-K6              PIC X(900)  VALUE SPACE.                 
042500     SKIP3                                                                
042600     03  K601 REDEFINES IO-AREA-K6.                                       
042700*        05  -COPY WDK601                                                 
042800     SKIP3                                                                
042900     03  K611 REDEFINES IO-AREA-K6.                                       
043000*        05  -COPY WDK611                                                 
043100     EJECT                                                                
043200                                                                          
043300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK626'.                      
043400 01  DLI-IO-WDK626.                                                       
043500*        05  -COPY WDK626                                                 
043600     EJECT                                                                
043700                                                                          
043800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK629'.                      
043900 01  DLI-IO-WDK629.                                                       
044000*        05  -COPY WDK629                                                 
044100     EJECT                                                                
044200                                                                          
044300 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDL801'.             
044400     SKIP3                                                                
044500 01  DLI-IO-AREA-WDL801.                                                  
044600*        05  -COPY WDL801                                                 
044700     EJECT                                                                
044800 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDL811'.             
044900     SKIP3                                                                
045000 01  DLI-IO-AREA-WDL811.                                                  
045100*        05  -COPY WDL811                                                 
045200     EJECT                                                                
045300 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDD311'.             
045400 01  DLI-IO-WDD311.                                                       
045500*        05  -COPY WDD311                                                 
045600     EJECT                                                                
045700 01  FILLER               PIC X(16)   VALUE 'WDB618 AREA'.                
045800 01   DLI-IO-AREA-B618.                                                   
045900*     03  -COPY WDB618                                                    
046000     EJECT                                                                
046100                                                                          
046200 LINKAGE SECTION.                                                         
046300                                                                          
046400     EJECT                                                                
046500*01  -COPY W0008  -PRE SB-WDK6-                                           
046600     05  WDK6-KEY-FB-AREA-IDARTNR       PIC S9(9) COMP-3.                 
046700     EJECT                                                                
046800*01  -COPY W0008  -PRE WDK6-                                              
046900     05  FILLER                  PIC X.                                   
047000     EJECT                                                                
047100*01  -COPY W0008  -PRE WDL8-                                              
047200     05  FILLER                  PIC X.                                   
047300     EJECT                                                                
047400*01  -COPY W0008  -PRE WDD3-                                              
047500     05  FILLER                  PIC X.                                   
047600     EJECT                                                                
047700*01  -COPY W0008  -PRE WDB6-                                              
047800     05  FILLER                  PIC X.                                   
047900     EJECT                                                                
048000 PROCEDURE DIVISION  USING SB-WDK6-PCB WDK6-PCB WDL8-PCB                  
048100                           WDD3-PCB WDB6-PCB.                             
048200     ENTRY 'DLITCBL' USING SB-WDK6-PCB WDK6-PCB WDL8-PCB                  
048300                           WDD3-PCB WDB6-PCB.                             
048400                                                                          
048500     PERFORM A-INIT                                                       
048600     PERFORM IMS-GN-WDK6                                                  
048700     PERFORM UNTIL SEGMENT-SLUT                                           
048800       EVALUATE SB-WDK6-SEG-NAME-FB                                       
048900         WHEN 'WDK601  '                                                  
049000           MOVE WDK6-KEY-FB-AREA-IDARTNR                                  
049100                             TO W-IDARTNR                                 
049200              MOVE ART-TIFINLV                                            
049300                             TO WS-TIFINLV                                
049400              MOVE ART-IDLEVNR                                            
049500                             TO WS-IDLEVNR                                
049600         WHEN 'WDK611  '                                                  
049700                                                                          
049800              IF (CLAG-KDERS > 20) AND                                    
049900                 (CLAG-IDDC-REF NOT = SPACE)                              
050000                PERFORM D-NOLLA-PB-ERS-20                                 
050100              ELSE                                                        
050200                DIVIDE WS-TIFINLV BY 10 GIVING WS-TIFINLV-AAVV            
050300                MOVE WS-TIFINLV-AAVV                                      
050400                               TO WS-TIFINLV-AAVV-PLUS-4                  
050500                MOVE +4        TO WS-ANTAL-VECKOR                         
050600                CALL W009VADD USING                                       
050700                           WS-TIFINLV-AAVV-PLUS-4 WS-ANTAL-VECKOR         
050800                MOVE DAGENS-TIAAVV-GRP    TO TMP1-YYWW                    
050900                MOVE WS-TIFINLV-AAVV-PLUS-4                               
051000                                          TO TMP2-YYWW                    
051100                PERFORM WY2000P3                                          
051200                IF TMP1-YYWW >= TMP2-YYWW                                 
051300                AND CLAG-KDERS < 10                                       
051400                AND CLAG-FLMPB = JA                                       
051500                    PERFORM C-BEHANDLA-ARTIKEL                            
051600                END-IF                                                    
051700              END-IF                                                      
051800                                                                          
051900       END-EVALUATE                                                       
052000       PERFORM IMS-GN-WDK6                                                
052100     END-PERFORM                                                          
052200                                                                          
052300     PERFORM Z-FINIT                                                      
052400                                                                          
052500     MOVE ZERO TO RETURN-CODE                                             
052600     GOBACK                                                               
052700     .                                                                    
052800     EJECT                                                                
052900                                                                          
053000                                                                          
053100 A-INIT SECTION.                                                          
053200     MOVE 'A-INIT '  TO CURRENT-SECTION                                   
053300                                                                          
053400     OPEN OUTPUT W22221                                                   
053500                 W22221B                                                  
053600                 W22221C                                                  
053700                 W22221D                                                  
053800                 W22221E                                                  
053900                                                                          
054000     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
054100     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
054200     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
054300     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
054400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
054500                                                                          
054600     MOVE 'AAMMDD'     TO DAT-KDDATFORM                                   
054700     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
054800                                                                          
054900     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
055000                         DAT-O-TIDATUM DAT-KDSVAR                         
055100                                                                          
055200     IF DAT-KDSVAR-OK                                                     
055300        MOVE DAT-TIAARP      TO DAGENS-TIAARP                             
055400                                FOREG-TIAARP                              
055500                                NAESTA-TIAARP                             
055600        MOVE DAT-TIVV        TO DAGENS-TIVV                               
055700        MOVE DAT-TIAAVV-GRP  TO DAGENS-TIAAVV-GRP                         
055800        MOVE DAT-TIAAVVD     TO DAGENS-TIAAVVD                            
055900        MOVE DAT-TISEKEL     TO DAGENS-TISS                               
056000        MOVE DAT-TIAAMMDD    TO DAGENS-TISSMMDD                           
056100     ELSE                                                                 
056200       MOVE 'SVAR 1 FRÅN WDATKONV I A SECTION EJ OK'                      
056300                         TO FELTEXT-STR                                   
056400       DISPLAY FELTEXT                                                    
056500       PERFORM S99-ABEND                                                  
056600     END-IF                                                               
056700                                                                          
056800                                                                          
056900*    DAGENS DATUM ETT ÅR TILLBAKA                                         
057000                                                                          
057100     MOVE DAGENS-TIAAVVD   TO DAGENS-TIAAVVD-LAST-YEAR                    
057200     IF DAGENS-TIAAVVD-LAST-YEAR-AA = 00                                  
057300       MOVE 99      TO DAGENS-TIAAVVD-LAST-YEAR-AA                        
057400     ELSE                                                                 
057500       SUBTRACT +1  FROM DAGENS-TIAAVVD-LAST-YEAR-AA                      
057600     END-IF                                                               
057700                                                                          
057800     PERFORM AZ-VECKO-ELLER-PERIOD-KORNING                                
057900                                                                          
058000     IF ACC-KORNING                                                       
058100       MOVE 12               TO MAX-TAB-RADIX                             
058200     ELSE                                                                 
058300       MOVE 13               TO MAX-TAB-RADIX                             
058400     END-IF                                                               
058500                                                                          
058600     PERFORM AA-INITERA-PERIODTABELL                                      
058700     PERFORM AAC-INITERA-PROGFAKT                                         
058800                                                                          
058900     IF ACC-KORNING                                                       
059000       CONTINUE                                                           
059100     ELSE                                                                 
059200       PERFORM AB-PROGNOS-TABELL                                          
059300     END-IF                                                               
059400     .                                                                    
059500     EJECT                                                                
059600 AA-INITERA-PERIODTABELL SECTION.                                         
059700     MOVE 'AA-INITERA-PERIODTABELL '  TO CURRENT-SECTION                  
059800*--------------------------------------------------------------*          
059900* HÄR INITIERAS PERIODTABELLEN. INDX 1 MOTSVARAR INNEVARANDE   *          
060000* PERIOD, INDX 2 PERIODEN INNAN OSV.                           *          
060100* PERIODERNA ÄR REDOVISNINGSPERIODER. TABELLEN INITIERAS MED   *          
060200* PER.NR + START/SLUTVECKA FÖR ATT KUNNA UTFÖRA SUMMERINGAR    *          
060300*--------------------------------------------------------------*          
060400                                                                          
060500     MOVE 1 TO TAB-RADIX                                                  
060600     PERFORM AAA-TA-HAND-OM-INNEV-PERIOD                                  
060700     ADD +1 TO TAB-RADIX                                                  
060800     PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                              
060900*---TA REDA PÅ FÖREGÅENDE PERIOD                                          
061000       COMPUTE FOREG-TIRP = FOREG-TIRP - 1                                
061100       IF FOREG-TIRP = +0                                                 
061200         IF FOREG-TIAA = 00                                               
061300           MOVE 99      TO   FOREG-TIAA                                   
061400         ELSE                                                             
061500           SUBTRACT  +1 FROM FOREG-TIAA                                   
061600         END-IF                                                           
061700         MOVE +12     TO   FOREG-TIRP                                     
061800       END-IF                                                             
061900       MOVE FOREG-TIAARP TO TABELL-TIAARP(TAB-RADIX)                      
062000                                                                          
062100*---BEHANDLAD PERIODS START-VECKA - 1 = FÖREGÅENDE PER. SLUT-VECKA        
062200      IF START-VV > +1                                                    
062300        COMPUTE SLUT-VV = START-VV - +1                                   
062400        MOVE SLUT-VV TO TABELL-SISTA-TIAAVV(TAB-RADIX)                    
062500      ELSE                                                                
062600        MOVE FOREG-TIAA      TO WS-TIAA                                   
062700        PERFORM AAB-KOLLA-ANTAL-VECKOR                                    
062800        MOVE SLUT-VV TO TABELL-SISTA-TIAAVV(TAB-RADIX)                    
062900      END-IF                                                              
063000                                                                          
063100*--- TA REDA PÅ START-VECKA                                               
063200                                                                          
063300       MOVE FOREG-TIAARP TO DAT-I-TIDATUM                                 
063400       MOVE 'AARP'       TO DAT-KDDATFORM                                 
063500                                                                          
063600       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
063700                           DAT-O-TIDATUM DAT-KDSVAR                       
063800                                                                          
063900       IF DAT-KDSVAR-OK                                                   
064000*--- START-VECKA ÄR ALLTID VECKA 1 PÅ NYTT ÅR                             
064100         IF DAT-TIVV = +52 OR +53                                         
064200           MOVE +1 TO START-VV                                            
064300                      TABELL-FORSTA-TIAAVV(TAB-RADIX)                     
064400         ELSE                                                             
064500           MOVE DAT-TIVV   TO START-VV                                    
064600                              TABELL-FORSTA-TIAAVV(TAB-RADIX)             
064700         END-IF                                                           
064800       ELSE                                                               
064900         MOVE 'SVAR 2 FRÅN WDATKONV EJ OK' TO FELTEXT-STR                 
065000         DISPLAY FELTEXT                                                  
065100         PERFORM S99-ABEND                                                
065200       END-IF                                                             
065300       MOVE DAT-TIAARP TO TABELL-TIAARP(TAB-RADIX)                        
065400       MOVE ZERO       TO TABELL-KVOI(TAB-RADIX)                          
065500       ADD +1 TO TAB-RADIX                                                
065600     END-PERFORM                                                          
065700     .                                                                    
065800     EJECT                                                                
065900 AAA-TA-HAND-OM-INNEV-PERIOD SECTION.                                     
066000     MOVE 'AAA-TA-HAND-OM-INNEV-PERIOD' TO CURRENT-SECTION                
066100                                                                          
066200*--- KOLLA START-VECKA                                                    
066300                                                                          
066400     MOVE 'AARP'        TO DAT-KDDATFORM                                  
066500     MOVE DAGENS-TIAARP TO DAT-I-TIDATUM                                  
066600                                                                          
066700     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
066800                         DAT-O-TIDATUM DAT-KDSVAR                         
066900                                                                          
067000     IF DAT-KDSVAR-OK                                                     
067100*--- START-VECKA ÄR ALLTID VECKA 1 PÅ NYTT ÅR                             
067200       IF DAT-TIVV = +52 OR +53                                           
067300         MOVE +1 TO START-VV                                              
067400                                                                          
067500         IF DAGENS-TIVV = 52 OR 53                                        
067600           MOVE 1            TO WS-VECKA-I-AKT-PERIOD                     
067700         END-IF                                                           
067800         IF DAGENS-TIVV = 1                                               
067900           MOVE 2            TO WS-VECKA-I-AKT-PERIOD                     
068000         END-IF                                                           
068100         IF DAGENS-TIVV = 2                                               
068200           MOVE 3            TO WS-VECKA-I-AKT-PERIOD                     
068300         END-IF                                                           
068400         IF DAGENS-TIVV = 3                                               
068500           MOVE 4            TO WS-VECKA-I-AKT-PERIOD                     
068600         END-IF                                                           
068700         IF DAGENS-TIVV = 4                                               
068800           MOVE 5            TO WS-VECKA-I-AKT-PERIOD                     
068900         END-IF                                                           
069000       ELSE                                                               
069100         MOVE DAT-TIVV   TO START-VV                                      
069200                                                                          
069300         COMPUTE WS-VECKA-I-AKT-PERIOD =                                  
069400            DAGENS-TIVV - START-VV + 1                                    
069500       END-IF                                                             
069600       MOVE DAT-KVVIPER  TO WS-KVVIPER                                    
069700     ELSE                                                                 
069800       MOVE 'SVAR 3 FRÅN WDATKONV EJ OK' TO FELTEXT-STR                   
069900       DISPLAY FELTEXT                                                    
070000       PERFORM S99-ABEND                                                  
070100     END-IF                                                               
070200                                                                          
070300*--- KOLLA SLUT-VECKA, NÄSTA PERIODS START-VECKA - 1                      
070400                                                                          
070500     COMPUTE NAESTA-TIRP = DAGENS-TIRP + 1                                
070600     IF NAESTA-TIRP = +13                                                 
070700       ADD  +1 TO NAESTA-TIAA                                             
070800       MOVE +1 TO NAESTA-TIRP                                             
070900     END-IF                                                               
071000                                                                          
071100     MOVE 'AARP'        TO DAT-KDDATFORM                                  
071200     MOVE NAESTA-TIAARP TO DAT-I-TIDATUM                                  
071300                                                                          
071400     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
071500                         DAT-O-TIDATUM DAT-KDSVAR                         
071600                                                                          
071700     IF DAT-KDSVAR-OK                                                     
071800       MOVE DAT-TIVV   TO SLUT-VV                                         
071900     ELSE                                                                 
072000       MOVE 'SVAR 4 FRÅN WDATKONV EJ OK' TO FELTEXT-STR                   
072100       DISPLAY FELTEXT                                                    
072200       PERFORM S99-ABEND                                                  
072300     END-IF                                                               
072400                                                                          
072500     IF SLUT-VV = +1                                                      
072600       MOVE DAGENS-TIAA  TO WS-TIAA                                       
072700       PERFORM AAB-KOLLA-ANTAL-VECKOR                                     
072800     ELSE                                                                 
072900       COMPUTE SLUT-VV = SLUT-VV - +1                                     
073000     END-IF                                                               
073100                                                                          
073200     MOVE DAGENS-TIAARP TO TABELL-TIAARP(TAB-RADIX)                       
073300     MOVE START-VV      TO TABELL-FORSTA-TIAAVV(TAB-RADIX)                
073400     MOVE SLUT-VV       TO TABELL-SISTA-TIAAVV(TAB-RADIX)                 
073500     MOVE ZERO          TO TABELL-KVOI(TAB-RADIX)                         
073600                                                                          
073700     .                                                                    
073800     EJECT                                                                
073900 AAB-KOLLA-ANTAL-VECKOR SECTION.                                          
074000     MOVE 'AAB-KOLLA-ANTAL-VECKOR '  TO CURRENT-SECTION                   
074100                                                                          
074200* --- TAG REDA PÅ OM DET ÄR 52 ELLER 53 VECKOR PÅ ÅRET                    
074300                                                                          
074400     MOVE 53        TO WS-TIVV                                            
074500     MOVE WS-TIAAVV TO DAT-I-TIDATUM                                      
074600     MOVE 'AAVV  '  TO DAT-KDDATFORM                                      
074700     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
074800                         DAT-O-TIDATUM DAT-KDSVAR                         
074900     IF DAT-KDSVAR-OK                                                     
075000       MOVE 53 TO SLUT-VV                                                 
075100     ELSE                                                                 
075200       MOVE 52 TO SLUT-VV                                                 
075300     END-IF                                                               
075400     .                                                                    
075500     EJECT                                                                
075600                                                                          
075700 AAC-INITERA-PROGFAKT SECTION.                                            
075800     MOVE 'AAC-INITERA-PROGFAKT'  TO CURRENT-SECTION                      
075900                                                                          
076000       MOVE WS-IDDC-SE  TO W-IDDC                                         
076100       PERFORM IMS-GU-WDB618                                              
076200       IF SEGMENT-FINNS                                                   
076300         MOVE +1    TO IX-TAB                                             
076400         PERFORM UNTIL IX-TAB > 12                                        
076500           MOVE PROG-REFTREND-NORM (IX-TAB)                               
076600                           TO PROG-NORMAL (IX-TAB)                        
076700           MOVE PROG-REFTREND-SVAG (IX-TAB)                               
076800                        TO PROG-SVAG-TREND (IX-TAB)                       
076900           MOVE PROG-REFTREND-STARK (IX-TAB)                              
077000                        TO PROG-STARK-TREND (IX-TAB)                      
077100           ADD +1   TO IX-TAB                                             
077200         END-PERFORM                                                      
077300       ELSE                                                               
077400         MOVE '99'   TO W-IDDC                                            
077500         PERFORM IMS-GU-WDB618                                            
077600         IF SEGMENT-FINNS                                                 
077700           MOVE +1  TO IX-TAB                                             
077800           PERFORM UNTIL IX-TAB > 12                                      
077900             MOVE PROG-REFTREND-NORM (IX-TAB)                             
078000                             TO PROG-NORMAL (IX-TAB)                      
078100             MOVE PROG-REFTREND-SVAG (IX-TAB)                             
078200                          TO PROG-SVAG-TREND (IX-TAB)                     
078300             MOVE PROG-REFTREND-STARK (IX-TAB)                            
078400                          TO PROG-STARK-TREND (IX-TAB)                    
078500             ADD +1 TO IX-TAB                                             
078600           END-PERFORM                                                    
078700         ELSE                                                             
078800           CALL FELLOG                                                    
078900         END-IF                                                           
079000       END-IF                                                             
079100                                                                          
079200     .                                                                    
079300     EJECT                                                                
079400                                                                          
079500 AB-PROGNOS-TABELL SECTION.                                               
079600     MOVE 'AB-PROGNOS-TABELL '  TO CURRENT-SECTION                        
079700                                                                          
079800                                                                          
079900       COMPUTE WS-TRENDFAKT-VKA (1, 1) ROUNDED =                          
080000               PROG-TRENDFAKT (1, 1)                                      
080100             * (WS-VECKA-I-AKT-PERIOD / DAT-KVVIPER)                      
080200       COMPUTE WS-TRENDFAKT-VKA (1, 2) ROUNDED =                          
080300               PROG-TRENDFAKT (1, 2)                                      
080400             * (WS-VECKA-I-AKT-PERIOD / DAT-KVVIPER)                      
080500       COMPUTE WS-TRENDFAKT-VKA (1, 3) ROUNDED =                          
080600               PROG-TRENDFAKT (1, 3)                                      
080700             * (WS-VECKA-I-AKT-PERIOD / DAT-KVVIPER)                      
080800       MOVE 2                TO PER-INDX                                  
080900       PERFORM UNTIL PER-INDX > 12                                        
081000         MOVE 1              TO TREND-INDX                                
081100         PERFORM UNTIL TREND-INDX > 3                                     
081200           COMPUTE WS-TRENDFAKT-VKA                                       
081300                      (PER-INDX, TREND-INDX) ROUNDED =                    
081400            (PROG-TRENDFAKT (PER-INDX - 1,             TREND-INDX)        
081500          * ((DAT-KVVIPER - WS-VECKA-I-AKT-PERIOD) / DAT-KVVIPER))        
081600          +                                                               
081700            (PROG-TRENDFAKT (PER-INDX, TREND-INDX)                        
081800          * (WS-VECKA-I-AKT-PERIOD / DAT-KVVIPER))                        
081900           ADD 1             TO TREND-INDX                                
082000         END-PERFORM                                                      
082100         ADD 1               TO PER-INDX                                  
082200       END-PERFORM                                                        
082300       COMPUTE WS-TRENDFAKT-VKA (13, 1) ROUNDED =                         
082400               PROG-TRENDFAKT (12, 1)                                     
082500           * ((DAT-KVVIPER - WS-VECKA-I-AKT-PERIOD) / DAT-KVVIPER)        
082600       COMPUTE WS-TRENDFAKT-VKA (13, 2) ROUNDED =                         
082700               PROG-TRENDFAKT (12, 2)                                     
082800           * ((DAT-KVVIPER - WS-VECKA-I-AKT-PERIOD) / DAT-KVVIPER)        
082900       COMPUTE WS-TRENDFAKT-VKA (13, 3) ROUNDED =                         
083000               PROG-TRENDFAKT (12, 3)                                     
083100           * ((DAT-KVVIPER - WS-VECKA-I-AKT-PERIOD) / DAT-KVVIPER)        
083200     .                                                                    
083300     EJECT                                                                
083400                                                                          
083500                                                                          
083600 AZ-VECKO-ELLER-PERIOD-KORNING SECTION.                                   
083700     MOVE 'AZ-VECKO-ELLER-PERIOD-KORNING'  TO CURRENT-SECTION             
083800                                                                          
083900*--- KOLLA START-VECKA                                                    
084000                                                                          
084100     MOVE 'AARP'        TO DAT-KDDATFORM                                  
084200     MOVE DAGENS-TIAARP TO DAT-I-TIDATUM                                  
084300                                                                          
084400     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
084500                         DAT-O-TIDATUM DAT-KDSVAR                         
084600                                                                          
084700     IF DAT-KDSVAR-OK                                                     
084800*--- START-VECKA ÄR ALLTID VECKA 1 PÅ NYTT ÅR                             
084900       IF DAT-TIVV = +52 OR +53                                           
085000         MOVE +1 TO START-VV                                              
085100                                                                          
085200         IF DAGENS-TIVV = 52 OR 53                                        
085300           MOVE 1            TO WS-VECKA-I-AKT-PERIOD                     
085400         END-IF                                                           
085500         IF DAGENS-TIVV = 1                                               
085600           MOVE 2            TO WS-VECKA-I-AKT-PERIOD                     
085700         END-IF                                                           
085800         IF DAGENS-TIVV = 2                                               
085900           MOVE 3            TO WS-VECKA-I-AKT-PERIOD                     
086000         END-IF                                                           
086100         IF DAGENS-TIVV = 3                                               
086200           MOVE 4            TO WS-VECKA-I-AKT-PERIOD                     
086300         END-IF                                                           
086400         IF DAGENS-TIVV = 4                                               
086500           MOVE 5            TO WS-VECKA-I-AKT-PERIOD                     
086600         END-IF                                                           
086700       ELSE                                                               
086800         MOVE DAT-TIVV   TO START-VV                                      
086900                                                                          
087000         COMPUTE WS-VECKA-I-AKT-PERIOD =                                  
087100            DAGENS-TIVV - START-VV + 1                                    
087200       END-IF                                                             
087300     ELSE                                                                 
087400       MOVE 'SVAR 3 FRÅN WDATKONV EJ OK' TO FELTEXT-STR                   
087500       DISPLAY FELTEXT                                                    
087600       PERFORM S99-ABEND                                                  
087700     END-IF                                                               
087800     IF WS-VECKA-I-AKT-PERIOD = DAT-KVVIPER                               
087900        MOVE 'ACC '          TO TYP-PARAMETER                             
088000     ELSE                                                                 
088100        MOVE 'WEEK'          TO TYP-PARAMETER                             
088200     END-IF                                                               
088300                                                                          
088400     .                                                                    
088500     EJECT                                                                
088600                                                                          
088700                                                                          
088800 C-BEHANDLA-ARTIKEL SECTION.                                              
088900     MOVE 'C-BEHANDLA-ARTIKEL '  TO CURRENT-SECTION                       
089000                                                                          
089100     PERFORM IMS-GU-K626                                                  
089200     IF SEGMENT-SAKNAS                                                    
089300        MOVE ZERO            TO JUST-DAMANSEA                             
089400                                JUST-DASPSEA                              
089500        MOVE 1.0             TO JUST-RESEASON (1)                         
089600                                JUST-RESEASON (2)                         
089700                                JUST-RESEASON (3)                         
089800                                JUST-RESEASON (4)                         
089900                                JUST-RESEASON (5)                         
090000                                JUST-RESEASON (6)                         
090100                                JUST-RESEASON (7)                         
090200                                JUST-RESEASON (8)                         
090300                                JUST-RESEASON (9)                         
090400                                JUST-RESEASON (10)                        
090500                                JUST-RESEASON (11)                        
090600                                JUST-RESEASON (12)                        
090700     END-IF                                                               
090800                                                                          
090900     IF JUST-RESEASON (DAGENS-TIRP) > 0.5                                 
091000                                                                          
091100        MOVE NEJ TO ARTIKEL-SW                                            
091200                                                                          
091300        PERFORM CA-UPPDATERA-PERIODTABELL                                 
091400                                                                          
091500        PERFORM CB-UTFOR-BERAKNINGAR                                      
091600                                                                          
091700        IF ARTIKEL-SKALL-FORAENDRAS                                       
091800                                                                          
091900          IF NY-KVPB-SEP > CLAG-KVPB-SEP + WS-ONORM-OI-GRAENS-PB          
092000            CONTINUE                                                      
092100                                                                          
092200*-----      SKRIV POST PÅ ARTIKELFIL                                      
092300            MOVE W-IDARTNR             TO UT-IDARTNR                      
092400            MOVE CLAG-KVPB-SEP         TO UT-KVPB-SEP                     
092500            COMPUTE UT-RVPROFEL = CLAG-RVPROFEL + 1                       
092600            PERFORM S12-SKRIV-W22221                                      
092700                                                                          
092800*-----      WRITE POST ON ONORMAL FILE                                    
092900            IF CLAG-REDIRLEV = 1.0                                        
093000               CONTINUE                                                   
093100            ELSE                                                          
093200               PERFORM CC-UTFOR-ONORMAL                                   
093300            END-IF                                                        
093400                                                                          
093500          ELSE                                                            
093600            IF NY-KVPB-SEP = ZERO                                         
093700            AND CLAG-KVPB-SEP > ZERO                                      
093800*-----      PROGNOSEN FÅR EJ SJUNKA TILL NOLL                             
093900              MOVE 0.1                 TO NY-KVPB-SEP                     
094000            END-IF                                                        
094100                                                                          
094200*-----      SKRIV POST PÅ ARTIKELFIL                                      
094300            MOVE W-IDARTNR             TO UT-IDARTNR                      
094400            MOVE NY-KVPB-SEP           TO UT-KVPB-SEP                     
094500            MOVE ZERO                  TO UT-RVPROFEL                     
094600            PERFORM S12-SKRIV-W22221                                      
094700                                                                          
094800            IF WS-IDLEVNR = '1002 '                                       
094900            AND CLAG-KVPB-SEP NOT = NY-KVPB-SEP                           
095000              MOVE W-IDARTNR           TO UT5-IDARTNR-SATS                
095100              MOVE +1                  TO UT5-KDCLAGER                    
095200              MOVE NY-KVPB-SEP         TO UT5-KVPB-SEP-NY                 
095300              MOVE CLAG-KVPB-SEP       TO UT5-KVPB-SEP-GAMMAL             
095400              PERFORM S16-SKRIV-W22221D                                   
095500            END-IF                                                        
095600                                                                          
095700            IF CLAG-KVPB-SEP NOT = NY-KVPB-SEP                            
095800               MOVE W-IDARTNR          TO UT4-IDARTNR                     
095900               PERFORM S15-SKRIV-W22221C                                  
096000            END-IF                                                        
096100                                                                          
096200*-----      WRITE POST ON ONORMAL FILE                                    
096300            IF NY-KVPB-SEP <                                              
096400                        (CLAG-KVPB-SEP - WS-ONORM-OI-GRAENS-PB)           
096500               IF CLAG-REDIRLEV = 1.0                                     
096600                  CONTINUE                                                
096700               ELSE                                                       
096800                  PERFORM CC-UTFOR-ONORMAL                                
096900               END-IF                                                     
097000            END-IF                                                        
097100*                                                                         
097200*           IF NY-KVPB-SEP = ZERO                                         
097300*           AND CLAG-KVPB-SEP > ZERO                                      
097400                                                                          
097500*-----                                                                    
097600*-----        SKRIV POST PÅ LARMFIL                                       
097700*-----        ONORMAL FÖRÄNDRING                                          
097800*-----                                                                    
097900*             MOVE CLAG-IDANSK         TO UT3-IDANSK                      
098000*             MOVE W-IDARTNR           TO UT3-IDARTNR                     
098100*             MOVE CLAG-KVPB-SEP       TO UT3-KVPB-SEP                    
098200*             MOVE NY-KVPB-SEP         TO UT3-NY-KVPB-SEP                 
098300*             PERFORM S14-SKRIV-W22221B                                   
098400*           END-IF                                                        
098500                                                                          
098600          END-IF                                                          
098700        END-IF                                                            
098800     END-IF                                                               
098900     .                                                                    
099000     EJECT                                                                
099100 CA-UPPDATERA-PERIODTABELL SECTION.                                       
099200     MOVE 'CA-UPPDATERA-PERIODTABELL '  TO CURRENT-SECTION                
099300                                                                          
099400     PERFORM CAA-NOLLSTAELL-FAELT                                         
099500     PERFORM CAB-SUMMERA-PERIODTABELL                                     
099600     PERFORM CAC-JUSTERA-OI                                               
099700     PERFORM CAD-SASONGSANPASSA                                           
099800     .                                                                    
099900     EJECT                                                                
100000 CAA-NOLLSTAELL-FAELT SECTION.                                            
100100     MOVE 'CAA-NOLLSTAELL-FAELT '  TO CURRENT-SECTION                     
100200                                                                          
100300*---  NOLLSTÄLL KVOI I PERIODTABELLEN                                     
100400                                                                          
100500     MOVE 1 TO TAB-RADIX                                                  
100600     PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                              
100700       MOVE ZERO TO TABELL-KVOI (TAB-RADIX)                               
100800       ADD 1 TO TAB-RADIX                                                 
100900     END-PERFORM                                                          
101000                                                                          
101100*---  NOLLSTÄLL ARBETSFÄLT                                                
101200     MOVE ZERO TO WS-TIPBDAT-TIAARP                                       
101300                  WS-TEST-TIPBDAT                                         
101400                  WS-ONORM-OI-GRAENS-PB                                   
101500                  WS-KVOI-TOT                                             
101600                  WS-NY-KVPB-SEP                                          
101700                  NY-KVPB-SEP                                             
101800                  WS-PREL-KVPB-SEP                                        
101900                  WS-MEDEL-KVPB-SEP                                       
102000                  WS-ANTAL-FAKTORER-STOERRE-NOLL                          
102100                  WS-KVOTEN                                               
102200                                                                          
102300     .                                                                    
102400     EJECT                                                                
102500 CAB-SUMMERA-PERIODTABELL SECTION.                                        
102600     MOVE 'CAB-SUMMERA-PERIODTABELL ' TO CURRENT-SECTION                  
102700*---------------------------------------------------------------*         
102800* HÄR SUMMERAS DE VECKOR IN I RÄTT PERIOD                       *         
102900* EV. NEGATIV OI ÄNDRAS TILL NOLL.                              *         
103000*---------------------------------------------------------------*         
103100                                                                          
103200     MOVE DAGENS-TISSSSMMDD (1:4)                                         
103300                             TO W-TIAAAA                                  
103400     PERFORM IMS-GU-L811                                                  
103500                                                                          
103600*--- SUMMERA ALLA PERIODER                                                
103700                                                                          
103800     MOVE +1 TO TAB-RADIX                                                 
103900     PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                              
104000       MOVE TABELL-SISTA-TIAAVV(TAB-RADIX)                                
104100                             TO VECKO-INDX                                
104200       PERFORM UNTIL VECKO-INDX < TABELL-FORSTA-TIAAVV(TAB-RADIX)         
104300         IF SEGMENT-FINNS                                                 
104400            ADD AAR-KVOI-PROG(VECKO-INDX)                                 
104500                             TO TABELL-KVOI(TAB-RADIX)                    
104600         END-IF                                                           
104700         SUBTRACT +1 FROM VECKO-INDX                                      
104800       END-PERFORM                                                        
104900       ADD +1 TO TAB-RADIX                                                
105000       IF VECKO-INDX = ZERO                                               
105100          SUBTRACT 1         FROM W-TIAAAA                                
105200          PERFORM IMS-GU-L811                                             
105300       END-IF                                                             
105400                                                                          
105500     END-PERFORM                                                          
105600                                                                          
105700*--- KOLLA OM EV. NEGATIV OI I TABELLEN - ÄNDRA TILL 0                    
105800                                                                          
105900     MOVE +1 TO TAB-RADIX                                                 
106000     PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                              
106100       IF TABELL-KVOI(TAB-RADIX) < 0                                      
106200         MOVE ZERO TO TABELL-KVOI(TAB-RADIX)                              
106300       END-IF                                                             
106400       ADD +1 TO TAB-RADIX                                                
106500     END-PERFORM                                                          
106600                                                                          
106700     .                                                                    
106800     EJECT                                                                
106900 CAC-JUSTERA-OI SECTION.                                                  
107000     MOVE 'CAC-JUSTERA-OI '  TO CURRENT-SECTION                           
107100*---------------------------------------------------------------*         
107200* HÄR JUSTERAS OI BEROENDE PÅ OM DET ÄR 4 ELLER 5 VECKOR        *         
107300* I PERIODEN. GENOMSNITTLIG OI PER VECKA RÄKNAS UT OCH          *         
107400* MULTIPLICERAS MED 4.33, EG 52/12 (VECKOR/MÅNADER)             *         
107500*---------------------------------------------------------------*         
107600                                                                          
107700     IF ACC-KORNING                                                       
107800                                                                          
107900       MOVE +1 TO TAB-RADIX                                               
108000                                                                          
108100     ELSE                                                                 
108200                                                                          
108300       COMPUTE WS-VECKO-IO ROUNDED =                                      
108400               TABELL-KVOI(1) / WS-VECKA-I-AKT-PERIOD                     
108500       COMPUTE TABELL-KVOI(1) = WS-VECKO-IO * 4.33                        
108600                                                                          
108700       MOVE +2 TO TAB-RADIX                                               
108800                                                                          
108900     END-IF                                                               
109000                                                                          
109100     PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                              
109200                                                                          
109300       MOVE TABELL-TIAARP(TAB-RADIX) TO DAT-I-TIDATUM                     
109400       MOVE 'AARP  '  TO DAT-KDDATFORM                                    
109500       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
109600                           DAT-O-TIDATUM DAT-KDSVAR                       
109700                                                                          
109800       IF DAT-KDSVAR-OK                                                   
109900         MOVE DAT-KVVIPER TO WS-ANTAL-VECKOR                              
110000       ELSE                                                               
110100         MOVE 'SVAR 6 FRÅN WDATKONV EJ OK' TO FELTEXT-STR                 
110200         DISPLAY FELTEXT                                                  
110300         PERFORM S99-ABEND                                                
110400       END-IF                                                             
110500                                                                          
110600       COMPUTE WS-VECKO-IO =                                              
110700                       TABELL-KVOI(TAB-RADIX) / WS-ANTAL-VECKOR           
110800       COMPUTE TABELL-KVOI(TAB-RADIX) = WS-VECKO-IO * 4.33                
110900                                                                          
111000       ADD +1 TO TAB-RADIX                                                
111100     END-PERFORM                                                          
111200     .                                                                    
111300     EJECT                                                                
111400                                                                          
111500                                                                          
111600 CAD-SASONGSANPASSA SECTION.                                              
111700     MOVE 'CAD-SASONGSANPASSA '  TO CURRENT-SECTION                       
111800                                                                          
111900     MOVE 1 TO TAB-RADIX                                                  
112000     PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                              
112100        MOVE TABELL-TIAARP(TAB-RADIX) TO SEASON-TIAARP                    
112200        MOVE TABELL-KVOI(TAB-RADIX) TO WS-KVOI-SEASON                     
112300        IF JUST-RESEASON(SEASON-TIRP) = ZERO                              
112400           CONTINUE                                                       
112500        ELSE                                                              
112600           IF JUST-RESEASON(SEASON-TIRP) < 0.51                           
112700              MOVE CLAG-KVPB-SEP                                          
112800                             TO TABELL-KVOI(TAB-RADIX)                    
112900           ELSE                                                           
113000              COMPUTE TABELL-KVOI(TAB-RADIX) =                            
113100              WS-KVOI-SEASON / JUST-RESEASON (SEASON-TIRP)                
113200           END-IF                                                         
113300        END-IF                                                            
113400        ADD 1 TO TAB-RADIX                                                
113500     END-PERFORM                                                          
113600     .                                                                    
113700     EJECT                                                                
113800                                                                          
113900                                                                          
114000 CB-UTFOR-BERAKNINGAR SECTION.                                            
114100     MOVE 'CB-UTFOR-BERAKNINGAR '  TO CURRENT-SECTION                     
114200                                                                          
114300     PERFORM S01-KONTROLLERA-BERAEKNA-PROGN                               
114400     .                                                                    
114500     EJECT                                                                
114600                                                                          
114700                                                                          
114800 CC-UTFOR-ONORMAL     SECTION.                                            
114900     MOVE 'CC-UTFOR-ONORMAL     '    TO CURRENT-SECTION                   
115000                                                                          
115100     IF (WS-IDLEVNR                   = '9998' OR '9999' )                
115200        CONTINUE                                                          
115300     ELSE                                                                 
115400        IF CLAG-IDDC-REF              > SPACES                            
115500           MOVE W-IDARTNR            TO UT6-IDARTNR                       
115600           MOVE WS-IDDC-SE           TO UT6-IDDC                          
115700           MOVE CLAG-KVPB-SEP        TO UT6-KVPB-SEP                      
115800           MOVE NY-KVPB-SEP          TO UT6-NY-KVPB-SEP                   
115900           MOVE CLAG-IDDC-REF        TO UT6-IDDC-REF                      
116000           MOVE ZERO                 TO UT6-IDANSK                        
116100*                                                                         
116200           PERFORM IMS-GU-K629                                            
116300           IF SEGMENT-FINNS                                               
116400              MOVE CREF-IDPERSON-BUY                                      
116500                                     TO UT6-IDPERSON-BUY                  
116600           ELSE                                                           
116700              MOVE ZERO              TO UT6-IDPERSON-BUY                  
116800           END-IF                                                         
116900*                                                                         
117000           PERFORM IMS-GU-WDD311-BSEQ                                     
117100           IF SEGMENT-FINNS                                               
117200              MOVE TEXT-BEART        TO UT6-BEART                         
117300           ELSE                                                           
117400              MOVE SPACE             TO UT6-BEART                         
117500           END-IF                                                         
117600           PERFORM S17-SKRIV-W22221E                                      
117700        ELSE                                                              
117800           IF CLAG-KDERS         NOT  > 20                                
117900              IF CLAG-IDANSK          > ZERO                              
118000                 MOVE CLAG-IDANSK    TO WS-IDANSK                         
118100                 MOVE ZERO           TO WS-IDANSK3                        
118200                 MOVE WS-IDANSK      TO UT6-IDANSK                        
118300              ELSE                                                        
118400                 MOVE ZERO           TO UT6-IDANSK                        
118500              END-IF                                                      
118600              MOVE W-IDARTNR         TO UT6-IDARTNR                       
118700              MOVE WS-IDDC-SE        TO UT6-IDDC                          
118800              MOVE CLAG-KVPB-SEP     TO UT6-KVPB-SEP                      
118900              MOVE NY-KVPB-SEP       TO UT6-NY-KVPB-SEP                   
119000              MOVE ZERO              TO UT6-IDPERSON-BUY                  
119100              MOVE SPACES            TO UT6-IDDC-REF                      
119200                                        UT6-BEART                         
119300              PERFORM S17-SKRIV-W22221E                                   
119400           END-IF                                                         
119500        END-IF                                                            
119600     END-IF                                                               
119700     .                                                                    
119800     EJECT                                                                
119900                                                                          
120000                                                                          
120100 D-NOLLA-PB-ERS-20 SECTION.                                               
120200     MOVE 'D-NOLLA-PB-ERS-20 '  TO CURRENT-SECTION                        
120300                                                                          
120400     MOVE W-IDARTNR            TO UT-IDARTNR                              
120500     MOVE ZERO                 TO UT-KVPB-SEP                             
120600     MOVE ZERO                 TO UT-RVPROFEL                             
120700                                                                          
120800     PERFORM S12-SKRIV-W22221                                             
120900     .                                                                    
121000     EJECT                                                                
121100                                                                          
121200                                                                          
121300 Z-FINIT SECTION.                                                         
121400                                                                          
121500     CLOSE W22221                                                         
121600           W22221B                                                        
121700           W22221C                                                        
121800           W22221D                                                        
121900           W22221E                                                        
122000                                                                          
122100     MOVE 'S' TO POSTSUM-OPKOD                                            
122200     CALL POSTSUM USING POSTSUM-PARM                                      
122300     .                                                                    
122400     EJECT                                                                
122500 S01-KONTROLLERA-BERAEKNA-PROGN SECTION.                                  
122600                                                                          
122700     MOVE CLAG-KVPB-SEP      TO NY-KVPB-SEP                               
122800     PERFORM S01A-KOLLA-DATUM-MANUELL-PROGN                               
122900     PERFORM S01E-KOLLA-PROGNOS-LAASNING                                  
123000                                                                          
123100     IF (MANUELL-PROGNOS-SATT AND                                         
123230        (WS-TIPBDAT-TIAARP = 1 OR 2))                                     
123300*----- MANUELLT SATT PROGNOS SKALL GÄLLA, DVS MANUELL PB SATT I           
123400*----- TVÅ SENASTE PERIODERNA                                             
123500         CONTINUE                                                         
123600     ELSE                                                                 
123700       PERFORM S01B-KOLLA-OI                                              
123800       PERFORM S01C-KOLLA-MANUELL-PROGNOS                                 
123900       PERFORM S01D-BERAEKNA-NY-PROGNOS                                   
124000     END-IF                                                               
124100     .                                                                    
124200     EJECT                                                                
124300 S01A-KOLLA-DATUM-MANUELL-PROGN SECTION.                                  
124400                                                                          
124500     IF CLAG-TIPBDAT > 0                                                  
124600        MOVE CLAG-TIPBDAT    TO TMP1-YYWWD                                
124700        MOVE WS-TIFINLV      TO TMP2-YYWWD                                
124800        PERFORM WY2000P2                                                  
124900        IF TMP1-YYWWD > TMP2-YYWWD                                        
125000           MOVE CLAG-TIPBDAT TO WS-TEST-TIPBDAT                           
125100        ELSE                                                              
125200           MOVE WS-TIFINLV   TO WS-TEST-TIPBDAT                           
125300        END-IF                                                            
125400     ELSE                                                                 
125500        MOVE WS-TIFINLV                 TO TMP1-YYWWD                     
125600        MOVE DAGENS-TIAAVVD-LAST-YEAR   TO TMP2-YYWWD                     
125700        PERFORM WY2000P2                                                  
125800        IF TMP1-YYWWD >= TMP2-YYWWD                                       
125900           MOVE WS-TIFINLV   TO WS-TEST-TIPBDAT                           
126000        END-IF                                                            
126100     END-IF                                                               
126200                                                                          
126300     IF WS-TEST-TIPBDAT > 0                                               
126400       IF WS-TEST-TIPBDAT (5:1) = ZERO                                    
126500          MOVE 1             TO WS-TEST-TIPBDAT (5:1)                     
126600       END-IF                                                             
126700*----- KONTROLLERA VILKEN VECKA TIPBDAT LIGGER I                          
126800       MOVE 'AAVVD'      TO DAT-KDDATFORM                                 
126900       MOVE WS-TEST-TIPBDAT TO DAT-I-TIDATUM                              
127000                                                                          
127100       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
127200                           DAT-O-TIDATUM DAT-KDSVAR                       
127300       MOVE DAT-TIAAVV-GRP    TO WS-DAT-TIAAVV                            
127400                                                                          
127500       MOVE TABELL-TIAARP(1)  TO WS-TIAAVV                                
127600       MOVE TABELL-SISTA-TIAAVV(1) TO WS-TIVV                             
127700       MOVE WS-TIAAVV         TO WS-FORSTA-TIAAVV                         
127800                                                                          
127900       MOVE TABELL-TIAARP (MAX-TAB-RADIX) TO WS-TIAAVV                    
128000       MOVE TABELL-FORSTA-TIAAVV (MAX-TAB-RADIX) TO WS-TIVV               
128100       MOVE WS-TIAAVV                    TO WS-SISTA-TIAAVV               
128200                                                                          
128300       IF DAT-KDSVAR-OK                                                   
128400*-----   KOLLA ATT MANUELLT PB LIGGER I TABELLEN                          
128500         MOVE WS-DAT-TIAAVV      TO TMP1-YYWW                             
128600         MOVE WS-FORSTA-TIAAVV   TO TMP2-YYWW                             
128700         MOVE WS-SISTA-TIAAVV    TO TMP3-YYWW                             
128800         PERFORM WY2000Q3                                                 
128900         IF TMP1-YYWW <= TMP2-YYWW AND                                    
129000            TMP1-YYWW >= TMP3-YYWW                                        
129100           MOVE JA TO MANUELL-PROGNOS-SW                                  
129200*-----     KOLLA I VILKEN PERIOD TIREFMPB LIGGER                          
129300           MOVE +1 TO TAB-RADIX                                           
129400           MOVE NEJ TO INDX-SW                                            
129500           PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX OR INDX-HITTAT         
129600             MOVE WS-DAT-TIAAVV                                           
129700                             TO TMP1-YYWW                                 
129800                                                                          
129900             MOVE TABELL-TIAARP(TAB-RADIX)                                
130000                             TO WS-TIAAVV                                 
130100             MOVE TABELL-FORSTA-TIAAVV(TAB-RADIX)                         
130200                             TO WS-TIVV                                   
130300             MOVE WS-TIAAVV  TO WS-FORSTA-TIAAVV                          
130400             MOVE WS-FORSTA-TIAAVV                                        
130500                             TO TMP2-YYWW                                 
130600                                                                          
130700             MOVE TABELL-TIAARP(TAB-RADIX)                                
130800                             TO WS-TIAAVV                                 
130900             MOVE TABELL-SISTA-TIAAVV(TAB-RADIX)                          
131000                             TO WS-TIVV                                   
131100             MOVE WS-TIAAVV  TO WS-SISTA-TIAAVV                           
131200             MOVE WS-SISTA-TIAAVV                                         
131300                             TO TMP3-YYWW                                 
131400                                                                          
131500             PERFORM WY2000Q3                                             
131600             IF  TMP1-YYWW >= TMP2-YYWW                                   
131700             AND TMP1-YYWW <= TMP3-YYWW                                   
131800*-----         TRÄFF I RÄTT PERIOD                                        
131900               MOVE TAB-RADIX TO WS-TIPBDAT-TIAARP                        
132000               MOVE JA TO INDX-SW                                         
132100             END-IF                                                       
132200             ADD +1 TO TAB-RADIX                                          
132300           END-PERFORM                                                    
132400         ELSE                                                             
132500           MOVE NEJ TO MANUELL-PROGNOS-SW                                 
132600         END-IF                                                           
132700       ELSE                                                               
132800         MOVE NEJ TO MANUELL-PROGNOS-SW                                   
132900       END-IF                                                             
133000     ELSE                                                                 
133100       MOVE NEJ TO MANUELL-PROGNOS-SW                                     
133200     END-IF                                                               
133300                                                                          
133400     .                                                                    
133500     EJECT                                                                
133600 S01B-KOLLA-OI SECTION.                                                   
133700                                                                          
133800     PERFORM S01BA-BERAKN-ONORM-OI-GRAENSER                               
133900     .                                                                    
134000     EJECT                                                                
134100 S01BA-BERAKN-ONORM-OI-GRAENSER SECTION.                                  
134200                                                                          
134300*--- BERÄKNA GRÄNSER FÖR ONORMAL OI (COPYTEXT W222FSG)                    
134400*--- MED HJÄLP AV PROGNOS                                                 
134500                                                                          
134600     MOVE 1 TO INDX                                                       
134700     PERFORM UNTIL INDX > MAX-FSGFAKT                                     
134800       IF CLAG-PRARTSTD <=                                                
134900                               FSG-PRARTSTD-MAX (INDX)                    
135000         COMPUTE WS-ONORM-OI-GRAENS-PB ROUNDED =                          
135100            (FSG-A (INDX) * CLAG-KVPB-SEP) +                              
135200             FSG-B (INDX)                                                 
135300         MOVE 99 TO INDX                                                  
135400       ELSE                                                               
135500         ADD 1 TO INDX                                                    
135600       END-IF                                                             
135700     END-PERFORM                                                          
135800     .                                                                    
135900     EJECT                                                                
136000 S01C-KOLLA-MANUELL-PROGNOS SECTION.                                      
136100                                                                          
136200                                                                          
136300     IF MANUELL-PROGNOS-SATT                                              
136400                                                                          
136500*----- BYT UT ALLA OI FR.O.M TIPBDAT PERIOD OCH BAKÅT                     
136600*------MOT SPARAD PROGNOS (KVPB-HIST)                                     
136700       MOVE WS-TIPBDAT-TIAARP TO TAB-RADIX                                
136800       PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                            
136900         IF CLAG-TIPBDAT > 0                                              
137000            IF CLAG-KVPB-HIST > ZERO                                      
137100               MOVE CLAG-KVPB-HIST                                        
137200                             TO TABELL-KVOI (TAB-RADIX)                   
137300            ELSE                                                          
137400               MOVE CLAG-KVPB-SEP                                         
137500                             TO TABELL-KVOI (TAB-RADIX)                   
137600            END-IF                                                        
137700         ELSE                                                             
137800            MOVE CLAG-KVPB-SEP                                            
137900                             TO TABELL-KVOI (TAB-RADIX)                   
138000         END-IF                                                           
138100         ADD 1 TO TAB-RADIX                                               
138200       END-PERFORM                                                        
138300                                                                          
138400     END-IF                                                               
138500                                                                          
138600     .                                                                    
138700     EJECT                                                                
138800 S01D-BERAEKNA-NY-PROGNOS SECTION.                                        
138900                                                                          
139000     PERFORM S01DA-BERAEKNA-PREL-PROGNOS                                  
139100                                                                          
139200     IF WS-PREL-KVPB-SEP < 1                                              
139300       MOVE WS-PREL-KVPB-SEP TO WS-NY-KVPB-SEP                            
139400     ELSE                                                                 
139500       PERFORM S01DD-BERAEKNA-MEDELPROGNOS                                
139600                                                                          
139700       COMPUTE WS-KVOTEN ROUNDED = (WS-PREL-KVPB-SEP + 1) /               
139800                                   (WS-MEDEL-KVPB-SEP + 1)                
139900                                                                          
140000       PERFORM S01-KONTROLLERA-TREND                                      
140100       IF INGEN-TREND                                                     
140200         MOVE WS-PREL-KVPB-SEP TO WS-NY-KVPB-SEP                          
140300       ELSE                                                               
140400         IF SVAG-TREND                                                    
140500           MOVE 2 TO INDX                                                 
140600         ELSE                                                             
140700           IF STARK-TREND                                                 
140800             MOVE 3 TO INDX                                               
140900           END-IF                                                         
141000         END-IF                                                           
141100                                                                          
141200         IF ACC-KORNING                                                   
141300                                                                          
141400           MOVE +1 TO TAB-RADIX                                           
141500           MOVE ZERO TO WS-NY-KVPB-SEP                                    
141600                                                                          
141700           PERFORM UNTIL TAB-RADIX > 12                                   
141800             COMPUTE WS-NY-KVPB-SEP ROUNDED =                             
141900                       WS-NY-KVPB-SEP +                                   
142000                (PROG-TRENDFAKT (TAB-RADIX, INDX)                *        
142100                         TABELL-KVOI (TAB-RADIX))                         
142200             ADD 1 TO TAB-RADIX                                           
142300           END-PERFORM                                                    
142400         ELSE                                                             
142500                                                                          
142600           MOVE +1 TO TAB-RADIX                                           
142700           MOVE ZERO TO WS-NY-KVPB-SEP                                    
142800                                                                          
142900           PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                        
143000             COMPUTE WS-NY-KVPB-SEP ROUNDED =                             
143100                       WS-NY-KVPB-SEP +                                   
143200              (WS-TRENDFAKT-VKA (TAB-RADIX, INDX)                *        
143300                         TABELL-KVOI (TAB-RADIX))                         
143400             ADD 1 TO TAB-RADIX                                           
143500           END-PERFORM                                                    
143600         END-IF                                                           
143700                                                                          
143800       END-IF                                                             
143900     END-IF                                                               
144000                                                                          
144100*--- NY PROGNOS KAN INTE BLI LÄGRE ÄN 70% AV DEN GAMLA                    
144200*--- (KONTROLL SÅ DEN INTE SJUNKER FÖR SNABBT)                            
144300     IF  CLAG-KVPB-SEP  = 0.1                                             
144400     AND WS-NY-KVPB-SEP < 0.1                                             
144500         CONTINUE                                                         
144600     ELSE                                                                 
144700        IF WS-NY-KVPB-SEP < 0.7 * CLAG-KVPB-SEP                           
144800           COMPUTE WS-NY-KVPB-SEP = 0.7 * CLAG-KVPB-SEP                   
144900        END-IF                                                            
145000     END-IF                                                               
145100                                                                          
145200*--- AVRUNDA TILL EN DECIMAL                                              
145300     COMPUTE NY-KVPB-SEP = WS-NY-KVPB-SEP + 0.05                          
145400                                                                          
145500     IF NY-KVPB-SEP NOT = CLAG-KVPB-SEP                                   
145600        IF PROGNOS-AER-LAAST                                              
145700           IF NY-KVPB-SEP > CLAG-KVPB-SEP                                 
145800*--- SÄTT NY PROGNOS FÖR ARTIKELN                                         
145900              MOVE JA         TO ARTIKEL-SW                               
146000           END-IF                                                         
146100        ELSE                                                              
146200*--- SÄTT NY PROGNOS FÖR ARTIKELN                                         
146300           MOVE JA         TO ARTIKEL-SW                                  
146400        END-IF                                                            
146500     END-IF                                                               
146600     .                                                                    
146700     EJECT                                                                
146800 S01DA-BERAEKNA-PREL-PROGNOS SECTION.                                     
146900*----------------------------------------------------------------*        
147000* HÄR BERÄKNAS PRELIMINÄR PROGNOS MHA VIKTNINGSTABELL            *        
147100* (PROGNOSFAKTORER) MED INDX=1 (NORMAL)                          *        
147200*----------------------------------------------------------------*        
147300                                                                          
147400     IF ACC-KORNING                                                       
147500       MOVE 1 TO INDX                                                     
147600                 TAB-RADIX                                                
147700       MOVE ZERO TO WS-PREL-KVPB-SEP                                      
147800                    WS-ANTAL-FAKTORER-STOERRE-NOLL                        
147900                                                                          
148000       PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                            
148100         COMPUTE WS-PREL-KVPB-SEP ROUNDED =                               
148200                        WS-PREL-KVPB-SEP +                                
148300                  (PROG-TRENDFAKT (TAB-RADIX, INDX) *                     
148400                         TABELL-KVOI (TAB-RADIX) )                        
148500         IF PROG-TRENDFAKT (TAB-RADIX, INDX) > ZERO                       
148600           ADD 1 TO WS-ANTAL-FAKTORER-STOERRE-NOLL                        
148700         END-IF                                                           
148800         ADD 1 TO TAB-RADIX                                               
148900       END-PERFORM                                                        
149000     ELSE                                                                 
149100       MOVE 1 TO INDX                                                     
149200                 TAB-RADIX                                                
149300       MOVE ZERO TO WS-PREL-KVPB-SEP                                      
149400                    WS-ANTAL-FAKTORER-STOERRE-NOLL                        
149500                                                                          
149600       PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                            
149700         COMPUTE WS-PREL-KVPB-SEP ROUNDED =                               
149800                        WS-PREL-KVPB-SEP +                                
149900                 (WS-TRENDFAKT-VKA (TAB-RADIX, INDX)             *        
150000                         TABELL-KVOI (TAB-RADIX) )                        
150100         IF WS-TRENDFAKT-VKA (TAB-RADIX, INDX) > ZERO                     
150200           ADD 1 TO WS-ANTAL-FAKTORER-STOERRE-NOLL                        
150300         END-IF                                                           
150400         ADD 1 TO TAB-RADIX                                               
150500       END-PERFORM                                                        
150600     END-IF                                                               
150700     .                                                                    
150800     EJECT                                                                
150900 S01DB-BERAEKNA-MEDELPROGNOS SECTION.                                     
151000*----------------------------------------------------------------*        
151100* HÄR BERÄKNAS MEDELPROGNOS                                      *        
151200*----------------------------------------------------------------*        
151300                                                                          
151400     IF ACC-KORNING                                                       
151500       MOVE +1 TO TAB-RADIX                                               
151600     ELSE                                                                 
151700       MOVE +2 TO TAB-RADIX                                               
151800     END-IF                                                               
151900                                                                          
152000     MOVE ZERO TO WS-KVOI-TOT                                             
152100                  WS-MEDEL-KVPB-SEP                                       
152200                                                                          
152300     PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                              
152400       COMPUTE WS-KVOI-TOT ROUNDED =                                      
152500                         WS-KVOI-TOT +                                    
152600                         TABELL-KVOI (TAB-RADIX)                          
152700       ADD 1 TO TAB-RADIX                                                 
152800     END-PERFORM                                                          
152900                                                                          
153000     IF WS-ANTAL-FAKTORER-STOERRE-NOLL = ZERO                             
153100       MOVE ZERO TO WS-MEDEL-KVPB-SEP                                     
153200     ELSE                                                                 
153300       COMPUTE WS-MEDEL-KVPB-SEP ROUNDED =                                
153400               WS-KVOI-TOT / 12                                           
153500     END-IF                                                               
153600     .                                                                    
153700     EJECT                                                                
153800 S01-KONTROLLERA-TREND SECTION.                                           
153900*----------------------------------------------------------------*        
154000* HÄR KONTROLLERAS OM ARTIKELN HAR NÅGON TREND MHA KVOTEN.       *        
154100* ARTIKELN KAN HA:                                               *        
154200*     ¤ INGEN TREND                                              *        
154300*     ¤ SVAG TREND  (TREND UPP ELLER TREND NER)                  *        
154400*     ¤ STARK TREND (TREND STARKT UPP ELLER TREND STARKT NER)    *        
154500*----------------------------------------------------------------*        
154600                                                                          
154700                                                                          
154800     IF (WS-KVOTEN >= 0.80) AND                                           
154900        (WS-KVOTEN <= 1.20)                                               
155000       MOVE 'INGEN' TO TREND-SW                                           
155100     ELSE                                                                 
155200       IF (WS-KVOTEN > 1.20 AND WS-KVOTEN < 1.35) OR                      
155300          (WS-KVOTEN > 0.64 AND WS-KVOTEN < 0.80)                         
155400         MOVE 'SVAG ' TO TREND-SW                                         
155500       ELSE                                                               
155600         MOVE 'STARK' TO TREND-SW                                         
155700       END-IF                                                             
155800     END-IF                                                               
155900     .                                                                    
156000     EJECT                                                                
156100 S01DD-BERAEKNA-MEDELPROGNOS SECTION.                                     
156200*----------------------------------------------------------------*        
156300* HÄR BERÄKNAS MEDELPROGNOS                                      *        
156400*----------------------------------------------------------------*        
156500                                                                          
156600     MOVE +1   TO TAB-RADIX                                               
156700                                                                          
156800     MOVE ZERO TO WS-KVOI-TOT                                             
156900                  WS-MEDEL-KVPB-SEP                                       
157000                                                                          
157100     IF ACC-KORNING                                                       
157200       PERFORM UNTIL TAB-RADIX > 12                                       
157300         COMPUTE WS-KVOI-TOT ROUNDED =                                    
157400                           WS-KVOI-TOT +                                  
157500                           TABELL-KVOI (TAB-RADIX)                        
157600         ADD 1 TO TAB-RADIX                                               
157700       END-PERFORM                                                        
157800                                                                          
157900       COMPUTE WS-MEDEL-KVPB-SEP ROUNDED =                                
158000                 WS-KVOI-TOT / 12                                         
158100     ELSE                                                                 
158200       PERFORM UNTIL TAB-RADIX > 13                                       
158300         IF TAB-RADIX = 1                                                 
158400         OR TAB-RADIX = 13                                                
158500            IF TAB-RADIX = 1                                              
158600               COMPUTE WS-KVOI-TOT ROUNDED =                              
158700                           WS-KVOI-TOT +                                  
158800                          (TABELL-KVOI (TAB-RADIX)                        
158900                        * (WS-VECKA-I-AKT-PERIOD / WS-KVVIPER))           
159000            ELSE                                                          
159100*                   ( TAB-RADIX = 13 )                                    
159200               COMPUTE WS-KVOI-TOT ROUNDED =                              
159300                           WS-KVOI-TOT +                                  
159400                          (TABELL-KVOI (TAB-RADIX)                        
159500          * ((WS-KVVIPER - WS-VECKA-I-AKT-PERIOD) / WS-KVVIPER))          
159600            END-IF                                                        
159700         ELSE                                                             
159800            COMPUTE WS-KVOI-TOT ROUNDED =                                 
159900                           WS-KVOI-TOT +                                  
160000                           TABELL-KVOI (TAB-RADIX)                        
160100         END-IF                                                           
160200         ADD 1 TO TAB-RADIX                                               
160300       END-PERFORM                                                        
160400                                                                          
160500       COMPUTE WS-MEDEL-KVPB-SEP ROUNDED =                                
160600                 WS-KVOI-TOT / 12                                         
160700     END-IF                                                               
160800     .                                                                    
160900     EJECT                                                                
161000 S01E-KOLLA-PROGNOS-LAASNING SECTION.                                     
161100                                                                          
161200     MOVE NEJ                TO LOCK-SW                                   
161300                                                                          
161400*    KONTROLLERA OM ARTIKELN HAR EN LÅST PROGNOS                          
161500*    (FÅR HÖJAS MEN EJ SÄNKAS)                                            
161600     IF CLAG-TIPBLOCK >= DAGENS-DATUM                                     
161700        MOVE JA              TO LOCK-SW                                   
161800     END-IF                                                               
161900                                                                          
162000     .                                                                    
162100     EJECT                                                                
162200 S12-SKRIV-W22221 SECTION.                                                
162300                                                                          
162400     WRITE UT-POST FROM UT-AREA                                           
162500                                                                          
162600     MOVE 'W22221'   TO POSTSUM-FDNAMN                                    
162700     MOVE 'W22221D1' TO POSTSUM-DDNAMN2                                   
162800     CALL POSTSUM USING POSTSUM-PARM                                      
162900     .                                                                    
163000     SKIP3                                                                
163100                                                                          
163200 S14-SKRIV-W22221B SECTION.                                               
163300                                                                          
163400     WRITE UT3-POST FROM UT3-AREA                                         
163500                                                                          
163600     MOVE 'W22221B'  TO POSTSUM-FDNAMN                                    
163700     MOVE 'W22221D2' TO POSTSUM-DDNAMN2                                   
163800     CALL POSTSUM USING POSTSUM-PARM                                      
163900     .                                                                    
164000     SKIP3                                                                
164100                                                                          
164200 S15-SKRIV-W22221C SECTION.                                               
164300                                                                          
164400     WRITE UT4-POST FROM UT4-AREA                                         
164500                                                                          
164600     MOVE 'W22221C'  TO POSTSUM-FDNAMN                                    
164700     MOVE 'W22221D3' TO POSTSUM-DDNAMN2                                   
164800     CALL POSTSUM USING POSTSUM-PARM                                      
164900     .                                                                    
165000     SKIP3                                                                
165100                                                                          
165200 S16-SKRIV-W22221D SECTION.                                               
165300                                                                          
165400     WRITE UT5-POST FROM UT5-AREA                                         
165500                                                                          
165600     MOVE 'W22221D'  TO POSTSUM-FDNAMN                                    
165700     MOVE 'W22221D4' TO POSTSUM-DDNAMN2                                   
165800     CALL POSTSUM USING POSTSUM-PARM                                      
165900     .                                                                    
166000     SKIP3                                                                
166100                                                                          
166200 S17-SKRIV-W22221E SECTION.                                               
166300                                                                          
166400     WRITE UT6-POST FROM UT6-AREA                                         
166500                                                                          
166600     MOVE 'W22221D'  TO POSTSUM-FDNAMN                                    
166700     MOVE 'W22221D5' TO POSTSUM-DDNAMN2                                   
166800     CALL POSTSUM USING POSTSUM-PARM                                      
166900     .                                                                    
167000     SKIP3                                                                
167100                                                                          
167200 S99-ABEND SECTION.                                                       
167300                                                                          
167400     MOVE 'S' TO POSTSUM-OPKOD                                            
167500     CALL POSTSUM USING POSTSUM-PARM                                      
167600     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
167700     .                                                                    
167800     EJECT                                                                
167900* --- IMS SEKTIONER ---                                                   
168000     SKIP3                                                                
168100     EJECT                                                                
168200 IMS-GN-WDK6 SECTION.                                                     
168300                                                                          
168400     CALL CBLTDLI USING GN SB-WDK6-PCB DLI-IO-AREA-K6                     
168500     MOVE SB-WDK6-STATUS-CODE TO STATUS-WS                                
168600     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
168700     PERFORM IMS-STATUSKONTROLL                                           
168800     .                                                                    
168900     EJECT                                                                
169000 IMS-GU-K611 SECTION.                                                     
169100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
169200          DELIMITED BY SIZE INTO SSA1                                     
169300     MOVE 'WDK611  (KDSEGKEY =1)' TO SSA2                                 
169400     MOVE '  GE' TO GODK-STATUSKODER                                      
169500     CALL CBLTDLI USING GU                                                
169600                      WDK6-PCB DLI-IO-AREA-K6 SSA1 SSA2                   
169700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
169800     PERFORM IMS-STATUSKONTROLL                                           
169900     .                                                                    
170000     EJECT                                                                
170100 IMS-GU-K626 SECTION.                                                     
170200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
170300          DELIMITED BY SIZE INTO SSA1                                     
170400     MOVE 'WDK611  (KDSEGKEY =1)' TO SSA2                                 
170500     MOVE 'WDK626   ' TO SSA3                                             
170600     MOVE '  GE' TO GODK-STATUSKODER                                      
170700     CALL CBLTDLI USING GU                                                
170800                      WDK6-PCB DLI-IO-WDK626 SSA1 SSA2 SSA3               
170900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
171000     PERFORM IMS-STATUSKONTROLL                                           
171100     .                                                                    
171200     EJECT                                                                
171300 IMS-GU-K629 SECTION.                                                     
171400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
171500          DELIMITED BY SIZE INTO SSA1                                     
171600     MOVE 'WDK611  (KDSEGKEY =1)' TO SSA2                                 
171700     MOVE 'WDK629   ' TO SSA3                                             
171800     MOVE '  GE' TO GODK-STATUSKODER                                      
171900     CALL CBLTDLI USING GU                                                
172000                      WDK6-PCB DLI-IO-WDK629 SSA1 SSA2 SSA3               
172100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
172200     PERFORM IMS-STATUSKONTROLL                                           
172300     .                                                                    
172400     EJECT                                                                
172500                                                                          
172600 IMS-GU-L811  SECTION.                                                    
172700                                                                          
172800     STRING 'WDL801  (IDARTNR  =' W-IDARTNR-X ')'                         
172900          DELIMITED BY SIZE INTO SSA1                                     
173000     STRING 'WDL811  (TIAAAA   =' W-TIAAAA-X ')'                          
173100          DELIMITED BY SIZE INTO SSA2                                     
173200     MOVE '  GE' TO GODK-STATUSKODER                                      
173300     CALL CBLTDLI USING GU WDL8-PCB                                       
173400                                 DLI-IO-AREA-WDL811 SSA1 SSA2             
173500     MOVE WDL8-STATUS-CODE TO STATUS-WS                                   
173600     PERFORM IMS-STATUSKONTROLL                                           
173700     .                                                                    
173800     EJECT                                                                
173900 IMS-GU-WDD311-BSEQ SECTION.                                              
174000                                                                          
174100     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
174200             DELIMITED BY SIZE INTO SSA1                                  
174300     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
174400              DELIMITED BY SIZE INTO SSA2                                 
174500     MOVE '  GE' TO GODK-STATUSKODER                                      
174600     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
174700     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
174800     PERFORM IMS-STATUSKONTROLL                                           
174900     .                                                                    
175000     EJECT                                                                
175100 IMS-GU-WDB618 SECTION.                                                   
175200                                                                          
175300     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
175400          DELIMITED BY SIZE  INTO SSA1                                    
175500     STRING 'WDB618  (KDPROGOI =' W-KDPROGOI-X ')'                        
175600          DELIMITED BY SIZE INTO SSA2                                     
175700     MOVE '  GE'           TO GODK-STATUSKODER                            
175800     CALL CBLTDLI USING GU  WDB6-PCB DLI-IO-AREA-B618  SSA1 SSA2          
175900     MOVE WDB6-STATUS-CODE   TO STATUS-WS                                 
176000     PERFORM IMS-STATUSKONTROLL                                           
176100     .                                                                    
176200     EJECT                                                                
176300 IMS-STATUSKONTROLL SECTION.                                              
176400                                                                          
176500     SET STATUS-IX TO 1                                                   
176600     SEARCH GODK-STATUS                                                   
176700       AT END                                                             
176800         STRING 'OTILLÅTEN RETURKOD FRÅN IMS: ' STATUS-WS                 
176900           DELIMITED BY SIZE INTO FELTEXT-STR                             
177000         DISPLAY FELTEXT                                                  
177100         CALL FELLOG                                                      
177200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
177300         CONTINUE                                                         
177400     END-SEARCH                                                           
177500     .                                                                    
177600     EJECT                                                                
177700*    -COPY WY2000P1                                                       
177800     EJECT                                                                
177900*    -COPY WY2000P2                                                       
178000     EJECT                                                                
178100*    -COPY WY2000P3                                                       
178200     EJECT                                                                
178300*    -COPY WY2000Q3                                                       
