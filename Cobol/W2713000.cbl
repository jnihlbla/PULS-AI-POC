000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2713000.                                                
000400*AUTHOR.         ANN JORDEBO.                                             
000500*DATE-WRITTEN.   JAN 1994.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET BERÄKNAR NY PROGNOS FÖR ALLA AKTIVA                   
001100*        ARTIKLAR PÅ ALLA SDC                                             
001200*                                                                         
001300*        PROGRAMMET GÅR DAGLIGEN OCH VID SAMTLIGA VECKOSLUT               
001400*        - VID ACCOUNT-PERIODSKIFTE BERÄKNAS PROGNOS PÅ SAMTLIGA          
001500*          AKTIVA ARTIKLAR                                                
001600*                                                                         
001700*        PROGRAMMET KÖRS AV TRE JOBB, W271J030 W271J130 OCH               
001800*          W271J230. VARJE JOB TAR MED SIG EN UPPGIFT FRÅN                
001900*          VAR SIN CONSTANTMEDLEM MED UPPGIFT OM VILKEN                   
002000*          KÖRDAG DET ÄR.                                                 
002100*          DAY - PROGNOS BERÄKNAS FÖR NYAKTIVERADE ARTIKAR                
002200*          WEEK- (EJ PERIODKÖRNING) PROGNOS FÖR NYAKTIVERADE              
002300*                ARTIKLAR                                                 
002400*          ACC - PROGNOS FÖR SAMTLIGA AKTIVA ARTIKAR UTAN                 
002500*                ERSÄTTNINGSKOD                                           
002600*        - VID DAGLIG KÖRNING OCH VID VECKOSLUT, BERÄKNAS PROGNOS         
002700*        - ENBART PÅ SAMMA DAG AKTIVERADE ARTIKLAR.                       
002800*                                                                         
002900*        PROGRAMMET LÄSER      WDK7                                       
003000*                              WDL7                                       
003100*                                                                         
003200*                                                                         
003300*    ABENDKODER:                                                          
003400*        U0016 -  SVAR FRÅN WDATKONV EJ OK                                
003500*              -  "ÖVERSÄTTNING" AV LAGER TILL DC-INDX SAKNAS             
003600*        U1000 -  . . . .                                                 
003700     SKIP3                                                                
003800 ENVIRONMENT DIVISION.                                                    
003900     SKIP2                                                                
004000 INPUT-OUTPUT SECTION.                                                    
004100                                                                          
004200 FILE-CONTROL.                                                            
004300     SKIP2                                                                
004400*                                                                         
004500     SELECT W271TYP       ASSIGN W27130D1.                                
004600                                                                          
004700*          --- UPPGIFT OM DAG, VECKO- ELLER PERIODKÖRNING                 
004800                                                                          
004900     SELECT W271DC           ASSIGN W27110D2.                             
005000*         ---DC ATT KONTROLLERA I DENNA KÖRNING                           
005100                                                                          
005200     SELECT W27130                     ASSIGN TO W27130D3.                
005300*          --- UT-FIL                                                     
005400                                                                          
005500     SELECT W27138                     ASSIGN TO W27130D4.                
005600*          --- UT2-FIL, LARM AVVIKANDE PROGNOSER                          
005700                                                                          
005800     SELECT W2713F                     ASSIGN TO W27130D5.                
005900*          --- UT3-FIL, SLOW MOVING PARTS                                 
006000     EJECT                                                                
006100 DATA DIVISION.                                                           
006200     SKIP3                                                                
006300 FILE SECTION.                                                            
006400     SKIP3                                                                
006500                                                                          
006600 FD  W271TYP                                                              
006700     LABEL RECORD STANDARD                                                
006800     RECORDING F                                                          
006900     BLOCK CONTAINS 0.                                                    
007000                                                                          
007100 01  FILLER                  PIC X(80).                                   
007200                                                                          
007300                                                                          
007400 FD  W271DC                                                               
007500     LABEL RECORD STANDARD                                                
007600     RECORDING F                                                          
007700     BLOCK CONTAINS 0.                                                    
007800                                                                          
007900 01  FILLER                  PIC X(80).                                   
008000                                                                          
008100 FD  W27130                                                               
008200     RECORDING       F                                                    
008300     BLOCK CONTAINS  0.                                                   
008400                                                                          
008500*01  POST -COPY W27130  -PRE UT-    -L.                                   
008600                                                                          
008700 FD  W27138                                                               
008800     RECORDING       F                                                    
008900     BLOCK CONTAINS  0.                                                   
009000                                                                          
009100*01  POST -COPY W27138  -PRE UT2-    -L.                                  
009200                                                                          
009300 FD  W2713F                                                               
009400     RECORDING       F                                                    
009500     BLOCK CONTAINS  0.                                                   
009600                                                                          
009700*01  POST -COPY W27138  -PRE UT3-    -L.                                  
009800                                                                          
009900     EJECT                                                                
010000 WORKING-STORAGE SECTION.                                                 
010100     SKIP2                                                                
010200*    -COPY WY2000W1                                                       
010300     SKIP2                                                                
010400*    -COPY WY2000W3                                                       
010500     SKIP3                                                                
010600*    -COPY WY2000W2                                                       
010700     SKIP3                                                                
010800*    -COPY WWPRODSL                                                       
010900     SKIP3                                                                
011000 77  IDPGM                       PIC X(8)    VALUE 'W2713000'.            
011100 77  JA                          PIC X       VALUE 'J'.                   
011200 77  NEJ                         PIC X       VALUE 'N'.                   
011300 77  AKTIV                       PIC X       VALUE 'A'.                   
011400 77  DCS-TRAEFF                  PIC X       VALUE 'N'.                   
011500                                                                          
011600*    --- INDEX SAMT MAX-INDEX                                             
011700 77  FILLER                      PIC X(16)   VALUE 'INDEX'.               
011800 77  INDX                        PIC 9(2)    VALUE ZERO.                  
011900 77  DC-INDX                     PIC 9(2)    VALUE ZERO.                  
012000 77  IX-TAB                      PIC 9(2)    VALUE ZERO.                  
012100 77  IX-DC                       PIC 9(3)    VALUE ZERO.                  
012200 77  DC-MAX                      PIC 9(3)    VALUE 200.                   
012300 77  PER-INDX                    PIC 9(2)    VALUE ZERO.                  
012400 77  TREND-INDX                  PIC 9(2)    VALUE ZERO.                  
012500 77  MAX-FSGFAKT                 PIC 9(2)    VALUE 12.                    
012600                                                                          
012700 77  VECKO-INDX                  PIC 9(2)    VALUE ZERO.                  
012800                                                                          
012900 01  TAB-RADIX                   PIC 9(2)    VALUE ZERO.                  
013000 77  MAX-TAB-RADIX               PIC 9(2)    VALUE ZERO.                  
013100                                                                          
013200 77  VV-INDX                     PIC 9(2)    VALUE ZERO.                  
013300 77  MAX-VV-INDX                 PIC 9(2)    VALUE 53.                    
013400                                                                          
013500*    --- SWITCHAR                                                         
013600 01  FILLER                      PIC X(16)   VALUE 'SWITCHAR'.            
013700                                                                          
014900                                                                          
014910 01  DC-POST.                                                             
014920     03  DC-PARAMETER-TIME      PIC X(2).                                 
014930     03  FILLER                 PIC X(78).                                
014940                                                                          
015400 01  TREND-SW                    PIC X(5)    VALUE SPACE.                 
015500     88  INGEN-TREND                         VALUE 'INGEN'.               
015600     88  SVAG-TREND                          VALUE 'SVAG '.               
015700     88  STARK-TREND                         VALUE 'STARK'.               
015800                                                                          
015900 01  INDX-SW                     PIC X       VALUE 'N'.                   
016000     88  INDX-HITTAT                         VALUE 'J'.                   
016100                                                                          
016200 01  ARTIKEL-SW                  PIC X       VALUE 'N'.                   
016300     88  ARTIKEL-SKALL-FORAENDRAS            VALUE 'J'.                   
016400                                                                          
016500 01  BEHANDLA-SW                 PIC X       VALUE 'N'.                   
016600     88  BEHANDLA                            VALUE 'J'.                   
016700                                                                          
016800 01  ESCLOCK-SW                  PIC X       VALUE 'N'.                   
016900     88  PROGNOS-AER-LAAST                   VALUE 'J'.                   
017000     88  PROGNOS-AER-FAST                    VALUE 'A'.                   
017100                                                                          
017200 01  MANUELL-PROGNOS-SW          PIC X       VALUE 'N'.                   
017300     88  MANUELL-PROGNOS-SATT                VALUE 'J'.                   
017400                                                                          
017500 01  SW-MAN-30-DAYS              PIC X       VALUE 'N'.                   
017600     88  SW-MAN-30-DAYS-TRUE                 VALUE 'J'.                   
017700                                                                          
017800 01  PB-JUST-SW                  PIC X       VALUE 'N'.                   
017900*    --- ARBETSFÄLT                                                       
018000 01  FILLER                      PIC X(16)   VALUE 'ARBETSFÄLT'.          
018100 01  ARBETSFAELT.                                                         
018200     03  PERIODTABELL            OCCURS 13.                               
018300         05 TABELL-TIAARP        PIC  9(4)    VALUE ZERO.                 
018400         05 TABELL-FORSTA-TIAAVV PIC  9(2)    VALUE ZERO.                 
018500         05 TABELL-SISTA-TIAAVV  PIC  9(2)    VALUE ZERO.                 
018600         05 TABELL-KVOI        PIC S9(9)V9(1)  VALUE ZERO COMP-3.         
018700                                                                          
018800     03  SLUT-VV                 PIC 9(2)    VALUE ZERO.                  
018900     03  START-VV                PIC 9(2)    VALUE ZERO.                  
019000                                                                          
019100     03  WS-TIAAVV               PIC 9(4)    VALUE ZERO.                  
019200     03  FILLER REDEFINES WS-TIAAVV.                                      
019300         05 WS-TIAA              PIC 9(2).                                
019400         05 WS-TIVV              PIC 9(2).                                
019500                                                                          
019600     03  FOREG-TIAARP            PIC  9(4)   VALUE ZERO.                  
019700     03  FILLER REDEFINES FOREG-TIAARP.                                   
019800         05 FOREG-TIAA           PIC  9(2).                               
019900         05 FOREG-TIRP           PIC  9(2).                               
020000                                                                          
020100     03  DAGENS-TIAARP           PIC  9(4)   VALUE ZERO.                  
020200     03  FILLER REDEFINES DAGENS-TIAARP.                                  
020300         05 DAGENS-TIAA          PIC  9(2).                               
020400         05 DAGENS-TIRP          PIC  9(2).                               
020500                                                                          
020600     03  NAESTA-TIAARP           PIC  9(4)   VALUE ZERO.                  
020700     03  FILLER REDEFINES NAESTA-TIAARP.                                  
020800         05 NAESTA-TIAA          PIC  9(2).                               
020900         05 NAESTA-TIRP          PIC  9(2).                               
021000                                                                          
021100     03  SEASON-TIAARP           PIC  9(4)   VALUE ZERO.                  
021200     03  FILLER REDEFINES SEASON-TIAARP.                                  
021300         05 SEASON-TIAA          PIC  9(2).                               
021400         05 SEASON-TIRP          PIC  9(2).                               
021500                                                                          
021600     03  DAGENS-TIAAVVD          PIC  9(5)   VALUE ZERO.                  
021700     03  FILLER REDEFINES DAGENS-TIAAVVD.                                 
021800         05 DAGENS-TIAAVVD-AA    PIC  9(2).                               
021900         05 DAGENS-TIAAVVD-VV    PIC  9(2).                               
022000         05 DAGENS-TIAAVVD-D     PIC  9(1).                               
022100                                                                          
022200     03  DAGENS-TIAAVV-GRP       PIC  9(4)   VALUE ZERO.                  
022300                                                                          
022400     03  WS-DAGENS-DAG30         PIC  9(6)   VALUE ZERO.                  
022500                                                                          
022600                                                                          
022700     03  DAGENS-TIAAVVD-LAST-YEAR        PIC  9(5) VALUE ZERO.            
022800     03  FILLER REDEFINES DAGENS-TIAAVVD-LAST-YEAR.                       
022900         05 DAGENS-TIAAVVD-LAST-YEAR-AA  PIC 9(2).                        
023000         05 DAGENS-TIAAVVD-LAST-YEAR-VV  PIC 9(2).                        
023100         05 DAGENS-TIAAVVD-LAST-YEAR-D   PIC 9(1).                        
023200                                                                          
023300     03  DAGENS-TIVV             PIC  9(2)   VALUE ZERO.                  
023400                                                                          
023500     03  PUBLICERINGSDATUM       PIC  9(6)   VALUE ZERO.                  
023600                                                                          
023700     03  WS-ANTAL-VECKOR         PIC  9(2)      VALUE ZERO.               
023800     03  WS-KVVIPER              PIC  9(1)      VALUE ZERO.               
023900     03  WS-VECKO-IO             PIC S9(9)V9 VALUE ZERO COMP-3.           
024000     03  WS-TEST-TIREFMPB        PIC S9(7)   VALUE ZERO COMP-3.           
024100     03  WS-TIREFMPB-TIAARP      PIC  9(4)      VALUE ZERO.               
024200     03  WS-FORSTA-TIAAVV        PIC  9(4)      VALUE ZERO.               
024300     03  WS-SISTA-TIAAVV         PIC  9(4)      VALUE ZERO.               
024400     03  WS-DAT-TIAAVV           PIC  9(4)      VALUE ZERO.               
024500     03  WS-ONORM-OI-GRAENS-PB   PIC S9(9)V9(1) VALUE ZERO COMP-3.        
024600     03  WS-KVOI-SEASON          PIC S9(9)V9(1) VALUE ZERO COMP-3.        
024700     03  WS-KVOI-TOT             PIC S9(11)V9(2)                          
024800                                                VALUE ZERO COMP-3.        
024900     03  WS-NY-KVPB-REF          PIC S9(6)V9(2) VALUE ZERO COMP-3.        
025000     03  NY-KVPB-REF             PIC S9(6)V9(1) VALUE ZERO COMP-3.        
025100     03  WS-A                    PIC S9(6)V9(1) VALUE ZERO COMP-3.        
025200     03  WS-B                    PIC S9(6)V9(1) VALUE ZERO COMP-3.        
025300     03  WS-PREL-KVPB-REF        PIC S9(6)V9(2) VALUE ZERO COMP-3.        
025400     03  WS-MEDEL-KVPB-REF       PIC S9(6)V9(2) VALUE ZERO COMP-3.        
025500     03  WS-ANTAL-FAKTORER-STOERRE-NOLL PIC S9(7)          COMP-3.        
025600     03  WS-KVOTEN               PIC S9(5)V9(2) VALUE ZERO COMP-3.        
025700     03  WS-TIFINLV              PIC S9(5)V     VALUE ZERO COMP-3.        
025800     03  WS-VECKA-I-AKT-PERIOD   PIC  9(2)      VALUE ZERO.               
025900     03  WS-FLREFILL             PIC  X         VALUE SPACE.              
026000     03  WS-KDREFSTA             PIC  X         VALUE SPACE.              
026100     03  WS-PRIS                 PIC S9(7)V9(2) VALUE ZERO COMP-3.        
026200     03  WS-TIPBJUST-1           PIC  9(5)   VALUE ZERO.                  
026300     03  WS-TIPBJUST-2           PIC  9(5)   VALUE ZERO.                  
026400     03  WS-DAPUBL               PIC  9(6)   VALUE ZERO.                  
026500     03  WS-TODAY-DATE-30        PIC  9(6)   VALUE ZERO.                  
026600*                                                                         
026700     03  WS-IDANSK               PIC 9(3)    VALUE ZERO.                  
026800     03  FILLER REDEFINES WS-IDANSK.                                      
026900         07 WS-IDANSK-1-2        PIC 9(2).                                
027000         07 WS-IDANSK3           PIC 9(1).                                
027100                                                                          
027200     EJECT                                                                
027300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
027400 01  FILLER REDEFINES DAGENS-DATUM.                                       
027500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
027600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
027700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
027800     EJECT                                                                
027900* ARBETSFÄLT FÖR ATT KONTROLLERA PB-LSÅNING, ESC-LÅS MM.                  
028000 01  DA-DAGENS-DATUM.                                                     
028100     03  DAGENS-DATUM-20         PIC 9(2) VALUE 20.                       
028200     03  DAGENS-DATUM-6LONG      PIC 9(6).                                
028300 01  WS-DAPUBL-US.                                                        
028400     03  WS-DAPUBL-US-YEAR       PIC 9(4).                                
028500     03  FILLER                  PIC 9(4).                                
028600 01  WS2-TIFINLV                 PIC 9(5).                                
028700 01  FILLER REDEFINES WS2-TIFINLV.                                        
028800     03  WS2-TIFINLV-AAVV        PIC 9(4).                                
028900     03  FILLER                  PIC 9(1).                                
029000 01  WS3-TIFINLV                 PIC 9(6).                                
029100     EJECT                                                                
029200                                                                          
029300* INFO OM KÖRTYP(DAG) FRPN CONSTANTMEDLEM VALD AV JCL'EN                  
029400* INFON KOMMER SOM FIL D1                                                 
029500                                                                          
029600 01  W271TYP-POST.                                                        
029700     03  TYP-PARAMETER        PIC X(4).                                   
029800         88 DAY-KORNING       VALUE 'DAY '.                               
029900         88 WEEK-KORNING      VALUE 'WEEK'.                               
030000         88 ACC-KORNING       VALUE 'ACC '.                               
030100     03  FILLER               PIC X(76).                                  
030200                                                                          
030300 01  DYNAMISKA-SUBPROGRAM.                                                
030400*                                                                         
030500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
030600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
030700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
030800     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
030900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
031000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
031100     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
031200     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
031300     SKIP2                                                                
031400*    --- PARAMETRAR TILL ABEND                                            
031500                                                                          
031600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
031700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
031800     SKIP2                                                                
031900 01  FELTEXT.                                                             
032000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
032100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
032200     EJECT                                                                
032300*    --- PARAMETRAR TILL DATKORT                                          
032400*                                                                         
032500 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W27130'.              
032600     SKIP2                                                                
032700 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
032800     SKIP2                                                                
032900*01  -COPY WDATKORT                                                       
033000                                                                          
033100*    --- PARAMETERS FOR WZ20DAYS SUBPROGRAM                               
033200*01  -COPY WZ20DAYS                                                       
033300*                                                                         
033400     EJECT                                                                
033500*    --- PARAMETRAR TILL POSTSUM                                          
033600*                                                                         
033700*01  -COPY W0005   -PRE  POSTSUM-                                         
033800     EJECT                                                                
033900*    --- PARAMETRAR TILL WDATKONV                                         
034000*                                                                         
034100*01  -COPY WDATAREA                                                       
034200     EJECT                                                                
034300*01    -COPY WWDC99                                                       
034400     EJECT                                                                
034410*01    -COPY WWDC99        -PRE REF-                                      
034420     EJECT                                                                
034500*    --- PARAMETRAR TILL W009VADD                                         
034600*                                                                         
034700 01  W009VADD-AREA.                                                       
034800     03  DATUM-AAVV              PIC S9(5) COMP-3.                        
034900     03  ANTAL                   PIC S9(3) COMP-3.                        
035000*                                                                         
035100     EJECT                                                                
035200*    --- TABELL MED PROGNOSFAKTORER                                       
038000 01  PROGNOSFAKTORER.                                                     
038100******LÄSES IN FRÅN WDB618*************                                   
038200   03 TAB-DC-PROG-PERIOD   OCCURS 200.                                    
038300        05 TAB-PROG-DC-IDDC                PIC X(2)  VALUE SPACE.         
038400        05 TAB-PROG-DC-PERIOD          OCCURS 12.                         
038500           07 TAB-PROG-DC-PERIODTREND.                                    
038600               09 TAB-PROG-DC-NORMAL       PIC 9V9(2).                    
038700               09 TAB-PROG-DC-SVAG-TREND   PIC 9V9(2).                    
038800               09 TAB-PROG-DC-STARK-TREND  PIC 9V9(2).                    
038900                                                                          
039000     EJECT                                                                
039100   03 WS-PROG-DC-PERIOD              OCCURS 12.                           
039200      05 PROG-DC-PERIODTREND.                                             
039300          07 PROG-DC-NORMAL           PIC 9V9(2).                         
039400          07 PROG-DC-SVAG-TREND       PIC 9V9(2).                         
039500          07 PROG-DC-STARK-TREND      PIC 9V9(2).                         
039600      05 FILLER REDEFINES PROG-DC-PERIODTREND.                            
039700          07 PROG-DC-TRENDFAKT       PIC 9V9(2) OCCURS 3.                 
039800     EJECT                                                                
039900                                                                          
040000     03  WS-DC-PROGFAKT-VKA.                                              
040100        07 WS-PROGFAKT-PER    OCCURS 13.                                  
040200           09 WS-DC-PERIODTREND-VKA.                                      
040300               11 WS-DC-NORMAL-VKA       PIC 9V9(4).                      
040400               11 WS-DC-SVAG-TREND-VKA   PIC 9V9(4).                      
040500               11 WS-DC-STARK-TREND-VKA  PIC 9V9(4).                      
040600           09 FILLER REDEFINES WS-DC-PERIODTREND-VKA.                     
040700               11 WS-DC-TRENDFAKT-VKA    PIC 9V9(4) OCCURS 3.             
040800**   03  WS-DC-PROGFAKT-MAX      PIC 9(2)       VALUE 15.                 
040900                                                                          
041000*    --- TABELL MED FAKTORER FÖR BERÄKNING AV ONORMAL OI                  
041100*                                                                         
041200*    TAGIT BORT COPYTEXT W271FSG EFTERSOM ALLA DC                         
041300*    ANVÄNDER SAMMA VÄRDEN                                                
041400*    ANLEDNINGEN ÄR UTÖKANDET AV ANTALET LDC'ER                           
041500*    OKTOBER 2004 / STEFAN Å                                              
041600*                                                                         
041700*                                                                         
041800*                                                                         
041900 01  W271FSG.                                                             
042000*                                 TABELL FAKTORER ONORMAL FÖ              
042100*                                 SÄLJNING. ANVÄNDS FÖR ATT               
042200*                                 GÖRA OM ONORMAL OI I NÅGON              
042300*                                 PERIOD VID PROGNOSBERÄKNIN              
042400*                                 GRÄNSVÄRDET ANVÄNDS I PROG              
042500*                                 BERÄKNINGEN ISTÄLLET FÖR D              
042600*                                 FAKTISKA ORDERINGÅNGEN.                 
042700*                                 (A * PROGNOS) + B                       
042800*                                                                         
042900     03 FSG-DC-PRISKLASSER.                                               
043000        05 FILLER PIC X(15) VALUE '000000050 02512'.                      
043100        05 FILLER PIC X(15) VALUE '000000250 02107'.                      
043200        05 FILLER PIC X(15) VALUE '000001000 01806'.                      
043300        05 FILLER PIC X(15) VALUE '000005000 01604'.                      
043400        05 FILLER PIC X(15) VALUE '000030000 01503'.                      
043500        05 FILLER PIC X(15) VALUE '000100000 01402'.                      
043600        05 FILLER PIC X(15) VALUE '999999999 01201'.                      
043700                                                                          
043800 01  FILLER REDEFINES W271FSG.                                            
043900        05 FSG-DC-FAKT             OCCURS 7.                              
044000           07 FSG-DC-PRARTSTD-MAX  PIC 9(7)V9(2).                         
044100           07 FILLER         PIC X.                                       
044200           07 FSG-A          PIC 9(2)V9.                                  
044300           07 FSG-B          PIC 9(2).                                    
044400     EJECT                                                                
044500 01  UT-AREA-START              PIC X(24)   VALUE                         
044600                                 'UT-AREA-START  '.                       
044700     SKIP2                                                                
044800                                                                          
044900*01  AREA -COPY W27130     -PRE UT-                                       
045000     EJECT                                                                
045100 01  UT-AREA2-START             PIC X(24)   VALUE                         
045200                                 'UT-AREA2-START '.                       
045300     SKIP2                                                                
045400                                                                          
045500*01  AREA -COPY W27138     -PRE UT2-                                      
045600     EJECT                                                                
045700                                                                          
045800 01  UT-AREA3-START             PIC X(24)   VALUE                         
045900                                 'UT-AREA3-START '.                       
046000     SKIP2                                                                
046100                                                                          
046200*01  AREA -COPY W27138     -PRE UT3-                                      
046300     EJECT                                                                
046400*                                                                         
046500*01    -COPY WWBYT03                                                      
046600     EJECT                                                                
046700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
046800                                                                          
046900     SKIP3                                                                
047000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
047100     SKIP3                                                                
047200 01  NYCKLAR-TILL-DLI.                                                    
047300     03  W-IDARTNR-X.                                                     
047400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
047500                                                                          
047600     03  W-IDDC-X.                                                        
047700         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
047800                                                                          
047900     03  W-IDDC-B6-X.                                                     
048000         05  W-IDDC-B6       PIC X(2)   VALUE SPACE.                      
048100                                                                          
048200     03  W-IDDC-B616-X.                                                   
048300         05  W-IDDC-B616     PIC X(2)   VALUE SPACE.                      
048400                                                                          
048500     03  W-IDDC-REF-X.                                                    
048600         05  W-IDDC-REF          PIC X(2)    VALUE SPACE.                 
048700                                                                          
048800     03  W-IDLAND-X.                                                      
048900         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
049000                                                                          
049100     03  W-KDSEGKEY-X.                                                    
049200         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
049300                                                                          
049400     03  W-KDPROGOI-X.                                                    
049500         05  W-KDPROGOI          PIC X       VALUE 'S'.                   
049600*                                                                         
049700     SKIP2                                                                
049800*    --- STATUS-KOD FRÅN IMS                                              
049900 01  STATUS-WS                   PIC XX.                                  
050000     88  SEGMENT-FINNS                       VALUE '  '.                  
050100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
050200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
050300     SKIP2                                                                
050400 01  GODK-STATUSKODER.                                                    
050500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
050600     SKIP3                                                                
050700 01  SSA1                        PIC X(64).                               
050800 01  SSA2                        PIC X(64).                               
050900 01  SSA3                        PIC X(64).                               
051000     EJECT                                                                
051100*    --- IMS FUNKTIONSKODER                                               
051200*01  -COPY W0003                                                          
051300     EJECT                                                                
051400*    ---  DLI INPUT-OUTPUT AREA                                           
051500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
051600 01  DLI-IO-WDK601.                                                       
051700*    03  -COPY WDK601     -PRE WDK6-                                      
051800     EJECT                                                                
051900                                                                          
052000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
052100 01  DLI-IO-WDK611.                                                       
052200*    03  -COPY WDK611                                                     
052300     EJECT                                                                
052400                                                                          
052500                                                                          
052600 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA-K7'.        
052700     SKIP3                                                                
052800 01  DLI-IO-AREA-K7.                                                      
052900     03  IO-AREA-K7              PIC X(300)  VALUE SPACE.                 
053000     SKIP3                                                                
053100     03  WLARTS01 REDEFINES IO-AREA-K7.                                   
053200*        05  -COPY WDK701                                                 
053300     SKIP3                                                                
053400     03  WLARTS11 REDEFINES IO-AREA-K7.                                   
053500*        05  -COPY WDK711                                                 
053600     EJECT                                                                
053700                                                                          
053800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK712'.                      
053900 01  DLI-IO-WDK712.                                                       
054000*    03  -COPY WDK712                                                     
054100     EJECT                                                                
054200                                                                          
054300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK722'.                      
054400 01  DLI-IO-WDK722.                                                       
054500*    03  -COPY WDK722                                                     
054600     EJECT                                                                
054700                                                                          
054800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL711'.                      
054900 01  DLI-IO-WDL711.                                                       
055000*    03  -COPY WDL711                                                     
055100     EJECT                                                                
055200                                                                          
055300 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
055400 01   DLI-IO-AREA-B601.                                                   
055500*     03  -COPY WDB601                                                    
055600     EJECT                                                                
055700                                                                          
055800 01  FILLER               PIC X(16)   VALUE 'WDB616 AREA'.                
055900 01   DLI-IO-AREA-B616.                                                   
056000*     03  -COPY WDB616                                                    
056100     EJECT                                                                
056200                                                                          
056300 01  FILLER               PIC X(16)   VALUE 'WDB618 AREA'.                
056400 01   DLI-IO-AREA-B618.                                                   
056500*     03  -COPY WDB618                                                    
056600     EJECT                                                                
056700                                                                          
056800 LINKAGE SECTION.                                                         
056900                                                                          
057000     EJECT                                                                
057100*01  -COPY W0008  -PRE WDK7-                                              
057200     05  WDK7-KEY-FB-AREA-IDARTNR       PIC S9(9) COMP-3.                 
057300     EJECT                                                                
057400*01  -COPY W0008  -PRE WDK72-                                             
057500     05  FILLER                  PIC X.                                   
057600     EJECT                                                                
057700*01  -COPY W0008  -PRE WDK6-                                              
057800     05  FILLER                  PIC X.                                   
057900     EJECT                                                                
058000*01  -COPY W0008  -PRE WDL7-                                              
058100     05  FILLER                  PIC X.                                   
058200     EJECT                                                                
058300*01  -COPY W0008      -PRE WDB6-                                          
058400     05  FILLER                  PIC X.                                   
058500     EJECT                                                                
058600 PROCEDURE DIVISION  USING WDK7-PCB WDK72-PCB WDK6-PCB WDL7-PCB           
058700                           WDB6-PCB.                                      
058800     ENTRY 'DLITCBL' USING WDK7-PCB WDK72-PCB WDK6-PCB WDL7-PCB           
058900                           WDB6-PCB.                                      
059000     PERFORM A-INIT                                                       
059100     PERFORM IMS-GN-WDK7                                                  
059200     PERFORM UNTIL SEGMENT-SLUT                                           
059300       EVALUATE WDK7-SEG-NAME-FB                                          
059400         WHEN 'WDK701  '                                                  
059500           MOVE WDK7-KEY-FB-AREA-IDARTNR TO W-IDARTNR                     
059600                                            BYT03-IDARTNR                 
059700         WHEN 'WDK711  '                                                  
059800                                                                          
059900            MOVE SLAG-IDDC             TO W-IDDC                          
060000                                          W-IDDC-B6                       
060100                                          WS-IDDC                         
060200            MOVE SLAG-IDDC-REF         TO W-IDDC-B616                     
060210                                          REF-WS-IDDC                     
060300            PERFORM IMS-GU-WDB601                                         
060400            IF (DCS-NDC-CN AND SLAG-IDDC-REF = SPACE)                     
060500            OR (DCS-USA AND SLAG-IDDC-REF = SPACE)                        
060601            OR ((DCS-FLLPO = 'Y') AND (SLAG-IDDC-REF = SPACE))            
060701************IF PURCHASING DC WE USE CDC AS SENDING DC FOR                 
060801************BATCH PARAMETER                                               
060901************ALSO FOR LOCAL PURCHASED PARTS                                
061001              MOVE '11'     TO W-IDDC-B616                                
061101            END-IF                                                        
061201                                                                          
061301            PERFORM IMS-GNP-WDB616                                        
061401            IF SEGMENT-FINNS                                              
061501              IF DCS-FLSTOFC = JA                                         
061601********   STOP FOR WAREHOUSES***                                         
061701                CONTINUE                                                  
061801              ELSE                                                        
061901***********W271V2     WEEKLY                                              
062001               IF (DC-PARAMETER-TIME = '99'                               
062101***********W271D*     DAILY                                               
062201               OR (DC-PARAMETER-TIME = REF-TIREFBAT))                     
062301                   PERFORM STYR                                           
066100               END-IF                                                     
066200              END-IF                                                      
066300            END-IF                                                        
066400                                                                          
066500       END-EVALUATE                                                       
066600       PERFORM IMS-GN-WDK7                                                
066700     END-PERFORM                                                          
066800                                                                          
066900     PERFORM Z-FINIT                                                      
067000                                                                          
067100     MOVE ZERO TO RETURN-CODE                                             
067200     GOBACK                                                               
067300     .                                                                    
067400     EJECT                                                                
067500                                                                          
067600                                                                          
067700 STYR SECTION.                                                            
067800                                                                          
067900     IF ACC-KORNING                                                       
068000     OR WEEK-KORNING                                                      
068100        PERFORM IMS-GU-WDK601                                             
068200        MOVE NEJ         TO PB-JUST-SW                                    
068300        MOVE 'N'         TO UT-NOLLA-JUST1                                
068400                            UT-NOLLA-JUST2                                
068500        MOVE SLAG-KDREFSTA TO WS-KDREFSTA                                 
068600        IF (DCS-NDC-CN AND SLAG-IDDC-REF = SPACE)                         
068700        OR (DCS-USA AND SLAG-IDDC-REF = SPACE)                            
068800**FLYTTAR A TILL WS-KDREFSTA FÖR ATT PB SKALL ALLTID RÄKNAS               
068900**OM FÖR LOKALT ANSKAFFADE ARTIKLAR                                       
069000          MOVE 'A'         TO WS-KDREFSTA                                 
069100          PERFORM IMS-GU-WDK722                                           
069200          IF SEGMENT-FINNS                                                
069300            MOVE XLAG-TIPBJUST-1   TO WS-TIPBJUST-1                       
069400            MOVE XLAG-TIPBJUST-2   TO WS-TIPBJUST-2                       
069500***MINSKA EN VECKA SÅ ATT KVPB-JUST UPPDATERAS I BATCHKÖRNINGEN           
069600***PÅ HELGEN FÖRE VECKAN SOM PB-JUST SKALL BÖRJA GÄLLA                    
069700            IF WS-TIPBJUST-1 > 0                                          
069800              MOVE WS-TIPBJUST-1   TO DATUM-AAVV                          
069900              MOVE -1              TO ANTAL                               
070000              CALL W009VADD USING DATUM-AAVV ANTAL                        
070100              MOVE DATUM-AAVV TO WS-TIPBJUST-1                            
070200            END-IF                                                        
070300            IF WS-TIPBJUST-2 > 0                                          
070400              MOVE WS-TIPBJUST-2   TO DATUM-AAVV                          
070500              MOVE -1              TO ANTAL                               
070600              CALL W009VADD USING DATUM-AAVV ANTAL                        
070700              MOVE DATUM-AAVV TO WS-TIPBJUST-2                            
070800            END-IF                                                        
070900            IF WS-TIPBJUST-1 = DAGENS-TIAAVV-GRP                          
071000            OR WS-TIPBJUST-2 = DAGENS-TIAAVV-GRP                          
071100              IF WS-TIPBJUST-1 = DAGENS-TIAAVV-GRP                        
071200                MOVE XLAG-KVPB-JUST1 TO UT-KVPB-REF                       
071300                PERFORM S05-DELETE-JUST                                   
071400              END-IF                                                      
071500              IF WS-TIPBJUST-2 = DAGENS-TIAAVV-GRP                        
071600                MOVE XLAG-KVPB-JUST2 TO UT-KVPB-REF                       
071700                PERFORM S05-DELETE-JUST                                   
071800              END-IF                                                      
071900              MOVE W-IDARTNR           TO UT-IDARTNR                      
072000              MOVE SLAG-IDDC           TO UT-IDDC                         
072100              MOVE +1                  TO UT-RETREND                      
072200***SÄTTER NOLL TILL KDERS FÖR ATT ALLTID UPPDATERA PB-JUST I              
072300***W2713100                                                               
072400              MOVE ZERO                TO UT-KDERS                        
072500              MOVE DAGENS-DATUM        TO UT-TIREFMPB                     
072600              PERFORM S12-SKRIV-W27130                                    
072700              MOVE JA                  TO PB-JUST-SW                      
072800            END-IF                                                        
072900          END-IF                                                          
073000        END-IF                                                            
073100        IF WDK6-ART-KDERS-UTG = 0                                         
073200        AND PB-JUST-SW = 'N'                                              
073300           PERFORM B-KOLLA-PUBVECKA-FLREFILL                              
073400           IF (CLAG-KDERS < 20)                                           
073500           OR BEHANDLA                                                    
073600             MOVE WDK6-ART-KDPRODSL                                       
073700                                 TO TEST-KDPRODSL                         
073800             IF WS-FLREFILL = JA                                          
073900*                                                                         
074000*   NEDANSTÅENDE GÄLLER RENAULARTIKLAR FÖR LDC 1A                         
074100*                                                                         
074200             OR  (DCS-FLEXCP1-REFBER = JA                                 
074300             AND KDPRODSL-BIMA                                            
074400             AND NOT (BYT03-OBJEKT                                        
074500             OR       WDK6-ART-KDSORT = 'SW'                              
074600             OR       CLAG-FLLSRDEL = NEJ))                               
074700                IF  WS-KDREFSTA = AKTIV                                   
074800                AND SLAG-RESEASON (DAGENS-TIRP) > 0.5                     
074900                   PERFORM C-BEHANDLA-ARTIKEL                             
075000                 END-IF                                                   
075100             END-IF                                                       
075200           ELSE                                                           
075300             IF ACC-KORNING                                               
075400               IF (SLAG-KVLS + SLAG-KVAKS-PAV) > ZERO                     
075500                 CONTINUE                                                 
075600               ELSE                                                       
075700                 PERFORM D-NOLLA-PB-MFL                                   
075800               END-IF                                                     
075900             END-IF                                                       
076000           END-IF                                                         
077000        END-IF                                                            
079000     END-IF                                                               
079100     .                                                                    
079200     EJECT                                                                
079300                                                                          
079400                                                                          
079500 A-INIT SECTION.                                                          
079600                                                                          
079700     OPEN INPUT  W271TYP                                                  
079800                 W271DC                                                   
079900     OPEN OUTPUT W27130                                                   
080000                 W27138                                                   
080100                 W2713F                                                   
080200                                                                          
080300     PERFORM S21-LAES-W271DC                                              
080800     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
080900     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
081000     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
081100     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
081200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
081300                                                                          
081400     MOVE 'AAMMDD'     TO DAT-KDDATFORM                                   
081500     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
081600     MOVE DAGENS-DATUM TO DAGENS-DATUM-6LONG                              
081700                                                                          
081800     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
081900                         DAT-O-TIDATUM DAT-KDSVAR                         
082000                                                                          
082100     IF DAT-KDSVAR-OK                                                     
082200       MOVE DAT-TIAARP     TO DAGENS-TIAARP                               
082300                              FOREG-TIAARP                                
082400                              NAESTA-TIAARP                               
082500       MOVE DAT-TIVV       TO DAGENS-TIVV                                 
082600       MOVE DAT-TIAAVV-GRP TO DAGENS-TIAAVV-GRP                           
082700       MOVE DAT-TIAAVVD    TO DAGENS-TIAAVVD                              
082800     ELSE                                                                 
082900       MOVE 'SVAR 1 FRÅN WDATKONV I A SECTION EJ OK'                      
083000                         TO FELTEXT-STR                                   
083100       DISPLAY FELTEXT                                                    
083200       PERFORM S99-ABEND                                                  
083300     END-IF                                                               
083400                                                                          
083500* TODAYS DATE MINUS 30 DAYS                                               
083600     MOVE  'YYMMDD'            TO DAYS-KDDATFMT1                          
083700     MOVE  'YYMMDD'            TO DAYS-KDDATFMT2                          
083800     MOVE  DAGENS-DATUM        TO DAYS-TIDATE1                            
083900     MOVE  -30                 TO DAYS-KVDAYS                             
084000     MOVE  SPACE               TO DAYS-TIDATE2                            
084100                                  DAYS-IDCALEND                           
084200     CALL  WZ20DAYS USING DAYS-WZ20DAYS                                   
084300                                                                          
084400     MOVE  DAYS-TIDATE2(1:6)   TO WS-TODAY-DATE-30                        
084500                                                                          
084600*    DAGENS DATUM ETT ÅR TILLBAKA                                         
084700                                                                          
084800     MOVE DAGENS-TIAAVVD   TO DAGENS-TIAAVVD-LAST-YEAR                    
084900     IF DAGENS-TIAAVVD-LAST-YEAR-AA = 00                                  
085000       MOVE 99      TO DAGENS-TIAAVVD-LAST-YEAR-AA                        
085100     ELSE                                                                 
085200       SUBTRACT +1  FROM DAGENS-TIAAVVD-LAST-YEAR-AA                      
085300     END-IF                                                               
085400                                                                          
085500     PERFORM AZ-VECKO-ELLER-PERIOD-KORNING                                
085600                                                                          
085700     IF ACC-KORNING                                                       
085800       MOVE 12               TO MAX-TAB-RADIX                             
085900     ELSE                                                                 
086000       MOVE 13               TO MAX-TAB-RADIX                             
086100     END-IF                                                               
086200                                                                          
086300     PERFORM AA-INITERA-PERIODTABELL                                      
086400                                                                          
086500*    IF ACC-KORNING                                                       
086600*      CONTINUE                                                           
086700*    ELSE                                                                 
086800       PERFORM AB-PROGNOS-TABELL                                          
086900*    END-IF                                                               
087000     .                                                                    
087100     EJECT                                                                
087200 AA-INITERA-PERIODTABELL SECTION.                                         
087300*--------------------------------------------------------------*          
087400* HÄR INITIERAS PERIODTABELLEN. INDX 1 MOTSVARAR INNEVARANDE   *          
087500* PERIOD, INDX 2 PERIODEN INNAN OSV.                           *          
087600* PERIODERNA ÄR REDOVISNINGSPERIODER. TABELLEN INITIERAS MED   *          
087700* PER.NR + START/SLUTVECKA FÖR ATT KUNNA UTFÖRA SUMMERINGAR    *          
087800*--------------------------------------------------------------*          
087900                                                                          
088000     MOVE 1 TO TAB-RADIX                                                  
088100     PERFORM AAA-TA-HAND-OM-INNEV-PERIOD                                  
088200     ADD +1 TO TAB-RADIX                                                  
088300     PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                              
088400*---TA REDA PÅ FÖREGÅENDE PERIOD                                          
088500       COMPUTE FOREG-TIRP = FOREG-TIRP - 1                                
088600       IF FOREG-TIRP = +0                                                 
088700         IF FOREG-TIAA = 00                                               
088800           MOVE 99      TO   FOREG-TIAA                                   
088900         ELSE                                                             
089000           SUBTRACT  +1 FROM FOREG-TIAA                                   
089100         END-IF                                                           
089200         MOVE +12     TO   FOREG-TIRP                                     
089300       END-IF                                                             
089400       MOVE FOREG-TIAARP TO TABELL-TIAARP(TAB-RADIX)                      
089500                                                                          
089600*---BEHANDLAD PERIODS START-VECKA - 1 = FÖREGÅENDE PER. SLUT-VECKA        
089700      IF START-VV > +1                                                    
089800        COMPUTE SLUT-VV = START-VV - +1                                   
089900        MOVE SLUT-VV TO TABELL-SISTA-TIAAVV(TAB-RADIX)                    
090000      ELSE                                                                
090100        MOVE FOREG-TIAA      TO WS-TIAA                                   
090200        PERFORM AAB-KOLLA-ANTAL-VECKOR                                    
090300        MOVE SLUT-VV TO TABELL-SISTA-TIAAVV(TAB-RADIX)                    
090400      END-IF                                                              
090500                                                                          
090600*--- TA REDA PÅ START-VECKA                                               
090700                                                                          
090800       MOVE FOREG-TIAARP TO DAT-I-TIDATUM                                 
090900       MOVE 'AARP'       TO DAT-KDDATFORM                                 
091000                                                                          
091100       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
091200                           DAT-O-TIDATUM DAT-KDSVAR                       
091300                                                                          
091400       IF DAT-KDSVAR-OK                                                   
091500*--- START-VECKA ÄR ALLTID VECKA 1 PÅ NYTT ÅR                             
091600         IF DAT-TIVV = +52 OR +53                                         
091700           MOVE +1 TO START-VV                                            
091800                      TABELL-FORSTA-TIAAVV(TAB-RADIX)                     
091900         ELSE                                                             
092000           MOVE DAT-TIVV   TO START-VV                                    
092100                              TABELL-FORSTA-TIAAVV(TAB-RADIX)             
092200         END-IF                                                           
092300       ELSE                                                               
092400         MOVE 'SVAR 2 FRÅN WDATKONV EJ OK' TO FELTEXT-STR                 
092500         DISPLAY FELTEXT                                                  
092600         PERFORM S99-ABEND                                                
092700       END-IF                                                             
092800       MOVE DAT-TIAARP TO TABELL-TIAARP(TAB-RADIX)                        
092900       MOVE ZERO       TO TABELL-KVOI(TAB-RADIX)                          
093000       ADD +1 TO TAB-RADIX                                                
093100     END-PERFORM                                                          
093200     .                                                                    
093300     EJECT                                                                
093400 AAA-TA-HAND-OM-INNEV-PERIOD SECTION.                                     
093500                                                                          
093600*--- KOLLA START-VECKA                                                    
093700                                                                          
093800     MOVE 'AARP'        TO DAT-KDDATFORM                                  
093900     MOVE DAGENS-TIAARP TO DAT-I-TIDATUM                                  
094000                                                                          
094100     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
094200                         DAT-O-TIDATUM DAT-KDSVAR                         
094300                                                                          
094400     IF DAT-KDSVAR-OK                                                     
094500*--- START-VECKA ÄR ALLTID VECKA 1 PÅ NYTT ÅR                             
094600       IF DAT-TIVV = +52 OR +53                                           
094700         MOVE +1 TO START-VV                                              
094800                                                                          
094900         IF DAGENS-TIVV = 52 OR 53                                        
095000           MOVE 1            TO WS-VECKA-I-AKT-PERIOD                     
095100         END-IF                                                           
095200         IF DAGENS-TIVV = 1                                               
095300           MOVE 2            TO WS-VECKA-I-AKT-PERIOD                     
095400         END-IF                                                           
095500         IF DAGENS-TIVV = 2                                               
095600           MOVE 3            TO WS-VECKA-I-AKT-PERIOD                     
095700         END-IF                                                           
095800         IF DAGENS-TIVV = 3                                               
095900           MOVE 4            TO WS-VECKA-I-AKT-PERIOD                     
096000         END-IF                                                           
096100         IF DAGENS-TIVV = 4                                               
096200           MOVE 5            TO WS-VECKA-I-AKT-PERIOD                     
096300         END-IF                                                           
096400       ELSE                                                               
096500         MOVE DAT-TIVV   TO START-VV                                      
096600                                                                          
096700         COMPUTE WS-VECKA-I-AKT-PERIOD =                                  
096800            DAGENS-TIVV - START-VV + 1                                    
096900       END-IF                                                             
097000     ELSE                                                                 
097100       MOVE 'SVAR 3 FRÅN WDATKONV EJ OK' TO FELTEXT-STR                   
097200       DISPLAY FELTEXT                                                    
097300       PERFORM S99-ABEND                                                  
097400     END-IF                                                               
097500                                                                          
097600*--- KOLLA SLUT-VECKA, NÄSTA PERIODS START-VECKA - 1                      
097700                                                                          
097800     COMPUTE NAESTA-TIRP = DAGENS-TIRP + 1                                
097900     IF NAESTA-TIRP = +13                                                 
098000       ADD  +1 TO NAESTA-TIAA                                             
098100       MOVE +1 TO NAESTA-TIRP                                             
098200     END-IF                                                               
098300                                                                          
098400     MOVE 'AARP'        TO DAT-KDDATFORM                                  
098500     MOVE NAESTA-TIAARP TO DAT-I-TIDATUM                                  
098600                                                                          
098700     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
098800                         DAT-O-TIDATUM DAT-KDSVAR                         
098900                                                                          
099000     IF DAT-KDSVAR-OK                                                     
099100       MOVE DAT-TIVV   TO SLUT-VV                                         
099200     ELSE                                                                 
099300       MOVE 'SVAR 4 FRÅN WDATKONV EJ OK' TO FELTEXT-STR                   
099400       DISPLAY FELTEXT                                                    
099500       PERFORM S99-ABEND                                                  
099600     END-IF                                                               
099700                                                                          
099800     IF SLUT-VV = +1                                                      
099900       MOVE DAGENS-TIAA  TO WS-TIAA                                       
100000       PERFORM AAB-KOLLA-ANTAL-VECKOR                                     
100100     ELSE                                                                 
100200       COMPUTE SLUT-VV = SLUT-VV - +1                                     
100300     END-IF                                                               
100400                                                                          
100500     MOVE DAGENS-TIAARP TO TABELL-TIAARP(TAB-RADIX)                       
100600     MOVE START-VV      TO TABELL-FORSTA-TIAAVV(TAB-RADIX)                
100700     MOVE SLUT-VV       TO TABELL-SISTA-TIAAVV(TAB-RADIX)                 
100800     MOVE ZERO          TO TABELL-KVOI(TAB-RADIX)                         
100900                                                                          
101000     .                                                                    
101100     EJECT                                                                
101200 AAB-KOLLA-ANTAL-VECKOR SECTION.                                          
101300                                                                          
101400* --- TAG REDA PÅ OM DET ÄR 52 ELLER 53 VECKOR PÅ ÅRET                    
101500                                                                          
101600     MOVE 53        TO WS-TIVV                                            
101700     MOVE WS-TIAAVV TO DAT-I-TIDATUM                                      
101800     MOVE 'AAVV  '  TO DAT-KDDATFORM                                      
101900     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
102000                         DAT-O-TIDATUM DAT-KDSVAR                         
102100     IF DAT-KDSVAR-OK                                                     
102200       MOVE 53 TO SLUT-VV                                                 
102300     ELSE                                                                 
102400       MOVE 52 TO SLUT-VV                                                 
102500     END-IF                                                               
102600     .                                                                    
102700     EJECT                                                                
102800                                                                          
102900                                                                          
103000 AB-PROGNOS-TABELL SECTION.                                               
103100                                                                          
103200     MOVE DAT-KVVIPER    TO WS-KVVIPER                                    
103300                                                                          
103400     MOVE +1             TO IX-DC                                         
103500     PERFORM IMS-GN-WDB601                                                
103600     PERFORM UNTIL SEGMENT-SLUT                                           
103700       PERFORM IMS-GNP-WDB618                                             
103800       IF SEGMENT-FINNS                                                   
103900         MOVE DCS-IDDC    TO TAB-PROG-DC-IDDC (IX-DC)                     
104000         MOVE +1    TO IX-TAB                                             
104100         PERFORM UNTIL IX-TAB > 12                                        
104200           MOVE PROG-REFTREND-NORM (IX-TAB)                               
104300                           TO TAB-PROG-DC-NORMAL (IX-DC, IX-TAB)          
104400           MOVE PROG-REFTREND-SVAG (IX-TAB)                               
104500                        TO TAB-PROG-DC-SVAG-TREND (IX-DC, IX-TAB)         
104600           MOVE PROG-REFTREND-STARK (IX-TAB)                              
104700                        TO TAB-PROG-DC-STARK-TREND (IX-DC, IX-TAB)        
104800           ADD +1   TO IX-TAB                                             
104900         END-PERFORM                                                      
105000         ADD +1         TO IX-DC                                          
105100       END-IF                                                             
105200       PERFORM IMS-GN-WDB601                                              
105300     END-PERFORM                                                          
105400     .                                                                    
105500     EJECT                                                                
105600                                                                          
105700                                                                          
105800 AZ-VECKO-ELLER-PERIOD-KORNING SECTION.                                   
105900                                                                          
106000     PERFORM S11-LAES-W271TYP                                             
106100                                                                          
106200     IF DAY-KORNING                                                       
106300        DISPLAY 'DAGKÖRNING'                                              
106400     ELSE                                                                 
106500        IF WEEK-KORNING                                                   
106600           DISPLAY 'VECKOKÖRNING, EJ PERIODSLUT'                          
106700        ELSE                                                              
106800           IF ACC-KORNING                                                 
106900              DISPLAY 'ACC-PERIODKÖRNING'                                 
107000           END-IF                                                         
107100        END-IF                                                            
107200     END-IF                                                               
107300     .                                                                    
107400     EJECT                                                                
107500                                                                          
107600 B-KOLLA-PUBVECKA-FLREFILL   SECTION.                                     
107700                                                                          
107800     MOVE NEJ               TO BEHANDLA-SW                                
107900     IF SLAG-IDDC-REF = '11'                                              
108000       MOVE WDK6-ART-TIFINLV TO WS-TIFINLV                                
108100     ELSE                                                                 
108200       MOVE DCS-IDLANDX2                TO W-IDLAND                       
108300       PERFORM IMS-GU-WDK712                                              
108400       IF SEGMENT-FINNS                                                   
108500         IF LART-TIERSDAT-VIPS > 0                                        
108600           IF DCS-CHINA                                                   
108700           OR DCS-USA                                                     
108800             MOVE JA TO BEHANDLA-SW                                       
108900           END-IF                                                         
109000         END-IF                                                           
109100         IF LART-DAPUBL = ZERO                                            
109200           MOVE WDK6-ART-TIFINLV TO WS-TIFINLV                            
109300         ELSE                                                             
109400           MOVE LART-DAPUBL (3:6) TO WS-DAPUBL                            
109500           MOVE 'AAMMDD' TO DAT-KDDATFORM                                 
109600           MOVE WS-DAPUBL        TO DAT-I-TIDATUM                         
109700                                                                          
109800           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
109900                               DAT-O-TIDATUM DAT-KDSVAR                   
110000                                                                          
110100           IF DAT-KDSVAR-OK                                               
110200             MOVE DAT-TIAAVVD TO WS-TIFINLV                               
110300           ELSE                                                           
110400             MOVE 'SVAR FRÅN WDATKONV I STYR SECTION EJ OK'               
110500                               TO FELTEXT-STR                             
110600             DISPLAY FELTEXT                                              
110700             PERFORM S99-ABEND                                            
110800           END-IF                                                         
110900         END-IF                                                           
111000         MOVE LART-PRMATRL           TO WS-PRIS                           
111100       ELSE                                                               
111200         MOVE WDK6-ART-TIFINLV TO WS-TIFINLV                              
111300       END-IF                                                             
111400     END-IF                                                               
111500     PERFORM IMS-GNP-WDK611                                               
111600*                                                                         
111700*                                                                         
112410     IF DCS-NDC                                                           
112420       IF REF-NDC                                                         
112500         MOVE SLAG-IDDC-REF TO W-IDDC                                     
112600         MOVE SLAG-FLREFILL TO WS-FLREFILL                                
112700       ELSE                                                               
112800         IF (SLAG-IDDC-REF = SPACE)                                       
112900         AND NOT DCS-CANADA                                               
113000           MOVE JA              TO WS-FLREFILL                            
113100         ELSE                                                             
113200           MOVE CLAG-FLREFILL TO WS-FLREFILL                              
113300         END-IF                                                           
113400       END-IF                                                             
113500     ELSE                                                                 
113600       MOVE CLAG-FLREFILL TO WS-FLREFILL                                  
113700     END-IF                                                               
113800***FLYTTA TILLBAKA BEHANDLADE DC TILL NYCKEL                              
113900     MOVE WS-IDDC     TO W-IDDC                                           
114000     .                                                                    
114100     EJECT                                                                
114200                                                                          
114300 C-BEHANDLA-ARTIKEL SECTION.                                              
114400                                                                          
114500     MOVE NEJ TO ARTIKEL-SW                                               
114600                                                                          
114700     PERFORM IMS-GU-WDL711                                                
114800     IF SEGMENT-FINNS                                                     
114900        IF SLAG-FLREFILL = JA                                             
115000           PERFORM CA-UPPDATERA-PERIODTABELL                              
115100                                                                          
115200           PERFORM CB-UTFOR-BERAKNINGAR                                   
115300                                                                          
115400           IF ARTIKEL-SKALL-FORAENDRAS                                    
115500                                                                          
115600             IF (NY-KVPB-REF > SLAG-KVPB-REF                              
115700                                     + WS-ONORM-OI-GRAENS-PB              
115800             OR  NY-KVPB-REF < SLAG-KVPB-REF                              
115900                                     - WS-ONORM-OI-GRAENS-PB)             
116000                                                                          
116100*-----                                                                    
116200*----- SKRIV POST PÅ LARMFIL                                              
116300*----- ONORMAL FÖRÄNDRING, KRÄVER MANUELL BEHANDLING                      
116400*-----                                                                    
116500               MOVE W-IDARTNR            TO UT2-IDARTNR                   
116600               MOVE SLAG-IDDC            TO UT2-IDDC                      
116700               MOVE SLAG-IDDC-REF        TO UT2-IDDC-REF                  
116800               MOVE SLAG-KVPB-REF        TO UT2-KVPB-REF                  
116900               MOVE NY-KVPB-REF          TO UT2-NY-KVPB-REF               
117000               MOVE SPACE                TO UT2-BEART                     
117100               PERFORM IMS-GU-WDK722                                      
117200               IF  SEGMENT-FINNS                                          
117300               AND XLAG-IDANSK            > ZERO                          
117400                   MOVE XLAG-IDANSK      TO WS-IDANSK                     
117500                   MOVE ZERO             TO WS-IDANSK3                    
117600                   MOVE WS-IDANSK        TO UT2-IDANSK                    
117700               ELSE                                                       
117800                   MOVE ZERO             TO UT2-IDANSK                    
117900               END-IF                                                     
118000               MOVE SLAG-IDPERSON-BUY    TO UT2-IDPERSON-BUY              
118100                                                                          
118200               MOVE WDK6-ART-KDPRODSL    TO TEST-KDPRODSL                 
118300                                                                          
118400               IF (CLAG-REDIRLEV = 1.0                                    
118500                   AND (DCS-SDC AND NOT SDC-NL)) OR                       
118600                  (KDPRODSL-BYTES AND SLAG-IDDC = '91')                   
118700                  CONTINUE                                                
118800               ELSE                                                       
118900                  PERFORM S13-SKRIV-W27138                                
119000               END-IF                                                     
119100                                                                          
119200             ELSE                                                         
119300*-----                                                                    
119400*-----WRITE ALARM FILE FOR SLOW MOVING PARTS                              
119500*-----WITH HIGHER NEW FC                                                  
119600*-----                                                                    
119700               IF   NY-KVPB-REF     > SLAG-KVPB-REF                       
119800               AND (SLAG-KVPB-REF  <= 0.6                                 
119900               AND (DC-TIREFEFT     < SLAG-TIORDREG                       
120000               OR   SLAG-FLREFNYO   = JA))                                
120100                    MOVE W-IDARTNR       TO UT3-IDARTNR                   
120200                    MOVE SLAG-IDDC       TO UT3-IDDC                      
120300                    MOVE SLAG-IDDC-REF   TO UT3-IDDC-REF                  
120400                    MOVE SLAG-KVPB-REF   TO UT3-KVPB-REF                  
120500                    MOVE NY-KVPB-REF     TO UT3-NY-KVPB-REF               
120600                    MOVE SPACE           TO UT3-BEART                     
120700                    PERFORM IMS-GU-WDK722                                 
120800                    IF  SEGMENT-FINNS                                     
120900                    AND XLAG-IDANSK       > ZERO                          
121000                        MOVE XLAG-IDANSK TO WS-IDANSK                     
121100                        MOVE ZERO        TO WS-IDANSK3                    
121200                        MOVE WS-IDANSK   TO UT3-IDANSK                    
121300                    ELSE                                                  
121400                        MOVE ZERO        TO UT3-IDANSK                    
121500                    END-IF                                                
121600                    MOVE SLAG-IDPERSON-BUY                                
121700                                         TO UT3-IDPERSON-BUY              
121800                    MOVE WDK6-ART-KDPRODSL                                
121900                                         TO TEST-KDPRODSL                 
122000                    IF  (CLAG-REDIRLEV = 1.0                              
122100                    AND (DCS-SDC AND NOT SDC-NL)) OR                      
122200                        (KDPRODSL-BYTES AND SLAG-IDDC = '91')             
122300                        CONTINUE                                          
122400                    ELSE                                                  
122500                        PERFORM S14-SKRIV-W2713F                          
122600                    END-IF                                                
122700               ELSE                                                       
122800*----- SKRIV POST PÅ ARTIKELFIL                                           
122900                  MOVE W-IDARTNR          TO UT-IDARTNR                   
123000                  MOVE SLAG-IDDC          TO UT-IDDC                      
123100                  MOVE NY-KVPB-REF        TO UT-KVPB-REF                  
123200                  MOVE WS-KVOTEN          TO UT-RETREND                   
123300                  MOVE CLAG-KDERS         TO UT-KDERS                     
123400                  MOVE ZERO               TO UT-TIREFMPB                  
123500                  PERFORM S12-SKRIV-W27130                                
123600               END-IF                                                     
123700             END-IF                                                       
123800           END-IF                                                         
123900        END-IF                                                            
124000     END-IF                                                               
124100     .                                                                    
124200     EJECT                                                                
124300 CA-UPPDATERA-PERIODTABELL SECTION.                                       
124400                                                                          
124500     PERFORM CAA-NOLLSTAELL-FAELT                                         
124600     PERFORM CAB-SUMMERA-PERIODTABELL                                     
124700     PERFORM CAC-JUSTERA-OI                                               
124800     PERFORM CAD-SASONGSANPASSA                                           
124900     .                                                                    
125000     EJECT                                                                
125100 CAA-NOLLSTAELL-FAELT SECTION.                                            
125200                                                                          
125300*---  NOLLSTÄLL KVOI I PERIODTABELLEN                                     
125400                                                                          
125500     MOVE 1 TO TAB-RADIX                                                  
125600     PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                              
125700       MOVE ZERO TO TABELL-KVOI (TAB-RADIX)                               
125800       ADD 1 TO TAB-RADIX                                                 
125900     END-PERFORM                                                          
126000                                                                          
126100*---  NOLLSTÄLL ARBETSFÄLT                                                
126200     MOVE ZERO TO WS-TIREFMPB-TIAARP                                      
126300                  WS-TEST-TIREFMPB                                        
126400                  WS-ONORM-OI-GRAENS-PB                                   
126500                  WS-KVOI-TOT                                             
126600                  WS-NY-KVPB-REF                                          
126700                  NY-KVPB-REF                                             
126800                  WS-PREL-KVPB-REF                                        
126900                  WS-MEDEL-KVPB-REF                                       
127000                  WS-ANTAL-FAKTORER-STOERRE-NOLL                          
127100                  WS-KVOTEN                                               
127200                                                                          
127300     .                                                                    
127400     EJECT                                                                
127500 CAB-SUMMERA-PERIODTABELL SECTION.                                        
127600*---------------------------------------------------------------*         
127700* HÄR SUMMERAS DE VECKOR IN I RÄTT PERIOD                       *         
127800* EV. NEGATIV OI ÄNDRAS TILL NOLL.                              *         
127900*---------------------------------------------------------------*         
128000                                                                          
128100*--- SUMMERA FÖRST VECKOR FRÅN INNEV. PERIOD                              
128200                                                                          
128300     MOVE +5   TO INDX                                                    
128400     MOVE +1   TO TAB-RADIX                                               
128500     PERFORM UNTIL INDX = 0                                               
128600       IF DC-TIVV(INDX) > 0 AND <= DAGENS-TIVV                            
128700         ADD DC-KVOI-INNEV(INDX)    TO TABELL-KVOI(TAB-RADIX)             
128800         ADD DC-KVOI-PP-INNEV(INDX) TO TABELL-KVOI(TAB-RADIX)             
128900       END-IF                                                             
129000       SUBTRACT 1 FROM INDX                                               
129100     END-PERFORM                                                          
129200                                                                          
129300*--- SUMMERA IN RESTERANDE PERIODER                                       
129400                                                                          
129500     ADD +1 TO TAB-RADIX                                                  
129600     PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                              
129700       MOVE TABELL-SISTA-TIAAVV(TAB-RADIX) TO VECKO-INDX                  
129800       PERFORM UNTIL TABELL-FORSTA-TIAAVV(TAB-RADIX) = VECKO-INDX         
129900         ADD DC-KVOI-RULL(VECKO-INDX) TO                                  
130000                                          TABELL-KVOI(TAB-RADIX)          
130100         IF VECKO-INDX > +1                                               
130200           SUBTRACT +1 FROM VECKO-INDX                                    
130300         ELSE                                                             
130400                                                                          
130500*--- KOLLA OM 52 ELLER 53 VECKOR PÅ ÅRET                                  
130600                                                                          
130700           MOVE TABELL-TIAARP(TAB-RADIX) TO FOREG-TIAARP                  
130800           IF FOREG-TIAA = 00                                             
130900             MOVE 99 TO FOREG-TIAA                                        
131000           ELSE                                                           
131100             SUBTRACT +1 FROM  FOREG-TIAA                                 
131200           END-IF                                                         
131300           MOVE FOREG-TIAA   TO WS-TIAA                                   
131400           MOVE 53        TO WS-TIVV                                      
131500           MOVE WS-TIAAVV TO DAT-I-TIDATUM                                
131600           MOVE 'AAVV  '  TO DAT-KDDATFORM                                
131700           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
131800                               DAT-O-TIDATUM DAT-KDSVAR                   
131900           IF DAT-KDSVAR-OK                                               
132000             MOVE 53 TO VECKO-INDX                                        
132100           ELSE                                                           
132200             MOVE 52 TO VECKO-INDX                                        
132300           END-IF                                                         
132400         END-IF                                                           
132500       END-PERFORM                                                        
132600                                                                          
132700*--- ADDERA DEN SISTA VECKAN OCKSÅ                                        
132800                                                                          
132900       ADD DC-KVOI-RULL(VECKO-INDX) TO                                    
133000                                    TABELL-KVOI(TAB-RADIX)                
133100       ADD +1 TO TAB-RADIX                                                
133200     END-PERFORM                                                          
133300                                                                          
133400*--- KOLLA OM EV. NEGATIV OI I TABELLEN - ÄNDRA TILL 0                    
133500                                                                          
133600     MOVE +1 TO TAB-RADIX                                                 
133700     PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                              
133800       IF TABELL-KVOI(TAB-RADIX) < 0                                      
133900         MOVE ZERO TO TABELL-KVOI(TAB-RADIX)                              
134000       END-IF                                                             
134100       ADD +1 TO TAB-RADIX                                                
134200     END-PERFORM                                                          
134300                                                                          
134400     .                                                                    
134500     EJECT                                                                
134600 CAC-JUSTERA-OI SECTION.                                                  
134700*---------------------------------------------------------------*         
134800* HÄR JUSTERAS OI BEROENDE PÅ OM DET ÄR 4 ELLER 5 VECKOR        *         
134900* I PERIODEN. GENOMSNITTLIG OI PER VECKA RÄKNAS UT OCH          *         
135000* MULTIPLICERAS MED 4.33, EG 52/12 (VECKOR/MÅNADER)             *         
135100*---------------------------------------------------------------*         
135200                                                                          
135300     IF ACC-KORNING                                                       
135400                                                                          
135500       MOVE +1 TO TAB-RADIX                                               
135600                                                                          
135700     ELSE                                                                 
135800                                                                          
135900       COMPUTE WS-VECKO-IO ROUNDED =                                      
136000               TABELL-KVOI(1) / WS-VECKA-I-AKT-PERIOD                     
136100       COMPUTE TABELL-KVOI(1) = WS-VECKO-IO * 4.33                        
136200                                                                          
136300       MOVE +2 TO TAB-RADIX                                               
136400                                                                          
136500     END-IF                                                               
136600                                                                          
136700     PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                              
136800                                                                          
136900       MOVE TABELL-TIAARP(TAB-RADIX) TO DAT-I-TIDATUM                     
137000       MOVE 'AARP  '  TO DAT-KDDATFORM                                    
137100       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
137200                           DAT-O-TIDATUM DAT-KDSVAR                       
137300                                                                          
137400       IF DAT-KDSVAR-OK                                                   
137500         MOVE DAT-KVVIPER TO WS-ANTAL-VECKOR                              
137600       ELSE                                                               
137700         MOVE 'SVAR 6 FRÅN WDATKONV EJ OK' TO FELTEXT-STR                 
137800         DISPLAY FELTEXT                                                  
137900         PERFORM S99-ABEND                                                
138000       END-IF                                                             
138100                                                                          
138200       COMPUTE WS-VECKO-IO =                                              
138300                       TABELL-KVOI(TAB-RADIX) / WS-ANTAL-VECKOR           
138400       COMPUTE TABELL-KVOI(TAB-RADIX) = WS-VECKO-IO * 4.33                
138500                                                                          
138600       ADD +1 TO TAB-RADIX                                                
138700     END-PERFORM                                                          
138800     .                                                                    
138900     EJECT                                                                
139000                                                                          
139100                                                                          
139200 CAD-SASONGSANPASSA SECTION.                                              
139300                                                                          
139400     MOVE 1 TO TAB-RADIX                                                  
139500     PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                              
139600        MOVE TABELL-TIAARP(TAB-RADIX) TO SEASON-TIAARP                    
139700        MOVE TABELL-KVOI(TAB-RADIX) TO WS-KVOI-SEASON                     
139800        IF SLAG-RESEASON(SEASON-TIRP) = ZERO                              
139900           CONTINUE                                                       
140000        ELSE                                                              
140100           IF SLAG-RESEASON(SEASON-TIRP) < 0.51                           
140200              MOVE SLAG-KVPB-REF                                          
140300                             TO TABELL-KVOI(TAB-RADIX)                    
140400           ELSE                                                           
140500              COMPUTE TABELL-KVOI(TAB-RADIX) =                            
140600              WS-KVOI-SEASON / SLAG-RESEASON (SEASON-TIRP)                
140700           END-IF                                                         
140800        END-IF                                                            
140900        ADD 1 TO TAB-RADIX                                                
141000     END-PERFORM                                                          
141100     .                                                                    
141200     EJECT                                                                
141300                                                                          
141400                                                                          
141500 CB-UTFOR-BERAKNINGAR SECTION.                                            
141600                                                                          
141700     PERFORM S01-KONTROLLERA-BERAEKNA-PROGN                               
141800     .                                                                    
141900     EJECT                                                                
142000                                                                          
142100                                                                          
142200 D-NOLLA-PB-MFL SECTION.                                                  
142300                                                                          
142400     MOVE W-IDARTNR            TO UT-IDARTNR                              
142500     MOVE SLAG-IDDC            TO UT-IDDC                                 
142600     MOVE ZERO                 TO UT-KVPB-REF                             
142700                                  UT-RETREND                              
142800                                  UT-TIREFMPB                             
142900     MOVE CLAG-KDERS           TO UT-KDERS                                
143000     PERFORM S12-SKRIV-W27130                                             
143100     .                                                                    
143200     EJECT                                                                
143300                                                                          
143400                                                                          
143500 Z-FINIT SECTION.                                                         
143600                                                                          
143700     CLOSE W271TYP                                                        
143800           W271DC                                                         
143900           W27130                                                         
144000           W27138                                                         
144100           W2713F                                                         
144200                                                                          
144300     MOVE 'S' TO POSTSUM-OPKOD                                            
144400     CALL POSTSUM USING POSTSUM-PARM                                      
144500     .                                                                    
144600     EJECT                                                                
144700 S01-KONTROLLERA-BERAEKNA-PROGN SECTION.                                  
144800                                                                          
144900     PERFORM S01A-KOLLA-DATUM-MANUELL-PROGN                               
145000                                                                          
145100*  ÄT LOCK FORECAST ON NEW PARTS 010506  JOHAN L    ESC-LOCK              
145200     PERFORM S01E-KOLLA-PROGNOS-LAASNING                                  
145300*                                                                         
145400     MOVE NEJ TO SW-MAN-30-DAYS                                           
145500     MOVE SLAG-TIREFMPB      TO TMP1-YYMMDD                               
145600     MOVE DAGENS-DATUM       TO TMP2-YYMMDD                               
145700     PERFORM WY2000P1                                                     
145800     IF SLAG-TIREFMPB > WS-TODAY-DATE-30                                  
145900       MOVE JA TO SW-MAN-30-DAYS                                          
146000     END-IF                                                               
146100                                                                          
146200     IF (MANUELL-PROGNOS-SATT                                             
146300     AND SW-MAN-30-DAYS-TRUE                                              
146400     OR  (TMP1-YYMMDD > TMP2-YYMMDD))                                     
146500*----- MANUAL SET FORECAST SHALL BE VALID                                 
146600*----- ALSO MANUAL SET FORECAST SET WITHIN THE 30 LAST DAYS               
146700*----- SHOULD BE VALID                                                    
146800       CONTINUE                                                           
146900     ELSE                                                                 
147000*  INGEN PROGNOSFÖRÄNDRING OM TIREFMPB INTE ÄR PASSERAT !!!               
147100        IF PROGNOS-AER-FAST                                               
147200          CONTINUE                                                        
147300        ELSE                                                              
147400          PERFORM S01B-KOLLA-OI                                           
147500          PERFORM S01C-KOLLA-MANUELL-PROGNOS                              
147600          PERFORM S01D-BERAEKNA-NY-PROGNOS                                
147700        END-IF                                                            
147800     END-IF                                                               
147900     .                                                                    
148000     EJECT                                                                
148100 S01A-KOLLA-DATUM-MANUELL-PROGN SECTION.                                  
148200                                                                          
148300     IF SLAG-TIREFMPB > 0                                                 
148400        MOVE SLAG-TIREFMPB   TO TMP1-YYMMDD                               
148500        MOVE 'AAVVD'         TO DAT-KDDATFORM                             
148600        MOVE WS-TIFINLV      TO DAT-I-TIDATUM                             
148700                                                                          
148800        CALL WDATKONV USING DAT-KDDATFORM                                 
148900                            DAT-I-TIDATUM                                 
149000                            DAT-O-TIDATUM                                 
149100                            DAT-KDSVAR                                    
149200                                                                          
149300        IF DAT-KDSVAR-OK                                                  
149400          MOVE DAT-TIAAMMDD  TO TMP2-YYMMDD                               
149500        ELSE                                                              
149600          MOVE 'SVAR WDATKONV   I S01A SECTION'                           
149700                                TO FELTEXT-STR                            
149800          DISPLAY FELTEXT                                                 
149900          PERFORM S99-ABEND                                               
150000        END-IF                                                            
150100        PERFORM WY2000P1                                                  
150200        IF TMP1-YYMMDD > TMP2-YYMMDD                                      
150300           MOVE SLAG-TIREFMPB                                             
150400                             TO WS-TEST-TIREFMPB                          
150500        ELSE                                                              
150600           MOVE DAT-TIAAMMDD TO WS-TEST-TIREFMPB                          
150700        END-IF                                                            
150800     ELSE                                                                 
150900        MOVE WS-TIFINLV                 TO TMP1-YYWWD                     
151000        MOVE DAGENS-TIAAVVD-LAST-YEAR   TO TMP2-YYWWD                     
151100        PERFORM WY2000P2                                                  
151200        IF TMP1-YYWWD >= TMP2-YYWWD                                       
151300           MOVE 'AAVVD'               TO DAT-KDDATFORM                    
151400           MOVE WS-TIFINLV            TO DAT-I-TIDATUM                    
151500                                                                          
151600           CALL WDATKONV USING DAT-KDDATFORM                              
151700                               DAT-I-TIDATUM                              
151800                               DAT-O-TIDATUM                              
151900                               DAT-KDSVAR                                 
152000                                                                          
152100           IF DAT-KDSVAR-OK                                               
152200             MOVE DAT-TIAAMMDD        TO WS-TEST-TIREFMPB                 
152300           ELSE                                                           
152400             MOVE 'SVAR WDATKONV   I S01A SECTION'                        
152500                                   TO FELTEXT-STR                         
152600             DISPLAY FELTEXT                                              
152700             PERFORM S99-ABEND                                            
152800           END-IF                                                         
152900        END-IF                                                            
153000     END-IF                                                               
153100                                                                          
153200     IF WS-TEST-TIREFMPB > 0                                              
153300*----- CHECK WHAT WEEK TIREFMPB IS IN                                     
153400       MOVE 'AAMMDD'      TO DAT-KDDATFORM                                
153500       MOVE WS-TEST-TIREFMPB TO DAT-I-TIDATUM                             
153600                                                                          
153700       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
153800                           DAT-O-TIDATUM DAT-KDSVAR                       
153900       MOVE DAT-TIAAVV-GRP    TO WS-DAT-TIAAVV                            
154000                                                                          
154100       MOVE TABELL-TIAARP(1)  TO WS-TIAAVV                                
154200       MOVE TABELL-SISTA-TIAAVV(1) TO WS-TIVV                             
154300       MOVE WS-TIAAVV         TO WS-FORSTA-TIAAVV                         
154400                                                                          
154500       MOVE TABELL-TIAARP (MAX-TAB-RADIX) TO WS-TIAAVV                    
154600       MOVE TABELL-FORSTA-TIAAVV (MAX-TAB-RADIX) TO WS-TIVV               
154700       MOVE WS-TIAAVV                    TO WS-SISTA-TIAAVV               
154800                                                                          
154900       IF DAT-KDSVAR-OK                                                   
155000*-----   CHECK THAT MANUEL FC IS IN TABLE                                 
155100*-----   12 PERIODS BACK                                                  
155200         MOVE WS-DAT-TIAAVV      TO TMP1-YYWW                             
155300         MOVE WS-FORSTA-TIAAVV   TO TMP2-YYWW                             
155400         MOVE WS-SISTA-TIAAVV    TO TMP3-YYWW                             
155500         PERFORM WY2000Q3                                                 
155600         IF TMP1-YYWW <= TMP2-YYWW AND                                    
155700            TMP1-YYWW >= TMP3-YYWW                                        
155800           MOVE JA TO MANUELL-PROGNOS-SW                                  
155900*-----     CHECK WHAT PERIOD TIREFMPB IS IN                               
156000           MOVE +1 TO TAB-RADIX                                           
156100           MOVE NEJ TO INDX-SW                                            
156200           PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX OR INDX-HITTAT         
156300             MOVE WS-DAT-TIAAVV                                           
156400                             TO TMP1-YYWW                                 
156500                                                                          
156600             MOVE TABELL-TIAARP(TAB-RADIX)                                
156700                             TO WS-TIAAVV                                 
156800             MOVE TABELL-FORSTA-TIAAVV(TAB-RADIX)                         
156900                             TO WS-TIVV                                   
157000             MOVE WS-TIAAVV  TO WS-FORSTA-TIAAVV                          
157100             MOVE WS-FORSTA-TIAAVV                                        
157200                             TO TMP2-YYWW                                 
157300                                                                          
157400             MOVE TABELL-TIAARP(TAB-RADIX)                                
157500                             TO WS-TIAAVV                                 
157600             MOVE TABELL-SISTA-TIAAVV(TAB-RADIX)                          
157700                             TO WS-TIVV                                   
157800             MOVE WS-TIAAVV  TO WS-SISTA-TIAAVV                           
157900             MOVE WS-SISTA-TIAAVV                                         
158000                             TO TMP3-YYWW                                 
158100                                                                          
158200             PERFORM WY2000Q3                                             
158300             IF  TMP1-YYWW >= TMP2-YYWW                                   
158400             AND TMP1-YYWW <= TMP3-YYWW                                   
158500*-----         TRÄFF I RÄTT PERIOD                                        
158600               MOVE TAB-RADIX TO WS-TIREFMPB-TIAARP                       
158700               MOVE JA TO INDX-SW                                         
158800             END-IF                                                       
158900             ADD +1 TO TAB-RADIX                                          
159000           END-PERFORM                                                    
159100         ELSE                                                             
159200           MOVE NEJ TO MANUELL-PROGNOS-SW                                 
159300         END-IF                                                           
159400       ELSE                                                               
159500         MOVE 'SVAR 5 FRÅN WDATKONV EJ OK' TO FELTEXT-STR                 
159600         DISPLAY FELTEXT                                                  
159700         PERFORM S99-ABEND                                                
159800       END-IF                                                             
159900     ELSE                                                                 
160000       MOVE NEJ TO MANUELL-PROGNOS-SW                                     
160100     END-IF                                                               
160200                                                                          
160300     .                                                                    
160400     EJECT                                                                
160500 S01B-KOLLA-OI SECTION.                                                   
160600                                                                          
160700     PERFORM S01BA-BERAKN-ONORM-OI-GRAENSER                               
160800                                                                          
160900*--- OM 2 PERIODER I RAD (ELLER FLER) LIGGER ÖVER GRÄNSEN                 
161000*--- FÖR ONORMAL OI BETRAKTAS DETTA EJ SOM ONORMAL OI                     
161100*--- OCH SKALL DÅ FÖLJDAKTLIGEN EJ BYTAS UT                               
161200                                                                          
161300     MOVE 1   TO TAB-RADIX                                                
161400     PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                              
161500                                                                          
161600       IF TAB-RADIX = MAX-TAB-RADIX                                       
161700         IF  TABELL-KVOI(TAB-RADIX)   > WS-ONORM-OI-GRAENS-PB             
161800         AND TABELL-KVOI(TAB-RADIX - 1) NOT >                             
161900                                 WS-ONORM-OI-GRAENS-PB                    
162000           MOVE WS-ONORM-OI-GRAENS-PB TO TABELL-KVOI(TAB-RADIX)           
162100         END-IF                                                           
162200       ELSE                                                               
162300         IF  TABELL-KVOI(TAB-RADIX) > WS-ONORM-OI-GRAENS-PB               
162400         AND TABELL-KVOI(TAB-RADIX + 1) > WS-ONORM-OI-GRAENS-PB           
162500           CONTINUE                                                       
162600         ELSE                                                             
162700           IF TABELL-KVOI(TAB-RADIX) > WS-ONORM-OI-GRAENS-PB              
162800             IF TAB-RADIX = 1                                             
162900               MOVE WS-ONORM-OI-GRAENS-PB TO                              
163000                                 TABELL-KVOI(TAB-RADIX)                   
163100             ELSE                                                         
163200               IF TABELL-KVOI(TAB-RADIX - 1) NOT >                        
163300                  WS-ONORM-OI-GRAENS-PB                                   
163400                 MOVE WS-ONORM-OI-GRAENS-PB TO                            
163500                                       TABELL-KVOI(TAB-RADIX)             
163600               END-IF                                                     
163700             END-IF                                                       
163800           END-IF                                                         
163900         END-IF                                                           
164000       END-IF                                                             
164100       ADD +1 TO TAB-RADIX                                                
164200                                                                          
164300     END-PERFORM                                                          
164400     .                                                                    
164500     EJECT                                                                
164600 S01BA-BERAKN-ONORM-OI-GRAENSER SECTION.                                  
164700                                                                          
164800*--- BERÄKNA GRÄNSER FÖR ONORMAL OI (COPYTEXT W271FSG)                    
164900*--- MED HJÄLP AV PROGNOS                                                 
165000                                                                          
165100     IF DCS-CHINA                                                         
165200     OR DCS-NDC-NA                                                        
165300**MATERIAL PRIS REDAN INLAGT I WS-PRIS TIDIGARE VID LÄSNING WDK712        
165400       CONTINUE                                                           
165500     ELSE                                                                 
165600       MOVE CLAG-PRARTSTD  TO WS-PRIS                                     
165700     END-IF                                                               
165800     MOVE 1 TO INDX                                                       
165900     PERFORM UNTIL INDX > MAX-FSGFAKT                                     
166000       IF WS-PRIS <=                                                      
166100                               FSG-DC-PRARTSTD-MAX (INDX)                 
166200         COMPUTE WS-ONORM-OI-GRAENS-PB ROUNDED =                          
166300            (FSG-A (INDX) * SLAG-KVPB-REF) +                              
166400             FSG-B (INDX)                                                 
166500         MOVE 99 TO INDX                                                  
166600       ELSE                                                               
166700         ADD 1 TO INDX                                                    
166800       END-IF                                                             
166900     END-PERFORM                                                          
167000     .                                                                    
167100     EJECT                                                                
167200 S01C-KOLLA-MANUELL-PROGNOS SECTION.                                      
167300                                                                          
167400     IF MANUELL-PROGNOS-SATT                                              
167500                                                                          
167600*----- BYT UT ALLA OI FR.O.M TIREFMPB:S PERIOD OCH BAKÅT                  
167700*------MOT NUVARANDE PROGNOS (KVPB-REF)                                   
167800       MOVE WS-TIREFMPB-TIAARP TO TAB-RADIX                               
167900       PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                            
168000         IF SLAG-TIREFMPB > ZERO                                          
168100            IF SLAG-KVPB-HIST > ZERO                                      
168200               MOVE SLAG-KVPB-HIST                                        
168300                            TO TABELL-KVOI (TAB-RADIX)                    
168400            ELSE                                                          
168500               MOVE SLAG-KVPB-REF                                         
168600                            TO TABELL-KVOI (TAB-RADIX)                    
168700            END-IF                                                        
168800         ELSE                                                             
168900            MOVE SLAG-KVPB-REF                                            
169000                            TO TABELL-KVOI (TAB-RADIX)                    
169100         END-IF                                                           
169200         ADD 1 TO TAB-RADIX                                               
169300       END-PERFORM                                                        
169400                                                                          
169500     END-IF                                                               
169600                                                                          
169700     .                                                                    
169800     EJECT                                                                
169900 S01D-BERAEKNA-NY-PROGNOS SECTION.                                        
170000                                                                          
170100     PERFORM S01DF-SEARCH-CORRECT-DC                                      
170200     PERFORM S01DE-BERAEKNA-TRENDFAKT                                     
170300     PERFORM S01DA-BERAEKNA-PREL-PROGNOS                                  
170400                                                                          
170500     IF WS-PREL-KVPB-REF < 1                                              
170600       MOVE WS-PREL-KVPB-REF TO WS-NY-KVPB-REF                            
170700     ELSE                                                                 
170800       PERFORM S01DB-BERAEKNA-MEDELPROGNOS                                
170900                                                                          
171000       COMPUTE WS-KVOTEN ROUNDED = (WS-PREL-KVPB-REF + 1) /               
171100                                   (WS-MEDEL-KVPB-REF + 1)                
171200                                                                          
171300       PERFORM S01DC-KONTROLLERA-TREND                                    
171400       IF INGEN-TREND                                                     
171500         MOVE WS-PREL-KVPB-REF TO WS-NY-KVPB-REF                          
171600       ELSE                                                               
171700         IF SVAG-TREND                                                    
171800           MOVE 2 TO INDX                                                 
171900         ELSE                                                             
172000           IF STARK-TREND                                                 
172100             MOVE 3 TO INDX                                               
172200           END-IF                                                         
172300         END-IF                                                           
172400                                                                          
172500         IF ACC-KORNING                                                   
172600                                                                          
172700           MOVE +1 TO TAB-RADIX                                           
172800           MOVE ZERO TO WS-NY-KVPB-REF                                    
172900                                                                          
173000           PERFORM UNTIL TAB-RADIX > 12                                   
173100             COMPUTE WS-NY-KVPB-REF ROUNDED =                             
173200                       WS-NY-KVPB-REF +                                   
173300                (PROG-DC-TRENDFAKT (TAB-RADIX, INDX) *                    
173400                         TABELL-KVOI (TAB-RADIX))                         
173500             ADD 1 TO TAB-RADIX                                           
173600           END-PERFORM                                                    
173700         ELSE                                                             
173800                                                                          
173900           MOVE +1 TO TAB-RADIX                                           
174000           MOVE ZERO TO WS-NY-KVPB-REF                                    
174100                                                                          
174200           PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                        
174300             COMPUTE WS-NY-KVPB-REF ROUNDED =                             
174400                       WS-NY-KVPB-REF +                                   
174500              (WS-DC-TRENDFAKT-VKA (TAB-RADIX, INDX) *                    
174600                         TABELL-KVOI (TAB-RADIX))                         
174700             ADD 1 TO TAB-RADIX                                           
174800           END-PERFORM                                                    
174900         END-IF                                                           
175000                                                                          
175100       END-IF                                                             
175200     END-IF                                                               
175300                                                                          
175400*--- NY PROGNOS KAN INTE BLI LÄGRE ÄN 70% AV DEN GAMLA                    
175500*--- (KONTROLL SÅ DEN INTE SJUNKER FÖR SNABBT)                            
175600     IF  SLAG-KVPB-REF  = 0.1                                             
175700     AND WS-NY-KVPB-REF < 0.1                                             
175800        CONTINUE                                                          
175900     ELSE                                                                 
176000        IF WS-NY-KVPB-REF < 0.7 * SLAG-KVPB-REF                           
176100          COMPUTE WS-NY-KVPB-REF = 0.7 * SLAG-KVPB-REF                    
176200        END-IF                                                            
176300     END-IF                                                               
176400                                                                          
176500*--- AVRUNDA TILL EN DECIMAL                                              
176600     COMPUTE NY-KVPB-REF = WS-NY-KVPB-REF + 0.05                          
176700                                                                          
176800*--- SÄTT NY PROGNOS FÖR ARTIKELN                                         
176900*--- OM * INTE * PROGNOSEN ÄR LÅST, DÅ FÅR DEN ENDAST HÖJAS               
177000     IF NY-KVPB-REF NOT = SLAG-KVPB-REF                                   
177100        IF PROGNOS-AER-LAAST                                              
177200           IF NY-KVPB-REF > SLAG-KVPB-REF                                 
177300              MOVE JA         TO ARTIKEL-SW                               
177400           END-IF                                                         
177500        ELSE                                                              
177600           MOVE JA         TO ARTIKEL-SW                                  
177700        END-IF                                                            
177800     END-IF                                                               
177900     .                                                                    
178000     EJECT                                                                
178100 S01DA-BERAEKNA-PREL-PROGNOS SECTION.                                     
178200*----------------------------------------------------------------*        
178300* HÄR BERÄKNAS PRELIMINÄR PROGNOS MHA VIKTNINGSTABELL            *        
178400* (PROGNOSFAKTORER) MED INDX=1 (NORMAL)                          *        
178500* COPYTEXT W271PROG                                              *        
178600*----------------------------------------------------------------*        
178700                                                                          
178800     IF ACC-KORNING                                                       
178900       MOVE 1 TO INDX                                                     
179000                 TAB-RADIX                                                
179100       MOVE ZERO TO WS-PREL-KVPB-REF                                      
179200                    WS-ANTAL-FAKTORER-STOERRE-NOLL                        
179300                                                                          
179400       PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                            
179500         COMPUTE WS-PREL-KVPB-REF ROUNDED =                               
179600                        WS-PREL-KVPB-REF +                                
179700                  (PROG-DC-TRENDFAKT (TAB-RADIX, INDX) *                  
179800                         TABELL-KVOI (TAB-RADIX) )                        
179900         IF PROG-DC-TRENDFAKT (TAB-RADIX, INDX) > ZERO                    
180000           ADD 1 TO WS-ANTAL-FAKTORER-STOERRE-NOLL                        
180100         END-IF                                                           
180200         ADD 1 TO TAB-RADIX                                               
180300       END-PERFORM                                                        
180400     ELSE                                                                 
180500       MOVE 1 TO INDX                                                     
180600                 TAB-RADIX                                                
180700       MOVE ZERO TO WS-PREL-KVPB-REF                                      
180800                    WS-ANTAL-FAKTORER-STOERRE-NOLL                        
180900                                                                          
181000       PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                            
181100         COMPUTE WS-PREL-KVPB-REF ROUNDED =                               
181200                        WS-PREL-KVPB-REF +                                
181300                 (WS-DC-TRENDFAKT-VKA (TAB-RADIX, INDX)          *        
181400                         TABELL-KVOI (TAB-RADIX) )                        
181500         IF WS-DC-TRENDFAKT-VKA (TAB-RADIX, INDX) > ZERO                  
181600           ADD 1 TO WS-ANTAL-FAKTORER-STOERRE-NOLL                        
181700         END-IF                                                           
181800         ADD 1 TO TAB-RADIX                                               
181900       END-PERFORM                                                        
182000     END-IF                                                               
182100     .                                                                    
182200     EJECT                                                                
182300 S01DB-BERAEKNA-MEDELPROGNOS SECTION.                                     
182400*----------------------------------------------------------------*        
182500* HÄR BERÄKNAS MEDELPROGNOS                                      *        
182600*----------------------------------------------------------------*        
182700                                                                          
182800     IF ACC-KORNING                                                       
182900       MOVE +1 TO TAB-RADIX                                               
183000     ELSE                                                                 
183100       MOVE +2 TO TAB-RADIX                                               
183200     END-IF                                                               
183300                                                                          
183400     MOVE ZERO TO WS-KVOI-TOT                                             
183500                  WS-MEDEL-KVPB-REF                                       
183600                                                                          
183700     PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                              
183800       COMPUTE WS-KVOI-TOT ROUNDED =                                      
183900                         WS-KVOI-TOT +                                    
184000                         TABELL-KVOI (TAB-RADIX)                          
184100       ADD 1 TO TAB-RADIX                                                 
184200     END-PERFORM                                                          
184300                                                                          
184400     IF WS-ANTAL-FAKTORER-STOERRE-NOLL = ZERO                             
184500       MOVE ZERO TO WS-MEDEL-KVPB-REF                                     
184600     ELSE                                                                 
184700       COMPUTE WS-MEDEL-KVPB-REF ROUNDED =                                
184800               WS-KVOI-TOT / 12                                           
184900     END-IF                                                               
185000     .                                                                    
185100     EJECT                                                                
185200 S01DC-KONTROLLERA-TREND SECTION.                                         
185300*----------------------------------------------------------------*        
185400* HÄR KONTROLLERAS OM ARTIKELN HAR NÅGON TREND MHA KVOTEN.       *        
185500* ARTIKELN KAN HA:                                               *        
185600*     ¤ INGEN TREND                                              *        
185700*     ¤ SVAG TREND  (TREND UPP ELLER TREND NER)                  *        
185800*     ¤ STARK TREND (TREND STARKT UPP ELLER TREND STARKT NER)    *        
185900*----------------------------------------------------------------*        
186000                                                                          
186100                                                                          
186200     IF (WS-KVOTEN >= 0.80) AND                                           
186300        (WS-KVOTEN <= 1.20)                                               
186400       MOVE 'INGEN' TO TREND-SW                                           
186500     ELSE                                                                 
186600       IF (WS-KVOTEN > 1.20 AND WS-KVOTEN < 1.35) OR                      
186700          (WS-KVOTEN > 0.64 AND WS-KVOTEN < 0.80)                         
186800         MOVE 'SVAG ' TO TREND-SW                                         
186900       ELSE                                                               
187000         MOVE 'STARK' TO TREND-SW                                         
187100       END-IF                                                             
187200     END-IF                                                               
187300     .                                                                    
187400     EJECT                                                                
187500 S01DD-BERAEKNA-MEDELPROGNOS SECTION.                                     
187600*----------------------------------------------------------------*        
187700* HÄR BERÄKNAS MEDELPROGNOS                                      *        
187800*----------------------------------------------------------------*        
187900                                                                          
188000     IF ACC-KORNING                                                       
188100       MOVE +1 TO TAB-RADIX                                               
188200     ELSE                                                                 
188300       MOVE +2 TO TAB-RADIX                                               
188400     END-IF                                                               
188500                                                                          
188600     MOVE ZERO TO WS-KVOI-TOT                                             
188700                  WS-MEDEL-KVPB-REF                                       
188800                                                                          
188900     PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                              
189000       COMPUTE WS-KVOI-TOT ROUNDED =                                      
189100                         WS-KVOI-TOT +                                    
189200                         TABELL-KVOI (TAB-RADIX)                          
189300       ADD 1 TO TAB-RADIX                                                 
189400     END-PERFORM                                                          
189500                                                                          
189600     COMPUTE WS-MEDEL-KVPB-REF ROUNDED =                                  
189700               WS-KVOI-TOT / 12                                           
189800     .                                                                    
189900     EJECT                                                                
190000                                                                          
190100 S01DE-BERAEKNA-TRENDFAKT SECTION.                                        
190200                                                                          
190300     COMPUTE WS-DC-TRENDFAKT-VKA (1, 1) ROUNDED =                         
190400             PROG-DC-TRENDFAKT (1, 1)                                     
190500           * (WS-VECKA-I-AKT-PERIOD / WS-KVVIPER)                         
190600     COMPUTE WS-DC-TRENDFAKT-VKA (1, 2) ROUNDED =                         
190700             PROG-DC-TRENDFAKT (1, 2)                                     
190800           * (WS-VECKA-I-AKT-PERIOD / WS-KVVIPER)                         
190900     COMPUTE WS-DC-TRENDFAKT-VKA (1, 3) ROUNDED =                         
191000             PROG-DC-TRENDFAKT (1, 3)                                     
191100           * (WS-VECKA-I-AKT-PERIOD / WS-KVVIPER)                         
191200     MOVE 2                  TO PER-INDX                                  
191300     PERFORM UNTIL PER-INDX > 12                                          
191400       MOVE 1                TO TREND-INDX                                
191500       PERFORM UNTIL TREND-INDX > 3                                       
191600         COMPUTE WS-DC-TRENDFAKT-VKA                                      
191700                    (PER-INDX, TREND-INDX) ROUNDED =                      
191800          (PROG-DC-TRENDFAKT (PER-INDX - 1,            TREND-INDX)        
191900        * ((WS-KVVIPER - WS-VECKA-I-AKT-PERIOD) / WS-KVVIPER))            
192000        +                                                                 
192100          (PROG-DC-TRENDFAKT (PER-INDX, TREND-INDX)                       
192200        * (WS-VECKA-I-AKT-PERIOD / WS-KVVIPER))                           
192300         ADD 1               TO TREND-INDX                                
192400       END-PERFORM                                                        
192500       ADD 1                 TO PER-INDX                                  
192600     END-PERFORM                                                          
192700     COMPUTE WS-DC-TRENDFAKT-VKA (13, 1) ROUNDED =                        
192800             PROG-DC-TRENDFAKT (12, 1)                                    
192900         * ((WS-KVVIPER - WS-VECKA-I-AKT-PERIOD) / WS-KVVIPER)            
193000     COMPUTE WS-DC-TRENDFAKT-VKA (13, 2) ROUNDED =                        
193100             PROG-DC-TRENDFAKT (12, 2)                                    
193200         * ((WS-KVVIPER - WS-VECKA-I-AKT-PERIOD) / WS-KVVIPER)            
193300     COMPUTE WS-DC-TRENDFAKT-VKA (13, 3) ROUNDED =                        
193400             PROG-DC-TRENDFAKT (12, 3)                                    
193500         * ((WS-KVVIPER - WS-VECKA-I-AKT-PERIOD) / WS-KVVIPER)            
193600     .                                                                    
193700     EJECT                                                                
193800 S01DF-SEARCH-CORRECT-DC SECTION.                                         
193900                                                                          
194000     MOVE 'N'   TO DCS-TRAEFF                                             
194100     MOVE +1 TO IX-DC                                                     
194200     PERFORM UNTIL (IX-DC > DC-MAX) OR (DCS-TRAEFF = 'J')                 
194300       IF TAB-PROG-DC-IDDC (IX-DC) = SLAG-IDDC                            
194400         MOVE +1 TO IX-TAB                                                
194500         MOVE JA TO DCS-TRAEFF                                            
194600         PERFORM UNTIL IX-TAB > 12                                        
194700           MOVE TAB-PROG-DC-NORMAL (IX-DC, IX-TAB) TO                     
194800                                 PROG-DC-NORMAL (IX-TAB)                  
194900           MOVE TAB-PROG-DC-SVAG-TREND (IX-DC, IX-TAB) TO                 
195000                                 PROG-DC-SVAG-TREND (IX-TAB)              
195100           MOVE TAB-PROG-DC-STARK-TREND (IX-DC, IX-TAB) TO                
195200                                 PROG-DC-STARK-TREND (IX-TAB)             
195300           ADD +1 TO IX-TAB                                               
195400         END-PERFORM                                                      
195500       END-IF                                                             
195600       ADD +1 TO IX-DC                                                    
195700     END-PERFORM                                                          
195800                                                                          
195900     IF DCS-TRAEFF = 'N'                                                  
196000       MOVE +1 TO IX-DC                                                   
196100       PERFORM UNTIL (IX-DC > DC-MAX) OR (DCS-TRAEFF = 'J')               
196200         IF TAB-PROG-DC-IDDC (IX-DC) = '99'                               
196300           MOVE +1 TO IX-TAB                                              
196400           MOVE JA TO DCS-TRAEFF                                          
196500           PERFORM UNTIL IX-TAB > 12                                      
196600             MOVE TAB-PROG-DC-NORMAL (IX-DC, IX-TAB) TO                   
196700                                   PROG-DC-NORMAL (IX-TAB)                
196800             MOVE TAB-PROG-DC-SVAG-TREND (IX-DC, IX-TAB) TO               
196900                                   PROG-DC-SVAG-TREND (IX-TAB)            
197000             MOVE TAB-PROG-DC-STARK-TREND (IX-DC, IX-TAB) TO              
197100                                   PROG-DC-STARK-TREND (IX-TAB)           
197200             ADD +1 TO IX-TAB                                             
197300           END-PERFORM                                                    
197400         END-IF                                                           
197500         ADD +1 TO IX-DC                                                  
197600       END-PERFORM                                                        
197700     END-IF                                                               
197800                                                                          
197900     .                                                                    
198000 S01E-KOLLA-PROGNOS-LAASNING SECTION.                                     
198100                                                                          
198200     MOVE NEJ TO ESCLOCK-SW                                               
198300                                                                          
198400                                                                          
198500* KONTROLLERA OM PB ÄR MANUELLT LÅST                                      
198600     MOVE SLAG-TIREFMPB      TO TMP1-YYMMDD                               
198700     MOVE DAGENS-DATUM       TO TMP2-YYMMDD                               
198800     PERFORM WY2000P1                                                     
198900     PERFORM S01EA-HAMTA-PUBLICERINGSDATUM                                
199000***FÖR ATT SLÄPPA IGENOM ART MED PUBLICERINGSDATUM FÖRE ÅR 2000           
199100     IF PUBLICERINGSDATUM > 500000                                        
199200       MOVE 010101 TO PUBLICERINGSDATUM                                   
199300     END-IF                                                               
199400***************************************************************           
199500     IF WDK6-ART-FLERS = JA AND                                           
199600        PUBLICERINGSDATUM > DAGENS-DATUM                                  
199700        MOVE 'A' TO ESCLOCK-SW                                            
199800     ELSE                                                                 
199900        IF  MANUELL-PROGNOS-SATT AND                                      
200000            TMP1-YYMMDD > TMP2-YYMMDD                                     
200100           MOVE 'A' TO ESCLOCK-SW                                         
200200        ELSE                                                              
200300*    OM INTE ART. ÄR MANUELLT LÅST SÅ KAN DEN VARA ESC-LÅST               
200400                                                                          
200500*    KONTROLLERA OM ARTIKLEN ÄR ESC-LÅST                                  
200600           IF SLAG-DAREFESC > ZERO                                        
200700              IF SLAG-DAREFESC >= DA-DAGENS-DATUM                         
200800                 MOVE JA TO ESCLOCK-SW                                    
200900              END-IF                                                      
201000           END-IF                                                         
201100                                                                          
201200*    KONTROLLERA OM PUBVECKA DC ÄR INOM ETT ÅR                            
201300           IF DCS-NDC-NA                                                  
201400             MOVE DCS-IDLANDX2          TO W-IDLAND                       
201500             PERFORM IMS-GU-WDK712                                        
201600              IF SEGMENT-FINNS                                            
201700                IF LART-DAPUBL > ZERO                                     
201800                   PERFORM S02-DAPUBL-US-1YEAR                            
201900                   IF WS-DAPUBL-US >= DA-DAGENS-DATUM                     
202000                      MOVE JA TO ESCLOCK-SW                               
202100                   END-IF                                                 
202200                END-IF                                                    
202300              END-IF                                                      
202400           END-IF                                                         
202500                                                                          
202600*    KONTROLLERA OM PUBVECKA CDC ÄR INOM 1 ÅR                             
202700           IF DCS-SDC    OR DCS-NDC-PF OR                                 
202800              DCS-NDC-CN OR DCS-NDC-OTHERS OR DCS-NDC-SA                  
202900              CONTINUE                                                    
203000           ELSE                                                           
203100              IF WS-TIFINLV > ZERO                                        
203200                 PERFORM S04-WS-TIFINLV-1YEAR                             
203300                 MOVE WS3-TIFINLV TO TMP1-YYMMDD                          
203400                 MOVE DAGENS-DATUM TO TMP2-YYMMDD                         
203500                 PERFORM WY2000P1                                         
203600                 IF TMP1-YYMMDD > TMP2-YYMMDD                             
203700                    MOVE JA TO ESCLOCK-SW                                 
203800                 END-IF                                                   
203900              END-IF                                                      
204000           END-IF                                                         
204100        END-IF                                                            
204200     END-IF                                                               
204300                                                                          
204400     .                                                                    
204500     EJECT                                                                
204600 S01EA-HAMTA-PUBLICERINGSDATUM  SECTION.                                  
204700                                                                          
204800     IF DCS-SDC                                                           
204900        MOVE 'AAVVD'            TO DAT-KDDATFORM                          
205000        MOVE WS-TIFINLV         TO DAT-I-TIDATUM                          
205100* WS-TIFINLV KOMMER FRÅN WDK6-ART-TIFINLV                                 
205200                                                                          
205300        CALL WDATKONV USING DAT-KDDATFORM                                 
205400                            DAT-I-TIDATUM                                 
205500                            DAT-O-TIDATUM                                 
205600                            DAT-KDSVAR                                    
205700                                                                          
205800        IF DAT-KDSVAR-OK                                                  
205900           MOVE DAT-TIAAMMDD TO PUBLICERINGSDATUM                         
206000        ELSE                                                              
206100           MOVE 'SVAR WDATKONV   I S01EA SECTION'                         
206200                             TO FELTEXT-STR                               
206300           DISPLAY FELTEXT                                                
206400           PERFORM S99-ABEND                                              
206500        END-IF                                                            
206600     ELSE                                                                 
206700        MOVE WS-TIFINLV         TO PUBLICERINGSDATUM                      
206800     END-IF                                                               
206900     .                                                                    
207000                                                                          
207100     EJECT                                                                
207200 S02-DAPUBL-US-1YEAR SECTION.                                             
207300     MOVE LART-DAPUBL TO WS-DAPUBL-US                                     
207400     ADD +1 TO WS-DAPUBL-US-YEAR                                          
207500     .                                                                    
207600     EJECT                                                                
207700 S04-WS-TIFINLV-1YEAR SECTION.                                            
207800*    ADDERA 1 ÅR TILL PUBVECKA CDC                                        
207900     MOVE WS-TIFINLV       TO WS2-TIFINLV                                 
208000     MOVE WS2-TIFINLV-AAVV TO DATUM-AAVV                                  
208100     MOVE +52              TO ANTAL                                       
208200     CALL W009VADD USING DATUM-AAVV ANTAL                                 
208300     MOVE DATUM-AAVV TO WS2-TIFINLV-AAVV                                  
208400                                                                          
208500*    RÄKNA OM PUBVECKA CDC TILL ÅÅMMDD                                    
208600     IF WS2-TIFINLV-AAVV > 5000                                           
208700       MOVE 010101           TO WS3-TIFINLV                               
208800     ELSE                                                                 
208900       MOVE 'AAVVD'          TO DAT-KDDATFORM                             
209000       MOVE WS2-TIFINLV      TO DAT-I-TIDATUM                             
209100                                                                          
209200       CALL WDATKONV USING DAT-KDDATFORM                                  
209300                           DAT-I-TIDATUM                                  
209400                           DAT-O-TIDATUM                                  
209500                           DAT-KDSVAR                                     
209600                                                                          
209700       IF DAT-KDSVAR-OK                                                   
209800         MOVE DAT-TIAAMMDD   TO WS3-TIFINLV                               
209900       ELSE                                                               
210000         MOVE 'SVAR WDATKONV   I S04 SECTION'                             
210100                               TO FELTEXT-STR                             
210200         DISPLAY FELTEXT                                                  
210300         PERFORM S99-ABEND                                                
210400       END-IF                                                             
210500     END-IF                                                               
210600     .                                                                    
210700     EJECT                                                                
210800 S05-DELETE-JUST SECTION.                                                 
210900                                                                          
211000     IF WS-TIPBJUST-1 = DAGENS-TIAAVV-GRP                                 
211100       MOVE 'J'             TO UT-NOLLA-JUST1                             
211200     END-IF                                                               
211300     IF WS-TIPBJUST-2 = DAGENS-TIAAVV-GRP                                 
211400       MOVE 'J'             TO UT-NOLLA-JUST2                             
211500     END-IF                                                               
211600     .                                                                    
211700     EJECT                                                                
211800 S11-LAES-W271TYP  SECTION.                                               
211900                                                                          
212000     READ W271TYP             INTO W271TYP-POST                           
212100     .                                                                    
212200     SKIP3                                                                
212300                                                                          
212400 S12-SKRIV-W27130 SECTION.                                                
212500                                                                          
212600     WRITE UT-POST FROM UT-AREA                                           
212700                                                                          
212800     MOVE 'W27130'   TO POSTSUM-FDNAMN                                    
212900     MOVE 'W27130D3' TO POSTSUM-DDNAMN2                                   
213000     CALL POSTSUM USING POSTSUM-PARM                                      
213100     .                                                                    
213200     SKIP3                                                                
213300                                                                          
213400 S13-SKRIV-W27138 SECTION.                                                
213500                                                                          
213600     WRITE UT2-POST FROM UT2-AREA                                         
213700                                                                          
213800     MOVE 'W27138'   TO POSTSUM-FDNAMN                                    
213900     MOVE 'W27130D4' TO POSTSUM-DDNAMN2                                   
214000     CALL POSTSUM USING POSTSUM-PARM                                      
214100     .                                                                    
214200     SKIP3                                                                
214300                                                                          
214400 S14-SKRIV-W2713F SECTION.                                                
214500                                                                          
214600     WRITE UT3-POST FROM UT3-AREA                                         
214700                                                                          
214800     MOVE 'W2713F'   TO POSTSUM-FDNAMN                                    
214900     MOVE 'W27130D5' TO POSTSUM-DDNAMN2                                   
215000     CALL POSTSUM USING POSTSUM-PARM                                      
215100     .                                                                    
215200     SKIP3                                                                
215300                                                                          
215400 S21-LAES-W271DC   SECTION.                                               
215500                                                                          
215600      READ W271DC             INTO DC-POST                                
215700     .                                                                    
215800     EJECT                                                                
215900 S99-ABEND SECTION.                                                       
216000                                                                          
216100     MOVE 'S' TO POSTSUM-OPKOD                                            
216200     CALL POSTSUM USING POSTSUM-PARM                                      
216300     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
216400     .                                                                    
216500     EJECT                                                                
216600* --- IMS SEKTIONER ---                                                   
216700     SKIP3                                                                
216800     EJECT                                                                
216900 IMS-GU-WDK601 SECTION.                                                   
217000                                                                          
217100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
217200          DELIMITED BY SIZE INTO SSA1                                     
217300     MOVE '  ' TO GODK-STATUSKODER                                        
217400     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
217500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
217600     PERFORM IMS-STATUSKONTROLL                                           
217700     .                                                                    
217800     EJECT                                                                
217900 IMS-GNP-WDK611 SECTION.                                                  
218000                                                                          
218100     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
218200          DELIMITED BY SIZE INTO SSA1                                     
218300     MOVE '  ' TO GODK-STATUSKODER                                        
218400     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
218500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
218600     PERFORM IMS-STATUSKONTROLL                                           
218700     .                                                                    
218800     EJECT                                                                
218900 IMS-GN-WDK7 SECTION.                                                     
219000                                                                          
219100     CALL CBLTDLI USING GN WDK7-PCB DLI-IO-AREA-K7                        
219200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
219300     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
219400     PERFORM IMS-STATUSKONTROLL                                           
219500     .                                                                    
219600     EJECT                                                                
219700 IMS-GU-WDK712      SECTION.                                              
219800                                                                          
219900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
220000          DELIMITED BY SIZE  INTO SSA1                                    
220100     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
220200          DELIMITED BY SIZE  INTO SSA2                                    
220300     MOVE '  GE'               TO GODK-STATUSKODER                        
220400     CALL CBLTDLI USING GU WDK72-PCB DLI-IO-WDK712 SSA1 SSA2              
220500     MOVE WDK72-STATUS-CODE TO STATUS-WS                                  
220600     PERFORM IMS-STATUSKONTROLL                                           
220700     .                                                                    
220800     EJECT                                                                
220900 IMS-GU-WDK722      SECTION.                                              
221000                                                                          
221100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
221200          DELIMITED BY SIZE  INTO SSA1                                    
221300     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
221400          DELIMITED BY SIZE  INTO SSA2                                    
221500     STRING 'WDK722  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
221600          DELIMITED BY SIZE INTO SSA3                                     
221700     MOVE '  GE'               TO GODK-STATUSKODER                        
221800     CALL CBLTDLI USING GU WDK72-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3         
221900     MOVE WDK72-STATUS-CODE TO STATUS-WS                                  
222000     PERFORM IMS-STATUSKONTROLL                                           
222100     .                                                                    
222200     EJECT                                                                
222300 IMS-GU-WDL711 SECTION.                                                   
222400                                                                          
222500     STRING 'WDL701  (IDARTNR  =' W-IDARTNR-X ')'                         
222600          DELIMITED BY SIZE INTO SSA1                                     
222700     STRING 'WDL711  (IDDC     =' W-IDDC-X ')'                            
222800          DELIMITED BY SIZE INTO SSA2                                     
222900     MOVE '  GE' TO GODK-STATUSKODER                                      
223000     CALL CBLTDLI USING GU WDL7-PCB DLI-IO-WDL711 SSA1 SSA2               
223100     MOVE WDL7-STATUS-CODE TO STATUS-WS                                   
223200     PERFORM IMS-STATUSKONTROLL                                           
223300     .                                                                    
223400     EJECT                                                                
223500                                                                          
223600 IMS-GU-WDB601    SECTION.                                                
223700     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
223800          DELIMITED BY SIZE INTO SSA1                                     
223900     MOVE '  ' TO GODK-STATUSKODER                                        
224000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
224100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
224200     PERFORM IMS-STATUSKONTROLL                                           
224300     .                                                                    
224400     EJECT                                                                
224500                                                                          
224600 IMS-GN-WDB601    SECTION.                                                
224700     MOVE 'WDB601  ' TO SSA1                                              
224800     MOVE '  GB'     TO GODK-STATUSKODER                                  
224900     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-AREA-B601 SSA1                 
225000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
225100     PERFORM IMS-STATUSKONTROLL                                           
225200     .                                                                    
225300     EJECT                                                                
225400                                                                          
225500 IMS-GNP-WDB616    SECTION.                                               
225600                                                                          
225700     STRING 'WDB616  (IDDCREF  =' W-IDDC-B616-X ')'                       
225800          DELIMITED BY SIZE INTO SSA1                                     
225900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
226000     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-AREA-B616 SSA1                
226100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
226200     PERFORM IMS-STATUSKONTROLL                                           
226300     .                                                                    
226400     EJECT                                                                
226500                                                                          
226600 IMS-GNP-WDB618 SECTION.                                                  
226700                                                                          
226800     STRING 'WDB618  (KDPROGOI =' W-KDPROGOI-X ')'                        
226900          DELIMITED BY SIZE INTO SSA1                                     
227000     MOVE '  GEGB'           TO GODK-STATUSKODER                          
227100     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-AREA-B618  SSA1               
227200     MOVE WDB6-STATUS-CODE   TO STATUS-WS                                 
227300     PERFORM IMS-STATUSKONTROLL                                           
227400     .                                                                    
227500     EJECT                                                                
227600 IMS-STATUSKONTROLL SECTION.                                              
227700                                                                          
227800     SET STATUS-IX TO 1                                                   
227900     SEARCH GODK-STATUS                                                   
228000       AT END                                                             
228100         STRING 'OTILLÅTEN RETURKOD FRÅN IMS: ' STATUS-WS                 
228200           DELIMITED BY SIZE INTO FELTEXT-STR                             
228300         DISPLAY FELTEXT                                                  
228400         CALL FELLOG                                                      
228500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
228600         CONTINUE                                                         
228700     END-SEARCH                                                           
228800     .                                                                    
228900     EJECT                                                                
229000*    -COPY WY2000P1                                                       
229100     EJECT                                                                
229200*    -COPY WY2000P2                                                       
229300     EJECT                                                                
230000*    -COPY WY2000Q3                                                       
