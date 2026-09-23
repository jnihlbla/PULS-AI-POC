000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2713400.                                                
000400*AUTHOR.         JOHAN NIHLBLAD.                                          
000500*DATE-WRITTEN.   OKTOBER 2012.                                            
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET BERÄKNAR NY PROGNOS FÖR ALLA AKTIVA                   
001100*        ARTIKLAR PÅ ALLA SDC GÄLLANDE REFILL OI                          
001200*        DETTA ÄR NÄSTAN EN KOPIA PÅ W2713000                             
001300*                                                                         
001400*        PROGRAMMET GÅR DAGLIGEN OCH VID SAMTLIGA VECKOSLUT               
001500*        - VID ACCOUNT-PERIODSKIFTE BERÄKNAS PROGNOS PÅ SAMTLIGA          
001600*          AKTIVA ARTIKLAR                                                
001700*                                                                         
001800*        PROGRAMMET KÖRS AV TVÅ JOBB, W271J134 OCH W271J234               
001900*          VARJE JOB TAR MED SIG EN UPPGIFT FRÅN                          
002000*          VAR SIN CONSTANTMEDLEM MED UPPGIFT OM VILKEN                   
002100*          KÖRDAG DET ÄR.                                                 
002200*          WEEK- (EJ PERIODKÖRNING) PROGNOS FÖR NYAKTIVERADE              
002300*                ARTIKLAR                                                 
002400*          ACC - PROGNOS FÖR SAMTLIGA AKTIVA ARTIKAR UTAN                 
002500*                ERSÄTTNINGSKOD                                           
002600*                                                                         
002700*        PROGRAMMET LÄSER      WDK7                                       
002800*                              WDL7                                       
002900*                                                                         
003000*    ABENDKODER:                                                          
003100*        U0016 -  SVAR FRÅN WDATKONV EJ OK                                
003200*              -  "ÖVERSÄTTNING" AV LAGER TILL DC-INDX SAKNAS             
003300*        U1000 -  . . . .                                                 
003400*                                                                         
003410*                                                                         
003500                                                                          
003600     SKIP3                                                                
003700 ENVIRONMENT DIVISION.                                                    
003800     SKIP2                                                                
003900 INPUT-OUTPUT SECTION.                                                    
004000                                                                          
004100 FILE-CONTROL.                                                            
004200     SKIP2                                                                
004300*                                                                         
004400     SELECT W271TYP       ASSIGN W27134D1.                                
004500                                                                          
004600*          --- UPPGIFT OM DAG, VECKO- ELLER PERIODKÖRNING                 
004700                                                                          
004800     SELECT W271DC           ASSIGN W27110D2.                             
004900*         ---DC ATT KONTROLLERA I DENNA KÖRNING                           
005000                                                                          
005100     SELECT W27134                     ASSIGN TO W27134D3.                
005200*          --- UT-FIL                                                     
005300                                                                          
005400     SELECT W2713A                     ASSIGN TO W27134D4.                
005500*          --- UT2-FIL, LARM AVVIKANDE PROGNOSER                          
005600     EJECT                                                                
005700 DATA DIVISION.                                                           
005800     SKIP3                                                                
005900 FILE SECTION.                                                            
006000     SKIP3                                                                
006100                                                                          
006200                                                                          
006300 FD  W271TYP                                                              
006400     LABEL RECORD STANDARD                                                
006500     RECORDING F                                                          
006600     BLOCK CONTAINS 0.                                                    
006700                                                                          
006800 01  FILLER                  PIC X(80).                                   
006900                                                                          
007000                                                                          
007100 FD  W271DC                                                               
007200     LABEL RECORD STANDARD                                                
007300     RECORDING F                                                          
007400     BLOCK CONTAINS 0.                                                    
007500                                                                          
007600 01  FILLER                  PIC X(80).                                   
007700                                                                          
007800 FD  W27134                                                               
007900     RECORDING       F                                                    
008000     BLOCK CONTAINS  0.                                                   
008100                                                                          
008200*01  POST -COPY W27134  -PRE UT-    -L.                                   
008300                                                                          
008400 FD  W2713A                                                               
008500     RECORDING       F                                                    
008600     BLOCK CONTAINS  0.                                                   
008700                                                                          
008800*01  POST -COPY W2713A  -PRE UT2-    -L.                                  
008900                                                                          
009000     EJECT                                                                
009100 WORKING-STORAGE SECTION.                                                 
009200     SKIP2                                                                
009300*    -COPY WY2000W1                                                       
009400     SKIP2                                                                
009500*    -COPY WY2000W3                                                       
009600     SKIP3                                                                
009700*    -COPY WY2000W2                                                       
009800     SKIP3                                                                
009900*    -COPY WWPRODSL                                                       
010000     SKIP3                                                                
010100 77  IDPGM                       PIC X(8)    VALUE 'W2713400'.            
010200 77  JA                          PIC X       VALUE 'J'.                   
010300 77  NEJ                         PIC X       VALUE 'N'.                   
010400 77  AKTIV                       PIC X       VALUE 'A'.                   
010500 77  DCS-TRAEFF                  PIC X       VALUE 'N'.                   
010600                                                                          
010700*    --- INDEX SAMT MAX-INDEX                                             
010800 77  FILLER                      PIC X(16)   VALUE 'INDEX'.               
010900 77  INDX                        PIC 9(2)    VALUE ZERO.                  
011000 77  DC-INDX                     PIC 9(2)    VALUE ZERO.                  
011100 77  IX-TAB                      PIC 9(2)    VALUE ZERO.                  
011200 77  IX-DC                       PIC 9(3)    VALUE ZERO.                  
011300 77  DC-MAX                      PIC 9(3)    VALUE 200.                   
011400 77  PER-INDX                    PIC 9(2)    VALUE ZERO.                  
011500 77  TREND-INDX                  PIC 9(2)    VALUE ZERO.                  
011600 77  MAX-FSGFAKT                 PIC 9(2)    VALUE 12.                    
011700                                                                          
011800 77  VECKO-INDX                  PIC 9(2)    VALUE ZERO.                  
011900                                                                          
012000 01  TAB-RADIX                   PIC 9(2)    VALUE ZERO.                  
012100 77  MAX-TAB-RADIX               PIC 9(2)    VALUE ZERO.                  
012200                                                                          
012300 77  VV-INDX                     PIC 9(2)    VALUE ZERO.                  
012400 77  MAX-VV-INDX                 PIC 9(2)    VALUE 53.                    
012500                                                                          
012600*    --- SWITCHAR                                                         
012700 01  FILLER                      PIC X(16)   VALUE 'SWITCHAR'.            
012800                                                                          
014500 01  TREND-SW                    PIC X(5)    VALUE SPACE.                 
014600     88  INGEN-TREND                         VALUE 'INGEN'.               
014700     88  SVAG-TREND                          VALUE 'SVAG '.               
014800     88  STARK-TREND                         VALUE 'STARK'.               
014900                                                                          
015000 01  INDX-SW                     PIC X       VALUE 'N'.                   
015100     88  INDX-HITTAT                         VALUE 'J'.                   
015200                                                                          
015300 01  BEHANDLA-SW                 PIC X       VALUE 'N'.                   
015400     88  BEHANDLA                            VALUE 'J'.                   
015500                                                                          
015600 01  WS-LOCAL-SRC-PART-SW        PIC X       VALUE 'N'.                   
015700     88  LOCAL-SRC-PART                      VALUE 'J'.                   
015800                                                                          
015900 01  ARTIKEL-SW                  PIC X       VALUE 'N'.                   
016000     88  ARTIKEL-SKALL-FORAENDRAS            VALUE 'J'.                   
016100                                                                          
016200 01  ESCLOCK-SW                  PIC X       VALUE 'N'.                   
016300     88  PROGNOS-AER-LAAST                   VALUE 'J'.                   
016400     88  PROGNOS-AER-FAST                    VALUE 'A'.                   
016500                                                                          
016600 01  MANUELL-PROGNOS-SW          PIC X       VALUE 'N'.                   
016700     88  MANUELL-PROGNOS-SATT                VALUE 'J'.                   
016800                                                                          
016900*    --- ARBETSFÄLT                                                       
017000 01  FILLER                      PIC X(16)   VALUE 'ARBETSFÄLT'.          
017100 01  ARBETSFAELT.                                                         
017200     03  PERIODTABELL            OCCURS 13.                               
017300         05 TABELL-TIAARP        PIC  9(4)    VALUE ZERO.                 
017400         05 TABELL-FORSTA-TIAAVV PIC  9(2)    VALUE ZERO.                 
017500         05 TABELL-SISTA-TIAAVV  PIC  9(2)    VALUE ZERO.                 
017600         05 TABELL-KVOI        PIC S9(9)V9(1)  VALUE ZERO COMP-3.         
017700                                                                          
017800     03  SLUT-VV                 PIC 9(2)    VALUE ZERO.                  
017900     03  START-VV                PIC 9(2)    VALUE ZERO.                  
018000                                                                          
018100     03  WS-TIAAVV               PIC 9(4)    VALUE ZERO.                  
018200     03  FILLER REDEFINES WS-TIAAVV.                                      
018300         05 WS-TIAA              PIC 9(2).                                
018400         05 WS-TIVV              PIC 9(2).                                
018500                                                                          
018600     03  FOREG-TIAARP            PIC  9(4)   VALUE ZERO.                  
018700     03  FILLER REDEFINES FOREG-TIAARP.                                   
018800         05 FOREG-TIAA           PIC  9(2).                               
018900         05 FOREG-TIRP           PIC  9(2).                               
019000                                                                          
019100     03  DAGENS-TIAARP           PIC  9(4)   VALUE ZERO.                  
019200     03  FILLER REDEFINES DAGENS-TIAARP.                                  
019300         05 DAGENS-TIAA          PIC  9(2).                               
019400         05 DAGENS-TIRP          PIC  9(2).                               
019500                                                                          
019600     03  NAESTA-TIAARP           PIC  9(4)   VALUE ZERO.                  
019700     03  FILLER REDEFINES NAESTA-TIAARP.                                  
019800         05 NAESTA-TIAA          PIC  9(2).                               
019900         05 NAESTA-TIRP          PIC  9(2).                               
020000                                                                          
020100     03  SEASON-TIAARP           PIC  9(4)   VALUE ZERO.                  
020200     03  FILLER REDEFINES SEASON-TIAARP.                                  
020300         05 SEASON-TIAA          PIC  9(2).                               
020400         05 SEASON-TIRP          PIC  9(2).                               
020500                                                                          
020600     03  DAGENS-TIAAVVD          PIC  9(5)   VALUE ZERO.                  
020700     03  FILLER REDEFINES DAGENS-TIAAVVD.                                 
020800         05 DAGENS-TIAAVVD-AA    PIC  9(2).                               
020900         05 DAGENS-TIAAVVD-VV    PIC  9(2).                               
021000         05 DAGENS-TIAAVVD-D     PIC  9(1).                               
021100                                                                          
021200     03  DAGENS-TIAAVV-GRP       PIC  9(4)   VALUE ZERO.                  
021300                                                                          
021400                                                                          
021500     03  DAGENS-TIAAVVD-LAST-YEAR        PIC  9(5) VALUE ZERO.            
021600     03  FILLER REDEFINES DAGENS-TIAAVVD-LAST-YEAR.                       
021700         05 DAGENS-TIAAVVD-LAST-YEAR-AA  PIC 9(2).                        
021800         05 DAGENS-TIAAVVD-LAST-YEAR-VV  PIC 9(2).                        
021900         05 DAGENS-TIAAVVD-LAST-YEAR-D   PIC 9(1).                        
022000                                                                          
022100     03  DAGENS-TIVV             PIC  9(2)   VALUE ZERO.                  
022200                                                                          
022300     03  PUBLICERINGSDATUM       PIC  9(6)   VALUE ZERO.                  
022400                                                                          
022500     03  WS-ANTAL-VECKOR         PIC  9(2)      VALUE ZERO.               
022600     03  WS-KVVIPER              PIC  9(1)      VALUE ZERO.               
022700     03  WS-VECKO-IO             PIC S9(9)V9 VALUE ZERO COMP-3.           
022800     03  WS-TEST-TIPBREOI        PIC S9(7)   VALUE ZERO COMP-3.           
022900     03  WS-TIPBREOI-TIAARP      PIC  9(4)      VALUE ZERO.               
023000     03  WS-FORSTA-TIAAVV        PIC  9(4)      VALUE ZERO.               
023100     03  WS-SISTA-TIAAVV         PIC  9(4)      VALUE ZERO.               
023200     03  WS-DAT-TIAAVV           PIC  9(4)      VALUE ZERO.               
023300     03  WS-ONORM-OI-GRAENS-PB   PIC S9(9)V9(1) VALUE ZERO COMP-3.        
023400     03  WS-KVOI-REF-SEASON      PIC S9(9)V9(1) VALUE ZERO COMP-3.        
023500     03  WS-KVOI-REF-TOT         PIC S9(11)V9(2)                          
023600                                                VALUE ZERO COMP-3.        
023700     03  WS-NY-KVPBREOI          PIC S9(6)V9(2) VALUE ZERO COMP-3.        
023800     03  NY-KVPBREOI             PIC S9(6)V9(1) VALUE ZERO COMP-3.        
023900     03  WS-PREL-KVPBREOI        PIC S9(6)V9(2) VALUE ZERO COMP-3.        
024000     03  WS-MEDEL-KVPBREOI       PIC S9(6)V9(2) VALUE ZERO COMP-3.        
024100     03  WS-ANTAL-FAKTORER-STOERRE-NOLL PIC S9(7)          COMP-3.        
024200     03  WS-KVOTEN               PIC S9(5)V9(2) VALUE ZERO COMP-3.        
024300     03  WS-TIFINLV              PIC S9(5)      VALUE ZERO COMP-3.        
024400     03  WS-DAPUBL               PIC 9(6)       VALUE ZERO.               
024500     03  WS-VECKA-I-AKT-PERIOD   PIC  9(2)      VALUE ZERO.               
024600     03  WS-FLREFILL             PIC  X         VALUE SPACE.              
024700     03  WS-KDREFSTA             PIC  X         VALUE SPACE.              
024800     03  WS-PRIS                 PIC S9(7)V9(2) VALUE ZERO COMP-3.        
024900                                                                          
025000     03  WS-IDANSK               PIC 9(3)       VALUE ZERO.               
025100     03  FILLER REDEFINES WS-IDANSK.                                      
025200         07 WS-IDANSK-1-2        PIC 9(2).                                
025300         07 WS-IDANSK3           PIC 9(1).                                
025400                                                                          
025500     EJECT                                                                
025600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
025700 01  FILLER REDEFINES DAGENS-DATUM.                                       
025800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
025900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
026000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
026100     EJECT                                                                
026200* ARBETSFÄLT FÖR ATT KONTROLLERA PB-LSÅNING, ESC-LÅS MM.                  
026300 01  DA-DAGENS-DATUM.                                                     
026400     03  DAGENS-DATUM-20         PIC 9(2) VALUE 20.                       
026500     03  DAGENS-DATUM-6LONG      PIC 9(6).                                
026600 01  WS-DAPUBL-US.                                                        
026700     03  WS-DAPUBL-US-YEAR       PIC 9(4).                                
026800     03  FILLER                  PIC 9(4).                                
026900 01  WS2-TIFINLV                 PIC 9(5).                                
027000 01  FILLER REDEFINES WS2-TIFINLV.                                        
027100     03  WS2-TIFINLV-AAVV        PIC 9(4).                                
027200     03  FILLER                  PIC 9(1).                                
027300 01  WS3-TIFINLV                 PIC 9(6).                                
027400     EJECT                                                                
027500                                                                          
027600* INFO OM KÖRTYP(DAG) FRPN CONSTANTMEDLEM VALD AV JCL'EN                  
027700* INFON KOMMER SOM FIL D1                                                 
027800                                                                          
027900 01  W271TYP-POST.                                                        
028000     03  TYP-PARAMETER        PIC X(4).                                   
028100         88 DAY-KORNING       VALUE 'DAY '.                               
028200         88 WEEK-KORNING      VALUE 'WEEK'.                               
028300         88 ACC-KORNING       VALUE 'ACC '.                               
028400     03  FILLER               PIC X(76).                                  
028500                                                                          
028600 01  DYNAMISKA-SUBPROGRAM.                                                
028700*                                                                         
028800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
028900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
029000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
029100     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
029200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
029300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
029400     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
029500     SKIP2                                                                
029600*    --- PARAMETRAR TILL ABEND                                            
029700                                                                          
029800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
029900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
030000     SKIP2                                                                
030100 01  FELTEXT.                                                             
030200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
030300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
030400     EJECT                                                                
030500*    --- PARAMETRAR TILL DATKORT                                          
030600*                                                                         
030700 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W27134'.              
030800     SKIP2                                                                
030900 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
031000     SKIP2                                                                
031100*01  -COPY WDATKORT                                                       
031200     EJECT                                                                
031300*    --- PARAMETRAR TILL POSTSUM                                          
031400*                                                                         
031500*01  -COPY W0005   -PRE  POSTSUM-                                         
031600     EJECT                                                                
031700*    --- PARAMETRAR TILL WDATKONV                                         
031800*                                                                         
031900*01  -COPY WDATAREA                                                       
032000     EJECT                                                                
032100*01    -COPY WWDC99                                                       
032200     EJECT                                                                
032210*01    -COPY WWDC99            -PRE REF-                                  
032220     EJECT                                                                
032300*    --- PARAMETRAR TILL W009VADD                                         
032400*                                                                         
032500 01  W009VADD-AREA.                                                       
032600     03  DATUM-AAVV              PIC S9(5) COMP-3.                        
032700     03  ANTAL                   PIC S9(3) COMP-3.                        
032800*                                                                         
032900     EJECT                                                                
033000*    --- TABELL MED PROGNOSFAKTORER                                       
033100*                                                                         
033200*01  W271PROG.                                                            
033300*                                 TABELL PROGNOSFAKTORER SDC              
033400*                                 ANVÄNDS VID BERÄKNING AV N              
033500*                                 PROGNOS. RAD MOTSVARAR PER              
033600*                                                                         
033700*    TAGIT BORT COPYTEXT W271PROG EFTERSOM ALLA DC                        
033800*    ANVÄNDER SAMMA VÄRDEN                                                
033900*    ANLEDNINGEN ÄR UTÖKANDET AV ANTALET LDC'ER                           
034000*    OKTOBER 2004 / STEFAN Å                                              
034100*                                                                         
034200*                                                                         
034300*                                                                         
034400*    03 PROG-DC21-PROGFAKT.                                               
034500*       05 FILLER PIC X(09) VALUE '020025030'.                            
034600*       05 FILLER PIC X(09) VALUE '016020021'.                            
034700*       05 FILLER PIC X(09) VALUE '013015015'.                            
034800*       05 FILLER PIC X(09) VALUE '010010010'.                            
034900*       05 FILLER PIC X(09) VALUE '009009007'.                            
035000*       05 FILLER PIC X(09) VALUE '008006005'.                            
035100*       05 FILLER PIC X(09) VALUE '008004004'.                            
035200*       05 FILLER PIC X(09) VALUE '007004002'.                            
035300*       05 FILLER PIC X(09) VALUE '005003002'.                            
035400*       05 FILLER PIC X(09) VALUE '002002002'.                            
035500*       05 FILLER PIC X(09) VALUE '001001001'.                            
035600*       05 FILLER PIC X(09) VALUE '001001001'.                            
035700                                                                          
035800 01  PROGNOSFAKTORER.                                                     
035900******LÄSES IN FRÅN WDB618*************                                   
036000   03 TAB-DC-PROG-PERIOD   OCCURS 200.                                    
036100        05 TAB-PROG-DC-IDDC                PIC X(2)  VALUE SPACE.         
036200        05 TAB-PROG-DC-PERIOD          OCCURS 12.                         
036300           07 TAB-PROG-DC-PERIODTREND.                                    
036400               09 TAB-PROG-DC-NORMAL       PIC 9V9(2).                    
036500               09 TAB-PROG-DC-SVAG-TREND   PIC 9V9(2).                    
036600               09 TAB-PROG-DC-STARK-TREND  PIC 9V9(2).                    
036700                                                                          
036800     EJECT                                                                
036900   03 WS-PROG-DC-PERIOD              OCCURS 12.                           
037000      05 PROG-DC-PERIODTREND.                                             
037100          07 PROG-DC-NORMAL           PIC 9V9(2).                         
037200          07 PROG-DC-SVAG-TREND       PIC 9V9(2).                         
037300          07 PROG-DC-STARK-TREND      PIC 9V9(2).                         
037400      05 FILLER REDEFINES PROG-DC-PERIODTREND.                            
037500          07 PROG-DC-TRENDFAKT       PIC 9V9(2) OCCURS 3.                 
037600     EJECT                                                                
037700                                                                          
037800     03  WS-DC-PROGFAKT-VKA.                                              
037900        07 WS-PROGFAKT-PER    OCCURS 13.                                  
038000           09 WS-DC-PERIODTREND-VKA.                                      
038100               11 WS-DC-NORMAL-VKA       PIC 9V9(4).                      
038200               11 WS-DC-SVAG-TREND-VKA   PIC 9V9(4).                      
038300               11 WS-DC-STARK-TREND-VKA  PIC 9V9(4).                      
038400           09 FILLER REDEFINES WS-DC-PERIODTREND-VKA.                     
038500               11 WS-DC-TRENDFAKT-VKA    PIC 9V9(4) OCCURS 3.             
038600     EJECT                                                                
038700*    --- TABELL MED FAKTORER FÖR BERÄKNING AV ONORMAL OI                  
038800*                                                                         
038900*    TAGIT BORT COPYTEXT W271FSG EFTERSOM ALLA DC                         
039000*    ANVÄNDER SAMMA VÄRDEN                                                
039100*    ANLEDNINGEN ÄR UTÖKANDET AV ANTALET LDC'ER                           
039200*    OKTOBER 2004 / STEFAN Å                                              
039300*                                                                         
039400*                                                                         
039500*                                                                         
039600 01  W271FSG.                                                             
039700*                                 TABELL FAKTORER ONORMAL FÖ              
039800*                                 SÄLJNING. ANVÄNDS FÖR ATT               
039900*                                 GÖRA OM ONORMAL OI I NÅGON              
040000*                                 PERIOD VID PROGNOSBERÄKNIN              
040100*                                 GRÄNSVÄRDET ANVÄNDS I PROG              
040200*                                 BERÄKNINGEN ISTÄLLET FÖR D              
040300*                                 FAKTISKA ORDERINGÅNGEN.                 
040400*                                 (A * PROGNOS) + B                       
040500*                                                                         
040600     03 FSG-DC-PRISKLASSER.                                               
040700        05 FILLER PIC X(15) VALUE '000000050 02512'.                      
040800        05 FILLER PIC X(15) VALUE '000000250 02107'.                      
040900        05 FILLER PIC X(15) VALUE '000001000 01806'.                      
041000        05 FILLER PIC X(15) VALUE '000005000 01604'.                      
041100        05 FILLER PIC X(15) VALUE '000030000 01503'.                      
041200        05 FILLER PIC X(15) VALUE '000100000 01402'.                      
041300        05 FILLER PIC X(15) VALUE '999999999 01201'.                      
041400                                                                          
041500 01  FILLER REDEFINES W271FSG.                                            
041600        05 FSG-DC-FAKT             OCCURS 7.                              
041700           07 FSG-DC-PRARTSTD-MAX  PIC 9(7)V9(2).                         
041800           07 FILLER         PIC X.                                       
041900           07 FSG-A          PIC 9(2)V9.                                  
042000           07 FSG-B          PIC 9(2).                                    
042100     EJECT                                                                
042200 01  UT-AREA-START              PIC X(24)   VALUE                         
042300                                 'UT-AREA-START  '.                       
042400     SKIP2                                                                
042500                                                                          
042600*01  AREA -COPY W27134     -PRE UT-                                       
042700     EJECT                                                                
042800 01  UT-AREA2-START             PIC X(24)   VALUE                         
042900                                 'UT-AREA2-START '.                       
043000     SKIP2                                                                
043100                                                                          
043200*01  AREA -COPY W2713A     -PRE UT2-                                      
043300     EJECT                                                                
043400*                                                                         
043500*01    -COPY WWBYT03                                                      
043600     EJECT                                                                
043700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
043800                                                                          
043900     SKIP3                                                                
044000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
044100     SKIP3                                                                
044200 01  NYCKLAR-TILL-DLI.                                                    
044300     03  W-IDARTNR-X.                                                     
044400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
044500                                                                          
044600     03  W-IDDC-X.                                                        
044700         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
044800                                                                          
044900     03  W-IDDC-REF-X.                                                    
045000         05  W-IDDC-REF          PIC X(2)    VALUE SPACE.                 
045100                                                                          
045200     03  W-IDDC-B6-X.                                                     
045300         05  W-IDDC-B6       PIC X(2)   VALUE SPACE.                      
045400                                                                          
045500     03  W-IDLAND-X.                                                      
045600         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
045700                                                                          
045800     03  W-KDSEGKEY-X.                                                    
045900         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
046000                                                                          
046100     03  W-KDPROGOI-X.                                                    
046200         05  W-KDPROGOI          PIC X       VALUE 'R'.                   
046300*                                                                         
046400     SKIP2                                                                
046500*    --- STATUS-KOD FRÅN IMS                                              
046600 01  STATUS-WS                   PIC XX.                                  
046700     88  SEGMENT-FINNS                       VALUE '  '.                  
046800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
046900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
047000     SKIP2                                                                
047100 01  GODK-STATUSKODER.                                                    
047200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
047300     SKIP3                                                                
047400 01  SSA1                        PIC X(64).                               
047500 01  SSA2                        PIC X(64).                               
047600 01  SSA3                        PIC X(64).                               
047700     EJECT                                                                
047800*    --- IMS FUNKTIONSKODER                                               
047900*01  -COPY W0003                                                          
048000     EJECT                                                                
048100*    ---  DLI INPUT-OUTPUT AREA                                           
048200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
048300 01  DLI-IO-WDK601.                                                       
048400*    03  -COPY WDK601     -PRE WDK6-                                      
048500     EJECT                                                                
048600                                                                          
048700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
048800 01  DLI-IO-WDK611.                                                       
048900*    03  -COPY WDK611                                                     
049000     EJECT                                                                
049100                                                                          
049200                                                                          
049300 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA-K7'.        
049400     SKIP3                                                                
049500 01  DLI-IO-AREA-K7.                                                      
049600     03  IO-AREA-K7              PIC X(300)  VALUE SPACE.                 
049700     SKIP3                                                                
049800     03  WLARTS01 REDEFINES IO-AREA-K7.                                   
049900*        05  -COPY WDK701                                                 
050000     SKIP3                                                                
050100     03  WLARTS11 REDEFINES IO-AREA-K7.                                   
050200*        05  -COPY WDK711                                                 
050300     EJECT                                                                
050400                                                                          
050500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK712'.                      
050600 01  DLI-IO-WDK712.                                                       
050700*    03  -COPY WDK712                                                     
050800     EJECT                                                                
050900                                                                          
051000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK722'.                      
051100 01  DLI-IO-WDK722.                                                       
051200*    03  -COPY WDK722                                                     
051300     EJECT                                                                
051400                                                                          
051500*01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL701'.                      
051600*01  DLI-IO-WDL701.                                                       
051700*    03  -COPY WDL701                                                     
051800*    EJECT                                                                
051900                                                                          
052000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL711'.                      
052100 01  DLI-IO-WDL711.                                                       
052200*    03  -COPY WDL711                                                     
052300     EJECT                                                                
052400                                                                          
052500 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
052600 01   DLI-IO-AREA-B601.                                                   
052700*     03  -COPY WDB601                                                    
052800     EJECT                                                                
052900                                                                          
053000 01  FILLER               PIC X(16)   VALUE 'WDB618 AREA'.                
053100 01   DLI-IO-AREA-B618.                                                   
053200*     03  -COPY WDB618                                                    
053300     EJECT                                                                
053400                                                                          
053500 LINKAGE SECTION.                                                         
053600                                                                          
053700     EJECT                                                                
053800*01  -COPY W0008  -PRE WDK7-                                              
053900     05  WDK7-KEY-FB-AREA-IDARTNR       PIC S9(9) COMP-3.                 
054000     EJECT                                                                
054100*01  -COPY W0008  -PRE WDK7LART-                                          
054200     05  FILLER                  PIC X.                                   
054300     EJECT                                                                
054400*01  -COPY W0008  -PRE WDK72-                                             
054500     05  FILLER                  PIC X.                                   
054600     EJECT                                                                
054700*01  -COPY W0008  -PRE WDK6-                                              
054800     05  FILLER                  PIC X.                                   
054900     EJECT                                                                
055000*01  -COPY W0008  -PRE WDL7-                                              
055100     05  FILLER                  PIC X.                                   
055200     EJECT                                                                
055300*01  -COPY W0008      -PRE WDB6-                                          
055400     05  FILLER                  PIC X.                                   
055500     EJECT                                                                
055600 PROCEDURE DIVISION  USING WDK7-PCB WDK7LART-PCB WDK72-PCB                
055700                           WDK6-PCB WDL7-PCB     WDB6-PCB.                
055800     ENTRY 'DLITCBL' USING WDK7-PCB WDK7LART-PCB WDK72-PCB                
055900                           WDK6-PCB WDL7-PCB     WDB6-PCB.                
056000                                                                          
056100     PERFORM A-INIT                                                       
056200     PERFORM IMS-GN-WDK7                                                  
056300     PERFORM UNTIL SEGMENT-SLUT                                           
056400       EVALUATE WDK7-SEG-NAME-FB                                          
056500         WHEN 'WDK701  '                                                  
056600           MOVE WDK7-KEY-FB-AREA-IDARTNR TO W-IDARTNR                     
056700                                            BYT03-IDARTNR                 
056800         WHEN 'WDK711  '                                                  
056900                                                                          
057000            MOVE SLAG-IDDC             TO W-IDDC                          
057100                                          W-IDDC-B6                       
057200                                          WS-IDDC                         
057210            MOVE SLAG-IDDC-REF         TO REF-WS-IDDC                     
057300            PERFORM IMS-GU-WDB601                                         
057500            IF DCS-NDC                                                    
057700               PERFORM STYR                                               
057800            END-IF                                                        
058000       END-EVALUATE                                                       
058100       PERFORM IMS-GN-WDK7                                                
058200     END-PERFORM                                                          
058300                                                                          
058400     PERFORM Z-FINIT                                                      
058500                                                                          
058600     MOVE ZERO TO RETURN-CODE                                             
058700     GOBACK                                                               
058800     .                                                                    
058900     EJECT                                                                
059000                                                                          
059100                                                                          
059200 STYR SECTION.                                                            
059300                                                                          
059400     MOVE NEJ          TO WS-LOCAL-SRC-PART-SW                            
059500     IF ACC-KORNING                                                       
059600     OR WEEK-KORNING                                                      
059700        MOVE SLAG-KDREFSTA  TO WS-KDREFSTA                                
059800**FLYTTAR A TILL WS-KDREFSTA FÖR ATT PB SKALL ALLTID RÄKNAS               
059900**OM FÖR LOKALT ANSKAFFADE ARTIKLAR                                       
060000        IF (DCS-NDC-CN AND SLAG-IDDC-REF = SPACE)                         
060100        OR (DCS-USA AND SLAG-IDDC-REF = SPACE)                            
060200          MOVE 'A'           TO WS-KDREFSTA                               
060300        END-IF                                                            
060400        PERFORM IMS-GU-WDK6-WDK601                                        
060500        IF WDK6-ART-KDERS-UTG = 0                                         
060600           IF SLAG-IDDC-REF      = '11'                                   
060700             MOVE WDK6-ART-TIFINLV TO WS-TIFINLV                          
060800           ELSE                                                           
060900             MOVE NEJ       TO BEHANDLA-SW                                
061000             MOVE DCS-IDLANDX2          TO W-IDLAND                       
061100             PERFORM IMS-GU-WDK712                                        
061200             IF SEGMENT-FINNS                                             
061300               IF LART-TIERSDAT-VIPS > 0                                  
061400                 IF DCS-CHINA                                             
061500                 OR DCS-USA                                               
061600                   MOVE JA TO BEHANDLA-SW                                 
061700                 END-IF                                                   
061800               END-IF                                                     
061900               IF LART-DAPUBL = ZERO                                      
062000                 MOVE WDK6-ART-TIFINLV TO WS-TIFINLV                      
062100               ELSE                                                       
062200                 MOVE LART-DAPUBL (3:6) TO WS-DAPUBL                      
062300                 MOVE 'AAMMDD' TO DAT-KDDATFORM                           
062400                 MOVE WS-DAPUBL  TO DAT-I-TIDATUM                         
062500                                                                          
062600                 CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM          
062700                                     DAT-O-TIDATUM DAT-KDSVAR             
062800                                                                          
062900                 IF DAT-KDSVAR-OK                                         
063000                   MOVE DAT-TIAAVVD TO WS-TIFINLV                         
063100                 ELSE                                                     
063200                   MOVE 'SVAR FRÅN WDATKONV I STYR SECTION EJ OK'         
063300                                     TO FELTEXT-STR                       
063400                   DISPLAY FELTEXT                                        
063500                   PERFORM S99-ABEND                                      
063600                 END-IF                                                   
063700               END-IF                                                     
063800               MOVE LART-PRMATRL      TO WS-PRIS                          
063900             ELSE                                                         
064000               MOVE WDK6-ART-TIFINLV TO WS-TIFINLV                        
064100             END-IF                                                       
064200           END-IF                                                         
064300           PERFORM IMS-GNP-WDK6-WDK611                                    
064400*                                                                         
064500*                                                                         
064600           IF (CLAG-KDERS < 20)                                           
064700           OR BEHANDLA                                                    
065220             IF DCS-NDC                                                   
065230               IF REF-NDC                                                 
065300                 MOVE SLAG-IDDC-REF TO W-IDDC                             
065400                 MOVE SLAG-FLREFILL TO WS-FLREFILL                        
065500               ELSE                                                       
065600                 IF (SLAG-IDDC-REF = SPACE)                               
065700                   MOVE JA      TO WS-FLREFILL                            
065800                                   WS-LOCAL-SRC-PART-SW                   
065900                 ELSE                                                     
066000                   MOVE CLAG-FLREFILL TO WS-FLREFILL                      
066100                 END-IF                                                   
066200               END-IF                                                     
066300             ELSE                                                         
066400               MOVE CLAG-FLREFILL TO WS-FLREFILL                          
066500             END-IF                                                       
066600                                                                          
066700             MOVE WDK6-ART-KDPRODSL                                       
066800                                 TO TEST-KDPRODSL                         
066900             IF WS-FLREFILL = JA                                          
067000*                                                                         
067100* NEDANSTÅENDE GÄLLER BIMA ARTIKLAR                                       
067200*                                                                         
067300             OR  (DCS-FLEXCP1-REFBER = JA                                 
067400             AND KDPRODSL-BIMA                                            
067500             AND NOT (BYT03-OBJEKT                                        
067600             OR       WDK6-ART-KDSORT = 'SW'                              
067700             OR       CLAG-FLLSRDEL = NEJ))                               
067800               IF  WS-KDREFSTA = AKTIV                                    
067900                 PERFORM C-BEHANDLA-ARTIKEL                               
068000               END-IF                                                     
068100             END-IF                                                       
068200           ELSE                                                           
068300             IF ACC-KORNING                                               
068400               IF (SLAG-KVLS + SLAG-KVAKS-PAV) > ZERO                     
068500                 CONTINUE                                                 
068600               ELSE                                                       
068700                 PERFORM D-NOLLA-PB-MFL                                   
068800               END-IF                                                     
068900             END-IF                                                       
069000           END-IF                                                         
069100        END-IF                                                            
069200     END-IF                                                               
069300     .                                                                    
069400     EJECT                                                                
069500                                                                          
069600                                                                          
069700 A-INIT SECTION.                                                          
069800                                                                          
069900     OPEN INPUT  W271TYP                                                  
070000                 W271DC                                                   
070100     OPEN OUTPUT W27134                                                   
070200                 W2713A                                                   
070300                                                                          
070900     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
071000     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
071100     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
071200     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
071300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
071400                                                                          
071500     MOVE 'AAMMDD'     TO DAT-KDDATFORM                                   
071600     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
071700     MOVE DAGENS-DATUM TO DAGENS-DATUM-6LONG                              
071800                                                                          
071900     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
072000                         DAT-O-TIDATUM DAT-KDSVAR                         
072100                                                                          
072200     IF DAT-KDSVAR-OK                                                     
072300       MOVE DAT-TIAARP     TO DAGENS-TIAARP                               
072400                              FOREG-TIAARP                                
072500                              NAESTA-TIAARP                               
072600       MOVE DAT-TIVV       TO DAGENS-TIVV                                 
072700       MOVE DAT-TIAAVV-GRP TO DAGENS-TIAAVV-GRP                           
072800       MOVE DAT-TIAAVVD    TO DAGENS-TIAAVVD                              
072900     ELSE                                                                 
073000       MOVE 'SVAR 1 FRÅN WDATKONV I A SECTION EJ OK'                      
073100                         TO FELTEXT-STR                                   
073200       DISPLAY FELTEXT                                                    
073300       PERFORM S99-ABEND                                                  
073400     END-IF                                                               
073500                                                                          
073600                                                                          
073700*    DAGENS DATUM ETT ÅR TILLBAKA                                         
073800                                                                          
073900     MOVE DAGENS-TIAAVVD   TO DAGENS-TIAAVVD-LAST-YEAR                    
074000     IF DAGENS-TIAAVVD-LAST-YEAR-AA = 00                                  
074100       MOVE 99      TO DAGENS-TIAAVVD-LAST-YEAR-AA                        
074200     ELSE                                                                 
074300       SUBTRACT +1  FROM DAGENS-TIAAVVD-LAST-YEAR-AA                      
074400     END-IF                                                               
074500                                                                          
074600     PERFORM AZ-VECKO-ELLER-PERIOD-KORNING                                
074700                                                                          
074800     IF ACC-KORNING                                                       
074900       MOVE 12               TO MAX-TAB-RADIX                             
075000     ELSE                                                                 
075100       MOVE 13               TO MAX-TAB-RADIX                             
075200     END-IF                                                               
075300                                                                          
075400     PERFORM AA-INITERA-PERIODTABELL                                      
075500                                                                          
075600*    IF ACC-KORNING                                                       
075700*      CONTINUE                                                           
075800*    ELSE                                                                 
075900       PERFORM AB-PROGNOS-TABELL                                          
076000*    END-IF                                                               
076100     .                                                                    
076200     EJECT                                                                
076300 AA-INITERA-PERIODTABELL SECTION.                                         
076400*--------------------------------------------------------------*          
076500* HÄR INITIERAS PERIODTABELLEN. INDX 1 MOTSVARAR INNEVARANDE   *          
076600* PERIOD, INDX 2 PERIODEN INNAN OSV.                           *          
076700* PERIODERNA ÄR REDOVISNINGSPERIODER. TABELLEN INITIERAS MED   *          
076800* PER.NR + START/SLUTVECKA FÖR ATT KUNNA UTFÖRA SUMMERINGAR    *          
076900*--------------------------------------------------------------*          
077000                                                                          
077100     MOVE 1 TO TAB-RADIX                                                  
077200     PERFORM AAA-TA-HAND-OM-INNEV-PERIOD                                  
077300     ADD +1 TO TAB-RADIX                                                  
077400     PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                              
077500*---TA REDA PÅ FÖREGÅENDE PERIOD                                          
077600       COMPUTE FOREG-TIRP = FOREG-TIRP - 1                                
077700       IF FOREG-TIRP = +0                                                 
077800         IF FOREG-TIAA = 00                                               
077900           MOVE 99      TO   FOREG-TIAA                                   
078000         ELSE                                                             
078100           SUBTRACT  +1 FROM FOREG-TIAA                                   
078200         END-IF                                                           
078300         MOVE +12     TO   FOREG-TIRP                                     
078400       END-IF                                                             
078500       MOVE FOREG-TIAARP TO TABELL-TIAARP(TAB-RADIX)                      
078600                                                                          
078700*---BEHANDLAD PERIODS START-VECKA - 1 = FÖREGÅENDE PER. SLUT-VECKA        
078800      IF START-VV > +1                                                    
078900        COMPUTE SLUT-VV = START-VV - +1                                   
079000        MOVE SLUT-VV TO TABELL-SISTA-TIAAVV(TAB-RADIX)                    
079100      ELSE                                                                
079200        MOVE FOREG-TIAA      TO WS-TIAA                                   
079300        PERFORM AAB-KOLLA-ANTAL-VECKOR                                    
079400        MOVE SLUT-VV TO TABELL-SISTA-TIAAVV(TAB-RADIX)                    
079500      END-IF                                                              
079600                                                                          
079700*--- TA REDA PÅ START-VECKA                                               
079800                                                                          
079900       MOVE FOREG-TIAARP TO DAT-I-TIDATUM                                 
080000       MOVE 'AARP'       TO DAT-KDDATFORM                                 
080100                                                                          
080200       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
080300                           DAT-O-TIDATUM DAT-KDSVAR                       
080400                                                                          
080500       IF DAT-KDSVAR-OK                                                   
080600*--- START-VECKA ÄR ALLTID VECKA 1 PÅ NYTT ÅR                             
080700         IF DAT-TIVV = +52 OR +53                                         
080800           MOVE +1 TO START-VV                                            
080900                      TABELL-FORSTA-TIAAVV(TAB-RADIX)                     
081000         ELSE                                                             
081100           MOVE DAT-TIVV   TO START-VV                                    
081200                              TABELL-FORSTA-TIAAVV(TAB-RADIX)             
081300         END-IF                                                           
081400       ELSE                                                               
081500         MOVE 'SVAR 2 FRÅN WDATKONV EJ OK' TO FELTEXT-STR                 
081600         DISPLAY FELTEXT                                                  
081700         PERFORM S99-ABEND                                                
081800       END-IF                                                             
081900       MOVE DAT-TIAARP TO TABELL-TIAARP(TAB-RADIX)                        
082000       MOVE ZERO       TO TABELL-KVOI(TAB-RADIX)                          
082100       ADD +1 TO TAB-RADIX                                                
082200     END-PERFORM                                                          
082300     .                                                                    
082400     EJECT                                                                
082500 AAA-TA-HAND-OM-INNEV-PERIOD SECTION.                                     
082600                                                                          
082700*--- KOLLA START-VECKA                                                    
082800                                                                          
082900     MOVE 'AARP'        TO DAT-KDDATFORM                                  
083000     MOVE DAGENS-TIAARP TO DAT-I-TIDATUM                                  
083100                                                                          
083200     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
083300                         DAT-O-TIDATUM DAT-KDSVAR                         
083400                                                                          
083500     IF DAT-KDSVAR-OK                                                     
083600*--- START-VECKA ÄR ALLTID VECKA 1 PÅ NYTT ÅR                             
083700       IF DAT-TIVV = +52 OR +53                                           
083800         MOVE +1 TO START-VV                                              
083900                                                                          
084000         IF DAGENS-TIVV = 52 OR 53                                        
084100           MOVE 1            TO WS-VECKA-I-AKT-PERIOD                     
084200         END-IF                                                           
084300         IF DAGENS-TIVV = 1                                               
084400           MOVE 2            TO WS-VECKA-I-AKT-PERIOD                     
084500         END-IF                                                           
084600         IF DAGENS-TIVV = 2                                               
084700           MOVE 3            TO WS-VECKA-I-AKT-PERIOD                     
084800         END-IF                                                           
084900         IF DAGENS-TIVV = 3                                               
085000           MOVE 4            TO WS-VECKA-I-AKT-PERIOD                     
085100         END-IF                                                           
085200         IF DAGENS-TIVV = 4                                               
085300           MOVE 5            TO WS-VECKA-I-AKT-PERIOD                     
085400         END-IF                                                           
085500       ELSE                                                               
085600         MOVE DAT-TIVV   TO START-VV                                      
085700                                                                          
085800         COMPUTE WS-VECKA-I-AKT-PERIOD =                                  
085900            DAGENS-TIVV - START-VV + 1                                    
086000       END-IF                                                             
086100     ELSE                                                                 
086200       MOVE 'SVAR 3 FRÅN WDATKONV EJ OK' TO FELTEXT-STR                   
086300       DISPLAY FELTEXT                                                    
086400       PERFORM S99-ABEND                                                  
086500     END-IF                                                               
086600                                                                          
086700*--- KOLLA SLUT-VECKA, NÄSTA PERIODS START-VECKA - 1                      
086800                                                                          
086900     COMPUTE NAESTA-TIRP = DAGENS-TIRP + 1                                
087000     IF NAESTA-TIRP = +13                                                 
087100       ADD  +1 TO NAESTA-TIAA                                             
087200       MOVE +1 TO NAESTA-TIRP                                             
087300     END-IF                                                               
087400                                                                          
087500     MOVE 'AARP'        TO DAT-KDDATFORM                                  
087600     MOVE NAESTA-TIAARP TO DAT-I-TIDATUM                                  
087700                                                                          
087800     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
087900                         DAT-O-TIDATUM DAT-KDSVAR                         
088000                                                                          
088100     IF DAT-KDSVAR-OK                                                     
088200       MOVE DAT-TIVV   TO SLUT-VV                                         
088300     ELSE                                                                 
088400       MOVE 'SVAR 4 FRÅN WDATKONV EJ OK' TO FELTEXT-STR                   
088500       DISPLAY FELTEXT                                                    
088600       PERFORM S99-ABEND                                                  
088700     END-IF                                                               
088800                                                                          
088900     IF SLUT-VV = +1                                                      
089000       MOVE DAGENS-TIAA  TO WS-TIAA                                       
089100       PERFORM AAB-KOLLA-ANTAL-VECKOR                                     
089200     ELSE                                                                 
089300       COMPUTE SLUT-VV = SLUT-VV - +1                                     
089400     END-IF                                                               
089500                                                                          
089600     MOVE DAGENS-TIAARP TO TABELL-TIAARP(TAB-RADIX)                       
089700     MOVE START-VV      TO TABELL-FORSTA-TIAAVV(TAB-RADIX)                
089800     MOVE SLUT-VV       TO TABELL-SISTA-TIAAVV(TAB-RADIX)                 
089900     MOVE ZERO          TO TABELL-KVOI(TAB-RADIX)                         
090000                                                                          
090100     .                                                                    
090200     EJECT                                                                
090300 AAB-KOLLA-ANTAL-VECKOR SECTION.                                          
090400                                                                          
090500* --- TAG REDA PÅ OM DET ÄR 52 ELLER 53 VECKOR PÅ ÅRET                    
090600                                                                          
090700     MOVE 53        TO WS-TIVV                                            
090800     MOVE WS-TIAAVV TO DAT-I-TIDATUM                                      
090900     MOVE 'AAVV  '  TO DAT-KDDATFORM                                      
091000     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
091100                         DAT-O-TIDATUM DAT-KDSVAR                         
091200     IF DAT-KDSVAR-OK                                                     
091300       MOVE 53 TO SLUT-VV                                                 
091400     ELSE                                                                 
091500       MOVE 52 TO SLUT-VV                                                 
091600     END-IF                                                               
091700     .                                                                    
091800     EJECT                                                                
091900                                                                          
092000                                                                          
092100 AB-PROGNOS-TABELL SECTION.                                               
092200                                                                          
092300     MOVE DAT-KVVIPER    TO WS-KVVIPER                                    
092400                                                                          
092500     MOVE +1             TO IX-DC                                         
092600     PERFORM IMS-GN-WDB601                                                
092700     PERFORM UNTIL SEGMENT-SLUT                                           
092800       PERFORM IMS-GNP-WDB618                                             
092900       IF SEGMENT-FINNS                                                   
093000         MOVE DCS-IDDC    TO TAB-PROG-DC-IDDC (IX-DC)                     
093100         MOVE +1    TO IX-TAB                                             
093200         PERFORM UNTIL IX-TAB > 12                                        
093300           MOVE PROG-REFTREND-NORM (IX-TAB)                               
093400                           TO TAB-PROG-DC-NORMAL (IX-DC, IX-TAB)          
093500           MOVE PROG-REFTREND-SVAG (IX-TAB)                               
093600                        TO TAB-PROG-DC-SVAG-TREND (IX-DC, IX-TAB)         
093700           MOVE PROG-REFTREND-STARK (IX-TAB)                              
093800                        TO TAB-PROG-DC-STARK-TREND (IX-DC, IX-TAB)        
093900           ADD +1   TO IX-TAB                                             
094000         END-PERFORM                                                      
094100         ADD +1         TO IX-DC                                          
094200       END-IF                                                             
094300       PERFORM IMS-GN-WDB601                                              
094400     END-PERFORM                                                          
094500     .                                                                    
094600     EJECT                                                                
094700                                                                          
094800 AZ-VECKO-ELLER-PERIOD-KORNING SECTION.                                   
094900                                                                          
095000     PERFORM S11-LAES-W271TYP                                             
095100                                                                          
095200     IF WEEK-KORNING                                                      
095300        DISPLAY 'VECKOKÖRNING, EJ PERIODSLUT'                             
095400     ELSE                                                                 
095500        IF ACC-KORNING                                                    
095600           DISPLAY 'ACC-PERIODKÖRNING'                                    
095700        END-IF                                                            
095800     END-IF                                                               
095900     .                                                                    
096000     EJECT                                                                
096100                                                                          
096200                                                                          
096300 C-BEHANDLA-ARTIKEL SECTION.                                              
096400                                                                          
096500     MOVE NEJ TO ARTIKEL-SW                                               
096600     MOVE SLAG-IDDC  TO W-IDDC                                            
096700     PERFORM IMS-GU-WDL711                                                
096800     IF SEGMENT-FINNS                                                     
097210        IF SLAG-IDDC-REF = '11'                                           
097300          MOVE CLAG-FLREFILL TO WS-FLREFILL                               
097400        ELSE                                                              
097500          MOVE SLAG-FLREFILL TO WS-FLREFILL                               
097600        END-IF                                                            
097700*                                                                         
097800        IF WS-FLREFILL = JA                                               
097900        OR LOCAL-SRC-PART                                                 
098000           PERFORM CA-UPPDATERA-PERIODTABELL                              
098100                                                                          
098200           PERFORM CB-UTFOR-BERAKNINGAR                                   
098300                                                                          
098400           IF ARTIKEL-SKALL-FORAENDRAS                                    
098500             IF (NY-KVPBREOI > SLAG-KVPBREOI                              
098600                                     + WS-ONORM-OI-GRAENS-PB              
098700             OR  NY-KVPBREOI < SLAG-KVPBREOI                              
098800                                     - WS-ONORM-OI-GRAENS-PB)             
098900*-----                                                                    
099000*----- SKRIV POST PÅ LARMFIL                                              
099100*----- ONORMAL FÖRÄNDRING, KRÄVER MANUELL BEHANDLING                      
099200*-----                                                                    
099300               MOVE W-IDARTNR          TO UT2-IDARTNR                     
099400               MOVE SLAG-IDDC          TO UT2-IDDC                        
099500               MOVE SLAG-IDDC-REF      TO UT2-IDDC-REF                    
099600               MOVE SLAG-KVPBREOI      TO UT2-KVPBREOI                    
099700               MOVE NY-KVPBREOI        TO UT2-NY-KVPBREOI                 
099800               MOVE SPACE              TO UT2-BEART                       
099900               PERFORM IMS-GU-WDK722                                      
100000               IF  SEGMENT-FINNS                                          
100100               AND XLAG-IDANSK          > ZERO                            
100200                   MOVE XLAG-IDANSK    TO WS-IDANSK                       
100300                   MOVE ZERO           TO WS-IDANSK3                      
100400                   MOVE WS-IDANSK      TO UT2-IDANSK                      
100500               ELSE                                                       
100600                   MOVE ZERO           TO UT2-IDANSK                      
100700               END-IF                                                     
100800               MOVE SLAG-IDPERSON-BUY  TO UT2-IDPERSON-BUY                
100900               PERFORM S13-SKRIV-W2713A                                   
101000                                                                          
101100             ELSE                                                         
101200*----- SKRIV POST PÅ ARTIKELFIL                                           
101300               MOVE W-IDARTNR          TO UT-IDARTNR                      
101400               MOVE SLAG-IDDC          TO UT-IDDC                         
101500               MOVE NY-KVPBREOI        TO UT-KVPBREOI                     
101600               MOVE WS-KVOTEN          TO UT-RETREND-REOI                 
101700               MOVE CLAG-KDERS         TO UT-KDERS                        
101800               PERFORM S12-SKRIV-W27134                                   
101900             END-IF                                                       
102000           END-IF                                                         
102100        END-IF                                                            
102200     END-IF                                                               
102300     .                                                                    
102400     EJECT                                                                
102500 CA-UPPDATERA-PERIODTABELL SECTION.                                       
102600                                                                          
102700     PERFORM CAA-NOLLSTAELL-FAELT                                         
102800     PERFORM CAB-SUMMERA-PERIODTABELL                                     
102900     PERFORM CAC-JUSTERA-OI                                               
103000     .                                                                    
103100     EJECT                                                                
103200 CAA-NOLLSTAELL-FAELT SECTION.                                            
103300                                                                          
103400*---  NOLLSTÄLL KVOI I PERIODTABELLEN                                     
103500                                                                          
103600     MOVE 1 TO TAB-RADIX                                                  
103700     PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                              
103800       MOVE ZERO TO TABELL-KVOI (TAB-RADIX)                               
103900       ADD 1 TO TAB-RADIX                                                 
104000     END-PERFORM                                                          
104100                                                                          
104200*---  NOLLSTÄLL ARBETSFÄLT                                                
104300     MOVE ZERO TO WS-TIPBREOI-TIAARP                                      
104400                  WS-TEST-TIPBREOI                                        
104500                  WS-ONORM-OI-GRAENS-PB                                   
104600                  WS-KVOI-REF-TOT                                         
104700                  WS-NY-KVPBREOI                                          
104800                  NY-KVPBREOI                                             
104900                  WS-PREL-KVPBREOI                                        
105000                  WS-MEDEL-KVPBREOI                                       
105100                  WS-ANTAL-FAKTORER-STOERRE-NOLL                          
105200                  WS-KVOTEN                                               
105300                                                                          
105400     .                                                                    
105500     EJECT                                                                
105600 CAB-SUMMERA-PERIODTABELL SECTION.                                        
105700*---------------------------------------------------------------*         
105800* HÄR SUMMERAS DE VECKOR IN I RÄTT PERIOD                       *         
105900* EV. NEGATIV OI ÄNDRAS TILL NOLL.                              *         
106000*---------------------------------------------------------------*         
106100                                                                          
106200*--- SUMMERA FÖRST VECKOR FRÅN INNEV. PERIOD                              
106300                                                                          
106400     MOVE +5   TO INDX                                                    
106500     MOVE +1   TO TAB-RADIX                                               
106600     PERFORM UNTIL INDX = 0                                               
106700       IF DC-TIVV(INDX) > 0 AND <= DAGENS-TIVV                            
106800         ADD DC-KVOI-REF-INNEV(INDX) TO TABELL-KVOI(TAB-RADIX)            
106900       END-IF                                                             
107000       SUBTRACT 1 FROM INDX                                               
107100     END-PERFORM                                                          
107200                                                                          
107300*--- SUMMERA IN RESTERANDE PERIODER                                       
107400                                                                          
107500     ADD +1 TO TAB-RADIX                                                  
107600     PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                              
107700       MOVE TABELL-SISTA-TIAAVV(TAB-RADIX) TO VECKO-INDX                  
107800       PERFORM UNTIL TABELL-FORSTA-TIAAVV(TAB-RADIX) = VECKO-INDX         
107900         ADD DC-KVOI-REF-RULL(VECKO-INDX) TO                              
108000                                          TABELL-KVOI(TAB-RADIX)          
108100         IF VECKO-INDX > +1                                               
108200           SUBTRACT +1 FROM VECKO-INDX                                    
108300         ELSE                                                             
108400                                                                          
108500*--- KOLLA OM 52 ELLER 53 VECKOR PÅ ÅRET                                  
108600                                                                          
108700           MOVE TABELL-TIAARP(TAB-RADIX) TO FOREG-TIAARP                  
108800           IF FOREG-TIAA = 00                                             
108900             MOVE 99 TO FOREG-TIAA                                        
109000           ELSE                                                           
109100             SUBTRACT +1 FROM  FOREG-TIAA                                 
109200           END-IF                                                         
109300           MOVE FOREG-TIAA   TO WS-TIAA                                   
109400           MOVE 53        TO WS-TIVV                                      
109500           MOVE WS-TIAAVV TO DAT-I-TIDATUM                                
109600           MOVE 'AAVV  '  TO DAT-KDDATFORM                                
109700           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
109800                               DAT-O-TIDATUM DAT-KDSVAR                   
109900           IF DAT-KDSVAR-OK                                               
110000             MOVE 53 TO VECKO-INDX                                        
110100           ELSE                                                           
110200             MOVE 52 TO VECKO-INDX                                        
110300           END-IF                                                         
110400         END-IF                                                           
110500       END-PERFORM                                                        
110600                                                                          
110700*--- ADDERA DEN SISTA VECKAN OCKSÅ                                        
110800                                                                          
110900       ADD DC-KVOI-REF-RULL(VECKO-INDX) TO                                
111000                                    TABELL-KVOI(TAB-RADIX)                
111100       ADD +1 TO TAB-RADIX                                                
111200     END-PERFORM                                                          
111300                                                                          
111400*--- KOLLA OM EV. NEGATIV OI I TABELLEN - ÄNDRA TILL 0                    
111500                                                                          
111600     MOVE +1 TO TAB-RADIX                                                 
111700     PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                              
111800       IF TABELL-KVOI(TAB-RADIX) < 0                                      
111900         MOVE ZERO TO TABELL-KVOI(TAB-RADIX)                              
112000       END-IF                                                             
112100       ADD +1 TO TAB-RADIX                                                
112200     END-PERFORM                                                          
112300                                                                          
112400     .                                                                    
112500     EJECT                                                                
112600 CAC-JUSTERA-OI SECTION.                                                  
112700*---------------------------------------------------------------*         
112800* HÄR JUSTERAS OI BEROENDE PÅ OM DET ÄR 4 ELLER 5 VECKOR        *         
112900* I PERIODEN. GENOMSNITTLIG OI PER VECKA RÄKNAS UT OCH          *         
113000* MULTIPLICERAS MED 4.33, EG 52/12 (VECKOR/MÅNADER)             *         
113100*---------------------------------------------------------------*         
113200                                                                          
113300     IF ACC-KORNING                                                       
113400                                                                          
113500       MOVE +1 TO TAB-RADIX                                               
113600                                                                          
113700     ELSE                                                                 
113800                                                                          
113900       COMPUTE WS-VECKO-IO ROUNDED =                                      
114000               TABELL-KVOI(1) / WS-VECKA-I-AKT-PERIOD                     
114100       COMPUTE TABELL-KVOI(1) = WS-VECKO-IO * 4.33                        
114200                                                                          
114300       MOVE +2 TO TAB-RADIX                                               
114400                                                                          
114500     END-IF                                                               
114600                                                                          
114700     PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                              
114800                                                                          
114900       MOVE TABELL-TIAARP(TAB-RADIX) TO DAT-I-TIDATUM                     
115000       MOVE 'AARP  '  TO DAT-KDDATFORM                                    
115100       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
115200                           DAT-O-TIDATUM DAT-KDSVAR                       
115300                                                                          
115400       IF DAT-KDSVAR-OK                                                   
115500         MOVE DAT-KVVIPER TO WS-ANTAL-VECKOR                              
115600       ELSE                                                               
115700         MOVE 'SVAR 6 FRÅN WDATKONV EJ OK' TO FELTEXT-STR                 
115800         DISPLAY FELTEXT                                                  
115900         PERFORM S99-ABEND                                                
116000       END-IF                                                             
116100                                                                          
116200       COMPUTE WS-VECKO-IO =                                              
116300                       TABELL-KVOI(TAB-RADIX) / WS-ANTAL-VECKOR           
116400       COMPUTE TABELL-KVOI(TAB-RADIX) = WS-VECKO-IO * 4.33                
116500                                                                          
116600       ADD +1 TO TAB-RADIX                                                
116700     END-PERFORM                                                          
116800     .                                                                    
116900     EJECT                                                                
117000                                                                          
117100 CB-UTFOR-BERAKNINGAR SECTION.                                            
117200                                                                          
117300     PERFORM S01-KONTROLLERA-BERAEKNA-PROGN                               
117400     .                                                                    
117500     EJECT                                                                
117600                                                                          
117700 D-NOLLA-PB-MFL SECTION.                                                  
117800                                                                          
117900     MOVE W-IDARTNR            TO UT-IDARTNR                              
118000     MOVE SLAG-IDDC            TO UT-IDDC                                 
118100     MOVE ZERO                 TO UT-KVPBREOI                             
118200                                  UT-RETREND-REOI                         
118300     MOVE CLAG-KDERS           TO UT-KDERS                                
118400     PERFORM S12-SKRIV-W27134                                             
118500     .                                                                    
118600     EJECT                                                                
118700                                                                          
118800                                                                          
118900 Z-FINIT SECTION.                                                         
119000                                                                          
119100     CLOSE W271TYP                                                        
119200           W271DC                                                         
119300           W27134                                                         
119400           W2713A                                                         
119500                                                                          
119600     MOVE 'S' TO POSTSUM-OPKOD                                            
119700     CALL POSTSUM USING POSTSUM-PARM                                      
119800     .                                                                    
119900     EJECT                                                                
120000 S01-KONTROLLERA-BERAEKNA-PROGN SECTION.                                  
120100                                                                          
120200     PERFORM S01A-KOLLA-DATUM-MANUELL-PROGN                               
120300                                                                          
120400*  ÄT LOCK FORECAST ON NEW PARTS 010506  JOHAN L    ESC-LOCK              
120500     PERFORM S01E-KOLLA-PROGNOS-LAASNING                                  
120600*                                                                         
120700     MOVE SLAG-TIPBREOI      TO TMP1-YYMMDD                               
120800     MOVE DAGENS-DATUM       TO TMP2-YYMMDD                               
120900     PERFORM WY2000P1                                                     
121000     IF (MANUELL-PROGNOS-SATT                                             
121100     AND (WS-TIPBREOI-TIAARP = 1                                          
121200     OR   WS-TIPBREOI-TIAARP = 2)                                         
121300     OR  TMP1-YYMMDD > TMP2-YYMMDD)                                       
121400* IF MANUAL FC IS SET IT SHOULD NOT BE CHANGED:                           
121500* 1. IF MANUAL SET FC DATE(TIPBREOI) IS IN CURRENT OR PREVIOUS            
121600*    PERIOD                                                               
121700* 2. IF PUBLICATION DATE(DAPUBL/TIFINLV) IS IN CURRENT OR PREVIOUS        
121800*    PERIOD                                                               
121900* 3. TIPBREOI IS IN THE FUTURE                                            
122000       CONTINUE                                                           
122100     ELSE                                                                 
122200*  INGEN PROGNOSFÖRÄNDRING OM TIPBREOI INTE ÄR PASSERAT !!!               
122300        IF PROGNOS-AER-FAST                                               
122400          CONTINUE                                                        
122500        ELSE                                                              
122600          PERFORM S01B-KOLLA-OI                                           
122700          PERFORM S01C-KOLLA-MANUELL-PROGNOS                              
122800          PERFORM S01D-BERAEKNA-NY-PROGNOS                                
122900        END-IF                                                            
123000     END-IF                                                               
123100     .                                                                    
123200     EJECT                                                                
123300 S01A-KOLLA-DATUM-MANUELL-PROGN SECTION.                                  
123400                                                                          
123500     IF SLAG-TIPBREOI > 0                                                 
123600        MOVE SLAG-TIPBREOI   TO TMP1-YYMMDD                               
123700        MOVE 'AAVVD'         TO DAT-KDDATFORM                             
123800        MOVE WS-TIFINLV      TO DAT-I-TIDATUM                             
123900                                                                          
124000        CALL WDATKONV USING DAT-KDDATFORM                                 
124100                            DAT-I-TIDATUM                                 
124200                            DAT-O-TIDATUM                                 
124300                            DAT-KDSVAR                                    
124400                                                                          
124500        IF DAT-KDSVAR-OK                                                  
124600          MOVE DAT-TIAAMMDD  TO TMP2-YYMMDD                               
124700        ELSE                                                              
124800          MOVE 'SVAR WDATKONV   I S01A SECTION'                           
124900                                TO FELTEXT-STR                            
125000          DISPLAY FELTEXT                                                 
125100          PERFORM S99-ABEND                                               
125200        END-IF                                                            
125300        PERFORM WY2000P1                                                  
125400        IF TMP1-YYMMDD > TMP2-YYMMDD                                      
125500           MOVE SLAG-TIPBREOI                                             
125600                             TO WS-TEST-TIPBREOI                          
125700        ELSE                                                              
125800           MOVE DAT-TIAAMMDD TO WS-TEST-TIPBREOI                          
125900        END-IF                                                            
126000     ELSE                                                                 
126100        MOVE WS-TIFINLV                 TO TMP1-YYWWD                     
126200        MOVE DAGENS-TIAAVVD-LAST-YEAR   TO TMP2-YYWWD                     
126300        PERFORM WY2000P2                                                  
126400        IF TMP1-YYWWD >= TMP2-YYWWD                                       
126500           MOVE 'AAVVD'               TO DAT-KDDATFORM                    
126600           MOVE WS-TIFINLV            TO DAT-I-TIDATUM                    
126700                                                                          
126800           CALL WDATKONV USING DAT-KDDATFORM                              
126900                               DAT-I-TIDATUM                              
127000                               DAT-O-TIDATUM                              
127100                               DAT-KDSVAR                                 
127200                                                                          
127300           IF DAT-KDSVAR-OK                                               
127400             MOVE DAT-TIAAMMDD        TO WS-TEST-TIPBREOI                 
127500           ELSE                                                           
127600             MOVE 'SVAR WDATKONV   I S01A SECTION'                        
127700                                   TO FELTEXT-STR                         
127800             DISPLAY FELTEXT                                              
127900             PERFORM S99-ABEND                                            
128000           END-IF                                                         
128100        END-IF                                                            
128200     END-IF                                                               
128300                                                                          
128400     IF WS-TEST-TIPBREOI > 0                                              
128500*----- KONTROLLERA VILKEN VECKA TIPBREOI LIGGER I                         
128600       MOVE 'AAMMDD'      TO DAT-KDDATFORM                                
128700       MOVE WS-TEST-TIPBREOI TO DAT-I-TIDATUM                             
128800                                                                          
128900       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
129000                           DAT-O-TIDATUM DAT-KDSVAR                       
129100       MOVE DAT-TIAAVV-GRP    TO WS-DAT-TIAAVV                            
129200                                                                          
129300       MOVE TABELL-TIAARP(1)  TO WS-TIAAVV                                
129400       MOVE TABELL-SISTA-TIAAVV(1) TO WS-TIVV                             
129500       MOVE WS-TIAAVV         TO WS-FORSTA-TIAAVV                         
129600                                                                          
129700       MOVE TABELL-TIAARP (MAX-TAB-RADIX) TO WS-TIAAVV                    
129800       MOVE TABELL-FORSTA-TIAAVV (MAX-TAB-RADIX) TO WS-TIVV               
129900       MOVE WS-TIAAVV                    TO WS-SISTA-TIAAVV               
130000                                                                          
130100       IF DAT-KDSVAR-OK                                                   
130200*-----   KOLLA ATT MANUELLT PB LIGGER I TABELLEN                          
130300         MOVE WS-DAT-TIAAVV      TO TMP1-YYWW                             
130400         MOVE WS-FORSTA-TIAAVV   TO TMP2-YYWW                             
130500         MOVE WS-SISTA-TIAAVV    TO TMP3-YYWW                             
130600         PERFORM WY2000Q3                                                 
130700         IF TMP1-YYWW <= TMP2-YYWW AND                                    
130800            TMP1-YYWW >= TMP3-YYWW                                        
130900           MOVE JA TO MANUELL-PROGNOS-SW                                  
131000*-----     KOLLA I VILKEN PERIOD TIPBREOI LIGGER                          
131100           MOVE +1 TO TAB-RADIX                                           
131200           MOVE NEJ TO INDX-SW                                            
131300           PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX OR INDX-HITTAT         
131400             MOVE WS-DAT-TIAAVV                                           
131500                             TO TMP1-YYWW                                 
131600                                                                          
131700             MOVE TABELL-TIAARP(TAB-RADIX)                                
131800                             TO WS-TIAAVV                                 
131900             MOVE TABELL-FORSTA-TIAAVV(TAB-RADIX)                         
132000                             TO WS-TIVV                                   
132100             MOVE WS-TIAAVV  TO WS-FORSTA-TIAAVV                          
132200             MOVE WS-FORSTA-TIAAVV                                        
132300                             TO TMP2-YYWW                                 
132400                                                                          
132500             MOVE TABELL-TIAARP(TAB-RADIX)                                
132600                             TO WS-TIAAVV                                 
132700             MOVE TABELL-SISTA-TIAAVV(TAB-RADIX)                          
132800                             TO WS-TIVV                                   
132900             MOVE WS-TIAAVV  TO WS-SISTA-TIAAVV                           
133000             MOVE WS-SISTA-TIAAVV                                         
133100                             TO TMP3-YYWW                                 
133200                                                                          
133300             PERFORM WY2000Q3                                             
133400             IF  TMP1-YYWW >= TMP2-YYWW                                   
133500             AND TMP1-YYWW <= TMP3-YYWW                                   
133600*-----         TRÄFF I RÄTT PERIOD                                        
133700               MOVE TAB-RADIX TO WS-TIPBREOI-TIAARP                       
133800               MOVE JA TO INDX-SW                                         
133900             END-IF                                                       
134000             ADD +1 TO TAB-RADIX                                          
134100           END-PERFORM                                                    
134200         ELSE                                                             
134300           MOVE NEJ TO MANUELL-PROGNOS-SW                                 
134400         END-IF                                                           
134500       ELSE                                                               
134600         MOVE 'SVAR 5 FRÅN WDATKONV EJ OK' TO FELTEXT-STR                 
134700         DISPLAY FELTEXT                                                  
134800         PERFORM S99-ABEND                                                
134900       END-IF                                                             
135000     ELSE                                                                 
135100       MOVE NEJ TO MANUELL-PROGNOS-SW                                     
135200     END-IF                                                               
135300                                                                          
135400     .                                                                    
135500     EJECT                                                                
135600 S01B-KOLLA-OI SECTION.                                                   
135700                                                                          
135800     PERFORM S01BA-BERAKN-ONORM-OI-GRAENSER                               
135900                                                                          
136000*--- OM 2 PERIODER I RAD (ELLER FLER) LIGGER ÖVER GRÄNSEN                 
136100*--- FÖR ONORMAL OI BETRAKTAS DETTA EJ SOM ONORMAL OI                     
136200*--- OCH SKALL DÅ FÖLJDAKTLIGEN EJ BYTAS UT                               
136300                                                                          
136400     MOVE 1   TO TAB-RADIX                                                
136500     PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                              
136600                                                                          
136700       IF TAB-RADIX = MAX-TAB-RADIX                                       
136800         IF  TABELL-KVOI(TAB-RADIX)   > WS-ONORM-OI-GRAENS-PB             
136900         AND TABELL-KVOI(TAB-RADIX - 1) NOT >                             
137000                                 WS-ONORM-OI-GRAENS-PB                    
137100           MOVE WS-ONORM-OI-GRAENS-PB TO TABELL-KVOI(TAB-RADIX)           
137200         END-IF                                                           
137300       ELSE                                                               
137400         IF  TABELL-KVOI(TAB-RADIX) > WS-ONORM-OI-GRAENS-PB               
137500         AND TABELL-KVOI(TAB-RADIX + 1) > WS-ONORM-OI-GRAENS-PB           
137600           CONTINUE                                                       
137700         ELSE                                                             
137800           IF TABELL-KVOI(TAB-RADIX) > WS-ONORM-OI-GRAENS-PB              
137900             IF TAB-RADIX = 1                                             
138000               MOVE WS-ONORM-OI-GRAENS-PB TO                              
138100                                 TABELL-KVOI(TAB-RADIX)                   
138200             ELSE                                                         
138300               IF TABELL-KVOI(TAB-RADIX - 1) NOT >                        
138400                  WS-ONORM-OI-GRAENS-PB                                   
138500                 MOVE WS-ONORM-OI-GRAENS-PB TO                            
138600                                       TABELL-KVOI(TAB-RADIX)             
138700               END-IF                                                     
138800             END-IF                                                       
138900           END-IF                                                         
139000         END-IF                                                           
139100       END-IF                                                             
139200       ADD +1 TO TAB-RADIX                                                
139300                                                                          
139400     END-PERFORM                                                          
139500     .                                                                    
139600     EJECT                                                                
139700 S01BA-BERAKN-ONORM-OI-GRAENSER SECTION.                                  
139800                                                                          
139900*--- BERÄKNA GRÄNSER FÖR ONORMAL OI (COPYTEXT W271FSG)                    
140000*--- MED HJÄLP AV PROGNOS                                                 
140100     IF DCS-CHINA                                                         
140200     OR DCS-NDC-NA                                                        
140300       CONTINUE                                                           
140400     ELSE                                                                 
140500       MOVE CLAG-PRARTSTD  TO WS-PRIS                                     
140600     END-IF                                                               
140700                                                                          
140800     MOVE 1 TO INDX                                                       
140900     PERFORM UNTIL INDX > MAX-FSGFAKT                                     
141000       IF WS-PRIS <=                                                      
141100                               FSG-DC-PRARTSTD-MAX (INDX)                 
141200         COMPUTE WS-ONORM-OI-GRAENS-PB ROUNDED =                          
141300            (FSG-A (INDX) * SLAG-KVPBREOI) +                              
141400             FSG-B (INDX)                                                 
141500         MOVE 99 TO INDX                                                  
141600       ELSE                                                               
141700         ADD 1 TO INDX                                                    
141800       END-IF                                                             
141900     END-PERFORM                                                          
142000     .                                                                    
142100     EJECT                                                                
142200 S01C-KOLLA-MANUELL-PROGNOS SECTION.                                      
142300                                                                          
142400     IF MANUELL-PROGNOS-SATT                                              
142500                                                                          
142600*----- BYT UT ALLA OI FR.O.M TIPBREOI:S PERIOD OCH BAKÅT                  
142700*------MOT NUVARANDE PROGNOS (KVPBREOI)                                   
142800       MOVE WS-TIPBREOI-TIAARP TO TAB-RADIX                               
142900       PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                            
143000         IF SLAG-TIPBREOI > ZERO                                          
143100            IF SLAG-KVPBREOI-HIST > ZERO                                  
143200               MOVE SLAG-KVPBREOI-HIST                                    
143300                            TO TABELL-KVOI (TAB-RADIX)                    
143400            ELSE                                                          
143500               MOVE SLAG-KVPBREOI                                         
143600                            TO TABELL-KVOI (TAB-RADIX)                    
143700            END-IF                                                        
143800         ELSE                                                             
143900            MOVE SLAG-KVPBREOI                                            
144000                            TO TABELL-KVOI (TAB-RADIX)                    
144100         END-IF                                                           
144200         ADD 1 TO TAB-RADIX                                               
144300       END-PERFORM                                                        
144400                                                                          
144500     END-IF                                                               
144600                                                                          
144700     .                                                                    
144800     EJECT                                                                
144900 S01D-BERAEKNA-NY-PROGNOS SECTION.                                        
145000                                                                          
145100     PERFORM S01DF-SEARCH-CORRECT-DC                                      
145200     PERFORM S01DE-BERAEKNA-TRENDFAKT                                     
145300     PERFORM S01DA-BERAEKNA-PREL-PROGNOS                                  
145400                                                                          
145500     IF WS-PREL-KVPBREOI < 1                                              
145600       MOVE WS-PREL-KVPBREOI TO WS-NY-KVPBREOI                            
145700     ELSE                                                                 
145800       PERFORM S01DB-BERAEKNA-MEDELPROGNOS                                
145900                                                                          
146000       COMPUTE WS-KVOTEN ROUNDED = (WS-PREL-KVPBREOI + 1) /               
146100                                   (WS-MEDEL-KVPBREOI + 1)                
146200                                                                          
146300       PERFORM S01DC-KONTROLLERA-TREND                                    
146400       IF INGEN-TREND                                                     
146500         MOVE WS-PREL-KVPBREOI TO WS-NY-KVPBREOI                          
146600       ELSE                                                               
146700         IF SVAG-TREND                                                    
146800           MOVE 2 TO INDX                                                 
146900         ELSE                                                             
147000           IF STARK-TREND                                                 
147100             MOVE 3 TO INDX                                               
147200           END-IF                                                         
147300         END-IF                                                           
147400                                                                          
147500         IF ACC-KORNING                                                   
147600                                                                          
147700           MOVE +1 TO TAB-RADIX                                           
147800           MOVE ZERO TO WS-NY-KVPBREOI                                    
147900                                                                          
148000           PERFORM UNTIL TAB-RADIX > 12                                   
148100             COMPUTE WS-NY-KVPBREOI ROUNDED =                             
148200                       WS-NY-KVPBREOI +                                   
148300                (PROG-DC-TRENDFAKT (TAB-RADIX, INDX) *                    
148400                         TABELL-KVOI (TAB-RADIX))                         
148500             ADD 1 TO TAB-RADIX                                           
148600           END-PERFORM                                                    
148700         ELSE                                                             
148800                                                                          
148900           MOVE +1 TO TAB-RADIX                                           
149000           MOVE ZERO TO WS-NY-KVPBREOI                                    
149100                                                                          
149200           PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                        
149300             COMPUTE WS-NY-KVPBREOI ROUNDED =                             
149400                       WS-NY-KVPBREOI +                                   
149500              (WS-DC-TRENDFAKT-VKA (TAB-RADIX, INDX) *                    
149600                         TABELL-KVOI (TAB-RADIX))                         
149700             ADD 1 TO TAB-RADIX                                           
149800           END-PERFORM                                                    
149900         END-IF                                                           
150000                                                                          
150100       END-IF                                                             
150200     END-IF                                                               
150300                                                                          
150400*--- NY PROGNOS KAN INTE BLI LÄGRE ÄN 70% AV DEN GAMLA                    
150500*--- (KONTROLL SÅ DEN INTE SJUNKER FÖR SNABBT)                            
150600     IF  SLAG-KVPBREOI  = 0.1                                             
150700     AND WS-NY-KVPBREOI < 0.1                                             
150800        CONTINUE                                                          
150900     ELSE                                                                 
151000        IF WS-NY-KVPBREOI < 0.7 * SLAG-KVPBREOI                           
151100          COMPUTE WS-NY-KVPBREOI = 0.7 * SLAG-KVPBREOI                    
151200        END-IF                                                            
151300     END-IF                                                               
151400                                                                          
151500*--- AVRUNDA TILL EN DECIMAL                                              
151600     COMPUTE NY-KVPBREOI = WS-NY-KVPBREOI + 0.05                          
151700                                                                          
151800*--- SÄTT NY PROGNOS FÖR ARTIKELN                                         
151900*--- OM * INTE * PROGNOSEN ÄR LÅST, DÅ FÅR DEN ENDAST HÖJAS               
152000     IF NY-KVPBREOI NOT = SLAG-KVPBREOI                                   
152100        IF PROGNOS-AER-LAAST                                              
152200           IF NY-KVPBREOI > SLAG-KVPBREOI                                 
152300              MOVE JA         TO ARTIKEL-SW                               
152400           END-IF                                                         
152500        ELSE                                                              
152600           MOVE JA         TO ARTIKEL-SW                                  
152700        END-IF                                                            
152800     END-IF                                                               
152900     .                                                                    
153000     EJECT                                                                
153100 S01DA-BERAEKNA-PREL-PROGNOS SECTION.                                     
153200*----------------------------------------------------------------*        
153300* HÄR BERÄKNAS PRELIMINÄR PROGNOS MHA VIKTNINGSTABELL            *        
153400* (PROGNOSFAKTORER) MED INDX=1 (NORMAL)                          *        
153500* COPYTEXT W271PROG                                              *        
153600*----------------------------------------------------------------*        
153700                                                                          
153800     IF ACC-KORNING                                                       
153900       MOVE 1 TO INDX                                                     
154000                 TAB-RADIX                                                
154100       MOVE ZERO TO WS-PREL-KVPBREOI                                      
154200                    WS-ANTAL-FAKTORER-STOERRE-NOLL                        
154300                                                                          
154400       PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                            
154500         COMPUTE WS-PREL-KVPBREOI ROUNDED =                               
154600                        WS-PREL-KVPBREOI +                                
154700                  (PROG-DC-TRENDFAKT (TAB-RADIX, INDX) *                  
154800                         TABELL-KVOI (TAB-RADIX) )                        
154900         IF PROG-DC-TRENDFAKT (TAB-RADIX, INDX) > ZERO                    
155000           ADD 1 TO WS-ANTAL-FAKTORER-STOERRE-NOLL                        
155100         END-IF                                                           
155200         ADD 1 TO TAB-RADIX                                               
155300       END-PERFORM                                                        
155400     ELSE                                                                 
155500       MOVE 1 TO INDX                                                     
155600                 TAB-RADIX                                                
155700       MOVE ZERO TO WS-PREL-KVPBREOI                                      
155800                    WS-ANTAL-FAKTORER-STOERRE-NOLL                        
155900                                                                          
156000       PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                            
156100         COMPUTE WS-PREL-KVPBREOI ROUNDED =                               
156200                        WS-PREL-KVPBREOI +                                
156300                 (WS-DC-TRENDFAKT-VKA (TAB-RADIX, INDX)          *        
156400                         TABELL-KVOI (TAB-RADIX) )                        
156500         IF WS-DC-TRENDFAKT-VKA (TAB-RADIX, INDX) > ZERO                  
156600           ADD 1 TO WS-ANTAL-FAKTORER-STOERRE-NOLL                        
156700         END-IF                                                           
156800         ADD 1 TO TAB-RADIX                                               
156900       END-PERFORM                                                        
157000     END-IF                                                               
157100     .                                                                    
157200     EJECT                                                                
157300 S01DB-BERAEKNA-MEDELPROGNOS SECTION.                                     
157400*----------------------------------------------------------------*        
157500* HÄR BERÄKNAS MEDELPROGNOS                                      *        
157600*----------------------------------------------------------------*        
157700                                                                          
157800     IF ACC-KORNING                                                       
157900       MOVE +1 TO TAB-RADIX                                               
158000     ELSE                                                                 
158100       MOVE +2 TO TAB-RADIX                                               
158200     END-IF                                                               
158300                                                                          
158400     MOVE ZERO TO WS-KVOI-REF-TOT                                         
158500                  WS-MEDEL-KVPBREOI                                       
158600                                                                          
158700     PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                              
158800       COMPUTE WS-KVOI-REF-TOT ROUNDED =                                  
158900                         WS-KVOI-REF-TOT +                                
159000                         TABELL-KVOI (TAB-RADIX)                          
159100       ADD 1 TO TAB-RADIX                                                 
159200     END-PERFORM                                                          
159300                                                                          
159400     IF WS-ANTAL-FAKTORER-STOERRE-NOLL = ZERO                             
159500       MOVE ZERO TO WS-MEDEL-KVPBREOI                                     
159600     ELSE                                                                 
159700       COMPUTE WS-MEDEL-KVPBREOI ROUNDED =                                
159800               WS-KVOI-REF-TOT / 12                                       
159900     END-IF                                                               
160000     .                                                                    
160100     EJECT                                                                
160200 S01DC-KONTROLLERA-TREND SECTION.                                         
160300*----------------------------------------------------------------*        
160400* HÄR KONTROLLERAS OM ARTIKELN HAR NÅGON TREND MHA KVOTEN.       *        
160500* ARTIKELN KAN HA:                                               *        
160600*     ¤ INGEN TREND                                              *        
160700*     ¤ SVAG TREND  (TREND UPP ELLER TREND NER)                  *        
160800*     ¤ STARK TREND (TREND STARKT UPP ELLER TREND STARKT NER)    *        
160900*----------------------------------------------------------------*        
161000                                                                          
161100                                                                          
161200     IF (WS-KVOTEN >= 0.80) AND                                           
161300        (WS-KVOTEN <= 1.20)                                               
161400       MOVE 'INGEN' TO TREND-SW                                           
161500     ELSE                                                                 
161600       IF (WS-KVOTEN > 1.20 AND WS-KVOTEN < 1.35) OR                      
161700          (WS-KVOTEN > 0.64 AND WS-KVOTEN < 0.80)                         
161800         MOVE 'SVAG ' TO TREND-SW                                         
161900       ELSE                                                               
162000         MOVE 'STARK' TO TREND-SW                                         
162100       END-IF                                                             
162200     END-IF                                                               
162300     .                                                                    
162400     EJECT                                                                
162500 S01DD-BERAEKNA-MEDELPROGNOS SECTION.                                     
162600*----------------------------------------------------------------*        
162700* HÄR BERÄKNAS MEDELPROGNOS                                      *        
162800*----------------------------------------------------------------*        
162900                                                                          
163000     IF ACC-KORNING                                                       
163100       MOVE +1 TO TAB-RADIX                                               
163200     ELSE                                                                 
163300       MOVE +2 TO TAB-RADIX                                               
163400     END-IF                                                               
163500                                                                          
163600     MOVE ZERO TO WS-KVOI-REF-TOT                                         
163700                  WS-MEDEL-KVPBREOI                                       
163800                                                                          
163900     PERFORM UNTIL TAB-RADIX > MAX-TAB-RADIX                              
164000       COMPUTE WS-KVOI-REF-TOT ROUNDED =                                  
164100                         WS-KVOI-REF-TOT +                                
164200                         TABELL-KVOI (TAB-RADIX)                          
164300       ADD 1 TO TAB-RADIX                                                 
164400     END-PERFORM                                                          
164500                                                                          
164600     COMPUTE WS-MEDEL-KVPBREOI ROUNDED =                                  
164700               WS-KVOI-REF-TOT / 12                                       
164800     .                                                                    
164900     EJECT                                                                
165000 S01DE-BERAEKNA-TRENDFAKT SECTION.                                        
165100                                                                          
165200     COMPUTE WS-DC-TRENDFAKT-VKA (1, 1) ROUNDED =                         
165300             PROG-DC-TRENDFAKT (1, 1)                                     
165400           * (WS-VECKA-I-AKT-PERIOD / WS-KVVIPER)                         
165500     COMPUTE WS-DC-TRENDFAKT-VKA (1, 2) ROUNDED =                         
165600             PROG-DC-TRENDFAKT (1, 2)                                     
165700           * (WS-VECKA-I-AKT-PERIOD / WS-KVVIPER)                         
165800     COMPUTE WS-DC-TRENDFAKT-VKA (1, 3) ROUNDED =                         
165900             PROG-DC-TRENDFAKT (1, 3)                                     
166000           * (WS-VECKA-I-AKT-PERIOD / WS-KVVIPER)                         
166100     MOVE 2                  TO PER-INDX                                  
166200     PERFORM UNTIL PER-INDX > 12                                          
166300       MOVE 1                TO TREND-INDX                                
166400       PERFORM UNTIL TREND-INDX > 3                                       
166500         COMPUTE WS-DC-TRENDFAKT-VKA                                      
166600                    (PER-INDX, TREND-INDX) ROUNDED =                      
166700          (PROG-DC-TRENDFAKT (PER-INDX - 1,            TREND-INDX)        
166800        * ((WS-KVVIPER - WS-VECKA-I-AKT-PERIOD) / WS-KVVIPER))            
166900        +                                                                 
167000          (PROG-DC-TRENDFAKT (PER-INDX, TREND-INDX)                       
167100        * (WS-VECKA-I-AKT-PERIOD / WS-KVVIPER))                           
167200         ADD 1               TO TREND-INDX                                
167300       END-PERFORM                                                        
167400       ADD 1                 TO PER-INDX                                  
167500     END-PERFORM                                                          
167600     COMPUTE WS-DC-TRENDFAKT-VKA (13, 1) ROUNDED =                        
167700             PROG-DC-TRENDFAKT (12, 1)                                    
167800         * ((WS-KVVIPER - WS-VECKA-I-AKT-PERIOD) / WS-KVVIPER)            
167900     COMPUTE WS-DC-TRENDFAKT-VKA (13, 2) ROUNDED =                        
168000             PROG-DC-TRENDFAKT (12, 2)                                    
168100         * ((WS-KVVIPER - WS-VECKA-I-AKT-PERIOD) / WS-KVVIPER)            
168200     COMPUTE WS-DC-TRENDFAKT-VKA (13, 3) ROUNDED =                        
168300             PROG-DC-TRENDFAKT (12, 3)                                    
168400         * ((WS-KVVIPER - WS-VECKA-I-AKT-PERIOD) / WS-KVVIPER)            
168500     .                                                                    
168600     EJECT                                                                
168700 S01DF-SEARCH-CORRECT-DC SECTION.                                         
168800                                                                          
168900     MOVE 'N'   TO DCS-TRAEFF                                             
169000     MOVE +1 TO IX-DC                                                     
169100     PERFORM UNTIL (IX-DC > DC-MAX) OR (DCS-TRAEFF = 'J')                 
169200       IF TAB-PROG-DC-IDDC (IX-DC) = SLAG-IDDC                            
169300         MOVE +1 TO IX-TAB                                                
169400         MOVE JA TO DCS-TRAEFF                                            
169500         PERFORM UNTIL IX-TAB > 12                                        
169600           MOVE TAB-PROG-DC-NORMAL (IX-DC, IX-TAB) TO                     
169700                                 PROG-DC-NORMAL (IX-TAB)                  
169800           MOVE TAB-PROG-DC-SVAG-TREND (IX-DC, IX-TAB) TO                 
169900                                 PROG-DC-SVAG-TREND (IX-TAB)              
170000           MOVE TAB-PROG-DC-STARK-TREND (IX-DC, IX-TAB) TO                
170100                                 PROG-DC-STARK-TREND (IX-TAB)             
170200           ADD +1 TO IX-TAB                                               
170300         END-PERFORM                                                      
170400       END-IF                                                             
170500       ADD +1 TO IX-DC                                                    
170600     END-PERFORM                                                          
170700                                                                          
170800     IF DCS-TRAEFF = 'N'                                                  
170900       MOVE +1 TO IX-DC                                                   
171000       PERFORM UNTIL (IX-DC > DC-MAX) OR (DCS-TRAEFF = 'J')               
171100         IF TAB-PROG-DC-IDDC (IX-DC) = '99'                               
171200           MOVE +1 TO IX-TAB                                              
171300           MOVE JA TO DCS-TRAEFF                                          
171400           PERFORM UNTIL IX-TAB > 12                                      
171500             MOVE TAB-PROG-DC-NORMAL (IX-DC, IX-TAB) TO                   
171600                                   PROG-DC-NORMAL (IX-TAB)                
171700             MOVE TAB-PROG-DC-SVAG-TREND (IX-DC, IX-TAB) TO               
171800                                   PROG-DC-SVAG-TREND (IX-TAB)            
171900             MOVE TAB-PROG-DC-STARK-TREND (IX-DC, IX-TAB) TO              
172000                                   PROG-DC-STARK-TREND (IX-TAB)           
172100             ADD +1 TO IX-TAB                                             
172200           END-PERFORM                                                    
172300         END-IF                                                           
172400         ADD +1 TO IX-DC                                                  
172500       END-PERFORM                                                        
172600     END-IF                                                               
172700                                                                          
172800     .                                                                    
172900 S01E-KOLLA-PROGNOS-LAASNING SECTION.                                     
173000                                                                          
173100     MOVE NEJ TO ESCLOCK-SW                                               
173200                                                                          
173300                                                                          
173400* KONTROLLERA OM PB ÄR MANUELLT LÅST                                      
173500     MOVE SLAG-TIPBREOI      TO TMP1-YYMMDD                               
173600     MOVE DAGENS-DATUM       TO TMP2-YYMMDD                               
173700     PERFORM WY2000P1                                                     
173800     PERFORM S01EA-HAMTA-PUBLICERINGSDATUM                                
173900***FÖR ATT SLÄPPA IGENOM ART MED PUBLICERINGSDATUM FÖRE ÅR 2000           
174000     IF PUBLICERINGSDATUM > 500000                                        
174100       MOVE 010101 TO PUBLICERINGSDATUM                                   
174200     END-IF                                                               
174300***************************************************************           
174400     IF WDK6-ART-FLERS = JA AND                                           
174500        PUBLICERINGSDATUM > DAGENS-DATUM                                  
174600        MOVE 'A' TO ESCLOCK-SW                                            
174700     ELSE                                                                 
174800        IF  MANUELL-PROGNOS-SATT AND                                      
174900            TMP1-YYMMDD > TMP2-YYMMDD                                     
175000           MOVE 'A' TO ESCLOCK-SW                                         
175100        ELSE                                                              
175200*    OM INTE ART. ÄR MANUELLT LÅST SÅ KAN DEN VARA ESC-LÅST               
175300                                                                          
175400*    KONTROLLERA OM ARTIKLEN ÄR ESC-LÅST                                  
175500           IF SLAG-DAREFESC-REOI > ZERO                                   
175600              IF SLAG-DAREFESC-REOI >= DA-DAGENS-DATUM                    
175700                 MOVE JA TO ESCLOCK-SW                                    
175800              END-IF                                                      
175900           END-IF                                                         
176000                                                                          
176100*    KONTROLLERA OM PUBVECKA DC ÄR INOM ETT ÅR                            
176200           IF DCS-NDC-NA                                                  
176300              IF LART-DAPUBL > ZERO                                       
176400                 PERFORM S02-DAPUBL-US-1YEAR                              
176500                 IF WS-DAPUBL-US >= DA-DAGENS-DATUM                       
176600                    MOVE JA TO ESCLOCK-SW                                 
176700                 END-IF                                                   
176800              END-IF                                                      
176900           END-IF                                                         
177000                                                                          
177100*    KONTROLLERA OM PUBVECKA CDC ÄR INOM 1 ÅR                             
177200           IF DCS-SDC    OR DCS-NDC-PF     OR                             
177210              DCS-NDC-CN OR DCS-NDC-OTHERS OR DCS-NDC-SA                  
177300              CONTINUE                                                    
177400           ELSE                                                           
177500              IF WS-TIFINLV > ZERO                                        
177600                 PERFORM S04-WS-TIFINLV-1YEAR                             
177700                 MOVE WS3-TIFINLV TO TMP1-YYMMDD                          
177800                 MOVE DAGENS-DATUM TO TMP2-YYMMDD                         
177900                 PERFORM WY2000P1                                         
178000                 IF TMP1-YYMMDD > TMP2-YYMMDD                             
178100                    MOVE JA TO ESCLOCK-SW                                 
178200                 END-IF                                                   
178300              END-IF                                                      
178400           END-IF                                                         
178500        END-IF                                                            
178600     END-IF                                                               
178700                                                                          
178800     .                                                                    
178900     EJECT                                                                
179000 S01EA-HAMTA-PUBLICERINGSDATUM  SECTION.                                  
179100                                                                          
179200     IF DCS-SDC                                                           
179300        MOVE 'AAVVD'            TO DAT-KDDATFORM                          
179400        MOVE WS-TIFINLV         TO DAT-I-TIDATUM                          
179500**WS-TIFINLV KOMMER FRÅN WDK6-ART-TIFINLV                                 
179600                                                                          
179700        CALL WDATKONV USING DAT-KDDATFORM                                 
179800                            DAT-I-TIDATUM                                 
179900                            DAT-O-TIDATUM                                 
180000                            DAT-KDSVAR                                    
180100                                                                          
180200        IF DAT-KDSVAR-OK                                                  
180300           MOVE DAT-TIAAMMDD TO PUBLICERINGSDATUM                         
180400        ELSE                                                              
180500           MOVE 'SVAR WDATKONV   I S01EA SECTION'                         
180600                             TO FELTEXT-STR                               
180700           DISPLAY FELTEXT                                                
180800           PERFORM S99-ABEND                                              
180900        END-IF                                                            
181000      ELSE                                                                
181100         MOVE WS-TIFINLV        TO PUBLICERINGSDATUM                      
181200      END-IF                                                              
181300     .                                                                    
181400                                                                          
181500     EJECT                                                                
181600 S02-DAPUBL-US-1YEAR SECTION.                                             
181700     MOVE LART-DAPUBL TO WS-DAPUBL-US                                     
181800     ADD +1 TO WS-DAPUBL-US-YEAR                                          
181900     .                                                                    
182000     EJECT                                                                
182100 S04-WS-TIFINLV-1YEAR SECTION.                                            
182200*    ADDERA 1 ÅR TILL PUBVECKA CDC                                        
182300     MOVE WS-TIFINLV       TO WS2-TIFINLV                                 
182400     MOVE WS2-TIFINLV-AAVV TO DATUM-AAVV                                  
182500     MOVE +52              TO ANTAL                                       
182600     CALL W009VADD USING DATUM-AAVV ANTAL                                 
182700     MOVE DATUM-AAVV TO WS2-TIFINLV-AAVV                                  
182800                                                                          
182900*    RÄKNA OM PUBVECKA CDC TILL ÅÅMMDD                                    
183000     MOVE 'AAVVD'            TO DAT-KDDATFORM                             
183100     MOVE WS2-TIFINLV        TO DAT-I-TIDATUM                             
183200                                                                          
183300     CALL WDATKONV USING DAT-KDDATFORM                                    
183400                         DAT-I-TIDATUM                                    
183500                         DAT-O-TIDATUM                                    
183600                         DAT-KDSVAR                                       
183700                                                                          
183800     IF DAT-KDSVAR-OK                                                     
183900       MOVE DAT-TIAAMMDD     TO WS3-TIFINLV                               
184000     ELSE                                                                 
184100       MOVE 'SVAR WDATKONV   I S04 SECTION'                               
184200                             TO FELTEXT-STR                               
184300       DISPLAY FELTEXT                                                    
184400       PERFORM S99-ABEND                                                  
184500     END-IF                                                               
184600     .                                                                    
184700     EJECT                                                                
184800 S11-LAES-W271TYP  SECTION.                                               
184900                                                                          
185000     READ W271TYP             INTO W271TYP-POST                           
185100     .                                                                    
185200     SKIP3                                                                
185300                                                                          
185400 S12-SKRIV-W27134 SECTION.                                                
185500                                                                          
185600     WRITE UT-POST FROM UT-AREA                                           
185700                                                                          
185800     MOVE 'W27134'   TO POSTSUM-FDNAMN                                    
185900     MOVE 'W27134D3' TO POSTSUM-DDNAMN2                                   
186000     CALL POSTSUM USING POSTSUM-PARM                                      
186100     .                                                                    
186200     SKIP3                                                                
186300                                                                          
186400 S13-SKRIV-W2713A SECTION.                                                
186500                                                                          
186600     WRITE UT2-POST FROM UT2-AREA                                         
186700                                                                          
186800     MOVE 'W2713A'   TO POSTSUM-FDNAMN                                    
186900     MOVE 'W27134D4' TO POSTSUM-DDNAMN2                                   
187000     CALL POSTSUM USING POSTSUM-PARM                                      
187100     .                                                                    
187200     SKIP3                                                                
187300                                                                          
187900 S99-ABEND SECTION.                                                       
188000                                                                          
188100     MOVE 'S' TO POSTSUM-OPKOD                                            
188200     CALL POSTSUM USING POSTSUM-PARM                                      
188300     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
188400     .                                                                    
188500     EJECT                                                                
188600* --- IMS SEKTIONER ---                                                   
188700     SKIP3                                                                
188800     EJECT                                                                
188900 IMS-GU-WDK6-WDK601 SECTION.                                              
189000                                                                          
189100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
189200          DELIMITED BY SIZE INTO SSA1                                     
189300     MOVE '  ' TO GODK-STATUSKODER                                        
189400     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
189500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
189600     PERFORM IMS-STATUSKONTROLL                                           
189700     .                                                                    
189800     EJECT                                                                
189900 IMS-GNP-WDK6-WDK611 SECTION.                                             
190000                                                                          
190100     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
190200          DELIMITED BY SIZE INTO SSA1                                     
190300     MOVE '  ' TO GODK-STATUSKODER                                        
190400     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
190500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
190600     PERFORM IMS-STATUSKONTROLL                                           
190700     .                                                                    
190800     EJECT                                                                
190900 IMS-GN-WDK7 SECTION.                                                     
191000                                                                          
191100     CALL CBLTDLI USING GN WDK7-PCB DLI-IO-AREA-K7                        
191200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
191300     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
191400     PERFORM IMS-STATUSKONTROLL                                           
191500     .                                                                    
191600     EJECT                                                                
191700 IMS-GU-WDK712      SECTION.                                              
191800                                                                          
191900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
192000          DELIMITED BY SIZE  INTO SSA1                                    
192100     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
192200          DELIMITED BY SIZE  INTO SSA2                                    
192300     MOVE '  GE'               TO GODK-STATUSKODER                        
192400     CALL CBLTDLI USING GU WDK7LART-PCB DLI-IO-WDK712 SSA1 SSA2           
192500     MOVE WDK7LART-STATUS-CODE TO STATUS-WS                               
192600     PERFORM IMS-STATUSKONTROLL                                           
192700     .                                                                    
192800     EJECT                                                                
192900 IMS-GU-WDK722      SECTION.                                              
193000                                                                          
193100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
193200          DELIMITED BY SIZE  INTO SSA1                                    
193300     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
193400          DELIMITED BY SIZE  INTO SSA2                                    
193500     STRING 'WDK722  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
193600          DELIMITED BY SIZE INTO SSA3                                     
193700     MOVE '  GE'               TO GODK-STATUSKODER                        
193800     CALL CBLTDLI USING GU WDK72-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3         
193900     MOVE WDK72-STATUS-CODE TO STATUS-WS                                  
194000     PERFORM IMS-STATUSKONTROLL                                           
194100     .                                                                    
194200     EJECT                                                                
194300 IMS-GU-WDL711 SECTION.                                                   
194400                                                                          
194500     STRING 'WDL701  (IDARTNR  =' W-IDARTNR-X ')'                         
194600          DELIMITED BY SIZE INTO SSA1                                     
194700     STRING 'WDL711  (IDDC     =' W-IDDC-X ')'                            
194800          DELIMITED BY SIZE INTO SSA2                                     
194900     MOVE '  GE' TO GODK-STATUSKODER                                      
195000     CALL CBLTDLI USING GU WDL7-PCB DLI-IO-WDL711 SSA1 SSA2               
195100     MOVE WDL7-STATUS-CODE TO STATUS-WS                                   
195200     PERFORM IMS-STATUSKONTROLL                                           
195300     .                                                                    
195400     EJECT                                                                
195500                                                                          
195600 IMS-GU-WDB601    SECTION.                                                
195700     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
195800          DELIMITED BY SIZE INTO SSA1                                     
195900     MOVE '  ' TO GODK-STATUSKODER                                        
196000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
196100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
196200     PERFORM IMS-STATUSKONTROLL                                           
196300     .                                                                    
196400     EJECT                                                                
196500                                                                          
196600 IMS-GN-WDB601    SECTION.                                                
196700     MOVE 'WDB601  ' TO SSA1                                              
196800     MOVE '  GB'     TO GODK-STATUSKODER                                  
196900     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-AREA-B601 SSA1                 
197000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
197100     PERFORM IMS-STATUSKONTROLL                                           
197200     .                                                                    
197300     EJECT                                                                
197400                                                                          
197500 IMS-GNP-WDB618 SECTION.                                                  
197600                                                                          
197700     STRING 'WDB618  (KDPROGOI =' W-KDPROGOI-X ')'                        
197800          DELIMITED BY SIZE INTO SSA1                                     
197900     MOVE '  GEGB'           TO GODK-STATUSKODER                          
198000     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-AREA-B618  SSA1               
198100     MOVE WDB6-STATUS-CODE   TO STATUS-WS                                 
198200     PERFORM IMS-STATUSKONTROLL                                           
198300     .                                                                    
198400     EJECT                                                                
198500                                                                          
198600 IMS-STATUSKONTROLL SECTION.                                              
198700                                                                          
198800     SET STATUS-IX TO 1                                                   
198900     SEARCH GODK-STATUS                                                   
199000       AT END                                                             
199100         STRING 'OTILLÅTEN RETURKOD FRÅN IMS: ' STATUS-WS                 
199200           DELIMITED BY SIZE INTO FELTEXT-STR                             
199300         DISPLAY FELTEXT                                                  
199400         CALL FELLOG                                                      
199500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
199600         CONTINUE                                                         
199700     END-SEARCH                                                           
199800     .                                                                    
199900     EJECT                                                                
200000*    -COPY WY2000P1                                                       
200100     EJECT                                                                
200200*    -COPY WY2000P2                                                       
200300     EJECT                                                                
200400*    -COPY WY2000Q3                                                       
