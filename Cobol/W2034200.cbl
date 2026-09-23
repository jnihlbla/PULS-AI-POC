000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W2034200.                                                
000400 AUTHOR.         STEFAN KIHLBERG                                          
000500 DATE-WRITTEN.   97/01/01.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PROGRAM SOM VISAR SALDO, EFTERFRÅGAN OCH PROGNOS                 
001000*        FÖR S-LAGER. SALDO OCH PROGNOS VISAS ÄVEN FÖR C-LAGER.           
001100*        EFTERFRÅGAN, SOM LAGRAS I 53 VECKOR, SUMMERAS I RULL 12,         
001200*        PERIODER - ENDAST DE SENASTE 6 PERIODERNA VISAS. MÖJLIG-         
001300*        HET ATT UPPD. OI, PROGNOS SAMT DATUM MANUELL PROGNOS.            
001400*        OI UPPDATERAS FÖR ANGIVEN PERIOD - MED BÖRJAN I SISTA            
001500*        VECKAN SEDAN NÄST SISTA OSV.                                     
001600*                                                                         
001700*        SÄTTS MANUELL PROGNOS PÅ PASSIV ARTIKEL AKTIVERAS DEN.           
001800*                                                                         
001900*        PROGRAMMET STARTAR W20109 VIA EN PPSW                            
002000*                                                                         
002100*------- CHANGE LOG 2015/12/30                                            
002200*        THE PROGRAM IS CHANGED TO DISPLAY INFORMATION                    
002300*        FOR PARTS ELIGIBLE FOR REFILL TO CDC FROM CHINA. THE             
002400*        UPDATE FUNTIONALITY REMAINS SAME AS DESCRIBED ABOVE.             
002500*                                                                         
002600*------- DATABASES USED                                                   
002700*        PROGRAMMET UPPDATERAR WDK7                                       
002800*        PROGRAMMET UPPDATERAR WDK6                                       
002900*        PROGRAMMET LÄSER      WDL7 + WDL4                                
003000*        PROGRAMMET LÄSER      WDK6                                       
003100*        PROGRAMMET LÄSER      WLARTM  (WDK9)                             
003200*        PROGRAMMET LÄSER      WLBENA  (WDD3)                             
003300*        PROGRAMMET LÄSER      WDL8                                       
003400*        PROGRAMMET LÄSER      W6D1                                       
003500*        PROGRAMMET LÄSER      TP1ARTK (DB2)                              
003600*        PROGRAMMET LÄSER      TP1KAMP (DB2)                              
003700*                                                                         
003800*    INDATA.                                                              
003900*        TRANSAKTION: W2T342                                              
004000*        MID:         W2I34201                                            
004100*                                                                         
004200*    UTDATA.                                                              
004300*        MOD:         W2O34201                                            
004400                                                                          
004500     SKIP3                                                                
004600 ENVIRONMENT DIVISION.                                                    
004700     EJECT                                                                
004800 DATA DIVISION.                                                           
004900 WORKING-STORAGE SECTION.                                                 
005000*    -COPY WY2000W1                                                       
005100     SKIP3                                                                
005200 77  IDPGM                       PIC X(08)   VALUE 'W2034200'.            
005300                                                                          
005400*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
005500 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005600                                                                          
005700 77  JA                          PIC X       VALUE 'J'.                   
005800 77  NEJ                         PIC X       VALUE 'N'.                   
005900 77  AKTIV                       PIC X       VALUE 'A'.                   
006000 77  PASSIV                      PIC X       VALUE 'P'.                   
006100 77  DEFINITIV                   PIC S9      VALUE +1 COMP-3.             
006200                                                                          
006300 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
006400 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +0    COMP SYNC.        
006500 77  INDX-2                      PIC S9(4)  VALUE +0    COMP SYNC.        
006600 77  INDX-3                      PIC S9(4)  VALUE +0    COMP SYNC.        
006700 77  IX                          PIC S9(4)  VALUE +0    COMP SYNC.        
006800 77  IX2                         PIC S9(4)  VALUE +0    COMP SYNC.        
006900 77  IX-DC                       PIC 9(3)   VALUE ZERO.                   
007000                                                                          
007100*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
007200 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
007300 77  WS-IDDISTR                  PIC 9(4)    VALUE ZERO.                  
007400 77  WS-STD-TEXT                 PIC X(4)    VALUE ' Std'.                
007500 77  WS-MTRL-TEXT                PIC X(4)    VALUE 'Mtrl'.                
007600 77  WS-FC-TEXT-XDC              PIC X(5)    VALUE 'reoi '.               
007700 77  WS-FC-TEXT-CDC              PIC X(5)    VALUE 'tot  '.               
007800 77  WS-NDC-CN                   PIC X(2)    VALUE 'CN'.                  
007900 77  WS-NDC-US                   PIC X(2)    VALUE 'US'.                  
008000 77  WS-NDC-CA                   PIC X(2)    VALUE 'CA'.                  
008100                                                                          
008200 77  INDATA-SW                   PIC X       VALUE 'J'.                   
008300     88  INDATA-OK                           VALUE 'J'.                   
008400     88  INDATA-FEL                          VALUE 'N'.                   
008500                                                                          
008600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
008700     88  NYCKLAR-OK                          VALUE 'J'.                   
008800     88  NYCKLAR-FEL                         VALUE 'N'.                   
008900                                                                          
009000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
009100     88  EGEN-MID                            VALUE '2342'.                
009200     88  GODK-MID                            VALUE '2341' '2342'          
009300                                                   '2343'.                
009400     88  HELP-MID                            VALUE '0551'.                
009500                                                                          
009600 77  SW-WDK711                   PIC X       VALUE 'N'.                   
009700     88  WDK711-FINNS                        VALUE 'J'.                   
009800                                                                          
009900 77  REFILL-ARTIKEL-OK-SW        PIC X       VALUE 'J'.                   
010000     88  REFILL-ARTIKEL-OK                   VALUE 'J'.                   
010100                                                                          
010200 77  MANUELLT-DATUM-SW           PIC X       VALUE 'N'.                   
010300     88  MANUELLT-DATUM-UPPD                 VALUE 'J'.                   
010400                                                                          
010500 77  SW-LYNK-PART                PIC X       VALUE 'N'.                   
010600     88  LYNK-PART                           VALUE 'J'.                   
010700                                                                          
010800 77  SW-MESSAGE                  PIC X       VALUE 'N'.                   
010900     88  MESSAGE-JA                          VALUE 'J'.                   
011000     88  MESSAGE-NEJ                         VALUE 'N'.                   
011100                                                                          
011200 77  SW-CAMPAIGN                 PIC X       VALUE 'N'.                   
011300     88  CAMPAIGN-NEJ                        VALUE 'N'.                   
011400     88  CAMPAIGN-JA                         VALUE 'J'.                   
011500     EJECT                                                                
011600*    --- ARBETSFÄLT                                                       
011700 01  FILLER                      PIC  X(9)   VALUE 'ARB-FAELT'.           
011800 01  ARBETSFAELT.                                                         
011900     03  INDX                    PIC  9(2)   VALUE ZERO.                  
012000     03  MOD-IX                  PIC  9(2)   VALUE ZERO.                  
012100     03  VV                      PIC  9(2)   VALUE ZERO.                  
012200     03  IX-VV                   PIC  9(2)   VALUE ZERO.                  
012300     03  WS-TABELL    OCCURS 12.                                          
012400         05 WS-PER               PIC  9(2)   VALUE ZERO.                  
012500         05 WS-FORSTA-V          PIC  9(2)   VALUE ZERO.                  
012600         05 WS-SISTA-V           PIC  9(2)   VALUE ZERO.                  
012700         05 WS-KVOT              PIC S9(7)   VALUE ZERO COMP-3.           
012800         05 WS-KVOT-CDC          PIC S9(7)   VALUE ZERO COMP-3.           
012900         05 WS-KVOI              PIC S9(7)   VALUE ZERO COMP-3.           
013000         05 WS-KVOI-REF          PIC S9(7)   VALUE ZERO COMP-3.           
013100     03  WS-TABELL-IN OCCURS 12.                                          
013200         05 WS-TIPP-IN           PIC  9(2)   VALUE ZERO.                  
013300         05 WS-TIVV-FOM-IN       PIC  9(2)   VALUE ZERO.                  
013400         05 WS-TIVV-TOM-IN       PIC  9(2)   VALUE ZERO.                  
013500         05 WS-KVOI-IN           PIC S9(7)   VALUE ZERO.                  
013600     03  WS-BALANCE-CLAG         PIC S9(7)   VALUE ZERO COMP-3.           
013700     03  WS-BALANCE-SLAG         PIC S9(7)   VALUE ZERO COMP-3.           
013800     03  WS-KVOI-INNEV-IN        PIC S9(7)   VALUE ZERO COMP-3.           
013900     03  WS-KVOI-INNEV-SUM       PIC S9(7)   VALUE ZERO COMP-3.           
014000     03  WS-KVPB                 PIC S9(10)V9(2)                          
014100                                             VALUE ZERO.                  
014200     03  WS-TIREFMPB             PIC S9(7)   VALUE ZERO COMP-3.           
014300     03  WS-TIPBREOI             PIC S9(7)   VALUE ZERO COMP-3.           
014400     03  WS-TIPBDAT              PIC S9(5)   VALUE ZERO COMP-3.           
014500     03  WS-DAPBPLAN.                                                     
014600         05 WS-DAPBPLAN-CC       PIC  9(2)   VALUE 20.                    
014700         05 WS-DAPBPLAN-AAMMDD   PIC  9(6)   VALUE ZERO.                  
014800     03  WS-KVOT-TOT12           PIC S9(9)   VALUE ZERO COMP-3.           
014900     03  WS-KVOT-CDC-TOT12       PIC S9(9)   VALUE ZERO COMP-3.           
015000     03  WS-KVOI-TOT12           PIC S9(9)   VALUE ZERO COMP-3.           
015100     03  WS-KVOI-REF-TOT12       PIC S9(9)   VALUE ZERO COMP-3.           
015200     03  WS-KVOT-TOT6            PIC S9(9)   VALUE ZERO COMP-3.           
015300     03  WS-KVOT-CDC-TOT6        PIC S9(9)   VALUE ZERO COMP-3.           
015400     03  WS-KVOI-TOT6            PIC S9(9)   VALUE ZERO COMP-3.           
015500     03  WS-KVOI-REF-TOT6        PIC S9(9)   VALUE ZERO COMP-3.           
015600     03  WS-KVOT-INNEV           PIC S9(9)   VALUE ZERO COMP-3.           
015700     03  WS-KVOT-CDC-INNEV       PIC S9(9)   VALUE ZERO COMP-3.           
015800     03  WS-KVOI-INNEV           PIC S9(9)   VALUE ZERO COMP-3.           
015900     03  WS-KVOI-REF-INNEV       PIC S9(9)   VALUE ZERO COMP-3.           
016000     03  WS-DIFF-OI              PIC S9(7)   VALUE ZERO COMP-3.           
016100     03  WS-VECKA                PIC  9(2)   VALUE ZERO.                  
016200     03  WS-DIFF                 PIC S9(7)   VALUE ZERO COMP-3.           
016300     03  WS-KVART                PIC S9(7)   VALUE ZERO COMP-3.           
016400     03  WS-KVOKS-TOT            PIC S9(7)   VALUE ZERO COMP-3.           
016500     03  WS-KVART-TOT-C1         PIC S9(7)   VALUE ZERO COMP-3.           
016600     03  WS-SPAR-SALDO           PIC S9(7)   VALUE ZERO COMP-3.           
016700     03  WS-TIVV5                PIC  X(5)   VALUE SPACE.                 
016800     03  WS-KDTECKEN             PIC  X(1)   VALUE SPACE.                 
016900     03  WS-KVOI-FOREG-1         PIC S9(7)   VALUE ZERO COMP-3.           
017000     03  WS-KVOI-FOREG-2         PIC S9(7)   VALUE ZERO COMP-3.           
017100     03  WS-ORDERED-Q            PIC S9(7)   VALUE ZERO.                  
017200     03  TIAAVV-OI               PIC  9(4)   VALUE ZERO.                  
017300     03  WS-TIAAVV-OI REDEFINES TIAAVV-OI.                                
017400         05 WS-TIAA-OI           PIC  9(2).                               
017500         05 WS-TIVV-OI           PIC  9(2).                               
017600     03  DAGENS-AAAAMMDD         PIC  9(8)   VALUE ZERO.                  
017700     03  DAGENS-DATUM-SEKEL      PIC  9(8)   VALUE ZERO.                  
017800     03  DAGENS-DATUM            PIC  9(6)   VALUE ZERO.                  
017900     03  DAGENS-AAR              PIC  9(4)   VALUE ZERO.                  
018000     03  DAGENS-VECKA            PIC  9(2)   VALUE ZERO.                  
018100     03  DAGENS-PER              PIC  9(4)   VALUE ZERO.                  
018200     03  DAG-PER REDEFINES DAGENS-PER.                                    
018300         05 DAGENS-AA            PIC  9(2).                               
018400         05 DAGENS-PP            PIC  9(2).                               
018500     03  WS-TIAAMMDD.                                                     
018600         05 WS-TIAA              PIC  9(2)   VALUE ZERO.                  
018700         05 WS-TIMM              PIC  9(2)   VALUE ZERO.                  
018800         05 WS-TIDD              PIC  9(2)   VALUE ZERO.                  
018900     03  TIAAMMDD REDEFINES WS-TIAAMMDD PIC 9(6).                         
019000     03  WS-TIAAVV.                                                       
019100         05 WS-AAR               PIC  9(2)   VALUE ZERO.                  
019200         05 WS-VV                PIC  9(2)   VALUE ZERO.                  
019300     03  TIAAVV REDEFINES WS-TIAAVV PIC 9(4).                             
019400     03  WS-TIAAPER.                                                      
019500         05 TIAA                 PIC  9(2)   VALUE ZERO.                  
019600         05 PER                  PIC  9(2)   VALUE ZERO.                  
019700     03  TIAAPER REDEFINES WS-TIAAPER PIC 9(4).                           
019800     03  WS-TIAAPP               PIC  9(4)   VALUE ZERO.                  
019900     03  FILLER REDEFINES WS-TIAAPP.                                      
020000         05 WS-TIAAPP-AA         PIC  9(2).                               
020100         05 WS-TIAAPP-PP         PIC  9(2).                               
020200     03  WS-ARHAA.                                                        
020300         05 WS-ARH               PIC  9(2)   VALUE 20.                    
020400         05 WS-AA                PIC  9(2)   VALUE ZERO.                  
020500     03  WS-FOM-TOM.                                                      
020600         05 WS-FOM               PIC  X(2)   VALUE ZERO.                  
020700         05 WS-STRECK            PIC  X(1)   VALUE '-'.                   
020800         05 WS-TOM               PIC  X(2)   VALUE ZERO.                  
020900     03  ANT-VV                  PIC  9(2)   VALUE ZERO.                  
021000     03  WS-ANT-PP               PIC  9(2)   VALUE ZERO.                  
021100     03  WS-ANT-VV               PIC  9(2)   VALUE ZERO.                  
021200     03  WS-ANTAL-VECKOR         PIC S9(3)   VALUE ZERO.                  
021300     03  WS-FOREG-AAR            PIC  9(4)   VALUE ZERO.                  
021400     03  WS-KDERS                PIC  9(3)   VALUE ZERO.                  
021500     03  WS-PRARTBES           PIC S9(7)V9(2) VALUE ZERO COMP-3.          
021600     03  FL-PRARTBES           PIC X          VALUE 'N'.                  
021700     03  WS-KVAKS                PIC S9(7)   VALUE ZERO COMP-3.           
021800     03  WS-KVPB-SEP          PIC S9(6)V9(1) VALUE ZERO COMP-3.           
021900     03  WS-KVPB-PLAN            PIC 9(6)V9  VALUE ZERO.                  
022000     03  WS-KVPB-TOT             PIC 9(10)V9(1) VALUE ZERO.               
022100     03  WS-KVPB-TOT-CDC         PIC  9(6)V9(1) VALUE ZERO.               
022200     03  WS-KVART-FORAVIS        PIC  S9(8)  VALUE ZERO.                  
022300     03  WS-IDDC-REF             PIC  X(2)   VALUE SPACE.                 
022400     03  WS-IDDC-LAND            PIC  X(2)   VALUE SPACE.                 
022500     03  WS-KVOT-FORE            PIC  9(6)   VALUE ZERO.                  
022600     03  WS-KVDISP               PIC  S9(7)  VALUE ZERO.                  
022700     03  WS-KVAVIS               PIC S9(9)   VALUE ZERO.                  
022800*                                                                         
022900     03  WS-PERIODTABELL-AR-1    OCCURS 12.                               
023000         05 WS-PER-TIAAPP-1      PIC  9(2)   VALUE ZERO.                  
023100         05 WS-PER-STA-VV-1      PIC  9(2)   VALUE ZERO.                  
023200         05 WS-PER-END-VV-1      PIC  9(2)   VALUE ZERO.                  
023300         05 WS-KVOT-VV-1         PIC S9(7)   VALUE ZERO.                  
023400         05 WS-KVOI-F1-VV-1      PIC S9(7)   VALUE ZERO.                  
023500         05 WS-KVOI-F2-VV-1      PIC S9(7)   VALUE ZERO.                  
023600                                                                          
023700     03  WS-PERIODTABELL-AR-0    OCCURS 12.                               
023800         05 WS-PER-TIAAPP-0      PIC  9(2)   VALUE ZERO.                  
023900         05 WS-PER-STA-VV-0      PIC  9(2)   VALUE ZERO.                  
024000         05 WS-PER-END-VV-0      PIC  9(2)   VALUE ZERO.                  
024100         05 WS-KVOT-VV-0         PIC S9(7)   VALUE ZERO.                  
024200         05 WS-KVOI-F1-VV-0      PIC S9(7)   VALUE ZERO.                  
024300         05 WS-KVOI-F2-VV-0      PIC S9(7)   VALUE ZERO.                  
024400                                                                          
024500     03  WS-PERIODTABELL-SORTED  OCCURS 12.                               
024600         05 WS-PER-TIAAPP-S      PIC  9(2)   VALUE ZERO.                  
024700         05 WS-PER-STA-VV-S      PIC  9(2)   VALUE ZERO.                  
024800         05 WS-PER-END-VV-S      PIC  9(2)   VALUE ZERO.                  
024900         05 WS-KVOT-VV-S         PIC S9(7)   VALUE ZERO.                  
025000         05 WS-KVOI-F1-VV-S      PIC S9(7)   VALUE ZERO.                  
025100         05 WS-KVOI-F2-VV-S      PIC S9(7)   VALUE ZERO.                  
025200*                                                                         
025300*--- GENOMSNITTLIGT ANTAL VECKOR PER PERIOD UNDER 1 ÅR (52/12)            
025400*                                                                         
025500     03  WS-SNITT-VECKOR      PIC 9(1)V9(2)  VALUE 4.33.                  
025600*                                                                         
025700*--- CHECK USED TO CHECK PART IS ELIGIBLE FOR CAMPAIGN                    
025800*                                                                         
025900                                                                          
026000     03  WS-DATUM                PIC 9(6)    VALUE ZERO.                  
026100     03  FILLER   REDEFINES WS-DATUM.                                     
026200       05 WS-DATUM-AAR           PIC 9(2).                                
026300       05 WS-DATUM-MAN           PIC 9(2).                                
026400       05 WS-DATUM-DAG           PIC 9(2).                                
026500                                                                          
026600     03  WS-TISTODAT-KAMP        PIC 9(6)    VALUE ZERO.                  
026700     EJECT                                                                
026800*      --- VALID IDDC CODES                                               
026900*                                                                         
027000*01    -COPY WWDC99                                                       
027100       EJECT                                                              
027200*01    -COPY WWDC99    -PRE WS1-                                          
027300       EJECT                                                              
027400*01    -COPY WWDCKONS                                                     
027500       EJECT                                                              
027600*01    -COPY WWDCLAND                                                     
027700*                                                                         
027800 01  WS.                                                                  
027900     03 WS-SECTION               PIC X(24)   VALUE SPACE.                 
028000     03 FILLER                   PIC X(16)   VALUE                        
028100                                             'WS-DB2-SEKTION'.            
028200     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
028300     EJECT                                                                
028400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
028500 01  GENERELLA-SUBPROGRAM.                                                
028600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
028700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
028800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
028900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
029000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
029100     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
029200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
029300     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
029400     03  W271UTIL                PIC X(8)    VALUE 'W271UTIL'.            
029500     EJECT                                                                
029600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
029700*01 -COPY WMEDAREA                                                        
029800     EJECT                                                                
029900*    --- COPYTEXT TILL SUBPROGRAM WDATKONV                                
030000*01  -COPY WDATAREA                                                       
030100     EJECT                                                                
030200*    --- COPYTEXT TILL SUBPROGRAM WDECEDIT                                
030300*01  -COPY WDECAREA                                                       
030400     EJECT                                                                
030500*    --- COPYTEXT TILL SUBPROGRAM W005INIT                                
030600*01  -COPY WMSGINIT                                                       
030700     EJECT                                                                
030800*    --- PARAMETRAR TILL SUBPROGRAM W005WDK7                              
030900 01 FILLER                       PIC X(8)    VALUE 'W005WDK7'.            
031000*   -COPY W005WDK7                                                        
031100     EJECT                                                                
031200*    --- PARAMETRAR TILL SUBPROGRAM W271UTIL                              
031300*01 -COPY W271UTIL                                                        
031400     EJECT                                                                
031500 01  FILLER                      PIC X(16)   VALUE 'BYTESOBJEKT'.         
031600     SKIP3                                                                
031700*01 -COPY WWBYT03                                                         
031800     EJECT                                                                
031900     SKIP3                                                                
032000 01  MESSAGE-CODES.                                                       
032100     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
032200     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
032300     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
032400     03  ARTIKEL-UTGANGEN        PIC X(3)    VALUE '018'.                 
032500     03  ARTIKEL-SAKNAS          PIC X(3)    VALUE '017'.                 
032600     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
032700     03  ARTIKEL-ERSATT          PIC X(3)    VALUE '220'.                 
032800     03  PRIS-SAKNAS             PIC X(3)    VALUE '301'.                 
032900     03  ARTIKEL-SAKNAS-SDC      PIC X(3)    VALUE '305'.                 
033000     03  DIREKTLEV               PIC X(3)    VALUE '306'.                 
033100     03  EJ-GODK-REFILL          PIC X(3)    VALUE '307'.                 
033200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
033300     03  INF-NOT-REFILL-PART     PIC X(3)    VALUE '957'.                 
033400                                                                          
033500     EJECT                                                                
033600                                                                          
033700 01  MEDDELANDE.                                                          
033800     03  MED-1                   PIC X(30)                                
033900         VALUE 'KIT                           '.                          
034000     03  MED-2                   PIC X(30)                                
034100         VALUE 'TPO                           '.                          
034200     03  MED-3                   PIC X(30)                                
034300         VALUE 'CAMPAIGN                      '.                          
034400     03  MED-4                   PIC X(30)                                
034500         VALUE 'LYNK & CO PART                '.                          
034600     EJECT                                                                
034700                                                                          
034800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
034900*                                                                         
035000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
035100     SKIP3                                                                
035200*01  MID -COPY W2I34201                                                   
035300     EJECT                                                                
035400 01  FILLER            PIC X(16)  VALUE 'WMSG/MID-AREA'.                  
035500     SKIP3                                                                
035600*01  -COPY WMSGAREA   -PRE W                                              
035700     EJECT                                                                
035800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
035900     SKIP3                                                                
036000*01  -COPY WMSGAREA                                                       
036100     EJECT                                                                
036200     03  MOD REDEFINES MSG-AREA.                                          
036300*      05  -COPY W2O34201                                                 
036400     EJECT                                                                
036500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
036600     SKIP3                                                                
036700*01  -COPY WMFSAREA                                                       
036800     EJECT                                                                
036900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
037000*                                                                         
037100     EJECT                                                                
037200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
037300     SKIP3                                                                
037400 01  NYCKLAR-TILL-DLI.                                                    
037500     03  W-IDDC-X.                                                        
037600         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
037700     03  W-IDLAND-X.                                                      
037800         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
037900     03  W-IDDC-B6-X.                                                     
038000         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
038100     03  W-IDDC-B616-X.                                                   
038200         05  W-IDDC-B616         PIC X(2)    VALUE SPACE.                 
038300     03  W-IDDC-K7-X.                                                     
038400         05  W-IDDC-K7           PIC X(2)    VALUE '71'.                  
038500     03  W-IDDC-REF-X.                                                    
038600         05  W-IDDC-REF          PIC X(02)   VALUE SPACE.                 
038700     03  W-IDARTNR-X.                                                     
038800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
038900     03  W-KDSEGKEY-X.                                                    
039000         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
039100     03  W-IDLEVNR-X.                                                     
039200         05  W-IDLEVNR           PIC  X(5)   VALUE SPACE.                 
039300     03  W-IDLEVNR-K7-X.                                                  
039400         05  W-IDLEVNR-K7        PIC  X(5)   VALUE SPACE.                 
039500     03  W-DAPRLIST-X.                                                    
039600         05  W-DAPRLIST          PIC  9(8)   VALUE ZERO.                  
039700     03  W-IDSKYLT-X.                                                     
039800         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
039900     03  W-W6D1HSEQ-X.                                                    
040000         05  W-IDARTNR-HSEQ      PIC S9(9)   VALUE ZERO  COMP-3.          
040100     03  W-KDSEGKEY-K722-X.                                               
040200         05  W-KDSEGKEY-K722     PIC X(1)    VALUE '1'.                   
040300     03  W-TIAAAA-X.                                                      
040400         05  W-TIAAAA            PIC 9(4)    VALUE ZERO.                  
040500     SKIP2                                                                
040600*    --- STATUS-KOD FRÅN IMS                                              
040700 01  STATUS-WS                   PIC XX.                                  
040800     88  SEGMENT-FINNS                       VALUE '  '.                  
040900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
041000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
041100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
041200     SKIP2                                                                
041300 01  GODK-STATUSKODER.                                                    
041400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
041500     SKIP3                                                                
041600 01  SSA1                        PIC X(64).                               
041700 01  SSA2                        PIC X(64).                               
041800 01  SSA3                        PIC X(64).                               
041900     EJECT                                                                
042000*    --- IMS FUNKTIONSKODER                                               
042100*01  -COPY W0003                                                          
042200     EJECT                                                                
042300*    ---  DB2 SECTION                                                     
042400 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
042500       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
042600                                                                          
042700 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
042800 01  DB2-WS.                                                              
042900     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
043000         88  CURSOR-OK                      VALUE 000.                    
043100         88  LINES-FOUND                    VALUE 000.                    
043200         88  LINES-MISSING                  VALUE 100.                    
043300         88  RESOURCE-WRONG                 VALUE 904.                    
043400     03  GOOD-SQLCODECODES.                                               
043500         05  GOOD-SQLCODE OCCURS 5                                        
043600             INDEXED BY SQLCODE-IX PIC 9(3).                              
043700     EJECT                                                                
043800                                                                          
043900*    ---  DLI INPUT-OUTPUT AREA                                           
044000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
044100 01  DLI-IO-WDK601.                                                       
044200*    03  -COPY WDK601                                                     
044300     EJECT                                                                
044400                                                                          
044500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
044600 01  DLI-IO-WDK611.                                                       
044700*    03  -COPY WDK611                                                     
044800                                                                          
044900     EJECT                                                                
045000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK621'.                      
045100 01  DLI-IO-WDK621.                                                       
045200*    03  -COPY WDK621                                                     
045300                                                                          
045400     EJECT                                                                
045500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK629'.                      
045600 01  DLI-IO-WDK629.                                                       
045700*    03  -COPY WDK629                                                     
045800                                                                          
045900     EJECT                                                                
046000                                                                          
046100 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTM01'.                      
046200 01  DLI-IO-ARTM01.                                                       
046300*    03  -COPY WDK901  -PRE ARTM-                                         
046400                                                                          
046500     EJECT                                                                
046600 01  FILLER         PIC X(24) VALUE 'DLI-IO-BENA01'.                      
046700 01  DLI-IO-BENA01.                                                       
046800*    03  -COPY WDD301  -PRE BENA-                                         
046900                                                                          
047000     EJECT                                                                
047100 01  FILLER         PIC X(24) VALUE 'DLI-IO-BENA11'.                      
047200 01  DLI-IO-BENA11.                                                       
047300*    03  -COPY WDD311  -PRE BENA-                                         
047400                                                                          
047500     EJECT                                                                
047600 01  FILLER         PIC X(24) VALUE 'DLI-IO-LEVA16'.                      
047700 01  DLI-IO-LEVA16.                                                       
047800*    03  -COPY WDF116                                                     
047900                                                                          
048000     EJECT                                                                
048100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-L711'.         
048200 01  DLI-IO-L711.                                                         
048300*    03  -COPY WDL711                                                     
048400     EJECT                                                                
048500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-L411'.         
048600 01  DLI-IO-L411.                                                         
048700*    03  -COPY WDL411                                                     
048800     EJECT                                                                
048900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-K701'.         
049000 01  DLI-IO-K701.                                                         
049100*    03  -COPY WDK701                                                     
049200     EJECT                                                                
049300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-K711'.         
049400 01  DLI-IO-K711.                                                         
049500*    03  -COPY WDK711                                                     
049600     EJECT                                                                
049700 01  FILLER                 PIC X(16) VALUE 'DLI-IO-WDK722'.              
049800 01  DLI-IO-AREA-WDK722.                                                  
049900*    03  -COPY WDK722                                                     
050000     EJECT                                                                
050100                                                                          
050200 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
050300 01   DLI-IO-AREA-B601.                                                   
050400*     03  -COPY WDB601                                                    
050500                                                                          
050600 01  FILLER               PIC X(16)   VALUE 'WDB601 NEXT'.                
050700 01   DLI-IO-AREA-B601-NEXT.                                              
050800*     03  -COPY WDB601   -PRE NEXT-                                       
050900     EJECT                                                                
051000                                                                          
051100 01  FILLER               PIC X(16)   VALUE 'WDB616 AREA'.                
051200 01   DLI-IO-AREA-B616.                                                   
051300*     03  -COPY WDB616                                                    
051400                                                                          
051500 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK711'.             
051600 01  DLI-IO-WDK711.                                                       
051700*        05  -COPY WDK711 -PRE K7-                                        
051800                                                                          
051900 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK712'.             
052000 01  DLI-IO-WDK712.                                                       
052100*        05  -COPY WDK712                                                 
052200     EJECT                                                                
052300                                                                          
052400 01  FILLER                  PIC X(16) VALUE 'DLI-IO-W6D111'.             
052500 01  DLI-IO-W6D111.                                                       
052600*    03  -COPY W6D111 -PRE W6D1-                                          
052700     EJECT                                                                
052800                                                                          
052900 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDL811'.             
053000 01  DLI-IO-WDL811.                                                       
053100*    03  -COPY WDL811                                                     
053200     EJECT                                                                
053300 01  FILLER                  PIC X(16)  VALUE 'TP1KAMP-AREA'.             
053400                                                                          
053500*01  -COPY TP1KAMP -PRE TP1KAMP-                                          
053600     EJECT                                                                
053700 01  FILLER                  PIC X(16)  VALUE 'TP1ARTK-AREA'.             
053800                                                                          
053900*01  -COPY TP1ARTK -PRE TP1ARTK-                                          
054000     EJECT                                                                
054100     EXEC SQL INCLUDE TP1KAMP END-EXEC.                                   
054200     EJECT                                                                
054300     EXEC SQL INCLUDE TP1ARTK END-EXEC.                                   
054400     EJECT                                                                
054500                                                                          
054600 LINKAGE SECTION.                                                         
054700                                                                          
054800*01  -COPY W0009   -PRE MSG-                                              
054900     EJECT                                                                
055000*01  -COPY W0008  -PRE USEA-                                              
055100     05  FILLER                  PIC X.                                   
055200     EJECT                                                                
055300*01  -COPY W0008  -PRE WDL7-                                              
055400     05  FILLER                  PIC X.                                   
055500     EJECT                                                                
055600*01  -COPY W0008  -PRE WDL4-                                              
055700     05  FILLER                  PIC X.                                   
055800     EJECT                                                                
055900*01  -COPY W0008  -PRE WDK6-                                              
056000     05  FILLER                  PIC X.                                   
056100     EJECT                                                                
056200*01  -COPY W0008  -PRE ARTM-                                              
056300     05  FILLER                  PIC X.                                   
056400     EJECT                                                                
056500*01  -COPY W0008  -PRE BENA-                                              
056600     05  FILLER                  PIC X.                                   
056700     EJECT                                                                
056800*01  -COPY W0008  -PRE WDK7I-                                             
056900     05  FILLER                  PIC X.                                   
057000     EJECT                                                                
057100*01  -COPY W0008  -PRE LEVA-                                              
057200     05  FILLER                  PIC X.                                   
057300     EJECT                                                                
057400*01  -COPY W0008  -PRE REFL-2501-                                         
057500     05  FILLER                  PIC X.                                   
057600     EJECT                                                                
057700*01  -COPY W0008      -PRE WDB6-                                          
057800     05  FILLER                  PIC X.                                   
057900     EJECT                                                                
058000*01  -COPY W0008      -PRE WDB6-GN-                                       
058100     05  FILLER                  PIC X.                                   
058200     EJECT                                                                
058300*01  -COPY W0008      -PRE WDK7-                                          
058400     05  FILLER                  PIC X.                                   
058500     EJECT                                                                
058600*01  -COPY W0008      -PRE WDL8-                                          
058700     05  FILLER                  PIC X.                                   
058800     EJECT                                                                
058900*01  -COPY W0008      -PRE W6D1-                                          
059000     05  FILLER                  PIC X.                                   
059100     EJECT                                                                
059200 01  UTIL-WDK6-PCB               PIC X.                                   
059300 01  UTIL-WDK7-PCB               PIC X.                                   
059400 01  UTIL-WDB6-PCB               PIC X.                                   
059500     EJECT                                                                
059600 PROCEDURE DIVISION  USING MSG-PCB  USEA-PCB                              
059700                           WDL7-PCB WDL4-PCB WDK6-PCB ARTM-PCB            
059800                           BENA-PCB WDK7I-PCB LEVA-PCB                    
059900                           REFL-2501-PCB WDB6-PCB WDB6-GN-PCB             
060000                           WDK7-PCB WDL8-PCB W6D1-PCB                     
060100                           UTIL-WDK6-PCB UTIL-WDK7-PCB                    
060200                           UTIL-WDB6-PCB.                                 
060300 MAIN SECTION.                                                            
060400     ENTRY 'DLITCBL' USING MSG-PCB  USEA-PCB                              
060500                           WDL7-PCB WDL4-PCB WDK6-PCB ARTM-PCB            
060600                           BENA-PCB WDK7I-PCB LEVA-PCB                    
060700                           REFL-2501-PCB WDB6-PCB WDB6-GN-PCB             
060800                           WDK7-PCB WDL8-PCB W6D1-PCB                     
060900                           UTIL-WDK6-PCB UTIL-WDK7-PCB                    
061000                           UTIL-WDB6-PCB.                                 
061100                                                                          
061200     PERFORM IMS-GET-MSG                                                  
061300     IF SEGMENT-FINNS                                                     
061400       PERFORM A-INIT                                                     
061500       PERFORM B-KOLLA-NYCKLAR                                            
061600       IF NYCKLAR-OK                                                      
061700         IF MFS-UPDATE                                                    
061800           PERFORM G-KOLLA-INPUT                                          
061900           IF INDATA-OK                                                   
062000             PERFORM H-UPPDATERA-VISA-INFO                                
062100             PERFORM F-LAES-VISA-INFO                                     
062200           END-IF                                                         
062300         ELSE                                                             
062400           IF MFS-FIRST                                                   
062500             PERFORM C-FOERSTA-SIDA                                       
062600           ELSE                                                           
062700             PERFORM E-SAMMA-SIDA                                         
062800           END-IF                                                         
062900           PERFORM F-LAES-VISA-INFO                                       
063000         END-IF                                                           
063100       END-IF                                                             
063200       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
063300       PERFORM IMS-INSERT-MSG                                             
063400     END-IF                                                               
063500                                                                          
063600     MOVE ZERO TO RETURN-CODE                                             
063700     GOBACK                                                               
063800     .                                                                    
063900     EJECT                                                                
064000 A-INIT SECTION.                                                          
064100                                                                          
064200     IF MSG-DUBBLA-TRANSKODER                                             
064300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I34201                 
064400       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
064500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
064600     ELSE                                                                 
064700       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I34201                  
064800       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
064900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
065000     END-IF                                                               
065100                                                                          
065200     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
065300     MOVE MSG-IDPFK TO MFS-IDPFK                                          
065400     MOVE MFS-IDTRANS TO W-IDTRANS                                        
065500                                                                          
065600     MOVE LOW-VALUE TO MSG-AREA WMSG-AREA                                 
065700     MOVE 'W2O342N1' TO MFS-IDMOD                                         
065800     MOVE '2342' TO MOD-IDTRANS                                           
065900     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
066000                                                                          
066100     IF MSGI-IDLAND-SPR = 'SE'                                            
066200        MOVE '0' TO MFS-KDHUVOMR                                          
066300     END-IF                                                               
066400                                                                          
066500     COMPUTE MAX-MOD-LAENGD = LENGTH OF MOD-W2O34201 + 4                  
066600                                                                          
066700     IF EGEN-MID OR HELP-MID                                              
066800       CONTINUE                                                           
066900     ELSE                                                                 
067000       MOVE SPACE TO MFS-KDTRTYP                                          
067100       MOVE '7' TO MFS-IDPFK                                              
067200     END-IF                                                               
067300                                                                          
067400     PERFORM AA-FIXA-DATUM                                                
067500     .                                                                    
067600     SKIP2                                                                
067700 AA-FIXA-DATUM SECTION.                                                   
067800                                                                          
067900     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM-SEKEL                
068000                                                                          
068100     ACCEPT DAGENS-DATUM   FROM DATE                                      
068200                                                                          
068300     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
068400     MOVE DAGENS-DATUM       TO DAT-I-TIDATUM                             
068500                                                                          
068600     CALL WDATKONV        USING DAT-KDDATFORM DAT-I-TIDATUM               
068700                                DAT-O-TIDATUM DAT-KDSVAR                  
068800                                                                          
068900     IF DAT-KDSVAR-OK                                                     
069000       MOVE DAT-TISEKEL      TO DAGENS-AAR(1:2)                           
069100       MOVE DAT-TIAARP       TO DAGENS-PER                                
069200       MOVE DAT-TIVV         TO WS-VECKA                                  
069300                                DAGENS-VECKA                              
069400     ELSE                                                                 
069500       MOVE 'FELAKTIGT DATUM - DATKONV1' TO FELTEXT                       
069600       CALL FELLOG                                                        
069700     END-IF                                                               
069800                                                                          
069900     MOVE DAGENS-DATUM(1:2)  TO DAGENS-AAR(3:2)                           
070000                                                                          
070100     MOVE 'AARP  '           TO DAT-KDDATFORM                             
070200     MOVE DAT-TIAARP         TO DAT-I-TIDATUM                             
070300                                                                          
070400     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
070500                     DAT-O-TIDATUM DAT-KDSVAR                             
070600                                                                          
070700     IF DAT-KDSVAR-OK                                                     
070800****             COUNT NUMBER OF WEEKS IN CURRENT PERIOD                  
070900                                                                          
071000       COMPUTE WS-ANTAL-VECKOR = DAGENS-VECKA - DAT-TIVV + 1              
071100                                                                          
071200     ELSE                                                                 
071300         STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
071400         DELIMITED BY SIZE INTO FELTEXT                                   
071500         CALL FELLOG                                                      
071600     END-IF                                                               
071700                                                                          
071800     .                                                                    
071900     EJECT                                                                
072000 B-KOLLA-NYCKLAR SECTION.                                                 
072100                                                                          
072200     MOVE JA TO NYCKLAR-SW                                                
072300                                                                          
072400                                                                          
072500*    -- KONTROLL AV IDARTNR                                               
072600                                                                          
072700     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
072800     MOVE ALL '+' TO MSGI-WMSGINIT                                        
072900     MOVE '001'             TO MSGI-KDCALL                                
073000     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
073100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
073200     MOVE '2342'            TO MSGI-IDTRANS                               
073300     IF EGEN-MID                                                          
073400       MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                               
073500       MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                              
073600     END-IF                                                               
073700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
073800                                                                          
073900     IF MSGI-IDLAND-SPR = 'SE'                                            
074000       MOVE +1 TO SPRAK-IX                                                
074100       MOVE 'S  ' TO MED-IDSKYLT                                          
074200                     W-IDSKYLT                                            
074300     ELSE                                                                 
074400       MOVE +2 TO SPRAK-IX                                                
074500       MOVE 'GB ' TO MED-IDSKYLT                                          
074600                     W-IDSKYLT                                            
074700     END-IF                                                               
074800     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
074900     INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                       
075000                                                                          
075100     IF MID-IDARTNR-IN = ALL '+'                                          
075200       CONTINUE                                                           
075300     ELSE                                                                 
075400       MOVE '7'         TO MFS-IDPFK                                      
075500       MOVE SPACE       TO MFS-KDTRTYP                                    
075600     END-IF                                                               
075700                                                                          
075800     IF WS-IDARTNR NUMERIC AND WS-IDARTNR > ZERO                          
075900       MOVE WS-IDARTNR     TO W-IDARTNR                                   
076000     ELSE                                                                 
076100       MOVE NEJ            TO NYCKLAR-SW                                  
076200     END-IF                                                               
076300                                                                          
076400*    -- KONTROLL AV IDDC                                                  
076500                                                                          
076600     MOVE MFS-RENSA-FAELT    TO MOD-IDDC-IN                               
076700                                                                          
076800     IF MID-IDDC-IN NOT = ALL '+'                                         
076900       MOVE '7'              TO MFS-IDPFK                                 
077000       MOVE SPACE            TO MFS-KDTRTYP                               
077100     END-IF                                                               
077200                                                                          
077300     MOVE MSGI-IDDC-KEY    TO WS-IDDC                                     
077400                              W-IDDC-B6                                   
077500     PERFORM IMS-GU-WDB601                                                
077600     IF GOOD-DC                                                           
077700     AND SEGMENT-FINNS                                                    
077800     AND NYCKLAR-OK                                                       
077900       MOVE WS-IDDC        TO WDK7-IDDC                                   
078000                              W-IDDC                                      
078100       MOVE WS-IDARTNR     TO W-IDARTNR                                   
078200       PERFORM BA-GET-REFILL-DISTRICT                                     
078300     ELSE                                                                 
078400       MOVE NEJ            TO NYCKLAR-SW                                  
078500     END-IF                                                               
078600                                                                          
078700***LYNK PARTS ONLY IN EUROPE                                              
078800     MOVE NEJ     TO SW-LYNK-PART                                         
078900     PERFORM IMS-GU-WDK601                                                
079000     IF SEGMENT-FINNS                                                     
079110       IF  ART-KDPRODSL > 30                                              
079120       AND ART-KDPRODSL < 40                                              
079200       AND NDC                                                            
079300         MOVE NEJ          TO NYCKLAR-SW                                  
079400         MOVE JA           TO SW-LYNK-PART                                
079500       END-IF                                                             
079600     END-IF                                                               
079700     MOVE WS-IDARTNR       TO MOD-IDARTNR-UT                              
079800     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
079900     MOVE WS-IDDC          TO MOD-IDDC-UT                                 
080000                              W-IDDC                                      
080100***På kundens begäran i kina export projektet                             
080200     MOVE ZERO             TO MOD-IDDISTR                                 
080300*    MOVE WS-IDDISTR       TO MOD-IDDISTR                                 
080400                                                                          
080500     IF NYCKLAR-FEL                                                       
080600       IF LYNK-PART                                                       
080700         MOVE MED-4           TO MOD-TEMFSFEL                             
080800       ELSE                                                               
080900         MOVE ERR-WRONG-KEY   TO MED-IDMFSFEL                             
081000         CALL WMEDKONV USING MED-WMEDAREA                                 
081100         MOVE MED-MFSFEL      TO MOD-TEMFSFEL                             
081200       END-IF                                                             
081300       PERFORM MFS-RENSA-FAELT-IN                                         
081400       PERFORM MFS-RENSA-FAELT-GRUND                                      
081500       PERFORM MFS-RENSA-FAELT-SLAG                                       
081600       PERFORM MFS-RENSA-FAELT-CLAG                                       
081700       MOVE MFS-RENSA-FAELT   TO MOD-KVLS                                 
081800                                 MOD-KVAKS-S                              
081900                                 MOD-KVBEART                              
082000                                 MOD-KVPB-REF                             
082100                                 MOD-KVREFBER                             
082200     END-IF                                                               
082300     .                                                                    
082400     EJECT                                                                
082500 BA-GET-REFILL-DISTRICT SECTION.                                          
082600                                                                          
082700     IF CDC-SE                                                            
082800        PERFORM IMS-GU-WDK611                                             
082900        IF SEGMENT-FINNS                                                  
083000        AND CLAG-IDDC-REF NOT = SPACES                                    
083100           MOVE CLAG-IDDC-REF         TO W-IDDC-B616                      
083200           PERFORM IMS-GU-WDB616                                          
083300           IF SEGMENT-FINNS                                               
083400              MOVE REF-IDDISTR-REFILL TO WS-IDDISTR                       
083500           END-IF                                                         
083600        END-IF                                                            
083700     ELSE                                                                 
083800        PERFORM IMS-GU-WDK711                                             
083900        IF SEGMENT-FINNS                                                  
084000        AND SLAG-IDDC-REF NOT = SPACES                                    
084100           MOVE SLAG-IDDC-REF         TO W-IDDC-B616                      
084200           PERFORM IMS-GU-WDB616                                          
084300           IF SEGMENT-FINNS                                               
084400              MOVE REF-IDDISTR-REFILL TO WS-IDDISTR                       
084500           END-IF                                                         
084600        END-IF                                                            
084700     END-IF                                                               
084800     .                                                                    
084900     EJECT                                                                
085000 C-FOERSTA-SIDA SECTION.                                                  
085100                                                                          
085200     PERFORM MFS-RENSA-FAELT-IN                                           
085300     .                                                                    
085400     EJECT                                                                
085500 E-SAMMA-SIDA SECTION.                                                    
085600                                                                          
085700     IF MID-INPUT            = ALL '+'                                    
085800       PERFORM MFS-RENSA-FAELT-IN                                         
085900     ELSE                                                                 
086000       IF EGEN-MID OR HELP-MID                                            
086100         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
086200         CALL WMEDKONV USING MED-WMEDAREA                                 
086300         MOVE MED-MFSINF     TO MOD-TEMFSINF                              
086400         MOVE JA             TO SW-MESSAGE                                
086500         PERFORM MFS-LAES-IN-IGEN                                         
086600         PERFORM EA-MID-INDATA-TILL-MOD                                   
086700       ELSE                                                               
086800         PERFORM MFS-RENSA-FAELT-IN                                       
086900       END-IF                                                             
087000     END-IF                                                               
087100     .                                                                    
087200     EJECT                                                                
087300 EA-MID-INDATA-TILL-MOD SECTION.                                          
087400                                                                          
087500     IF MID-TIREFMPB             NOT = ALL '+'                            
087600       MOVE MID-TIREFMPB         TO MOD-TIREFMPB-IN                       
087700     ELSE                                                                 
087800       MOVE MFS-RENSA-FAELT      TO MOD-TIREFMPB-IN                       
087900     END-IF                                                               
088000                                                                          
088100     IF MID-TIPBREOI             NOT = ALL '+'                            
088200       MOVE MID-TIPBREOI         TO MOD-TIPBREOI-IN                       
088300     ELSE                                                                 
088400       MOVE MFS-RENSA-FAELT      TO MOD-TIPBREOI-IN                       
088500     END-IF                                                               
088600     .                                                                    
088700     EJECT                                                                
088800 F-LAES-VISA-INFO SECTION.                                                
088900                                                                          
089000     MOVE MSGI-IDDC-KEY  TO WS1-WS-IDDC                                   
089100                            W-IDDC                                        
089200     IF WS1-CDC-SE                                                        
089300        PERFORM IMS-GU-WDK611                                             
089400        IF SEGMENT-FINNS                                                  
089500           IF CLAG-IDDC-REF    = SPACES                                   
089600              MOVE INF-NOT-REFILL-PART   TO MED-IDMFSFEL                  
089700              CALL WMEDKONV USING MED-WMEDAREA                            
089800              MOVE MED-MFSFEL            TO MOD-TEMFSFEL                  
089900              MOVE NEJ                   TO INDATA-SW                     
090000              MOVE JA                    TO SW-MESSAGE                    
090100                                                                          
090200              PERFORM MFS-RENSA-FAELT-IN                                  
090300              PERFORM MFS-RENSA-FAELT-GRUND                               
090400              PERFORM MFS-RENSA-FAELT-SLAG                                
090500              PERFORM MFS-RENSA-FAELT-CLAG                                
090600           ELSE                                                           
090700              MOVE CLAG-IDDC-REF  TO W-IDDC-REF                           
090800                                     MOD-IDDC-REF                         
090900              MOVE WS-FC-TEXT-CDC TO MOD-FC-TEXT                          
091000           END-IF                                                         
091100        END-IF                                                            
091200     ELSE                                                                 
091300        PERFORM IMS-GU-WDK711                                             
091400        IF SEGMENT-FINNS                                                  
091500          IF SLAG-IDDC-REF = SPACES                                       
091600           IF WS1-NDC-CN OR WS1-NDC-US                                    
091700             MOVE INF-NOT-REFILL-PART   TO MED-IDMFSFEL                   
091800             CALL WMEDKONV USING MED-WMEDAREA                             
091900             MOVE MED-MFSFEL            TO MOD-TEMFSFEL                   
092000             MOVE NEJ                   TO INDATA-SW                      
092100                                                                          
092200             PERFORM MFS-RENSA-FAELT-IN                                   
092300             PERFORM MFS-RENSA-FAELT-GRUND                                
092400             PERFORM MFS-RENSA-FAELT-SLAG                                 
092500             PERFORM MFS-RENSA-FAELT-CLAG                                 
092600           END-IF                                                         
092700          ELSE                                                            
092800           MOVE SLAG-IDDC-REF  TO WS-IDDC-REF                             
092900                                  MOD-IDDC-REF                            
093000                                  WS-IDDC-LAND                            
093100           MOVE WS-FC-TEXT-XDC TO MOD-FC-TEXT                             
093200          END-IF                                                          
093300        END-IF                                                            
093400     END-IF                                                               
093500                                                                          
093600     IF  WS1-CDC-SE                                                       
093700     AND INDATA-OK                                                        
093800         PERFORM FD-CDC-REFILL-INFO                                       
093900     ELSE                                                                 
094000        IF INDATA-OK                                                      
094100          PERFORM IMS-GU-WDK601                                           
094200          IF SEGMENT-FINNS                                                
094300            PERFORM FA-LAES-GRUNDDATA                                     
094400            PERFORM FB-LAES-DC-INFO                                       
094500            PERFORM FC-LAES-CDC-INFO                                      
094600            IF WS-KDERS                    >  0                           
094700              IF WS-KDERS                  <  29                          
094800                MOVE ARTIKEL-ERSATT        TO MED-IDMFSFEL                
094900                CALL WMEDKONV USING MED-WMEDAREA                          
095000                MOVE MED-MFSFEL            TO MOD-TEMFSFEL                
095100              ELSE                                                        
095200                MOVE ARTIKEL-UTGANGEN      TO MED-IDMFSFEL                
095300                CALL WMEDKONV USING MED-WMEDAREA                          
095400                MOVE MED-MFSFEL            TO MOD-TEMFSFEL                
095500              END-IF                                                      
095600            END-IF                                                        
095700                                                                          
095800          ELSE                                                            
095900            MOVE ARTIKEL-SAKNAS            TO MED-IDMFSFEL                
096000            CALL WMEDKONV USING MED-WMEDAREA                              
096100            MOVE MED-MFSFEL                TO MOD-TEMFSFEL                
096200                                                                          
096300            PERFORM MFS-RENSA-FAELT-GRUND                                 
096400            PERFORM MFS-RENSA-FAELT-SLAG                                  
096500            MOVE MFS-RENSA-FAELT   TO MOD-KVLS                            
096600                                      MOD-KVAKS-S                         
096700                                      MOD-KVBEART                         
096800                                      MOD-KVPB-REF                        
096900                                      MOD-KVREFBER                        
097000            PERFORM MFS-RENSA-FAELT-CLAG                                  
097100          END-IF                                                          
097200        END-IF                                                            
097300     END-IF                                                               
097400     .                                                                    
097500     EJECT                                                                
097600 FA-LAES-GRUNDDATA SECTION.                                               
097700                                                                          
097800     MOVE ART-TIFINLV         TO MOD-TIFINLV                              
097900     MOVE ART-FLERS           TO MOD-FLERS                                
098000                                                                          
098100*    --- HÄMTA BENÄMNING                                                  
098200                                                                          
098300     PERFORM IMS-GU-BENA01-BSEQ                                           
098400     IF SEGMENT-FINNS                                                     
098500       PERFORM IMS-GNP-BENA11                                             
098600       IF SEGMENT-FINNS                                                   
098700         MOVE BENA-TEXT-BEART TO MOD-BEART-SVE                            
098800       ELSE                                                               
098900         MOVE MFS-RENSA-FAELT TO MOD-BEART-SVE                            
099000       END-IF                                                             
099100     ELSE                                                                 
099200       MOVE MFS-RENSA-FAELT   TO MOD-BEART-SVE                            
099300     END-IF                                                               
099400     .                                                                    
099500     EJECT                                                                
099600 FB-LAES-DC-INFO SECTION.                                                 
099700                                                                          
099800     PERFORM FBA-HAMTA-VV-I-PER                                           
099900                                                                          
100000     PERFORM FBB-BEHANDLA-ORDERINGGANG                                    
100100                                                                          
100200     PERFORM FBC-BEHANDLA-DC-INFO                                         
100300     .                                                                    
100400     EJECT                                                                
100500 FBA-HAMTA-VV-I-PER SECTION.                                              
100600                                                                          
100700     MOVE +1             TO INDX                                          
100800     MOVE DAGENS-PER     TO WS-TIAAPER                                    
100900                                                                          
101000     IF TIAA = 00   MOVE    99 TO TIAA                                    
101100     ELSE           SUBTRACT 1 FROM TIAA                                  
101200     END-IF                                                               
101300                                                                          
101400*    --- TA FRAM HUR MÅNGA VECKOR DET VAR FÖREGÅENDE ÅR                   
101500     MOVE TIAA           TO WS-AAR                                        
101600     MOVE 53             TO WS-VV                                         
101700     MOVE 'AAVV  '       TO DAT-KDDATFORM                                 
101800     MOVE TIAAVV         TO DAT-I-TIDATUM                                 
101900     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
102000                         DAT-O-TIDATUM DAT-KDSVAR                         
102100     IF DAT-KDSVAR-OK                                                     
102200       MOVE 53           TO ANT-VV                                        
102300     ELSE                                                                 
102400       MOVE 52           TO ANT-VV                                        
102500     END-IF                                                               
102600                                                                          
102700*    --- FYLL I VECKONR FÖR PERIODERNA                                    
102800                                                                          
102900     MOVE 'AARP  '       TO DAT-KDDATFORM                                 
103000     MOVE TIAAPER        TO DAT-I-TIDATUM                                 
103100     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
103200                         DAT-O-TIDATUM DAT-KDSVAR                         
103300     IF DAT-KDSVAR-OK                                                     
103400       IF PER            = 1                                              
103500         MOVE 1          TO WS-PER(INDX)                                  
103600                            WS-FORSTA-V(INDX)                             
103700       ELSE                                                               
103800         MOVE PER        TO WS-PER(INDX)                                  
103900         MOVE DAT-TIVV   TO WS-FORSTA-V(INDX)                             
104000       END-IF                                                             
104100     ELSE                                                                 
104200       MOVE 'FELAKTIGT DATUM - DATKONV2' TO FELTEXT                       
104300       CALL FELLOG                                                        
104400     END-IF                                                               
104500                                                                          
104600     PERFORM UNTIL INDX       >  12                                       
104700       ADD +1                 TO PER                                      
104800       IF PER                 >  12                                       
104900         ADD +1               TO TIAA                                     
105000         MOVE +1              TO PER                                      
105100       END-IF                                                             
105200                                                                          
105300       MOVE TIAAPER           TO DAT-I-TIDATUM                            
105400       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
105500                           DAT-O-TIDATUM DAT-KDSVAR                       
105600       IF DAT-KDSVAR-OK                                                   
105700         IF PER               =  1                                        
105800           MOVE ANT-VV        TO WS-SISTA-V(INDX)                         
105900         ELSE                                                             
106000           COMPUTE WS-SISTA-V(INDX) = DAT-TIVV - 1                        
106100         END-IF                                                           
106200         ADD +1               TO INDX                                     
106300         IF INDX              <= 12                                       
106400           MOVE PER           TO WS-PER(INDX)                             
106500           IF PER             =  1                                        
106600             MOVE +1          TO WS-FORSTA-V(INDX)                        
106700           ELSE                                                           
106800             MOVE DAT-TIVV    TO WS-FORSTA-V(INDX)                        
106900           END-IF                                                         
107000         END-IF                                                           
107100       ELSE                                                               
107200         MOVE 'FELAKTIGT DATUM - DATKONV3' TO FELTEXT                     
107300         CALL FELLOG                                                      
107400       END-IF                                                             
107500     END-PERFORM                                                          
107600*    --- OM VECKOR I FÖRSTA OCH SISTA PERIODEN ÖVERLAPPAR,                
107700*    --- RÄTTA I FÖRSTA (DVS DEN ÄLDSTA) PERIODEN.                        
107800     IF WS-FORSTA-V(1)        = WS-SISTA-V(12)                            
107900     OR WS-FORSTA-V(1)        = WS-SISTA-V(12) - 1                        
108000       COMPUTE WS-FORSTA-V(1) = WS-SISTA-V(12) + 1                        
108100     END-IF                                                               
108200     .                                                                    
108300     EJECT                                                                
108400 FBB-BEHANDLA-ORDERINGGANG SECTION.                                       
108500                                                                          
108600     PERFORM FBA-HAMTA-VV-I-PER                                           
108700                                                                          
108800*    --- FYLL PÅ TABELLEN MED OI/OT                                       
108900                                                                          
109000     PERFORM IMS-GU-WDL711                                                
109100     IF SEGMENT-FINNS                                                     
109200       PERFORM IMS-GU-WDL411                                              
109300       IF SEGMENT-SAKNAS                                                  
109400******** IF PART/DC FROM WDL7 IS MISSING IN WDL4 THEN L411-AREA           
109500******** MUST BE ZEROED BCZ SOME ITEMS ARE USED IN COMPUTES BELOW         
109600         INITIALIZE OIHD-WDL411                                           
109700       END-IF                                                             
109800                                                                          
109900        MOVE +1                          TO INDX                          
110000        MOVE ZERO                        TO WS-KVOI-TOT12                 
110100        PERFORM UNTIL INDX               >  12                            
110200          MOVE WS-FORSTA-V(INDX)         TO VV                            
110300          PERFORM UNTIL VV               >  WS-SISTA-V(INDX)              
110400            ADD DC-KVOT-RULL(VV)         TO WS-KVOT(INDX)                 
110500                                               WS-KVOT-TOT12              
110600            ADD DC-KVOT-REF-RULL(VV)     TO WS-KVOT(INDX)                 
110700                                               WS-KVOT-TOT12              
110800            ADD DC-KVOT-CDC-RULL(VV)     TO WS-KVOT-CDC(INDX)             
110900                                               WS-KVOT-CDC-TOT12          
111000            ADD DC-KVOI-RULL(VV)         TO WS-KVOI(INDX)                 
111100                                               WS-KVOI-TOT12              
111200            ADD DC-KVOI-REF-RULL(VV)     TO WS-KVOI-REF(INDX)             
111300                                               WS-KVOI-REF-TOT12          
111400            IF INDX > 6                                                   
111500              ADD DC-KVOT-RULL(VV)       TO WS-KVOT-TOT6                  
111600              ADD DC-KVOT-REF-RULL(VV)   TO WS-KVOT-TOT6                  
111700              ADD DC-KVOT-CDC-RULL(VV)   TO WS-KVOT-CDC-TOT6              
111800              ADD DC-KVOI-RULL(VV)       TO WS-KVOI-TOT6                  
111900              ADD DC-KVOI-REF-RULL(VV)   TO WS-KVOI-REF-TOT6              
112000            END-IF                                                        
112100            ADD +1                       TO VV                            
112200          END-PERFORM                                                     
112300          ADD +1                         TO INDX                          
112400        END-PERFORM                                                       
112500                                                                          
112600*       --- FLYTTA UT TABELLEN TILL MOD:EN                                
112700*       --- ENDAST DE SENASTE SEX PERIODERNA VISAS                        
112800                                                                          
112900        MOVE +7               TO INDX                                     
113000        MOVE +1                  TO MOD-IX                                
113100        PERFORM UNTIL INDX > +12                                          
113200          MOVE WS-PER(INDX)      TO MOD-TIPP(MOD-IX)                      
113300          INSPECT MOD-TIPP(MOD-IX) REPLACING LEADING ZERO BY SPACE        
113400          MOVE WS-FORSTA-V(INDX) TO WS-FOM                                
113500          MOVE WS-SISTA-V(INDX)  TO WS-TOM                                
113600          MOVE WS-FOM-TOM        TO MOD-TIVV-FOM-TOM(MOD-IX)              
113700          MOVE WS-KVOT(INDX)     TO MOD-KVOT(MOD-IX)                      
113800          MOVE WS-KVOT-CDC(INDX) TO MOD-KVOT-CDC(MOD-IX)                  
113900          MOVE WS-KVOI(INDX)     TO MOD-KVOI(MOD-IX)                      
114000          MOVE WS-KVOI-REF(INDX) TO MOD-KVOI-REFILL(MOD-IX)               
114100          ADD +1                 TO INDX MOD-IX                           
114200        END-PERFORM                                                       
114300                                                                          
114400*       --- INNEVARANDE PERIODS OI/OT                                     
114500                                                                          
114600        MOVE +1                   TO INDX                                 
114700        PERFORM UNTIL INDX        >  +5                                   
114800          ADD DC-KVOT-INNEV(INDX)     TO WS-KVOT-INNEV                    
114900          ADD DC-KVOT-PP-INNEV(INDX)  TO WS-KVOT-INNEV                    
115000          ADD DC-KVOT-REF-INNEV(INDX) TO WS-KVOT-INNEV                    
115100          ADD DC-KVOT-CDC-INNEV(INDX) TO WS-KVOT-CDC-INNEV                
115200          ADD DC-KVOI-INNEV(INDX)     TO WS-KVOI-INNEV                    
115300          ADD DC-KVOI-PP-INNEV(INDX)  TO WS-KVOI-INNEV                    
115400          ADD DC-KVOI-REF-INNEV(INDX) TO WS-KVOI-REF-INNEV                
115500          ADD +1                      TO INDX                             
115600        END-PERFORM                                                       
115700        MOVE WS-KVOT-INNEV            TO MOD-KVOT-INNEV                   
115800        MOVE WS-KVOT-CDC-INNEV        TO MOD-KVOT-CDC-INNEV               
115900        MOVE WS-KVOI-INNEV            TO MOD-KVOI-INNEV                   
116000        MOVE WS-KVOI-REF-INNEV        TO MOD-KVOI-REF-INNEV               
116100                                                                          
116200*       --- SUMMA RULLANDE 12 PERIODERS OI/OT                             
116300*       --- OCH RULLANDE 6 PERIODERS OI/OT                                
116400                                                                          
116500        MOVE WS-KVOT-TOT12     TO MOD-KVOT-RULL-12                        
116600        MOVE WS-KVOT-CDC-TOT12 TO MOD-KVOT-CDC-RULL-12                    
116700        MOVE WS-KVOI-TOT12     TO MOD-KVOI-RULL-12                        
116800        MOVE WS-KVOI-REF-TOT12 TO MOD-KVOI-REF-RULL-12                    
116900        MOVE WS-KVOT-TOT6      TO MOD-KVOT-RULL-6                         
117000        MOVE WS-KVOT-CDC-TOT6  TO MOD-KVOT-CDC-RULL-6                     
117100        MOVE WS-KVOI-TOT6      TO MOD-KVOI-RULL-6                         
117200        MOVE WS-KVOI-REF-TOT6  TO MOD-KVOI-REF-RULL-6                     
117300                                                                          
117400        MOVE DAGENS-DATUM TO WS-TIAAMMDD                                  
117500                                                                          
117600*       --- HÄMTA FÖREGÅENDE ÅRS OI/OT                                    
117700                                                                          
117800        IF WS-TIAA = 00    MOVE 99 TO WS-AA                               
117900        ELSE               COMPUTE WS-AA = WS-TIAA - 1                    
118000        END-IF                                                            
118100        MOVE WS-ARHAA                 TO MOD-TIAA-FORE(2)                 
118200        MOVE ZEROS                    TO WS-KVOT-FORE                     
118300        COMPUTE WS-KVOT-FORE = OIHD-KVOT-FOREG(1) +                       
118400                               OIHD-KVOT-REF-FOREG(1)                     
118500        MOVE WS-KVOT-FORE             TO MOD-KVOT-FORE(2)                 
118600        MOVE OIHD-KVOT-CDC-FOREG(1)   TO MOD-KVOT-CDC-FORE(2)             
118700        MOVE +1 TO IX                                                     
118800        PERFORM UNTIL IX > 12                                             
118900           COMPUTE WS-KVOI-FOREG-1 =                                      
119000                   WS-KVOI-FOREG-1 + OIHD-KVOI (1, IX)                    
119100           ADD +1 TO IX                                                   
119200        END-PERFORM                                                       
119300        MOVE WS-KVOI-FOREG-1          TO MOD-KVOI-FORE(2)                 
119400                                                                          
119500        MOVE ZERO TO WS-KVOI-FOREG-1                                      
119600        MOVE +1 TO IX                                                     
119700        PERFORM UNTIL IX > 12                                             
119800           COMPUTE WS-KVOI-FOREG-1 =                                      
119900                   WS-KVOI-FOREG-1 + OIHD-KVOI-REFILL (1, IX)             
120000           ADD +1 TO IX                                                   
120100        END-PERFORM                                                       
120200        MOVE WS-KVOI-FOREG-1          TO MOD-KVOI-REFILL-FORE(2)          
120300*       --- HÄMTA FÖR-FÖREGÅENDE ÅRS OI/OT                                
120400                                                                          
120500        IF WS-AA = 00      MOVE 99 TO WS-AA                               
120600        ELSE               COMPUTE WS-AA = WS-AA - 1                      
120700        END-IF                                                            
120800        MOVE WS-ARHAA                 TO MOD-TIAA-FORE(1)                 
120900        MOVE ZEROS                    TO WS-KVOT-FORE                     
121000        COMPUTE WS-KVOT-FORE = OIHD-KVOT-FOREG(2) +                       
121100                               OIHD-KVOT-REF-FOREG(2)                     
121200        MOVE WS-KVOT-FORE             TO MOD-KVOT-FORE(1)                 
121300        MOVE OIHD-KVOT-CDC-FOREG(2)   TO MOD-KVOT-CDC-FORE(1)             
121400        MOVE +1 TO IX                                                     
121500        PERFORM UNTIL IX > 12                                             
121600           COMPUTE WS-KVOI-FOREG-2 = WS-KVOI-FOREG-2 +                    
121700                                     OIHD-KVOI (2, IX)                    
121800           ADD +1 TO IX                                                   
121900        END-PERFORM                                                       
122000        MOVE WS-KVOI-FOREG-2          TO MOD-KVOI-FORE(1)                 
122100                                                                          
122200        MOVE ZERO TO WS-KVOI-FOREG-2                                      
122300        MOVE +1 TO IX                                                     
122400        PERFORM UNTIL IX > 12                                             
122500           COMPUTE WS-KVOI-FOREG-2 = WS-KVOI-FOREG-2 +                    
122600                                     OIHD-KVOI-REFILL(2, IX)              
122700                                                                          
122800           ADD +1 TO IX                                                   
122900        END-PERFORM                                                       
123000        MOVE WS-KVOI-FOREG-2          TO MOD-KVOI-REFILL-FORE(1)          
123100     ELSE                                                                 
123200        PERFORM MFS-RENSA-FAELT-SLAG                                      
123300     END-IF                                                               
123400     .                                                                    
123500     EJECT                                                                
123600 FBC-BEHANDLA-DC-INFO SECTION.                                            
123700                                                                          
123800     PERFORM IMS-GHU-WDK711                                               
123900     IF SEGMENT-FINNS                                                     
124000                                                                          
124100*        --- HÄMTA PROGNOS OCH PROGNOSDATUM                               
124200         MOVE SLAG-KVPB-REF     TO MOD-KVPB-REF                           
124300         MOVE SLAG-KVPBREOI     TO MOD-KVPBREOI                           
124400         IF SLAG-TIREFMPB       > 0                                       
124500           MOVE SLAG-TIREFMPB   TO MOD-TIREFMPB                           
124600         ELSE                                                             
124700           MOVE MFS-RENSA-FAELT TO MOD-TIREFMPB                           
124800         END-IF                                                           
124900         IF SLAG-TIPBREOI       > 0                                       
125000           MOVE SLAG-TIPBREOI   TO MOD-TIPBREOI                           
125100         ELSE                                                             
125200           MOVE MFS-RENSA-FAELT TO MOD-TIPBREOI                           
125300         END-IF                                                           
125400                                                                          
125500                                                                          
125600*        --- HÄMTA SALDON                                                 
125700         MOVE SLAG-KVREFBER   TO MOD-KVREFBER                             
125800         COMPUTE WS-BALANCE-SLAG = SLAG-KVLS       -                      
125900                                   SLAG-KVOKS-DAG  -                      
126000                                   SLAG-KVOKS-BULK -                      
126100                                   SLAG-KVROS-DAG  -                      
126200                                   SLAG-KVROS-BULK -                      
126300                                   SLAG-KVSPARR-KVAL                      
126400                                                                          
126500         COMPUTE WS-KVAKS     =  SLAG-KVAKS-PAV +                         
126600                                 SLAG-KVAKS-SDC                           
126700                                                                          
126800         MOVE WS-BALANCE-SLAG      TO MOD-KVLS                            
126900         MOVE WS-KVAKS             TO MOD-KVAKS-S                         
127000         MOVE SLAG-KVBEART         TO MOD-KVBEART                         
127100         MOVE SLAG-DAREFESC        TO MOD-DAREFESC                        
127200         MOVE SLAG-DAREFESC-REOI   TO MOD-DAREFESC-REOI                   
127300         IF WS-IDDC-REF NOT = WC-CDC-SE OR WC-CDC-TR                      
127400           PERFORM IMS-GU-WDK722                                          
127500           IF SEGMENT-FINNS                                               
127600              MOVE XLAG-IDANSK     TO MOD-IDANSK                          
127700           ELSE                                                           
127800              MOVE MFS-RENSA-FAELT TO MOD-IDANSK                          
127900           END-IF                                                         
128000**LÄGGER WS-IDDC-REF TILL BÅDE W-IDDC + W-IDDC-REF                        
128100**FÖR ATT ANVÄNDA W-IDDC TILL ATT LÄSA REFILLANDE DC                      
128200**OCH W-IDDC-REF FÖR ATT LÄSA AVISERAT ANTAL I FDBA-SECTION               
128300           MOVE WS-IDDC-REF        TO W-IDDC                              
128400                                      W-IDDC-REF                          
128500           PERFORM IMS-GU-WDK711                                          
128600           MOVE ZEROS              TO WS-KVDISP                           
128700           COMPUTE WS-KVDISP       = SLAG-KVLS       -                    
128800                                     SLAG-KVRESS     -                    
128900                                     SLAG-KVOKS-BULK -                    
129000                                     SLAG-KVOKS-DAG                       
129100                                                                          
129200           MOVE WS-KVDISP          TO MOD-KVDISP                          
129300                                                                          
129400           MOVE ZEROS     TO WS-KVPB-TOT                                  
129500           COMPUTE WS-KVPB-TOT = SLAG-KVPB-REF +                          
129600                                     SLAG-KVPBREOI                        
129700           MOVE WS-KVPB-TOT        TO MOD-KVPB-TOT                        
129800                                                                          
129900           MOVE ZEROS              TO WS-KVART-FORAVIS                    
130000****OM REFILLANDE DC ÄR ANSKAFFAT VISA FRÅN w6D1 ANNARS PAV+BEART         
130100           IF SLAG-IDDC-REF = SPACE                                       
130200             PERFORM FDBA-GET-PREADV-INFO                                 
130300           ELSE                                                           
130400             COMPUTE WS-KVART-FORAVIS = SLAG-KVAKS-PAV +                  
130500                                        SLAG-KVBEART                      
130600             MOVE WS-KVART-FORAVIS TO MOD-KVART-FORAVIS                   
130700           END-IF                                                         
130800           MOVE SLAG-KVAKS-SDC     TO MOD-KVAKS                           
130900*****läs tillbaka behandlat dc istället för refillande dc                 
131000           MOVE WS-IDDC   TO W-IDDC                                       
131100           PERFORM IMS-GU-WDK711                                          
131200                                                                          
131300         END-IF                                                           
131400     ELSE                                                                 
131500         MOVE ARTIKEL-SAKNAS-SDC TO MED-IDMFSFEL                          
131600         CALL WMEDKONV USING MED-WMEDAREA                                 
131700         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
131800         MOVE MFS-RENSA-FAELT TO MOD-KVLS                                 
131900                                 MOD-KVAKS-S                              
132000                                 MOD-KVBEART                              
132100                                 MOD-KVPB-REF                             
132200                                 MOD-KVREFBER                             
132300                                 MOD-TIREFMPB                             
132400                                 MOD-TIPBREOI                             
132500     END-IF                                                               
132600     .                                                                    
132700     EJECT                                                                
132800 FC-LAES-CDC-INFO SECTION.                                                
132900                                                                          
133000     PERFORM FCA-SUMMERA-OKS                                              
133100                                                                          
133200     PERFORM IMS-GNP-WDK611                                               
133300     IF SEGMENT-FINNS                                                     
133400        PERFORM S02-GET-BESPRIS                                           
133500        IF WS-IDDC-REF = WC-CDC-SE OR WC-CDC-TR OR SPACES                 
133600          MOVE CLAG-IDANSK     TO MOD-IDANSK                              
133700          COMPUTE WS-KVAKS       =  CLAG-KVAKS-CDC +                      
133800                                    CLAG-KVAKS-PAV +                      
133900                                    CLAG-KVAKS-T                          
134000          MOVE WS-KVAKS        TO MOD-KVAKS                               
134100          COMPUTE WS-BALANCE-CLAG = CLAG-KVLS     -                       
134200                                    CLAG-KVROS    -                       
134300                                    WS-KVOKS-TOT  -                       
134400                                    CLAG-KVSPARR-KVAL                     
134500                                                                          
134600          MOVE WS-BALANCE-CLAG TO MOD-KVDISP                              
134700          PERFORM FCB-HAEMTA-KVPB                                         
134800          MOVE WS-KVPB         TO MOD-KVPB-TOT                            
134900          MOVE WS-STD-TEXT     TO MOD-PRICE-TEXT                          
135000          MOVE CLAG-PRARTSTD   TO MOD-PRARTSTD                            
135100        END-IF                                                            
135200        IF NDC-CN OR NDC-NA                                               
135300          MOVE SLAG-IDDC TO WS-IDDC-LAND                                  
135400          PERFORM S9-SEARCH-IDLAND                                        
135500          PERFORM IMS-GU-WDK712                                           
135600          IF SEGMENT-FINNS                                                
135700            MOVE WS-MTRL-TEXT      TO MOD-PRICE-TEXT                      
135800            MOVE LART-PRMATRL      TO MOD-PRARTSTD                        
135900          ELSE                                                            
136000            MOVE WS-MTRL-TEXT      TO MOD-PRICE-TEXT                      
136100            MOVE ZERO              TO MOD-PRARTSTD                        
136200            MOVE 'NO MATERIALPRICE'  TO MOD-TEMFSINF                      
136300          END-IF                                                          
136400        END-IF                                                            
136500        IF SLAG-IDDC-REF NOT = SPACES                                     
136700          MOVE SLAG-IDDC-REF TO WS-IDDC-LAND                              
136800          PERFORM S9-SEARCH-IDLAND                                        
136900          PERFORM IMS-GU-WDK712                                           
137000          IF SEGMENT-FINNS                                                
137100            IF LART-KVQPACK-3 > 0                                         
137200              MOVE LART-KVQPACK-3 TO MOD-KVQPACK-3                        
137300            ELSE                                                          
137400              MOVE CLAG-KVQPACK-3 TO MOD-KVQPACK-3                        
137500            END-IF                                                        
137600          ELSE                                                            
137700            MOVE CLAG-KVQPACK-3 TO MOD-KVQPACK-3                          
137800          END-IF                                                          
137900        ELSE                                                              
138000          MOVE CLAG-KVQPACK-3 TO MOD-KVQPACK-3                            
138100        END-IF                                                            
138200        MOVE CLAG-KDERS        TO MOD-KDERS                               
138300                                  WS-KDERS                                
138400        MOVE CLAG-KVQPACK-1    TO MOD-KVQPACK-1                           
138500                                                                          
138600                                                                          
138700                                                                          
138800     ELSE                                                                 
138900        MOVE MFS-RENSA-FAELT   TO MOD-IDANSK                              
139000                                  MOD-PRICE-TEXT                          
139100                                  MOD-PRARTSTD                            
139200                                  MOD-KDERS                               
139300                                  MOD-KVQPACK-1                           
139400                                  MOD-KVPB-TOT                            
139500                                  MOD-KVQPACK-3                           
139600                                  MOD-KVAKS                               
139700                                  MOD-KVDISP                              
139800     END-IF                                                               
139900                                                                          
140000     MOVE WS-IDARTNR TO W-IDARTNR-HSEQ                                    
140100     MOVE ZERO       TO WS-KVART-TOT-C1                                   
140200     IF WS-IDDC-REF = WC-CDC-SE OR WC-CDC-TR OR SPACES                    
140300       MOVE WS-KVART-TOT-C1    TO MOD-KVART-FORAVIS                       
140400     END-IF                                                               
140500     .                                                                    
140600     EJECT                                                                
140700 FCA-SUMMERA-OKS SECTION.                                                 
140800                                                                          
140900     PERFORM IMS-GU-ARTM01                                                
141000     IF SEGMENT-FINNS                                                     
141100       COMPUTE WS-KVOKS-TOT =  ARTM-ART-KVOKS-BULK +                      
141200                               ARTM-ART-KVOKS-DAG  +                      
141300                               ARTM-ART-KVOKS-VOR                         
141400     ELSE                                                                 
141500       MOVE ZERO            TO WS-KVOKS-TOT                               
141600     END-IF                                                               
141700     .                                                                    
141800     EJECT                                                                
141900                                                                          
142000 FCB-HAEMTA-KVPB SECTION.                                                 
142100                                                                          
142200*  ----  CLAG-KVPB ÄR PROGNOS FÖR EN 6-VECKORS PERIOD                     
142300*  ----  DETTA GÖRS OM OCH VISAS SOM EN MÅNADSPERIOD                      
142400                                                                          
142500     COMPUTE WS-KVPB ROUNDED =                                            
142600               (CLAG-KVPB-SATS + CLAG-KVPB-SEP + CLAG-KVPB-TPO)           
142700*    MOVE DCS-IDDC         TO W-IDDC                                      
142800*    PERFORM IMS-GU-K711                                                  
142900*    IF SEGMENT-FINNS                                                     
143000** ----  SLAG-KVPB ÄR PROGNOS FÖR EN MÅNADSPERIOD                         
143100*      ADD SLAG-KVPB-REF   TO WS-KVPB                                     
143200*    END-IF                                                               
143300                                                                          
143400*                                                                         
143500*    PERFORM IMS-GN-WDB601                                                
143600*    PERFORM UNTIL SEGMENT-SLUT                                           
143700*      IF NEXT-DCS-CDC OR NEXT-DCS-DDC                                    
143800*         CONTINUE                                                        
143900*      ELSE                                                               
144000*         MOVE NEXT-DCS-IDDC     TO W-IDDC                                
144100*         PERFORM IMS-GU-K711                                             
144200*         IF SEGMENT-FINNS                                                
144300*  ----  SLAG-KVPB ÄR PROGNOS FÖR EN MÅNADSPERIOD                         
144400*            IF SLAG-IDDC-REF = WC-CDC-SE                                 
144500*              ADD SLAG-KVPB-REF TO WS-KVPB                               
144600*            END-IF                                                       
144700*         END-IF                                                          
144800*      END-IF                                                             
144900*      PERFORM IMS-GN-WDB601                                              
145000*    END-PERFORM                                                          
145100     .                                                                    
145200     EJECT                                                                
145300 FD-CDC-REFILL-INFO   SECTION.                                            
145400                                                                          
145500     PERFORM IMS-GU-WDK601                                                
145600     IF SEGMENT-FINNS                                                     
145700        PERFORM FA-LAES-GRUNDDATA                                         
145800        PERFORM FDA-LAES-CDC-INFO                                         
145900        PERFORM FDB-LAES-DC-INFO                                          
146000                                                                          
146100        IF MESSAGE-NEJ                                                    
146200           IF CLAG-KVPB-SATS            > ZERO                            
146300              MOVE MED-1               TO MOD-TEMFSINF                    
146400           ELSE                                                           
146500              IF CLAG-KVPB-TPO          > ZERO                            
146600                 MOVE MED-2            TO MOD-TEMFSINF                    
146700              END-IF                                                      
146800           END-IF                                                         
146900                                                                          
147000*          --- CHECK IF ARTICLE IS PART OF CAMPAIGN                       
147100           PERFORM FDC-CHECK-CAMPAIGN                                     
147200           IF CAMPAIGN-JA                                                 
147300              MOVE MED-3               TO MOD-TEMFSINF                    
147400           END-IF                                                         
147500        END-IF                                                            
147600                                                                          
147700        IF WS-KDERS                     >  0                              
147800          IF WS-KDERS                   <  29                             
147900            MOVE ARTIKEL-ERSATT        TO MED-IDMFSFEL                    
148000            CALL WMEDKONV USING MED-WMEDAREA                              
148100            MOVE MED-MFSFEL            TO MOD-TEMFSFEL                    
148200          ELSE                                                            
148300            MOVE ARTIKEL-UTGANGEN      TO MED-IDMFSFEL                    
148400            CALL WMEDKONV USING MED-WMEDAREA                              
148500            MOVE MED-MFSFEL            TO MOD-TEMFSFEL                    
148600          END-IF                                                          
148700        END-IF                                                            
148800     ELSE                                                                 
148900        MOVE ARTIKEL-SAKNAS            TO MED-IDMFSFEL                    
149000        CALL WMEDKONV USING MED-WMEDAREA                                  
149100        MOVE MED-MFSFEL                TO MOD-TEMFSFEL                    
149200                                                                          
149300        PERFORM MFS-RENSA-FAELT-GRUND                                     
149400        PERFORM MFS-RENSA-FAELT-SLAG                                      
149500        MOVE MFS-RENSA-FAELT   TO MOD-KVLS                                
149600                                  MOD-KVAKS-S                             
149700                                  MOD-KVBEART                             
149800                                  MOD-KVPB-REF                            
149900                                  MOD-KVREFBER                            
150000         PERFORM MFS-RENSA-FAELT-CLAG                                     
150100     END-IF                                                               
150200     .                                                                    
150300     EJECT                                                                
150400 FDA-LAES-CDC-INFO   SECTION.                                             
150500                                                                          
150600     PERFORM FDAA-BEHANDLA-ORDERINGGANG                                   
150700                                                                          
150800     PERFORM FDAB-BEHANDLA-CDC-INFO                                       
150900     .                                                                    
151000     EJECT                                                                
151100 FDAA-BEHANDLA-ORDERINGGANG  SECTION.                                     
151200                                                                          
151300*    -- GET TOTAL ORDER STATISTICS FOR YEAR-2                             
151400                                                                          
151500     SUBTRACT 2 FROM DAGENS-AAR GIVING WS-FOREG-AAR                       
151600                                                                          
151700     MOVE WS-FOREG-AAR       TO W-TIAAAA                                  
151800     MOVE DAGENS-PER         TO WS-TIAAPER                                
151900     SUBTRACT 2            FROM TIAA                                      
152000                                                                          
152100     PERFORM FDAAA-ORDERINGANG-STAT-PREV-AA                               
152200                                                                          
152300     MOVE WS-FOREG-AAR       TO MOD-TIAA-FORE (1)                         
152400     MOVE WS-KVOT-FORE       TO MOD-KVOT-FORE (1)                         
152500     MOVE WS-KVOI-FOREG-1    TO MOD-KVOI-FORE (1)                         
152600     MOVE WS-KVOI-FOREG-2    TO MOD-KVOI-REFILL-FORE (1)                  
152700     MOVE ZERO               TO MOD-KVOT-CDC-FORE    (1)                  
152800                                                                          
152900*    -- GET TOTAL ORDER STATISTICS FOR YEAR-1                             
153000                                                                          
153100     SUBTRACT 1 FROM DAGENS-AAR GIVING WS-FOREG-AAR                       
153200                                                                          
153300     MOVE WS-FOREG-AAR       TO W-TIAAAA                                  
153400     MOVE DAGENS-PER         TO WS-TIAAPER                                
153500     SUBTRACT 1            FROM TIAA                                      
153600                                                                          
153700     PERFORM FDAAA-ORDERINGANG-STAT-PREV-AA                               
153800                                                                          
153900     MOVE WS-FOREG-AAR       TO MOD-TIAA-FORE (2)                         
154000     MOVE WS-KVOT-FORE       TO MOD-KVOT-FORE (2)                         
154100     MOVE WS-KVOI-FOREG-1    TO MOD-KVOI-FORE (2)                         
154200     MOVE WS-KVOI-FOREG-2    TO MOD-KVOI-REFILL-FORE (2)                  
154300     MOVE ZERO               TO MOD-KVOT-CDC-FORE    (2)                  
154400                                                                          
154500*    -- WORKING TABLE WITH ORDERINGGANG STAT FOR LAST 12 PERIOD           
154600                                                                          
154700     PERFORM FDAAB-CALC-VV-I-PER-AA-1                                     
154800                                                                          
154900     MOVE PER                TO INDX                                      
155000     PERFORM UNTIL INDX       > 12                                        
155100       MOVE WS-PER-STA-VV-1 (INDX)                                        
155200                             TO IX-VV                                     
155300       PERFORM UNTIL IX-VV    > WS-PER-END-VV-1 (INDX)                    
155400         IF SEGMENT-FINNS                                                 
155500            ADD AAR-KVOT-PROG   (IX-VV)                                   
155600                             TO WS-KVOT-VV-1    (INDX)                    
155700            ADD AAR-KVOT-REFILL (IX-VV)                                   
155800                             TO WS-KVOT-VV-1    (INDX)                    
155900            ADD AAR-KVOI-PROG   (IX-VV)                                   
156000                             TO WS-KVOI-F1-VV-1 (INDX)                    
156100            ADD AAR-KVOI-REFILL (IX-VV)                                   
156200                             TO WS-KVOI-F2-VV-1 (INDX)                    
156300         ELSE                                                             
156400            MOVE ZERO        TO WS-KVOT-VV-1    (INDX)                    
156500                                WS-KVOI-F1-VV-1 (INDX)                    
156600                                WS-KVOI-F2-VV-1 (INDX)                    
156700         END-IF                                                           
156800         ADD +1              TO IX-VV                                     
156900       END-PERFORM                                                        
157000       ADD +1                TO INDX                                      
157100     END-PERFORM                                                          
157200                                                                          
157300*  --- GET TOTAL INCOMING PROPOSALS FOR CURRENT YEAR                      
157400                                                                          
157500     MOVE DAGENS-AAR         TO W-TIAAAA                                  
157600     PERFORM FDAAC-CALC-VV-I-PER-AA-0                                     
157700                                                                          
157800     PERFORM IMS-GU-WDL811                                                
157900                                                                          
158000*    -- COMPLETE WORKING TABLE WITH OI FOR CURR YEAR PER PERIOD           
158100                                                                          
158200     MOVE +1                 TO INDX                                      
158300     PERFORM UNTIL INDX       > DAGENS-PP                                 
158400       MOVE WS-PER-STA-VV-0 (INDX)                                        
158500                             TO IX-VV                                     
158600       PERFORM UNTIL IX-VV    > WS-PER-END-VV-0 (INDX)                    
158700         IF SEGMENT-FINNS                                                 
158800            ADD AAR-KVOT-PROG   (IX-VV)                                   
158900                             TO WS-KVOT-VV-0    (INDX)                    
159000            ADD AAR-KVOT-REFILL (IX-VV)                                   
159100                             TO WS-KVOT-VV-0    (INDX)                    
159200            ADD AAR-KVOI-PROG   (IX-VV)                                   
159300                             TO WS-KVOI-F1-VV-0 (INDX)                    
159400            ADD AAR-KVOI-REFILL (IX-VV)                                   
159500                             TO WS-KVOI-F2-VV-0 (INDX)                    
159600         ELSE                                                             
159700            MOVE ZERO        TO WS-KVOT-VV-0    (INDX)                    
159800                                WS-KVOI-F1-VV-0 (INDX)                    
159900                                WS-KVOI-F2-VV-0 (INDX)                    
160000         END-IF                                                           
160100         ADD +1              TO IX-VV                                     
160200       END-PERFORM                                                        
160300       ADD +1                TO INDX                                      
160400     END-PERFORM                                                          
160500                                                                          
160600*    --- WORKING TABLE FOR OI, SORTED, LATEST ON TOP                      
160700     MOVE WS-ANT-PP          TO INDX                                      
160800     MOVE 1                  TO IX-VV                                     
160900     PERFORM UNTIL INDX      =  ZERO                                      
161000       MOVE WS-PER-TIAAPP-0 (INDX)                                        
161100                             TO WS-PER-TIAAPP-S (IX-VV)                   
161200       MOVE WS-PER-STA-VV-0 (INDX)                                        
161300                             TO WS-PER-STA-VV-S (IX-VV)                   
161400       MOVE WS-PER-END-VV-0 (INDX)                                        
161500                             TO WS-PER-END-VV-S (IX-VV)                   
161600       MOVE WS-KVOT-VV-0    (INDX)                                        
161700                             TO WS-KVOT-VV-S    (IX-VV)                   
161800       MOVE WS-KVOI-F1-VV-0 (INDX)                                        
161900                             TO WS-KVOI-F1-VV-S (IX-VV)                   
162000       MOVE WS-KVOI-F2-VV-0 (INDX)                                        
162100                             TO WS-KVOI-F2-VV-S (IX-VV)                   
162200       ADD       +1          TO IX-VV                                     
162300       SUBTRACT  +1        FROM INDX                                      
162400     END-PERFORM                                                          
162500                                                                          
162600*    --- POPULATE SORTED TABLE WITH OI INFORMATION FROM                   
162700*    --- PREVIOUS YEAR                                                    
162800                                                                          
162900     IF IX-VV                 < 13                                        
163000        MOVE PER             TO INDX                                      
163100        PERFORM UNTIL INDX    > 12                                        
163200          MOVE WS-PER-TIAAPP-1 (INDX)                                     
163300                             TO WS-PER-TIAAPP-S (IX-VV)                   
163400          MOVE WS-PER-STA-VV-1 (INDX)                                     
163500                             TO WS-PER-STA-VV-S (IX-VV)                   
163600          MOVE WS-PER-END-VV-1 (INDX)                                     
163700                             TO WS-PER-END-VV-S (IX-VV)                   
163800          MOVE WS-KVOT-VV-1    (INDX)                                     
163900                             TO WS-KVOT-VV-S    (IX-VV)                   
164000          MOVE WS-KVOI-F1-VV-1 (INDX)                                     
164100                             TO WS-KVOI-F1-VV-S (IX-VV)                   
164200          MOVE WS-KVOI-F2-VV-1 (INDX)                                     
164300                             TO WS-KVOI-F2-VV-S (IX-VV)                   
164400          ADD       +1       TO IX-VV                                     
164500                                INDX                                      
164600        END-PERFORM                                                       
164700     END-IF                                                               
164800                                                                          
164900*    --- POPULATE OUTPUT FROM SORTED TABLE ON OI                          
165000                                                                          
165100     MOVE +1                 TO INDX                                      
165200     MOVE +1                 TO MOD-IX                                    
165300     PERFORM UNTIL INDX       > +12                                       
165400       IF INDX                > + 6                                       
165500          MOVE WS-PER-TIAAPP-S (INDX)                                     
165600                             TO MOD-TIPP        (MOD-IX)                  
165700          INSPECT MOD-TIPP(MOD-IX)                                        
165800                       REPLACING LEADING ZERO BY SPACE                    
165900          MOVE WS-PER-STA-VV-S (INDX)                                     
166000                             TO WS-FOM                                    
166100          MOVE WS-PER-END-VV-S (INDX)                                     
166200                             TO WS-TOM                                    
166300          MOVE WS-FOM-TOM       TO MOD-TIVV-FOM-TOM(MOD-IX)               
166400          MOVE WS-KVOT-VV-S    (INDX)                                     
166500                             TO MOD-KVOT        (MOD-IX)                  
166600          MOVE WS-KVOI-F1-VV-S (INDX)                                     
166700                             TO MOD-KVOI        (MOD-IX)                  
166800          MOVE WS-KVOI-F2-VV-S (INDX)                                     
166900                             TO MOD-KVOI-REFILL (MOD-IX)                  
167000          MOVE ZERO          TO MOD-KVOT-CDC    (MOD-IX)                  
167100*                                                                         
167200          ADD WS-KVOT-VV-S     (INDX)                                     
167300                             TO WS-KVOT-TOT6                              
167400          ADD WS-KVOI-F1-VV-S  (INDX)                                     
167500                             TO WS-KVOI-TOT6                              
167600          ADD WS-KVOI-F2-VV-S  (INDX)                                     
167700                             TO WS-KVOI-REF-TOT6                          
167800          ADD +1             TO MOD-IX                                    
167900       END-IF                                                             
168000       ADD WS-KVOT-VV-S        (INDX)                                     
168100                             TO WS-KVOT-TOT12                             
168200       ADD WS-KVOI-F1-VV-S  (INDX)                                        
168300                             TO WS-KVOI-TOT12                             
168400       ADD WS-KVOI-F2-VV-S  (INDX)                                        
168500                             TO WS-KVOI-REF-TOT12                         
168600       ADD +1                TO INDX                                      
168700     END-PERFORM                                                          
168800*                                                                         
168900     MOVE WS-KVOT-TOT12      TO MOD-KVOT-RULL-12                          
169000     MOVE ZERO               TO MOD-KVOT-CDC-RULL-12                      
169100     MOVE WS-KVOI-TOT12      TO MOD-KVOI-RULL-12                          
169200     MOVE WS-KVOI-REF-TOT12  TO MOD-KVOI-REF-RULL-12                      
169300     MOVE WS-KVOT-TOT6       TO MOD-KVOT-RULL-6                           
169400     MOVE ZERO               TO MOD-KVOT-CDC-RULL-6                       
169500     MOVE WS-KVOI-TOT6       TO MOD-KVOI-RULL-6                           
169600     MOVE WS-KVOI-REF-TOT6   TO MOD-KVOI-REF-RULL-6                       
169700                                                                          
169800*    --- DISPLAY KVOI FOR THE CURRENT PERIOD                              
169900*                                                                         
170000     MOVE DAGENS-PP          TO INDX                                      
170100     MOVE WS-KVOT-VV-0      (INDX)                                        
170200                             TO MOD-KVOT-INNEV                            
170300     MOVE WS-KVOI-F1-VV-0   (INDX)                                        
170400                             TO MOD-KVOI-INNEV                            
170500     MOVE WS-KVOI-F2-VV-0   (INDX)                                        
170600                             TO MOD-KVOI-REF-INNEV                        
170700     MOVE ZERO               TO MOD-KVOT-CDC-INNEV                        
170800     .                                                                    
170900     EJECT                                                                
171000                                                                          
171100 FDAAA-ORDERINGANG-STAT-PREV-AA  SECTION.                                 
171200                                                                          
171300     MOVE ZERO               TO WS-KVOT-FORE                              
171400                                WS-KVOI-FOREG-1                           
171500                                WS-KVOI-FOREG-2                           
171600     PERFORM S03-CALC-VV-I-AA                                             
171700*                                                                         
171800     PERFORM IMS-GU-WDL811                                                
171900     IF SEGMENT-FINNS                                                     
172000        MOVE +1              TO INDX                                      
172100        PERFORM UNTIL  INDX   > WS-ANT-VV                                 
172200          ADD AAR-KVOT-PROG   (INDX)                                      
172300                             TO WS-KVOT-FORE                              
172400          ADD AAR-KVOT-REFILL (INDX)                                      
172500                             TO WS-KVOT-FORE                              
172600          ADD AAR-KVOI-PROG   (INDX)                                      
172700                             TO WS-KVOI-FOREG-1                           
172800          ADD AAR-KVOI-REFILL (INDX)                                      
172900                             TO WS-KVOI-FOREG-2                           
173000          ADD +1             TO INDX                                      
173100        END-PERFORM                                                       
173200     END-IF                                                               
173300     .                                                                    
173400     EJECT                                                                
173500                                                                          
173600 FDAAB-CALC-VV-I-PER-AA-1   SECTION.                                      
173700                                                                          
173800     MOVE +1                 TO IX                                        
173900     MOVE DAGENS-PER         TO WS-TIAAPER                                
174000     SUBTRACT 1            FROM TIAA                                      
174100     PERFORM S03-CALC-VV-I-AA                                             
174200                                                                          
174300*    --- FILL IN WEEKS FOR DIFFERENT PERIODS                              
174400     MOVE TIAA               TO WS-TIAAPP-AA                              
174500     MOVE PER                TO WS-TIAAPP-PP                              
174600                                                                          
174700     PERFORM UNTIL WS-TIAAPP-PP > 12                                      
174800                                                                          
174900       MOVE 'AARP'           TO DAT-KDDATFORM                             
175000       MOVE WS-TIAAPP        TO DAT-I-TIDATUM                             
175100                                                                          
175200       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
175300                           DAT-O-TIDATUM DAT-KDSVAR                       
175400                                                                          
175500       IF DAT-KDSVAR-OK                                                   
175600          MOVE DAT-TIVV      TO WS-PER-STA-VV-1(WS-TIAAPP-PP)             
175700          MOVE WS-TIAAPP-PP                                               
175800                             TO WS-PER-TIAAPP-1(WS-TIAAPP-PP)             
175900                                                                          
176000       ELSE                                                               
176100          STRING ' FEL FRÅN DATUMRUTIN WDATKONV AA-1'                     
176200          DELIMITED BY SIZE INTO FELTEXT                                  
176300          CALL FELLOG                                                     
176400       END-IF                                                             
176500                                                                          
176600       ADD 1                 TO WS-TIAAPP                                 
176700     END-PERFORM                                                          
176800                                                                          
176900     MOVE +1                 TO IX                                        
177000                                                                          
177100     PERFORM UNTIL IX > +11                                               
177200                                                                          
177300       COMPUTE WS-PER-END-VV-1 (IX) =                                     
177400               WS-PER-STA-VV-1 (IX + 1) - 1                               
177500                                                                          
177600       ADD +1                TO IX                                        
177700     END-PERFORM                                                          
177800                                                                          
177900     MOVE WS-ANT-VV          TO WS-PER-END-VV-1 (12)                      
178000     .                                                                    
178100     EJECT                                                                
178200                                                                          
178300 FDAAC-CALC-VV-I-PER-AA-0   SECTION.                                      
178400                                                                          
178500     MOVE +1                 TO IX                                        
178600     MOVE DAGENS-PER         TO WS-TIAAPER                                
178700     PERFORM S03-CALC-VV-I-AA                                             
178800*                                                                         
178900     MOVE DAGENS-DATUM (1:2) TO WS-TIAAPP-AA                              
179000     MOVE 1                  TO WS-TIAAPP-PP                              
179100                                                                          
179200     PERFORM UNTIL WS-TIAAPP-PP > 12                                      
179300                                                                          
179400       MOVE 'AARP'           TO DAT-KDDATFORM                             
179500       MOVE WS-TIAAPP        TO DAT-I-TIDATUM                             
179600                                                                          
179700       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
179800                           DAT-O-TIDATUM DAT-KDSVAR                       
179900                                                                          
180000       IF DAT-KDSVAR-OK                                                   
180100*--- START WEEK IS WEEK 1 OF THE CURRENT/NEW YEAR                         
180200         IF DAT-TIVV = +52 OR +53                                         
180300            MOVE 1           TO WS-PER-STA-VV-0(WS-TIAAPP-PP)             
180400                                WS-PER-TIAAPP-0(WS-TIAAPP-PP)             
180500         ELSE                                                             
180600            MOVE DAT-TIVV    TO WS-PER-STA-VV-0(WS-TIAAPP-PP)             
180700            MOVE WS-TIAAPP-PP                                             
180800                             TO WS-PER-TIAAPP-0(WS-TIAAPP-PP)             
180900         END-IF                                                           
181000                                                                          
181100       ELSE                                                               
181200           STRING ' FEL FRÅN DATUMRUTIN WDATKONV AA-0'                    
181300           DELIMITED BY SIZE INTO FELTEXT                                 
181400           CALL FELLOG                                                    
181500       END-IF                                                             
181600                                                                          
181700       ADD 1                 TO WS-TIAAPP                                 
181800     END-PERFORM                                                          
181900     COMPUTE WS-ANT-PP        = DAGENS-PP - 1                             
182000                                                                          
182100     MOVE +1                 TO IX                                        
182200                                                                          
182300     PERFORM UNTIL IX > 11                                                
182400                                                                          
182500       COMPUTE WS-PER-END-VV-0 (IX) =                                     
182600               WS-PER-STA-VV-0 (IX + 1) - 1                               
182700                                                                          
182800       ADD +1                TO IX                                        
182900     END-PERFORM                                                          
183000     MOVE WS-ANT-VV          TO WS-PER-END-VV-0(12)                       
183100     .                                                                    
183200     EJECT                                                                
183300                                                                          
183400 FDAB-BEHANDLA-CDC-INFO SECTION.                                          
183500                                                                          
183600     PERFORM IMS-GU-WDK611                                                
183700     IF SEGMENT-FINNS                                                     
183800        PERFORM IMS-GNP-WDK629                                            
183900        IF SEGMENT-FINNS                                                  
184000           MOVE CREF-IDDC-REF TO WS-IDDC-LAND                             
184100           PERFORM S9-SEARCH-IDLAND                                       
184200           PERFORM IMS-GU-WDK712                                          
184300           IF SEGMENT-FINNS                                               
184400             IF LART-KVQPACK-3 > 0                                        
184500               MOVE LART-KVQPACK-3 TO MOD-KVQPACK-3                       
184600             ELSE                                                         
184700               MOVE CLAG-KVQPACK-3 TO MOD-KVQPACK-3                       
184800             END-IF                                                       
184900           ELSE                                                           
185000             MOVE CLAG-KVQPACK-3 TO MOD-KVQPACK-3                         
185100           END-IF                                                         
185200                                                                          
185300           MOVE CLAG-KDERS        TO MOD-KDERS                            
185400                                     WS-KDERS                             
185500           MOVE WS-STD-TEXT       TO MOD-PRICE-TEXT                       
185600           MOVE CLAG-PRARTSTD     TO MOD-PRARTSTD                         
185700           MOVE CLAG-KVQPACK-1    TO MOD-KVQPACK-1                        
185800*          --- HÄMTA PROGNOS OCH PROGNOSDATUM                             
185900           MOVE CLAG-KVPB-SEP     TO MOD-KVPB-REF                         
186000                                                                          
186100           PERFORM S04-GET-FC-TOT                                         
186200           MOVE WS-KVPB-TOT-CDC   TO MOD-KVPBREOI                         
186300                                                                          
186400           IF CLAG-TIPBDAT         > ZERO                                 
186500              MOVE CLAG-TIPBDAT   TO WS-TIPBDAT                           
186600              MOVE 'AAVVD'        TO DAT-KDDATFORM                        
186700              MOVE WS-TIPBDAT     TO DAT-I-TIDATUM                        
186800              CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM             
186900                                  DAT-O-TIDATUM DAT-KDSVAR                
187000              IF DAT-KDSVAR-OK                                            
187100                 MOVE DAT-TIAAMMDD                                        
187200                                  TO MOD-TIREFMPB                         
187300              ELSE                                                        
187400                 MOVE MFS-RENSA-FAELT                                     
187500                                  TO MOD-TIREFMPB                         
187600              END-IF                                                      
187700           ELSE                                                           
187800              MOVE MFS-RENSA-FAELT                                        
187900                                  TO MOD-TIREFMPB                         
188000           END-IF                                                         
188100                                                                          
188200           IF CLAG-DAPBPLAN        > ZERO                                 
188300              MOVE CLAG-DAPBPLAN (3:6)                                    
188400                                  TO MOD-TIPBREOI                         
188500           ELSE                                                           
188600              MOVE MFS-RENSA-FAELT                                        
188700                                  TO MOD-TIPBREOI                         
188800           END-IF                                                         
188900                                                                          
189000*          --- HÄMTA SALDON                                               
189100           MOVE CLAG-KVQ          TO MOD-KVREFBER                         
189200           COMPUTE WS-ORDERED-Q    = CLAG-KVBEART   +                     
189300                                     CLAG-KVAKS-PAV +                     
189400                                     CLAG-KVAKS-T                         
189500           MOVE WS-ORDERED-Q      TO MOD-KVBEART                          
189600*                                                                         
189700           PERFORM FCA-SUMMERA-OKS                                        
189800           MOVE CLAG-KVAKS-CDC    TO MOD-KVAKS-S                          
189900                                                                          
190000           COMPUTE WS-BALANCE-CLAG = CLAG-KVLS     -                      
190100                                     CLAG-KVROS    -                      
190200                                     WS-KVOKS-TOT  -                      
190300                                     CLAG-KVSPARR-KVAL                    
190400           MOVE WS-BALANCE-CLAG   TO MOD-KVLS                             
190500*                                                                         
190600           MOVE CLAG-TIPBLOCK     TO MOD-DAREFESC                         
190700           MOVE ZEROS             TO MOD-DAREFESC-REOI                    
190800        ELSE                                                              
190900           STRING 'INVALID REFILL INFO - WDK629 STATUSCODE '              
191000                  STATUS-WS                                               
191100           DELIMITED BY SIZE INTO FELTEXT                                 
191200           CALL FELLOG                                                    
191300        END-IF                                                            
191400     ELSE                                                                 
191500         MOVE MFS-RENSA-FAELT     TO MOD-KVLS                             
191600                                     MOD-KVAKS-S                          
191700                                     MOD-KVBEART                          
191800                                     MOD-KVPB-REF                         
191900                                     MOD-KVREFBER                         
192000                                     MOD-TIREFMPB                         
192100                                     MOD-TIPBREOI                         
192200                                     MOD-PRICE-TEXT                       
192300                                     MOD-PRARTSTD                         
192400                                     MOD-KDERS                            
192500                                     MOD-KVQPACK-1                        
192600                                     MOD-KVQPACK-3                        
192700     END-IF                                                               
192800     .                                                                    
192900     EJECT                                                                
193000 FDB-LAES-DC-INFO SECTION.                                                
193100                                                                          
193200     MOVE ZEROS                TO WS-KVPB-TOT                             
193300                                  WS-KVDISP                               
193400                                  WS-KVAKS                                
193500                                  WS-BALANCE-SLAG                         
193600                                  WS-KVART-FORAVIS                        
193700     MOVE W-IDDC-REF           TO W-IDDC                                  
193800     PERFORM IMS-GU-WDK711                                                
193900     IF SEGMENT-FINNS                                                     
194000        COMPUTE WS-KVDISP       = SLAG-KVLS       -                       
194100                                  SLAG-KVRESS     -                       
194200                                  SLAG-KVOKS-BULK -                       
194300                                  SLAG-KVOKS-DAG                          
194400        MOVE WS-KVDISP         TO MOD-KVDISP                              
194500*                                                                         
194600        MOVE SLAG-KVAKS-SDC    TO MOD-KVAKS                               
194700                                                                          
194800        COMPUTE WS-KVPB-TOT     = SLAG-KVPB-REF +                         
194900                                  SLAG-KVPBREOI                           
195000        MOVE WS-KVPB-TOT       TO MOD-KVPB-TOT                            
195100*                                                                         
195200        PERFORM IMS-GU-WDK722                                             
195300        IF SEGMENT-FINNS                                                  
195400          MOVE XLAG-IDANSK     TO MOD-IDANSK                              
195500        ELSE                                                              
195600          MOVE MFS-RENSA-FAELT TO MOD-IDANSK                              
195700        END-IF                                                            
195800*                                                                         
195900        PERFORM FDBA-GET-PREADV-INFO                                      
196000     ELSE                                                                 
196100        MOVE ARTIKEL-SAKNAS-SDC TO MED-IDMFSFEL                           
196200        CALL WMEDKONV        USING MED-WMEDAREA                           
196300        MOVE MED-MFSFEL         TO MOD-TEMFSFEL                           
196400        MOVE MFS-RENSA-FAELT    TO MOD-IDANSK                             
196500                                   MOD-KVPB-TOT                           
196600                                   MOD-KVAKS                              
196700                                   MOD-KVDISP                             
196800                                   MOD-KVART-FORAVIS                      
196900     END-IF                                                               
197000     .                                                                    
197100     EJECT                                                                
197200 FDBA-GET-PREADV-INFO SECTION.                                            
197300                                                                          
197400     MOVE ZERO                 TO WS-KVAVIS                               
197500     IF SLAG-IDDC-REF       NOT > SPACES                                  
197600        MOVE W-IDARTNR         TO W-IDARTNR-HSEQ                          
197700        PERFORM IMS-GN-W6D111-W6D1SEQ                                     
197800        PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                      
197900          IF  W6D1-ART-IDDC     = W-IDDC-REF                              
198000          AND W6D1-ART-IDLOPNRM                                           
198100                                = ZERO                                    
198200          AND W6D1-ART-FLFEL    = NEJ                                     
198300              ADD W6D1-ART-KVAVIS                                         
198400                               TO WS-KVAVIS                               
198500          END-IF                                                          
198600          PERFORM IMS-GN-W6D111-W6D1SEQ                                   
198700        END-PERFORM                                                       
198800     ELSE                                                                 
198900        COMPUTE WS-KVART-FORAVIS                                          
199000                                = SLAG-KVAKS-PAV +                        
199100                                  SLAG-KVBEART                            
199200                                                                          
199300        MOVE WS-KVART-FORAVIS  TO WS-KVAVIS                               
199400     END-IF                                                               
199500     MOVE WS-KVAVIS            TO MOD-KVART-FORAVIS                       
199600     .                                                                    
199700     EJECT                                                                
199800 FDC-CHECK-CAMPAIGN  SECTION.                                             
199900                                                                          
200000     MOVE NEJ                     TO SW-CAMPAIGN                          
200100                                                                          
200200     PERFORM DB2-DCL-OPN-TP1ARTK-CRS                                      
200300                                                                          
200400     IF SQLCODE-WS  = ZERO                                                
200500        PERFORM DB2-FETCH-TP1ARTK-CRS                                     
200600     END-IF                                                               
200700                                                                          
200800     IF SQLCODE-WS  = ZERO                                                
200900                                                                          
201000        PERFORM UNTIL SQLCODE      > ZERO                                 
201100          IF TP1KAMP-TISTODAT-KAMP = ZERO                                 
201200             COMPUTE WS-TISTODAT-KAMP =                                   
201300                     TP1KAMP-TISTADAT-KAMP + 50000                        
201400          ELSE                                                            
201500             MOVE TP1KAMP-TISTODAT-KAMP                                   
201600                                  TO WS-TISTODAT-KAMP                     
201700          END-IF                                                          
201800                                                                          
201900          MOVE DAGENS-DATUM       TO WS-DATUM                             
202000                                                                          
202100          IF WS-DATUM-MAN          > 06                                   
202200             SUBTRACT 600       FROM WS-DATUM                             
202300          ELSE                                                            
202400             ADD 600              TO WS-DATUM                             
202500             SUBTRACT 10000     FROM WS-DATUM                             
202600          END-IF                                                          
202700                                                                          
202800          MOVE WS-TISTODAT-KAMP   TO TMP1-YYMMDD                          
202900          MOVE WS-DATUM           TO TMP2-YYMMDD                          
203000          PERFORM WY2000P1                                                
203100          IF TMP1-YYMMDD          >= TMP2-YYMMDD                          
203200             MOVE JA              TO SW-CAMPAIGN                          
203300             MOVE +100            TO SQLCODE                              
203400          END-IF                                                          
203500                                                                          
203600          IF SQLCODE = ZERO                                               
203700             PERFORM DB2-FETCH-TP1ARTK-CRS                                
203800          END-IF                                                          
203900       END-PERFORM                                                        
204000     END-IF                                                               
204100                                                                          
204200     PERFORM DB2-CLOSE-TP1ARTK-CRS                                        
204300     .                                                                    
204400     EJECT                                                                
204500 G-KOLLA-INPUT SECTION.                                                   
204600                                                                          
204700     MOVE JA                     TO INDATA-SW                             
204800     IF MID-INPUT                =  ALL '+'                               
204900       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
205000       CALL WMEDKONV USING MED-WMEDAREA                                   
205100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
205200       PERFORM MFS-ROER-EJ-FAELT-IN                                       
205300       PERFORM MFS-ROER-EJ-FAELT-GRUND                                    
205400       PERFORM MFS-ROER-EJ-FAELT-SLAG                                     
205500       PERFORM MFS-ROER-EJ-FAELT-CDC-INFO                                 
205600       MOVE NEJ                  TO INDATA-SW                             
205700     ELSE                                                                 
205800                                                                          
205900       PERFORM GA-KOLLA-GRUND                                             
206000       IF INDATA-OK                                                       
206100         PERFORM GC-KOLLA-PB-INDATA                                       
206200         IF INDATA-FEL                                                    
206300           CALL WMEDKONV USING MED-WMEDAREA                               
206400           MOVE MED-MFSFEL           TO MOD-TEMFSFEL                      
206500           PERFORM MFS-ROER-EJ-FAELT-GRUND                                
206600           PERFORM MFS-ROER-EJ-FAELT-SLAG                                 
206700           PERFORM MFS-ROER-EJ-FAELT-CDC-INFO                             
206800           PERFORM MFS-ROER-EJ-FAELT-IN                                   
206900         END-IF                                                           
207000       ELSE                                                               
207100         PERFORM MFS-ROER-EJ-FAELT-GRUND                                  
207200         PERFORM MFS-ROER-EJ-FAELT-SLAG                                   
207300         PERFORM MFS-ROER-EJ-FAELT-CDC-INFO                               
207400         PERFORM MFS-RENSA-FAELT-IN                                       
207500       END-IF                                                             
207600     END-IF                                                               
207700     .                                                                    
207800     EJECT                                                                
207900 GA-KOLLA-GRUND SECTION.                                                  
208000                                                                          
208100     PERFORM IMS-GU-WDK601                                                
208200     IF SEGMENT-FINNS                                                     
208300       PERFORM IMS-GNP-WDK611                                             
208400       IF SEGMENT-FINNS                                                   
208500         PERFORM S02-GET-BESPRIS                                          
208600         IF CLAG-KDERS            <  10                                   
208700           IF CLAG-FLREFILL       =  NEJ                                  
208800             MOVE NEJ             TO INDATA-SW                            
208900             MOVE EJ-GODK-REFILL TO MED-IDMFSFEL                          
209000             CALL WMEDKONV USING MED-WMEDAREA                             
209100             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
209200           END-IF                                                         
209300         ELSE                                                             
209400           IF CLAG-KDERS           > 9                                    
209500             IF CLAG-KDERS         < 29                                   
209600               MOVE ARTIKEL-ERSATT TO MED-IDMFSFEL                        
209700               CALL WMEDKONV USING MED-WMEDAREA                           
209800               MOVE MED-MFSFEL     TO MOD-TEMFSFEL                        
209900             ELSE                                                         
210000               MOVE ARTIKEL-UTGANGEN TO MED-IDMFSFEL                      
210100               CALL WMEDKONV USING MED-WMEDAREA                           
210200               MOVE MED-MFSFEL       TO MOD-TEMFSFEL                      
210300             END-IF                                                       
210400           END-IF                                                         
210500           MOVE NEJ TO INDATA-SW                                          
210600         END-IF                                                           
210700       ELSE                                                               
210800           MOVE NEJ TO INDATA-SW                                          
210900       END-IF                                                             
211000     ELSE                                                                 
211100       MOVE NEJ TO INDATA-SW                                              
211200     END-IF                                                               
211300     .                                                                    
211400     EJECT                                                                
211500 GC-KOLLA-PB-INDATA SECTION.                                              
211600                                                                          
211700     IF MID-TIREFMPB                  NOT = ALL '+'                       
211800       IF MID-TIREFMPB NUMERIC                                            
211900         MOVE 'AAMMDD'                TO DAT-KDDATFORM                    
212000         MOVE MID-TIREFMPB            TO DAT-I-TIDATUM                    
212100         CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                  
212200                             DAT-O-TIDATUM DAT-KDSVAR                     
212300         IF DAT-KDSVAR-OK OR DAT-I-TIDATUM = 0                            
212400           MOVE MID-TIREFMPB          TO WS-TIREFMPB                      
212500           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-TIREFMPB-IN-ATTR             
212600           MOVE JA    TO MANUELLT-DATUM-SW                                
212700           IF CDC-SE AND DAT-I-TIDATUM > ZERO                             
212800              MOVE DAT-TIAAVVD        TO WS-TIPBDAT                       
212900           END-IF                                                         
213000         ELSE                                                             
213100           MOVE MFS-ALFA-FAELT-FEL    TO MOD-TIREFMPB-IN-ATTR             
213200           MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                     
213300           MOVE NEJ                   TO INDATA-SW                        
213400         END-IF                                                           
213500       ELSE                                                               
213600         MOVE MFS-ALFA-FAELT-FEL      TO MOD-TIREFMPB-IN-ATTR             
213700         MOVE ERR-CORR-HILITE-FLDS    TO MED-IDMFSFEL                     
213800         MOVE NEJ                     TO INDATA-SW                        
213900       END-IF                                                             
214000     END-IF                                                               
214100                                                                          
214200     IF MID-TIPBREOI                  NOT = ALL '+'                       
214300       IF MID-TIPBREOI NUMERIC                                            
214400         MOVE 'AAMMDD'                TO DAT-KDDATFORM                    
214500         MOVE MID-TIPBREOI            TO DAT-I-TIDATUM                    
214600         CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                  
214700                             DAT-O-TIDATUM DAT-KDSVAR                     
214800         IF DAT-KDSVAR-OK OR DAT-I-TIDATUM = 0                            
214900           MOVE MID-TIPBREOI          TO WS-TIPBREOI                      
215000           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-TIPBREOI-IN-ATTR             
215100           MOVE JA    TO MANUELLT-DATUM-SW                                
215200           IF CDC-SE AND DAT-I-TIDATUM > ZERO                             
215300              MOVE WS-TIPBREOI        TO WS-DAPBPLAN-AAMMDD               
215400              IF WS-DAPBPLAN          <= DAGENS-AAAAMMDD                  
215500              OR CLAG-DAPBPLAN    NOT  > ZERO                             
215600                 MOVE MFS-ALFA-FAELT-FEL                                  
215700                                      TO MOD-TIPBREOI-IN-ATTR             
215800                 MOVE ERR-CORR-HILITE-FLDS                                
215900                                      TO MED-IDMFSFEL                     
216000                 MOVE NEJ             TO INDATA-SW                        
216100              END-IF                                                      
216200           END-IF                                                         
216300         ELSE                                                             
216400           MOVE MFS-ALFA-FAELT-FEL    TO MOD-TIPBREOI-IN-ATTR             
216500           MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                     
216600           MOVE NEJ                   TO INDATA-SW                        
216700         END-IF                                                           
216800       ELSE                                                               
216900         MOVE MFS-ALFA-FAELT-FEL      TO MOD-TIPBREOI-IN-ATTR             
217000         MOVE ERR-CORR-HILITE-FLDS    TO MED-IDMFSFEL                     
217100         MOVE NEJ                     TO INDATA-SW                        
217200       END-IF                                                             
217300     END-IF                                                               
217400     .                                                                    
217500     EJECT                                                                
217600 H-UPPDATERA-VISA-INFO SECTION.                                           
217700                                                                          
217800     IF CDC-SE                                                            
217900        PERFORM HD-UPDATE-WDK6                                            
218000     ELSE                                                                 
218100        PERFORM IMS-GHU-WDK611                                            
218200                                                                          
218300        PERFORM HB-UPPDATERA-WDK7                                         
218400     END-IF                                                               
218500                                                                          
218600     MOVE JA              TO SW-MESSAGE                                   
218700     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
218800     CALL WMEDKONV USING MED-WMEDAREA                                     
218900     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
219000     PERFORM MFS-FORM-ATTR                                                
219100     PERFORM MFS-RENSA-FAELT-IN                                           
219200     PERFORM MFS-ROER-EJ-FAELT-GRUND                                      
219300     .                                                                    
219400     EJECT                                                                
219500 HB-UPPDATERA-WDK7    SECTION.                                            
219600                                                                          
219700     PERFORM IMS-GU-K711                                                  
219800     IF SEGMENT-SAKNAS                                                    
219900       MOVE ALL '+'           TO WDK7-W005WDK7                            
220000       MOVE 'WDK711'          TO WDK7-IDSEGM                              
220100       MOVE W-IDARTNR         TO WDK7-IDARTNR-KFB                         
220200       MOVE W-IDDC            TO WDK7-IDDC-KFB                            
220300                                 WDK7-IDDC                                
220400       PERFORM S01-KOLLA-OM-REFILL-OK                                     
220500       IF REFILL-ARTIKEL-OK                                               
220600         MOVE JA         TO WDK7-FLREFILL                                 
220700       ELSE                                                               
220800         MOVE NEJ        TO WDK7-FLREFILL                                 
220900       END-IF                                                             
221000       CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB                         
221100                                         WDK6-PCB WDK7I-PCB               
221200       MOVE NEJ               TO SW-WDK711                                
221300* LÄS OM NYUPPLAGD K711 FÖR POSITIONERING AV SENARE REPL                  
221400       PERFORM IMS-GHU-WDK711                                             
221500       IF SEGMENT-FINNS                                                   
221600          MOVE JA             TO SW-WDK711                                
221700       END-IF                                                             
221800     ELSE                                                                 
221900       MOVE JA                TO SW-WDK711                                
222000       PERFORM IMS-GHU-WDK711                                             
222100     END-IF                                                               
222200                                                                          
222300     IF MID-TIREFMPB NOT = ALL '+'                                        
222400       PERFORM HBA-UPPDAT-VISA-MPB                                        
222500                                                                          
222600       MOVE MFS-ROER-EJ-FAELT TO MOD-TIREFMPB                             
222700     END-IF                                                               
222800                                                                          
222900     IF MID-TIPBREOI NOT = ALL '+'                                        
223000       PERFORM HBB-UPPDAT-VISA-REOI                                       
223100                                                                          
223200       MOVE MFS-ROER-EJ-FAELT TO MOD-TIPBREOI                             
223300     END-IF                                                               
223400                                                                          
223500     IF WDK711-FINNS                                                      
223600       PERFORM IMS-REPL-WDK711                                            
223700     ELSE                                                                 
223800       CALL FELLOG                                                        
223900     END-IF                                                               
224000     .                                                                    
224100     EJECT                                                                
224200                                                                          
224300 HBA-UPPDAT-VISA-MPB SECTION.                                             
224400                                                                          
224500     MOVE WS-TIREFMPB         TO SLAG-TIREFMPB                            
224600     IF WS-TIREFMPB > 0                                                   
224700       MOVE WS-TIREFMPB       TO MOD-TIREFMPB                             
224800     ELSE                                                                 
224900       MOVE MFS-RENSA-FAELT   TO MOD-TIREFMPB                             
225000     END-IF                                                               
225100     .                                                                    
225200     EJECT                                                                
225300 HBB-UPPDAT-VISA-REOI SECTION.                                            
225400                                                                          
225500     MOVE WS-TIPBREOI         TO SLAG-TIPBREOI                            
225600     IF WS-TIPBREOI > 0                                                   
225700       MOVE WS-TIPBREOI       TO MOD-TIPBREOI                             
225800     ELSE                                                                 
225900       MOVE MFS-RENSA-FAELT   TO MOD-TIPBREOI                             
226000     END-IF                                                               
226100     .                                                                    
226200     EJECT                                                                
226300 HD-UPDATE-WDK6    SECTION.                                               
226400                                                                          
226500     MOVE MSGI-IDDC-KEY      TO W-IDDC                                    
226600     PERFORM IMS-GHU-WDK611                                               
226700     IF SEGMENT-FINNS                                                     
226800                                                                          
226900        IF MID-TIREFMPB NOT = ALL '+'                                     
227000           MOVE WS-TIPBDAT          TO CLAG-TIPBDAT                       
227100           IF WS-TIREFMPB > 0                                             
227200             MOVE WS-TIREFMPB       TO MOD-TIREFMPB                       
227300           ELSE                                                           
227400             MOVE MFS-RENSA-FAELT   TO MOD-TIREFMPB                       
227500           END-IF                                                         
227600           MOVE MFS-ROER-EJ-FAELT   TO MOD-TIREFMPB                       
227700        END-IF                                                            
227800                                                                          
227900        IF MID-TIPBREOI NOT = ALL '+'                                     
228000           MOVE WS-DAPBPLAN         TO CLAG-DAPBPLAN                      
228100           IF WS-TIPBREOI > 0                                             
228200             MOVE WS-TIPBREOI       TO MOD-TIPBREOI                       
228300           ELSE                                                           
228400             MOVE MFS-RENSA-FAELT   TO MOD-TIPBREOI                       
228500           END-IF                                                         
228600           MOVE MFS-ROER-EJ-FAELT   TO MOD-TIPBREOI                       
228700        END-IF                                                            
228800                                                                          
228900        PERFORM IMS-REPL-WDK611                                           
229000                                                                          
229100     END-IF                                                               
229200      .                                                                   
229300      EJECT                                                               
229400                                                                          
229500 S01-KOLLA-OM-REFILL-OK SECTION.                                          
229600                                                                          
229700     IF CLAG-FLREFILL = NEJ                                               
229800       MOVE EJ-GODK-REFILL TO MED-IDMFSFEL                                
229900       CALL WMEDKONV USING MED-WMEDAREA                                   
230000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
230100       MOVE NEJ TO REFILL-ARTIKEL-OK-SW                                   
230200     END-IF                                                               
230300                                                                          
230400     IF CLAG-REDIRLEV = 1.0                                               
230500       MOVE DIREKTLEV TO MED-IDMFSFEL                                     
230600       CALL WMEDKONV USING MED-WMEDAREA                                   
230700       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
230800       MOVE NEJ TO REFILL-ARTIKEL-OK-SW                                   
230900     END-IF                                                               
231000                                                                          
231100     MOVE W-IDARTNR          TO BYT03-IDARTNR                             
231200     IF BYT03-OBJEKT                                                      
231300       MOVE NEJ TO REFILL-ARTIKEL-OK-SW                                   
231400     END-IF                                                               
231500     .                                                                    
231600     EJECT                                                                
231700                                                                          
231800 S02-GET-BESPRIS  SECTION.                                                
231900     IF DCS-CHINA OR DCS-NDC-NA                                           
232000       PERFORM S02A-GET-BESPRIS                                           
232100     ELSE                                                                 
232200       PERFORM S02B-GET-BESPRIS                                           
232300     END-IF                                                               
232400     .                                                                    
232500     EJECT                                                                
232600                                                                          
232700 S02A-GET-BESPRIS SECTION.                                                
232800*    -- WDK712                                                            
232900     MOVE DCS-IDLANDX2    TO W-IDLAND                                     
233000     PERFORM IMS-GU-WDK712                                                
233100***FLYTTAR PRMATRL TILL WS-PRARTBES                                       
233200     IF SEGMENT-FINNS                                                     
233300       MOVE LART-PRMATRL     TO WS-PRARTBES                               
233400     ELSE                                                                 
233500       MOVE ZERO             TO WS-PRARTBES                               
233600     END-IF                                                               
233700     .                                                                    
233800     EJECT                                                                
233900                                                                          
234000 S02B-GET-BESPRIS SECTION.                                                
234100                                                                          
234200     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-AAAAMMDD                   
234300     COMPUTE W-DAPRLIST = 99999999 - DAGENS-AAAAMMDD                      
234400     PERFORM IMS-GNP-WDK621                                               
234500     IF SEGMENT-SAKNAS                                                    
234600       MOVE CLAG-PRARTSTD       TO WS-PRARTBES                            
234700     ELSE                                                                 
234800       MOVE NEJ                 TO FL-PRARTBES                            
234900       PERFORM UNTIL  SEGMENT-SAKNAS                                      
235000         IF PRL-SUINLEV-PR > ZERO                                         
235100           MOVE PRL-PRARTBES-PR  TO WS-PRARTBES                           
235200           SET SEGMENT-SAKNAS TO TRUE                                     
235300         ELSE                                                             
235400           IF FL-PRARTBES = NEJ                                           
235500             MOVE PRL-PRARTBES-PR TO WS-PRARTBES                          
235600             MOVE JA              TO FL-PRARTBES                          
235700           END-IF                                                         
235800           PERFORM IMS-GNP-WDK621                                         
235900         END-IF                                                           
236000       END-PERFORM                                                        
236100     END-IF                                                               
236200     .                                                                    
236300     EJECT                                                                
236400 S03-CALC-VV-I-AA SECTION.                                                
236500                                                                          
236600*    --- CALCULATE THE YEAR HAD HOW MANY WEEKS                            
236700     MOVE TIAA               TO WS-AAR                                    
236800     MOVE 53                 TO WS-VV                                     
236900     MOVE 'AAVV  '           TO DAT-KDDATFORM                             
237000     MOVE TIAAVV             TO DAT-I-TIDATUM                             
237100     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
237200                         DAT-O-TIDATUM DAT-KDSVAR                         
237300     IF DAT-KDSVAR-OK                                                     
237400       MOVE 53               TO WS-ANT-VV                                 
237500     ELSE                                                                 
237600       MOVE 52               TO WS-ANT-VV                                 
237700     END-IF                                                               
237800     .                                                                    
237900     EJECT                                                                
238000                                                                          
238100 S04-GET-FC-TOT       SECTION.                                            
238200                                                                          
238300     INITIALIZE UTIL-W271UTIL                                             
238400     MOVE W-IDARTNR                 TO UTIL-IDARTNR                       
238500     MOVE 003                       TO UTIL-KDCALL                        
238600                                                                          
238700     CALL W271UTIL USING UTIL-W271UTIL                                    
238800                         UTIL-WDK6-PCB                                    
238900                         UTIL-WDK7-PCB                                    
239000                         UTIL-WDB6-PCB                                    
239100                                                                          
239200     IF UTIL-KDSVAR-OK                                                    
239300        MOVE UTIL-KVPB-TOT          TO WS-KVPB-TOT-CDC                    
239400     ELSE                                                                 
239500        STRING 'FEL FRÅN W271UTIL '   UTIL-KDSVAR                         
239600        DELIMITED BY SIZE INTO FELTEXT                                    
239700        CALL FELLOG                                                       
239800     END-IF                                                               
239900     .                                                                    
240000     EJECT                                                                
240100                                                                          
240200 S9-SEARCH-IDLAND SECTION.                                                
240300                                                                          
240400     SEARCH ALL DC-LAND                                                   
240500       AT END                                                             
240600         MOVE SPACE          TO W-IDLAND                                  
240700       WHEN DCLAND-IDDC (DCLAND-IX) = WS-IDDC-LAND                        
240800         MOVE DCLAND-IDLANDX2(DCLAND-IX) TO W-IDLAND                      
240900     END-SEARCH                                                           
241000     .                                                                    
241100     EJECT                                                                
241200                                                                          
241300 MFS-RENSA-FAELT-GRUND SECTION.                                           
241400                                                                          
241500*    --- ALLA GRUND-UTDATA-FÄLT                                           
241600     MOVE MFS-RENSA-FAELT   TO MOD-BEART-SVE                              
241700                               MOD-IDANSK                                 
241800                               MOD-PRICE-TEXT                             
241900                               MOD-PRARTSTD                               
242000                               MOD-KVQPACK-1                              
242100                               MOD-KDERS                                  
242200                               MOD-FLERS                                  
242300                               MOD-TIFINLV                                
242400     .                                                                    
242500     SKIP2                                                                
242600 MFS-RENSA-FAELT-SLAG  SECTION.                                           
242700                                                                          
242800*    --- ALLA S-LAGER-UTDATA-FÄLT FRÅN WDL7                               
242900     MOVE +1 TO INDX                                                      
243000     PERFORM UNTIL INDX > 6                                               
243100       MOVE MFS-RENSA-FAELT TO MOD-TIPP(INDX)                             
243200                               MOD-TIVV-FOM-TOM(INDX)                     
243300                               MOD-KVOT(INDX)                             
243400                               MOD-KVOT-CDC(INDX)                         
243500       ADD +1 TO INDX                                                     
243600     END-PERFORM                                                          
243700     MOVE MFS-RENSA-FAELT   TO MOD-KVOT-INNEV                             
243800                               MOD-KVOT-CDC-INNEV                         
243900                               MOD-KVOI-INNEV                             
244000                               MOD-KVOI-REF-INNEV                         
244100     MOVE MFS-RENSA-FAELT   TO MOD-KVOT-RULL-12                           
244200                               MOD-KVOT-CDC-RULL-12                       
244300                               MOD-KVOI-RULL-12                           
244400                               MOD-KVOI-REF-RULL-12                       
244500                               MOD-KVOT-RULL-6                            
244600                               MOD-KVOT-CDC-RULL-6                        
244700                               MOD-KVOI-RULL-6                            
244800                               MOD-KVOI-REF-RULL-6                        
244900     MOVE +1 TO INDX                                                      
245000     PERFORM UNTIL INDX > 2                                               
245100       MOVE MFS-RENSA-FAELT TO MOD-TIAA-FORE(INDX)                        
245200                               MOD-KVOT-FORE(INDX)                        
245300                               MOD-KVOT-CDC-FORE(INDX)                    
245400                               MOD-KVOI-FORE(INDX)                        
245500                               MOD-KVOI-REFILL-FORE(INDX)                 
245600       ADD +1 TO INDX                                                     
245700     END-PERFORM                                                          
245800     .                                                                    
245900     EJECT                                                                
246000 MFS-RENSA-FAELT-CLAG  SECTION.                                           
246100                                                                          
246200*    --- ALLA C-LAGER-UTDATA-FÄLT                                         
246300     MOVE MFS-RENSA-FAELT   TO MOD-KVDISP                                 
246400                               MOD-KVAKS                                  
246500                               MOD-KVART-FORAVIS                          
246600                               MOD-KVPB-TOT                               
246700                               MOD-KVQPACK-3                              
246800     .                                                                    
246900     EJECT                                                                
247000 MFS-RENSA-FAELT-IN SECTION.                                              
247100                                                                          
247200*    --- ALLA INDATA-FÄLT                                                 
247300     MOVE MFS-RENSA-FAELT   TO MOD-TIPBREOI-IN                            
247400                               MOD-TIREFMPB-IN                            
247500                                                                          
247600     .                                                                    
247700     EJECT                                                                
247800 MFS-ROER-EJ-FAELT-GRUND SECTION.                                         
247900                                                                          
248000*    --- ALLA GRUND-UTDATA-FÄLT                                           
248100     MOVE MFS-ROER-EJ-FAELT TO MOD-BEART-SVE                              
248200                               MOD-IDANSK                                 
248300                               MOD-PRICE-TEXT                             
248400                               MOD-PRARTSTD                               
248500                               MOD-KVQPACK-1                              
248600                               MOD-KDERS                                  
248700                               MOD-FLERS                                  
248800     .                                                                    
248900     SKIP2                                                                
249000 MFS-ROER-EJ-FAELT-SLAG SECTION.                                          
249100                                                                          
249200*    --- ALLA S-LAGER-UTDATA-FÄLT                                         
249300     PERFORM MFS-ROER-EJ-FAELT-SL-INFO                                    
249400     PERFORM MFS-ROER-EJ-FAELT-OI-6PER                                    
249500     PERFORM MFS-ROER-EJ-FAELT-OI-INNEV                                   
249600     PERFORM MFS-ROER-EJ-FAELT-OI-RULL                                    
249700     PERFORM MFS-ROER-EJ-FAELT-OI-FORE                                    
249800     MOVE MFS-ROER-EJ-FAELT TO MOD-KVPB-REF                               
249900                               MOD-TIREFMPB                               
250000                               MOD-TIPBREOI                               
250100     .                                                                    
250200     EJECT                                                                
250300 MFS-ROER-EJ-FAELT-SL-INFO SECTION.                                       
250400                                                                          
250500     MOVE MFS-ROER-EJ-FAELT TO MOD-KVLS                                   
250600                               MOD-KVAKS-S                                
250700                               MOD-KVBEART                                
250800                               MOD-KVPB-REF                               
250900                               MOD-KVREFBER                               
251000     .                                                                    
251100     EJECT                                                                
251200 MFS-ROER-EJ-FAELT-CDC-INFO SECTION.                                      
251300                                                                          
251400     MOVE MFS-ROER-EJ-FAELT TO MOD-TIFINLV                                
251500                               MOD-KVDISP                                 
251600                               MOD-KVAKS                                  
251700                               MOD-KVART-FORAVIS                          
251800                               MOD-KVPB-TOT                               
251900                               MOD-KVQPACK-3                              
252000     .                                                                    
252100     EJECT                                                                
252200 MFS-ROER-EJ-FAELT-OI-6PER SECTION.                                       
252300                                                                          
252400     MOVE +1 TO INDX                                                      
252500     PERFORM UNTIL INDX > 6                                               
252600       MOVE MFS-ROER-EJ-FAELT TO MOD-TIPP(INDX)                           
252700                                 MOD-TIVV-FOM-TOM(INDX)                   
252800                                 MOD-KVOT(INDX)                           
252900                                 MOD-KVOT-CDC(INDX)                       
253000                                 MOD-KVOI(INDX)                           
253100       ADD +1 TO INDX                                                     
253200     END-PERFORM                                                          
253300     .                                                                    
253400     EJECT                                                                
253500 MFS-ROER-EJ-FAELT-OI-RULL SECTION.                                       
253600                                                                          
253700     MOVE MFS-ROER-EJ-FAELT   TO MOD-KVOT-RULL-12                         
253800                                 MOD-KVOT-CDC-RULL-12                     
253900                                 MOD-KVOI-RULL-12                         
254000                                 MOD-KVOT-RULL-6                          
254100                                 MOD-KVOT-CDC-RULL-6                      
254200                                 MOD-KVOI-RULL-6                          
254300                                 MOD-KVOI-REF-RULL-6                      
254400     .                                                                    
254500     EJECT                                                                
254600 MFS-ROER-EJ-FAELT-OI-INNEV SECTION.                                      
254700                                                                          
254800     MOVE MFS-ROER-EJ-FAELT   TO MOD-KVOT-INNEV                           
254900                                 MOD-KVOT-CDC-INNEV                       
255000                                 MOD-KVOI-INNEV                           
255100                                 MOD-KVOI-REF-INNEV                       
255200     .                                                                    
255300     EJECT                                                                
255400 MFS-ROER-EJ-FAELT-OI-FORE SECTION.                                       
255500                                                                          
255600     MOVE +1 TO INDX                                                      
255700     PERFORM UNTIL INDX > 2                                               
255800       MOVE MFS-ROER-EJ-FAELT TO MOD-TIAA-FORE(INDX)                      
255900                                 MOD-KVOT-FORE(INDX)                      
256000                                 MOD-KVOT-CDC-FORE(INDX)                  
256100                                 MOD-KVOI-FORE(INDX)                      
256200                                 MOD-KVOI-REFILL-FORE(INDX)               
256300       ADD +1 TO INDX                                                     
256400     END-PERFORM                                                          
256500     .                                                                    
256600     EJECT                                                                
256700 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
256800                                                                          
256900*    --- ALLA INDATA-FÄLT                                                 
257000     MOVE MFS-ROER-EJ-FAELT   TO MOD-TIPBREOI-IN                          
257100*                                MOD-KVOI-REF-INNEV                       
257200                                 MOD-TIREFMPB-IN                          
257300     .                                                                    
257400     EJECT                                                                
257500 MFS-FORM-ATTR SECTION.                                                   
257600                                                                          
257700*    --- ALLA INDATA-FÄLT                                                 
257800     MOVE MFS-FORMATETS-ATTR TO MOD-TIREFMPB-IN-ATTR                      
257900                                MOD-TIPBREOI-IN-ATTR                      
258000                                                                          
258100     .                                                                    
258200     SKIP2                                                                
258300 MFS-LAES-IN-IGEN SECTION.                                                
258400                                                                          
258500*    --- ALLA INDATA-FÄLT                                                 
258600     MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-TIPBREOI-IN-ATTR                 
258700                                     MOD-TIREFMPB-IN-ATTR                 
258800                                                                          
258900     .                                                                    
259000     EJECT                                                                
259100* --- IMS SEKTIONER ---                                                   
259200     SKIP3                                                                
259300 IMS-GET-MSG SECTION.                                                     
259400                                                                          
259500     MOVE '  QC' TO GODK-STATUSKODER                                      
259600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
259700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
259800     PERFORM IMS-STATUSKONTROLL                                           
259900     .                                                                    
260000     SKIP3                                                                
260100 IMS-INSERT-MSG SECTION.                                                  
260200                                                                          
260300     IF ENGLISH-TEXT                                                      
260400       MOVE 'N' TO MFS-KDHUVOMR                                           
260500     END-IF                                                               
260600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
260700     MOVE SPACE TO GODK-STATUSKODER                                       
260800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
260900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
261000     PERFORM IMS-STATUSKONTROLL                                           
261100     .                                                                    
261200     EJECT                                                                
261300 IMS-GU-WDL711     SECTION.                                               
261400     STRING 'WDL701  (IDARTNR  =' W-IDARTNR-X ')'                         
261500          DELIMITED BY SIZE INTO SSA1                                     
261600     STRING 'WDL711  (IDDC     =' W-IDDC-X ')'                            
261700          DELIMITED BY SIZE INTO SSA2                                     
261800     MOVE '  GE' TO GODK-STATUSKODER                                      
261900     CALL CBLTDLI USING GHU WDL7-PCB DLI-IO-L711 SSA1 SSA2                
262000     MOVE WDL7-STATUS-CODE TO STATUS-WS                                   
262100     PERFORM IMS-STATUSKONTROLL                                           
262200     .                                                                    
262300     SKIP3                                                                
262400                                                                          
262500 IMS-GU-WDL411     SECTION.                                               
262600     STRING 'WDL401  (IDARTNR  =' W-IDARTNR-X ')'                         
262700          DELIMITED BY SIZE INTO SSA1                                     
262800     STRING 'WDL411  (IDDC     =' W-IDDC-X ')'                            
262900          DELIMITED BY SIZE INTO SSA2                                     
263000     MOVE '  GE' TO GODK-STATUSKODER                                      
263100     CALL CBLTDLI USING GHU WDL4-PCB DLI-IO-L411 SSA1 SSA2                
263200     MOVE WDL4-STATUS-CODE TO STATUS-WS                                   
263300     PERFORM IMS-STATUSKONTROLL                                           
263400     .                                                                    
263500     SKIP3                                                                
263600                                                                          
263700 IMS-GU-K711 SECTION.                                                     
263800                                                                          
263900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
264000          DELIMITED BY SIZE INTO SSA1                                     
264100     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
264200          DELIMITED BY SIZE INTO SSA2                                     
264300     MOVE '  GE' TO GODK-STATUSKODER                                      
264400     CALL CBLTDLI USING GU WDK7I-PCB DLI-IO-K711 SSA1 SSA2                
264500     MOVE WDK7I-STATUS-CODE TO STATUS-WS                                  
264600     PERFORM IMS-STATUSKONTROLL                                           
264700     .                                                                    
264800     SKIP3                                                                
264900 IMS-GHU-WDK711     SECTION.                                              
265000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
265100          DELIMITED BY SIZE INTO SSA1                                     
265200     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
265300          DELIMITED BY SIZE INTO SSA2                                     
265400     MOVE '  GE' TO GODK-STATUSKODER                                      
265500     CALL CBLTDLI USING GHU WDK7I-PCB DLI-IO-K711 SSA1 SSA2               
265600     MOVE WDK7I-STATUS-CODE TO STATUS-WS                                  
265700     PERFORM IMS-STATUSKONTROLL                                           
265800     .                                                                    
265900     SKIP3                                                                
266000 IMS-REPL-WDK711 SECTION.                                                 
266100                                                                          
266200     MOVE '  ' TO GODK-STATUSKODER                                        
266300     CALL CBLTDLI USING REPL WDK7I-PCB DLI-IO-K711                        
266400     MOVE WDK7I-STATUS-CODE TO STATUS-WS                                  
266500     PERFORM IMS-STATUSKONTROLL                                           
266600     .                                                                    
266700     EJECT                                                                
266800 IMS-GU-WDK601   SECTION.                                                 
266900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
267000          DELIMITED BY SIZE INTO SSA1                                     
267100     MOVE '  GE' TO GODK-STATUSKODER                                      
267200     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
267300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
267400     PERFORM IMS-STATUSKONTROLL                                           
267500     .                                                                    
267600     SKIP3                                                                
267700 IMS-GNP-WDK611   SECTION.                                                
267800     MOVE 'WDK611  '       TO SSA1                                        
267900     MOVE '  GE' TO GODK-STATUSKODER                                      
268000     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
268100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
268200     PERFORM IMS-STATUSKONTROLL                                           
268300     .                                                                    
268400     EJECT                                                                
268500 IMS-GHU-WDK611   SECTION.                                                
268600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
268700          DELIMITED BY SIZE INTO SSA1                                     
268800     MOVE 'WDK611  '       TO SSA2                                        
268900     MOVE '  GE' TO GODK-STATUSKODER                                      
269000     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
269100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
269200     PERFORM IMS-STATUSKONTROLL                                           
269300     .                                                                    
269400     SKIP3                                                                
269500 IMS-GNP-WDK621   SECTION.                                                
269600     STRING 'WDK621  (DAPRLIST=>' W-DAPRLIST-X ')'                        
269700            DELIMITED BY SIZE INTO SSA1                                   
269800     MOVE '  GE' TO GODK-STATUSKODER                                      
269900     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK621 SSA1                   
270000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
270100     PERFORM IMS-STATUSKONTROLL                                           
270200     .                                                                    
270300     EJECT                                                                
270400 IMS-GU-WDK611       SECTION.                                             
270500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
270600          DELIMITED BY SIZE INTO SSA1                                     
270700     MOVE 'WDK611  '       TO SSA2                                        
270800     MOVE '  GE' TO GODK-STATUSKODER                                      
270900     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
271000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
271100     PERFORM IMS-STATUSKONTROLL                                           
271200     .                                                                    
271300     EJECT                                                                
271400 IMS-GNP-WDK629     SECTION.                                              
271500     MOVE 'WDK629  '       TO SSA1                                        
271600     MOVE '  GE' TO GODK-STATUSKODER                                      
271700     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK629 SSA1                   
271800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
271900     PERFORM IMS-STATUSKONTROLL                                           
272000     .                                                                    
272100     EJECT                                                                
272200 IMS-REPL-WDK611    SECTION.                                              
272300     MOVE '  ' TO GODK-STATUSKODER                                        
272400     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
272500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
272600     PERFORM IMS-STATUSKONTROLL                                           
272700     .                                                                    
272800     EJECT                                                                
272900 IMS-GU-ARTM01         SECTION.                                           
273000     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
273100            DELIMITED BY SIZE INTO SSA1                                   
273200     MOVE '  GE' TO GODK-STATUSKODER                                      
273300     CALL CBLTDLI USING GU ARTM-PCB DLI-IO-ARTM01 SSA1                    
273400     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
273500     PERFORM IMS-STATUSKONTROLL                                           
273600     .                                                                    
273700     SKIP3                                                                
273800 IMS-GU-BENA01-BSEQ SECTION.                                              
273900     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
274000          DELIMITED BY SIZE INTO SSA1                                     
274100     MOVE '  GE' TO GODK-STATUSKODER                                      
274200     CALL CBLTDLI USING GU BENA-PCB DLI-IO-BENA01 SSA1                    
274300     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
274400     PERFORM IMS-STATUSKONTROLL                                           
274500     .                                                                    
274600 IMS-GNP-BENA11 SECTION.                                                  
274700     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
274800          DELIMITED BY SIZE INTO SSA1                                     
274900     MOVE '  GE' TO GODK-STATUSKODER                                      
275000     CALL CBLTDLI USING GU BENA-PCB DLI-IO-BENA11 SSA1                    
275100     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
275200     PERFORM IMS-STATUSKONTROLL                                           
275300     .                                                                    
275400     EJECT                                                                
275500 IMS-GU-LEVA16 SECTION.                                                   
275600                                                                          
275700     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
275800          DELIMITED BY SIZE INTO SSA1                                     
275900     STRING 'WLLEVA16(IDDC =' W-IDDC-X ')'                                
276000          DELIMITED BY SIZE INTO SSA2                                     
276100     MOVE '  GE' TO GODK-STATUSKODER                                      
276200     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-LEVA16 SSA1 SSA2               
276300     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
276400     PERFORM IMS-STATUSKONTROLL                                           
276500     .                                                                    
276600     EJECT                                                                
276700                                                                          
276800 IMS-GU-WDB601    SECTION.                                                
276900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
277000          DELIMITED BY SIZE INTO SSA1                                     
277100     MOVE '  GE' TO GODK-STATUSKODER                                      
277200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
277300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
277400     PERFORM IMS-STATUSKONTROLL                                           
277500     .                                                                    
277600     EJECT                                                                
277700 IMS-GU-WDB616    SECTION.                                                
277800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
277900          DELIMITED BY SIZE INTO SSA1                                     
278000     STRING 'WDB616  (IDDCREF  =' W-IDDC-B616-X ')'                       
278100          DELIMITED BY SIZE INTO SSA2                                     
278200     MOVE '  '                TO GODK-STATUSKODER                         
278300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B616 SSA1 SSA2            
278400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
278500     PERFORM IMS-STATUSKONTROLL                                           
278600     .                                                                    
278700     EJECT                                                                
278800 IMS-GU-WDK701 SECTION.                                                   
278900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
279000          DELIMITED BY SIZE INTO SSA1                                     
279100     MOVE '  GE' TO GODK-STATUSKODER                                      
279200     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-K701 SSA1                      
279300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
279400     PERFORM IMS-STATUSKONTROLL                                           
279500     .                                                                    
279600     EJECT                                                                
279700 IMS-GNP-WDK711 SECTION.                                                  
279800     MOVE 'WDK711   ' TO SSA1                                             
279900     MOVE '  GE' TO GODK-STATUSKODER                                      
280000     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-K711   SSA1                   
280100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
280200     PERFORM IMS-STATUSKONTROLL                                           
280300     .                                                                    
280400     EJECT                                                                
280500                                                                          
280600 IMS-GU-WDK711 SECTION.                                                   
280700     MOVE SPACES      TO SSA1                                             
280800                         SSA2                                             
280900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
281000          DELIMITED BY SIZE INTO SSA1                                     
281100     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
281200          DELIMITED BY SIZE INTO SSA2                                     
281300     MOVE '  GE' TO GODK-STATUSKODER                                      
281400     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-K711   SSA1 SSA2               
281500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
281600     PERFORM IMS-STATUSKONTROLL                                           
281700     .                                                                    
281800     EJECT                                                                
281900 IMS-GU-WDK722 SECTION.                                                   
282000                                                                          
282100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
282200          DELIMITED BY SIZE INTO SSA1                                     
282300     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
282400          DELIMITED BY SIZE INTO SSA2                                     
282500     STRING 'WDK722  (KDSEGKEY =' W-KDSEGKEY-K722-X ')'                   
282600          DELIMITED BY SIZE INTO SSA3                                     
282700     MOVE '  GE'              TO GODK-STATUSKODER                         
282800     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK722 SSA1 SSA2          
282900                                                       SSA3               
283000     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
283100     PERFORM IMS-STATUSKONTROLL                                           
283200     .                                                                    
283300     EJECT                                                                
283400 IMS-GN-WDB601    SECTION.                                                
283500     MOVE 'WDB601 ' TO SSA1                                               
283600     MOVE '  GB' TO GODK-STATUSKODER                                      
283700     CALL CBLTDLI USING GN WDB6-GN-PCB DLI-IO-AREA-B601-NEXT SSA1         
283800     MOVE WDB6-GN-STATUS-CODE    TO STATUS-WS                             
283900     PERFORM IMS-STATUSKONTROLL                                           
284000     .                                                                    
284100     EJECT                                                                
284200 IMS-GU-WDK712   SECTION.                                                 
284300                                                                          
284400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
284500          DELIMITED BY SIZE INTO SSA1                                     
284600     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
284700          DELIMITED BY SIZE INTO SSA2                                     
284800     MOVE '  GE' TO GODK-STATUSKODER                                      
284900     CALL CBLTDLI USING GU WDK7I-PCB DLI-IO-WDK712                        
285000          SSA1 SSA2                                                       
285100     MOVE WDK7I-STATUS-CODE TO STATUS-WS                                  
285200     PERFORM IMS-STATUSKONTROLL                                           
285300     .                                                                    
285400     EJECT                                                                
285500 IMS-GN-W6D111-W6D1SEQ  SECTION.                                          
285600                                                                          
285700     STRING 'W6D111  (W6D1HSEQ =' W-W6D1HSEQ-X ')'                        
285800            DELIMITED BY SIZE INTO SSA1                                   
285900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
286000     CALL CBLTDLI USING GN W6D1-PCB DLI-IO-W6D111 SSA1                    
286100     MOVE W6D1-STATUS-CODE TO STATUS-WS                                   
286200     PERFORM IMS-STATUSKONTROLL                                           
286300     .                                                                    
286400     EJECT                                                                
286500 IMS-GU-WDL811      SECTION.                                              
286600                                                                          
286700     STRING 'WDL801  (IDARTNR  =' W-IDARTNR-X ')'                         
286800          DELIMITED BY SIZE INTO SSA1                                     
286900     STRING 'WDL811  (TIAAAA   =' W-TIAAAA-X ')'                          
287000          DELIMITED BY SIZE INTO SSA2                                     
287100     MOVE '  GE' TO GODK-STATUSKODER                                      
287200     CALL CBLTDLI USING GU WDL8-PCB DLI-IO-WDL811 SSA1 SSA2               
287300     MOVE WDL8-STATUS-CODE TO STATUS-WS                                   
287400     PERFORM IMS-STATUSKONTROLL                                           
287500     .                                                                    
287600     EJECT                                                                
287700                                                                          
287800 IMS-STATUSKONTROLL SECTION.                                              
287900                                                                          
288000     SET STATUS-IX TO 1                                                   
288100     SEARCH GODK-STATUS                                                   
288200       AT END                                                             
288300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
288400         DELIMITED BY SIZE INTO FELTEXT                                   
288500         CALL FELLOG                                                      
288600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
288700         CONTINUE                                                         
288800     END-SEARCH                                                           
288900     .                                                                    
289000     EJECT                                                                
289100 DB2-DCL-OPN-TP1ARTK-CRS  SECTION.                                        
289200     MOVE 'DB2-DCL-OPN-TP1ARTK   ' TO  WS-DB2-SEKTION                     
289300                                                                          
289400     MOVE 000100  TO GOOD-SQLCODECODES                                    
289500                                                                          
289600     EXEC SQL                                                             
289700         DECLARE TP1ARTK-CRS CURSOR FOR                                   
289800           SELECT  A.IDKAMP                                               
289900                  ,A.IDARTNR                                              
290000                  ,B.TISTADAT_KAMP                                        
290100                  ,B.TISTODAT_KAMP                                        
290200                                                                          
290300           FROM    TP1ARTK A                                              
290400                  ,TP1KAMP B                                              
290500                                                                          
290600           WHERE   A.IDARTNR = :W-IDARTNR                                 
290700               AND A.IDKAMP  =  B.IDKAMP                                  
290800                                                                          
290900           ORDER BY B.IDKAMP                                              
291000     END-EXEC                                                             
291100                                                                          
291200     MOVE 000100  TO GOOD-SQLCODECODES                                    
291300     EXEC SQL OPEN TP1ARTK-CRS END-EXEC                                   
291400     .                                                                    
291500     EJECT                                                                
291600 DB2-FETCH-TP1ARTK-CRS  SECTION.                                          
291700     MOVE 'DB2-FETCH-TP1ARTK   ' TO  WS-DB2-SEKTION                       
291800     SKIP2                                                                
291900     MOVE 000100  TO GOOD-SQLCODECODES                                    
292000     EXEC SQL                                                             
292100         FETCH TP1ARTK-CRS INTO                                           
292200                    :TP1KAMP-IDKAMP                                       
292300                   ,:TP1ARTK-IDARTNR                                      
292400                   ,:TP1KAMP-TISTADAT-KAMP                                
292500                   ,:TP1KAMP-TISTODAT-KAMP                                
292600     END-EXEC                                                             
292700                                                                          
292800     MOVE SQLCODE TO SQLCODE-WS                                           
292900     PERFORM DB2-STATUS-CHECK                                             
293000     .                                                                    
293100     EJECT                                                                
293200 DB2-CLOSE-TP1ARTK-CRS  SECTION.                                          
293300     MOVE 'DB2-CLOSE-TP1ARTK   ' TO  WS-DB2-SEKTION                       
293400                                                                          
293500     EXEC SQL CLOSE TP1ARTK-CRS END-EXEC                                  
293600     .                                                                    
293700     EJECT                                                                
293800 DB2-STATUS-CHECK  SECTION.                                               
293900                                                                          
294000     SET SQLCODE-IX TO 1                                                  
294100     SEARCH GOOD-SQLCODE                                                  
294200       AT END                                                             
294300*         STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
294400*         DELIMITED BY SIZE INTO ERROR-TEXT                               
294500          CALL FELLOG                                                     
294600       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
294700     END-SEARCH                                                           
294800     .                                                                    
294900     EJECT                                                                
295000*    -COPY WY2000P1                                                       
