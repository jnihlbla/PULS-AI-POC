000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4029400.                                                
000400 AUTHOR.         JAN-ERIK FRANTZEN.                                       
000500 DATE-WRITTEN.   90/11/16.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION.                                                            
000900*        LÄSER ALLA ORDERRADER PÅ WDQ1 (ORDERBEKRÄFTELSER)                
001000*        SKRIVER UT DE RADER SOM UPPFYLLER DE VILLKOR                     
001100*        SOM ANGIVITS FRÅN BILD 4281 (VARIFRÅN PROGRAMMET                 
001200*        STARTAS).                                             .          
001300*                                                                         
001400*        PROGRAMMET LÄSER   WLORQM  (WDQ1)                                
001500*        PROGRAMMET LÄSER   WLORQI  (WDQ2)                                
001600*        PROGRAMMET LÄSER   WLPROC  (WDE8)                                
001700*        PROGRAMMET LÄSER   WLBENA  (WDD3)                                
001800*        PROGRAMMET LÄSER   WL4732  (WDR1)                                
001900*        PROGRAMMET LÄSER   WLSATB  (WDJ1)                                
002000*                                                                         
002100*    INDATA.                                                              
002200*        MID: W4I28101C0                                                  
002300*                                                                         
002400*    UTDATA.                                                              
002500*        MOD: W4O29401C0                                                  
002600                                                                          
002700                                                                          
002800 ENVIRONMENT DIVISION.                                                    
002900                                                                          
003000 DATA DIVISION.                                                           
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300*    -COPY WY2000W1                                                       
003400     SKIP3                                                                
003500 77  IDPGM                       PIC X(08)   VALUE 'W4029400'.            
003600                                                                          
003700 77  JA                          PIC X       VALUE 'J'.                   
003800 77  NEJ                         PIC X       VALUE 'N'.                   
003900                                                                          
004000 77  SW-MID-ORDER-SLUT           PIC X       VALUE 'N'.                   
004100 77  IX1                         PIC S9(9)  VALUE +0    COMP SYNC.        
004200 77  IX2                         PIC S9(9)  VALUE +0    COMP SYNC.        
004300 77  IX-BEFRAKT                  PIC S9(9)  VALUE +0    COMP SYNC.        
004400 77  RADANT                      PIC S9(9)  VALUE +99   COMP-3.           
004500 77  RADANT-MAX                  PIC S9(9)  VALUE +45   COMP-3.           
004600 77  SIDANT                      PIC S9(9)  VALUE +0    COMP-3.           
004700 77  RADANT-MID                  PIC S9(9)  VALUE +0    COMP SYNC.        
004800                                                                          
004900                                                                          
005000 77  ALLT-SW                     PIC X.                                   
005100     88  ALLT-OK                             VALUE 'J'.                   
005200     88  ALLT-FEL                            VALUE 'N'.                   
005300                                                                          
005400 77  PRINTER-SW                  PIC X       VALUE 'N'.                   
005500     88  PRINTER-OPEN                        VALUE 'J'.                   
005600     88  PRINTER-CLOSE                       VALUE 'N'.                   
005700                                                                          
005800                                                                          
005900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006000     88  GODK-MID                            VALUE '4281'.                
006100                                                                          
006200 77  WS-INDX-SATS                PIC S9(9)   VALUE +0  COMP SYNC.         
006300 77  WS-INDX-SATS-MAX            PIC S9(9)   VALUE +5  COMP SYNC.         
006400 77  W-IDDC                      PIC  X(2)   VALUE SPACE.                 
006500                                                                          
006600 01  WS-SATS-TABELL.                                                      
006700     03  WS-IDARTNR-SATS         PIC S9(9)   COMP-3 OCCURS 5.             
006800                                                                          
006900 01  WS-IDKUNDRF                 PIC X(10).                               
007000 01  FILLER REDEFINES WS-IDKUNDRF.                                        
007100     03  WS-IDORDNR5             PIC 9(5).                                
007200     03  WS-IDKUNDRF-6--10       PIC X(5).                                
007300 01  FILLER REDEFINES WS-IDKUNDRF.                                        
007400     03  WS-IDORDNR7             PIC 9(7).                                
007500     03  FILLER                  PIC X(3).                                
007600     EJECT                                                                
007700                                                                          
007800 01  WS-PV-FTG-NAMN              PIC X(29)                                
007900     VALUE 'VOLVO CAR CORPORATION, PARTS '.                               
008000                                                                          
008100 01  CURRENT-DATE.                                                        
008200     03  CURRENT-YY              PIC 9(2).                                
008300     03  CURRENT-MM              PIC 9(2).                                
008400     03  CURRENT-DD              PIC 9(2).                                
008500 01  DAGENS-DATUM REDEFINES CURRENT-DATE   PIC 9(6).                      
008600                                                                          
008700 01  W-TIREGDAT                  PIC 9(7).                                
008800 01  W-TIREGDAT-X REDEFINES W-TIREGDAT.                                   
008900     03  FILLER                  PIC X.                                   
009000     03  W-TIREGDAT6.                                                     
009100       05 W-TIREGDAT-YY          PIC X(2).                                
009200       05 W-TIREGDAT-MM          PIC X(2).                                
009300       05 W-TIREGDAT-DD          PIC X(2).                                
009400                                                                          
009500 01  W-DATRPAVD                  PIC 9(8).                                
009600 01  W-DATRPAVT-X REDEFINES W-DATRPAVD.                                   
009700     03  FILLER                  PIC X(2).                                
009800     03  W-TIAAMMDD6.                                                     
009900       05 W-TIAAMMDD-YY          PIC X(2).                                
010000       05 W-TIAAMMDD-MM          PIC X(2).                                
010100       05 W-TIAAMMDD-DD          PIC X(2).                                
010200                                                                          
010300                                                                          
010400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
010500 01  GENERELLA-SUBPROGRAM.                                                
010600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010800     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
010900     EJECT                                                                
011000*    --- AREA FÖR SUBPROGRAM W006PRS1                                     
011100*                                                                         
011200 01  FILLER                      PIC X(16)  VALUE 'W006PRS1'.             
011300                                                                          
011400*01  -COPY W006PRAR                                                       
011500                                                                          
011600 01  WS-PRT-AREA.                                                         
011700     03 WS-PRT-PRINTER           PIC X(8)    VALUE SPACE.                 
011800     03 WS-PRT-LISTRAD.                                                   
011900        05 FILLER                PIC X(3)    VALUE SPACE.                 
012000        05 WS-PRT-RAD            PIC X(132)  VALUE SPACE.                 
012100     03 WS-DUMMY                 PIC X(1)    VALUE SPACE.                 
012200     EJECT                                                                
012300*    --- AREOR FÖR MSG-HANTERING                                          
012400*                                                                         
012500 01  FILLER                      PIC X(16)  VALUE 'MSG-AREA'.             
012600                                                                          
012700*01  -COPY WMSGAREA                                                       
012800     EJECT                                                                
012900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013000*                                                                         
013100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013200                                                                          
013300 01  NYCKLAR-TILL-DLI.                                                    
013400                                                                          
013500                                                                          
013600*----> NYCKEL TILL ORDERBEKR     WDQ1                                     
013700                                                                          
013800     03  W-OBKR-NYCKEL-MIN-X.                                             
013900         05  W-OBKR-IDORDER-MIN  PIC S9(7) COMP-3.                        
014000         05  W-OBKR-IDARTNR-MIN  PIC S9(9) COMP-3.                        
014100         05  W-OBKR-IDLOPNR-MIN  PIC S9(3) COMP-3.                        
014200         05  W-OBKR-IDSEKVNR-MIN PIC S9(3) COMP-3.                        
014300         05  W-OBKR-IDDC-MIN     PIC X(2).                                
014400         05  W-OBKR-KDORDBEK-MIN PIC  9(2).                               
014500                                                                          
014600     03  W-OBKR-NYCKEL-MAX-X.                                             
014700         05  W-OBKR-IDORDER-MAX  PIC S9(7) COMP-3.                        
014800         05  W-OBKR-IDARTNR-MAX  PIC S9(9) COMP-3.                        
014900         05  W-OBKR-IDLOPNR-MAX  PIC S9(3) COMP-3.                        
015000         05  W-OBKR-IDSEKVNR-MAX PIC S9(3) COMP-3.                        
015100         05  W-OBKR-IDDC-MAX     PIC X(2).                                
015200         05  W-OBKR-KDORDBEK-MAX PIC  9(2).                               
015300                                                                          
015400     03  W-IDSYSTEM-X.                                                    
015500         05  W-IDSYSTEM          PIC  X(4) VALUE 'PROF'.                  
015600                                                                          
015700*----> DIREKTNYCKEL TILL ORDERHUVUDET  WDQ2                               
015800                                                                          
015900     03  W-IDORDER-X.                                                     
016000         05  W-IDORDER           PIC S9(7) COMP-3.                        
016100                                                                          
016200*----> DIREKTNYCKEL TILL ARBETSTABELL  WDQ212                             
016300                                                                          
016400     03  W-IDDC-X.                                                        
016500         05  W-IDDC-WDQ212       PIC  X(2).                               
016600                                                                          
016700*----> DIREKTNYCKEL TILL PROFORMAHUVUDET  WDE8                            
016800                                                                          
016900     03  W-IDGMTREF-X.                                                    
017000         05  W-IDDISTR           PIC S9(5) COMP-3.                        
017100         05  W-IDKUNDNR          PIC S9(7) COMP-3.                        
017200         05  W-IDKUNDRF          PIC X(10) VALUE SPACE.                   
017300         05  W-IDORDNR7-FILLER REDEFINES W-IDKUNDRF.                      
017400            07 W-IDORDNR7        PIC 9(7).                                
017500            07 FILLER            PIC X(3).                                
017600                                                                          
017700*----> DIREKTNYCKEL TILL WDD301                                           
017800                                                                          
017900     03  W-IDARTNR-X.                                                     
018000         05  W-IDARTNR           PIC S9(9) COMP-3.                        
018100                                                                          
018200*----> DIREKTNYCKEL TILL WDD311                                           
018300                                                                          
018400     03  W-IDSKYLT-X.                                                     
018500         05  W-IDSKYLT           PIC  X(3).                               
018600                                                                          
018700*----> FRAKTTEXT 4732                                                     
018800                                                                          
018900     03  W-4732-IDHTYP-X.                                                 
019000         05  W-4732-IDHTYP       PIC  X(4)  VALUE '4732'.                 
019100         05  W-4732-KDFRAKT      PIC S9(3)  COMP-3.                       
019200         05  W-4732-LOW-VALUE    PIC  X(24) VALUE LOW-VALUE.              
019300                                                                          
019400*----> SATSREGISTRET                                                      
019500                                                                          
019600     03  W-WDJ1CSEQ-X.                                                    
019700         05  W-IDLEVNR-J1        PIC  X(5)  VALUE SPACE.                  
019800         05  W-BELEVART-J1       PIC X(30)  VALUE SPACE.                  
019900         05  W-IDARTNR-J1        PIC S9(9)  VALUE +0 COMP-3.              
020000                                                                          
020100     03  W-IDLEVNR-X             PIC  X(5)  VALUE '1002 '.                
020200                                                                          
020300*    --- STATUS-KOD FRÅN IMS                                              
020400                                                                          
020500 01  STATUS-WS                   PIC  X(02).                              
020600     88  SEGMENT-FINNS                       VALUE '  '.                  
020700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
020800     88  END-OF-DATA                         VALUE 'GB'.                  
020900     SKIP2                                                                
021000 01  GODK-STATUSKODER.                                                    
021100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021200                                                                          
021300 01  SSA1                        PIC X(96).                               
021400 01  SSA2                        PIC X(96).                               
021500 01  SSA3                        PIC X(96).                               
021600                                                                          
021700***********************************                                       
021800*  PRINTRADER FÖR ORDERBEKRÄFTELSE*                                       
021900***********************************                                       
022000                                                                          
022100 01  HRAD-STRECK.                                                         
022200     03   FILLER                  PIC X(112) VALUE ALL '-'.               
022300                                                                          
022400 01  HRAD1.                                                               
022500     03   HRAD1-FTG-NAMN          PIC X(29).                              
022600     03   FILLER                  PIC X(3)  VALUE SPACE.                  
022700     03   HRAD1-RUBRIK            PIC X(29).                              
022800     03   FILLER                  PIC X(6)  VALUE 'DATE '.                
022900     03   HRAD1-DATUM.                                                    
023000       05 HRAD1-YY                PIC 9(2).                               
023100       05 HRAD1-MM                PIC 9(2).                               
023200       05 HRAD1-DD                PIC 9(2).                               
023300     03 FILLER                    PIC X(25) VALUE SPACE.                  
023400     03 FILLER                    PIC X(6)  VALUE 'PAGE  '.               
023500     03 HRAD1-SIDNR               PIC ZZ9   VALUE ZERO.                   
023600                                                                          
023700 01  HRAD2.                                                               
023800     03   FILLER                  PIC X(37) VALUE                         
023900          'NAME/ADDRESS'.                                                 
024000     03   FILLER                  PIC X(40) VALUE                         
024100          'DISTR    CUST    ORDER    DC    CUST.REF'.                     
024200                                                                          
024300 01  HRAD3.                                                               
024400     03   HRAD3-BEGMT-RAD1        PIC X(35).                              
024500     03   FILLER                  PIC X(3)  VALUE SPACE.                  
024600     03   HRAD3-IDDISTR           PIC Z(3)9.                              
024700     03   FILLER                  PIC X(2)  VALUE SPACE.                  
024800     03   HRAD3-IDKUNDNR          PIC Z(5)9.                              
024900     03   FILLER                  PIC X(2)  VALUE SPACE.                  
025000     03   HRAD3-IDORDNR7          PIC Z(6)9.                              
025100     03   FILLER                  PIC X(3)  VALUE SPACE.                  
025200     03   HRAD3-KDORDKL           PIC Z(2)9.                              
025300     03   FILLER                  PIC X(4)  VALUE SPACE.                  
025400     03   HRAD3-BEKUNDRF          PIC X(15).                              
025500                                                                          
025600 01  HRAD4.                                                               
025700     03   HRAD4-BEGMT-RAD2        PIC X(35).                              
025800     03   FILLER                  PIC X(2)  VALUE SPACE.                  
025900     03   FILLER                  PIC X(75) VALUE ALL '-'.                
026000                                                                          
026100 01  HRAD5.                                                               
026200     03   HRAD5-ADGMT-GATA        PIC X(35).                              
026300     03   FILLER                  PIC X(2)  VALUE SPACE.                  
026400     03   FILLER                  PIC X(75) VALUE ALL '-'.                
026500                                                                          
026600 01  HRAD6.                                                               
026700     03   HRAD6-ADGMT-PADR        PIC X(35).                              
026800     03   FILLER                  PIC X(2)  VALUE SPACE.                  
026900     03   FILLER                  PIC X(49) VALUE                         
027000          'REG.DATE   PLAN.TRANSP.DEP  FC  MODE OF TRANSPORT'.            
027100                                                                          
027200 01  HRAD7.                                                               
027300     03   HRAD7-ADGMT-LAND        PIC X(35).                              
027400     03   FILLER                  PIC X(2)  VALUE SPACE.                  
027500     03   HRAD7-TIREGDAT-YY       PIC X(2).                               
027600     03   HRAD7-TIREGDAT-MM       PIC X(2).                               
027700     03   HRAD7-TIREGDAT-DD       PIC X(2).                               
027800     03   FILLER                  PIC X(5)  VALUE SPACE.                  
027900     03   HRAD7-TIAAMMDD          PIC X(8).                               
028000     03   FILLER                  PIC X(9)  VALUE SPACE.                  
028100     03   HRAD7-KDFRAKT           PIC Z(2).                               
028200     03   FILLER                  PIC X(2)  VALUE SPACE.                  
028300     03   HRAD7-BEFRAKT           PIC X(20).                              
028400                                                                          
028500 01  HRAD8.                                                               
028600     03   FILLER                  PIC X(43) VALUE                         
028700          'CODE   DATE      PARTNO  DESCRIPTION'.                         
028800     03   FILLER                  PIC X(34) VALUE                         
028900          '  QTY      Q1   CW   ORDER   AVAIL'.                           
029000                                                                          
029100 01  HRAD9.                                                               
029200     03   FILLER                  PIC X(43) VALUE SPACE.                  
029300     03   FILLER                  PIC X(34) VALUE                         
029400          '                       REF    DATE'.                           
029500                                                                          
029600 01  RAD.                                                                 
029700     03   RAD-KDORDBEK            PIC Z(2).                               
029800     03   RAD-ASTERISK            PIC X(1)  VALUE SPACE.                  
029900     03   FILLER                  PIC X(2)  VALUE SPACE.                  
030000     03   RAD-TIREGDAT            PIC Z(6).                               
030100     03   FILLER                  PIC X(2)  VALUE SPACE.                  
030200     03   RAD-IDARTNR             PIC Z(7)9.                              
030300     03   RAD-STRECK              PIC X(1).                               
030400     03   RAD-REKSIFFR            PIC 9(1).                               
030500     03   FILLER                  PIC X(2)  VALUE SPACE.                  
030600     03   RAD-BEART               PIC X(15).                              
030700     03   FILLER                  PIC X(2)  VALUE SPACE.                  
030800     03   RAD-KVBEART             PIC Z(5)9.                              
030900     03   FILLER                  PIC X(2)  VALUE SPACE.                  
031000     03   RAD-KVQPACK-1           PIC Z(5)9.                              
031100     03   FILLER                  PIC X(3)  VALUE SPACE.                  
031200     03   RAD-IDDC                PIC X(2).                               
031300     03   FILLER                  PIC X(2)  VALUE SPACE.                  
031400     03   RAD-IDORDNR             PIC Z(7).                               
031500     03   FILLER                  PIC X(2)  VALUE SPACE.                  
031600     03   RAD-TITPO               PIC 9(6).                               
031700                                                                          
031800     EJECT                                                                
031900*    --- IMS FUNKTIONSKODER                                               
032000*01  -COPY W0003                                                          
032100     EJECT                                                                
032200*    ---  DLI INPUT-OUTPUT AREA                                           
032300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA0'.        
032400                                                                          
032500 01  DLI-IO-AREA0.                                                        
032600*        05 MID  -COPY W4I28101    -PRE MID-                              
032700     EJECT                                                                
032800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA1'.        
032900                                                                          
033000 01  DLI-IO-AREA1.                                                        
033100     03  WLORQM01.                                                        
033200*        05 -COPY WDQ101                                                  
033300     EJECT                                                                
033400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
033500                                                                          
033600 01  DLI-IO-AREA2.                                                        
033700     03  WLORQI01.                                                        
033800*        05 -COPY WDQ201                                                  
033900     EJECT                                                                
034000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA3'.        
034100                                                                          
034200 01  DLI-IO-AREA3.                                                        
034300     03  WLBENA11.                                                        
034400*        05 -COPY WDD311                                                  
034500                                                                          
034600     EJECT                                                                
034700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA4'.        
034800                                                                          
034900 01  DLI-IO-AREA4.                                                        
035000                                                                          
035100     03  IO-AREA4                PIC X(200)  VALUE SPACE.                 
035200                                                                          
035300     03  WL473201 REDEFINES IO-AREA4.                                     
035400*        05 -COPY WDGX473B                                                
035500     03  WL473201 REDEFINES IO-AREA4.                                     
035600*        05 -COPY WDGX4732                                                
035700     EJECT                                                                
035800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA5'.        
035900                                                                          
036000 01  DLI-IO-AREA5.                                                        
036100                                                                          
036200     03  IO-AREA5                PIC X(400)  VALUE SPACE.                 
036300                                                                          
036400     03  WL473201 REDEFINES IO-AREA5.                                     
036500*        05 -COPY WDJ111     -PRE SATB-                                   
036600*        05 -COPY WDJ101     -PRE SATB-                                   
036700     EJECT                                                                
036800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA6'.        
036900                                                                          
037000 01  DLI-IO-AREA6.                                                        
037100     03  WLPROC01.                                                        
037200*        05 -COPY WDE801                                                  
037300     EJECT                                                                
037400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA7'.        
037500                                                                          
037600 01  DLI-IO-AREA7.                                                        
037700     03  WLORQI12.                                                        
037800         05 -COPY WDQ212                                                  
037900     EJECT                                                                
038000 LINKAGE SECTION.                                                         
038100                                                                          
038200*01  -COPY W0009      -PRE MSG-                                           
038300                                                                          
038400*01  -COPY W0009      -PRE ALT1-                                          
038500     EJECT                                                                
038600*01  -COPY W0008      -PRE ORQM-                                          
038700     05  FILLER                  PIC X.                                   
038800                                                                          
038900*01  -COPY W0008      -PRE ORQI-                                          
039000     05  FILLER                  PIC X.                                   
039100     EJECT                                                                
039200*01  -COPY W0008      -PRE BENA-                                          
039300     05  FILLER                  PIC X.                                   
039400                                                                          
039500*01  -COPY W0008      -PRE 4732-                                          
039600     05  FILLER                  PIC X.                                   
039700     EJECT                                                                
039800*01  -COPY W0008      -PRE SATB-                                          
039900     05  FILLER                  PIC X.                                   
040000                                                                          
040100*01  -COPY W0008      -PRE PROC-                                          
040200     05  FILLER                  PIC X.                                   
040300     EJECT                                                                
040400 PROCEDURE DIVISION  USING MSG-PCB                                        
040500                           ALT1-PCB                                       
040600                           ORQM-PCB                                       
040700                           ORQI-PCB                                       
040800                           BENA-PCB                                       
040900                           4732-PCB                                       
041000                           SATB-PCB                                       
041100                           PROC-PCB.                                      
041200 MAIN SECTION.                                                            
041300                                                                          
041400     ENTRY 'DLITCBL' USING MSG-PCB                                        
041500                           ALT1-PCB                                       
041600                           ORQM-PCB                                       
041700                           ORQI-PCB                                       
041800                           BENA-PCB                                       
041900                           4732-PCB                                       
042000                           SATB-PCB                                       
042100                           PROC-PCB.                                      
042200                                                                          
042300     PERFORM IMS-GU-MSG                                                   
042400     IF SEGMENT-FINNS                                                     
042500        PERFORM A-INIT                                                    
042600        IF ALLT-OK                                                        
042700           PERFORM B-NAESTA-ORDER-I-MID                                   
042800           PERFORM UNTIL SW-MID-ORDER-SLUT = JA                           
042900              PERFORM C-BEHANDLA-ORDER                                    
043000              PERFORM B-NAESTA-ORDER-I-MID                                
043100           END-PERFORM                                                    
043200        END-IF                                                            
043300     END-IF                                                               
043400                                                                          
043500     PERFORM Z-FINIT                                                      
043600                                                                          
043700     MOVE ZERO TO RETURN-CODE                                             
043800     GOBACK                                                               
043900     .                                                                    
044000     EJECT                                                                
044100 A-INIT SECTION.                                                          
044200                                                                          
044300     MOVE JA  TO ALLT-SW                                                  
044400                                                                          
044500     IF MSG-DUBBLA-TRANSKODER                                             
044600        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I28101                
044700     ELSE                                                                 
044800        MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W4I28101                  
044900     END-IF                                                               
045000                                                                          
045100     MOVE MSG-IDTRANS-1 TO W-IDTRANS                                      
045200     IF NOT GODK-MID                                                      
045300        MOVE NEJ TO ALLT-SW                                               
045400     END-IF                                                               
045500                                                                          
045600     MOVE MID-KDPRT    TO WS-PRT-PRINTER                                  
045700     IF WS-PRT-PRINTER = 'OBK     '                                       
045800       MOVE SPACE      TO HRAD1-FTG-NAMN                                  
045900       MOVE 'W40295  ' TO WS-PRT-PRINTER                                  
046000     END-IF                                                               
046100                                                                          
046200                                                                          
046300     IF ALLT-SW = JA                                                      
046400        CALL W006PRS1 USING PRT-SPOOL-OVR                                 
046500                            PRT-OPEN                                      
046600                            WS-PRT-PRINTER                                
046700                            ALT1-PCB                                      
046800                            WS-DUMMY                                      
046900                            WS-DUMMY                                      
047000                                                                          
047100        MOVE JA TO PRINTER-SW                                             
047200     END-IF                                                               
047300                                                                          
047400     ACCEPT CURRENT-DATE  FROM DATE                                       
047500     .                                                                    
047600     EJECT                                                                
047700                                                                          
047800 B-NAESTA-ORDER-I-MID SECTION.                                            
047900                                                                          
048000     ADD +1              TO RADANT-MID                                    
048100                                                                          
048200     PERFORM UNTIL MID-KDCMD (RADANT-MID) = 'X' OR                        
048300        RADANT-MID > +13                                                  
048400        ADD +1           TO RADANT-MID                                    
048500     END-PERFORM                                                          
048600                                                                          
048700     IF RADANT-MID > +13                                                  
048800        MOVE JA          TO SW-MID-ORDER-SLUT                             
048900     ELSE                                                                 
049000        MOVE NEJ         TO SW-MID-ORDER-SLUT                             
049100     END-IF                                                               
049200     .                                                                    
049300     EJECT                                                                
049400                                                                          
049500                                                                          
049600 C-BEHANDLA-ORDER   SECTION.                                              
049700                                                                          
049800                                                                          
049900     MOVE +100             TO RADANT                                      
050000     PERFORM CA-LAS-OHUV                                                  
050100                                                                          
050200     IF SEGMENT-FINNS                                                     
050300        PERFORM CC-POS-OBKR                                               
050400        PERFORM CD-LAS-OBKR                                               
050500                                                                          
050600        PERFORM UNTIL SEGMENT-SAKNAS OR END-OF-DATA                       
050700                                                                          
050800           IF RADANT > RADANT-MAX                                         
050900              PERFORM S01-NYSIDA                                          
051000           END-IF                                                         
051100                                                                          
051200           PERFORM CE-REDIG-SKRIV-RAD                                     
051300           PERFORM CD-LAS-OBKR                                            
051400        END-PERFORM                                                       
051500     END-IF                                                               
051600     .                                                                    
051700     EJECT                                                                
051800 CA-LAS-OHUV        SECTION.                                              
051900                                                                          
052000     IF MID-PROFORMA = JA                                                 
052100        MOVE MID-IDDISTR-UT        TO W-IDDISTR                           
052200        MOVE MID-IDKUNDNR-UT       TO W-IDKUNDNR                          
052300        MOVE MID-IDORDNR7(RADANT-MID) TO W-IDORDNR7                       
052400        INSPECT W-IDORDNR7 REPLACING LEADING SPACE BY ZERO                
052500        PERFORM IMS-GU-WDE8                                               
052600     ELSE                                                                 
052700        MOVE MID-IDORDER (RADANT-MID) TO W-IDORDER                        
052800        PERFORM IMS-GU-WDQ2                                               
052900        IF SEGMENT-FINNS                                                  
053000           MOVE MID-IDDC(RADANT-MID) TO W-IDDC-WDQ212                     
053100           PERFORM IMS-GNP-WDQ212                                         
053200           IF SEGMENT-FINNS                                               
053300              MOVE ARB-DATRPAVD TO W-DATRPAVD                             
053400           ELSE                                                           
053500              MOVE ZERO         TO W-DATRPAVD                             
053600              MOVE '  '         TO STATUS-WS                              
053700           END-IF                                                         
053800        END-IF                                                            
053900     END-IF                                                               
054000     .                                                                    
054100     EJECT                                                                
054200                                                                          
054300 CC-POS-OBKR        SECTION.                                              
054400                                                                          
054500     MOVE LOW-VALUE                TO W-OBKR-NYCKEL-MIN-X                 
054600                                                                          
054700     IF MID-PROFORMA = NEJ                                                
054800        PERFORM IMS-GU-WDQ1                                               
054900     ELSE                                                                 
055000        PERFORM IMS-GU-WDQ1-PROF                                          
055100     END-IF                                                               
055200                                                                          
055300     .                                                                    
055400     EJECT                                                                
055500                                                                          
055600 CD-LAS-OBKR        SECTION.                                              
055700                                                                          
055800     MOVE LOW-VALUE                TO W-OBKR-NYCKEL-MIN-X                 
055900     MOVE MID-IDORDER (RADANT-MID) TO W-OBKR-IDORDER-MIN                  
056000                                                                          
056100     MOVE HIGH-VALUE               TO W-OBKR-NYCKEL-MAX-X                 
056200     MOVE MID-IDORDER (RADANT-MID) TO W-OBKR-IDORDER-MAX                  
056300                                                                          
056400     IF MID-PROFORMA = NEJ                                                
056500        PERFORM IMS-GN-WDQ1                                               
056600     ELSE                                                                 
056700        PERFORM IMS-GN-WDQ1-PROF                                          
056800     END-IF                                                               
056900                                                                          
057000     .                                                                    
057100     EJECT                                                                
057200 CE-REDIG-SKRIV-RAD SECTION.                                              
057300                                                                          
057400     MOVE SPACE                    TO RAD                                 
057500     MOVE '-'                      TO RAD-STRECK                          
057600     MOVE OBKR-KDORDBEK            TO RAD-KDORDBEK                        
057700     MOVE OBKR-TIREGDAT            TO RAD-TIREGDAT                        
057800     IF MID-PROFORMA = JA                                                 
057900        MOVE PHUV-IDSKYLT          TO W-IDSKYLT-X                         
058000     ELSE                                                                 
058100        MOVE OHUV-IDSKYLT          TO W-IDSKYLT-X                         
058200     END-IF                                                               
058300                                                                          
058400     PERFORM CEA-REDIGERA-PER-OBKR-KOD                                    
058500                                                                          
058600     MOVE PRT-AFTER-1  TO PRT-RADSKIP                                     
058700     MOVE RAD          TO WS-PRT-RAD                                      
058800     PERFORM S02-SKRIV-RAD                                                
058900                                                                          
059000     ADD  +1                       TO RADANT                              
059100     .                                                                    
059200     EJECT                                                                
059300                                                                          
059400 CEA-REDIGERA-PER-OBKR-KOD SECTION.                                       
059500                                                                          
059600     EVALUATE OBKR-KDORDBEK                                               
059700     WHEN 10                                                              
059800        PERFORM CEAA-REDIGERA-KOD-10                                      
059900     WHEN 15 THRU 16                                                      
060000        PERFORM CEAB-REDIGERA-KOD-15-TILL-16                              
060100     WHEN 30 THRU 34                                                      
060200        PERFORM CEAL-REDIGERA-KOD-80--83-85-87                            
060300     WHEN 40                                                              
060400        PERFORM CEAC-REDIGERA-KOD-40                                      
060500     WHEN 41 THRU 42                                                      
060600        PERFORM CEAD-REDIGERA-KOD-41-TILL-42                              
060700     WHEN 43 THRU 44                                                      
060800        PERFORM CEAE-REDIGERA-KOD-43-TILL-44                              
060900     WHEN 51 THRU 55                                                      
061000        PERFORM CEAF-REDIGERA-KOD-52-55-66-67                             
061100     WHEN 66 THRU 67                                                      
061200        PERFORM CEAF-REDIGERA-KOD-52-55-66-67                             
061300     WHEN 57                                                              
061400        PERFORM CEAG-REDIGERA-KOD-57                                      
061500     WHEN 58 THRU 59                                                      
061600        PERFORM CEAH-REDIGERA-KOD-58-TILL-59                              
061700     WHEN 61                                                              
061800        PERFORM CEAI-REDIGERA-KOD-61-65                                   
061900     WHEN 65                                                              
062000        PERFORM CEAI-REDIGERA-KOD-61-65                                   
062100     WHEN 70 THRU 71                                                      
062200        PERFORM CEAJ-REDIGERA-KOD-70-TILL-71                              
062300     WHEN 72 THRU 76                                                      
062400        PERFORM CEAK-REDIGERA-KOD-72-TILL-76                              
062500     WHEN 77                                                              
062600        PERFORM CEAJ-REDIGERA-KOD-70-TILL-71                              
062700     WHEN 80 THRU 83                                                      
062800        PERFORM CEAL-REDIGERA-KOD-80--83-85-87                            
062900     WHEN 85                                                              
063000        PERFORM CEAL-REDIGERA-KOD-80--83-85-87                            
063100     WHEN 87                                                              
063200        PERFORM CEAL-REDIGERA-KOD-80--83-85-87                            
063300     WHEN 90 THRU 93                                                      
063400        PERFORM CEAM-REDIGERA-KOD-90-TILL-93                              
063500     WHEN 95                                                              
063600        PERFORM CEAN-REDIGERA-KOD-95                                      
063700     WHEN 98                                                              
063800*       PERFORM CEAO-REDIGERA-KOD-98                                      
063900        PERFORM CEAM-REDIGERA-KOD-90-TILL-93                              
064000     WHEN 99                                                              
064100        PERFORM CEAP-REDIGERA-KOD-99                                      
064200     END-EVALUATE                                                         
064300     .                                                                    
064400     EJECT                                                                
064500                                                                          
064600 CEAA-REDIGERA-KOD-10 SECTION.                                            
064700                                                                          
064800     MOVE OBKR-IDARTNR         TO RAD-IDARTNR                             
064900     MOVE OBKR-REKSIFFR        TO RAD-REKSIFFR                            
065000     MOVE OBKR-IDARTNR         TO W-IDARTNR                               
065100     PERFORM S03-HAMTA-BENAMNING                                          
065200     MOVE OBKR-KVBEART-Q       TO RAD-KVBEART                             
065300     MOVE OBKR-IDDC            TO RAD-IDDC                                
065400     IF OBKR-IDKUNDRF-RO = SPACE OR '0000000   '                          
065500        MOVE OBKR-IDKUNDRF         TO WS-IDKUNDRF                         
065600     ELSE                                                                 
065700        MOVE OBKR-IDKUNDRF-RO      TO WS-IDKUNDRF                         
065800     END-IF                                                               
065900     IF WS-IDKUNDRF-6--10 = SPACE                                         
066000        MOVE WS-IDORDNR5       TO RAD-IDORDNR                             
066100     ELSE                                                                 
066200        MOVE WS-IDORDNR7       TO RAD-IDORDNR                             
066300     END-IF                                                               
066400     MOVE OBKR-TITPO           TO RAD-TITPO                               
066500     .                                                                    
066600     EJECT                                                                
066700                                                                          
066800 CEAB-REDIGERA-KOD-15-TILL-16 SECTION.                                    
066900                                                                          
067000     IF OBKR-IDARTNR-TILLK > ZERO                                         
067100        MOVE OBKR-IDARTNR-TILLK    TO RAD-IDARTNR                         
067200        MOVE OBKR-REKSIFFR-TILLK   TO RAD-REKSIFFR                        
067300        MOVE OBKR-IDARTNR-TILLK    TO W-IDARTNR                           
067400     ELSE                                                                 
067500        MOVE OBKR-IDARTNR          TO RAD-IDARTNR                         
067600        MOVE OBKR-REKSIFFR         TO RAD-REKSIFFR                        
067700        MOVE OBKR-IDARTNR          TO W-IDARTNR                           
067800     END-IF                                                               
067900     PERFORM S03-HAMTA-BENAMNING                                          
068000     MOVE OBKR-KVBEART-Q           TO RAD-KVBEART                         
068100     MOVE OBKR-IDDC                TO RAD-IDDC                            
068200     IF OBKR-IDKUNDRF-RO = SPACE OR '0000000   '                          
068300        MOVE OBKR-IDKUNDRF         TO WS-IDKUNDRF                         
068400     ELSE                                                                 
068500        MOVE OBKR-IDKUNDRF-RO      TO WS-IDKUNDRF                         
068600     END-IF                                                               
068700     IF WS-IDKUNDRF-6--10 = SPACE                                         
068800        MOVE WS-IDORDNR5           TO RAD-IDORDNR                         
068900     ELSE                                                                 
069000        MOVE WS-IDORDNR7           TO RAD-IDORDNR                         
069100     END-IF                                                               
069200     .                                                                    
069300     EJECT                                                                
069400                                                                          
069500 CEAC-REDIGERA-KOD-40 SECTION.                                            
069600                                                                          
069700     MOVE '*'                      TO RAD-ASTERISK                        
069800     IF OBKR-IDARTNR-TILLK > ZERO                                         
069900        MOVE ZERO                  TO RAD-KDORDBEK                        
070000        MOVE OBKR-IDARTNR-TILLK    TO RAD-IDARTNR                         
070100        MOVE OBKR-REKSIFFR-TILLK   TO RAD-REKSIFFR                        
070200        MOVE OBKR-IDARTNR-TILLK    TO W-IDARTNR                           
070300     ELSE                                                                 
070400        MOVE OBKR-IDARTNR          TO RAD-IDARTNR                         
070500        MOVE OBKR-REKSIFFR         TO RAD-REKSIFFR                        
070600        MOVE OBKR-IDARTNR          TO W-IDARTNR                           
070700     END-IF                                                               
070800     PERFORM S03-HAMTA-BENAMNING                                          
070900     MOVE OBKR-KVBEART-TILLK       TO RAD-KVBEART                         
071000     MOVE OBKR-IDDC                TO RAD-IDDC                            
071100     IF OBKR-IDKUNDRF-RO = SPACE OR '0000000   '                          
071200        MOVE OBKR-IDKUNDRF         TO WS-IDKUNDRF                         
071300     ELSE                                                                 
071400        MOVE OBKR-IDKUNDRF-RO      TO WS-IDKUNDRF                         
071500     END-IF                                                               
071600     IF WS-IDKUNDRF-6--10 = SPACE                                         
071700        MOVE WS-IDORDNR5           TO RAD-IDORDNR                         
071800     ELSE                                                                 
071900        MOVE WS-IDORDNR7           TO RAD-IDORDNR                         
072000     END-IF                                                               
072100     .                                                                    
072200     EJECT                                                                
072300                                                                          
072400 CEAD-REDIGERA-KOD-41-TILL-42 SECTION.                                    
072500                                                                          
072600     IF OBKR-IDARTNR-TILLK = +0                                           
072700        IF RADANT + 1 > RADANT-MAX                                        
072800           PERFORM S01-NYSIDA                                             
072900        END-IF                                                            
073000                                                                          
073100        PERFORM CEADA-RED-ERSATT-ARTIKEL                                  
073200     ELSE                                                                 
073300        PERFORM CEADB-RED-ERSETTANDE-ARTIKEL                              
073400     END-IF                                                               
073500     .                                                                    
073600     EJECT                                                                
073700                                                                          
073800 CEADA-RED-ERSATT-ARTIKEL SECTION.                                        
073900                                                                          
074000     MOVE '*'                      TO RAD-ASTERISK                        
074100     MOVE OBKR-IDARTNR             TO RAD-IDARTNR                         
074200     MOVE OBKR-REKSIFFR            TO RAD-REKSIFFR                        
074300     MOVE OBKR-IDARTNR             TO W-IDARTNR                           
074400     PERFORM S03-HAMTA-BENAMNING                                          
074500     MOVE OBKR-KVBEART             TO RAD-KVBEART                         
074600     MOVE OBKR-IDDC                TO RAD-IDDC                            
074700     IF OBKR-IDKUNDRF-RO = SPACE OR '0000000   '                          
074800        MOVE OBKR-IDKUNDRF         TO WS-IDKUNDRF                         
074900     ELSE                                                                 
075000        MOVE OBKR-IDKUNDRF-RO      TO WS-IDKUNDRF                         
075100     END-IF                                                               
075200     IF WS-IDKUNDRF-6--10 = SPACE                                         
075300        MOVE WS-IDORDNR5           TO RAD-IDORDNR                         
075400     ELSE                                                                 
075500        MOVE WS-IDORDNR7           TO RAD-IDORDNR                         
075600     END-IF                                                               
075700     .                                                                    
075800     EJECT                                                                
075900                                                                          
076000 CEADB-RED-ERSETTANDE-ARTIKEL SECTION.                                    
076100                                                                          
076200     MOVE ZERO                     TO RAD-KDORDBEK                        
076300     MOVE '*'                      TO RAD-ASTERISK                        
076400     MOVE ZERO                     TO RAD-TIREGDAT                        
076500     MOVE OBKR-IDARTNR-TILLK       TO RAD-IDARTNR                         
076600     MOVE OBKR-REKSIFFR-TILLK      TO RAD-REKSIFFR                        
076700     MOVE OBKR-IDARTNR-TILLK       TO W-IDARTNR                           
076800     PERFORM S03-HAMTA-BENAMNING                                          
076900     MOVE OBKR-KVBEART-TILLK       TO RAD-KVBEART                         
077000     MOVE OBKR-DIERS-KVOT          TO RAD-KVQPACK-1                       
077100     .                                                                    
077200     EJECT                                                                
077300                                                                          
077400 CEAE-REDIGERA-KOD-43-TILL-44 SECTION.                                    
077500                                                                          
077600     IF OBKR-IDARTNR-TILLK > ZERO                                         
077700        MOVE OBKR-IDARTNR-TILLK    TO RAD-IDARTNR                         
077800        MOVE OBKR-REKSIFFR-TILLK   TO RAD-REKSIFFR                        
077900        MOVE OBKR-IDARTNR-TILLK    TO W-IDARTNR                           
078000     ELSE                                                                 
078100        MOVE OBKR-IDARTNR          TO RAD-IDARTNR                         
078200        MOVE OBKR-REKSIFFR         TO RAD-REKSIFFR                        
078300        MOVE OBKR-IDARTNR          TO W-IDARTNR                           
078400     END-IF                                                               
078500     PERFORM S03-HAMTA-BENAMNING                                          
078600     MOVE OBKR-KVBEART-Q           TO RAD-KVBEART                         
078700     MOVE OBKR-IDDC                TO RAD-IDDC                            
078800     MOVE OBKR-KVQPACK             TO RAD-KVQPACK-1                       
078900     IF OBKR-IDKUNDRF-RO = SPACE OR '0000000   '                          
079000        MOVE OBKR-IDKUNDRF         TO WS-IDKUNDRF                         
079100     ELSE                                                                 
079200        MOVE OBKR-IDKUNDRF-RO      TO WS-IDKUNDRF                         
079300     END-IF                                                               
079400     IF WS-IDKUNDRF-6--10 = SPACE                                         
079500        MOVE WS-IDORDNR5           TO RAD-IDORDNR                         
079600     ELSE                                                                 
079700        MOVE WS-IDORDNR7           TO RAD-IDORDNR                         
079800     END-IF                                                               
079900     .                                                                    
080000     EJECT                                                                
080100                                                                          
080200 CEAF-REDIGERA-KOD-52-55-66-67 SECTION.                                   
080300                                                                          
080400     IF OBKR-IDARTNR-TILLK > ZERO                                         
080500        MOVE OBKR-IDARTNR-TILLK    TO RAD-IDARTNR                         
080600        MOVE OBKR-REKSIFFR-TILLK   TO RAD-REKSIFFR                        
080700        MOVE OBKR-IDARTNR-TILLK    TO W-IDARTNR                           
080800     ELSE                                                                 
080900        MOVE OBKR-IDARTNR          TO RAD-IDARTNR                         
081000        MOVE OBKR-REKSIFFR         TO RAD-REKSIFFR                        
081100        MOVE OBKR-IDARTNR          TO W-IDARTNR                           
081200     END-IF                                                               
081300     PERFORM S03-HAMTA-BENAMNING                                          
081400     MOVE OBKR-KVBEART             TO RAD-KVBEART                         
081500     MOVE OBKR-IDDC                TO RAD-IDDC                            
081600     IF OBKR-IDKUNDRF-RO = SPACE OR '0000000   '                          
081700        MOVE OBKR-IDKUNDRF         TO WS-IDKUNDRF                         
081800     ELSE                                                                 
081900        MOVE OBKR-IDKUNDRF-RO      TO WS-IDKUNDRF                         
082000     END-IF                                                               
082100     IF WS-IDKUNDRF-6--10 = SPACE                                         
082200        MOVE WS-IDORDNR5           TO RAD-IDORDNR                         
082300     ELSE                                                                 
082400        MOVE WS-IDORDNR7           TO RAD-IDORDNR                         
082500     END-IF                                                               
082600     .                                                                    
082700     EJECT                                                                
082800                                                                          
082900 CEAG-REDIGERA-KOD-57 SECTION.                                            
083000                                                                          
083100     PERFORM CEAGA-HAMTA-KOLLA-SATSARTIKLAR                               
083200                                                                          
083300     IF OBKR-IDARTNR-TILLK > ZERO                                         
083400        MOVE OBKR-IDARTNR-TILLK    TO RAD-IDARTNR                         
083500        MOVE OBKR-REKSIFFR-TILLK   TO RAD-REKSIFFR                        
083600        MOVE OBKR-IDARTNR-TILLK    TO W-IDARTNR                           
083700     ELSE                                                                 
083800        MOVE OBKR-IDARTNR          TO RAD-IDARTNR                         
083900        MOVE OBKR-REKSIFFR         TO RAD-REKSIFFR                        
084000        MOVE OBKR-IDARTNR          TO W-IDARTNR                           
084100     END-IF                                                               
084200     PERFORM S03-HAMTA-BENAMNING                                          
084300     MOVE OBKR-KVBEART             TO RAD-KVBEART                         
084400     MOVE OBKR-IDDC                TO RAD-IDDC                            
084500     MOVE OBKR-IDKUNDRF            TO WS-IDKUNDRF                         
084600     IF WS-IDKUNDRF-6--10 = SPACE                                         
084700        MOVE WS-IDORDNR5           TO RAD-IDORDNR                         
084800     ELSE                                                                 
084900        MOVE WS-IDORDNR7           TO RAD-IDORDNR                         
085000     END-IF                                                               
085100                                                                          
085200     IF WS-INDX-SATS > +1                                                 
085300        MOVE PRT-AFTER-1           TO PRT-RADSKIP                         
085400        MOVE RAD                   TO WS-PRT-RAD                          
085500        PERFORM S02-SKRIV-RAD                                             
085600        ADD 1 TO RADANT                                                   
085700                                                                          
085800        MOVE SPACE                 TO RAD                                 
085900        MOVE +1                    TO WS-INDX-SATS                        
086000        MOVE WS-IDARTNR-SATS(WS-INDX-SATS)                                
086100                                   TO RAD-BEART                           
086200        INSPECT RAD-BEART REPLACING LEADING ZERO BY SPACE                 
086300        ADD +1 TO RADANT                                                  
086400        ADD +1 TO WS-INDX-SATS                                            
086500                                                                          
086600*    DETTA LILLA INITIERINGSTRIXET ÄR GJORD EFTERSOM LÄSNINGEN            
086700*    AV INFILEN TRASSLAR TILL DET LITE OM MAN SKALL BEHÅLLA               
086800*    SKRIVNINGEN AV LISTAN I CD-REDIGERA-SKRIV-RAD.                       
086900*    I ANAT FALL SKULLE LISTRADERNA BEHÖVA SKRIVAS I VARJE                
087000*    SECTION. TILLSVIDARE FÖRSEKER VI LÅTA BLI DET.                       
087100*    SISTA SATSARTIKELN KOMMER ALLTSÅ ATT SKRIVAS I CD-..                 
087200                                                                          
087300        PERFORM UNTIL WS-INDX-SATS > WS-INDX-SATS-MAX OR                  
087400                      WS-IDARTNR-SATS(WS-INDX-SATS) = +0                  
087500           PERFORM S02-SKRIV-RAD                                          
087600           MOVE WS-IDARTNR-SATS(WS-INDX-SATS)                             
087700                                      TO RAD-BEART                        
087800           INSPECT RAD-BEART REPLACING LEADING ZERO BY SPACE              
087900           ADD +1 TO RADANT                                               
088000           ADD +1 TO WS-INDX-SATS                                         
088100        END-PERFORM                                                       
088200     END-IF                                                               
088300     .                                                                    
088400     EJECT                                                                
088500                                                                          
088600 CEAGA-HAMTA-KOLLA-SATSARTIKLAR SECTION.                                  
088700                                                                          
088800     MOVE +1 TO WS-INDX-SATS                                              
088900     PERFORM UNTIL WS-INDX-SATS > WS-INDX-SATS-MAX                        
089000        MOVE +0                TO WS-IDARTNR-SATS(WS-INDX-SATS)           
089100        ADD +1                 TO WS-INDX-SATS                            
089200     END-PERFORM                                                          
089300                                                                          
089400     MOVE +1 TO WS-INDX-SATS                                              
089500     IF OBKR-IDARTNR-TILLK > ZERO                                         
089600        MOVE OBKR-IDARTNR-TILLK   TO W-IDARTNR-J1                         
089700     ELSE                                                                 
089800        MOVE OBKR-IDARTNR         TO W-IDARTNR-J1                         
089900     END-IF                                                               
090000                                                                          
090100     PERFORM IMS-GU-SATB-WDJ111-01                                        
090200     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
090300                   WS-INDX-SATS > WS-INDX-SATS-MAX                        
090400        MOVE SATB-RAD-TISTODAT   TO TMP1-YYMMDD                           
090500        MOVE DAGENS-DATUM        TO TMP2-YYMMDD                           
090600        MOVE SATB-RAD-TISTADAT   TO TMP3-YYMMDD                           
090700        PERFORM WY2000Q1                                                  
090800        IF SATB-STR-IDARTNR < 100000000 AND                               
090900           SATB-STR-TIBORT = 0 AND                                        
091000           TMP3-YYMMDD <= TMP2-YYMMDD   AND                               
091100           TMP1-YYMMDD >= TMP2-YYMMDD                                     
091200                                                                          
091300           MOVE SATB-STR-IDARTNR                                          
091400                               TO WS-IDARTNR-SATS(WS-INDX-SATS)           
091500           ADD +1              TO WS-INDX-SATS                            
091600        END-IF                                                            
091700        PERFORM IMS-GN-SATB-WDJ111-01                                     
091800     END-PERFORM                                                          
091900                                                                          
092000     IF RADANT + WS-INDX-SATS > RADANT-MAX                                
092100        PERFORM S01-NYSIDA                                                
092200     END-IF                                                               
092300     .                                                                    
092400     EJECT                                                                
092500                                                                          
092600 CEAH-REDIGERA-KOD-58-TILL-59 SECTION.                                    
092700                                                                          
092800     IF OBKR-IDARTNR-TILLK > ZERO                                         
092900        MOVE OBKR-IDARTNR-TILLK    TO RAD-IDARTNR                         
093000        MOVE OBKR-REKSIFFR-TILLK   TO RAD-REKSIFFR                        
093100        MOVE OBKR-IDARTNR-TILLK    TO W-IDARTNR                           
093200     ELSE                                                                 
093300        MOVE OBKR-IDARTNR          TO RAD-IDARTNR                         
093400        MOVE OBKR-REKSIFFR         TO RAD-REKSIFFR                        
093500        MOVE OBKR-IDARTNR          TO W-IDARTNR                           
093600     END-IF                                                               
093700     PERFORM S03-HAMTA-BENAMNING                                          
093800     MOVE OBKR-KVBEART             TO RAD-KVBEART                         
093900     MOVE OBKR-IDDC                TO RAD-IDDC                            
094000     MOVE OBKR-IDKUNDRF            TO WS-IDKUNDRF                         
094100     IF WS-IDKUNDRF-6--10 = SPACE                                         
094200        MOVE WS-IDORDNR5           TO RAD-IDORDNR                         
094300     ELSE                                                                 
094400        MOVE WS-IDORDNR7           TO RAD-IDORDNR                         
094500     END-IF                                                               
094600     .                                                                    
094700     EJECT                                                                
094800                                                                          
094900 CEAI-REDIGERA-KOD-61-65 SECTION.                                         
095000                                                                          
095100     IF OBKR-IDARTNR-TILLK = +0 AND                                       
095200        OBKR-BEERS = SPACE                                                
095300        IF RADANT + 1 > RADANT-MAX                                        
095400           PERFORM S01-NYSIDA                                             
095500        END-IF                                                            
095600                                                                          
095700        PERFORM CEAIA-RED-ERSATT-ARTIKEL                                  
095800     ELSE                                                                 
095900        PERFORM CEAIB-RED-ERSETTANDE-ARTIKEL                              
096000     END-IF                                                               
096100     .                                                                    
096200     EJECT                                                                
096300                                                                          
096400 CEAIA-RED-ERSATT-ARTIKEL SECTION.                                        
096500                                                                          
096600     MOVE '*'                      TO RAD-ASTERISK                        
096700     MOVE OBKR-IDARTNR             TO RAD-IDARTNR                         
096800     MOVE OBKR-REKSIFFR            TO RAD-REKSIFFR                        
096900     MOVE OBKR-IDARTNR             TO W-IDARTNR                           
097000     PERFORM S03-HAMTA-BENAMNING                                          
097100     MOVE OBKR-KVBEART             TO RAD-KVBEART                         
097200     MOVE OBKR-IDDC                TO RAD-IDDC                            
097300     IF OBKR-IDKUNDRF-RO = SPACE OR '0000000   '                          
097400        MOVE OBKR-IDKUNDRF         TO WS-IDKUNDRF                         
097500     ELSE                                                                 
097600        MOVE OBKR-IDKUNDRF-RO      TO WS-IDKUNDRF                         
097700     END-IF                                                               
097800     IF WS-IDKUNDRF-6--10 = SPACE                                         
097900        MOVE WS-IDORDNR5           TO RAD-IDORDNR                         
098000     ELSE                                                                 
098100        MOVE WS-IDORDNR7           TO RAD-IDORDNR                         
098200     END-IF                                                               
098300     .                                                                    
098400     EJECT                                                                
098500                                                                          
098600 CEAIB-RED-ERSETTANDE-ARTIKEL SECTION.                                    
098700                                                                          
098800     MOVE ZERO                     TO RAD-KDORDBEK                        
098900     MOVE ZERO                     TO RAD-TIREGDAT                        
099000     IF OBKR-BEERS NOT = SPACE                                            
099100        MOVE OBKR-BEERS            TO RAD-BEART                           
099200        MOVE SPACE                 TO RAD-STRECK                          
099300     ELSE                                                                 
099400        MOVE '*'                   TO RAD-ASTERISK                        
099500        MOVE OBKR-IDARTNR-TILLK    TO RAD-IDARTNR                         
099600        MOVE OBKR-REKSIFFR-TILLK   TO RAD-REKSIFFR                        
099700        MOVE OBKR-IDARTNR-TILLK    TO W-IDARTNR                           
099800        PERFORM S03-HAMTA-BENAMNING                                       
099900        MOVE OBKR-KVBEART-TILLK    TO RAD-KVBEART                         
100000        MOVE OBKR-DIERS-KVOT       TO RAD-KVQPACK-1                       
100100     END-IF                                                               
100200     .                                                                    
100300     EJECT                                                                
100400                                                                          
100500 CEAJ-REDIGERA-KOD-70-TILL-71 SECTION.                                    
100600                                                                          
100700     IF OBKR-IDARTNR-TILLK > ZERO                                         
100800        MOVE OBKR-IDARTNR-TILLK    TO RAD-IDARTNR                         
100900        MOVE OBKR-REKSIFFR-TILLK   TO RAD-REKSIFFR                        
101000        MOVE OBKR-IDARTNR-TILLK    TO W-IDARTNR                           
101100     ELSE                                                                 
101200        MOVE OBKR-IDARTNR          TO RAD-IDARTNR                         
101300        MOVE OBKR-REKSIFFR         TO RAD-REKSIFFR                        
101400        MOVE OBKR-IDARTNR          TO W-IDARTNR                           
101500     END-IF                                                               
101600     PERFORM S03-HAMTA-BENAMNING                                          
101700     MOVE OBKR-KVBEART-Q           TO RAD-KVBEART                         
101800     MOVE OBKR-IDDC                TO RAD-IDDC                            
101900     IF OBKR-IDKUNDRF-RO = SPACE OR '0000000   '                          
102000        MOVE OBKR-IDKUNDRF         TO WS-IDKUNDRF                         
102100     ELSE                                                                 
102200        MOVE OBKR-IDKUNDRF-RO      TO WS-IDKUNDRF                         
102300     END-IF                                                               
102400     IF WS-IDKUNDRF-6--10 = SPACE                                         
102500        MOVE WS-IDORDNR5           TO RAD-IDORDNR                         
102600     ELSE                                                                 
102700        MOVE WS-IDORDNR7           TO RAD-IDORDNR                         
102800     END-IF                                                               
102900     MOVE OBKR-TITPO               TO RAD-TITPO                           
103000     .                                                                    
103100     EJECT                                                                
103200                                                                          
103300 CEAK-REDIGERA-KOD-72-TILL-76 SECTION.                                    
103400                                                                          
103500     IF OBKR-IDARTNR-TILLK > ZERO                                         
103600        MOVE OBKR-IDARTNR-TILLK    TO RAD-IDARTNR                         
103700        MOVE OBKR-REKSIFFR-TILLK   TO RAD-REKSIFFR                        
103800        MOVE OBKR-IDARTNR-TILLK    TO W-IDARTNR                           
103900     ELSE                                                                 
104000        MOVE OBKR-IDARTNR          TO RAD-IDARTNR                         
104100        MOVE OBKR-REKSIFFR         TO RAD-REKSIFFR                        
104200        MOVE OBKR-IDARTNR          TO W-IDARTNR                           
104300     END-IF                                                               
104400     PERFORM S03-HAMTA-BENAMNING                                          
104500     IF OBKR-KDORDBEK = 74                                                
104600        MOVE OBKR-KVBEART-Q        TO RAD-KVBEART                         
104700     ELSE                                                                 
104800        MOVE OBKR-KVBEART          TO RAD-KVBEART                         
104900     END-IF                                                               
105000     MOVE OBKR-IDDC                TO RAD-IDDC                            
105100     IF OBKR-IDKUNDRF-RO = SPACE OR '0000000   '                          
105200        MOVE OBKR-IDKUNDRF         TO WS-IDKUNDRF                         
105300     ELSE                                                                 
105400        MOVE OBKR-IDKUNDRF-RO      TO WS-IDKUNDRF                         
105500     END-IF                                                               
105600     IF WS-IDKUNDRF-6--10 = SPACE                                         
105700        MOVE WS-IDORDNR5           TO RAD-IDORDNR                         
105800     ELSE                                                                 
105900        MOVE WS-IDORDNR7           TO RAD-IDORDNR                         
106000     END-IF                                                               
106100     IF OBKR-KDORDBEK = 75 OR 76                                          
106200        IF OBKR-KDTPOTYP NOT = ZERO                                       
106300           MOVE OBKR-TITPO         TO RAD-TITPO                           
106400        END-IF                                                            
106500     END-IF                                                               
106600     .                                                                    
106700     EJECT                                                                
106800                                                                          
106900 CEAL-REDIGERA-KOD-80--83-85-87 SECTION.                                  
107000                                                                          
107100     IF OBKR-IDARTNR-TILLK > ZERO                                         
107200        MOVE OBKR-IDARTNR-TILLK    TO RAD-IDARTNR                         
107300        MOVE OBKR-REKSIFFR-TILLK   TO RAD-REKSIFFR                        
107400        MOVE OBKR-IDARTNR-TILLK    TO W-IDARTNR                           
107500     ELSE                                                                 
107600        MOVE OBKR-IDARTNR          TO RAD-IDARTNR                         
107700        MOVE OBKR-REKSIFFR         TO RAD-REKSIFFR                        
107800        MOVE OBKR-IDARTNR          TO W-IDARTNR                           
107900     END-IF                                                               
108000     PERFORM S03-HAMTA-BENAMNING                                          
108100     MOVE OBKR-IDDC                TO RAD-IDDC                            
108200     IF OBKR-KDORDBEK = 82                                                
108300        MOVE OBKR-KVBEART          TO RAD-KVBEART                         
108400     ELSE                                                                 
108500        MOVE OBKR-KVANNANT         TO RAD-KVBEART                         
108600     END-IF                                                               
108700     IF OBKR-IDKUNDRF-RO = SPACE OR '0000000   '                          
108800        MOVE OBKR-IDKUNDRF         TO WS-IDKUNDRF                         
108900     ELSE                                                                 
109000        MOVE OBKR-IDKUNDRF-RO      TO WS-IDKUNDRF                         
109100     END-IF                                                               
109200     IF WS-IDKUNDRF-6--10 = SPACE                                         
109300        MOVE WS-IDORDNR5           TO RAD-IDORDNR                         
109400     ELSE                                                                 
109500        MOVE WS-IDORDNR7           TO RAD-IDORDNR                         
109600     END-IF                                                               
109700     .                                                                    
109800     EJECT                                                                
109900                                                                          
110000 CEAM-REDIGERA-KOD-90-TILL-93 SECTION.                                    
110100                                                                          
110200     IF OBKR-IDARTNR-TILLK > ZERO                                         
110300        MOVE OBKR-IDARTNR-TILLK    TO RAD-IDARTNR                         
110400        MOVE OBKR-REKSIFFR-TILLK   TO RAD-REKSIFFR                        
110500        MOVE OBKR-IDARTNR-TILLK    TO W-IDARTNR                           
110600     ELSE                                                                 
110700        MOVE OBKR-IDARTNR          TO RAD-IDARTNR                         
110800        MOVE OBKR-REKSIFFR         TO RAD-REKSIFFR                        
110900        MOVE OBKR-IDARTNR          TO W-IDARTNR                           
111000     END-IF                                                               
111100     MOVE OBKR-IDDC                TO RAD-IDDC                            
111200     PERFORM S03-HAMTA-BENAMNING                                          
111300     IF OBKR-KDORDBEK = 92                                                
111400        MOVE OBKR-KVPRERO          TO RAD-KVBEART                         
111500     ELSE                                                                 
111600        IF OBKR-KDORDBEK = 93                                             
111700           MOVE OBKR-KVANNANT      TO RAD-KVBEART                         
111800        ELSE                                                              
111900           MOVE OBKR-KVRO          TO RAD-KVBEART                         
112000        END-IF                                                            
112100     END-IF                                                               
112200     IF OBKR-IDKUNDRF-RO = SPACE OR '0000000   '                          
112300        MOVE OBKR-IDKUNDRF         TO WS-IDKUNDRF                         
112400     ELSE                                                                 
112500        MOVE OBKR-IDKUNDRF-RO      TO WS-IDKUNDRF                         
112600     END-IF                                                               
112700     IF WS-IDKUNDRF-6--10 = SPACE                                         
112800        MOVE WS-IDORDNR5           TO RAD-IDORDNR                         
112900     ELSE                                                                 
113000        MOVE WS-IDORDNR7           TO RAD-IDORDNR                         
113100     END-IF                                                               
113200     .                                                                    
113300     EJECT                                                                
113400                                                                          
113500 CEAN-REDIGERA-KOD-95 SECTION.                                            
113600                                                                          
113700     IF OBKR-IDARTNR-TILLK > ZERO                                         
113800        MOVE OBKR-IDARTNR-TILLK    TO RAD-IDARTNR                         
113900        MOVE OBKR-REKSIFFR-TILLK   TO RAD-REKSIFFR                        
114000        MOVE OBKR-IDARTNR-TILLK    TO W-IDARTNR                           
114100     ELSE                                                                 
114200        MOVE OBKR-IDARTNR          TO RAD-IDARTNR                         
114300        MOVE OBKR-REKSIFFR         TO RAD-REKSIFFR                        
114400        MOVE OBKR-IDARTNR          TO W-IDARTNR                           
114500     END-IF                                                               
114600     PERFORM S03-HAMTA-BENAMNING                                          
114700     MOVE OBKR-KVBEART-Q           TO RAD-KVBEART                         
114800     MOVE OBKR-IDDC                TO RAD-IDDC                            
114900     IF OBKR-IDKUNDRF-RO = SPACE OR '0000000   '                          
115000        MOVE OBKR-IDKUNDRF         TO WS-IDKUNDRF                         
115100     ELSE                                                                 
115200        MOVE OBKR-IDKUNDRF-RO      TO WS-IDKUNDRF                         
115300     END-IF                                                               
115400     IF WS-IDKUNDRF-6--10 = SPACE                                         
115500        MOVE WS-IDORDNR5           TO RAD-IDORDNR                         
115600     ELSE                                                                 
115700        MOVE WS-IDORDNR7           TO RAD-IDORDNR                         
115800     END-IF                                                               
115900     .                                                                    
116000     EJECT                                                                
116100                                                                          
116200 CEAO-REDIGERA-KOD-98 SECTION.                                            
116300                                                                          
116400     MOVE OBKR-IDARTNR             TO RAD-IDARTNR                         
116500     MOVE OBKR-REKSIFFR            TO RAD-REKSIFFR                        
116600     MOVE OBKR-IDARTNR             TO W-IDARTNR                           
116700     PERFORM S03-HAMTA-BENAMNING                                          
116800     MOVE OBKR-KVBEART             TO RAD-KVBEART                         
116900     MOVE OBKR-IDDC                TO RAD-IDDC                            
117000     IF OBKR-IDKUNDRF-RO = SPACE OR '0000000   '                          
117100        MOVE OBKR-IDKUNDRF         TO WS-IDKUNDRF                         
117200     ELSE                                                                 
117300        MOVE OBKR-IDKUNDRF-RO      TO WS-IDKUNDRF                         
117400     END-IF                                                               
117500     IF WS-IDKUNDRF-6--10 = SPACE                                         
117600        MOVE WS-IDORDNR5           TO RAD-IDORDNR                         
117700     ELSE                                                                 
117800        MOVE WS-IDORDNR7           TO RAD-IDORDNR                         
117900     END-IF                                                               
118000     .                                                                    
118100     EJECT                                                                
118200                                                                          
118300 CEAP-REDIGERA-KOD-99 SECTION.                                            
118400                                                                          
118500     IF OBKR-IDARTNR-TILLK > ZERO                                         
118600        MOVE OBKR-IDARTNR-TILLK    TO RAD-IDARTNR                         
118700        MOVE OBKR-REKSIFFR-TILLK   TO RAD-REKSIFFR                        
118800        MOVE OBKR-IDARTNR-TILLK    TO W-IDARTNR                           
118900     ELSE                                                                 
119000        MOVE OBKR-IDARTNR          TO RAD-IDARTNR                         
119100        MOVE OBKR-REKSIFFR         TO RAD-REKSIFFR                        
119200        MOVE OBKR-IDARTNR          TO W-IDARTNR                           
119300     END-IF                                                               
119400     MOVE OBKR-IDDC                TO RAD-IDDC                            
119500     PERFORM S03-HAMTA-BENAMNING                                          
119600     MOVE OBKR-KVPRERO             TO RAD-KVBEART                         
119700     MOVE OBKR-IDKUNDRF            TO WS-IDKUNDRF                         
119800     IF WS-IDKUNDRF-6--10 = SPACE                                         
119900        MOVE WS-IDORDNR5           TO RAD-IDORDNR                         
120000     ELSE                                                                 
120100        MOVE WS-IDORDNR7           TO RAD-IDORDNR                         
120200     END-IF                                                               
120300     .                                                                    
120400     EJECT                                                                
120500                                                                          
120600 Z-FINIT      SECTION.                                                    
120700                                                                          
120800     MOVE '************** LAST PAGE **************'                       
120900                           TO WS-PRT-RAD                                  
121000     MOVE PRT-AFTER-2      TO PRT-RADSKIP                                 
121100     PERFORM S02-SKRIV-RAD                                                
121200                                                                          
121300     CALL W006PRS1 USING PRT-SPOOL-OVR                                    
121400                         PRT-CLOSE                                        
121500                         WS-PRT-PRINTER                                   
121600                         ALT1-PCB                                         
121700                         WS-DUMMY                                         
121800                         WS-DUMMY                                         
121900                                                                          
122000     MOVE NEJ TO PRINTER-SW                                               
122100     .                                                                    
122200     EJECT                                                                
122300 S01-NYSIDA         SECTION.                                              
122400                                                                          
122500     ADD +1                        TO SIDANT                              
122600     MOVE SIDANT                   TO HRAD1-SIDNR                         
122700     MOVE PRT-NYSIDA-RAD1   TO PRT-RADSKIP                                
122800     MOVE HRAD-STRECK  TO WS-PRT-RAD                                      
122900     PERFORM S02-SKRIV-RAD                                                
123000                                                                          
123100     MOVE WS-PV-FTG-NAMN           TO HRAD1-FTG-NAMN                      
123200     IF MID-PROFORMA = JA                                                 
123300       MOVE 'PRO FORMA CONFIRMATIONS' TO HRAD1-RUBRIK                     
123400     ELSE                                                                 
123500       MOVE 'ORDERCONFIRMATIONS' TO HRAD1-RUBRIK                          
123600     END-IF                                                               
123700                                                                          
123800     MOVE CURRENT-YY   TO HRAD1-YY                                        
123900     MOVE CURRENT-MM   TO HRAD1-MM                                        
124000     MOVE CURRENT-DD   TO HRAD1-DD                                        
124100     MOVE PRT-AFTER-1  TO PRT-RADSKIP                                     
124200     MOVE HRAD1        TO WS-PRT-RAD                                      
124300     PERFORM S02-SKRIV-RAD                                                
124400                                                                          
124500     MOVE PRT-AFTER-1  TO PRT-RADSKIP                                     
124600     MOVE HRAD-STRECK  TO WS-PRT-RAD                                      
124700     PERFORM S02-SKRIV-RAD                                                
124800                                                                          
124900     MOVE PRT-AFTER-1  TO PRT-RADSKIP                                     
125000     MOVE HRAD2        TO WS-PRT-RAD                                      
125100     PERFORM S02-SKRIV-RAD                                                
125200                                                                          
125300     IF MID-PROFORMA = JA                                                 
125400        MOVE PHUV-BEGMT-RAD1        TO HRAD3-BEGMT-RAD1                   
125500     ELSE                                                                 
125600        MOVE OHUV-BEGMT-RAD1        TO HRAD3-BEGMT-RAD1                   
125700     END-IF                                                               
125800     MOVE MID-IDDISTR-UT            TO HRAD3-IDDISTR                      
125900     MOVE MID-IDKUNDNR (RADANT-MID) TO HRAD3-IDKUNDNR                     
126000*    MOVE MID-IDDC     (RADANT-MID) TO HRAD3-IDDC                         
126100     MOVE OBKR-IDORDNR7             TO HRAD3-IDORDNR7                     
126200     MOVE OBKR-BEKUNDRF             TO HRAD3-BEKUNDRF                     
126300     MOVE MID-KDORDKL  (RADANT-MID) TO HRAD3-KDORDKL                      
126400     MOVE PRT-AFTER-1  TO PRT-RADSKIP                                     
126500     MOVE HRAD3        TO WS-PRT-RAD                                      
126600     PERFORM S02-SKRIV-RAD                                                
126700                                                                          
126800     IF MID-PROFORMA = JA                                                 
126900        MOVE PHUV-BEGMT-RAD2        TO HRAD4-BEGMT-RAD2                   
127000     ELSE                                                                 
127100        MOVE OHUV-BEGMT-RAD2        TO HRAD4-BEGMT-RAD2                   
127200     END-IF                                                               
127300     MOVE PRT-AFTER-1  TO PRT-RADSKIP                                     
127400     MOVE HRAD4        TO WS-PRT-RAD                                      
127500     PERFORM S02-SKRIV-RAD                                                
127600                                                                          
127700     IF MID-PROFORMA = JA                                                 
127800        MOVE PHUV-ADGMT-GATA        TO HRAD5-ADGMT-GATA                   
127900     ELSE                                                                 
128000        MOVE OHUV-ADGMT-GATA        TO HRAD5-ADGMT-GATA                   
128100     END-IF                                                               
128200     MOVE PRT-AFTER-1  TO PRT-RADSKIP                                     
128300     MOVE HRAD5        TO WS-PRT-RAD                                      
128400     PERFORM S02-SKRIV-RAD                                                
128500                                                                          
128600     IF MID-PROFORMA = JA                                                 
128700        MOVE PHUV-ADGMT-PADR        TO HRAD6-ADGMT-PADR                   
128800     ELSE                                                                 
128900        MOVE OHUV-ADGMT-PADR        TO HRAD6-ADGMT-PADR                   
129000     END-IF                                                               
129100     MOVE PRT-AFTER-1  TO PRT-RADSKIP                                     
129200     MOVE HRAD6        TO WS-PRT-RAD                                      
129300     PERFORM S02-SKRIV-RAD                                                
129400                                                                          
129500     IF MID-PROFORMA = JA                                                 
129600        MOVE PHUV-ADGMT-LAND        TO HRAD7-ADGMT-LAND                   
129700        MOVE PHUV-TIREGDAT          TO W-TIREGDAT                         
129800     ELSE                                                                 
129900        MOVE OHUV-ADGMT-LAND        TO HRAD7-ADGMT-LAND                   
130000        MOVE OHUV-TIREGDAT          TO W-TIREGDAT                         
130100     END-IF                                                               
130200     MOVE W-TIREGDAT-YY             TO HRAD7-TIREGDAT-YY                  
130300     MOVE W-TIREGDAT-MM             TO HRAD7-TIREGDAT-MM                  
130400     MOVE W-TIREGDAT-DD             TO HRAD7-TIREGDAT-DD                  
130500     MOVE W-TIAAMMDD6               TO HRAD7-TIAAMMDD                     
130600     MOVE MID-KDFRAKT (RADANT-MID)  TO HRAD7-KDFRAKT                      
130700     PERFORM S04-BEFRAKT                                                  
130800     MOVE PRT-AFTER-1  TO PRT-RADSKIP                                     
130900     MOVE HRAD7        TO WS-PRT-RAD                                      
131000     PERFORM S02-SKRIV-RAD                                                
131100                                                                          
131200     MOVE PRT-AFTER-1  TO PRT-RADSKIP                                     
131300     MOVE HRAD-STRECK  TO WS-PRT-RAD                                      
131400     PERFORM S02-SKRIV-RAD                                                
131500                                                                          
131600     MOVE PRT-AFTER-1  TO PRT-RADSKIP                                     
131700     MOVE HRAD8        TO WS-PRT-RAD                                      
131800     PERFORM S02-SKRIV-RAD                                                
131900                                                                          
132000     MOVE PRT-AFTER-1  TO PRT-RADSKIP                                     
132100     MOVE HRAD9        TO WS-PRT-RAD                                      
132200     PERFORM S02-SKRIV-RAD                                                
132300                                                                          
132400     MOVE +15                      TO RADANT                              
132500     .                                                                    
132600     EJECT                                                                
132700 S02-SKRIV-RAD SECTION.                                                   
132800                                                                          
132900     CALL W006PRS1 USING PRT-SPOOL-OVR                                    
133000                         PRT-WRITE                                        
133100                         WS-PRT-PRINTER                                   
133200                         ALT1-PCB                                         
133300                         PRT-RADSKIP                                      
133400                         WS-PRT-LISTRAD                                   
133500     .                                                                    
133600     EJECT                                                                
133700 S03-HAMTA-BENAMNING SECTION.                                             
133800                                                                          
133900     PERFORM IMS-GU-BENA                                                  
134000                                                                          
134100     IF SEGMENT-FINNS                                                     
134200        MOVE TEXT-BEART        TO RAD-BEART                               
134300     END-IF                                                               
134400     .                                                                    
134500     EJECT                                                                
134600                                                                          
134700 S04-BEFRAKT        SECTION.                                              
134800                                                                          
134900     MOVE MID-KDFRAKT (RADANT-MID) TO W-4732-KDFRAKT                      
135000     PERFORM IMS-GU-4732                                                  
135100                                                                          
135200     IF SEGMENT-FINNS                                                     
135300         MOVE MID-IDDC     (RADANT-MID) TO W-IDDC                         
135400         MOVE W-IDDC(1:1)               TO IX-BEFRAKT                     
135500         MOVE FRAKT-BEFRAKT(IX-BEFRAKT) TO HRAD7-BEFRAKT                  
135600     ELSE                                                                 
135700       MOVE '---'                     TO HRAD7-BEFRAKT                    
135800     END-IF                                                               
135900     .                                                                    
136000     EJECT                                                                
136100* --- IMS SEKTIONER ---                                                   
136200                                                                          
136300 IMS-GU-MSG SECTION.                                                      
136400                                                                          
136500     MOVE '  QC' TO GODK-STATUSKODER                                      
136600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
136700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
136800     PERFORM IMS-STATUSKONTROLL                                           
136900     .                                                                    
137000     EJECT                                                                
137100 IMS-GN-WDQ1          SECTION.                                            
137200                                                                          
137300     STRING 'WLORQM01(WDQ101KY>=' W-OBKR-NYCKEL-MIN-X                     
137400                    '&WDQ101KY<=' W-OBKR-NYCKEL-MAX-X                     
137500                    '&IDSYSTEMNE' W-IDSYSTEM-X ')'                        
137600          DELIMITED BY SIZE INTO SSA1                                     
137700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
137800     CALL CBLTDLI USING GN ORQM-PCB DLI-IO-AREA1 SSA1                     
137900     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
138000     PERFORM IMS-STATUSKONTROLL                                           
138100     .                                                                    
138200     EJECT                                                                
138300 IMS-GN-WDQ1-PROF     SECTION.                                            
138400                                                                          
138500     STRING 'WLORQM01(WDQ101KY>=' W-OBKR-NYCKEL-MIN-X                     
138600                    '&WDQ101KY<=' W-OBKR-NYCKEL-MAX-X                     
138700                    '&IDSYSTEM =' W-IDSYSTEM-X ')'                        
138800          DELIMITED BY SIZE INTO SSA1                                     
138900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
139000     CALL CBLTDLI USING GN ORQM-PCB DLI-IO-AREA1 SSA1                     
139100     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
139200     PERFORM IMS-STATUSKONTROLL                                           
139300     .                                                                    
139400     EJECT                                                                
139500 IMS-GU-WDQ1          SECTION.                                            
139600                                                                          
139700     STRING 'WLORQM01(WDQ101KY= ' W-OBKR-NYCKEL-MIN-X                     
139800                    '&IDSYSTEMNE' W-IDSYSTEM-X ')'                        
139900          DELIMITED BY SIZE INTO SSA1                                     
140000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
140100     CALL CBLTDLI USING GU ORQM-PCB DLI-IO-AREA1 SSA1                     
140200     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
140300     PERFORM IMS-STATUSKONTROLL                                           
140400     .                                                                    
140500                                                                          
140600                                                                          
140700 IMS-GU-WDQ1-PROF     SECTION.                                            
140800                                                                          
140900     STRING 'WLORQM01(WDQ101KY= ' W-OBKR-NYCKEL-MIN-X                     
141000                    '&IDSYSTEM =' W-IDSYSTEM-X ')'                        
141100          DELIMITED BY SIZE INTO SSA1                                     
141200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
141300     CALL CBLTDLI USING GU ORQM-PCB DLI-IO-AREA1 SSA1                     
141400     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
141500     PERFORM IMS-STATUSKONTROLL                                           
141600     .                                                                    
141700                                                                          
141800 IMS-GU-WDQ2          SECTION.                                            
141900                                                                          
142000     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
142100          DELIMITED BY SIZE INTO SSA1                                     
142200     MOVE '  GE' TO GODK-STATUSKODER                                      
142300     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-AREA2 SSA1                     
142400     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
142500     PERFORM IMS-STATUSKONTROLL                                           
142600     .                                                                    
142700     EJECT                                                                
142800                                                                          
142900                                                                          
143000 IMS-GNP-WDQ212        SECTION.                                           
143100                                                                          
143200     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
143300          DELIMITED BY SIZE INTO SSA1                                     
143400     MOVE '  GE' TO GODK-STATUSKODER                                      
143500     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-AREA7 SSA1                    
143600     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
143700     PERFORM IMS-STATUSKONTROLL                                           
143800     .                                                                    
143900     EJECT                                                                
144000                                                                          
144100                                                                          
144200 IMS-GU-WDE8          SECTION.                                            
144300                                                                          
144400     STRING 'WLPROC01(WDE801KY =' W-IDGMTREF-X ')'                        
144500          DELIMITED BY SIZE INTO SSA1                                     
144600     MOVE '  GE' TO GODK-STATUSKODER                                      
144700     CALL CBLTDLI USING GU PROC-PCB DLI-IO-AREA6 SSA1                     
144800     MOVE PROC-STATUS-CODE TO STATUS-WS                                   
144900     PERFORM IMS-STATUSKONTROLL                                           
145000     .                                                                    
145100     EJECT                                                                
145200                                                                          
145300 IMS-GU-BENA          SECTION.                                            
145400                                                                          
145500     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
145600          DELIMITED BY SIZE INTO SSA1                                     
145700     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
145800          DELIMITED BY SIZE INTO SSA2                                     
145900     MOVE '  GE' TO GODK-STATUSKODER                                      
146000     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA3 SSA1 SSA2                
146100     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
146200     PERFORM IMS-STATUSKONTROLL                                           
146300     .                                                                    
146400     EJECT                                                                
146500                                                                          
146600                                                                          
146700 IMS-GU-4732          SECTION.                                            
146800                                                                          
146900     STRING 'WL473201(WDGXKEY  =' W-4732-IDHTYP-X ')'                     
147000          DELIMITED BY SIZE INTO SSA1                                     
147100     MOVE 'WL473211 '        TO SSA2                                      
147200     MOVE '  GE' TO GODK-STATUSKODER                                      
147300     CALL CBLTDLI USING GU 4732-PCB DLI-IO-AREA4 SSA1 SSA2                
147400     MOVE 4732-STATUS-CODE TO STATUS-WS                                   
147500     PERFORM IMS-STATUSKONTROLL                                           
147600     .                                                                    
147700     EJECT                                                                
147800 IMS-GU-SATB-WDJ111-01 SECTION.                                           
147900     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
148000          DELIMITED BY SIZE INTO SSA1                                     
148100     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-X ')'                         
148200          DELIMITED BY SIZE INTO SSA2                                     
148300     MOVE '  GE'              TO GODK-STATUSKODER                         
148400     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA5 SSA1 SSA2                
148500     MOVE SATB-STATUS-CODE    TO STATUS-WS                                
148600     PERFORM IMS-STATUSKONTROLL                                           
148700     .                                                                    
148800                                                                          
148900                                                                          
149000                                                                          
149100 IMS-GN-SATB-WDJ111-01 SECTION.                                           
149200     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
149300          DELIMITED BY SIZE INTO SSA1                                     
149400     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-X ')'                         
149500          DELIMITED BY SIZE INTO SSA2                                     
149600     MOVE '  GEGB'            TO GODK-STATUSKODER                         
149700     CALL CBLTDLI USING GN SATB-PCB DLI-IO-AREA5 SSA1 SSA2                
149800     MOVE SATB-STATUS-CODE    TO STATUS-WS                                
149900     PERFORM IMS-STATUSKONTROLL                                           
150000     .                                                                    
150100 IMS-STATUSKONTROLL SECTION.                                              
150200                                                                          
150300     SET STATUS-IX TO 1                                                   
150400     SEARCH GODK-STATUS                                                   
150500       AT END                                                             
150600         CALL FELLOG                                                      
150700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
150800         CONTINUE                                                         
150900     END-SEARCH                                                           
151000     .                                                                    
151100     EJECT                                                                
151200*    -COPY WY2000Q1                                                       
