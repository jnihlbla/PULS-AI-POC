000100*                                                                         
000200******************************************************************        
000300*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0154      *        
000400******************************************************************        
000500*                                                                         
000600 ID DIVISION.                                                             
000700     SKIP2                                                                
000800 PROGRAM-ID.     W4079400.                                                
000900*AUTHOR.         LARS THELL.                                              
001000*DATE-WRITTEN.   95/06/29.                                                
001100                                                                          
001200*    REMARKS.                                                             
001300*                                                                         
001400*    FUNKTION:                                                            
001500*        BAKGRUNDS MPP SOM SKRIVER UT RETURTILLSTÅND.                     
001600*        STARTAS AV W40735, W40736 OCH W40737                             
001700*                                                                         
001800*        PROGRAMMET          LÄSER      WLKREE (WDA2)                     
001900*        PROGRAMMET          LÄSER      WLRETA (WDA3)                     
002000*        PROGRAMMET          LÄSER      WLARTC (WDK6)                     
002100*        PROGRAMMET          LÄSER      WLARTS (WDK7)                     
002200*        PROGRAMMET          LÄSER      WLBENA (WDD3)                     
002210*        PROGRAMMET          LÄSER      WDD5                              
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W4T794X                                             
002600*        MID:         W4I79401                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        RETURTILLSTÅND                                                   
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700*    -- CHECKED BY WY2000                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'W4079400'.            
003900                                                                          
004000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004200                                                                          
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  YES                         PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004600                                                                          
004700 77  W-IDDC                      PIC X(2)    VALUE SPACE.                 
004800 77  WS-IDDC-LEV                 PIC X(2)    VALUE SPACE.                 
004900                                                                          
005000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005100     88  NYCKLAR-OK                          VALUE 'J'.                   
005200     88  NYCKLAR-FEL                         VALUE 'N'.                   
005300                                                                          
005400 77  RNOT-SW                    PIC X        VALUE 'N'.                   
005500     88  SKRIV-RNOT                          VALUE 'J'.                   
005600     88  SKRIV-INTE-RNOT                     VALUE 'N'.                   
005700                                                                          
005800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005900     88  EGEN-MID                            VALUE '4794'.                
006000                                                                          
006100 77  INDX                        PIC S9(4)   VALUE ZERO COMP SYNC.        
006200 77  W-KVLEVANM-KVAR             PIC S9(7)   VALUE 0   COMP-3.            
006300 77  W-IDSIDNR                   PIC S9(3)   VALUE 0   COMP-3.            
006400 77  W-FLFLER-KOLLI              PIC  X(1)   VALUE 'N'.                   
006500 77  MAX-KVRADER                 PIC S9(3)   VALUE +40 COMP-3.            
006600 77  W-KVRADER                   PIC S9(3)   VALUE ZERO COMP-3.           
006700 77  W-REKSIFFR                  PIC S9(1)   VALUE ZERO COMP-3.           
006800 77  W-ADLAGOMR                  PIC S9(3)   VALUE ZERO COMP-3.           
006900 77  W-ADGANG                    PIC S9(3)   VALUE ZERO COMP-3.           
007000 77  W-ADPLATS                   PIC S9(5)   VALUE ZERO COMP-3.           
007100 77  W-KDERS                     PIC S9(3)   VALUE ZERO COMP-3.           
007200 77  W-BEART                     PIC X(25)   VALUE SPACE.                 
007300                                                                          
007400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007500 01  GENERELLA-SUBPROGRAM.                                                
007600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007800     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
007900     03  W418OKOD                PIC X(8)    VALUE 'W418OKOD'.            
008000     EJECT                                                                
008100*    ---  LÄNKAREA TILL W418OKOD                                          
008200 01  FILLER                      PIC X(16)   VALUE 'W418OKOD'.            
008300                                                                          
008400*01 -COPY W418OKOD           -PRE OKOD-.                                  
008500     SKIP3                                                                
008600* VARIABLER TILL SUBPROGRAM W006PRR1                                      
008700*01  -COPY W006PRAR                                                       
008800     SKIP2                                                                
008900     EJECT                                                                
009000*      --- VALID IDDC CODES                                               
009100*                                                                         
009200*01    -COPY WWDC99                                                       
009300       EJECT                                                              
009400*                                                                         
009500 01  WS-RAPP-AREA.                                                        
009600     03  WS-RAPP-PRINTER         PIC X(8).                                
009700     03  WS-RAPP-LISTRAD.                                                 
009800         05  FILLER              PIC X(1)    VALUE SPACE.                 
009900         05  WS-RAPP-RAD         PIC X(131).                              
010000     03  WS-DUMMY                PIC X(1).                                
010100     EJECT                                                                
010200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
010300*                                                                         
010400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010500     SKIP3                                                                
010600*01  MID -COPY W4I79401                                                   
010700     EJECT                                                                
010800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010900     SKIP3                                                                
011000*01  -COPY WMSGAREA                                                       
011100     EJECT                                                                
011200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011300*                                                                         
011400     EJECT                                                                
011500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011600     SKIP3                                                                
011700 01  NYCKLAR-TILL-DLI.                                                    
011800                                                                          
011900     03  W-IDLEVANM-X.                                                    
012000         05  W-IDDISTR-ANM       PIC S9(5)   COMP-3 VALUE ZERO.           
012100         05  W-IDKUNDNR-ANM      PIC S9(7)   COMP-3 VALUE ZERO.           
012200         05  W-IDRAPPNR-ANM      PIC  X(7)          VALUE ZERO.           
012300                                                                          
012400     03  W-WDA3F1KY-MIN-X.                                                
012500         05  W-IDDC-X.                                                    
012600           07  W-IDDC-RET-MIN    PIC  X(2)          VALUE SPACE.          
012700         05  W-IDDISTR-RET-MIN   PIC S9(5)   COMP-3 VALUE ZERO.           
012800         05  W-IDKUNDNR-RET-MIN  PIC S9(7)   COMP-3 VALUE ZERO.           
012900         05  W-IDRAPPNR-RET-MIN  PIC  X(7)          VALUE ZERO.           
013000         05  W-IDRT-RET-MIN      PIC  X(3)          VALUE SPACE.          
013100         05  W-IDRTLOP-RET-MIN   PIC  9(3)          VALUE ZERO.           
013200         05  W-IDKOLLI-RET-MIN   PIC S9(5)   COMP-3 VALUE ZERO.           
013300         05  W-DAREGDAT-RET-MIN  PIC  9(8)          VALUE ZERO.           
013400         05  W-TIKLOCK-RET-MIN   PIC S9(9)   COMP-3 VALUE ZERO.           
013500                                                                          
013600     03  W-WDA3F1KY-MAX-X.                                                
013700         05  W-IDDC-RET-MAX      PIC  X(2)          VALUE SPACE.          
013800         05  W-IDDISTR-RET-MAX   PIC S9(5)   COMP-3 VALUE ZERO.           
013900         05  W-IDKUNDNR-RET-MAX  PIC S9(7)   COMP-3 VALUE ZERO.           
014000         05  W-IDRAPPNR-RET-MAX  PIC  X(7)          VALUE ZERO.           
014100         05  W-IDRT-RET-MAX      PIC  X(3)          VALUE SPACE.          
014200         05  W-IDRTLOP-RET-MAX   PIC  9(3)          VALUE ZERO.           
014300         05  W-IDKOLLI-RET-MAX   PIC S9(5)   COMP-3 VALUE ZERO.           
014400         05  W-DAREGDAT-RET-MAX  PIC  9(8)          VALUE ZERO.           
014500         05  W-TIKLOCK-RET-MAX   PIC S9(9)   COMP-3 VALUE ZERO.           
014600                                                                          
014700     03  W-WDA211KY-X.                                                    
014800       04 W-IDARTNR-X.                                                    
014900         05  W-IDARTNR           PIC S9(9)   COMP-3 VALUE ZERO.           
015000       04 W-IDRADNR-X.                                                    
015100         05  W-IDRADNR           PIC S9(5)   COMP-3 VALUE ZERO.           
015200                                                                          
015300     03  W-IDSKYLT-X.                                                     
015400         05  W-IDSKYLT           PIC  X(3)   VALUE 'S  '.                 
015500                                                                          
015600     03  W-KDKVAINF-X.                                                    
015700         05  W-KDKVAINF          PIC  X(1)   VALUE 'R'.                   
015800                                                                          
015900     03  W-IDDC-B6-X.                                                     
016000         05 W-IDDC-B6                  PIC X(2).                          
016100                                                                          
016110     03  W-KDEMBAL-X.                                                     
016120         05  W-KDEMBAL           PIC X(3)    VALUE SPACE.                 
016200     SKIP2                                                                
016300*    --- STATUS-KOD FRÅN IMS                                              
016400 01  STATUS-WS                   PIC XX.                                  
016500     88  SEGMENT-FINNS                       VALUE '  '.                  
016600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
016900     SKIP2                                                                
017000 01  GODK-STATUSKODER.                                                    
017100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017200     SKIP3                                                                
017300 01  SSA1                        PIC X(128).                              
017400 01  SSA2                        PIC X(64).                               
017500 01  SSA3                        PIC X(64).                               
017600     EJECT                                                                
017700*    --- IMS FUNKTIONSKODER                                               
017800*01  -COPY W0003                                                          
017900     EJECT                                                                
018000*    ---  DLI INPUT-OUTPUT AREA                                           
018100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA1'.        
018200     SKIP3                                                                
018300 01  DLI-IO-AREA1.                                                        
018400     03  IO-AREA1                PIC X(500)  VALUE SPACE.                 
018500     SKIP3                                                                
018600     03  WLKREE01 REDEFINES IO-AREA1.                                     
018700*        05  -COPY WDA201                                                 
018800     EJECT                                                                
018900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
019000     SKIP3                                                                
019100 01  DLI-IO-AREA2.                                                        
019200     03  IO-AREA2                PIC X(500)  VALUE SPACE.                 
019300     SKIP3                                                                
019400     03  WLKREE11 REDEFINES IO-AREA2.                                     
019500*        05  -COPY WDA211                                                 
019600     EJECT                                                                
019700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA3'.        
019800     SKIP3                                                                
019900 01  DLI-IO-AREA3.                                                        
020000     03  IO-AREA3                PIC X(1200)  VALUE SPACE.                
020100     SKIP3                                                                
020200     03  WLRETG01 REDEFINES IO-AREA3.                                     
020300*        05  -COPY WDA3F1                                                 
020400     EJECT                                                                
020500     03  WLARTC01 REDEFINES IO-AREA3.                                     
020600*        05  -COPY WDK601                                                 
020700     EJECT                                                                
020800     03  WLARTC11 REDEFINES IO-AREA3.                                     
020900*        05  -COPY WDK611                                                 
021000     EJECT                                                                
021100     03  WLARTS11 REDEFINES IO-AREA3.                                     
021200*        05  -COPY WDK711                                                 
021300     EJECT                                                                
021400     03  WLBENA11 REDEFINES IO-AREA3.                                     
021500*        05  -COPY WDD311                                                 
021600     EJECT                                                                
021700     03  W6KVAH11 REDEFINES IO-AREA3.                                     
021800*        05  -COPY W6D211                                                 
021900     EJECT                                                                
022000     03  WLKREE21 REDEFINES IO-AREA3.                                     
022100*        05  -COPY WDA221                                                 
022200                                                                          
022300 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
022400 01   DLI-IO-AREA-B601.                                                   
022500*     03  -COPY WDB601                                                    
022600                                                                          
022610 01  FILLER               PIC X(16)   VALUE 'WDK613 AREA'.                
022620 01   DLI-IO-AREA-K613.                                                   
022630*     03  -COPY WDK613                                                    
022631                                                                          
022633 01  FILLER               PIC X(16)   VALUE 'WDD501 AREA'.                
022634 01   DLI-IO-AREA-D501.                                                   
022636*     03  -COPY WDD501                                                    
022640                                                                          
022800*  PRINTRADER FÖR INLÄGGNINGSLISTA RAPPORT                                
022900                                                                          
023000 01  LIST-HRAD1.                                                          
023100     03   FILLER                  PIC X(1)  VALUE SPACE.                  
023200     03   FILLER                  PIC X(22) VALUE                         
023300                                        'VOLVO CAR PARTS       '.         
023400     03   FILLER                  PIC X(9)  VALUE SPACE.                  
023500     03   FILLER                  PIC X(6)  VALUE 'W40794'.               
023600     03   FILLER                  PIC X(10) VALUE SPACE.                  
023700     03   FILLER                  PIC X(25) VALUE                         
023800                                        'RETURTILLSTÅND INTERNT'.         
023900     03   FILLER                  PIC X(24) VALUE SPACE.                  
024000     03   HRAD1-DATUM             PIC X(6)  VALUE SPACE.                  
024100     03   FILLER                  PIC X(8)  VALUE SPACE.                  
024200     03   FILLER                  PIC X(4)  VALUE 'SID '.                 
024300     03   HRAD1-IDSIDNR           PIC Z(2)9 VALUE ZERO.                   
024400                                                                          
024500 01  LIST-HRAD1-ENG.                                                      
024600     03   FILLER                  PIC X(1)  VALUE SPACE.                  
024700     03   FILLER                  PIC X(22) VALUE                         
024800                                        'VOLVO CAR PARTS       '.         
024900     03   FILLER                  PIC X(9)  VALUE SPACE.                  
025000     03   FILLER                  PIC X(6)  VALUE 'W40794'.               
025100     03   FILLER                  PIC X(10) VALUE SPACE.                  
025200     03   FILLER                  PIC X(25) VALUE                         
025300                                        'RETURN PERMIT INTERNAL'.         
025400     03   FILLER                  PIC X(24) VALUE SPACE.                  
025500     03   HRAD1-DATUM-ENG         PIC X(6)  VALUE SPACE.                  
025600     03   FILLER                  PIC X(8)  VALUE SPACE.                  
025700     03   FILLER                  PIC X(4)  VALUE 'SID '.                 
025800     03   HRAD1-IDSIDNR-ENG       PIC Z(2)9 VALUE ZERO.                   
025900                                                                          
026000 01  LIST-HRAD2.                                                          
026100     03   FILLER                  PIC X(30) VALUE SPACE.                  
026200     03   FILLER                  PIC X(5)  VALUE 'DISTR'.                
026300     03   FILLER                  PIC X(5)  VALUE SPACE.                  
026400     03   FILLER                  PIC X(4)  VALUE 'KUND'.                 
026500     03   FILLER                  PIC X(5)  VALUE SPACE.                  
026600     03   FILLER                  PIC X(9)  VALUE 'RAPPORTNR'.            
026700     03   FILLER                  PIC X(5)  VALUE SPACE.                  
026800     03   FILLER                  PIC X(21) VALUE                         
026900                                          'LIGGER I FLERA KOLLIN'.        
027000                                                                          
027100 01  LIST-HRAD2-ENG.                                                      
027200     03   FILLER                  PIC X(30) VALUE SPACE.                  
027300     03   FILLER                  PIC X(5)  VALUE 'DISTR'.                
027400     03   FILLER                  PIC X(5)  VALUE SPACE.                  
027500     03   FILLER                  PIC X(4)  VALUE 'CUST'.                 
027600     03   FILLER                  PIC X(5)  VALUE SPACE.                  
027700     03   FILLER                  PIC X(9)  VALUE 'REPORT NO'.            
027800     03   FILLER                  PIC X(5)  VALUE SPACE.                  
027900     03   FILLER                  PIC X(21) VALUE                         
028000                                          'IN SEVERAL CASES     '.        
028100                                                                          
028200 01  LIST-HRAD3.                                                          
028300     03   FILLER                  PIC X(31) VALUE SPACE.                  
028400     03   HRAD3-IDDISTR           PIC Z(4)  VALUE ZERO.                   
028500     03   FILLER                  PIC X(3)  VALUE SPACE.                  
028600     03   HRAD3-IDKUNDNR          PIC Z(6)  VALUE ZERO.                   
028700     03   FILLER                  PIC X(7)  VALUE SPACE.                  
028800     03   HRAD3-IDRAPPNR          PIC Z(7)  VALUE ZERO.                   
028900     03   FILLER                  PIC X(13) VALUE SPACE.                  
029000     03   HRAD3-FLFLER-KOLLI      PIC X(1)  VALUE SPACE.                  
029100                                                                          
029200 01  LIST-HRAD4.                                                          
029300     03   FILLER                  PIC X(32) VALUE SPACE.                  
029400     03   FILLER                  PIC X(1)  VALUE 'A'.                    
029500     03   FILLER                  PIC X(4)  VALUE SPACE.                  
029600     03   FILLER                  PIC X(1)  VALUE 'N'.                    
029700     03   FILLER                  PIC X(4)  VALUE SPACE.                  
029800     03   FILLER                  PIC X(1)  VALUE 'T'.                    
029900     03   FILLER                  PIC X(4)  VALUE SPACE.                  
030000     03   FILLER                  PIC X(1)  VALUE 'A'.                    
030100     03   FILLER                  PIC X(4)  VALUE SPACE.                  
030200     03   FILLER                  PIC X(1)  VALUE 'L'.                    
030300     03   FILLER                  PIC X(32) VALUE SPACE.                  
030400     03   FILLER                  PIC X(1)  VALUE 'A'.                    
030500     03   FILLER                  PIC X(4)  VALUE SPACE.                  
030600     03   FILLER                  PIC X(1)  VALUE 'N'.                    
030700     03   FILLER                  PIC X(4)  VALUE SPACE.                  
030800     03   FILLER                  PIC X(1)  VALUE 'T'.                    
030900     03   FILLER                  PIC X(4)  VALUE SPACE.                  
031000     03   FILLER                  PIC X(1)  VALUE 'A'.                    
031100     03   FILLER                  PIC X(4)  VALUE SPACE.                  
031200     03   FILLER                  PIC X(1)  VALUE 'L'.                    
031300                                                                          
031400 01  LIST-HRAD4-ENG.                                                      
031500     03   FILLER                  PIC X(32) VALUE SPACE.                  
031600     03   FILLER                  PIC X(1)  VALUE ' '.                    
031700     03   FILLER                  PIC X(4)  VALUE SPACE.                  
031800     03   FILLER                  PIC X(1)  VALUE 'Q'.                    
031900     03   FILLER                  PIC X(4)  VALUE SPACE.                  
032000     03   FILLER                  PIC X(1)  VALUE 'T'.                    
032100     03   FILLER                  PIC X(4)  VALUE SPACE.                  
032200     03   FILLER                  PIC X(1)  VALUE 'Y'.                    
032300     03   FILLER                  PIC X(4)  VALUE SPACE.                  
032400     03   FILLER                  PIC X(1)  VALUE ' '.                    
032500     03   FILLER                  PIC X(42) VALUE SPACE.                  
032600     03   FILLER                  PIC X(1)  VALUE ' '.                    
032700     03   FILLER                  PIC X(4)  VALUE SPACE.                  
032800     03   FILLER                  PIC X(1)  VALUE 'Q'.                    
032900     03   FILLER                  PIC X(4)  VALUE SPACE.                  
033000     03   FILLER                  PIC X(1)  VALUE 'T'.                    
033100     03   FILLER                  PIC X(4)  VALUE SPACE.                  
033200     03   FILLER                  PIC X(1)  VALUE 'Y'.                    
033300     03   FILLER                  PIC X(4)  VALUE SPACE.                  
033400     03   FILLER                  PIC X(1)  VALUE ' '.                    
033500                                                                          
033600 01  LIST-HRAD5A.                                                         
033700     03   FILLER                  PIC X(36) VALUE SPACE.                  
033710     03   FILLER                  PIC X(49) VALUE SPACE.                  
033800     03   FILLER                  PIC X(4)  VALUE 'FIFO'.                 
033810                                                                          
033820 01  LIST-HRAD5.                                                          
033830     03   FILLER                  PIC X(1)  VALUE SPACE.                  
033840     03   FILLER                  PIC X(7)  VALUE 'ORDER  '.              
033900     03   FILLER                  PIC X(1)  VALUE SPACE.                  
034000     03   FILLER                  PIC X(2)  VALUE 'DC'.                   
034100     03   FILLER                  PIC X(2)  VALUE SPACE.                  
034200     03   FILLER                  PIC X(9)  VALUE 'ARTIKELNR'.            
034300     03   FILLER                  PIC X(1)  VALUE SPACE.                  
034400     03   FILLER                  PIC X(9)  VALUE 'BENÄMNING'.            
034700     03   FILLER                  PIC X(4)  VALUE SPACE.                  
034710     03   FILLER                  PIC X(3)  VALUE SPACE.                  
034800     03   FILLER                  PIC X(5)  VALUE 'ANTAL'.                
034900     03   FILLER                  PIC X(2)  VALUE SPACE.                  
035000     03   FILLER                  PIC X(3)  VALUE 'KOD'.                  
035100     03   FILLER                  PIC X(3)  VALUE SPACE.                  
035200     03   FILLER                  PIC X(10) VALUE 'LAGERPLATS'.           
035300     03   FILLER                  PIC X(2)  VALUE SPACE.                  
035400     03   FILLER                  PIC X(3)  VALUE 'ERS'.                  
035500     03   FILLER                  PIC X(2)  VALUE SPACE.                  
035510     03   FILLER                  PIC X(4)  VALUE 'SORT'.                 
035520     03   FILLER                  PIC X(7)  VALUE SPACE.                  
035530     03   FILLER                  PIC X(3)  VALUE 'EMB'.                  
035540     03   FILLER                  PIC X(2)  VALUE SPACE.                  
035550     03   FILLER                  PIC X(5)  VALUE 'WEEKS'.                
035560     03   FILLER                  PIC X(2)  VALUE SPACE.                  
035600     03   WS-LAGV-KDARTURS        PIC X(5)  VALUE SPACE.                  
035700     03   FILLER                  PIC X(2)  VALUE SPACE.                  
035800     03   FILLER                  PIC X(10) VALUE 'NOTERINGAR'.           
036500                                                                          
036600 01  LIST-HRAD5A-ENG.                                                     
036700     03   FILLER                  PIC X(44) VALUE SPACE.                  
036710     03   FILLER                  PIC X(28) VALUE SPACE.                  
036800     03   FILLER                  PIC X(2)  VALUE 'RP'.                   
036801     03   FILLER                  PIC X(15) VALUE SPACE.                  
036802     03   FILLER                  PIC X(4)  VALUE 'FIFO'.                 
036810                                                                          
036820 01  LIST-HRAD5-ENG.                                                      
036830     03   FILLER                  PIC X(1)  VALUE SPACE.                  
036840     03   FILLER                  PIC X(7)  VALUE 'ORDERNO'.              
036900     03   FILLER                  PIC X(1)  VALUE SPACE.                  
037000     03   FILLER                  PIC X(2)  VALUE 'DC'.                   
037100     03   FILLER                  PIC X(4)  VALUE SPACE.                  
037200     03   FILLER                  PIC X(7)  VALUE 'PART NO'.              
037300     03   FILLER                  PIC X(1)  VALUE SPACE.                  
037400     03   FILLER                  PIC X(11) VALUE 'DESCRIPTION'.          
037500     03   FILLER                  PIC X(3)  VALUE SPACE.                  
037600     03   FILLER                  PIC X(7)  VALUE 'CONFIRM'.              
037700     03   FILLER                  PIC X(5)  VALUE SPACE.                  
037800     03   FILLER                  PIC X(4)  VALUE 'LEFT'.                 
037900     03   FILLER                  PIC X(1)  VALUE SPACE.                  
038000     03   FILLER                  PIC X(4)  VALUE 'CODE'.                 
038100     03   FILLER                  PIC X(2)  VALUE SPACE.                  
038200     03   FILLER                  PIC X(10) VALUE 'STORAGE   '.           
038300     03   FILLER                  PIC X(2)  VALUE SPACE.                  
038400     03   FILLER                  PIC X(3)  VALUE 'CD'.                   
038500     03   FILLER                  PIC X(2)  VALUE SPACE.                  
038600     03   WE-LAGV-KDARTURS        PIC X(5)  VALUE SPACE.                  
038700     03   FILLER                  PIC X(1)  VALUE SPACE.                  
038702     03   FILLER                  PIC X(4)  VALUE 'SORT'.                 
038703     03   FILLER                  PIC X(2)  VALUE SPACE.                  
038704     03   FILLER                  PIC X(5)  VALUE 'WEEKS'.                
038705     03   FILLER                  PIC X(2)  VALUE SPACE.                  
038800     03   FILLER                  PIC X(3)  VALUE 'BIN'.                  
038900     03   FILLER                  PIC X(2)  VALUE SPACE.                  
039000     03   FILLER                  PIC X(3)  VALUE 'SCR'.                  
039100     03   FILLER                  PIC X(2)  VALUE SPACE.                  
039200     03   FILLER                  PIC X(3)  VALUE 'DEV'.                  
039300     03   FILLER                  PIC X(2)  VALUE SPACE.                  
039400     03   FILLER                  PIC X(3)  VALUE 'QDE'.                  
039500                                                                          
039600 01  LIST-LRAD.                                                           
039700     03   FILLER                  PIC X(1)  VALUE SPACE.                  
039800     03   LRAD-IDORDNR7           PIC Z(6)9.                              
039900     03   FILLER                  PIC X(1)  VALUE SPACE.                  
040000     03   LRAD-IDDC               PIC X(2).                               
040100     03   FILLER                  PIC X(1)  VALUE SPACE.                  
040200     03   LRAD-IDARTNR            PIC Z(7)9.                              
040300     03   FILLER                  PIC X(1)  VALUE '-'.                    
040400     03   LRAD-REKSIFFR           PIC 9(1).                               
040500     03   FILLER                  PIC X(1)  VALUE SPACE.                  
040600     03   LRAD-BEART              PIC X(13).                              
040900     03   FILLER                  PIC X(1)  VALUE SPACE.                  
041000     03   LRAD-KVLEVANM-KVAR      PIC Z(6)9.                              
041100     03   FILLER                  PIC X(3)  VALUE SPACE.                  
041200     03   LRAD-KDANMORS           PIC Z(1)9.                              
041300     03   FILLER                  PIC X(2)  VALUE SPACE.                  
041400     03   LRAD-ADLAGOMR           PIC Z(1)9.                              
041500     03   FILLER REDEFINES LRAD-ADLAGOMR.                                 
041600          05 LRAD-NA-ADLAGOMR     PIC 9(2).                               
041700     03   FILLER                  PIC X(1)  VALUE SPACE.                  
041800     03   LRAD-ADGANG             PIC Z(1)9.                              
041900     03   FILLER REDEFINES LRAD-ADGANG.                                   
042000          05 LRAD-NA-ADGANG       PIC 9(2).                               
042100     03   FILLER                  PIC X(1)  VALUE SPACE.                  
042200     03   LRAD-ADPLATS            PIC Z(4)9.                              
042300     03   FILLER REDEFINES LRAD-ADPLATS.                                  
042400          05 LRAD-NA-ADPLATS      PIC 9(5).                               
042500     03   FILLER                  PIC X(3)  VALUE SPACE.                  
042600     03   LRAD-KDERS              PIC Z(2).                               
042700     03   FILLER                  PIC X(4)  VALUE SPACE.                  
042710     03   LRAD-KDSORT             PIC X(2).                               
042720     03   FILLER                  PIC X(1)  VALUE SPACE.                  
042730     03   LRAD-IDARTNR-EMB        PIC Z(9).                               
042740     03   FILLER                  PIC X(5)  VALUE SPACE.                  
042750     03   LRAD-KDFGPRIO           PIC X(3).                               
042760     03   FILLER                  PIC X(5)  VALUE SPACE.                  
042800     03   LRAD-LAGV-KDARTURS      PIC X(2)  VALUE SPACE.                  
042810                                                                          
043010 01  LIST-LRAD-ENG.                                                       
043020     03   FILLER                  PIC X(1)  VALUE SPACE.                  
043030     03   LRAD-E-IDORDNR7         PIC Z(6)9.                              
043040     03   FILLER                  PIC X(1)  VALUE SPACE.                  
043050     03   LRAD-E-IDDC             PIC X(2).                               
043060     03   FILLER                  PIC X(1)  VALUE SPACE.                  
043070     03   LRAD-E-IDARTNR          PIC Z(7)9.                              
043080     03   FILLER                  PIC X(1)  VALUE '-'.                    
043090     03   LRAD-E-REKSIFFR         PIC 9(1).                               
043091     03   FILLER                  PIC X(1)  VALUE SPACE.                  
043092     03   LRAD-E-BEART            PIC X(13).                              
043093     03   FILLER                  PIC X(1)  VALUE SPACE.                  
043094     03   LRAD-E-KVLEVANM-BEKR    PIC Z(6)9.                              
043095     03   FILLER                  PIC X(2)  VALUE SPACE.                  
043096     03   LRAD-E-KVLEVANM-KVAR    PIC Z(6)9.                              
043097     03   FILLER                  PIC X(3)  VALUE SPACE.                  
043098     03   LRAD-E-KDANMORS         PIC Z(1)9.                              
043099     03   FILLER                  PIC X(1)  VALUE SPACE.                  
043100     03   LRAD-E-ADLAGOMR         PIC Z(1)9.                              
043101     03   FILLER REDEFINES LRAD-E-ADLAGOMR.                               
043102          05 LRAD-E-NA-ADLAGOMR   PIC 9(2).                               
043103     03   FILLER                  PIC X(1)  VALUE SPACE.                  
043104     03   LRAD-E-ADGANG           PIC Z(1)9.                              
043105     03   FILLER REDEFINES LRAD-E-ADGANG.                                 
043106          05 LRAD-E-NA-ADGANG     PIC 9(2).                               
043107     03   FILLER                  PIC X(1)  VALUE SPACE.                  
043108     03   LRAD-E-ADPLATS          PIC Z(4)9.                              
043109     03   FILLER REDEFINES LRAD-E-ADPLATS.                                
043110          05 LRAD-E-NA-ADPLATS    PIC 9(5).                               
043111     03   FILLER                  PIC X(2)  VALUE SPACE.                  
043112     03   LRAD-E-KDERS            PIC Z(2).                               
043113     03   FILLER                  PIC X(4)  VALUE SPACE.                  
043114     03   LRAD-E-LAGV-KDARTURS    PIC X(2)  VALUE SPACE.                  
043115     03   FILLER                  PIC X(4)  VALUE SPACE.                  
043116     03   LRAD-E-KDSORT           PIC X(2).                               
043117     03   FILLER                  PIC X(3)  VALUE SPACE.                  
043120     03   LRAD-E-KDFGPRIO         PIC X(4).                               
043121     03   FILLER                  PIC X(3)  VALUE SPACE.                  
043122                                                                          
043130 01  LIST-TEXT.                                                           
043200     03   FILLER                  PIC X(1)  VALUE SPACE.                  
043300     03   TEXT-TYP                PIC X(3).                               
043400     03   FILLER                  PIC X(1)  VALUE SPACE.                  
043500     03   TEXT-TEXT               PIC X(100).                             
043600                                                                          
043700 LINKAGE SECTION.                                                         
043800                                                                          
043900*01  -COPY W0009   -PRE MSG-                                              
044000     EJECT                                                                
044100*01  -COPY W0009   -PRE ALT-                                              
044200     EJECT                                                                
044300*01  -COPY W0008   -PRE KREE-                                             
044400     05  FILLER                  PIC X.                                   
044500     EJECT                                                                
044600*01  -COPY W0008   -PRE RETG-                                             
044700     05  FILLER                  PIC X.                                   
044800     EJECT                                                                
044900*01  -COPY W0008   -PRE ARTC-                                             
045000     05  FILLER                  PIC X.                                   
045100     EJECT                                                                
045200*01  -COPY W0008   -PRE ARTS-                                             
045300     05  FILLER                  PIC X.                                   
045400     EJECT                                                                
045500*01  -COPY W0008   -PRE BENA-                                             
045600     05  FILLER                  PIC X.                                   
045700     EJECT                                                                
045800*01  -COPY W0008   -PRE KVAH-                                             
045900     05  FILLER                  PIC X.                                   
046000     EJECT                                                                
046100*01  -COPY W0008   -PRE WDB6-                                             
046200     05  FILLER                  PIC X.                                   
046300     EJECT                                                                
046310*01  -COPY W0008   -PRE WDD5-                                             
046320     05  FILLER                  PIC X.                                   
046321     EJECT                                                                
046322*01  -COPY W0008   -PRE WDK6-                                             
046323     05  FILLER                  PIC X.                                   
046330     EJECT                                                                
046400 PROCEDURE DIVISION  USING MSG-PCB  ALT-PCB                               
046500                           KREE-PCB RETG-PCB ARTC-PCB ARTS-PCB            
046600                           BENA-PCB KVAH-PCB WDB6-PCB WDD5-PCB            
046610                           WDK6-PCB.                                      
046700     ENTRY 'DLITCBL' USING MSG-PCB  ALT-PCB                               
046800                           KREE-PCB RETG-PCB ARTC-PCB ARTS-PCB            
046900                           BENA-PCB KVAH-PCB WDB6-PCB WDD5-PCB            
046910                           WDK6-PCB.                                      
047000                                                                          
047100     PERFORM IMS-GET-MSG                                                  
047200     IF SEGMENT-FINNS                                                     
047300       PERFORM A-INIT                                                     
047400       PERFORM B-SKAPA-RT-UTSKRIFT                                        
047500     END-IF                                                               
047600                                                                          
047700     PERFORM Z-FINIT                                                      
047800     MOVE ZERO TO RETURN-CODE                                             
047900     GOBACK                                                               
048000     .                                                                    
048100     EJECT                                                                
048200 A-INIT SECTION.                                                          
048300                                                                          
048400     IF MSG-DUBBLA-TRANSKODER                                             
048500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I79401                 
048600       IF MSG-KDMFSFOR-2 = 2                                              
048700         MOVE 'GB '                TO W-IDSKYLT                           
048800       END-IF                                                             
048900     ELSE                                                                 
049000       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I79401                 
049100       IF MSG-KDMFSFOR-1 = 2                                              
049200         MOVE 'GB '                TO W-IDSKYLT                           
049300       END-IF                                                             
049400     END-IF                                                               
049500                                                                          
049600     MOVE SPACE                TO WS-RAPP-LISTRAD                         
049700                                                                          
049800     MOVE LOW-VALUE              TO W-WDA3F1KY-MIN-X                      
049900     MOVE HIGH-VALUE             TO W-WDA3F1KY-MAX-X                      
050000                                                                          
050100     PERFORM AA-OPEN-PRINTER                                              
050200     .                                                                    
050300     EJECT                                                                
050400 AA-OPEN-PRINTER         SECTION.                                         
050500                                                                          
050600     MOVE MID-IDPRTLST        TO WS-RAPP-PRINTER                          
050601     IF WS-RAPP-PRINTER(4:3) = 'RT6'                                      
050602     OR WS-RAPP-PRINTER(4:3) = 'RT7'                                      
050610       MOVE '2'               TO PRT-FORMS-OVR                            
050620     END-IF                                                               
050700     CALL W006PRS1 USING PRT-SPOOL-OVR                                    
050800                         PRT-OPEN                                         
050900                         WS-RAPP-PRINTER                                  
051000                         ALT-PCB                                          
051100                         WS-DUMMY                                         
051200                         WS-DUMMY                                         
051300     .                                                                    
051400     EJECT                                                                
051500 B-SKAPA-RT-UTSKRIFT    SECTION.                                          
051600                                                                          
051700     MOVE +1                   TO INDX                                    
051800     PERFORM UNTIL INDX        >  MID-KVPOST                              
051900         MOVE ZERO             TO W-IDSIDNR                               
052000         PERFORM BA-BEHANDLA-RT                                           
052100         ADD +1                TO INDX                                    
052200     END-PERFORM                                                          
052300     .                                                                    
052400     EJECT                                                                
052500 BA-BEHANDLA-RT     SECTION.                                              
052600                                                                          
052700     MOVE LOW-VALUE            TO W-WDA3F1KY-MIN-X                        
052800     MOVE HIGH-VALUE           TO W-WDA3F1KY-MAX-X                        
052900                                                                          
053000     INSPECT MID-IDDISTR(INDX)  REPLACING LEADING SPACE BY ZERO           
053100     INSPECT MID-IDKUNDNR(INDX) REPLACING LEADING SPACE BY ZERO           
053200     INSPECT MID-IDRAPPNR(INDX) REPLACING LEADING SPACE BY ZERO           
053300                                                                          
053400     MOVE MID-IDDISTR(INDX)    TO W-IDDISTR-ANM                           
053500                                  W-IDDISTR-RET-MIN                       
053600                                  W-IDDISTR-RET-MAX                       
053700     MOVE MID-IDKUNDNR(INDX)   TO W-IDKUNDNR-ANM                          
053800                                  W-IDKUNDNR-RET-MIN                      
053900                                  W-IDKUNDNR-RET-MAX                      
054000     MOVE MID-IDRAPPNR(INDX)   TO W-IDRAPPNR-ANM                          
054100                                  W-IDRAPPNR-RET-MIN                      
054200                                  W-IDRAPPNR-RET-MAX                      
054300                                                                          
054400     MOVE MID-IDDC             TO W-IDDC                                  
054500                                  WS-IDDC                                 
054510     IF CDC                                                               
054511       MOVE 'LÅG V'             TO WS-LAGV-KDARTURS                       
054512       MOVE 'LOW V'             TO WE-LAGV-KDARTURS                       
054520     ELSE                                                                 
054521       MOVE 'URS'               TO WS-LAGV-KDARTURS                       
054522       MOVE 'ORG'               TO WE-LAGV-KDARTURS                       
054530     END-IF                                                               
054600                                                                          
054700     PERFORM IMS-GU-WLKREE01                                              
054800                                                                          
054900     IF SEGMENT-FINNS                                                     
055000                                                                          
055100        MOVE MID-IDDC                 TO W-IDDC-RET-MIN                   
055200                                         W-IDDC-RET-MAX                   
055300        PERFORM BAA-KOLLA-OM-FLERA-KOLLIN                                 
055400        PERFORM S01-SKAPA-HUVUD                                           
055500        PERFORM S03-LAES-WLKREE11                                         
055600                                                                          
055700        PERFORM UNTIL SEGMENT-SAKNAS                                      
055800            PERFORM BAB-SKAPA-RAD                                         
055900            PERFORM S03-LAES-WLKREE11                                     
056000        END-PERFORM                                                       
056100     END-IF                                                               
056200     .                                                                    
056300     EJECT                                                                
056400 BAA-KOLLA-OM-FLERA-KOLLIN       SECTION.                                 
056500                                                                          
056600     PERFORM IMS-GU-WLRETG01                                              
056700     IF SEGMENT-FINNS                                                     
056800        PERFORM IMS-GN-WLRETG01                                           
056900        IF SEGMENT-FINNS                                                  
057000          IF W-IDDC NOT = W-IDDC-B6                                       
057100             MOVE W-IDDC TO W-IDDC-B6                                     
057200             PERFORM IMS-GU-WDB601                                        
057300          END-IF                                                          
057400          IF DCS-NDC-NA OR DCS-NDC-PF OR                                  
057500            (DCS-SDC AND DCS-ENGLAND OR DCS-HOLLAND OR DCS-ITALY)         
057600             MOVE YES              TO W-FLFLER-KOLLI                      
057700          ELSE                                                            
057800             MOVE JA               TO W-FLFLER-KOLLI                      
057900          END-IF                                                          
058000        ELSE                                                              
058100           MOVE NEJ              TO W-FLFLER-KOLLI                        
058200        END-IF                                                            
058300     ELSE                                                                 
058400        MOVE NEJ                 TO W-FLFLER-KOLLI                        
058500     END-IF                                                               
058600                                                                          
058700     .                                                                    
058800     EJECT                                                                
058900 BAB-SKAPA-RAD       SECTION.                                             
059000                                                                          
059100     IF W-IDDC NOT = W-IDDC-B6                                            
059200        MOVE W-IDDC TO W-IDDC-B6                                          
059300        PERFORM IMS-GU-WDB601                                             
059400     END-IF                                                               
059500     PERFORM BABA-HAEMTA-ART-INFO                                         
059600                                                                          
059700     MOVE LEV-IDDC             TO LRAD-IDDC                               
059710                                  LRAD-E-IDDC                             
059800     MOVE LEV-IDORDNR7         TO LRAD-IDORDNR7                           
059810                                  LRAD-E-IDORDNR7                         
059900     MOVE LEV-IDARTNR          TO LRAD-IDARTNR                            
059910                                  LRAD-E-IDARTNR                          
060000     MOVE W-REKSIFFR           TO LRAD-REKSIFFR                           
060010                                  LRAD-E-REKSIFFR                         
060100     MOVE W-BEART              TO LRAD-BEART                              
060110                                  LRAD-E-BEART                            
060200*    MOVE LEV-KVLEVANM-BEKR    TO LRAD-KVLEVANM-BEKR                      
060210     MOVE LEV-KVLEVANM-BEKR    TO LRAD-E-KVLEVANM-BEKR                    
060300     COMPUTE W-KVLEVANM-KVAR   =  LEV-KVLEVANM-BEKR -                     
060400                                  LEV-KVRETINL -                          
060500                                  LEV-KVAVV-KVANT -                       
060600                                  LEV-KVRETINL-SKR -                      
060700                                  LEV-KVAVV-KVAL -                        
060800                                  LEV-KVANTAL-ILI                         
060900     MOVE W-KVLEVANM-KVAR      TO LRAD-KVLEVANM-KVAR                      
060910                                  LRAD-E-KVLEVANM-KVAR                    
061000                                                                          
061100     MOVE LEV-KDANMORS         TO LRAD-KDANMORS                           
061110                                  LRAD-E-KDANMORS                         
061200                                                                          
061300     IF DCS-NDC-NA AND DCS-USA                                            
061400       MOVE W-ADLAGOMR         TO LRAD-NA-ADLAGOMR                        
061410                                  LRAD-E-NA-ADLAGOMR                      
061500       MOVE W-ADGANG           TO LRAD-NA-ADGANG                          
061510                                  LRAD-E-NA-ADGANG                        
061600       MOVE W-ADPLATS          TO LRAD-NA-ADPLATS                         
061610                                  LRAD-E-NA-ADPLATS                       
061700     ELSE                                                                 
061800       MOVE W-ADLAGOMR         TO LRAD-ADLAGOMR                           
061810                                  LRAD-E-ADLAGOMR                         
061900       MOVE W-ADGANG           TO LRAD-ADGANG                             
061910                                  LRAD-E-ADGANG                           
062000       MOVE W-ADPLATS          TO LRAD-ADPLATS                            
062010                                  LRAD-E-ADPLATS                          
062100     END-IF                                                               
062200                                                                          
062300     MOVE W-KDERS              TO LRAD-KDERS                              
062310                                  LRAD-E-KDERS                            
062400                                                                          
062500     MOVE LEV-IDRADNR          TO W-IDRADNR                               
062600                                                                          
062610     PERFORM IMS-GU-WDD501                                                
062611     IF SEGMENT-FINNS                                                     
062620       MOVE ART-KDFGPRIO       TO LRAD-KDFGPRIO                           
062621                                  LRAD-E-KDFGPRIO                         
062630     ELSE                                                                 
062631       MOVE SPACE              TO LRAD-KDFGPRIO                           
062632                                  LRAD-E-KDFGPRIO                         
062650     END-IF                                                               
062660                                                                          
062700     PERFORM IMS-GNP-WLKREE21                                             
062800                                                                          
062900     IF W-KVRADER              >  MAX-KVRADER                             
063000         PERFORM S01-SKAPA-HUVUD                                          
063100         MOVE PRT-AFTER-1      TO PRT-RADSKIP                             
063200         ADD +1                TO W-KVRADER                               
063300      ELSE                                                                
063400         MOVE PRT-AFTER-2      TO PRT-RADSKIP                             
063500         ADD +2                TO W-KVRADER                               
063600     END-IF                                                               
063610                                                                          
063620     IF DCS-NDC-NA OR DCS-NDC-PF OR                                       
063630       (DCS-SDC AND (DCS-ENGLAND OR DCS-HOLLAND OR DCS-ITALY))            
063640       MOVE LIST-LRAD-ENG          TO WS-RAPP-RAD                         
063650     ELSE                                                                 
063660       MOVE LIST-LRAD              TO WS-RAPP-RAD                         
063670     END-IF                                                               
063671                                                                          
063690                                                                          
063800     PERFORM S02-SKRIV-RAD                                                
063900                                                                          
064000     IF SEGMENT-FINNS                                                     
064100       IF TXT-TEANMNOT-REG(1) = SPACE AND                                 
064200          TXT-TEANMNOT-REG(2) = SPACE AND                                 
064300          TXT-TEANMNOT-REG(3) = SPACE AND                                 
064400          TXT-TEANMNOT-ADM(1) = SPACE AND                                 
064500          TXT-TEANMNOT-ADM(2) = SPACE AND                                 
064600          TXT-TEANMNOT-ADM(3) = SPACE                                     
064700         CONTINUE                                                         
064800       ELSE                                                               
064900         IF W-KVRADER              >  MAX-KVRADER - 6                     
065000             PERFORM S01-SKAPA-HUVUD                                      
065100         END-IF                                                           
065200         IF TXT-TEANMNOT-REG(1) NOT  = SPACE                              
065300           MOVE 'REG'                TO TEXT-TYP                          
065400           MOVE TXT-TEANMNOT-REG(1)  TO TEXT-TEXT                         
065500           INSPECT TEXT-TEXT    REPLACING ALL '¤' BY 'U'                  
065600           IF DCS-NDC-NA OR DCS-NDC-PF OR                                 
065700             (DCS-SDC AND DCS-ENGLAND OR DCS-HOLLAND OR DCS-ITALY)        
065800             INSPECT TEXT-TEXT   REPLACING ALL 'Å' BY 'A'                 
065900             INSPECT TEXT-TEXT   REPLACING ALL 'Ä' BY 'A'                 
066000             INSPECT TEXT-TEXT   REPLACING ALL 'Ö' BY 'O'                 
066100           END-IF                                                         
066200           MOVE LIST-TEXT            TO WS-RAPP-RAD                       
066300           MOVE PRT-AFTER-1      TO PRT-RADSKIP                           
066400           ADD +1                TO W-KVRADER                             
066500           PERFORM S02-SKRIV-RAD                                          
066600         END-IF                                                           
066700         IF TXT-TEANMNOT-REG(2) NOT  = SPACE                              
066800           MOVE 'REG'                TO TEXT-TYP                          
066900           MOVE TXT-TEANMNOT-REG(2)  TO TEXT-TEXT                         
067000           INSPECT TEXT-TEXT    REPLACING ALL '¤' BY 'U'                  
067100           IF DCS-NDC-NA OR DCS-NDC-PF OR                                 
067200             (DCS-SDC AND DCS-ENGLAND OR DCS-HOLLAND OR DCS-ITALY)        
067300             INSPECT TEXT-TEXT   REPLACING ALL 'Å' BY 'A'                 
067400             INSPECT TEXT-TEXT   REPLACING ALL 'Ä' BY 'A'                 
067500             INSPECT TEXT-TEXT   REPLACING ALL 'Ö' BY 'O'                 
067600           END-IF                                                         
067700           MOVE LIST-TEXT            TO WS-RAPP-RAD                       
067800           MOVE PRT-AFTER-1      TO PRT-RADSKIP                           
067900           ADD +1                TO W-KVRADER                             
068000           PERFORM S02-SKRIV-RAD                                          
068100         END-IF                                                           
068200         IF TXT-TEANMNOT-REG(3) NOT  = SPACE                              
068300           MOVE 'REG'                TO TEXT-TYP                          
068400           MOVE TXT-TEANMNOT-REG(3)  TO TEXT-TEXT                         
068500           INSPECT TEXT-TEXT    REPLACING ALL '¤' BY 'U'                  
068600           IF DCS-NDC-NA OR DCS-NDC-PF OR                                 
068700             (DCS-SDC AND DCS-ENGLAND OR DCS-HOLLAND OR DCS-ITALY)        
068800             INSPECT TEXT-TEXT   REPLACING ALL 'Å' BY 'A'                 
068900             INSPECT TEXT-TEXT   REPLACING ALL 'Ä' BY 'A'                 
069000             INSPECT TEXT-TEXT   REPLACING ALL 'Ö' BY 'O'                 
069100           END-IF                                                         
069200           MOVE LIST-TEXT            TO WS-RAPP-RAD                       
069300           MOVE PRT-AFTER-1      TO PRT-RADSKIP                           
069400           ADD +1                TO W-KVRADER                             
069500           PERFORM S02-SKRIV-RAD                                          
069600         END-IF                                                           
069700         IF TXT-TEANMNOT-ADM(1) NOT  = SPACE                              
069800           MOVE 'ADM'                TO TEXT-TYP                          
069900           MOVE TXT-TEANMNOT-ADM(1)  TO TEXT-TEXT                         
070000           INSPECT TEXT-TEXT    REPLACING ALL '¤' BY 'U'                  
070100           IF DCS-NDC-NA OR DCS-NDC-PF OR                                 
070200             (DCS-SDC AND DCS-ENGLAND OR DCS-HOLLAND OR DCS-ITALY)        
070300             INSPECT TEXT-TEXT   REPLACING ALL 'Å' BY 'A'                 
070400             INSPECT TEXT-TEXT   REPLACING ALL 'Ä' BY 'A'                 
070500             INSPECT TEXT-TEXT   REPLACING ALL 'Ö' BY 'O'                 
070600           END-IF                                                         
070700           MOVE LIST-TEXT            TO WS-RAPP-RAD                       
070800           MOVE PRT-AFTER-1      TO PRT-RADSKIP                           
070900           ADD +1                TO W-KVRADER                             
071000           PERFORM S02-SKRIV-RAD                                          
071100         END-IF                                                           
071200         IF TXT-TEANMNOT-ADM(2) NOT  = SPACE                              
071300           MOVE 'ADM'                TO TEXT-TYP                          
071400           MOVE TXT-TEANMNOT-ADM(2)  TO TEXT-TEXT                         
071500           INSPECT TEXT-TEXT    REPLACING ALL '¤' BY 'U'                  
071600           IF DCS-NDC-NA OR DCS-NDC-PF OR                                 
071700             (DCS-SDC AND DCS-ENGLAND OR DCS-HOLLAND OR DCS-ITALY)        
071800             INSPECT TEXT-TEXT   REPLACING ALL 'Å' BY 'A'                 
071900             INSPECT TEXT-TEXT   REPLACING ALL 'Ä' BY 'A'                 
072000             INSPECT TEXT-TEXT   REPLACING ALL 'Ö' BY 'O'                 
072100           END-IF                                                         
072200           MOVE LIST-TEXT            TO WS-RAPP-RAD                       
072300           MOVE PRT-AFTER-1      TO PRT-RADSKIP                           
072400           ADD +1                TO W-KVRADER                             
072500           PERFORM S02-SKRIV-RAD                                          
072600         END-IF                                                           
072700         IF TXT-TEANMNOT-ADM(3) NOT  = SPACE                              
072800           MOVE 'ADM'                TO TEXT-TYP                          
072900           MOVE TXT-TEANMNOT-ADM(3)  TO TEXT-TEXT                         
073000           INSPECT TEXT-TEXT    REPLACING ALL '¤' BY 'U'                  
073100           IF DCS-NDC-NA OR DCS-NDC-PF OR                                 
073200             (DCS-SDC AND DCS-ENGLAND OR DCS-HOLLAND OR DCS-ITALY)        
073300             INSPECT TEXT-TEXT   REPLACING ALL 'Å' BY 'A'                 
073400             INSPECT TEXT-TEXT   REPLACING ALL 'Ä' BY 'A'                 
073500             INSPECT TEXT-TEXT   REPLACING ALL 'Ö' BY 'O'                 
073600           END-IF                                                         
073700           MOVE LIST-TEXT            TO WS-RAPP-RAD                       
073800           MOVE PRT-AFTER-1      TO PRT-RADSKIP                           
073900           ADD +1                TO W-KVRADER                             
074000           PERFORM S02-SKRIV-RAD                                          
074100         END-IF                                                           
074200       END-IF                                                             
074300     END-IF                                                               
074400*** NY KOD                                                                
074500                                                                          
074600     PERFORM IMS-GU-W6KVAH11                                              
074700     IF SEGMENT-FINNS                                                     
074800       IF W-KVRADER              >  MAX-KVRADER - 3                       
074900           PERFORM S01-SKAPA-HUVUD                                        
075000       END-IF                                                             
075100                                                                          
075200       IF LDC AND LEV-KDANMORS = 72 AND INFO-KDKVAINF = 'R'               
075300         MOVE NEJ TO RNOT-SW                                              
075400       ELSE                                                               
075500         MOVE JA TO RNOT-SW                                               
075600       END-IF                                                             
075700       IF SKRIV-RNOT                                                      
075800         IF INFO-TEKVAINF-EXT(1) NOT = SPACE                              
075900           IF W-IDSKYLT = 'GB'                                            
076000             MOVE 'QUA'            TO TEXT-TYP                            
076100           ELSE                                                           
076200             MOVE 'KVA'            TO TEXT-TYP                            
076300           END-IF                                                         
076400           MOVE INFO-TEKVAINF-EXT(1) TO TEXT-TEXT                         
076500           INSPECT TEXT-TEXT     REPLACING ALL '#' BY ' '                 
076600           IF DCS-NDC-NA OR DCS-NDC-PF OR                                 
076700             (DCS-SDC AND DCS-ENGLAND OR DCS-HOLLAND OR DCS-ITALY)        
076800             INSPECT TEXT-TEXT   REPLACING ALL 'Å' BY 'A'                 
076900             INSPECT TEXT-TEXT   REPLACING ALL 'Ä' BY 'A'                 
077000             INSPECT TEXT-TEXT   REPLACING ALL 'Ö' BY 'O'                 
077100           END-IF                                                         
077200           MOVE LIST-TEXT        TO WS-RAPP-RAD                           
077300           MOVE PRT-AFTER-1      TO PRT-RADSKIP                           
077400           ADD +1                TO W-KVRADER                             
077500           PERFORM S02-SKRIV-RAD                                          
077600         END-IF                                                           
077700         IF INFO-TEKVAINF-EXT(2) NOT = SPACE                              
077800           IF W-IDSKYLT = 'GB'                                            
077900             MOVE 'QUA'            TO TEXT-TYP                            
078000           ELSE                                                           
078100             MOVE 'KVA'            TO TEXT-TYP                            
078200           END-IF                                                         
078300           MOVE INFO-TEKVAINF-EXT(2) TO TEXT-TEXT                         
078400           INSPECT TEXT-TEXT     REPLACING ALL '#' BY ' '                 
078500           IF DCS-NDC-NA OR DCS-NDC-PF OR                                 
078600             (DCS-SDC AND DCS-ENGLAND OR DCS-HOLLAND OR DCS-ITALY)        
078700             INSPECT TEXT-TEXT   REPLACING ALL 'Å' BY 'A'                 
078800             INSPECT TEXT-TEXT   REPLACING ALL 'Ä' BY 'A'                 
078900             INSPECT TEXT-TEXT   REPLACING ALL 'Ö' BY 'O'                 
079000           END-IF                                                         
079100           MOVE LIST-TEXT        TO WS-RAPP-RAD                           
079200           MOVE PRT-AFTER-1      TO PRT-RADSKIP                           
079300           ADD +1                TO W-KVRADER                             
079400           PERFORM S02-SKRIV-RAD                                          
079500         END-IF                                                           
079600         IF INFO-TEKVAINF-EXT(3) NOT = SPACE                              
079700           IF W-IDSKYLT = 'GB'                                            
079800             MOVE 'QUA'            TO TEXT-TYP                            
079900           ELSE                                                           
080000             MOVE 'KVA'            TO TEXT-TYP                            
080100           END-IF                                                         
080200           MOVE INFO-TEKVAINF-EXT(3) TO TEXT-TEXT                         
080300           INSPECT TEXT-TEXT     REPLACING ALL '#' BY ' '                 
080400           IF DCS-NDC-NA OR DCS-NDC-PF OR                                 
080500             (DCS-SDC AND DCS-ENGLAND OR DCS-HOLLAND OR DCS-ITALY)        
080600             INSPECT TEXT-TEXT   REPLACING ALL 'Å' BY 'A'                 
080700             INSPECT TEXT-TEXT   REPLACING ALL 'Ä' BY 'A'                 
080800             INSPECT TEXT-TEXT   REPLACING ALL 'Ö' BY 'O'                 
080900           END-IF                                                         
081000           MOVE LIST-TEXT        TO WS-RAPP-RAD                           
081100           MOVE PRT-AFTER-1      TO PRT-RADSKIP                           
081200           ADD +1                TO W-KVRADER                             
081300           PERFORM S02-SKRIV-RAD                                          
081400         END-IF                                                           
081500         IF INFO-TEKVAINF-EXT(4) NOT = SPACE                              
081600           IF W-IDSKYLT = 'GB'                                            
081700             MOVE 'QUA'            TO TEXT-TYP                            
081800           ELSE                                                           
081900             MOVE 'KVA'            TO TEXT-TYP                            
082000           END-IF                                                         
082100           MOVE INFO-TEKVAINF-EXT(4) TO TEXT-TEXT                         
082200           INSPECT TEXT-TEXT     REPLACING ALL '#' BY ' '                 
082300           IF DCS-NDC-NA OR DCS-NDC-PF OR                                 
082400             (DCS-SDC AND DCS-ENGLAND OR DCS-HOLLAND OR DCS-ITALY)        
082500             INSPECT TEXT-TEXT   REPLACING ALL 'Å' BY 'A'                 
082600             INSPECT TEXT-TEXT   REPLACING ALL 'Ä' BY 'A'                 
082700             INSPECT TEXT-TEXT   REPLACING ALL 'Ö' BY 'O'                 
082800           END-IF                                                         
082900           MOVE LIST-TEXT        TO WS-RAPP-RAD                           
083000           MOVE PRT-AFTER-1      TO PRT-RADSKIP                           
083100           ADD +1                TO W-KVRADER                             
083200           PERFORM S02-SKRIV-RAD                                          
083300         END-IF                                                           
083400         IF INFO-TEKVAINF-EXT(5) NOT = SPACE                              
083500           IF W-IDSKYLT = 'GB'                                            
083600             MOVE 'QUA'            TO TEXT-TYP                            
083700           ELSE                                                           
083800             MOVE 'KVA'            TO TEXT-TYP                            
083900           END-IF                                                         
084000           MOVE INFO-TEKVAINF-EXT(5) TO TEXT-TEXT                         
084100           INSPECT TEXT-TEXT     REPLACING ALL '#' BY ' '                 
084200           IF DCS-NDC-NA OR DCS-NDC-PF OR                                 
084300             (DCS-SDC AND DCS-ENGLAND OR DCS-HOLLAND OR DCS-ITALY)        
084400             INSPECT TEXT-TEXT   REPLACING ALL 'Å' BY 'A'                 
084500             INSPECT TEXT-TEXT   REPLACING ALL 'Ä' BY 'A'                 
084600             INSPECT TEXT-TEXT   REPLACING ALL 'Ö' BY 'O'                 
084700           END-IF                                                         
084800           MOVE LIST-TEXT        TO WS-RAPP-RAD                           
084900           MOVE PRT-AFTER-1      TO PRT-RADSKIP                           
085000           ADD +1                TO W-KVRADER                             
085100           PERFORM S02-SKRIV-RAD                                          
085200         END-IF                                                           
085300         IF INFO-TEKVAINF-EXT(6) NOT = SPACE                              
085400           IF W-IDSKYLT = 'GB'                                            
085500             MOVE 'QUA'            TO TEXT-TYP                            
085600           ELSE                                                           
085700             MOVE 'KVA'            TO TEXT-TYP                            
085800           END-IF                                                         
085900           MOVE INFO-TEKVAINF-EXT(6) TO TEXT-TEXT                         
086000           INSPECT TEXT-TEXT     REPLACING ALL '#' BY ' '                 
086100           IF DCS-NDC-NA OR DCS-NDC-PF OR                                 
086200             (DCS-SDC AND DCS-ENGLAND OR DCS-HOLLAND OR DCS-ITALY)        
086300             INSPECT TEXT-TEXT   REPLACING ALL 'Å' BY 'A'                 
086400             INSPECT TEXT-TEXT   REPLACING ALL 'Ä' BY 'A'                 
086500             INSPECT TEXT-TEXT   REPLACING ALL 'Ö' BY 'O'                 
086600           END-IF                                                         
086700           MOVE LIST-TEXT        TO WS-RAPP-RAD                           
086800           MOVE PRT-AFTER-1      TO PRT-RADSKIP                           
086900           ADD +1                TO W-KVRADER                             
087000           PERFORM S02-SKRIV-RAD                                          
087100         END-IF                                                           
087200         IF INFO-TEKVAINF-EXT(7) NOT = SPACE                              
087300           IF W-IDSKYLT = 'GB'                                            
087400             MOVE 'QUA'            TO TEXT-TYP                            
087500           ELSE                                                           
087600             MOVE 'KVA'            TO TEXT-TYP                            
087700           END-IF                                                         
087800           MOVE INFO-TEKVAINF-EXT(7) TO TEXT-TEXT                         
087900           INSPECT TEXT-TEXT     REPLACING ALL '#' BY ' '                 
088000           IF DCS-NDC-NA OR DCS-NDC-PF OR                                 
088100             (DCS-SDC AND DCS-ENGLAND OR DCS-HOLLAND OR DCS-ITALY)        
088200             INSPECT TEXT-TEXT   REPLACING ALL 'Å' BY 'A'                 
088300             INSPECT TEXT-TEXT   REPLACING ALL 'Ä' BY 'A'                 
088400             INSPECT TEXT-TEXT   REPLACING ALL 'Ö' BY 'O'                 
088500           END-IF                                                         
088600           MOVE LIST-TEXT        TO WS-RAPP-RAD                           
088700           MOVE PRT-AFTER-1      TO PRT-RADSKIP                           
088800           ADD +1                TO W-KVRADER                             
088900           PERFORM S02-SKRIV-RAD                                          
089000         END-IF                                                           
089100       END-IF                                                             
089200     END-IF                                                               
089300     .                                                                    
089400     EJECT                                                                
089500                                                                          
089600 BABA-HAEMTA-ART-INFO             SECTION.                                
089700                                                                          
089800     MOVE LEV-IDARTNR          TO W-IDARTNR                               
089900     MOVE LEV-IDDC-RET         TO W-IDDC-X                                
090000                                  WS-IDDC-LEV                             
090100     IF WS-IDDC-LEV NOT = W-IDDC-B6                                       
090200        MOVE WS-IDDC-LEV TO W-IDDC-B6                                     
090300        PERFORM IMS-GU-WDB601                                             
090400     END-IF                                                               
090500     MOVE SPACE                TO LRAD-LAGV-KDARTURS                      
090510                                  LRAD-E-LAGV-KDARTURS                    
090600     IF DCS-NDC-NA OR DCS-NDC-PF OR                                       
090700       (DCS-SDC AND (DCS-ENGLAND OR DCS-HOLLAND OR                        
090710                     DCS-ITALY   OR DCS-SWEDEN))                          
090800       PERFORM IMS-GU-WLARTC01                                            
090900       IF SEGMENT-FINNS                                                   
091000          MOVE ART-KDSORT        TO LRAD-KDSORT                           
091001                                    LRAD-E-KDSORT                         
091010          MOVE ART-REKSIFFR      TO W-REKSIFFR                            
091100          PERFORM IMS-GNP-WLARTC11                                        
091200          IF SEGMENT-FINNS                                                
091300            MOVE CLAG-KDERS     TO W-KDERS                                
091400            MOVE CLAG-KDARTURS  TO LRAD-LAGV-KDARTURS                     
091410                                   LRAD-E-LAGV-KDARTURS                   
091500          ELSE                                                            
091600            MOVE ZERO           TO W-KDERS                                
091700            MOVE SPACE          TO LRAD-LAGV-KDARTURS                     
091710                                   LRAD-E-LAGV-KDARTURS                   
091800          END-IF                                                          
091801                                                                          
091900       ELSE                                                               
092000          MOVE ZERO              TO W-REKSIFFR                            
092001          MOVE SPACE             TO LRAD-KDSORT                           
092002                                    LRAD-E-KDSORT                         
092100       END-IF                                                             
092200       PERFORM IMS-GU-WLARTS11                                            
092300       IF SEGMENT-FINNS                                                   
092400          MOVE SLAG-ADLAGOMR  TO W-ADLAGOMR                               
092500          MOVE SLAG-ADGANG    TO W-ADGANG                                 
092600          MOVE SLAG-ADPLATS   TO W-ADPLATS                                
092800       ELSE                                                               
092900          MOVE ZERO           TO W-ADLAGOMR                               
093000                                 W-ADGANG                                 
093100                                 W-ADPLATS                                
093200                                 W-KDERS                                  
093400       END-IF                                                             
093401                                                                          
093402       PERFORM BABAA-HAEMTA-EMB-INFO                                      
093403                                                                          
093500     ELSE                                                                 
093600       PERFORM IMS-GU-WLARTC01                                            
093700       IF SEGMENT-FINNS                                                   
093710          MOVE ART-KDSORT        TO LRAD-KDSORT                           
093720                                    LRAD-E-KDSORT                         
093800          MOVE ART-REKSIFFR      TO W-REKSIFFR                            
093900          PERFORM IMS-GNP-WLARTC11                                        
094000          IF SEGMENT-FINNS                                                
094100             MOVE CLAG-ADLAGOMR  TO W-ADLAGOMR                            
094200             MOVE CLAG-ADGANG    TO W-ADGANG                              
094300             MOVE CLAG-ADPLATS   TO W-ADPLATS                             
094400             MOVE CLAG-KDERS     TO W-KDERS                               
094410             IF CLAG-PRARTSTD < 50                                        
094420               MOVE 'J'          TO LRAD-LAGV-KDARTURS                    
094421                                    LRAD-E-LAGV-KDARTURS                  
094422             ELSE                                                         
094423               MOVE SPACE        TO LRAD-LAGV-KDARTURS                    
094424                                    LRAD-E-LAGV-KDARTURS                  
094430             END-IF                                                       
094600          ELSE                                                            
094700             MOVE ZERO           TO W-ADLAGOMR                            
094800                                    W-ADGANG                              
094900                                    W-ADPLATS                             
095000                                    W-KDERS                               
095100             MOVE SPACE          TO LRAD-LAGV-KDARTURS                    
095110                                    LRAD-E-LAGV-KDARTURS                  
095200          END-IF                                                          
095300       ELSE                                                               
095400          MOVE ZERO              TO W-ADLAGOMR                            
095500                                    W-ADGANG                              
095600                                    W-ADPLATS                             
095700                                    W-KDERS                               
095800          MOVE SPACE             TO LRAD-LAGV-KDARTURS                    
095810                                    LRAD-E-LAGV-KDARTURS                  
095820          MOVE SPACE             TO LRAD-KDSORT                           
095830                                    LRAD-E-KDSORT                         
095900                                                                          
096000       END-IF                                                             
096002       PERFORM BABAA-HAEMTA-EMB-INFO                                      
096110     END-IF                                                               
096230                                                                          
096300     PERFORM IMS-GU-WLBENA11                                              
096400     IF SEGMENT-FINNS                                                     
096500        MOVE TEXT-BEART        TO W-BEART                                 
096600     ELSE                                                                 
096700        MOVE SPACE             TO W-BEART                                 
096800     END-IF                                                               
096900                                                                          
096910     .                                                                    
096920     EJECT                                                                
096930                                                                          
096940 BABAA-HAEMTA-EMB-INFO             SECTION.                               
096950                                                                          
096960     MOVE 'Q0' TO W-KDEMBAL                                               
096970     PERFORM IMS-GU-WDK613                                                
096980     IF SEGMENT-FINNS                                                     
096990       IF EMB-IDARTNR-EMB > ZERO                                          
096991         MOVE EMB-IDARTNR-EMB TO LRAD-IDARTNR-EMB                         
096992       ELSE                                                               
096993         MOVE 'Q1' TO W-KDEMBAL                                           
096994         PERFORM IMS-GU-WDK613                                            
096995         IF SEGMENT-FINNS                                                 
096996           MOVE EMB-IDARTNR-EMB TO LRAD-IDARTNR-EMB                       
096997         ELSE                                                             
096998           MOVE ZERO            TO LRAD-IDARTNR-EMB                       
096999         END-IF                                                           
097000       END-IF                                                             
097001     ELSE                                                                 
097002       MOVE 'Q1' TO W-KDEMBAL                                             
097003       PERFORM IMS-GU-WDK613                                              
097004       IF SEGMENT-FINNS                                                   
097005         MOVE EMB-IDARTNR-EMB TO LRAD-IDARTNR-EMB                         
097006       ELSE                                                               
097007         MOVE ZERO              TO LRAD-IDARTNR-EMB                       
097008       END-IF                                                             
097009     END-IF                                                               
097010     .                                                                    
097100     EJECT                                                                
097200                                                                          
097300 Z-FINIT                   SECTION.                                       
097400                                                                          
097500     CALL W006PRS1 USING PRT-SPOOL-OVR                                    
097600                         PRT-CLOSE                                        
097700                         WS-RAPP-PRINTER                                  
097800                         ALT-PCB                                          
097900                         WS-DUMMY                                         
098000                         WS-DUMMY                                         
098100     .                                                                    
098200     EJECT                                                                
098300 S01-SKAPA-HUVUD     SECTION.                                             
098400                                                                          
098500     COMPUTE W-IDSIDNR            =  W-IDSIDNR + 1                        
098600     MOVE W-IDSIDNR               TO HRAD1-IDSIDNR                        
098700                                     HRAD1-IDSIDNR-ENG                    
098800     ACCEPT HRAD1-DATUM           FROM DATE                               
098900     ACCEPT HRAD1-DATUM-ENG       FROM DATE                               
099000                                                                          
099100     MOVE PRT-NYSIDA-RAD3         TO PRT-RADSKIP                          
099200     IF W-IDDC NOT = W-IDDC-B6                                            
099300        MOVE W-IDDC TO W-IDDC-B6                                          
099400        PERFORM IMS-GU-WDB601                                             
099500     END-IF                                                               
099600     IF DCS-NDC-NA OR DCS-NDC-PF OR                                       
099700       (DCS-SDC AND (DCS-ENGLAND OR DCS-HOLLAND OR DCS-ITALY))            
099800       MOVE LIST-HRAD1-ENG          TO WS-RAPP-RAD                        
099900     ELSE                                                                 
100000       MOVE LIST-HRAD1              TO WS-RAPP-RAD                        
100100     END-IF                                                               
100200     PERFORM S02-SKRIV-RAD                                                
100300                                                                          
100400     MOVE ANM-IDDISTR             TO HRAD3-IDDISTR                        
100500     MOVE ANM-IDKUNDNR            TO HRAD3-IDKUNDNR                       
100600     MOVE ANM-IDRAPPNR            TO HRAD3-IDRAPPNR                       
100700     MOVE W-FLFLER-KOLLI          TO HRAD3-FLFLER-KOLLI                   
100800     MOVE PRT-AFTER-2             TO PRT-RADSKIP                          
100900     IF DCS-NDC-NA OR DCS-NDC-PF OR                                       
101000       (DCS-SDC AND (DCS-ENGLAND OR DCS-HOLLAND OR DCS-ITALY))            
101100       MOVE LIST-HRAD2-ENG          TO WS-RAPP-RAD                        
101200     ELSE                                                                 
101300       MOVE LIST-HRAD2              TO WS-RAPP-RAD                        
101400     END-IF                                                               
101500     PERFORM S02-SKRIV-RAD                                                
101600                                                                          
101700     MOVE PRT-AFTER-1             TO PRT-RADSKIP                          
101800     MOVE LIST-HRAD3              TO WS-RAPP-RAD                          
101900     PERFORM S02-SKRIV-RAD                                                
102000                                                                          
102100     MOVE PRT-AFTER-2             TO PRT-RADSKIP                          
102200     IF DCS-NDC-NA OR DCS-NDC-PF OR                                       
102300       (DCS-SDC AND (DCS-ENGLAND OR DCS-HOLLAND OR DCS-ITALY))            
102400       MOVE LIST-HRAD4-ENG          TO WS-RAPP-RAD                        
102410       PERFORM S02-SKRIV-RAD                                              
102700     END-IF                                                               
102900                                                                          
103000     MOVE PRT-AFTER-1             TO PRT-RADSKIP                          
103100     IF DCS-NDC-NA OR DCS-NDC-PF OR                                       
103200       (DCS-SDC AND (DCS-ENGLAND OR DCS-HOLLAND OR DCS-ITALY))            
103300       MOVE LIST-HRAD5A-ENG         TO WS-RAPP-RAD                        
103301       PERFORM S02-SKRIV-RAD                                              
103310       MOVE LIST-HRAD5-ENG          TO WS-RAPP-RAD                        
103320       PERFORM S02-SKRIV-RAD                                              
103400     ELSE                                                                 
103500       MOVE LIST-HRAD5A             TO WS-RAPP-RAD                        
103501       PERFORM S02-SKRIV-RAD                                              
103510       MOVE LIST-HRAD5              TO WS-RAPP-RAD                        
103520       PERFORM S02-SKRIV-RAD                                              
103600     END-IF                                                               
103800                                                                          
103900     MOVE +11                     TO W-KVRADER                            
104000     .                                                                    
104100     EJECT                                                                
104200 S02-SKRIV-RAD SECTION.                                                   
104300                                                                          
104301     IF WS-RAPP-PRINTER(4:3) = 'RT6'                                      
104302     OR WS-RAPP-PRINTER(4:3) = 'RT7'                                      
104303       MOVE '2'               TO PRT-FORMS-OVR                            
104304     END-IF                                                               
104400     CALL W006PRS1 USING PRT-SPOOL-OVR                                    
104500                         PRT-WRITE                                        
104600                         WS-RAPP-PRINTER                                  
104700                         ALT-PCB                                          
104800                         PRT-RADSKIP                                      
104900                         WS-RAPP-LISTRAD                                  
105000                                                                          
105100     MOVE SPACE                TO WS-RAPP-LISTRAD                         
105200     .                                                                    
105300     EJECT                                                                
105400                                                                          
105500 S03-LAES-WLKREE11        SECTION.                                        
105600                                                                          
105700     MOVE NEJ                    TO OKOD-FL-RETILL                        
105800                                    OKOD-FL-INTERNUPPACKNING              
105900     PERFORM IMS-GNP-WLKREE11                                             
106000     PERFORM UNTIL OKOD-FL-RETILL = 'J' OR SEGMENT-SAKNAS                 
106100                OR OKOD-FL-INTERNUPPACKNING = 'J'                         
106200        IF LEV-KDKREBEH(1:1) = 'Y'   OR                                   
106300           LEV-KDKREBEH(1:1) = 'J'   OR                                   
106400           LEV-KDKREBEH(1:1) = 'C'   OR                                   
106500           LEV-KDKREBEH      = 'D01' OR                                   
106600           LEV-KDKREBEH      = 'D02' OR                                   
106700           LEV-KDKREBEH      = 'D03'                                      
106800           MOVE LEV-KDANMORS TO OKOD-KDANMORS                             
106900*--ANROPA KONTROLL AV ORSAKSKODER                                         
107000           CALL W418OKOD USING OKOD-W418OKOD                              
107100        END-IF                                                            
107200        IF OKOD-FL-RETILL = 'J' OR                                        
107300           OKOD-FL-INTERNUPPACKNING = 'J'                                 
107400           CONTINUE                                                       
107500        ELSE                                                              
107600          PERFORM IMS-GNP-WLKREE11                                        
107700        END-IF                                                            
107800     END-PERFORM                                                          
107900                                                                          
108000     .                                                                    
108100     EJECT                                                                
108200* --- IMS SEKTIONER ---                                                   
108300     SKIP3                                                                
108400 IMS-GET-MSG SECTION.                                                     
108500                                                                          
108600     MOVE '  QC' TO GODK-STATUSKODER                                      
108700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
108800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
108900     PERFORM IMS-STATUSKONTROLL                                           
109000     .                                                                    
109100     SKIP3                                                                
109200 IMS-GU-WLKREE01    SECTION.                                              
109300                                                                          
109400     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
109500          DELIMITED BY SIZE INTO SSA1                                     
109600     MOVE '  GE' TO GODK-STATUSKODER                                      
109700     CALL CBLTDLI USING GU KREE-PCB DLI-IO-AREA1 SSA1                     
109800     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
109900     PERFORM IMS-STATUSKONTROLL                                           
110000     .                                                                    
110100     SKIP2                                                                
110200 IMS-GNP-WLKREE11    SECTION.                                             
110300                                                                          
110400     MOVE 'WLKREE11'        TO SSA1                                       
110500     MOVE '  GE' TO GODK-STATUSKODER                                      
110600     CALL CBLTDLI USING GNP KREE-PCB DLI-IO-AREA2 SSA1                    
110700     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
110800     PERFORM IMS-STATUSKONTROLL                                           
110900     .                                                                    
111000     SKIP2                                                                
111100 IMS-GNP-WLKREE21    SECTION.                                             
111200                                                                          
111300     STRING 'WLKREE11(WDA211KY =' W-WDA211KY-X ')'                        
111400          DELIMITED BY SIZE INTO SSA1                                     
111500     MOVE 'WLKREE21'        TO SSA2                                       
111600     MOVE '  GE' TO GODK-STATUSKODER                                      
111700     CALL CBLTDLI USING GNP KREE-PCB DLI-IO-AREA3 SSA1 SSA2               
111800     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
111900     PERFORM IMS-STATUSKONTROLL                                           
112000     .                                                                    
112100     SKIP2                                                                
112200 IMS-GU-WLRETG01    SECTION.                                              
112300                                                                          
112400     STRING 'WLRETG01(WDA3F1KY>=' W-WDA3F1KY-MIN-X                        
112500                    '&WDA3F1KY<=' W-WDA3F1KY-MAX-X ')'                    
112600          DELIMITED BY SIZE INTO SSA1                                     
112700     MOVE '  GE' TO GODK-STATUSKODER                                      
112800     CALL CBLTDLI USING GU RETG-PCB DLI-IO-AREA3 SSA1                     
112900     MOVE RETG-STATUS-CODE TO STATUS-WS                                   
113000     PERFORM IMS-STATUSKONTROLL                                           
113100     .                                                                    
113200     SKIP2                                                                
113300 IMS-GN-WLRETG01    SECTION.                                              
113400                                                                          
113500     STRING 'WLRETG01(WDA3F1KY>=' W-WDA3F1KY-MIN-X                        
113600                    '&WDA3F1KY<=' W-WDA3F1KY-MAX-X ')'                    
113700          DELIMITED BY SIZE INTO SSA1                                     
113800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
113900     CALL CBLTDLI USING GN RETG-PCB DLI-IO-AREA3 SSA1                     
114000     MOVE RETG-STATUS-CODE TO STATUS-WS                                   
114100     PERFORM IMS-STATUSKONTROLL                                           
114200     .                                                                    
114300     SKIP2                                                                
114400 IMS-GU-WLARTC01     SECTION.                                             
114500                                                                          
114600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
114700          DELIMITED BY SIZE INTO SSA1                                     
114800     MOVE '  GE' TO GODK-STATUSKODER                                      
114900     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA3 SSA1                     
115000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
115100     PERFORM IMS-STATUSKONTROLL                                           
115200     .                                                                    
115300                                                                          
115400 IMS-GNP-WLARTC11     SECTION.                                            
115500                                                                          
115600     MOVE 'WLARTC11'   TO  SSA1                                           
115700     MOVE '  GE' TO GODK-STATUSKODER                                      
115800     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA3 SSA1                    
115900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
116000     PERFORM IMS-STATUSKONTROLL                                           
116100     .                                                                    
116200                                                                          
116300 IMS-GU-WLARTS11     SECTION.                                             
116400                                                                          
116500     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
116600          DELIMITED BY SIZE INTO SSA1                                     
116700     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
116800          DELIMITED BY SIZE INTO SSA2                                     
116900     MOVE '  GE' TO GODK-STATUSKODER                                      
117000     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA3 SSA1 SSA2                
117100     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
117200     PERFORM IMS-STATUSKONTROLL                                           
117300     .                                                                    
117400                                                                          
117500 IMS-GU-WLBENA11     SECTION.                                             
117600                                                                          
117700     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
117800            DELIMITED BY SIZE INTO SSA1                                   
117900     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
118000            DELIMITED BY SIZE INTO SSA2                                   
118100     MOVE '  ' TO GODK-STATUSKODER                                        
118200     CALL CBLTDLI USING GU  BENA-PCB DLI-IO-AREA3 SSA1 SSA2               
118300     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
118400     PERFORM IMS-STATUSKONTROLL                                           
118500     .                                                                    
118600     EJECT                                                                
118700 IMS-GU-W6KVAH11     SECTION.                                             
118800                                                                          
118900     STRING 'W6KVAH01(IDARTNR  =' W-IDARTNR-X ')'                         
119000            DELIMITED BY SIZE INTO SSA1                                   
119100     STRING 'W6KVAH11(KDKVAINF =' W-KDKVAINF-X ')'                        
119200            DELIMITED BY SIZE INTO SSA2                                   
119300     MOVE '  GE' TO GODK-STATUSKODER                                      
119400     CALL CBLTDLI USING GU  KVAH-PCB DLI-IO-AREA3 SSA1 SSA2               
119500     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
119600     PERFORM IMS-STATUSKONTROLL                                           
119700     .                                                                    
119800     EJECT                                                                
119900 IMS-GU-WDB601    SECTION.                                                
120000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
120100          DELIMITED BY SIZE INTO SSA1                                     
120200     MOVE '  GE' TO GODK-STATUSKODER                                      
120300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
120400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
120500     PERFORM IMS-STATUSKONTROLL                                           
120600     IF SEGMENT-SAKNAS                                                    
120700        MOVE SPACE TO DCS-KDDC                                            
120800     END-IF                                                               
120900     .                                                                    
120910 IMS-GU-WDK613   SECTION.                                                 
120920                                                                          
120923                                                                          
120924     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
120925          DELIMITED BY SIZE INTO SSA1                                     
120930     STRING 'WDK613  (KDEMBAL  =' W-KDEMBAL-X ')'                         
120940          DELIMITED BY SIZE INTO SSA2                                     
120950     MOVE '  GE' TO GODK-STATUSKODER                                      
120960     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-K613 SSA1 SSA2            
120970     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
120980     PERFORM IMS-STATUSKONTROLL                                           
120990     .                                                                    
120991 IMS-GU-WDD501 SECTION.                                                   
120992     STRING 'WDD501  (IDARTNR  =' W-IDARTNR-X ')'                         
120993          DELIMITED BY SIZE INTO SSA1                                     
120994     MOVE '  GE' TO GODK-STATUSKODER                                      
120995     CALL CBLTDLI USING GU WDD5-PCB DLI-IO-AREA-D501 SSA1                 
120996     MOVE WDD5-STATUS-CODE TO STATUS-WS                                   
120997     PERFORM IMS-STATUSKONTROLL                                           
120998     .                                                                    
121000 IMS-STATUSKONTROLL SECTION.                                              
121100                                                                          
121200     SET STATUS-IX TO 1                                                   
121300     SEARCH GODK-STATUS                                                   
121400       AT END                                                             
121500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
121600         DELIMITED BY SIZE INTO FELTEXT                                   
121700         CALL FELLOG                                                      
121800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
121900         CONTINUE                                                         
122000     END-SEARCH                                                           
122100     .                                                                    
