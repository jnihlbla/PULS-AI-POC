000100*                                                                         
000200******************************************************************        
000300*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0152      *        
000400******************************************************************        
000500*                                                                         
000600 ID DIVISION.                                                             
000700                                                                          
000800 PROGRAM-ID.     W4073700.                                                
000900 AUTHOR.         LARS THELL.                                              
001000 DATE-WRITTEN.   95/07/02.                                                
001100 DATE-COMPILED.                                                           
001200                                                                          
001300*    FUNKTION:                                                            
001400*        VISA RETURTILLSTÅNDS.                                            
001500*        ANVÄNDS FÖR BEHANDLING AV RETURTILLSTÅND.                        
001600*                                                                         
001700*        PROGRAMMET UPPDATERAR WLKREE (WDA2)                              
001800*                              WLKOMA (WDP8)                              
001810*                                      WDR5                               
001900*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
002000*        PROGRAMMET LÄSER      W6KVAH (W6D2)                              
002100*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
002200*        PROGRAMMET LÄSER      WL4111 (WDR1)                              
002300*                                                                         
002400*        PROGRAMMET SKICKAR R32-POSTER VIA DISPATCHEN                     
002500*        PROGRAMMET SKAPAR SKROTORDER VIA DISPATCHEN                      
002600*                                                                         
002700*    INDATA.                                                              
002800*        TRANSAKTION: W4T737                                              
002900*        MID:         W4I73701                                            
003000*                                                                         
003100*    UTDATA.                                                              
003200*        MOD:         W4O73701                                            
003300*                                                                         
003400*    E'TRACKER: 5708363 DATUM 20071003                                    
003500*                                                                         
003600                                                                          
003700     SKIP3                                                                
003800 ENVIRONMENT DIVISION.                                                    
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300*    -- CHECKED BY WY2000                                                 
004400 77  IDPGM                       PIC X(08)   VALUE 'W4073700'.            
004500                                                                          
004600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004700 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004800                                                                          
004900 77  JA                          PIC X       VALUE 'J'.                   
005000 77  YES                         PIC X       VALUE 'Y'.                   
005100 77  NEJ                         PIC X       VALUE 'N'.                   
005200 77  W-TXT                       PIC X(3)    VALUE 'TXT'.                 
005300 77  W-ILI                       PIC X(3)    VALUE 'ILI'.                 
005400 77  W-INL                       PIC X(3)    VALUE 'INL'.                 
005500 77  W-SKR                       PIC X(3)    VALUE 'SKR'.                 
005600 77  W-ANT                       PIC X(3)    VALUE 'ANT'.                 
005700 77  W-KVA                       PIC X(3)    VALUE 'KVA'.                 
005800 77  W-BLI                       PIC X(3)    VALUE 'BLI'.                 
005900 77  W-BIN                       PIC X(3)    VALUE 'BIN'.                 
006000 77  W-SCR                       PIC X(3)    VALUE 'SCR'.                 
006100 77  W-DEV                       PIC X(3)    VALUE 'DEV'.                 
006200 77  W-QDE                       PIC X(3)    VALUE 'QDE'.                 
006300 77  W-PLUS                      PIC X       VALUE '+'.                   
006400 77  INFO-1                      PIC X(15)   VALUE SPACE.                 
006500 77  INFO-2                      PIC X(15)   VALUE SPACE.                 
006600 77  INFO-3                      PIC X(15)   VALUE SPACE.                 
006700 77  INFO-4                      PIC X(15)   VALUE SPACE.                 
006800 77  INFO-5                      PIC X(15)   VALUE SPACE.                 
006900 77  INFO-6                      PIC X(15)   VALUE SPACE.                 
007000 77  INFO-7                      PIC X(15)   VALUE SPACE.                 
007100 77  INFO-8                      PIC X(15)   VALUE SPACE.                 
007200 77  INFO-1-ANTAL                PIC 9(7)    VALUE ZERO.                  
007300 77  INFO-2-ANTAL                PIC 9(7)    VALUE ZERO.                  
007400 77  INFO-3-ANTAL                PIC 9(7)    VALUE ZERO.                  
007500 77  INFO-4-ANTAL                PIC 9(7)    VALUE ZERO.                  
007600 77  INFO-5-ANTAL                PIC 9(7)    VALUE ZERO.                  
007700 77  INFO-6-ANTAL                PIC 9(7)    VALUE ZERO.                  
007800 77  INFO-7-ANTAL                PIC 9(7)    VALUE ZERO.                  
007900 77  INFO-8-ANTAL                PIC 9(7)    VALUE ZERO.                  
008000*  INNEHÅLLER X'3F'                                                       
008100 77  W-X3F                       PIC X       VALUE X'3F'.                 
008200                                                                          
008300*    --- INDEX FÖR BLÄDDRINGSRADER                                        
008400 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
008500 77  4794-IX                     PIC S9(4)  VALUE +0    COMP SYNC.        
008600 77  MAX-INDX                    PIC S9(4)  VALUE +9    COMP SYNC.        
008700 77  4797-IX2                    PIC S9(5)  VALUE +0    COMP SYNC.        
008800 77  SPAR4797-IX                 PIC S9(5)  VALUE +0    COMP SYNC.        
008900 77  4797-INDX                   PIC S9(4)  VALUE +0    COMP SYNC.        
009000 77  4797-MAX-INDX               PIC S9(4)  VALUE +16   COMP SYNC.        
009100 77  MAX-TRANS-IX                PIC S9(4)  VALUE +0    COMP SYNC.        
009200 77  ORAD-IX                     PIC S9(4)  VALUE +0    COMP SYNC.        
009300 77  ORAD-IX-MAX                 PIC S9(4)  VALUE +5    COMP SYNC.        
009400 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
009500                                                                          
009600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
009700 77  WS-IDDISTR                  PIC  X(4)  VALUE SPACE.                  
009800 77  WS-IDKUNDNR                 PIC  X(6)  VALUE SPACE.                  
009900 77  WS-IDRAPPNR                 PIC  X(6)  VALUE SPACE.                  
010000 77  WS-IDARTNR-NUM              PIC  9(9)  VALUE ZERO.                   
010100 77  WS-FLTOT                    PIC  X(1)  VALUE SPACE.                  
010200 77  WS-KDCMDVAL                 PIC  X(3)  VALUE SPACE.                  
010300 77  WS-KVANTAL                  PIC  9(6)  VALUE ZERO.                   
010400 77  WS-ADGANG                   PIC  9(2)  VALUE ZERO.                   
010500                                                                          
010600 77  W-IDPERSON                  PIC  9(3)  VALUE ZERO.                   
010700 77  W-ANM-AVIS                  PIC  X(1)  VALUE '4'.                    
010800 77  W-ANM-MOT                   PIC  X(1)  VALUE '5'.                    
010900 77  W-ANM-PAAB                  PIC  X(1)  VALUE '6'.                    
011000 77  W-ANM-KLAR                  PIC  X(1)  VALUE '7'.                    
011100 77  W-KVLEVANM-KVAR             PIC S9(7)  VALUE 0   COMP-3.             
011200 77  W-KVAVV-KVANT-R32           PIC S9(7)  VALUE 0   COMP-3.             
011300 77  W-KVRETINL-R32              PIC S9(7)  VALUE 0   COMP-3.             
011400 77  W-KVRETINL-R32-SKR          PIC S9(7)  VALUE 0   COMP-3.             
011500 77  W-KVRADER-BEH               PIC S9(3)  VALUE 0   COMP-3.             
011600 77  W-KVANTAL                   PIC S9(7)  VALUE 0   COMP-3.             
011700 77  W-IDILIST                   PIC  9(5)  VALUE 0.                      
011800 77  W-KDCMDVAL-NUM              PIC  9(3)  VALUE 0.                      
011900                                                                          
012000 77  W-TIKLOCK-R32               PIC  9(8)  VALUE ZERO.                   
012100 77  W-TIKLOCK-ORDER             PIC  9(8)  VALUE ZERO.                   
012200                                                                          
012300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   VALUE +16 COMP SYNC.         
012400                                                                          
012500 01  WS-IDKONTO.                                                          
012600     03  WS-IDKONTO1-6           PIC  9(6)   VALUE ZERO.                  
012700     03  WS-IDKONTO7-8           PIC  9(2)   VALUE ZERO.                  
012800     03  WS-IDKONTO9-10          PIC  9(2)   VALUE ZERO.                  
012900                                                                          
013000 01  W-SKROT-FAELT.                                                       
013100     05  W-TIME-X.                                                        
013200         10  W-TIME-TT       PIC 9(2).                                    
013300         10  FILLER          PIC 9(6).                                    
013400     05  W-TIME-N            REDEFINES W-TIME-X                           
013500                             PIC 9(8).                                    
013600     05  W-KVSKROT-6-X.                                                   
013700         10  W-KVSKROT-6         PIC 9(6).                                
013800                                                                          
013900     05  W-IDORDNR-X.                                                     
014000       07 FILLER          PIC 9(2).                                       
014100       07 W-IDORDNR.                                                      
014200         10  W-IDORDNR-VV    PIC 9(2).                                    
014300         10  W-IDORDNR-LLL   PIC 9(3).                                    
014400                                                                          
014500 01  WS-IDDISTR-X.                                                        
014600     05 WS-IDDISTR-N         PIC 9(4).                                    
014700                                                                          
014800 01  WS-IDKUNDNR-X.                                                       
014900     05 WS-IDKUNDNR-N        PIC 9(6).                                    
015000                                                                          
015100 77  TEST-KDCMDVAL               PIC X(3)    VALUE SPACE.                 
015200     88  GODK-KDCMDVAL                       VALUE 'INL'                  
015300                                                   'SKR'                  
015400                                                   'ANT'                  
015500                                                   'KVA'                  
015600                                                   'ILI'                  
015700                                                   'BIN'                  
015800                                                   'SCR'                  
015900                                                   'DEV'                  
016000                                                   'QDE'                  
016100                                                   'BLI'.                 
016200                                                                          
016300 77  SW-IDANSTNR                 PIC X       VALUE 'N'.                   
016400     88  IDANSTNR-IFYLLT                     VALUE 'J'.                   
016500                                                                          
016600 77  SW-ALLT-INLAGT              PIC X       VALUE 'N'.                   
016700     88  ALLT-INLAGT                         VALUE 'J'.                   
016800                                                                          
016900 77  SW-ALLT-ANT-AVV             PIC X       VALUE 'N'.                   
017000     88  ALLT-ANT-AVV                        VALUE 'J'.                   
017100                                                                          
017200 77  SW-ALLT-SKROT               PIC X       VALUE 'N'.                   
017300     88  ALLT-SKROT                          VALUE 'J'.                   
017400                                                                          
017500 77  SW-ALLT-TILL-ILI            PIC X       VALUE 'N'.                   
017600     88  ALLT-TILL-ILI                       VALUE 'J'.                   
017700                                                                          
017800 77  SW-RAD-CMD                  PIC X       VALUE 'N'.                   
017900     88  RAD-CMD                             VALUE 'J'.                   
018000                                                                          
018100 77  SW-ILI                      PIC X       VALUE 'J'.                   
018200     88  FOERSTA-ILI                         VALUE 'J'.                   
018300                                                                          
018400 77  SW-KLART                    PIC X       VALUE 'J'.                   
018500     88  KLART                               VALUE 'J'.                   
018600     88  EJ-KLART                            VALUE 'N'.                   
018700                                                                          
018800 77  TRANS-OHUVUD-DAM-SKAPAD-SW  PIC X       VALUE 'N'.                   
018900     88  TRANS-OHUVUD-DAM-SKAPAD             VALUE 'J'.                   
019000                                                                          
019100 77  ALLT-SKR-ORDER-SKAPAD-SW    PIC X       VALUE 'N'.                   
019200     88  ALLT-SKR-ORDER-SKAPAD               VALUE 'J'.                   
019300                                                                          
019400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
019500     88  INDATA-OK                           VALUE 'J'.                   
019600     88  INDATA-FEL                          VALUE 'N'.                   
019700                                                                          
019800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
019900     88  NYCKLAR-OK                          VALUE 'J'.                   
020000     88  NYCKLAR-FEL                         VALUE 'N'.                   
020010                                                                          
020020 77  SW-SKAPA-WDR5               PIC X       VALUE 'N'.                   
020030     88  SKAPA-WDR5                          VALUE 'J'.                   
020050                                                                          
020100                                                                          
020200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
020300     88  EGEN-MID                            VALUE '4737'.                
020400     88  GODK-MID                            VALUE '4737' '4735'.         
020500     88  4723-MID                            VALUE '4723'.                
020600     88  HELP-MID                            VALUE '0551'.                
020700     EJECT                                                                
020800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
020900 01  GENERELLA-SUBPROGRAM.                                                
021000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
021100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
021200     03  W009KSIF                PIC X(8)    VALUE 'W009KSIF'.            
021300     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
021400     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
021500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
021600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
021700     03  W418OKOD                PIC X(8)    VALUE 'W418OKOD'.            
021800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
021900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
022000     EJECT                                                                
022100*    ---  LÄNKAREA TILL W418OKOD                                          
022200 01  FILLER                      PIC X(16)   VALUE 'W418OKOD'.            
022300                                                                          
022400*01 -COPY W418OKOD           -PRE OKOD-.                                  
022500     EJECT                                                                
022600 01  FILLER                      PIC X(16)   VALUE 'DAT-AREA'.            
022700     SKIP3                                                                
022800 01  DAT-IO-AREA.                                                         
022900*    03  -COPY WDATAREA                                                   
023000     EJECT                                                                
023100*01  -COPY W006PRT                                                        
023200     EJECT                                                                
023300*    ---  DISTRICT CODES                                                  
023400*01  -COPY WWDIST35                                                       
023500     EJECT                                                                
023600                                                                          
023700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
023800*01 -COPY WMEDAREA                                                        
023900     SKIP3                                                                
024000 01  MESSAGE-CODES.                                                       
024100     03  ERR-CORR-HILITE-FLDS     PIC X(3)   VALUE '001'.                 
024200     03  INF-PRESS-PF11           PIC X(3)   VALUE '003'.                 
024300     03  ERR-FORBIDDEN-UPDATE     PIC X(3)   VALUE '007'.                 
024400     03  INF-PRESS-PF4            PIC X(3)   VALUE '081'.                 
024500     03  ERR-INFO-MISSING         PIC X(3)   VALUE '005'.                 
024600     03  ERR-PF11-AND-NO-DATA     PIC X(3)   VALUE '011'.                 
024700     03  ERR-FLERA-FUNKTIONER     PIC X(3)   VALUE '097'.                 
024800     03  ERR-RAPPORTERING-STARTAD PIC X(3)   VALUE '312'.                 
024900     03  ERR-RAPPORTERING-KLAR    PIC X(3)   VALUE '313'.                 
025000     03  INF-UPDATE-DONE          PIC X(3)   VALUE '101'.                 
025100     03  INF-FIRST-PAGE           PIC X(3)   VALUE '006'.                 
025200     03  INF-MORE-INFO-EXISTS     PIC X(3)   VALUE '105'.                 
025300     03  INF-PRINT-BEG            PIC X(3)   VALUE '118'.                 
025400     03  ERR-NOTHING-PRINTED      PIC X(3)   VALUE '167'.                 
025500     03  ERR-NO-LINE-CHOSEN       PIC X(3)   VALUE '231'.                 
025600     03  ERR-WRONG-KEY            PIC X(3)   VALUE '401'.                 
025700     03  ERR-WRONG-PRINTER        PIC X(3)   VALUE '772'.                 
025710     03  ERR-LAGER-SAKNAS         PIC X(3)   VALUE '764'.                 
025800     EJECT                                                                
025900*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
026000*                                                                         
026100 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
026200     SKIP3                                                                
026300*01 -COPY WMSGINIT                                                        
026400                                                                          
026500 01  KONTROLL-SIFFRA.                                                     
026600     03  REK-IDARTNR             PIC 9(9)    VALUE 0.                     
026700     03  REK-LNGD                PIC 9(1)    VALUE 9.                     
026800     03  REK-REKSIFFR            PIC 9(1)    VALUE 0.                     
026900                                                                          
027000     EJECT                                                                
027100*    --- FÄLT FÖR HOPP TILL ANDRA BILDER                                  
027200   77  SW-STARTA-ANNAN-BILD        PIC X       VALUE 'N'.                 
027300     88  STARTA-ANNAN-BILD                     VALUE 'J'.                 
027400                                                                          
027500 01  BILD-HOPP-AREOR.                                                     
027600                                                                          
027700   03 FILLER             PIC X(16)   VALUE 'P-TO-P-AREA'.                 
027800   03      P-TO-P-SW.                                                     
027900                                                                          
028000     05  P-TO-P-KVLL             PIC S9(4) VALUE +0   COMP SYNC.          
028100     05  P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
028200     05  P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
028300     05  P-TO-P-KDTRANS          PIC X(8).                                
028400     05  P-TO-P-IDTRANS          PIC X(4).                                
028500     05  P-TO-P-KDMFSFOR         PIC X(1).                                
028600     05  P-TO-P-DATA             PIC X(100) VALUE ALL '+'.                
028700                                                                          
028800   03 FILLER             PIC X(16)   VALUE 'P-TO-P-AREA2'.                
028900 01  P-TO-P-T94.                                                          
029000*----TILL W40794                                                          
029100     03  P-TO-P2-LL              PIC S9(4)   COMP SYNC.                   
029200     03  P-TO-P2-Z1              PIC X(1)    VALUE LOW-VALUE.             
029300     03  P-TO-P2-Z2              PIC X(1)    VALUE LOW-VALUE.             
029400     03  P-TO-P2-TRANSKOD        PIC X(7)    VALUE 'W4T794X'.             
029500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
029600     03  P-TO-P2-IDTRANS         PIC X(4)    VALUE '4737'.                
029700     03  P-TO-P2-KDMFSFOR        PIC X(1)    VALUE SPACE.                 
029800*    03  MID -COPY W4I79401 -PRE MOD4794-                                 
029900     EJECT                                                                
030000   03 FILLER             PIC X(16)   VALUE 'P-TO-P-AREA3'.                
030100 01  P-TO-P-T23.                                                          
030200*----TILL W40723                                                          
030300     03  P-TO-P3-LL              PIC S9(4)   COMP SYNC VALUE +117.        
030400     03  P-TO-P3-Z1              PIC X(1)    VALUE LOW-VALUE.             
030500     03  P-TO-P3-Z2              PIC X(1)    VALUE LOW-VALUE.             
030600     03  P-TO-P3-TRANSKOD        PIC X(7)    VALUE 'W4T723'.              
030700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
030800     03  P-TO-P3-IDTRANS         PIC X(4)    VALUE '4737'.                
030900     03  P-TO-P3-KDMFSFOR        PIC X(1)    VALUE SPACE.                 
031000     03  P-TO-P3-DATA            PIC X(100)  VALUE ALL '+'.               
031100     EJECT                                                                
031200 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA4'.        
031300*  AREA FÖR DISPATCHEN                                                    
031400 01  P-TO-P-AREA4.                                                        
031500     03  P-TO-P4-LL              PIC S9(4)            COMP SYNC.          
031600     03  P-TO-P4-Z1              PIC  X(1)   VALUE LOW-VALUE.             
031700     03  P-TO-P4-Z2              PIC  X(1)   VALUE LOW-VALUE.             
031800     03  P-TO-P4-TRANSKOD        PIC  X(7).                               
031900     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
032000     03  P-TO-P4-FROM-MID        PIC  X(4).                               
032100     03  P-TO-P4-KDMFSFOR        PIC  X(1).                               
032200     03  P-TO-P4-DATA            PIC  X(1000).                            
032300                                                                          
032400     EJECT                                                                
032500 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA5'.        
032600*  AREA FÖR DISPATCHEN                                                    
032700 01  P-TO-P-AREA5.                                                        
032800     03  P-TO-P5-LL              PIC S9(4)            COMP SYNC.          
032900     03  P-TO-P5-Z1              PIC  X(1)   VALUE LOW-VALUE.             
033000     03  P-TO-P5-Z2              PIC  X(1)   VALUE LOW-VALUE.             
033100     03  P-TO-P5-TRANSKOD        PIC  X(7).                               
033200     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
033300     03  P-TO-P5-FROM-MID        PIC  X(4).                               
033400     03  P-TO-P5-KDMFSFOR        PIC  X(1).                               
033500     03  P-TO-P5-DATA            PIC  X(1000).                            
033600                                                                          
033700     EJECT                                                                
033800******************************************************************        
033900*  SPAR-AREA R32-MID FÖR DISPATCHEN                                       
034000 01 SPAR-AREA-R32.                                                        
034100    03 SPAR4797-MID-R32-POST   OCCURS 500 TIMES.                          
034200      05  SPAR4797-MID-IDDISTR       PIC 9(4)  VALUE ZERO.                
034300      05  SPAR4797-MID-IDKUNDNR      PIC 9(6)  VALUE ZERO.                
034400      05  SPAR4797-MID-IDRAPPNR      PIC 9(7)  VALUE ZERO.                
034500      05  SPAR4797-MID-IDARTNR       PIC 9(8)  VALUE ZERO.                
034600      05  SPAR4797-MID-IDRADNR       PIC 9(4)  VALUE ZERO.                
034700      05  SPAR4797-MID-KVRETINL      PIC 9(6)  VALUE ZERO.                
034800      05  SPAR4797-MID-KVAVV-KVANT   PIC 9(6)  VALUE ZERO.                
034900      05  SPAR4797-MID-KVRETINL-TRP  PIC 9(6)  VALUE ZERO.                
035000      05  SPAR4797-MID-KVRETINL-SKR  PIC 9(6)  VALUE ZERO.                
035100                                                                          
035200******************************************************************        
035300     EJECT                                                                
035400*                                                                         
035500*    --- AREOR FÖR W006KOM SUBMODUL                                       
035600*                                                                         
035700 01  FILLER                      PIC X(16)   VALUE 'MSG-KOM-AREA'.        
035800*01  -COPY WMSGKOM                                                        
035900     EJECT                                                                
036000*                                                                         
036100 01  FILLER                      PIC X(16)   VALUE 'W4I25101'.            
036200 01  FILLER.                                                              
036300   03  OHUV-KOM-AREA.                                                     
036400*    05      -COPY W4I25101   -PRE OHUV-                                  
036500     EJECT                                                                
036600 01  FILLER                      PIC X(16)   VALUE 'W4I25201'.            
036700 01  FILLER.                                                              
036800   03  ORAD-KOM-AREA.                                                     
036900*    05      -COPY W4I25201   -PRE ORAD-                                  
037000     EJECT                                                                
037100*                                                                         
037200 01  FILLER                      PIC X(16)   VALUE 'W4I79701'.            
037300 01  FILLER.                                                              
037400    03 R32-KOM-AREA.                                                      
037500      05    -COPY W4I79701 -PRE MOD4797-                                  
037600     EJECT                                                                
037700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
037800*                                                                         
037900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
038000     SKIP3                                                                
038100*01  MID -COPY W4I73701                                                   
038200     EJECT                                                                
038300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
038400     SKIP3                                                                
038500*01  -COPY WMSGAREA                                                       
038600     EJECT                                                                
038700     03  MOD REDEFINES MSG-AREA.                                          
038800*      05  -COPY W4O73701                                                 
038900     EJECT                                                                
039000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
039100     SKIP3                                                                
039200*01  -COPY WMFSAREA                                                       
039300     EJECT                                                                
039400                                                                          
039500     SKIP2                                                                
039600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
039700*                                                                         
039800     EJECT                                                                
039900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
040000     SKIP3                                                                
040100 01  W-MINKEY-X.                                                          
040200     03  W-MINKEY-IDTRANS          PIC X(4)    VALUE '4737'.              
040300     03  W-MINKEY-WDA211KY-ENTER.                                         
040400         05  W-MINKEY-IDARTNR      PIC S9(9)   COMP-3 VALUE ZERO.         
040500         05  W-MINKEY-IDRADNR      PIC S9(5)   COMP-3 VALUE ZERO.         
040600     03  W-MINKEY-WDA211KY-NEXT.                                          
040700         05  W-MINKEY-IDARTNR-NEXT PIC S9(9)   COMP-3 VALUE ZERO.         
040800         05  W-MINKEY-IDRADNR-NEXT PIC S9(5)   COMP-3 VALUE ZERO.         
040900     SKIP3                                                                
041000                                                                          
041100 01  NYCKLAR-TILL-DLI.                                                    
041200                                                                          
041300     03  W-IDLEVANM-X.                                                    
041400         05  W-IDDISTR           PIC S9(5)   COMP-3 VALUE ZERO.           
041500         05  W-IDKUNDNR          PIC S9(7)   COMP-3 VALUE ZERO.           
041600         05  W-IDRAPPNR          PIC  9(7)          VALUE ZERO.           
041700                                                                          
041800     03  W-WDA211KY-X.                                                    
041900         05  W-IDARTNR-A2        PIC S9(9)   COMP-3 VALUE ZERO.           
042000         05  W-IDRADNR-A2        PIC S9(5)   COMP-3 VALUE ZERO.           
042100                                                                          
042200     03  W-WDA211KY-MIN-X.                                                
042300         05  W-IDARTNR-A2-MIN    PIC S9(9)   COMP-3 VALUE ZERO.           
042400         05  W-IDRADNR-A2-MIN    PIC S9(5)   COMP-3 VALUE ZERO.           
042500                                                                          
042600     03  W-WDA2E1KY-MIN-X.                                                
042700         05  W-IDDC-E1-MIN       PIC  X(2)          VALUE SPACE.          
042800         05  W-IDILIST-E1-MIN    PIC  9(5)          VALUE ZERO.           
042900         05  FILLER              PIC X(29)          VALUE SPACE.          
043000                                                                          
043100     03  W-WDA2E1KY-MAX-X.                                                
043200         05  W-IDDC-E1-MAX       PIC  X(2)          VALUE SPACE.          
043300         05  W-IDILIST-E1-MAX    PIC  9(5)          VALUE ZERO.           
043400         05  FILLER              PIC X(29)          VALUE SPACE.          
043500                                                                          
043600     03  W-WDA3FSEQ-X.                                                    
043700         05  W-IDDC-FSEQ         PIC  X(2)          VALUE SPACE.          
043800         05  W-IDDISTR-FSEQ      PIC S9(5)   COMP-3 VALUE ZERO.           
043900         05  W-IDKUNDNR-FSEQ     PIC S9(7)   COMP-3 VALUE ZERO.           
044000         05  W-IDRAPPNR-FSEQ     PIC  9(7)   VALUE ZERO.                  
044100                                                                          
044200     03  W-WDA3FSEQ-MIN-X.                                                
044300         05  W-IDDC-FSEQ-MIN     PIC  X(2)          VALUE SPACE.          
044400         05  W-IDDISTR-FSEQ-MIN  PIC S9(5)   COMP-3 VALUE ZERO.           
044500         05  W-IDKUNDNR-FSEQ-MIN PIC S9(7)   COMP-3 VALUE ZERO.           
044600         05  W-IDRAPPNR-FSEQ-MIN PIC  9(7)   VALUE ZERO.                  
044700                                                                          
044800     03  W-WDA3FSEQ-MAX-X.                                                
044900         05  W-IDDC-FSEQ-MAX     PIC  X(2)          VALUE SPACE.          
045000         05  W-IDDISTR-FSEQ-MAX  PIC S9(5)   COMP-3 VALUE ZERO.           
045100         05  W-IDKUNDNR-FSEQ-MAX PIC S9(7)   COMP-3 VALUE ZERO.           
045200         05  W-IDRAPPNR-FSEQ-MAX PIC  9(7)   VALUE ZERO.                  
045300                                                                          
045400     03  W-IDARTNR-X.                                                     
045500         05  W-IDARTNR           PIC S9(9)   COMP-3 VALUE ZERO.           
045600                                                                          
045700     03  W-IDDC-X.                                                        
045800         05  W-IDDC              PIC  X(2)   VALUE SPACE.                 
045900                                                                          
046000     03  W-IDSKYLT-X.                                                     
046100         05  W-IDSKYLT           PIC  X(3)   VALUE SPACE.                 
046200                                                                          
046300     03  W-KDKVAINF-X.                                                    
046400         05  W-KDKVAINF          PIC  X(1)   VALUE 'R'.                   
046500                                                                          
046600     03  W-IDFTG-X.                                                       
046700         05  W-IDFTG             PIC  9(2)   VALUE ZERO.                  
046800                                                                          
046900     03  W-WDGXKEY-4111-X.                                                
047000         05  FILLER              PIC  X(4)   VALUE '4111'.                
047100         05  W-IDRT-4111         PIC  X(3)   VALUE 'CDC'.                 
047200         05  FILLER              PIC  X(23)  VALUE LOW-VALUE.             
047300                                                                          
047400     03  W-IDDC-B6-X.                                                     
047500         05 W-IDDC-B6                  PIC X(2).                          
047600                                                                          
047610                                                                          
047620     03  W-WDGXKEY-X.                                                     
047630         05  W-IDHTYP            PIC  X(4)    VALUE '4703'.               
047640         05  W-IDDC-4703         PIC  X(2)    VALUE SPACE.                
047650         05  FILLER              PIC  X(24)   VALUE LOW-VALUE.            
047660                                                                          
047670     03  W-IDILIST-4704-X.                                                
047680         05  W-IDILIST-4704      PIC  9(5)          VALUE ZERO.           
047690                                                                          
047700     SKIP2                                                                
047800*    --- STATUS-KOD FRÅN IMS                                              
047900 01  STATUS-WS                   PIC XX.                                  
048000     88  STATUS-OK                           VALUE '  '.                  
048100     88  SEGMENT-FINNS                       VALUE '  '.                  
048200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
048300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
048400     88  SEGMENT-SLUT                        VALUE 'GB'.                  
048500     88  TRANSKOD-FEL                        VALUE 'A1'.                  
048600     88  SECURITY-FEL                        VALUE 'A4'.                  
048700     SKIP2                                                                
048800 01  GODK-STATUSKODER.                                                    
048900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
049000     SKIP3                                                                
049100 01  SSA1                        PIC X(192).                              
049200 01  SSA2                        PIC X(64).                               
049300     EJECT                                                                
049400*    --- IMS FUNKTIONSKODER                                               
049500*01  -COPY W0003                                                          
049600     EJECT                                                                
049700*    ---  DLI INPUT-OUTPUT AREA                                           
049800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
049900     SKIP3                                                                
050000 01  DLI-IO-AREA.                                                         
050100     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
050200     SKIP3                                                                
050300     03  WLKREE01 REDEFINES IO-AREA.                                      
050400*        05  -COPY WDA201                                                 
050500     EJECT                                                                
050600     03  WLKREE11 REDEFINES IO-AREA.                                      
050700*        05  -COPY WDA211                                                 
050800     EJECT                                                                
050900     03  WLKREJ01 REDEFINES IO-AREA.                                      
051000*        05  -COPY WDA2E1                                                 
051100     EJECT                                                                
051200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
051300     SKIP3                                                                
051400 01  DLI-IO-AREA2.                                                        
051500     03  IO-AREA2                PIC X(1200)  VALUE SPACE.                
051600     SKIP3                                                                
051700     03  WLKREE21 REDEFINES IO-AREA2.                                     
051800*        05  -COPY WDA221                                                 
051900     SKIP2                                                                
052000 01  DLI-IO-AREA4.                                                        
052100     03  IO-AREA4                PIC X(32)  VALUE SPACE.                  
052200     SKIP3                                                                
052300     03  WL411111 REDEFINES IO-AREA4.                                     
052400*        05  -COPY WDGX4112                                               
052500     SKIP2                                                                
052600 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDA301'.        
052700 01  DLI-IO-WDA301.                                                       
052800*    03  -COPY WDA301                                                     
052900     SKIP2                                                                
053400 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK611'.        
053500 01  DLI-IO-WDK611.                                                       
053600*    03  -COPY WDK611                                                     
053700     SKIP2                                                                
053800 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK711'.        
053900 01  DLI-IO-WDK711.                                                       
054000*    03  -COPY WDK711                                                     
054100     SKIP2                                                                
054200 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDD311'.        
054300 01  DLI-IO-WDD311.                                                       
054400*    03  -COPY WDD311                                                     
054500     SKIP2                                                                
054600 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-W6D211'.        
054700 01  DLI-IO-W6D211.                                                       
054800*    03  -COPY W6D211                                                     
054900                                                                          
055000 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
055100 01   DLI-IO-AREA-B601.                                                   
055200*     03  -COPY WDB601                                                    
055250                                                                          
055260 01  FILLER               PIC X(16)   VALUE 'WDGX01DC AREA'.              
055270 01   DLI-IO-WDGX01DC.                                                    
055280*     03  -COPY WDGX01DC                                                  
055290                                                                          
055291 01  FILLER               PIC X(16)   VALUE 'WDGX4704 AREA'.              
055292 01   DLI-IO-WDGX4704.                                                    
055293*     03  -COPY WDGX4704                                                  
055300                                                                          
055400     EJECT                                                                
055500 LINKAGE SECTION.                                                         
055600                                                                          
055700*01  -COPY W0009   -PRE MSG-                                              
055800*01  -COPY W0009   -PRE W4723-                                            
055900     EJECT                                                                
056000*01  -COPY W0009   -PRE W4794-                                            
056100     EJECT                                                                
056200*01  -COPY W0009   -PRE DISP-                                             
056300     EJECT                                                                
056400*01  -COPY W0008   -PRE USEA-                                             
056500     05  FILLER                  PIC X.                                   
056600     EJECT                                                                
056700*01  -COPY W0008  -PRE KREE-                                              
056800     05  FILLER                  PIC X.                                   
056900     EJECT                                                                
057000*01  -COPY W0008  -PRE KREJ-                                              
057100     05  FILLER                  PIC X.                                   
057200     EJECT                                                                
057300*01  -COPY W0008  -PRE RETA-                                              
057400     05  FILLER                  PIC X.                                   
057500     EJECT                                                                
057600*01  -COPY W0008  -PRE ARTC-                                              
057700     05  FILLER                  PIC X.                                   
057800     EJECT                                                                
057900*01  -COPY W0008  -PRE BENA-                                              
058000     05  FILLER                  PIC X.                                   
058100     EJECT                                                                
058200*01  -COPY W0008  -PRE KVAH-                                              
058300     05  FILLER                  PIC X.                                   
058400     EJECT                                                                
058500*01  -COPY W0008  -PRE 4111-                                              
058600     05  FILLER                  PIC X.                                   
058700     EJECT                                                                
058800*01  -COPY W0008  -PRE RETA2-                                             
058900     05  FILLER                  PIC X.                                   
059000     EJECT                                                                
059100*01  -COPY W0008  -PRE RETA3-                                             
059200     05  FILLER                  PIC X.                                   
059300     EJECT                                                                
059400*01  -COPY W0008  -PRE ARTS-                                              
059500     05  FILLER                  PIC X.                                   
059600     EJECT                                                                
059700*01  -COPY W0008  -PRE KOMA-                                              
059800     05  FILLER                  PIC X.                                   
059900     EJECT                                                                
060000*01  -COPY W0008  -PRE WDB6-                                              
060100     05  FILLER                  PIC X.                                   
060200     EJECT                                                                
060210*01  -COPY W0008  -PRE WDR5-                                              
060220     05  FILLER                  PIC X.                                   
060230     EJECT                                                                
060300 PROCEDURE DIVISION  USING MSG-PCB  W4723-PCB W4794-PCB DISP-PCB          
060400                           USEA-PCB                                       
060500                           KREE-PCB KREJ-PCB  RETA-PCB ARTC-PCB           
060600                           BENA-PCB KVAH-PCB  4111-PCB                    
060700                           RETA2-PCB RETA3-PCB ARTS-PCB KOMA-PCB          
060800                           WDB6-PCB WDR5-PCB.                             
060900     ENTRY 'DLITCBL' USING MSG-PCB  W4723-PCB W4794-PCB DISP-PCB          
061000                           USEA-PCB                                       
061100                           KREE-PCB KREJ-PCB  RETA-PCB ARTC-PCB           
061200                           BENA-PCB KVAH-PCB  4111-PCB                    
061300                           RETA2-PCB RETA3-PCB ARTS-PCB KOMA-PCB          
061400                           WDB6-PCB WDR5-PCB.                             
061500                                                                          
061600     PERFORM IMS-GET-MSG                                                  
061700     IF SEGMENT-FINNS                                                     
061800       PERFORM A-INIT                                                     
061900       PERFORM B-KOLLA-NYCKLAR                                            
062000       IF NYCKLAR-OK                                                      
062100         MOVE MID-MODFAELT-IN         TO MOD-INPUT                        
062200         INSPECT MOD-INPUT REPLACING ALL W-PLUS BY W-X3F                  
062300         IF (MFS-ENTER AND EGEN-MID) OR HELP-MID OR MFS-UPDATE OR         
062400             MFS-PRINT                                                    
062500             PERFORM G-KOLLA-INPUT                                        
062600         END-IF                                                           
062700                                                                          
062800         IF (MFS-PRINT OR MFS-UPDATE)                                     
062900             IF INDATA-OK                                                 
063000                PERFORM H-UPPDATERA-SKRIV-UT                              
063100             END-IF                                                       
063200         ELSE                                                             
063300            IF MFS-FIRST                                                  
063400               PERFORM C-FOERSTA-SIDA                                     
063500            ELSE                                                          
063600               IF MFS-NEXT                                                
063700                  PERFORM D-NAESTA-SIDA                                   
063800               ELSE                                                       
063900                  PERFORM E-SAMMA-SIDA                                    
064000               END-IF                                                     
064100            END-IF                                                        
064200         END-IF                                                           
064300         IF STARTA-ANNAN-BILD                                             
064400            CONTINUE                                                      
064500         ELSE                                                             
064600            IF INDATA-OK                                                  
064700               PERFORM F-LAES-VISA-INFO                                   
064800            END-IF                                                        
064900         END-IF                                                           
065000       END-IF                                                             
065100       IF STARTA-ANNAN-BILD                                               
065200          CONTINUE                                                        
065300       ELSE                                                               
065400          COMPUTE MSG-KVLL = LENGTH OF MOD-W4O73701 + 4                   
065500          PERFORM IMS-INSERT-MSG                                          
065600       END-IF                                                             
065700     END-IF                                                               
065800                                                                          
065900     MOVE ZERO TO RETURN-CODE                                             
066000     GOBACK                                                               
066100     .                                                                    
066200     EJECT                                                                
066300 A-INIT SECTION.                                                          
066400                                                                          
066500     IF MSG-DUBBLA-TRANSKODER                                             
066600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I73701                 
066700       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
066800       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
066900     ELSE                                                                 
067000       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I73701                 
067100       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
067200       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
067300     END-IF                                                               
067400                                                                          
067500     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
067600     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
067700     MOVE MFS-IDTRANS TO W-IDTRANS                                        
067800                                                                          
067900     MOVE LOW-VALUE TO MSG-AREA                                           
068000     MOVE 'W4O73701' TO MFS-IDMOD                                         
068100     MOVE '4737' TO MOD-IDTRANS                                           
068200     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
068300                                                                          
068400     IF EGEN-MID OR HELP-MID                                              
068500       CONTINUE                                                           
068600     ELSE                                                                 
068700       IF 4723-MID                                                        
068800         MOVE SPACE TO MFS-KDTRTYP                                        
068900       ELSE                                                               
069000         MOVE SPACE TO MFS-KDTRTYP                                        
069100         MOVE '7' TO MFS-IDPFK                                            
069200       END-IF                                                             
069300     END-IF                                                               
069400                                                                          
069500     MOVE LOW-VALUE             TO W-WDA211KY-MIN-X                       
069600                                   W-WDA2E1KY-MIN-X                       
069700                                   W-WDA3FSEQ-MIN-X                       
069800                                                                          
069900     MOVE HIGH-VALUE            TO W-WDA2E1KY-MAX-X                       
070000                                   W-WDA3FSEQ-MAX-X                       
070100                                                                          
070200     MOVE 'IDAG ' TO DAT-KDDATFORM                                        
070300     MOVE ZERO    TO DAT-I-TIDATUM                                        
070400                     DAT-O-TIDATUM                                        
070500     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
070600                         DAT-O-TIDATUM DAT-KDSVAR                         
070700     IF DAT-KDSVAR-FEL                                                    
070800        MOVE 'FEL FRÅN DATKONV' TO FELTEXT                                
070900        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
071000     END-IF                                                               
071100                                                                          
071200     ACCEPT W-TIKLOCK-R32      FROM TIME                                  
071300     ACCEPT W-TIKLOCK-ORDER    FROM TIME                                  
071400                                                                          
071500     .                                                                    
071600     EJECT                                                                
071700 B-KOLLA-NYCKLAR SECTION.                                                 
071800                                                                          
071900     MOVE ALL '+'              TO MSGI-WMSGINIT                           
072000     MOVE '001'                TO MSGI-KDCALL                             
072100     IF EGEN-MID                                                          
072200        MOVE MID-IDDISTR-IN    TO MSGI-IDDISTR                            
072300        MOVE MID-IDKUNDNR-IN   TO MSGI-IDKUNDNR                           
072400        MOVE MID-IDRAPPNR-IN   TO MSGI-IDRAPPNR                           
072500        MOVE MID-IDARTNR-IN    TO MSGI-IDARTNR                            
072600     END-IF                                                               
072700     MOVE MSG-SIGNON-USERID    TO MSGI-IDUSER                             
072800     MOVE '4737'               TO MSGI-IDTRANS                            
072900     MOVE MSG-LTERM-NAME       TO MSGI-IDLTERM-USER                       
073000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
073100                                                                          
073200     IF MSGI-IDLAND-SPR = 'GB'                                            
073300       MOVE 'GB'                  TO MED-IDSKYLT                          
073400                                       W-IDSKYLT                          
073500     ELSE                                                                 
073600       MOVE 'S '                  TO MED-IDSKYLT                          
073700                                       W-IDSKYLT                          
073800     END-IF                                                               
073900                                                                          
074000                                                                          
074100     MOVE JA TO NYCKLAR-SW                                                
074200                                                                          
074300     PERFORM BA-KOLLA-IDDISTR                                             
074400     PERFORM BB-KOLLA-IDKUNDNR                                            
074500     PERFORM BC-KOLLA-IDRAPPNR                                            
074600     PERFORM BD-KOLLA-IDARTNR                                             
074700     PERFORM BE-KOLLA-FLTOT                                               
074800     IF NOT DCS-IDDC = MSGI-IDDC                                          
074900        MOVE MSGI-IDDC         TO W-IDDC-B6                               
075000        PERFORM IMS-GU-WDB601                                             
075100     END-IF                                                               
075200     IF  DCS-KDDC NOT = SPACE                                             
075300     AND DCS-FLDCRET = JA                                                 
075400       MOVE MSGI-IDDC            TO W-IDDC-E1-MIN                         
075500                                    W-IDDC-E1-MAX                         
075600                                    W-IDDC-FSEQ                           
075700                                    W-IDDC-FSEQ-MIN                       
075800                                    W-IDDC-FSEQ-MAX                       
075900       MOVE MSGI-IDRT-KEY        TO W-IDRT-4111                           
076000       MOVE MSGI-IDFTG           TO W-IDFTG                               
076100     ELSE                                                                 
076200       MOVE NEJ                  TO NYCKLAR-SW                            
076300     END-IF                                                               
076400                                                                          
076500     IF GODK-MID OR NYCKLAR-OK                                            
076600        MOVE MSGI-IDDISTR         TO MOD-IDDISTR-UT                       
076700        INSPECT MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE           
076800        MOVE MSGI-IDKUNDNR        TO MOD-IDKUNDNR-UT                      
076900        INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE           
077000        MOVE MSGI-IDRAPPNR        TO MOD-IDRAPPNR-UT                      
077100        INSPECT MOD-IDRAPPNR-UT REPLACING LEADING ZERO BY SPACE           
077200        MOVE W-IDARTNR            TO MOD-IDARTNR-UT                       
077300        INSPECT MOD-IDARTNR-UT  REPLACING LEADING ZERO BY SPACE           
077400        MOVE WS-FLTOT             TO MOD-FLTOT-UT                         
077500     ELSE                                                                 
077600        MOVE MFS-RENSA-FAELT      TO MOD-IDDISTR-UT                       
077700                                     MOD-IDKUNDNR-UT                      
077800                                     MOD-IDRAPPNR-UT                      
077900                                     MOD-IDARTNR-UT                       
078000                                     MOD-FLTOT-UT                         
078100     END-IF                                                               
078200                                                                          
078300     IF NYCKLAR-FEL                                                       
078400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
078500       CALL WMEDKONV USING MED-WMEDAREA                                   
078600       MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                 
078700       PERFORM MFS-RENSA-FAELT-IN                                         
078800       PERFORM MFS-RENSA-FAELT-UT                                         
078900     END-IF                                                               
079000     .                                                                    
079100     EJECT                                                                
079200                                                                          
079300                                                                          
079400 BA-KOLLA-IDDISTR  SECTION.                                               
079500                                                                          
079600     MOVE MFS-RENSA-FAELT       TO MOD-IDDISTR-IN                         
079700                                                                          
079800     IF MID-IDDISTR-IN          NOT = ALL '+'                             
079900       MOVE '7'                 TO MFS-IDPFK                              
080000       MOVE SPACE               TO MFS-KDTRTYP                            
080100     END-IF                                                               
080200                                                                          
080300     IF MSGI-IDDISTR NUMERIC AND MSGI-IDDISTR > ZERO                      
080400       MOVE MSGI-IDDISTR        TO W-IDDISTR                              
080500     ELSE                                                                 
080600       MOVE NEJ                 TO NYCKLAR-SW                             
080700     END-IF                                                               
080800                                                                          
080900     .                                                                    
081000     EJECT                                                                
081100                                                                          
081200 BB-KOLLA-IDKUNDNR   SECTION.                                             
081300                                                                          
081400     MOVE MFS-RENSA-FAELT       TO MOD-IDKUNDNR-IN                        
081500                                                                          
081600     IF MID-IDKUNDNR-IN         NOT = ALL '+'                             
081700       MOVE '7'                 TO MFS-IDPFK                              
081800       MOVE SPACE               TO MFS-KDTRTYP                            
081900     END-IF                                                               
082000                                                                          
082100     IF MSGI-IDKUNDNR           NUMERIC                                   
082200       MOVE MSGI-IDKUNDNR       TO W-IDKUNDNR                             
082300     ELSE                                                                 
082400       MOVE NEJ                 TO NYCKLAR-SW                             
082500     END-IF                                                               
082600                                                                          
082700     .                                                                    
082800     EJECT                                                                
082900 BC-KOLLA-IDRAPPNR   SECTION.                                             
083000                                                                          
083100     MOVE MFS-RENSA-FAELT       TO MOD-IDRAPPNR-IN                        
083200                                                                          
083300     IF MID-IDRAPPNR-IN         NOT = ALL '+'                             
083400       MOVE '7'                 TO MFS-IDPFK                              
083500       MOVE SPACE               TO MFS-KDTRTYP                            
083600     END-IF                                                               
083700                                                                          
083800     IF MSGI-IDRAPPNR           NUMERIC                                   
083900       MOVE MSGI-IDRAPPNR       TO W-IDRAPPNR                             
084000     ELSE                                                                 
084100       MOVE NEJ                 TO NYCKLAR-SW                             
084200     END-IF                                                               
084300                                                                          
084400     .                                                                    
084500     EJECT                                                                
084600 BD-KOLLA-IDARTNR   SECTION.                                              
084700                                                                          
084800     MOVE MFS-RENSA-FAELT       TO MOD-IDARTNR-IN                         
084900                                                                          
085000     IF MID-IDARTNR-IN          NOT = ALL '+'                             
085100       MOVE '7'                 TO MFS-IDPFK                              
085200       MOVE SPACE               TO MFS-KDTRTYP                            
085300     END-IF                                                               
085400                                                                          
085500     IF MID-IDARTNR-IN NOT = ALL '+'                                      
085600       IF MSGI-IDARTNR            NUMERIC AND MSGI-IDARTNR > ZERO         
085700         MOVE MSGI-IDARTNR        TO W-IDARTNR-A2-MIN                     
085800                                     W-IDARTNR-A2                         
085900                                     W-IDARTNR                            
086000       END-IF                                                             
086100     END-IF                                                               
086200                                                                          
086300     .                                                                    
086400     EJECT                                                                
086500 BE-KOLLA-FLTOT      SECTION.                                             
086600                                                                          
086700     MOVE MFS-RENSA-FAELT       TO MOD-FLTOT-IN                           
086800                                                                          
086900     IF MID-FLTOT-IN         NOT = ALL '+'                                
087000       MOVE '7'                 TO MFS-IDPFK                              
087100       MOVE SPACE               TO MFS-KDTRTYP                            
087200       MOVE MID-FLTOT-IN TO WS-FLTOT                                      
087300     ELSE                                                                 
087400       MOVE MID-FLTOT-UT TO WS-FLTOT                                      
087500     END-IF                                                               
087600     IF WS-FLTOT = JA OR YES                                              
087700       CONTINUE                                                           
087800     ELSE                                                                 
087900       MOVE NEJ    TO WS-FLTOT                                            
088000     END-IF                                                               
088100                                                                          
088200     .                                                                    
088300     EJECT                                                                
088400 C-FOERSTA-SIDA SECTION.                                                  
088500                                                                          
088600     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
088700     CALL WMEDKONV USING MED-WMEDAREA                                     
088800     MOVE MED-MFSINF     TO MOD-TEMFSFEL                                  
088900                                                                          
089000*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
089100     PERFORM MFS-RENSA-FAELT-IN                                           
089200     .                                                                    
089300     EJECT                                                                
089400 D-NAESTA-SIDA SECTION.                                                   
089500                                                                          
089600     MOVE MSGI-SPAR-AREA         TO W-MINKEY-X                            
089700     IF W-MINKEY-IDTRANS = '4737'                                         
089800       MOVE W-MINKEY-IDARTNR-NEXT  TO W-IDARTNR-A2-MIN                    
089900       MOVE W-MINKEY-IDRADNR-NEXT  TO W-IDRADNR-A2-MIN                    
090000     ELSE                                                                 
090100       MOVE ZERO                   TO W-IDARTNR-A2-MIN                    
090200                                      W-IDRADNR-A2-MIN                    
090300     END-IF                                                               
090400     .                                                                    
090500     EJECT                                                                
090600 E-SAMMA-SIDA SECTION.                                                    
090700                                                                          
090800     MOVE MSGI-SPAR-AREA         TO W-MINKEY-X                            
090900     IF W-MINKEY-IDTRANS = '4737'                                         
091000       MOVE W-MINKEY-IDARTNR       TO W-IDARTNR-A2-MIN                    
091100       MOVE W-MINKEY-IDRADNR       TO W-IDRADNR-A2-MIN                    
091200       IF MID-INPUT                =  ALL '+' OR 4723-MID                 
091300          PERFORM MFS-RENSA-FAELT-IN                                      
091400       ELSE                                                               
091500          MOVE +1                  TO INDX                                
091600          PERFORM UNTIL INDX       >  MAX-INDX                            
091700             IF MID-KDCMDVAL(INDX) = W-TXT                                
091800                PERFORM EA-STARTA-4723                                    
091900                MOVE JA            TO SW-STARTA-ANNAN-BILD                
092000                MOVE MAX-INDX      TO INDX                                
092100             END-IF                                                       
092200             ADD +1                TO INDX                                
092300          END-PERFORM                                                     
092400          IF STARTA-ANNAN-BILD                                            
092500             CONTINUE                                                     
092600          ELSE                                                            
092700             MOVE INF-PRESS-PF11   TO MED-IDMFSINF                        
092800             CALL WMEDKONV USING MED-WMEDAREA                             
092900             MOVE MED-MFSINF       TO MOD-TEMFSFEL                        
093000          END-IF                                                          
093100       END-IF                                                             
093200     ELSE                                                                 
093300       MOVE ZERO                   TO W-IDARTNR-A2-MIN                    
093400                                      W-IDRADNR-A2-MIN                    
093500     END-IF                                                               
093600     .                                                                    
093700     EJECT                                                                
093800 EA-STARTA-4723        SECTION.                                           
093900                                                                          
094000     INSPECT MID-IDARTNR(INDX) REPLACING LEADING SPACE BY ZERO            
094100     IF MID-IDARTNR(INDX)  NUMERIC                                        
094200       MOVE MID-IDARTNR(INDX)      TO WS-IDARTNR-NUM                      
094300     ELSE                                                                 
094400       MOVE ZERO                   TO WS-IDARTNR-NUM                      
094500     END-IF                                                               
094600     MOVE WS-IDARTNR-NUM         TO MSGI-IDARTNR                          
094700     MOVE MID-IDRADNR(INDX)      TO MSGI-IDRADNR                          
094800     INSPECT MSGI-IDRADNR REPLACING LEADING SPACE BY ZERO                 
094900     MOVE '001'                  TO MSGI-KDCALL                           
095000     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
095100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
095200                                                                          
095300     MOVE MFS-KDMFSFOR          TO P-TO-P3-KDMFSFOR                       
095400     PERFORM IMS-ISRT-MSG-ALT-4723                                        
095500     .                                                                    
095600     EJECT                                                                
095700 F-LAES-VISA-INFO SECTION.                                                
095800                                                                          
095900     PERFORM IMS-GHU-WLKREE01                                             
096000                                                                          
096100     IF SEGMENT-SAKNAS                                                    
096200        MOVE ERR-INFO-MISSING   TO MED-IDMFSFEL                           
096300        CALL WMEDKONV USING MED-WMEDAREA                                  
096400        MOVE MED-MFSFEL         TO MOD-TEMFSFEL                           
096500        PERFORM MFS-RENSA-FAELT-UT                                        
096600     ELSE                                                                 
096700       PERFORM FA-REDIGERA-ANM-UPPGIFTER                                  
096800       PERFORM FF-LAES-NAESTA-RETILLRAD                                   
096900       PERFORM FB-FIXA-ENTER-KEY                                          
097000       MOVE +1                  TO INDX                                   
097100                                                                          
097200       PERFORM UNTIL INDX        > MAX-INDX                               
097300         IF SEGMENT-FINNS                                                 
097400                                                                          
097500            COMPUTE W-KVLEVANM-KVAR   =  LEV-KVLEVANM-BEKR -              
097600                                         LEV-KVRETINL -                   
097700                                         LEV-KVAVV-KVANT -                
097800                                         LEV-KVRETINL-SKR -               
097900                                         LEV-KVAVV-KVAL -                 
098000                                         LEV-KVANTAL-ILI                  
098100                                                                          
098200            IF WS-FLTOT = JA OR YES   OR                                  
098300              W-KVLEVANM-KVAR > ZERO                                      
098400              PERFORM FC-REDIGERA-RAD-UPPGIFTER                           
098500              ADD +1               TO INDX                                
098600            END-IF                                                        
098700                                                                          
098800            PERFORM FF-LAES-NAESTA-RETILLRAD                              
098900         ELSE                                                             
099000            PERFORM FD-RENSA-RAD                                          
099100            ADD +1               TO INDX                                  
099200         END-IF                                                           
099300       END-PERFORM                                                        
099400                                                                          
099500       PERFORM FE-FIXA-NEXT-KEY                                           
099600       MOVE '002'                TO MSGI-KDCALL                           
099700       MOVE '4737'               TO MSGI-IDTRANS                          
099800       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
099900                                                                          
100000     END-IF                                                               
100100     IF MOD-IDILIST-NY NUMERIC                                            
100200        CONTINUE                                                          
100300     ELSE                                                                 
100400        MOVE MFS-RENSA-FAELT    TO MOD-IDILIST-NY                         
100500     END-IF                                                               
100600     .                                                                    
100700     EJECT                                                                
100800                                                                          
100900 FA-REDIGERA-ANM-UPPGIFTER SECTION.                                       
101000                                                                          
101100     MOVE ANM-KDARBTYP       TO MOD-IDANSV   (1:3)                        
101200     MOVE ANM-IDPERSON       TO W-IDPERSON                                
101300     MOVE W-IDPERSON         TO MOD-IDANSV   (4:3)                        
101400     MOVE ANM-KVRADER-RT     TO MOD-KVRADER                               
101500     MOVE ANM-KVRADER-OBEH   TO MOD-KVRADER-OBEH                          
101600     MOVE ANM-DARETILL (3:6) TO MOD-TIRETILL                              
101700                                                                          
101800     IF ANM-KDLEVANM         =  W-ANM-MOT  OR                             
101900        ANM-KDLEVANM         =  W-ANM-PAAB OR                             
102000        ANM-KDLEVANM         =  W-ANM-KLAR                                
102100        PERFORM FAA-LAES-RETUR-UPPGIFTER                                  
102200     ELSE                                                                 
102300        MOVE MFS-RENSA-FAELT TO MOD-ADINLOMR                              
102400                                MOD-IDANSTNR                              
102500                                MOD-TILOSSN                               
102600                                MOD-KVKOLLI                               
102700     END-IF                                                               
102800     .                                                                    
102900     EJECT                                                                
103000                                                                          
103100 FAA-LAES-RETUR-UPPGIFTER SECTION.                                        
103200                                                                          
103300     MOVE ANM-IDDISTR     TO W-IDDISTR-FSEQ-MIN                           
103400                             W-IDDISTR-FSEQ-MAX                           
103500     MOVE ANM-IDKUNDNR    TO W-IDKUNDNR-FSEQ-MIN                          
103600                             W-IDKUNDNR-FSEQ-MAX                          
103700     MOVE ANM-IDRAPPNR    TO W-IDRAPPNR-FSEQ-MIN                          
103800                             W-IDRAPPNR-FSEQ-MAX                          
103900                                                                          
104000     PERFORM IMS-GU-WLRETA01                                              
104100                                                                          
104200     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
104300                                  OR RET-TILOSSN > ZERO                   
104400         PERFORM IMS-GN-WLRETA01                                          
104500     END-PERFORM                                                          
104600     IF SEGMENT-SAKNAS OR SEGMENT-SLUT                                    
104700         MOVE MFS-RENSA-FAELT   TO MOD-KVKOLLI                            
104800                                   MOD-ADINLOMR                           
104900                                   MOD-TILOSSN                            
105000                                   MOD-IDANSTNR                           
105100     ELSE                                                                 
105200         MOVE RET-ADINLOMR      TO MOD-ADINLOMR                           
105300         MOVE RET-IDANSTNR-MOT  TO MOD-IDANSTNR                           
105400         MOVE RET-TILOSSN       TO MOD-TILOSSN                            
105500         MOVE RET-KVKOLLI-AAF   TO MOD-KVKOLLI                            
105600     END-IF                                                               
105700                                                                          
105800     .                                                                    
105900     EJECT                                                                
106000 FB-FIXA-ENTER-KEY        SECTION.                                        
106100                                                                          
106200     IF SEGMENT-FINNS                                                     
106300        MOVE '4737'                 TO W-MINKEY-IDTRANS                   
106400        MOVE LEV-IDARTNR            TO W-MINKEY-IDARTNR                   
106500        MOVE LEV-IDRADNR            TO W-MINKEY-IDRADNR                   
106600     ELSE                                                                 
106700        MOVE ZERO                   TO W-MINKEY-IDARTNR                   
106800                                       W-MINKEY-IDRADNR                   
106900     END-IF                                                               
107000     MOVE W-MINKEY-X                TO MSGI-SPAR-AREA                     
107100                                                                          
107200     .                                                                    
107300     EJECT                                                                
107400                                                                          
107500 FC-REDIGERA-RAD-UPPGIFTER  SECTION.                                      
107600                                                                          
107700                                                                          
107800     MOVE W-KVLEVANM-KVAR      TO MOD-KVANTAL-KVAR (INDX)                 
107900                                                                          
108000     MOVE LEV-IDARTNR          TO MOD-IDARTNR  (INDX)                     
108100     MOVE LEV-KDANMORS         TO MOD-KDANMORS (INDX)                     
108200     MOVE LEV-IDRADNR          TO MOD-IDRADNR  (INDX)                     
108300     IF LEV-IDILIST            >  ZERO                                    
108400        MOVE LEV-IDILIST       TO MOD-IDILIST  (INDX)                     
108500     ELSE                                                                 
108600        MOVE MFS-RENSA-FAELT   TO MOD-IDILIST  (INDX)                     
108700     END-IF                                                               
108800                                                                          
108900     PERFORM FCA-FIXA-ART-UPPGIFTER                                       
109000     PERFORM FCB-KOLLA-OM-TEXTINFO                                        
109100     PERFORM FCC-KOLLA-OM-KONTROLLINFO                                    
109200     .                                                                    
109300     EJECT                                                                
109400                                                                          
109500 FCA-FIXA-ART-UPPGIFTER  SECTION.                                         
109600                                                                          
109700     MOVE LEV-IDARTNR          TO W-IDARTNR                               
109800                                                                          
109900     IF DCS-IDDC NOT = MSGI-IDDC                                          
110000        MOVE MSGI-IDDC TO W-IDDC-B6                                       
110100        PERFORM IMS-GU-WDB601                                             
110200     END-IF                                                               
110300     IF DCS-CDC                                                           
110400       PERFORM IMS-GU-WLARTC11                                            
110500       IF SEGMENT-FINNS                                                   
110600          MOVE CLAG-ADLAGOMR     TO MOD-ADLAGOMR (INDX)                   
110700          MOVE CLAG-ADGANG       TO WS-ADGANG                             
110800          MOVE WS-ADGANG         TO MOD-ADGANG   (INDX)                   
110900          MOVE CLAG-ADPLATS      TO MOD-ADPLATS  (INDX)                   
111000       ELSE                                                               
111100          MOVE MFS-RENSA-FAELT   TO MOD-ADLAGOMR (INDX)                   
111200                                    MOD-ADGANG   (INDX)                   
111300                                    MOD-ADPLATS  (INDX)                   
111400       END-IF                                                             
111500     ELSE                                                                 
111600       MOVE MSGI-IDDC TO W-IDDC                                           
111700       PERFORM IMS-GU-WLARTS11                                            
111800       IF SEGMENT-FINNS                                                   
111900          MOVE SLAG-ADLAGOMR     TO MOD-ADLAGOMR (INDX)                   
112000          MOVE SLAG-ADGANG       TO WS-ADGANG                             
112100          MOVE WS-ADGANG         TO MOD-ADGANG   (INDX)                   
112200          MOVE SLAG-ADPLATS      TO MOD-ADPLATS  (INDX)                   
112300       ELSE                                                               
112400          MOVE MFS-RENSA-FAELT   TO MOD-ADLAGOMR (INDX)                   
112500                                    MOD-ADGANG   (INDX)                   
112600                                    MOD-ADPLATS  (INDX)                   
112700       END-IF                                                             
112800     END-IF                                                               
112900                                                                          
113000     PERFORM IMS-GU-WLBENA11                                              
113100     IF SEGMENT-FINNS                                                     
113200        MOVE TEXT-BEART        TO MOD-BEART (INDX)                        
113300     ELSE                                                                 
113400        MOVE MFS-RENSA-FAELT   TO MOD-BEART (INDX)                        
113500     END-IF                                                               
113600                                                                          
113700     .                                                                    
113800     EJECT                                                                
113900                                                                          
114000 FCB-KOLLA-OM-TEXTINFO   SECTION.                                         
114100                                                                          
114200     MOVE LEV-IDARTNR           TO W-IDARTNR-A2                           
114300     MOVE LEV-IDRADNR           TO W-IDRADNR-A2                           
114400     PERFORM IMS-GNP-WLKREE21                                             
114500     IF SEGMENT-FINNS                                                     
114600        IF MSGI-IDLAND-SPR = 'GB '                                        
114700          MOVE YES                TO MOD-FLTEXT (INDX)                    
114800        ELSE                                                              
114900          MOVE JA                 TO MOD-FLTEXT (INDX)                    
115000        END-IF                                                            
115100     ELSE                                                                 
115200        MOVE MFS-RENSA-FAELT    TO MOD-FLTEXT (INDX)                      
115300     END-IF                                                               
115400     .                                                                    
115500     EJECT                                                                
115600                                                                          
115700 FCC-KOLLA-OM-KONTROLLINFO SECTION.                                       
115800                                                                          
115900     MOVE LEV-IDARTNR           TO W-IDARTNR                              
116000     PERFORM IMS-GU-W6KVAH11                                              
116100     IF SEGMENT-FINNS                                                     
116200        IF MSGI-IDLAND-SPR = 'GB '                                        
116300          MOVE YES                TO MOD-FLKONTROLL (INDX)                
116400        ELSE                                                              
116500          MOVE JA                 TO MOD-FLKONTROLL (INDX)                
116600        END-IF                                                            
116700     ELSE                                                                 
116800       MOVE MFS-RENSA-FAELT     TO MOD-FLKONTROLL (INDX)                  
116900     END-IF                                                               
117000     .                                                                    
117100     EJECT                                                                
117200                                                                          
117300 FD-RENSA-RAD SECTION.                                                    
117400                                                                          
117500     MOVE MFS-STAENG-FAELT TO MOD-KDCMDVAL-ATTR (INDX)                    
117600                              MOD-KVANTAL-ATTR  (INDX)                    
117700                                                                          
117800     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR      (INDX)                      
117900                             MOD-BEART        (INDX)                      
118000                             MOD-KVANTAL-KVAR (INDX)                      
118100                             MOD-KDANMORS     (INDX)                      
118200                             MOD-ADLAGOMR     (INDX)                      
118300                             MOD-ADGANG       (INDX)                      
118400                             MOD-ADPLATS      (INDX)                      
118500                             MOD-IDRADNR      (INDX)                      
118600                             MOD-IDILIST      (INDX)                      
118700                             MOD-FLTEXT       (INDX)                      
118800                             MOD-FLKONTROLL   (INDX)                      
118900     .                                                                    
119000     EJECT                                                                
119100                                                                          
119200 FE-FIXA-NEXT-KEY        SECTION.                                         
119300                                                                          
119400     IF SEGMENT-FINNS                                                     
119500        MOVE INF-MORE-INFO-EXISTS   TO MED-IDMFSINF                       
119600        CALL WMEDKONV USING MED-WMEDAREA                                  
119700        MOVE MED-MFSINF             TO MOD-TEMFSINF                       
119800                                                                          
119900        MOVE '4737'                 TO W-MINKEY-IDTRANS                   
120000        MOVE LEV-IDARTNR            TO W-MINKEY-IDARTNR-NEXT              
120100        MOVE LEV-IDRADNR            TO W-MINKEY-IDRADNR-NEXT              
120200     ELSE                                                                 
120300        MOVE ZERO                   TO W-MINKEY-IDARTNR-NEXT              
120400                                       W-MINKEY-IDRADNR-NEXT              
120500     END-IF                                                               
120600     MOVE W-MINKEY-X                TO MSGI-SPAR-AREA                     
120700                                                                          
120800     .                                                                    
120900     EJECT                                                                
121000 FF-LAES-NAESTA-RETILLRAD SECTION.                                        
121100                                                                          
121200     MOVE NEJ                    TO OKOD-FL-RETILL                        
121300                                    OKOD-FL-INTERNUPPACKNING              
121400     PERFORM IMS-GNP-WLKREE11                                             
121500     PERFORM UNTIL OKOD-FL-RETILL = 'J' OR SEGMENT-SAKNAS                 
121600                OR OKOD-FL-INTERNUPPACKNING = 'J'                         
121700        IF LEV-KDKREBEH(1:1) = 'Y'   OR                                   
121800           LEV-KDKREBEH(1:1) = 'J'   OR                                   
121900           LEV-KDKREBEH(1:1) = 'C'   OR                                   
122000           LEV-KDKREBEH      = 'D01' OR                                   
122100           LEV-KDKREBEH      = 'D02' OR                                   
122200           LEV-KDKREBEH      = 'D03'                                      
122300*--ANROPA KONTROLL AV ORSAKSKODER                                         
122400            MOVE LEV-KDANMORS   TO OKOD-KDANMORS                          
122500            CALL W418OKOD USING OKOD-W418OKOD                             
122600        END-IF                                                            
122700        IF OKOD-FL-RETILL = 'J' OR                                        
122800           OKOD-FL-INTERNUPPACKNING = 'J'                                 
122900           CONTINUE                                                       
123000        ELSE                                                              
123100          PERFORM IMS-GNP-WLKREE11                                        
123200        END-IF                                                            
123300     END-PERFORM                                                          
123400                                                                          
123500     .                                                                    
123600     EJECT                                                                
123700 G-KOLLA-INPUT SECTION.                                                   
123800                                                                          
123900     MOVE JA                      TO INDATA-SW                            
124000     MOVE ZERO                    TO MED-IDMFSFEL                         
124100                                                                          
124200     PERFORM GA-FORMELL-KONTROLL                                          
124300     IF INDATA-OK AND (MFS-UPDATE OR MFS-PRINT)                           
124400        PERFORM GB-LOGISK-KONTROLL                                        
124500     END-IF                                                               
124600                                                                          
124700     IF INDATA-FEL                                                        
124800        IF MED-IDMFSFEL               = ZERO                              
124900           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
125000        END-IF                                                            
125100        CALL WMEDKONV USING MED-WMEDAREA                                  
125200        MOVE MED-MFSFEL              TO MOD-TEMFSFEL                      
125300        PERFORM MFS-ROER-EJ-FAELT-UT                                      
125400        PERFORM MFS-ROER-EJ-FAELT-IN                                      
125500     END-IF                                                               
125600                                                                          
125700     .                                                                    
125800     EJECT                                                                
125900 GA-FORMELL-KONTROLL SECTION.                                             
126000                                                                          
126100     IF MID-INPUT                = ALL '+' AND MFS-UPDATE                 
126200       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
126300       CALL WMEDKONV USING MED-WMEDAREA                                   
126400       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
126500       PERFORM MFS-ROER-EJ-FAELT-IN                                       
126600       PERFORM MFS-ROER-EJ-FAELT-UT                                       
126700       MOVE NEJ                  TO INDATA-SW                             
126800     ELSE                                                                 
126900       IF MFS-PRINT                                                       
127000          PERFORM GAA-KOLLA-IDPRT                                         
127100       END-IF                                                             
127200       PERFORM GAB-KOLLA-IDANSTNR                                         
127300       PERFORM GAC-KOLLA-RADINFO                                          
127400       PERFORM GAD-KOLLA-FLKLAR                                           
127500       PERFORM GAG-KOLLA-FLSKROT                                          
127600       PERFORM GAH-KOLLA-FLANTAVV                                         
127700       PERFORM GAE-KOLLA-FLILI                                            
127800       PERFORM GAF-KOLLA-RELATION                                         
127900       PERFORM GAI-KOLLA-DISTRICT                                         
128000     END-IF                                                               
128100                                                                          
128200     .                                                                    
128300     EJECT                                                                
128400                                                                          
128500 GAA-KOLLA-IDPRT      SECTION.                                            
128600                                                                          
128700     IF MID-IDPRT                  NOT = ALL '+'                          
128800         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPRT-ATTR                      
128900     END-IF                                                               
129000                                                                          
129100     .                                                                    
129200     EJECT                                                                
129300                                                                          
129400 GAB-KOLLA-IDANSTNR   SECTION.                                            
129500                                                                          
129600     MOVE NEJ                        TO SW-IDANSTNR                       
129700     IF MID-IDANSTNR-UPD             NOT = ALL '+'                        
129800        IF MID-IDANSTNR-UPD NUMERIC                                       
129900           MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDANSTNR-UPD-ATTR             
130000           MOVE JA                   TO SW-IDANSTNR                       
130100        ELSE                                                              
130200           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDANSTNR-UPD-ATTR             
130300           MOVE NEJ                  TO INDATA-SW                         
130400        END-IF                                                            
130500     ELSE                                                                 
130600        MOVE MFS-NUM-FAELT-FEL    TO MOD-IDANSTNR-UPD-ATTR                
130700        MOVE NEJ                  TO INDATA-SW                            
130800     END-IF                                                               
130900                                                                          
131000     .                                                                    
131100     EJECT                                                                
131200                                                                          
131300 GAC-KOLLA-RADINFO    SECTION.                                            
131400                                                                          
131500     MOVE +1                           TO INDX                            
131600                                                                          
131700     PERFORM UNTIL INDX                 >  MAX-INDX                       
131800        IF MID-KDCMDVAL(INDX)           NOT = ALL '+'                     
131900           MOVE JA TO SW-RAD-CMD                                          
132000                                                                          
132100           IF MID-KDCMDVAL(INDX)(1:1) NUMERIC                             
132200             PERFORM GACA-FIXA-ILISTE-NUMMER                              
132300           END-IF                                                         
132400                                                                          
132500           MOVE MID-KDCMDVAL(INDX)      TO TEST-KDCMDVAL                  
132600           IF GODK-KDCMDVAL OR MID-KDCMDVAL(INDX) NUMERIC                 
132700              MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMDVAL-ATTR(INDX)        
132800           ELSE                                                           
132900              MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMDVAL-ATTR(INDX)        
133000              MOVE NEJ                  TO INDATA-SW                      
133100           END-IF                                                         
133200                                                                          
133300           IF MID-KVANTAL(INDX)           NOT = ALL '+'                   
133400              IF MID-KVANTAL(INDX)        NUMERIC                         
133500                MOVE MFS-NUM-FAELT-RAETT  TO                              
133600                                          MOD-KVANTAL-ATTR(INDX)          
133700              ELSE                                                        
133800                MOVE MFS-NUM-FAELT-FEL    TO                              
133900                                          MOD-KVANTAL-ATTR(INDX)          
134000                MOVE NEJ                  TO INDATA-SW                    
134100              END-IF                                                      
134200           END-IF                                                         
134300                                                                          
134400        END-IF                                                            
134500        ADD +1                          TO INDX                           
134600     END-PERFORM                                                          
134700     .                                                                    
134800     EJECT                                                                
134900                                                                          
135000 GACA-FIXA-ILISTE-NUMMER SECTION.                                         
135100                                                                          
135200     IF MID-KDCMDVAL(INDX)(2:1) NUMERIC                                   
135300       IF MID-KDCMDVAL(INDX)(3:1) NUMERIC                                 
135400         CONTINUE                                                         
135500       ELSE                                                               
135600         MOVE MID-KDCMDVAL(INDX)(1:2) TO WS-KDCMDVAL(2:2)                 
135700         MOVE ZERO                    TO WS-KDCMDVAL(1:1)                 
135800         MOVE WS-KDCMDVAL             TO MID-KDCMDVAL(INDX)               
135900       END-IF                                                             
136000     ELSE                                                                 
136100       MOVE MID-KDCMDVAL(INDX)(1:1) TO WS-KDCMDVAL(3:1)                   
136200       MOVE ZERO                    TO WS-KDCMDVAL(1:2)                   
136300       MOVE WS-KDCMDVAL             TO MID-KDCMDVAL(INDX)                 
136400     END-IF                                                               
136500     .                                                                    
136600     EJECT                                                                
136700 GAD-KOLLA-FLKLAR     SECTION.                                            
136800                                                                          
136900     MOVE NEJ                         TO SW-ALLT-INLAGT                   
137000                                                                          
137100     IF MID-FLKLAR                    NOT = ALL '+'                       
137200        IF MID-FLKLAR                 =  JA OR NEJ OR YES                 
137300           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-FLKLAR-ATTR                  
137400           IF MID-FLKLAR              =  JA OR YES                        
137500               MOVE JA                TO SW-ALLT-INLAGT                   
137600           END-IF                                                         
137700        ELSE                                                              
137800           MOVE MFS-ALFA-FAELT-FEL    TO MOD-FLKLAR-ATTR                  
137900           MOVE NEJ                   TO INDATA-SW                        
138000        END-IF                                                            
138100     END-IF                                                               
138200                                                                          
138300     .                                                                    
138400     EJECT                                                                
138500                                                                          
138600 GAE-KOLLA-FLILI      SECTION.                                            
138700                                                                          
138800     MOVE NEJ                         TO SW-ALLT-TILL-ILI                 
138900                                                                          
139000     IF MID-FLILI                     NOT = ALL '+'                       
139100        IF MID-FLILI                  =  JA OR NEJ OR YES                 
139200           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-FLILI-ATTR                   
139300           IF MID-FLILI               =  JA OR YES                        
139400               MOVE JA                TO SW-ALLT-TILL-ILI                 
139500           END-IF                                                         
139600        ELSE                                                              
139700           MOVE MFS-ALFA-FAELT-FEL    TO MOD-FLILI-ATTR                   
139800           MOVE NEJ                   TO INDATA-SW                        
139900        END-IF                                                            
140000     END-IF                                                               
140100                                                                          
140200     .                                                                    
140300     EJECT                                                                
140400                                                                          
140500 GAF-KOLLA-RELATION SECTION.                                              
140600                                                                          
140700     IF MFS-PRINT                                                         
140800       IF RAD-CMD     OR                                                  
140900          ALLT-INLAGT OR                                                  
141000          ALLT-SKROT  OR                                                  
141100          ALLT-ANT-AVV   OR                                               
141200          ALLT-TILL-ILI                                                   
141300         MOVE NEJ TO INDATA-SW                                            
141400         MOVE ERR-FLERA-FUNKTIONER TO MED-IDMFSFEL                        
141500       END-IF                                                             
141600     ELSE                                                                 
141700       IF (RAD-CMD AND ALLT-INLAGT)     OR                                
141800          (RAD-CMD AND ALLT-TILL-ILI)   OR                                
141900          (RAD-CMD AND ALLT-SKROT)      OR                                
142000          (RAD-CMD AND ALLT-ANT-AVV)    OR                                
142100          (ALLT-TILL-ILI AND ALLT-INLAGT)    OR                           
142200          (ALLT-TILL-ILI AND ALLT-SKROT)     OR                           
142300          (ALLT-TILL-ILI AND ALLT-ANT-AVV)   OR                           
142400          (ALLT-INLAGT AND ALLT-SKROT)       OR                           
142500          (ALLT-INLAGT AND ALLT-ANT-AVV)     OR                           
142600          (ALLT-SKROT AND ALLT-ANT-AVV)                                   
142700         MOVE NEJ TO INDATA-SW                                            
142800         MOVE ERR-FLERA-FUNKTIONER TO MED-IDMFSFEL                        
142900       END-IF                                                             
143000     END-IF                                                               
143100     .                                                                    
143200     EJECT                                                                
143300 GAG-KOLLA-FLSKROT    SECTION.                                            
143400                                                                          
143500     MOVE NEJ                         TO SW-ALLT-SKROT                    
143600                                                                          
143700     IF MID-FLSKROT                   NOT = ALL '+'                       
143800        IF MID-FLSKROT                =  JA OR NEJ OR YES                 
143900           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-FLSKROT-ATTR                 
144000           IF MID-FLSKROT             =  JA OR YES                        
144100               MOVE JA                TO SW-ALLT-SKROT                    
144200           END-IF                                                         
144300        ELSE                                                              
144400           MOVE MFS-ALFA-FAELT-FEL    TO MOD-FLSKROT-ATTR                 
144500           MOVE NEJ                   TO INDATA-SW                        
144600        END-IF                                                            
144700     END-IF                                                               
144800                                                                          
144900     .                                                                    
145000     EJECT                                                                
145100 GAH-KOLLA-FLANTAVV   SECTION.                                            
145200                                                                          
145300     MOVE NEJ                         TO SW-ALLT-ANT-AVV                  
145400                                                                          
145500     IF MID-FLANTAVV                  NOT = ALL '+'                       
145600        IF MID-FLANTAVV               =  JA OR NEJ OR YES                 
145700           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-FLANTAVV-ATTR                
145800           IF MID-FLANTAVV            =  JA OR YES                        
145900               MOVE JA                TO SW-ALLT-ANT-AVV                  
146000           END-IF                                                         
146100        ELSE                                                              
146200           MOVE MFS-ALFA-FAELT-FEL    TO MOD-FLANTAVV-ATTR                
146300           MOVE NEJ                   TO INDATA-SW                        
146400        END-IF                                                            
146500     END-IF                                                               
146600                                                                          
146700     .                                                                    
146800     EJECT                                                                
146900 GAI-KOLLA-DISTRICT   SECTION.                                            
147000                                                                          
147100     MOVE W-IDDISTR                        TO DIST35-IDDISTR              
147200     IF DIST35-CN-CDC-RETURNS                                             
147210     OR DIST35-TR-CDC-RETURNS                                             
147300       MOVE +1                             TO INDX                        
147400       PERFORM UNTIL INDX > MAX-INDX                                      
147500         IF MID-KDCMDVAL(INDX) =   ALL '+' OR SPACE                       
147600            CONTINUE                                                      
147700         ELSE                                                             
147800            IF MID-KDCMDVAL(INDX) = W-ANT OR W-DEV OR                     
147900                                    W-KVA OR W-QDE                        
148000               MOVE ERR-FORBIDDEN-UPDATE   TO MED-IDMFSFEL                
148100               MOVE NEJ                    TO INDATA-SW                   
148200            END-IF                                                        
148300         END-IF                                                           
148400         ADD +1                            TO INDX                        
148500       END-PERFORM                                                        
148600     END-IF                                                               
148700     .                                                                    
148800     EJECT                                                                
148900 GB-LOGISK-KONTROLL SECTION.                                              
149000                                                                          
149100     IF MFS-PRINT                                                         
149200        CONTINUE                                                          
149300     ELSE                                                                 
149400        PERFORM GBA-KOLLA-KDLEVANM-IDDC                                   
149500        IF INDATA-OK                                                      
149600          PERFORM GBF-KOLLA-MOTTAGET                                      
149700        END-IF                                                            
149800     END-IF                                                               
149900                                                                          
150000     IF INDATA-OK                                                         
150100        IF MFS-PRINT                                                      
150200           PERFORM GBB-KOLLA-IDPRT                                        
150300        ELSE                                                              
150400           IF RAD-CMD                                                     
150500             PERFORM GBC-KOLLA-RADINFO                                    
150600           ELSE                                                           
150700             IF ALLT-INLAGT OR ALLT-SKROT OR ALLT-ANT-AVV                 
150800               PERFORM GBD-KOLLA-ALLT-INLAGT                              
150900             ELSE                                                         
151000               PERFORM GBE-KOLLA-ALLT-ILI                                 
151100             END-IF                                                       
151200           END-IF                                                         
151300        END-IF                                                            
151400     END-IF                                                               
151500                                                                          
151600     .                                                                    
151700     EJECT                                                                
151800                                                                          
151900 GBA-KOLLA-KDLEVANM-IDDC          SECTION.                                
152000                                                                          
152100     PERFORM IMS-GHU-WLKREE01                                             
152200     IF SEGMENT-FINNS                                                     
152300         IF ANM-KDLEVANM              =  W-ANM-MOT OR                     
152400                                         W-ANM-PAAB                       
152500            CONTINUE                                                      
152600         ELSE                                                             
152700            MOVE ERR-FORBIDDEN-UPDATE TO MED-IDMFSFEL                     
152800            MOVE NEJ                  TO INDATA-SW                        
153000         END-IF                                                           
153100         PERFORM HS0-LAES-WLKREE11                                        
153200         IF SEGMENT-FINNS                                                 
153300           IF LEV-IDDC-RET = MSGI-IDDC                                    
153400             CONTINUE                                                     
153500           ELSE                                                           
153600             MOVE ERR-FORBIDDEN-UPDATE    TO MED-IDMFSFEL                 
153700             MOVE NEJ                     TO INDATA-SW                    
153800           END-IF                                                         
153900         END-IF                                                           
154000     ELSE                                                                 
154100         MOVE ERR-FORBIDDEN-UPDATE    TO MED-IDMFSFEL                     
154200         MOVE NEJ                     TO INDATA-SW                        
154300     END-IF                                                               
154400     .                                                                    
154500     EJECT                                                                
154600                                                                          
154700 GBB-KOLLA-IDPRT                  SECTION.                                
154800                                                                          
154900     MOVE SPACE                TO PRT-IDPRTLST                            
155000     MOVE '4RT'                TO PRT-IDPRTLST(1:3)                       
155100                                                                          
155200     MOVE MID-IDPRT            TO PRT-IDPRTLST(4:3)                       
155300     MOVE 1                    TO PRT-KDCALL                              
155400     CALL W006PRT USING PRT-W006PRT                                       
155500                                                                          
155600     IF PRT-KDSVAR                     = 'F'                              
155700         MOVE ERR-WRONG-PRINTER        TO MED-IDMFSFEL                    
155800         MOVE PRT-IDPRTLST             TO MED-TEMFSINF                    
155900         MOVE NEJ                      TO INDATA-SW                       
156000         MOVE MFS-ALFA-FAELT-FEL       TO MOD-IDPRT-ATTR                  
156100     END-IF                                                               
156200     .                                                                    
156300     EJECT                                                                
156400                                                                          
156500 GBC-KOLLA-RADINFO    SECTION.                                            
156600                                                                          
156700     MOVE +1                           TO INDX                            
156800                                                                          
156900     PERFORM UNTIL INDX                >  MAX-INDX                        
157000        IF MID-KDCMDVAL(INDX)          NOT = ALL '+'                      
157100                                                                          
157200           IF MID-KDCMDVAL(INDX)       NUMERIC OR                         
157300              MID-KDCMDVAL(INDX)       = W-ILI OR W-BLI                   
157400              PERFORM GBCA-KOLLA-IDILIST                                  
157500           ELSE                                                           
157600              PERFORM GBCB-KOLLA-RAD-OK                                   
157700           END-IF                                                         
157800                                                                          
157900        END-IF                                                            
158000        ADD +1                          TO INDX                           
158100     END-PERFORM                                                          
158200     .                                                                    
158300     EJECT                                                                
158400                                                                          
158500                                                                          
158600 GBCA-KOLLA-IDILIST    SECTION.                                           
158700                                                                          
158800     MOVE MID-IDARTNR (INDX)     TO W-IDARTNR-A2                          
158900     MOVE MID-IDRADNR (INDX)     TO W-IDRADNR-A2                          
159000                                                                          
159100     PERFORM IMS-GNP-WLKREE11-UNIK                                        
159200                                                                          
159300     IF SEGMENT-FINNS                                                     
159400        IF MID-KDCMDVAL(INDX)               = W-ILI OR W-BLI              
159500          IF LEV-IDILIST                  = ZERO                          
159600            IF MID-KVANTAL(INDX) NUMERIC                                  
159700              MOVE MID-KVANTAL (INDX)   TO WS-KVANTAL                     
159800            ELSE                                                          
159900              MOVE ZERO                 TO WS-KVANTAL                     
160000            END-IF                                                        
160100            IF WS-KVANTAL                >                                
160200              (LEV-KVLEVANM-BEKR -                                        
160300               LEV-KVRETINL -                                             
160400               LEV-KVAVV-KVANT -                                          
160500               LEV-KVRETINL-SKR -                                         
160600               LEV-KVAVV-KVAL -                                           
160700               LEV-KVANTAL-ILI)                                           
160800              MOVE MFS-ALFA-FAELT-FEL TO MOD-KVANTAL-ATTR(INDX)           
160900              MOVE NEJ                TO INDATA-SW                        
161000            END-IF                                                        
161100          END-IF                                                          
161200          IF LEV-IDILIST                  = ZERO AND                      
161300            (LEV-KVLEVANM-BEKR            >                               
161400             LEV-KVRETINL                 +                               
161500             LEV-KVRETINL-SKR             +                               
161600             LEV-KVAVV-KVANT              +                               
161700             LEV-KVAVV-KVAL)                                              
161800             CONTINUE                                                     
161900          ELSE                                                            
162000              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMDVAL-ATTR(INDX)          
162100              MOVE NEJ                TO INDATA-SW                        
162200          END-IF                                                          
162300        END-IF                                                            
162400                                                                          
162500        IF MID-KDCMDVAL(INDX)  NUMERIC   AND                              
162600           LEV-IDILIST > ZERO                                             
162700                                                                          
162800          MOVE MID-KDCMDVAL(INDX)  TO W-KDCMDVAL-NUM                      
162900                                                                          
163000          IF W-KDCMDVAL-NUM = LEV-IDILIST                                 
163100            IF MID-KVANTAL(INDX) NUMERIC                                  
163200              MOVE MID-KVANTAL (INDX)   TO WS-KVANTAL                     
163300            ELSE                                                          
163400              MOVE ZERO                 TO WS-KVANTAL                     
163500            END-IF                                                        
163600            IF WS-KVANTAL                >                                
163700              (LEV-KVLEVANM-BEKR -                                        
163800               LEV-KVRETINL -                                             
163900               LEV-KVAVV-KVANT -                                          
164000               LEV-KVRETINL-SKR -                                         
164100               LEV-KVAVV-KVAL -                                           
164200               LEV-KVANTAL-ILI)                                           
164300              MOVE MFS-ALFA-FAELT-FEL TO MOD-KVANTAL-ATTR(INDX)           
164400              MOVE NEJ                TO INDATA-SW                        
164500            END-IF                                                        
164600                                                                          
164700            IF INDATA-OK                                                  
164800              IF ( W-KDCMDVAL-NUM = LEV-IDILIST )  AND                    
164900                ((LEV-KVLEVANM-BEKR        >                              
165000                  LEV-KVRETINL             +                              
165100                  LEV-KVRETINL-SKR         +                              
165200                  LEV-KVAVV-KVANT          +                              
165300                  LEV-KVAVV-KVAL           +                              
165400                  LEV-KVANTAL-ILI))                                       
165500                                                                          
165600                MOVE MID-KDCMDVAL(INDX) TO W-IDILIST-E1-MIN               
165700                                             W-IDILIST-E1-MAX             
165800                                                                          
165900                PERFORM IMS-GU-WLKREJ01                                   
166000                IF SEGMENT-SAKNAS OR SEQE-TIUTSKR > ZERO                  
166100                   MOVE MFS-ALFA-FAELT-FEL TO                             
166200                                        MOD-KDCMDVAL-ATTR(INDX)           
166300                   MOVE NEJ           TO INDATA-SW                        
166400                END-IF                                                    
166500              ELSE                                                        
166600                MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMDVAL-ATTR(INDX)        
166700                MOVE NEJ                TO INDATA-SW                      
166800              END-IF                                                      
166900            END-IF                                                        
167000          ELSE                                                            
167100            MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMDVAL-ATTR(INDX)            
167200            MOVE NEJ                    TO INDATA-SW                      
167300          END-IF                                                          
167400        END-IF                                                            
167500                                                                          
167600        IF MID-KDCMDVAL (INDX)  NUMERIC   AND                             
167700           LEV-IDILIST = ZERO                                             
167800                                                                          
167900          IF MID-KVANTAL(INDX) NUMERIC                                    
168000            MOVE MID-KVANTAL (INDX)   TO WS-KVANTAL                       
168100          ELSE                                                            
168200            MOVE ZERO                 TO WS-KVANTAL                       
168300          END-IF                                                          
168400          IF WS-KVANTAL                >                                  
168500            (LEV-KVLEVANM-BEKR -                                          
168600             LEV-KVRETINL -                                               
168700             LEV-KVAVV-KVANT -                                            
168800             LEV-KVRETINL-SKR -                                           
168900             LEV-KVAVV-KVAL)                                              
169000            MOVE MFS-ALFA-FAELT-FEL TO MOD-KVANTAL-ATTR(INDX)             
169100            MOVE NEJ                TO INDATA-SW                          
169200          END-IF                                                          
169300                                                                          
169400          IF LEV-KVLEVANM-BEKR            >                               
169500             LEV-KVRETINL                 +                               
169600             LEV-KVRETINL-SKR             +                               
169700             LEV-KVAVV-KVANT              +                               
169800             LEV-KVAVV-KVAL                                               
169900                                                                          
170000             MOVE MID-KDCMDVAL(INDX)  TO W-IDILIST-E1-MIN                 
170100                                         W-IDILIST-E1-MAX                 
170200                                                                          
170300             PERFORM IMS-GU-WLKREJ01                                      
170400             IF SEGMENT-SAKNAS OR SEQE-TIUTSKR > ZERO                     
170500                MOVE MFS-ALFA-FAELT-FEL TO                                
170600                                     MOD-KDCMDVAL-ATTR(INDX)              
170700                MOVE NEJ               TO INDATA-SW                       
170800             END-IF                                                       
170900          ELSE                                                            
171000            MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMDVAL-ATTR(INDX)            
171100            MOVE NEJ                    TO INDATA-SW                      
171200          END-IF                                                          
171300        END-IF                                                            
171400     ELSE                                                                 
171500        MOVE MFS-ALFA-FAELT-FEL         TO MOD-KDCMDVAL-ATTR(INDX)        
171600        MOVE NEJ                        TO INDATA-SW                      
171700     END-IF                                                               
171800     .                                                                    
171900     EJECT                                                                
172000                                                                          
172100 GBCB-KOLLA-RAD-OK     SECTION.                                           
172200                                                                          
172300     MOVE MID-IDARTNR (INDX)     TO W-IDARTNR-A2                          
172400     MOVE MID-IDRADNR (INDX)     TO W-IDRADNR-A2                          
172500                                                                          
172600     PERFORM IMS-GNP-WLKREE11-UNIK                                        
172700                                                                          
172800     IF SEGMENT-FINNS                                                     
172900        IF LEV-KVLEVANM-BEKR            =                                 
173000           LEV-KVRETINL                 +                                 
173100           LEV-KVRETINL-SKR             +                                 
173200           LEV-KVAVV-KVANT              +                                 
173300           LEV-KVAVV-KVAL               +                                 
173400           LEV-KVANTAL-ILI                                                
173500            MOVE MFS-ALFA-FAELT-FEL     TO MOD-KDCMDVAL-ATTR(INDX)        
173600            MOVE NEJ                    TO INDATA-SW                      
173700        ELSE                                                              
173800          IF LEV-IDILIST > ZERO                                           
173900            IF MID-KVANTAL(INDX) NUMERIC                                  
174000              MOVE MID-KVANTAL (INDX)   TO WS-KVANTAL                     
174100            ELSE                                                          
174200              MOVE ZERO                 TO WS-KVANTAL                     
174300            END-IF                                                        
174400            IF (LEV-KVLEVANM-BEKR            -                            
174500                LEV-KVRETINL                 -                            
174600                LEV-KVRETINL-SKR             -                            
174700                LEV-KVAVV-KVANT              -                            
174800                LEV-KVAVV-KVAL               -                            
174900                LEV-KVANTAL-ILI)     <   WS-KVANTAL                       
175000              MOVE MFS-ALFA-FAELT-FEL  TO MOD-KVANTAL-ATTR(INDX)          
175100              MOVE NEJ                 TO INDATA-SW                       
175200            END-IF                                                        
175300          ELSE                                                            
175400            IF MID-KVANTAL(INDX) NUMERIC                                  
175500              MOVE MID-KVANTAL (INDX)   TO WS-KVANTAL                     
175600            ELSE                                                          
175700              MOVE ZERO                 TO WS-KVANTAL                     
175800            END-IF                                                        
175900            IF (LEV-KVLEVANM-BEKR            -                            
176000                LEV-KVRETINL                 -                            
176100                LEV-KVRETINL-SKR             -                            
176200                LEV-KVAVV-KVANT              -                            
176300                LEV-KVAVV-KVAL)     <   WS-KVANTAL                        
176400              MOVE MFS-ALFA-FAELT-FEL  TO MOD-KVANTAL-ATTR(INDX)          
176500              MOVE NEJ                 TO INDATA-SW                       
176600            END-IF                                                        
176700          END-IF                                                          
176800        END-IF                                                            
176900        IF MID-KDCMDVAL (INDX) = W-ANT OR W-KVA OR W-DEV OR W-QDE         
177000          IF LEV-KDANMORS = 97                                            
177100            MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDCMDVAL-ATTR(INDX)           
177200            MOVE NEJ                 TO INDATA-SW                         
177300          END-IF                                                          
177400        END-IF                                                            
177410*---    KOLLA ATT LAGERPLATS FINNS OM ÅTG = INL/BIN                       
177411        IF MID-KDCMDVAL (INDX) = W-INL OR W-BIN                           
177412          IF LEV-IDARTNR NOT = 100                                        
177420            PERFORM GBCBA-KOLLA-LAGERPLATS                                
177421          END-IF                                                          
177430        END-IF                                                            
177500     ELSE                                                                 
177600        MOVE MFS-ALFA-FAELT-FEL         TO MOD-KDCMDVAL-ATTR(INDX)        
177700        MOVE NEJ                        TO INDATA-SW                      
177800     END-IF                                                               
177900     .                                                                    
178000     EJECT                                                                
178001                                                                          
178010 GBCBA-KOLLA-LAGERPLATS  SECTION.                                         
178020                                                                          
178030     MOVE LEV-IDARTNR          TO W-IDARTNR                               
178040                                                                          
178050     IF DCS-IDDC NOT = MSGI-IDDC                                          
178060        MOVE MSGI-IDDC TO W-IDDC-B6                                       
178070        PERFORM IMS-GU-WDB601                                             
178080     END-IF                                                               
178090     IF DCS-CDC                                                           
178091       MOVE MSGI-IDDC TO W-IDDC                                           
178092       PERFORM IMS-GU-WLARTC11                                            
178093       IF SEGMENT-FINNS                                                   
178094         IF CLAG-ADLAGOMR > ZERO OR                                       
178095           CLAG-ADGANG   > ZERO OR                                        
178096           CLAG-ADPLATS  > ZERO                                           
178097           CONTINUE                                                       
178098         ELSE                                                             
178099           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMDVAL-ATTR(INDX)             
178101           MOVE ERR-LAGER-SAKNAS   TO MED-IDMFSFEL                        
178102           MOVE NEJ                TO INDATA-SW                           
178103         END-IF                                                           
178104       ELSE                                                               
178105         MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDCMDVAL-ATTR(INDX)              
178106         MOVE ERR-LAGER-SAKNAS    TO MED-IDMFSFEL                         
178107         MOVE NEJ                 TO INDATA-SW                            
178108       END-IF                                                             
178109     ELSE                                                                 
178110       MOVE MSGI-IDDC TO W-IDDC                                           
178111       PERFORM IMS-GU-WLARTS11                                            
178112       IF SEGMENT-FINNS                                                   
178113         IF SLAG-ADLAGOMR > ZERO OR                                       
178114           SLAG-ADGANG   > ZERO OR                                        
178115           SLAG-ADPLATS  > ZERO                                           
178116           CONTINUE                                                       
178117         ELSE                                                             
178118           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMDVAL-ATTR(INDX)             
178119           MOVE ERR-LAGER-SAKNAS   TO MED-IDMFSFEL                        
178120           MOVE NEJ                TO INDATA-SW                           
178121         END-IF                                                           
178122       ELSE                                                               
178123         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMDVAL-ATTR(INDX)             
178124         MOVE ERR-LAGER-SAKNAS     TO MED-IDMFSFEL                        
178125         MOVE NEJ                  TO INDATA-SW                           
178126       END-IF                                                             
178127     END-IF                                                               
178128                                                                          
178129     .                                                                    
178130     EJECT                                                                
178140                                                                          
178200 GBD-KOLLA-ALLT-INLAGT SECTION.                                           
178300                                                                          
178400     PERFORM IMS-GHU-WLKREE01                                             
178500                                                                          
178600     PERFORM HS0-LAES-WLKREE11                                            
178700     PERFORM UNTIL SEGMENT-SAKNAS OR INDATA-FEL                           
178800       IF LEV-IDILIST > ZERO                                              
178900         MOVE NEJ                        TO INDATA-SW                     
179000         MOVE ERR-RAPPORTERING-STARTAD   TO MED-IDMFSFEL                  
179100       END-IF                                                             
179200       IF LEV-KVLEVANM-BEKR NOT =                                         
179300          LEV-KVAVV-KVAL        +                                         
179400          LEV-KVAVV-KVANT       +                                         
179500          LEV-KVRETINL          +                                         
179600          LEV-KVRETINL-SKR                                                
179700         MOVE NEJ     TO SW-KLART                                         
179800       END-IF                                                             
179801       IF INDATA-OK                                                       
179802         IF LEV-IDARTNR NOT = 100                                         
179810           PERFORM GBDA-KOLLA-LAGERPLATS                                  
179811         END-IF                                                           
179820       END-IF                                                             
179900       PERFORM HS0-LAES-WLKREE11                                          
180000     END-PERFORM                                                          
180100     IF KLART                                                             
180200       MOVE NEJ                        TO INDATA-SW                       
180300       MOVE ERR-RAPPORTERING-KLAR      TO MED-IDMFSFEL                    
180400     END-IF                                                               
180500     .                                                                    
180510 GBDA-KOLLA-LAGERPLATS  SECTION.                                          
180520                                                                          
180530     MOVE LEV-IDARTNR          TO W-IDARTNR                               
180540                                                                          
180550     IF DCS-IDDC NOT = MSGI-IDDC                                          
180560        MOVE MSGI-IDDC TO W-IDDC-B6                                       
180570        PERFORM IMS-GU-WDB601                                             
180580     END-IF                                                               
180590     IF DCS-CDC                                                           
180591       PERFORM IMS-GU-WLARTC11                                            
180592       MOVE MSGI-IDDC TO W-IDDC                                           
180593       IF SEGMENT-FINNS                                                   
180594         IF CLAG-ADLAGOMR > ZERO OR                                       
180595           CLAG-ADGANG   > ZERO OR                                        
180596           CLAG-ADPLATS  > ZERO                                           
180597           CONTINUE                                                       
180598         ELSE                                                             
180600           MOVE NEJ               TO INDATA-SW                            
180601           MOVE ERR-LAGER-SAKNAS      TO MED-IDMFSFEL                     
180602         END-IF                                                           
180603       ELSE                                                               
180604         MOVE NEJ                   TO INDATA-SW                          
180605         MOVE ERR-LAGER-SAKNAS      TO MED-IDMFSFEL                       
180606       END-IF                                                             
180607     ELSE                                                                 
180608       MOVE MSGI-IDDC TO W-IDDC                                           
180609       PERFORM IMS-GU-WLARTS11                                            
180611       IF SEGMENT-FINNS                                                   
180612         IF SLAG-ADLAGOMR > ZERO OR                                       
180613           SLAG-ADGANG   > ZERO OR                                        
180614           SLAG-ADPLATS  > ZERO                                           
180615           CONTINUE                                                       
180616         ELSE                                                             
180617           MOVE NEJ               TO INDATA-SW                            
180618           MOVE ERR-LAGER-SAKNAS      TO MED-IDMFSFEL                     
180619         END-IF                                                           
180620       ELSE                                                               
180621         MOVE NEJ                   TO INDATA-SW                          
180622         MOVE ERR-LAGER-SAKNAS      TO MED-IDMFSFEL                       
180623       END-IF                                                             
180624     END-IF                                                               
180625                                                                          
180626     .                                                                    
180627     EJECT                                                                
180628                                                                          
180630     EJECT                                                                
180700 GBE-KOLLA-ALLT-ILI    SECTION.                                           
180800                                                                          
180900     PERFORM IMS-GHU-WLKREE01                                             
181000                                                                          
181100     PERFORM HS0-LAES-WLKREE11                                            
181200     PERFORM UNTIL SEGMENT-SAKNAS OR EJ-KLART                             
181300       IF LEV-IDILIST = ZERO AND                                          
181400         (LEV-KVLEVANM-BEKR NOT =                                         
181500          LEV-KVAVV-KVAL        +                                         
181600          LEV-KVAVV-KVANT       +                                         
181700          LEV-KVRETINL          +                                         
181800          LEV-KVRETINL-SKR)                                               
181900         MOVE NEJ     TO SW-KLART                                         
182000       END-IF                                                             
182100       PERFORM HS0-LAES-WLKREE11                                          
182200     END-PERFORM                                                          
182300     IF KLART                                                             
182400       MOVE NEJ                        TO INDATA-SW                       
182500       MOVE ERR-RAPPORTERING-KLAR      TO MED-IDMFSFEL                    
182600     END-IF                                                               
182700     .                                                                    
182800     EJECT                                                                
182900 GBF-KOLLA-MOTTAGET       SECTION.                                        
183000                                                                          
183100     MOVE W-IDDISTR       TO W-IDDISTR-FSEQ-MIN                           
183200                             W-IDDISTR-FSEQ-MAX                           
183300     MOVE W-IDKUNDNR      TO W-IDKUNDNR-FSEQ-MIN                          
183400                             W-IDKUNDNR-FSEQ-MAX                          
183500     MOVE W-IDRAPPNR      TO W-IDRAPPNR-FSEQ-MIN                          
183600                             W-IDRAPPNR-FSEQ-MAX                          
183700                                                                          
183800     PERFORM IMS-GU-WLRETA01                                              
183900                                                                          
184000     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
184100                                  OR RET-TIINLMOT > ZERO                  
184200         PERFORM IMS-GN-WLRETA01                                          
184300     END-PERFORM                                                          
184400     IF SEGMENT-SAKNAS OR SEGMENT-SLUT                                    
184500         MOVE NEJ TO INDATA-SW                                            
184600     END-IF                                                               
184700     .                                                                    
184800     EJECT                                                                
184900 H-UPPDATERA-SKRIV-UT SECTION.                                            
185000                                                                          
185100     MOVE +1                     TO 4797-INDX                             
185200                                    4797-IX2                              
185300                                                                          
185400     IF MFS-PRINT                                                         
185500        PERFORM HA-SKRIV-UT-TILLSTAND                                     
185600        MOVE INF-PRINT-BEG       TO MED-IDMFSINF                          
185700     ELSE                                                                 
185800        IF ALLT-INLAGT                                                    
185900           PERFORM HB-UPPDATERA-ALLT-INLAGT                               
186000        ELSE                                                              
186100          IF ALLT-ANT-AVV                                                 
186200             PERFORM HF-UPPDATERA-ALLT-ANT-AVV                            
186300          ELSE                                                            
186400             IF ALLT-TILL-ILI                                             
186500                PERFORM HC-UPPDATERA-ALLT-TILL-ILI                        
186600             ELSE                                                         
186700                IF ALLT-SKROT                                             
186800                  PERFORM HE-UPPDATERA-ALLT-SKROT                         
186900                  MOVE 'ALLT SKROT' TO INFO-1                             
187000                  ADD +1 TO INFO-1-ANTAL                                  
187100                ELSE                                                      
187200                  PERFORM HD-UPPDATERA-VALDA-RADER                        
187300                END-IF                                                    
187400             END-IF                                                       
187500          END-IF                                                          
187600        END-IF                                                            
187700                                                                          
187800        PERFORM IMS-GHU-WLKREE01                                          
187900                                                                          
188000        COMPUTE ANM-KVRADER-OBEH       =  ANM-KVRADER-OBEH -              
188100                                          W-KVRADER-BEH                   
188200                                                                          
188300        MOVE W-ANM-PAAB                TO ANM-KDLEVANM                    
188400                                                                          
188500        PERFORM IMS-REPL-WLKREE                                           
188600                                                                          
188700        MOVE INF-UPDATE-DONE           TO MED-IDMFSINF                    
188800                                                                          
188900     END-IF                                                               
189000**** BORTTAGET PGA SORTORDNING I KOLLIKÖN                                 
189100**    PERFORM S06-UPPDATERA-STATUS                                        
189200                                                                          
189300     IF ALLT-SKR-ORDER-SKAPAD                                             
189400       PERFORM HG-STARTA-R32-RAPP-SKROT                                   
189500     END-IF                                                               
189600                                                                          
189700     IF 4797-INDX              >  +1                                      
189800        PERFORM S03-STARTA-R32-RAPPORTERING                               
189900     END-IF                                                               
190000                                                                          
190100     CALL WMEDKONV USING MED-WMEDAREA                                     
190200     MOVE MED-MFSINF             TO MOD-TEMFSINF                          
190300     PERFORM MFS-FORM-ATTR                                                
190400     PERFORM MFS-RENSA-FAELT-IN                                           
190500     .                                                                    
190600     EJECT                                                                
190700                                                                          
190800 HA-SKRIV-UT-TILLSTAND SECTION.                                           
190900                                                                          
191000     MOVE +1                   TO 4794-IX                                 
191100                                                                          
191200     MOVE MSGI-IDDISTR         TO MOD4794-MID-IDDISTR  (4794-IX)          
191300     MOVE MSGI-IDKUNDNR        TO MOD4794-MID-IDKUNDNR (4794-IX)          
191400     MOVE MSGI-IDRAPPNR        TO MOD4794-MID-IDRAPPNR (4794-IX)          
191500     MOVE MSGI-IDDC            TO MOD4794-MID-IDDC                        
191600                                                                          
191700     PERFORM IMS-GHU-WLKREE01                                             
191800     IF SEGMENT-FINNS AND                                                 
191900        ANM-KDLEVANM           = W-ANM-MOT                                
192000         MOVE W-ANM-PAAB       TO ANM-KDLEVANM                            
192100         PERFORM IMS-REPL-WLKREE                                          
192200     END-IF                                                               
192300                                                                          
192400     PERFORM HAB-STARTA-4794                                              
192500     .                                                                    
192600     EJECT                                                                
192700                                                                          
192800 HAB-STARTA-4794  SECTION.                                                
192900                                                                          
193000     MOVE MFS-KDMFSFOR          TO P-TO-P2-KDMFSFOR                       
193100                                                                          
193200     MOVE 'W40737'              TO MOD4794-MID-IDPGM                      
193300     MOVE PRT-IDPRTLST          TO MOD4794-MID-IDPRTLST                   
193400     MOVE +1                    TO MOD4794-MID-KVPOST                     
193500                                                                          
193600     COMPUTE P-TO-P2-LL = (MOD4794-MID-KVPOST * 17) + 17 + 25             
193700                                                                          
193800     PERFORM IMS-ISRT-MSG-ALT-4794                                        
193900     .                                                                    
194000     EJECT                                                                
194100                                                                          
194200 HB-UPPDATERA-ALLT-INLAGT  SECTION.                                       
194300                                                                          
194400     MOVE ZERO                     TO W-KVRADER-BEH                       
194500     PERFORM IMS-GHU-WLKREE01                                             
194600                                                                          
194700     PERFORM HS0-LAES-WLKREE11                                            
194800     PERFORM UNTIL SEGMENT-SAKNAS OR MAX-TRANS-IX > 6                     
194900                                                                          
195000        COMPUTE W-KVLEVANM-KVAR    =  LEV-KVLEVANM-BEKR -                 
195100                                      LEV-KVRETINL -                      
195200                                      LEV-KVAVV-KVANT -                   
195300                                      LEV-KVRETINL-SKR -                  
195400                                      LEV-KVAVV-KVAL                      
195500                                                                          
195600        IF W-KVLEVANM-KVAR         >  ZERO                                
195700          MOVE W-KVLEVANM-KVAR   TO W-KVRETINL-R32                        
195800          MOVE ZERO              TO W-KVAVV-KVANT-R32                     
195900                                    W-KVRETINL-R32-SKR                    
196000          IF MID-IDANSTNR-UPD NUMERIC                                     
196100            MOVE MID-IDANSTNR-UPD    TO LEV-IDANSTNR-RET                  
196200          END-IF                                                          
196300          COMPUTE LEV-KVRETINL       =  LEV-KVRETINL +                    
196400                                        W-KVLEVANM-KVAR                   
196500                                                                          
196600          ACCEPT LEV-TIINLINL FROM DATE                                   
196700                                                                          
196800          COMPUTE W-KVLEVANM-KVAR    =  LEV-KVLEVANM-BEKR -               
196900                                        LEV-KVRETINL -                    
197000                                        LEV-KVAVV-KVANT -                 
197100                                        LEV-KVRETINL-SKR -                
197200                                        LEV-KVAVV-KVAL                    
197300                                                                          
197400          ADD +1                  TO W-KVRADER-BEH                        
197500                                                                          
197600          PERFORM IMS-REPL-WLKREE                                         
197700          PERFORM S04-FYLL-I-R32-MID                                      
197800        END-IF                                                            
197900                                                                          
198000        PERFORM HS0-LAES-WLKREE11                                         
198100     END-PERFORM                                                          
198200                                                                          
198300     .                                                                    
198400     EJECT                                                                
198500                                                                          
198600 HC-UPPDATERA-ALLT-TILL-ILI SECTION.                                      
198700                                                                          
198800     MOVE ZERO                     TO W-KVRADER-BEH                       
198810     MOVE NEJ                      TO SW-SKAPA-WDR5                       
198900     PERFORM S01-LAES-HOEGSTA-IDILIST                                     
199000                                                                          
199100     PERFORM IMS-GHU-WLKREE01                                             
199200                                                                          
199300     PERFORM HS0-LAES-WLKREE11                                            
199400     PERFORM UNTIL SEGMENT-SAKNAS                                         
199500                                                                          
199600        IF (LEV-IDILIST > ZERO) OR                                        
199700           (LEV-KVLEVANM-BEKR     =                                       
199800            LEV-KVAVV-KVAL        +                                       
199900            LEV-KVAVV-KVANT       +                                       
200000            LEV-KVRETINL          +                                       
200100            LEV-KVRETINL-SKR)                                             
200200          CONTINUE                                                        
200300        ELSE                                                              
200400          COMPUTE W-KVLEVANM-KVAR    =  LEV-KVLEVANM-BEKR -               
200500                                        LEV-KVRETINL -                    
200600                                        LEV-KVAVV-KVANT -                 
200700                                        LEV-KVRETINL-SKR -                
200800                                        LEV-KVAVV-KVAL                    
200900                                                                          
201000          COMPUTE LEV-KVANTAL-ILI   =  LEV-KVANTAL-ILI +                  
201100                                        W-KVLEVANM-KVAR                   
201200                                                                          
201300          IF MID-IDANSTNR-UPD NUMERIC                                     
201310            MOVE MID-IDANSTNR-UPD        TO LEV-IDANSTNR-RET              
201320          END-IF                                                          
201330                                                                          
201400          MOVE JA                         TO SW-SKAPA-WDR5                
201701          MOVE ZERO                       TO LEV-IDANSTNR-ILIU            
201800                                                                          
201900          IF DCS-IDDC NOT = MSGI-IDDC                                     
202000             MOVE MSGI-IDDC TO W-IDDC-B6                                  
202100             PERFORM IMS-GU-WDB601                                        
202200          END-IF                                                          
202300          IF DCS-CDC                                                      
202400            MOVE LEV-IDARTNR             TO W-IDARTNR                     
202500            PERFORM IMS-GU-WLARTC11                                       
202600                                                                          
202700            MOVE W-IDILIST               TO LEV-IDILIST                   
202900            MOVE CLAG-ADLAGOMR           TO LEV-ADLAGOMR                  
203000            IF LEV-ADLAGOMR = 22                                          
203100              MOVE 21                    TO LEV-ADLAGOMR                  
203200            END-IF                                                        
203300            IF LEV-ADLAGOMR = 31                                          
203400              MOVE 30                    TO LEV-ADLAGOMR                  
203500            END-IF                                                        
203600            MOVE CLAG-ADGANG             TO LEV-ADGANG                    
203700            MOVE CLAG-ADPLATS            TO LEV-ADPLATS                   
203800                                                                          
203900            PERFORM IMS-REPL-WLKREE                                       
204000          ELSE                                                            
204100            MOVE MSGI-IDDC TO W-IDDC                                      
204200            MOVE LEV-IDARTNR             TO W-IDARTNR                     
204300            PERFORM IMS-GU-WLARTS11                                       
204400                                                                          
204500            MOVE W-IDILIST               TO LEV-IDILIST                   
204700            MOVE SLAG-ADLAGOMR           TO LEV-ADLAGOMR                  
204800            MOVE SLAG-ADGANG             TO LEV-ADGANG                    
204900            MOVE SLAG-ADPLATS            TO LEV-ADPLATS                   
205000                                                                          
205100            PERFORM IMS-REPL-WLKREE                                       
205200          END-IF                                                          
205300        END-IF                                                            
205400        PERFORM HS0-LAES-WLKREE11                                         
205500     END-PERFORM                                                          
205600                                                                          
205610     IF SKAPA-WDR5                                                        
205611        PERFORM S05-SKAPA-WDR5-4704                                       
205620     END-IF                                                               
205700     .                                                                    
205800     EJECT                                                                
205900                                                                          
206000 HD-UPPDATERA-VALDA-RADER  SECTION.                                       
206100                                                                          
206200     MOVE ZERO                     TO W-KVRADER-BEH                       
206300     MOVE +1                       TO INDX                                
206400     PERFORM IMS-GHU-WLKREE01                                             
206500     PERFORM UNTIL INDX            >  MAX-INDX                            
206600                                                                          
206700        IF MID-KDCMDVAL(INDX)      = ALL '+' OR SPACE                     
206800           CONTINUE                                                       
206900        ELSE                                                              
207000                                                                          
207100           MOVE MID-IDARTNR (INDX) TO W-IDARTNR-A2                        
207200                                      W-IDARTNR-A2-MIN                    
207300                                      W-IDARTNR                           
207400           MOVE MID-IDRADNR (INDX) TO W-IDRADNR-A2                        
207500                                      W-IDRADNR-A2-MIN                    
207600                                                                          
207700           PERFORM IMS-GHNP-WLKREE11                                      
207800           COMPUTE W-KVLEVANM-KVAR = LEV-KVLEVANM-BEKR -                  
207900                                     LEV-KVRETINL -                       
208000                                     LEV-KVAVV-KVANT -                    
208100                                     LEV-KVRETINL-SKR -                   
208200                                     LEV-KVAVV-KVAL -                     
208300                                     LEV-KVANTAL-ILI                      
208400                                                                          
208500           EVALUATE MID-KDCMDVAL(INDX)                                    
208600                                                                          
208700               WHEN W-INL                                                 
208800                 PERFORM HDA-UPPDATERA-INL                                
208900                                                                          
209000               WHEN W-BIN                                                 
209100                 PERFORM HDA-UPPDATERA-INL                                
209200                                                                          
209300               WHEN W-SKR                                                 
209400                 PERFORM HDB-UPPDATERA-SKR                                
209500                                                                          
209600               WHEN W-SCR                                                 
209700                 PERFORM HDB-UPPDATERA-SKR                                
209800                                                                          
209900               WHEN W-ANT                                                 
210000                 PERFORM HDC-UPPDATERA-ANT                                
210100                                                                          
210200               WHEN W-DEV                                                 
210300                 PERFORM HDC-UPPDATERA-ANT                                
210400                                                                          
210500               WHEN W-KVA                                                 
210600                 PERFORM HDD-UPPDATERA-KVA                                
210700                                                                          
210800               WHEN W-QDE                                                 
210900                 PERFORM HDD-UPPDATERA-KVA                                
211000                                                                          
211100               WHEN W-ILI                                                 
211200                 PERFORM HDE-UPPDATERA-ILI                                
211300                                                                          
211400               WHEN W-BLI                                                 
211500                 PERFORM HDE-UPPDATERA-ILI                                
211600                                                                          
211700               WHEN OTHER                                                 
211800** KDCMDVAL ÄR NUMERISKT OCH INNEHÅLLER ETT ILISTE NR                     
211900                 PERFORM HDF-UPPDATERA-SPEC-ILI                           
212000                                                                          
212100           END-EVALUATE                                                   
212200                                                                          
212300           IF MID-KDCMDVAL(INDX)       = W-ILI OR W-BLI OR                
212400              MID-KDCMDVAL(INDX)       NUMERIC                            
212500               CONTINUE                                                   
212600           ELSE                                                           
212700              COMPUTE W-KVLEVANM-KVAR = LEV-KVLEVANM-BEKR -               
212800                                        LEV-KVRETINL -                    
212900                                        LEV-KVAVV-KVANT -                 
213000                                        LEV-KVRETINL-SKR -                
213100                                        LEV-KVAVV-KVAL                    
213200                                                                          
213300              IF W-KVLEVANM-KVAR       = ZERO                             
213400                 ACCEPT LEV-TIINLINL FROM DATE                            
213500                 ADD +1                TO W-KVRADER-BEH                   
213600              END-IF                                                      
213700           END-IF                                                         
213800                                                                          
213900           IF LEV-KVAVV-KVANT > ZERO AND LEV-KVAVV-KVAL > ZERO            
214000              MOVE 'D03'               TO LEV-KDKREBEH                    
214100           ELSE                                                           
214200             IF LEV-KVAVV-KVANT > ZERO                                    
214300                MOVE 'D01'             TO LEV-KDKREBEH                    
214400             ELSE                                                         
214500                IF LEV-KVAVV-KVAL > ZERO                                  
214600                   MOVE 'D02'          TO LEV-KDKREBEH                    
214700                END-IF                                                    
214800             END-IF                                                       
214900           END-IF                                                         
215000           PERFORM IMS-REPL-WLKREE                                        
215100        END-IF                                                            
215200                                                                          
215300        ADD +1                         TO INDX                            
215400     END-PERFORM                                                          
215500                                                                          
215600     IF ORAD-IX > +1                                                      
215700       MOVE 'J'              TO ORAD-MID-FLSLUT                           
215800       MOVE ORAD-KOM-AREA    TO P-TO-P4-DATA                              
215900       PERFORM S02-CALL-W006KOM                                           
216000     END-IF                                                               
216100     .                                                                    
216200     EJECT                                                                
216300                                                                          
216400 HDA-UPPDATERA-INL    SECTION.                                            
216500                                                                          
216600     IF MID-IDANSTNR-UPD NUMERIC                                          
216700       MOVE MID-IDANSTNR-UPD         TO LEV-IDANSTNR-RET                  
216800     END-IF                                                               
216900     IF MID-KVANTAL(INDX)          NUMERIC                                
217000         MOVE MID-KVANTAL(INDX)    TO W-KVANTAL                           
217100                                      W-KVRETINL-R32                      
217200         MOVE ZERO                 TO W-KVAVV-KVANT-R32                   
217300                                      W-KVRETINL-R32-SKR                  
217400         COMPUTE LEV-KVRETINL      = LEV-KVRETINL +                       
217500                                     W-KVANTAL                            
217600     ELSE                                                                 
217700         COMPUTE LEV-KVRETINL      = LEV-KVRETINL +                       
217800                                     W-KVLEVANM-KVAR                      
217900         MOVE W-KVLEVANM-KVAR TO W-KVRETINL-R32                           
218000         MOVE ZERO            TO W-KVAVV-KVANT-R32                        
218100                                 W-KVRETINL-R32-SKR                       
218200     END-IF                                                               
218300                                                                          
218400     PERFORM S04-FYLL-I-R32-MID                                           
218500                                                                          
218600     .                                                                    
218700     EJECT                                                                
218800                                                                          
218900 HDB-UPPDATERA-SKR  SECTION.                                              
219000                                                                          
219100     IF MID-IDANSTNR-UPD NUMERIC                                          
219200       MOVE MID-IDANSTNR-UPD         TO LEV-IDANSTNR-RET                  
219300     END-IF                                                               
219400     IF DCS-IDDC NOT = LEV-IDDC-RET                                       
219500        MOVE LEV-IDDC-RET        TO W-IDDC-B6                             
219600        PERFORM IMS-GU-WDB601                                             
219700     END-IF                                                               
219800                                                                          
219900     IF MID-KVANTAL(INDX)          NUMERIC                                
220000       IF  DCS-KDDC NOT = SPACE                                           
220100       AND DCS-KDSKRMET = 1                                               
220200         MOVE MID-KVANTAL(INDX)    TO W-KVANTAL                           
220300                                      W-KVRETINL-R32                      
220400         MOVE ZERO                 TO W-KVRETINL-R32-SKR                  
220500       ELSE                                                               
220600         MOVE MID-KVANTAL(INDX)    TO W-KVANTAL                           
220700                                      W-KVRETINL-R32-SKR                  
220800         MOVE ZERO                 TO W-KVRETINL-R32                      
220900       END-IF                                                             
221000       MOVE ZERO                 TO W-KVAVV-KVANT-R32                     
221100       COMPUTE LEV-KVRETINL-SKR  = LEV-KVRETINL-SKR +                     
221200                                   W-KVANTAL                              
221300     ELSE                                                                 
221400       IF  DCS-KDDC NOT = SPACE                                           
221500       AND DCS-KDSKRMET = 1                                               
221600         COMPUTE LEV-KVRETINL-SKR  = LEV-KVRETINL-SKR +                   
221700                                     W-KVLEVANM-KVAR                      
221800         MOVE W-KVLEVANM-KVAR    TO  W-KVRETINL-R32                       
221900         MOVE ZERO               TO W-KVAVV-KVANT-R32                     
222000                                    W-KVRETINL-R32-SKR                    
222100       ELSE                                                               
222200         COMPUTE LEV-KVRETINL-SKR  = LEV-KVRETINL-SKR +                   
222300                                     W-KVLEVANM-KVAR                      
222400         MOVE W-KVLEVANM-KVAR    TO  W-KVRETINL-R32-SKR                   
222500         MOVE ZERO               TO W-KVAVV-KVANT-R32                     
222600                                    W-KVRETINL-R32                        
222700       END-IF                                                             
222800     END-IF                                                               
222900                                                                          
223000     PERFORM S04-FYLL-I-R32-MID                                           
223100                                                                          
223200     IF LEV-IDARTNR NOT = 100                                             
223300       IF  DCS-KDDC NOT = SPACE                                           
223400       AND DCS-KDSKRMET = 1                                               
223500                                                                          
223600         IF  TRANS-OHUVUD-DAM-SKAPAD                                      
223700             CONTINUE                                                     
223800         ELSE                                                             
223900             PERFORM S08-SKAPA-TRANS-ORDERHUVUD                           
224000                                                                          
224100             PERFORM S09-SKAPA-HUVUD-ORDERRADER                           
224200                                                                          
224300             MOVE 1           TO ORAD-IX                                  
224400         END-IF                                                           
224500                                                                          
224600         IF ORAD-IX > ORAD-IX-MAX                                         
224700           MOVE ORAD-KOM-AREA    TO P-TO-P4-DATA                          
224800           PERFORM S02-CALL-W006KOM                                       
224900           PERFORM S09-SKAPA-HUVUD-ORDERRADER                             
225000           MOVE 1               TO ORAD-IX                                
225100                                                                          
225200           PERFORM S10-EDIT-TRANS-ORDERRADER                              
225300           ADD +1               TO ORAD-IX                                
225400         ELSE                                                             
225500           PERFORM S10-EDIT-TRANS-ORDERRADER                              
225600           ADD +1               TO ORAD-IX                                
225700         END-IF                                                           
225800                                                                          
225900       END-IF                                                             
226000     END-IF                                                               
226100     .                                                                    
226200     EJECT                                                                
226300                                                                          
226400 HDC-UPPDATERA-ANT   SECTION.                                             
226500                                                                          
226600                                                                          
226700     IF MID-IDANSTNR-UPD NUMERIC                                          
226800       MOVE MID-IDANSTNR-UPD           TO LEV-IDANSTNR-RET                
226900     END-IF                                                               
227000     IF MID-KVANTAL(INDX)            NUMERIC                              
227100         MOVE MID-KVANTAL(INDX)      TO W-KVANTAL                         
227200                                     W-KVAVV-KVANT-R32                    
227300         MOVE ZERO                   TO W-KVRETINL-R32                    
227400                                        W-KVRETINL-R32-SKR                
227500         COMPUTE LEV-KVAVV-KVANT     = LEV-KVAVV-KVANT +                  
227600                                       W-KVANTAL                          
227700     ELSE                                                                 
227800         COMPUTE LEV-KVAVV-KVANT     = LEV-KVAVV-KVANT +                  
227900                                       W-KVLEVANM-KVAR                    
228000         MOVE W-KVLEVANM-KVAR    TO  W-KVAVV-KVANT-R32                    
228100         MOVE ZERO               TO W-KVRETINL-R32                        
228200                                    W-KVRETINL-R32-SKR                    
228300     END-IF                                                               
228400                                                                          
228500     PERFORM S04-FYLL-I-R32-MID                                           
228600     .                                                                    
228700     EJECT                                                                
228800                                                                          
228900 HDD-UPPDATERA-KVA   SECTION.                                             
229000                                                                          
229100     IF MID-IDANSTNR-UPD NUMERIC                                          
229200       MOVE MID-IDANSTNR-UPD           TO LEV-IDANSTNR-RET                
229300     END-IF                                                               
229400     IF MID-KVANTAL(INDX)            NUMERIC                              
229500         MOVE MID-KVANTAL(INDX)      TO W-KVANTAL                         
229600                                        W-KVAVV-KVANT-R32                 
229700         MOVE ZERO                   TO W-KVRETINL-R32                    
229800                                        W-KVRETINL-R32-SKR                
229900         COMPUTE LEV-KVAVV-KVAL      = LEV-KVAVV-KVAL  +                  
230000                                       W-KVANTAL                          
230100     ELSE                                                                 
230200         COMPUTE LEV-KVAVV-KVAL      = LEV-KVAVV-KVAL  +                  
230300                                       W-KVLEVANM-KVAR                    
230400         MOVE W-KVLEVANM-KVAR    TO  W-KVAVV-KVANT-R32                    
230500         MOVE ZERO               TO W-KVRETINL-R32                        
230600                                    W-KVRETINL-R32-SKR                    
230700     END-IF                                                               
230800                                                                          
230900     PERFORM S04-FYLL-I-R32-MID                                           
231000     .                                                                    
231100     EJECT                                                                
231200                                                                          
231300 HDE-UPPDATERA-ILI    SECTION.                                            
231900                                                                          
232000     IF FOERSTA-ILI                                                       
232100       PERFORM S01-LAES-HOEGSTA-IDILIST                                   
232110       PERFORM S05-SKAPA-WDR5-4704                                        
232200       MOVE NEJ      TO SW-ILI                                            
232300     ELSE                                                                 
232301       PERFORM S06-UPPDATERA-WDR5-4704                                    
232302       IF MID-IDANSTNR-UPD NUMERIC                                        
232303         MOVE MID-IDANSTNR-UPD         TO LEV-IDANSTNR-ILIU               
232304       END-IF                                                             
232305       ACCEPT LEV-TIUPPDAT-ILI FROM DATE                                  
232310     END-IF                                                               
232400                                                                          
232410     IF MID-IDANSTNR-UPD NUMERIC                                          
232420       MOVE MID-IDANSTNR-UPD           TO LEV-IDANSTNR-RET                
232430     END-IF                                                               
232440                                                                          
232500     MOVE W-IDILIST                    TO LEV-IDILIST                     
232700                                                                          
232800     IF DCS-IDDC NOT = MSGI-IDDC                                          
232900        MOVE MSGI-IDDC                 TO W-IDDC-B6                       
233000        PERFORM IMS-GU-WDB601                                             
233100     END-IF                                                               
233200     IF DCS-CDC                                                           
233300       PERFORM IMS-GU-WLARTC11                                            
233400       MOVE CLAG-ADLAGOMR              TO LEV-ADLAGOMR                    
233500       IF LEV-ADLAGOMR = 22                                               
233600         MOVE 21                       TO LEV-ADLAGOMR                    
233700       END-IF                                                             
233800       IF LEV-ADLAGOMR = 31                                               
233900         MOVE 30                       TO LEV-ADLAGOMR                    
234000       END-IF                                                             
234100       MOVE CLAG-ADGANG                TO LEV-ADGANG                      
234200       MOVE CLAG-ADPLATS               TO LEV-ADPLATS                     
234300     ELSE                                                                 
234400       MOVE MSGI-IDDC TO W-IDDC                                           
234500       PERFORM IMS-GU-WLARTS11                                            
234600       MOVE SLAG-ADLAGOMR              TO LEV-ADLAGOMR                    
234700       MOVE SLAG-ADGANG                TO LEV-ADGANG                      
234800       MOVE SLAG-ADPLATS               TO LEV-ADPLATS                     
234900     END-IF                                                               
235000                                                                          
235100     IF MID-KVANTAL(INDX)            NUMERIC                              
235200         MOVE MID-KVANTAL(INDX)      TO W-KVANTAL                         
235300         COMPUTE LEV-KVANTAL-ILI     = LEV-KVANTAL-ILI +                  
235400                                       W-KVANTAL                          
235500     ELSE                                                                 
235600         COMPUTE LEV-KVANTAL-ILI     = LEV-KVANTAL-ILI +                  
235700                                       W-KVLEVANM-KVAR                    
235800     END-IF                                                               
235900                                                                          
236000     .                                                                    
236100     EJECT                                                                
236200                                                                          
236300 HDF-UPPDATERA-SPEC-ILI   SECTION.                                        
236400                                                                          
236410     IF MID-IDANSTNR-UPD NUMERIC                                          
236420       MOVE MID-IDANSTNR-UPD           TO LEV-IDANSTNR-RET                
236430     END-IF                                                               
236440                                                                          
236500     IF LEV-IDILIST = ZERO                                                
236600       MOVE MID-KDCMDVAL (INDX)        TO LEV-IDILIST                     
236601                                          W-IDILIST                       
236610       PERFORM S06-UPPDATERA-WDR5-4704                                    
236700       ACCEPT LEV-TIUPPDAT-ILI FROM DATE                                  
236800                                                                          
236900       IF MID-IDANSTNR-UPD NUMERIC                                        
237003         MOVE MID-IDANSTNR-UPD         TO LEV-IDANSTNR-ILIU               
237200       END-IF                                                             
237210       ACCEPT LEV-TIUPPDAT-ILI FROM DATE                                  
237300                                                                          
237400       IF DCS-IDDC NOT = MSGI-IDDC                                        
237500          MOVE MSGI-IDDC TO W-IDDC-B6                                     
237600          PERFORM IMS-GU-WDB601                                           
237700       END-IF                                                             
237800       IF DCS-CDC                                                         
237900         PERFORM IMS-GU-WLARTC11                                          
238000         MOVE CLAG-ADLAGOMR              TO LEV-ADLAGOMR                  
238100         IF LEV-ADLAGOMR = 22                                             
238200           MOVE 21                       TO LEV-ADLAGOMR                  
238300         END-IF                                                           
238400         IF LEV-ADLAGOMR = 31                                             
238500           MOVE 30                       TO LEV-ADLAGOMR                  
238600         END-IF                                                           
238700         MOVE CLAG-ADGANG                TO LEV-ADGANG                    
238800         MOVE CLAG-ADPLATS               TO LEV-ADPLATS                   
238900       ELSE                                                               
239000         MOVE MSGI-IDDC TO W-IDDC                                         
239100         PERFORM IMS-GU-WLARTS11                                          
239200         MOVE SLAG-ADLAGOMR              TO LEV-ADLAGOMR                  
239300         MOVE SLAG-ADGANG                TO LEV-ADGANG                    
239400         MOVE SLAG-ADPLATS               TO LEV-ADPLATS                   
239500       END-IF                                                             
239600     END-IF                                                               
239700                                                                          
239800     IF MID-KVANTAL(INDX)            NUMERIC                              
239900         MOVE MID-KVANTAL(INDX)      TO W-KVANTAL                         
240000         COMPUTE LEV-KVANTAL-ILI     = LEV-KVANTAL-ILI +                  
240100                                       W-KVANTAL                          
240200     ELSE                                                                 
240300         COMPUTE LEV-KVANTAL-ILI     = LEV-KVANTAL-ILI +                  
240400                                       W-KVLEVANM-KVAR                    
240500     END-IF                                                               
240600     .                                                                    
240700     EJECT                                                                
240800                                                                          
240900 HE-UPPDATERA-ALLT-SKROT   SECTION.                                       
241000                                                                          
241100     MOVE ZERO                     TO W-KVRADER-BEH                       
241200     PERFORM IMS-GHU-WLKREE01                                             
241300                                                                          
241400     PERFORM HS0-LAES-WLKREE11                                            
241500     PERFORM UNTIL SEGMENT-SAKNAS OR MAX-TRANS-IX > 10                    
241600        MOVE 'I PERFORM ' TO INFO-2                                       
241700          ADD +1 TO INFO-2-ANTAL                                          
241800                                                                          
241900        COMPUTE W-KVLEVANM-KVAR    =  LEV-KVLEVANM-BEKR -                 
242000                                      LEV-KVRETINL -                      
242100                                      LEV-KVAVV-KVANT -                   
242200                                      LEV-KVRETINL-SKR -                  
242300                                      LEV-KVAVV-KVAL                      
242400                                                                          
242500        IF W-KVLEVANM-KVAR         >  ZERO                                
242600          MOVE 'LEVANM > 0 '     TO INFO-3                                
242700          ADD +1 TO INFO-3-ANTAL                                          
242800          IF DCS-IDDC NOT = LEV-IDDC-RET                                  
242900             MOVE LEV-IDDC-RET   TO W-IDDC-B6                             
243000             PERFORM IMS-GU-WDB601                                        
243100          END-IF                                                          
243200                                                                          
243300          IF  DCS-KDDC NOT = SPACE                                        
243400          AND DCS-KDSKRMET = 1                                            
243500            MOVE W-KVLEVANM-KVAR   TO W-KVRETINL-R32                      
243600            MOVE ZERO              TO W-KVRETINL-R32-SKR                  
243700          ELSE                                                            
243800            MOVE ZERO              TO W-KVRETINL-R32                      
243900            MOVE W-KVLEVANM-KVAR   TO W-KVRETINL-R32-SKR                  
244000          END-IF                                                          
244100          MOVE ZERO              TO W-KVAVV-KVANT-R32                     
244200          IF MID-IDANSTNR-UPD NUMERIC                                     
244300            MOVE MID-IDANSTNR-UPD    TO LEV-IDANSTNR-RET                  
244400          END-IF                                                          
244500          COMPUTE LEV-KVRETINL-SKR   =  LEV-KVRETINL-SKR +                
244600                                        W-KVLEVANM-KVAR                   
244700                                                                          
244800          ACCEPT LEV-TIINLINL FROM DATE                                   
244900                                                                          
245000          COMPUTE W-KVLEVANM-KVAR    =  LEV-KVLEVANM-BEKR -               
245100                                        LEV-KVRETINL -                    
245200                                        LEV-KVAVV-KVANT -                 
245300                                        LEV-KVRETINL-SKR -                
245400                                        LEV-KVAVV-KVAL                    
245500                                                                          
245600          ADD +1                  TO W-KVRADER-BEH                        
245700                                                                          
245800          MOVE LEV-IDARTNR        TO W-IDARTNR                            
245900          PERFORM IMS-REPL-WLKREE                                         
246000                                                                          
246100          IF  DCS-KDDC NOT = SPACE                                        
246200          AND DCS-KDSKRMET = 1                                            
246300            PERFORM S11-SPARA-R32-MID                                     
246400            MOVE JA   TO ALLT-SKR-ORDER-SKAPAD-SW                         
246500          ELSE                                                            
246600            PERFORM S04-FYLL-I-R32-MID                                    
246700          END-IF                                                          
246800                                                                          
246900          IF LEV-IDARTNR NOT = 100                                        
247000                                                                          
247100            IF  DCS-KDDC NOT = SPACE                                      
247200            AND DCS-KDSKRMET = 1                                          
247300                                                                          
247400              MOVE 'I SKROT SEC'     TO INFO-4                            
247500              ADD +1 TO INFO-4-ANTAL                                      
247600              IF  TRANS-OHUVUD-DAM-SKAPAD                                 
247700                  CONTINUE                                                
247800                MOVE 'ORD HUV SKAPAD'  TO INFO-5                          
247900                ADD +1 TO INFO-5-ANTAL                                    
248000              ELSE                                                        
248100                PERFORM S08-SKAPA-TRANS-ORDERHUVUD                        
248200                                                                          
248300                PERFORM S09-SKAPA-HUVUD-ORDERRADER                        
248400                                                                          
248500                MOVE 1           TO ORAD-IX                               
248600                MOVE 'SKAPA ORD HUV '  TO INFO-6                          
248700                ADD +1 TO INFO-6-ANTAL                                    
248800              END-IF                                                      
248900                                                                          
249000              IF ORAD-IX > ORAD-IX-MAX                                    
249100                                                                          
249200                MOVE ORAD-KOM-AREA    TO P-TO-P4-DATA                     
249300                PERFORM S02-CALL-W006KOM                                  
249400                PERFORM S09-SKAPA-HUVUD-ORDERRADER                        
249500                MOVE 1               TO ORAD-IX                           
249600                ADD +1               TO MAX-TRANS-IX                      
249700                                                                          
249800                PERFORM S10-EDIT-TRANS-ORDERRADER                         
249900                ADD +1               TO ORAD-IX                           
250000              ELSE                                                        
250100                PERFORM S10-EDIT-TRANS-ORDERRADER                         
250200                ADD +1               TO ORAD-IX                           
250300              END-IF                                                      
250400                                                                          
250500            END-IF                                                        
250600          END-IF                                                          
250700        END-IF                                                            
250800                                                                          
250900        PERFORM HS0-LAES-WLKREE11                                         
251000     END-PERFORM                                                          
251100                                                                          
251200     IF ORAD-IX > +1                                                      
251300          MOVE 'ORAD-IX > 0   '  TO INFO-7                                
251400          ADD +1 TO INFO-7-ANTAL                                          
251500       MOVE 'J'              TO ORAD-MID-FLSLUT                           
251600       MOVE ORAD-KOM-AREA    TO P-TO-P4-DATA                              
251700       PERFORM S02-CALL-W006KOM                                           
251800     END-IF                                                               
251900                                                                          
252000     .                                                                    
252100     EJECT                                                                
252200                                                                          
252300 HF-UPPDATERA-ALLT-ANT-AVV    SECTION.                                    
252400                                                                          
252500     MOVE ZERO                   TO W-KVRADER-BEH                         
252600     PERFORM IMS-GHU-WLKREE01                                             
252700                                                                          
252800     PERFORM HS0-LAES-WLKREE11                                            
252900     PERFORM UNTIL SEGMENT-SAKNAS OR MAX-TRANS-IX > 6                     
253000                                                                          
253100        COMPUTE W-KVLEVANM-KVAR    =  LEV-KVLEVANM-BEKR -                 
253200                                      LEV-KVRETINL -                      
253300                                      LEV-KVAVV-KVANT -                   
253400                                      LEV-KVRETINL-SKR -                  
253500                                      LEV-KVAVV-KVAL                      
253600                                                                          
253700        IF W-KVLEVANM-KVAR         >  ZERO                                
253800          MOVE W-KVLEVANM-KVAR   TO W-KVAVV-KVANT-R32                     
253900          MOVE ZERO              TO W-KVRETINL-R32                        
254000                                    W-KVRETINL-R32-SKR                    
254100          IF MID-IDANSTNR-UPD NUMERIC                                     
254200            MOVE MID-IDANSTNR-UPD    TO LEV-IDANSTNR-RET                  
254300          END-IF                                                          
254400          COMPUTE LEV-KVAVV-KVANT    =  LEV-KVAVV-KVANT +                 
254500                                        W-KVLEVANM-KVAR                   
254600                                                                          
254700          ACCEPT LEV-TIINLINL FROM DATE                                   
254800                                                                          
254900          COMPUTE W-KVLEVANM-KVAR    =  LEV-KVLEVANM-BEKR -               
255000                                        LEV-KVRETINL -                    
255100                                        LEV-KVAVV-KVANT -                 
255200                                        LEV-KVRETINL-SKR -                
255300                                        LEV-KVAVV-KVAL                    
255400                                                                          
255500          ADD +1                  TO W-KVRADER-BEH                        
255600                                                                          
255700          IF LEV-KVAVV-KVANT > ZERO AND LEV-KVAVV-KVAL > ZERO             
255800             MOVE 'D03'               TO LEV-KDKREBEH                     
255900          ELSE                                                            
256000            IF LEV-KVAVV-KVANT > ZERO                                     
256100               MOVE 'D01'             TO LEV-KDKREBEH                     
256200            ELSE                                                          
256300               IF LEV-KVAVV-KVAL > ZERO                                   
256400                  MOVE 'D02'          TO LEV-KDKREBEH                     
256500               END-IF                                                     
256600            END-IF                                                        
256700          END-IF                                                          
256800                                                                          
256900          PERFORM IMS-REPL-WLKREE                                         
257000          PERFORM S04-FYLL-I-R32-MID                                      
257100        END-IF                                                            
257200                                                                          
257300        PERFORM HS0-LAES-WLKREE11                                         
257400     END-PERFORM                                                          
257500                                                                          
257600     .                                                                    
257700     EJECT                                                                
257800 HG-STARTA-R32-RAPP-SKROT  SECTION.                                       
257900                                                                          
258000     MOVE +1      TO 4797-INDX                                            
258100                     SPAR4797-IX                                          
258200                                                                          
258300     PERFORM UNTIL SPAR4797-IX = 4797-IX2                                 
258400                                                                          
258500       MOVE SPAR4797-MID-IDDISTR (SPAR4797-IX)                            
258600                            TO MOD4797-MID-IDDISTR     (4797-INDX)        
258700       MOVE SPAR4797-MID-IDKUNDNR (SPAR4797-IX)                           
258800                            TO MOD4797-MID-IDKUNDNR    (4797-INDX)        
258900       MOVE SPAR4797-MID-IDRAPPNR (SPAR4797-IX)                           
259000                            TO MOD4797-MID-IDRAPPNR    (4797-INDX)        
259100       MOVE SPAR4797-MID-IDARTNR (SPAR4797-IX)                            
259200                            TO MOD4797-MID-IDARTNR     (4797-INDX)        
259300       MOVE SPAR4797-MID-IDRADNR (SPAR4797-IX)                            
259400                            TO MOD4797-MID-IDRADNR     (4797-INDX)        
259500       MOVE SPAR4797-MID-KVRETINL (SPAR4797-IX)                           
259600                            TO MOD4797-MID-KVRETINL    (4797-INDX)        
259700       MOVE SPAR4797-MID-KVAVV-KVANT (SPAR4797-IX)                        
259800                            TO MOD4797-MID-KVAVV-KVANT (4797-INDX)        
259900                                                                          
260000       MOVE ZERO            TO MOD4797-MID-KVRETINL-TRP(4797-INDX)        
260100                                                                          
260200       MOVE SPAR4797-MID-KVRETINL-SKR (SPAR4797-IX)                       
260300                            TO MOD4797-MID-KVRETINL-SKR(4797-INDX)        
260400                                                                          
260500       ADD +1               TO 4797-INDX                                  
260600                                                                          
260700       IF 4797-INDX              >  4797-MAX-INDX                         
260800         PERFORM S03-STARTA-R32-RAPPORTERING                              
260900         MOVE +1                TO 4797-INDX                              
261000       END-IF                                                             
261100                                                                          
261200        ADD +1              TO SPAR4797-IX                                
261300     END-PERFORM                                                          
261400                                                                          
261500     .                                                                    
261600     EJECT                                                                
261700                                                                          
261800 HS0-LAES-WLKREE11          SECTION.                                      
261900                                                                          
262000     MOVE NEJ                    TO OKOD-FL-RETILL                        
262100                                    OKOD-FL-INTERNUPPACKNING              
262200     PERFORM IMS-GHNP-WLKREE11-OKVAL                                      
262300     PERFORM UNTIL OKOD-FL-RETILL = 'J' OR SEGMENT-SAKNAS                 
262400                OR OKOD-FL-INTERNUPPACKNING = 'J'                         
262500        IF LEV-KDKREBEH(1:1) = 'Y'   OR                                   
262600           LEV-KDKREBEH(1:1) = 'J'   OR                                   
262700           LEV-KDKREBEH(1:1) = 'C'   OR                                   
262800           LEV-KDKREBEH      = 'D01' OR                                   
262900           LEV-KDKREBEH      = 'D02' OR                                   
263000           LEV-KDKREBEH      = 'D03'                                      
263100*--ANROPA KONTROLL AV ORSAKSKODER                                         
263200            MOVE LEV-KDANMORS   TO OKOD-KDANMORS                          
263300            CALL W418OKOD USING OKOD-W418OKOD                             
263400        END-IF                                                            
263500        IF OKOD-FL-RETILL = 'J' OR                                        
263600           OKOD-FL-INTERNUPPACKNING = 'J'                                 
263700           CONTINUE                                                       
263800        ELSE                                                              
263900          PERFORM IMS-GHNP-WLKREE11-OKVAL                                 
264000        END-IF                                                            
264100     END-PERFORM                                                          
264200                                                                          
264300     .                                                                    
264400     EJECT                                                                
264500 S01-LAES-HOEGSTA-IDILIST  SECTION.                                       
264600                                                                          
264700     PERFORM IMS-GHU-WL411111                                             
264800     MOVE 4112-IDILIST TO W-IDILIST                                       
264900     IF W-IDILIST = 999                                                   
265000       MOVE +1           TO W-IDILIST                                     
265100     ELSE                                                                 
265200       ADD  +1           TO W-IDILIST                                     
265300     END-IF                                                               
265400     MOVE W-IDILIST    TO 4112-IDILIST                                    
265500     PERFORM IMS-REPL-WL4111                                              
265600                                                                          
265700     MOVE '001'                TO MSGI-KDCALL                             
265800     MOVE W-IDILIST            TO MSGI-IDILIST                            
265900                                  MOD-IDILIST-NY                          
266000                                                                          
266100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
266200     .                                                                    
266300     EJECT                                                                
266400                                                                          
266500 S02-CALL-W006KOM        SECTION.                                         
266600                                                                          
266700     CALL W006KOM         USING MSG-PCB                                   
266800                                DISP-PCB                                  
266900                                KOMA-PCB                                  
267000                                MSG-KOM-WMSGKOM                           
267100                                P-TO-P-AREA4                              
267200                                                                          
267300          MOVE 'S02 KOM       '  TO INFO-8                                
267400          ADD +1 TO INFO-8-ANTAL                                          
267500     .                                                                    
267600     EJECT                                                                
267700                                                                          
267800 S03-STARTA-R32-RAPPORTERING     SECTION.                                 
267900                                                                          
268000     MOVE SPACE                TO MSG-KOM-WMSGKOM                         
268100     COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM                     
268200     MOVE LOW-VALUE            TO MSG-KOM-KDZ1                            
268300     MOVE LOW-VALUE            TO MSG-KOM-KDZ2                            
268400     MOVE SPACE                TO MSG-KOM-KDTRANS                         
268500     MOVE 'W4I79701'           TO MSG-KOM-IDCPYTXT                        
268600     MOVE 'INLEVRET'           TO MSG-KOM-IDSNDNOD                        
268700     MOVE 'W4073700'           TO MSG-KOM-IDSNDJOB                        
268800     ACCEPT MSG-KOM-TIREGDAT   FROM DATE                                  
268900     ADD +1                    TO W-TIKLOCK-R32                           
269000     MOVE W-TIKLOCK-R32        TO MSG-KOM-TIKLOCK                         
269100                                                                          
269200     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
269300                                                                          
269400     COMPUTE P-TO-P5-LL        =  LNG-P-TO-P-PREFIX +                     
269500                                  LENGTH OF MOD4797-MID-W4I79701          
269600                                                                          
269700     MOVE 'W4T797X '           TO P-TO-P5-TRANSKOD                        
269800     MOVE '4737'               TO P-TO-P5-FROM-MID                        
269900     MOVE MFS-KDMFSFOR         TO P-TO-P5-KDMFSFOR                        
270000                                                                          
270100     COMPUTE MOD4797-MID-KVPOST  = 4797-INDX - 1                          
270200                                                                          
270300     MOVE R32-KOM-AREA   TO P-TO-P5-DATA                                  
270400                                                                          
270500     CALL W006KOM         USING MSG-PCB                                   
270600                                DISP-PCB                                  
270700                                KOMA-PCB                                  
270800                                MSG-KOM-WMSGKOM                           
270900                                P-TO-P-AREA5                              
271000                                                                          
271100                                                                          
271200     .                                                                    
271300     EJECT                                                                
271400                                                                          
271500 S04-FYLL-I-R32-MID SECTION.                                              
271600                                                                          
271700     MOVE MSGI-IDDISTR      TO MOD4797-MID-IDDISTR     (4797-INDX)        
271800     MOVE MSGI-IDKUNDNR     TO MOD4797-MID-IDKUNDNR    (4797-INDX)        
271900     MOVE MSGI-IDRAPPNR     TO MOD4797-MID-IDRAPPNR    (4797-INDX)        
272000     MOVE LEV-IDARTNR       TO MOD4797-MID-IDARTNR     (4797-INDX)        
272100     MOVE LEV-IDRADNR       TO MOD4797-MID-IDRADNR     (4797-INDX)        
272200     MOVE W-KVRETINL-R32    TO MOD4797-MID-KVRETINL    (4797-INDX)        
272300     MOVE W-KVAVV-KVANT-R32 TO MOD4797-MID-KVAVV-KVANT (4797-INDX)        
272400     MOVE ZERO              TO MOD4797-MID-KVRETINL-TRP(4797-INDX)        
272500     MOVE W-KVRETINL-R32-SKR                                              
272600                            TO MOD4797-MID-KVRETINL-SKR(4797-INDX)        
272700                                                                          
272800     ADD +1                    TO 4797-INDX                               
272900                                                                          
273000     IF 4797-INDX              >  4797-MAX-INDX                           
273100        PERFORM S03-STARTA-R32-RAPPORTERING                               
273200        MOVE +1                TO 4797-INDX                               
273300        ADD  +1                TO MAX-TRANS-IX                            
273400     END-IF                                                               
273500     .                                                                    
273600     EJECT                                                                
273700                                                                          
273800 S05-SKAPA-WDR5-4704 SECTION.                                             
273801                                                                          
273810     MOVE LEV-IDDC-RET     TO W-IDDC-4703                                 
273811     PERFORM IMS-GU-WDR501                                                
273812     IF SEGMENT-SAKNAS                                                    
273813        MOVE '4703'        TO W-IDHTYP                                    
273814        MOVE LEV-IDDC-RET  TO W-IDDC-4703                                 
273815        MOVE LOW-VALUE     TO NYCKEL-VALFRI                               
273816        PERFORM IMS-ISRT-WDR501                                           
273817     END-IF                                                               
273818                                                                          
273820     MOVE W-IDILIST        TO 4704-IDILIST                                
273830     MOVE MID-IDANSTNR-UPD TO 4704-IDANSTNR-ILIR                          
273840     ACCEPT 4704-TIREGDAT-ILI FROM DATE                                   
273850                                                                          
273860     MOVE ZERO             TO 4704-IDANSTNR-ILIU                          
273870                              4704-IDANSTNR-ILIP                          
273880                              4704-TIUPPDAT-ILI                           
273890                              4704-TIUTSKR                                
273891                                                                          
273892     PERFORM IMS-ISRT-WDR5-4704                                           
273900     .                                                                    
273910     EJECT                                                                
273920                                                                          
273921 S06-UPPDATERA-WDR5-4704 SECTION.                                         
273922                                                                          
273923     MOVE LEV-IDDC-RET     TO W-IDDC-4703                                 
273924     MOVE W-IDILIST        TO W-IDILIST-4704                              
273925                                                                          
273926     PERFORM IMS-GHU-WDR5-4704                                            
273927     MOVE MID-IDANSTNR-UPD TO 4704-IDANSTNR-ILIU                          
273928     ACCEPT 4704-TIUPPDAT-ILI FROM DATE                                   
273929     PERFORM IMS-REPL-WDR5-4704                                           
273930     .                                                                    
273931     EJECT                                                                
273932                                                                          
273933*S06-UPPDATERA-STATUS SECTION.                                            
273940*                                                                         
274000*    MOVE W-IDDISTR       TO W-IDDISTR-FSEQ                               
274100*    MOVE W-IDKUNDNR      TO W-IDKUNDNR-FSEQ                              
274200*    MOVE W-IDRAPPNR      TO W-IDRAPPNR-FSEQ                              
274300*                                                                         
274400*    PERFORM IMS-GHU-RETA01-FSEQ                                          
274500*    PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                      
274600*            RET-KDRETSTA = '5'                                           
274700*       MOVE '5'         TO  RET-KDRETSTA                                 
274800*       PERFORM IMS-REPL-RETA-FSEQ                                        
274900*       PERFORM IMS-GHN-RETA01-FSEQ                                       
275000*    END-PERFORM                                                          
275100*    .                                                                    
275200*    EJECT                                                                
275300 S07-LAES-HOEGSTA-IDORDNR  SECTION.                                       
275400                                                                          
275500     PERFORM IMS-GHU-WL411111                                             
275600     MOVE 4112-IDORDNR7 TO W-IDORDNR-X                                    
275700     IF W-IDORDNR-VV = DAT-TIVV                                           
275800       ADD  +1           TO W-IDORDNR-LLL                                 
275900     ELSE                                                                 
276000       MOVE DAT-TIVV     TO W-IDORDNR-VV                                  
276100       MOVE +1           TO W-IDORDNR-LLL                                 
276200     END-IF                                                               
276300     MOVE W-IDORDNR-X  TO 4112-IDORDNR7                                   
276400     PERFORM IMS-REPL-WL4111                                              
276500     .                                                                    
276600     EJECT                                                                
276700                                                                          
276800 S08-SKAPA-TRANS-ORDERHUVUD SECTION.                                      
276900                                                                          
277000     MOVE JA              TO TRANS-OHUVUD-DAM-SKAPAD-SW                   
277100                                                                          
277200     MOVE SPACE           TO MSG-KOM-WMSGKOM                              
277300     MOVE +54             TO MSG-KOM-KVLL                                 
277400     MOVE LOW-VALUE       TO MSG-KOM-KDZ1                                 
277500     MOVE LOW-VALUE       TO MSG-KOM-KDZ2                                 
277600     MOVE SPACE           TO MSG-KOM-KDTRANS                              
277700     MOVE 'W4I25101'      TO MSG-KOM-IDCPYTXT                             
277800     MOVE 'RET-RET '      TO MSG-KOM-IDSNDNOD                             
277900     MOVE 'W4073700'      TO MSG-KOM-IDSNDJOB                             
278000                                                                          
278100     ACCEPT MSG-KOM-TIREGDAT  FROM DATE                                   
278200                                                                          
278300     MOVE W-TIKLOCK-ORDER TO MSG-KOM-TIKLOCK                              
278400                                                                          
278500     MOVE SPACE           TO MSG-KOM-IDMFSMED                             
278600                             MSG-KOM-KDSVAR                               
278700                                                                          
278800     COMPUTE P-TO-P4-LL   =  LNG-P-TO-P-PREFIX +                          
278900                             LENGTH OF OHUV-MID-W4I25101                  
279000                                                                          
279100     MOVE LOW-VALUE              TO P-TO-P4-Z1                            
279200     MOVE LOW-VALUE              TO P-TO-P4-Z2                            
279300     MOVE 'W4T251X'              TO P-TO-P4-TRANSKOD                      
279400     MOVE '4251'                 TO P-TO-P4-FROM-MID                      
279500     MOVE MFS-KDMFSFOR           TO P-TO-P4-KDMFSFOR                      
279600                                                                          
279700                                                                          
279800     MOVE SPACE                  TO OHUV-KOM-AREA                         
279900                                                                          
280000     MOVE 'W407'                 TO OHUV-MID-IDSYSTEM                     
280100                                                                          
280200     IF DCS-IDDC NOT = LEV-IDDC-RET                                       
280300        MOVE LEV-IDDC-RET        TO W-IDDC-B6                             
280400        PERFORM IMS-GU-WDB601                                             
280500     END-IF                                                               
280600     IF  DCS-KDDC NOT = SPACE                                             
280700         MOVE DCS-IDDISTR-RSKROT     TO WS-IDDISTR-N                      
280800         MOVE WS-IDDISTR-X           TO OHUV-MID-IDDISTR                  
280900         MOVE DCS-IDKUNDNR-RSKROT    TO WS-IDKUNDNR-N                     
281000         MOVE WS-IDKUNDNR-X          TO OHUV-MID-IDKUNDNR                 
281100     END-IF                                                               
281200                                                                          
281300     PERFORM S07-LAES-HOEGSTA-IDORDNR                                     
281400     MOVE W-IDORDNR-X           TO OHUV-MID-IDORDNR                       
281500     MOVE '1'                   TO OHUV-MID-KDORDKL                       
281600                                                                          
281700     MOVE SPACE           TO OHUV-MID-KDFRAKT                             
281800                             OHUV-MID-TIRFS                               
281900     MOVE SPACE           TO OHUV-MID-BEKUNDRF                            
282000     MOVE 'N'             TO OHUV-MID-KDFAKTYP                            
282100     MOVE NEJ             TO OHUV-MID-FLRESTN                             
282200     MOVE SPACE           TO OHUV-MID-KDTPOTYP                            
282300                             OHUV-MID-TITPO                               
282400                             OHUV-MID-BELAGINS                            
282500                             OHUV-MID-BEGMT                               
282600                             OHUV-MID-ADGMT-GATA                          
282700                             OHUV-MID-ADGMT-PADR                          
282800                             OHUV-MID-KDROPACK                            
282900                             OHUV-MID-BEVARREF                            
283000                             OHUV-MID-KDTULLVE                            
283100                             OHUV-MID-KDNOTES                             
283200*SAP EJ KONTO HÄR I DETTA FALLET, ENL. BOSSE H. 981026.EFTERSOM           
283300*GULL-BRITT EJ HAR SVARAT,IFALL HON VILL HA ETT EGET ANALYSNR FÖR         
283400*DESSA SKROTNINGAR, HAMNAR ALLT PÅ 'ALLMÄNNA SKROTNINGAR' MED             
283500*INTERNTABELL. DETTA GÄLLER FÖR DC11.                                     
283600     MOVE ZERO            TO OHUV-MID-IDKONTO                             
283700     MOVE SPACE           TO OHUV-MID-IDANALYS                            
283800                             OHUV-MID-IDKST                               
283900*** OVAN SKALL BELASTA RETURAVD. ENL. G-B STARMAN 980904, MEN SAP         
284000*** SKALL EJ HA KOSTN.STÄLLE ENL TUULA 981026.                            
284100     MOVE JA              TO OHUV-MID-FLAUTFAK                            
284200     MOVE JA              TO OHUV-MID-FLAUTPAC                            
284300     MOVE NEJ             TO OHUV-MID-FLEMBORD                            
284400     MOVE NEJ             TO OHUV-MID-FLOVRLEV                            
284500     MOVE '57'            TO OHUV-MID-IDFTG                               
284600     MOVE SPACE           TO OHUV-MID-IDKAMPRF                            
284700                             OHUV-MID-ADBET                               
284800                             OHUV-MID-BEBET                               
284900                             OHUV-MID-IDSKYLT                             
285000                             OHUV-MID-FLLSBOK                             
285100                             OHUV-MID-IDBILREG                            
285200                             OHUV-MID-IDVIN                               
285300                             OHUV-MID-IDCISNR                             
285400     MOVE SPACE           TO OHUV-MID-KDORDTYP-LDC                        
285500     MOVE ZERO            TO OHUV-MID-TIREPDAT                            
285700                             OHUV-MID-IDDEPT                              
285800                             OHUV-MID-IDGROSS                             
285900     MOVE MSGI-IDDC       TO OHUV-MID-IDDC                                
286000     MOVE NEJ             TO OHUV-MID-FLFORBI                             
286100                             OHUV-MID-FLORDTIL                            
286200                                                                          
286300     MOVE OHUV-KOM-AREA   TO P-TO-P4-DATA                                 
286400                                                                          
286500     PERFORM S02-CALL-W006KOM                                             
286600     .                                                                    
286700     EJECT                                                                
286800 S09-SKAPA-HUVUD-ORDERRADER SECTION.                                      
286900                                                                          
287000     COMPUTE P-TO-P4-LL =  LNG-P-TO-P-PREFIX +                            
287100                          LENGTH OF ORAD-MID-W4I25201                     
287200                                                                          
287300     MOVE LOW-VALUE        TO P-TO-P4-Z1                                  
287400     MOVE LOW-VALUE        TO P-TO-P4-Z2                                  
287500     MOVE 'W4T252X'        TO P-TO-P4-TRANSKOD                            
287600     MOVE '4252'           TO P-TO-P4-FROM-MID                            
287700     MOVE MFS-KDMFSFOR     TO P-TO-P4-KDMFSFOR                            
287800                                                                          
287900     MOVE SPACE            TO ORAD-KOM-AREA                               
288000                                                                          
288100     MOVE 'W407'           TO ORAD-MID-IDSYSTEM                           
288200                                                                          
288300     IF DCS-IDDC NOT = LEV-IDDC-RET                                       
288400        MOVE LEV-IDDC-RET  TO W-IDDC-B6                                   
288500        PERFORM IMS-GU-WDB601                                             
288600     END-IF                                                               
288700     IF  DCS-KDDC NOT = SPACE                                             
288800         MOVE DCS-IDDISTR-RSKROT     TO WS-IDDISTR-N                      
288900         MOVE WS-IDDISTR-X           TO ORAD-MID-IDDISTR                  
289000         MOVE DCS-IDKUNDNR-RSKROT    TO WS-IDKUNDNR-N                     
289100         MOVE WS-IDKUNDNR-X          TO ORAD-MID-IDKUNDNR                 
289200     END-IF                                                               
289300                                                                          
289400     MOVE W-IDORDNR-X      TO ORAD-MID-IDORDNR                            
289500     MOVE SPACE            TO ORAD-MID-BEVOLREF                           
289600     MOVE SPACE            TO ORAD-MID-IDKUNDRF-RO                        
289700     MOVE 'N'              TO ORAD-MID-FLSLUT                             
289800                                                                          
289900     .                                                                    
290000     EJECT                                                                
290100 S10-EDIT-TRANS-ORDERRADER SECTION.                                       
290200                                                                          
290300     MOVE W-IDARTNR       TO ORAD-MID-IDARTNR      (ORAD-IX)              
290400                             REK-IDARTNR                                  
290500     MOVE 9               TO REK-LNGD                                     
290600     MOVE 0               TO REK-REKSIFFR                                 
290700                                                                          
290800     CALL W009KSIF        USING REK-IDARTNR                               
290900                                REK-LNGD                                  
291000                                REK-REKSIFFR                              
291100                                                                          
291200     MOVE REK-REKSIFFR     TO ORAD-MID-REKSIFFR  (ORAD-IX)                
291300     MOVE W-KVRETINL-R32   TO W-KVSKROT-6                                 
291400     MOVE W-KVSKROT-6-X    TO ORAD-MID-KVBEART   (ORAD-IX)                
291500     MOVE SPACE            TO ORAD-MID-PRARTNTO  (ORAD-IX)                
291600                              ORAD-MID-TITPO     (ORAD-IX)                
291700                              ORAD-MID-FLRESTN   (ORAD-IX)                
291800                              ORAD-MID-KDKVBRYT  (ORAD-IX)                
291900                              ORAD-MID-FLINVEST  (ORAD-IX)                
292000     MOVE ZERO             TO ORAD-MID-KDVRINFO  (ORAD-IX)                
292100**** MAN FLYTTAR EJ KONTO PÅ RADNIVÅ TILL SAP.                            
292200     MOVE ZERO             TO ORAD-MID-IDKONTO   (ORAD-IX)                
292400     MOVE SPACE            TO ORAD-MID-BERADREF  (ORAD-IX)                
292500                              ORAD-MID-IDBIL     (ORAD-IX)                
292510                              ORAD-MID-IDKST     (ORAD-IX)                
292600     MOVE 2                TO ORAD-MID-KDDSP     (ORAD-IX)                
292700     MOVE NEJ              TO ORAD-MID-FLSLATT   (ORAD-IX)                
292800     MOVE SPACE         TO ORAD-MID-PRARTNTO-LOC (ORAD-IX)                
292900     MOVE SPACE         TO ORAD-MID-PRARTBTO-LOC (ORAD-IX)                
293000     MOVE SPACE         TO     ORAD-MID-KDVALISO (ORAD-IX)                
293100     MOVE SPACE         TO     ORAD-MID-KDVAT    (ORAD-IX)                
293200     MOVE 0             TO     ORAD-MID-RERAB    (ORAD-IX)                
293300     MOVE SPACE         TO     ORAD-MID-KDRAB    (ORAD-IX)                
293400     MOVE SPACE         TO ORAD-MID-BEART-VIPS   (ORAD-IX)                
293500     MOVE ZERO          TO ORAD-MID-ADLAGOMR-CD  (ORAD-IX)                
293600                           ORAD-MID-ADGANG-CD    (ORAD-IX)                
293700                           ORAD-MID-ADPLATS-CD   (ORAD-IX)                
293800     MOVE SPACE         TO ORAD-MID-IDKUNDRF-WIP (ORAD-IX)                
293900     .                                                                    
294000     EJECT                                                                
294100 S11-SPARA-R32-MID  SECTION.                                              
294200                                                                          
294300     MOVE MSGI-IDDISTR      TO SPAR4797-MID-IDDISTR    (4797-IX2)         
294400     MOVE MSGI-IDKUNDNR     TO SPAR4797-MID-IDKUNDNR   (4797-IX2)         
294500     MOVE MSGI-IDRAPPNR     TO SPAR4797-MID-IDRAPPNR   (4797-IX2)         
294600     MOVE LEV-IDARTNR       TO SPAR4797-MID-IDARTNR    (4797-IX2)         
294700     MOVE LEV-IDRADNR       TO SPAR4797-MID-IDRADNR    (4797-IX2)         
294800     MOVE W-KVRETINL-R32    TO SPAR4797-MID-KVRETINL   (4797-IX2)         
294900     MOVE W-KVAVV-KVANT-R32                                               
295000                           TO SPAR4797-MID-KVAVV-KVANT (4797-IX2)         
295100     MOVE ZERO                                                            
295200                           TO SPAR4797-MID-KVRETINL-TRP(4797-IX2)         
295300     MOVE W-KVRETINL-R32-SKR                                              
295400                           TO SPAR4797-MID-KVRETINL-SKR(4797-IX2)         
295500                                                                          
295600     ADD +1                 TO 4797-IX2                                   
295700                                                                          
295800     .                                                                    
295900     EJECT                                                                
296000                                                                          
296100 MFS-RENSA-FAELT-UT SECTION.                                              
296200                                                                          
296300     MOVE MFS-RENSA-FAELT       TO MOD-IDANSV                             
296400                                   MOD-ADINLOMR                           
296500                                   MOD-IDANSTNR                           
296600                                   MOD-TIRETILL                           
296700                                   MOD-TILOSSN                            
296800                                   MOD-KVRADER                            
296900                                   MOD-KVRADER-OBEH                       
297000                                   MOD-KVKOLLI                            
297100                                   MOD-IDILIST-NY                         
297200                                                                          
297300     MOVE +1                    TO INDX                                   
297400     PERFORM UNTIL INDX         >  MAX-INDX                               
297500        PERFORM MFS-RENSA-RAD-FAELT-UT                                    
297600        ADD +1                  TO INDX                                   
297700     END-PERFORM                                                          
297800     .                                                                    
297900     SKIP3                                                                
298000 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
298100                                                                          
298200     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR      (INDX)                      
298300                             MOD-BEART        (INDX)                      
298400                             MOD-KVANTAL-KVAR (INDX)                      
298500                             MOD-KDANMORS     (INDX)                      
298600                             MOD-ADLAGOMR     (INDX)                      
298700                             MOD-ADGANG       (INDX)                      
298800                             MOD-ADPLATS      (INDX)                      
298900                             MOD-IDRADNR      (INDX)                      
299000                             MOD-IDILIST      (INDX)                      
299100                             MOD-FLTEXT       (INDX)                      
299200                             MOD-FLKONTROLL   (INDX)                      
299300     .                                                                    
299400     SKIP3                                                                
299500 MFS-RENSA-FAELT-IN SECTION.                                              
299600                                                                          
299700*    --- ALLA INDATA-FÄLT                                                 
299800     IF MID-IDANSTNR-UPD NUMERIC AND EGEN-MID                             
299900       MOVE MFS-ROER-EJ-FAELT     TO MOD-IDANSTNR-UPD                     
300000     ELSE                                                                 
300100       MOVE MFS-RENSA-FAELT       TO MOD-IDANSTNR-UPD                     
300200     END-IF                                                               
300300                                                                          
300400     MOVE MFS-RENSA-FAELT       TO MOD-IDPRT                              
300500                                   MOD-FLILI                              
300600                                   MOD-FLKLAR                             
300700                                   MOD-FLSKROT                            
300800                                   MOD-FLANTAVV                           
300900                                                                          
301000     MOVE +1 TO INDX                                                      
301100     PERFORM UNTIL INDX         >  MAX-INDX                               
301200       MOVE MFS-RENSA-FAELT     TO MOD-KDCMDVAL (INDX)                    
301300                                   MOD-KVANTAL  (INDX)                    
301400       ADD +1                   TO INDX                                   
301500     END-PERFORM                                                          
301600     .                                                                    
301700     EJECT                                                                
301800 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
301900                                                                          
302000     MOVE MFS-ROER-EJ-FAELT  TO MOD-IDANSV                                
302100                                MOD-ADINLOMR                              
302200                                MOD-IDANSTNR                              
302300                                MOD-TIRETILL                              
302400                                MOD-TILOSSN                               
302500                                MOD-KVRADER                               
302600                                MOD-KVRADER-OBEH                          
302700                                MOD-KVKOLLI                               
302800                                MOD-IDILIST-NY                            
302900                                                                          
303000     MOVE +1                  TO INDX                                     
303100     PERFORM UNTIL INDX       >  MAX-INDX                                 
303200       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
303300       ADD +1                 TO INDX                                     
303400     END-PERFORM                                                          
303500     .                                                                    
303600                                                                          
303700                                                                          
303800 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
303900                                                                          
304000     MOVE MFS-ROER-EJ-FAELT  TO MOD-IDARTNR      (INDX)                   
304100                                MOD-BEART        (INDX)                   
304200                                MOD-KVANTAL-KVAR (INDX)                   
304300                                MOD-KDANMORS   (INDX)                     
304400                                MOD-ADLAGOMR   (INDX)                     
304500                                MOD-ADGANG     (INDX)                     
304600                                MOD-ADPLATS    (INDX)                     
304700                                MOD-IDRADNR    (INDX)                     
304800                                MOD-IDILIST    (INDX)                     
304900                                MOD-FLTEXT     (INDX)                     
305000                                MOD-FLKONTROLL (INDX)                     
305100     .                                                                    
305200                                                                          
305300 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
305400                                                                          
305500*    --- ALLA INDATA-FÄLT                                                 
305600     MOVE MFS-ROER-EJ-FAELT     TO MOD-IDPRT                              
305700                                   MOD-FLILI                              
305800                                   MOD-IDANSTNR-UPD                       
305900                                   MOD-FLKLAR                             
306000                                   MOD-FLSKROT                            
306100                                   MOD-FLANTAVV                           
306200                                                                          
306300     MOVE +1 TO INDX                                                      
306400     PERFORM UNTIL INDX         >  MAX-INDX                               
306500       MOVE MFS-ROER-EJ-FAELT   TO MOD-KDCMDVAL(INDX)                     
306600                                   MOD-KVANTAL(INDX)                      
306700       ADD +1                   TO INDX                                   
306800     END-PERFORM                                                          
306900     .                                                                    
307000     EJECT                                                                
307100 MFS-FORM-ATTR SECTION.                                                   
307200                                                                          
307300*    --- ALLA INDATA-FÄLT                                                 
307400     MOVE MFS-FORMATETS-ATTR    TO MOD-IDPRT-ATTR                         
307500                                   MOD-IDANSTNR-UPD-ATTR                  
307600                                   MOD-FLKLAR-ATTR                        
307700                                   MOD-FLSKROT-ATTR                       
307800                                   MOD-FLANTAVV-ATTR                      
307900                                   MOD-FLILI-ATTR                         
308000                                                                          
308100     MOVE +1 TO INDX                                                      
308200     PERFORM UNTIL INDX         >  MAX-INDX                               
308300       MOVE MFS-FORMATETS-ATTR  TO MOD-KDCMDVAL-ATTR(INDX)                
308400                                   MOD-KVANTAL-ATTR(INDX)                 
308500       ADD +1                   TO INDX                                   
308600     END-PERFORM                                                          
308700     .                                                                    
308800     EJECT                                                                
308900                                                                          
309000* --- IMS SEKTIONER ---                                                   
309100     SKIP3                                                                
309200 IMS-GET-MSG SECTION.                                                     
309300                                                                          
309400     MOVE '  QC' TO GODK-STATUSKODER                                      
309500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
309600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
309700     PERFORM IMS-STATUSKONTROLL                                           
309800     .                                                                    
309900     SKIP3                                                                
310000 IMS-INSERT-MSG SECTION.                                                  
310100                                                                          
310200     IF MSGI-IDLAND-SPR = 'GB'                                            
310300       MOVE 'N' TO MFS-KDHUVOMR                                           
310400     END-IF                                                               
310500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
310600     MOVE SPACE TO GODK-STATUSKODER                                       
310700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
310800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
310900     PERFORM IMS-STATUSKONTROLL                                           
311000     .                                                                    
311100     EJECT                                                                
311200                                                                          
311300 IMS-ISRT-MSG-ALT-4794 SECTION.                                           
311400                                                                          
311500     MOVE SPACE              TO GODK-STATUSKODER                          
311600     CALL CBLTDLI USING      ISRT W4794-PCB                               
311700                                  P-TO-P-T94                              
311800     MOVE W4794-STATUS-CODE   TO STATUS-WS                                
311900     PERFORM IMS-STATUSKONTROLL                                           
312000     .                                                                    
312100     SKIP3                                                                
312200 IMS-ISRT-MSG-ALT-4723 SECTION.                                           
312300                                                                          
312400     MOVE SPACE              TO GODK-STATUSKODER                          
312500     CALL CBLTDLI USING      ISRT W4723-PCB                               
312600                                  P-TO-P-T23                              
312700     MOVE W4723-STATUS-CODE   TO STATUS-WS                                
312800     PERFORM IMS-STATUSKONTROLL                                           
312900     .                                                                    
313000     SKIP3                                                                
313100 IMS-GHU-WLKREE01       SECTION.                                          
313200                                                                          
313300     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X                            
313400                    '&IDFTG    =' W-IDFTG-X    ')'                        
313500          DELIMITED BY SIZE INTO SSA1                                     
313600     MOVE '  GE'           TO GODK-STATUSKODER                            
313700     CALL CBLTDLI USING GHU KREE-PCB DLI-IO-AREA SSA1                     
313800     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
313900     PERFORM IMS-STATUSKONTROLL                                           
314000     .                                                                    
314100                                                                          
314200 IMS-REPL-WLKREE        SECTION.                                          
314300                                                                          
314400     MOVE '    '           TO GODK-STATUSKODER                            
314500     CALL CBLTDLI USING REPL KREE-PCB DLI-IO-AREA                         
314600     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
314700     PERFORM IMS-STATUSKONTROLL                                           
314800     .                                                                    
314900     EJECT                                                                
315000                                                                          
315100                                                                          
315200 IMS-GNP-WLKREE11       SECTION.                                          
315300                                                                          
315400     STRING 'WLKREE11(WDA211KY>=' W-WDA211KY-MIN-X ')'                    
315500          DELIMITED BY SIZE INTO SSA1                                     
315600     MOVE '  GE'           TO GODK-STATUSKODER                            
315700     CALL CBLTDLI USING GNP KREE-PCB DLI-IO-AREA SSA1                     
315800     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
315900     PERFORM IMS-STATUSKONTROLL                                           
316000     .                                                                    
316100                                                                          
316200 IMS-GNP-WLKREE11-UNIK  SECTION.                                          
316300                                                                          
316400     STRING 'WLKREE11*F(WDA211KY =' W-WDA211KY-X ')'                      
316500          DELIMITED BY SIZE INTO SSA1                                     
316600     MOVE '  GE'           TO GODK-STATUSKODER                            
316700     CALL CBLTDLI USING GNP KREE-PCB DLI-IO-AREA SSA1                     
316800     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
316900     PERFORM IMS-STATUSKONTROLL                                           
317000     .                                                                    
317100                                                                          
317200 IMS-GHNP-WLKREE11      SECTION.                                          
317300                                                                          
317400     STRING 'WLKREE11*F(WDA211KY =' W-WDA211KY-X ')'                      
317500          DELIMITED BY SIZE INTO SSA1                                     
317600     MOVE '  '           TO GODK-STATUSKODER                              
317700     CALL CBLTDLI USING GHNP KREE-PCB DLI-IO-AREA SSA1                    
317800     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
317900     PERFORM IMS-STATUSKONTROLL                                           
318000     .                                                                    
318100                                                                          
318200 IMS-GHNP-WLKREE11-OKVAL    SECTION.                                      
318300                                                                          
318400     MOVE 'WLKREE11'       TO SSA1                                        
318500     MOVE '  GE'           TO GODK-STATUSKODER                            
318600     CALL CBLTDLI USING GHNP KREE-PCB DLI-IO-AREA SSA1                    
318700     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
318800     PERFORM IMS-STATUSKONTROLL                                           
318900     .                                                                    
319000                                                                          
319100 IMS-GNP-WLKREE21       SECTION.                                          
319200                                                                          
319300     STRING 'WLKREE11(WDA211KY =' W-WDA211KY-X ')'                        
319400          DELIMITED BY SIZE INTO SSA1                                     
319500     MOVE 'WLKREE21'       TO SSA2                                        
319600     MOVE '  GE'           TO GODK-STATUSKODER                            
319700     CALL CBLTDLI USING GNP KREE-PCB DLI-IO-AREA2 SSA1 SSA2               
319800     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
319900     PERFORM IMS-STATUSKONTROLL                                           
320000     .                                                                    
320100                                                                          
320200                                                                          
320300 IMS-GU-WLKREJ01       SECTION.                                           
320400                                                                          
320500     STRING 'WLKREJ01(WDA2E1KY>=' W-WDA2E1KY-MIN-X                        
320600                    '&WDA2E1KY<=' W-WDA2E1KY-MAX-X ')'                    
320700          DELIMITED BY SIZE INTO SSA1                                     
320800     MOVE '  GE'           TO GODK-STATUSKODER                            
320900     CALL CBLTDLI USING GU KREJ-PCB DLI-IO-AREA SSA1                      
321000     MOVE KREJ-STATUS-CODE TO STATUS-WS                                   
321100     PERFORM IMS-STATUSKONTROLL                                           
321200     .                                                                    
321300                                                                          
321400 IMS-GU-WLRETA01       SECTION.                                           
321500                                                                          
321600     STRING 'WLRETA01(WDA3FSEQ>=' W-WDA3FSEQ-MIN-X                        
321700                    '&WDA3FSEQ<=' W-WDA3FSEQ-MAX-X ')'                    
321800          DELIMITED BY SIZE INTO SSA1                                     
321900     MOVE '  GE'           TO GODK-STATUSKODER                            
322000     CALL CBLTDLI USING GU RETA-PCB RET-WDA301 SSA1                       
322100     MOVE RETA-STATUS-CODE TO STATUS-WS                                   
322200     PERFORM IMS-STATUSKONTROLL                                           
322300     .                                                                    
322400                                                                          
322500 IMS-GN-WLRETA01       SECTION.                                           
322600                                                                          
322700     STRING 'WLRETA01(WDA3FSEQ>=' W-WDA3FSEQ-MIN-X                        
322800                    '&WDA3FSEQ<=' W-WDA3FSEQ-MAX-X ')'                    
322900          DELIMITED BY SIZE INTO SSA1                                     
323000     MOVE '  GEGB'         TO GODK-STATUSKODER                            
323100     CALL CBLTDLI USING GN RETA-PCB RET-WDA301 SSA1                       
323200     MOVE RETA-STATUS-CODE TO STATUS-WS                                   
323300     PERFORM IMS-STATUSKONTROLL                                           
323400     .                                                                    
323500                                                                          
324600 IMS-GU-WLARTC11     SECTION.                                             
324700                                                                          
324800     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
324900          DELIMITED BY SIZE INTO SSA1                                     
325000     MOVE 'WLARTC11'   TO  SSA2                                           
325100     MOVE '  GE' TO GODK-STATUSKODER                                      
325200     CALL CBLTDLI USING GU ARTC-PCB CLAG-WDK611 SSA1 SSA2                 
325300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
325400     PERFORM IMS-STATUSKONTROLL                                           
325500     .                                                                    
325600                                                                          
325700 IMS-GU-WLARTS11     SECTION.                                             
325800                                                                          
325900     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
326000          DELIMITED BY SIZE INTO SSA1                                     
326100     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
326200          DELIMITED BY SIZE INTO SSA2                                     
326300     MOVE '  GE' TO GODK-STATUSKODER                                      
326400     CALL CBLTDLI USING GU ARTS-PCB SLAG-WDK711 SSA1 SSA2                 
326500     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
326600     PERFORM IMS-STATUSKONTROLL                                           
326700     .                                                                    
326800                                                                          
326900 IMS-GU-WLBENA11     SECTION.                                             
327000                                                                          
327100     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
327200            DELIMITED BY SIZE INTO SSA1                                   
327300     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
327400            DELIMITED BY SIZE INTO SSA2                                   
327500     MOVE '  ' TO GODK-STATUSKODER                                        
327600     CALL CBLTDLI USING GU  BENA-PCB TEXT-WDD311 SSA1 SSA2                
327700     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
327800     PERFORM IMS-STATUSKONTROLL                                           
327900     .                                                                    
328000     EJECT                                                                
328100 IMS-GHU-WL411111    SECTION.                                             
328200                                                                          
328300     STRING 'WL411101(WDGXKEY  =' W-WDGXKEY-4111-X ')'                    
328400            DELIMITED BY SIZE INTO SSA1                                   
328500     MOVE 'WL411111 ' TO SSA2                                             
328600     MOVE '  ' TO GODK-STATUSKODER                                        
328700     CALL CBLTDLI USING GHU 4111-PCB DLI-IO-AREA4 SSA1 SSA2               
328800     MOVE 4111-STATUS-CODE TO STATUS-WS                                   
328900     PERFORM IMS-STATUSKONTROLL                                           
329000     .                                                                    
329100     EJECT                                                                
329200 IMS-REPL-WL4111    SECTION.                                              
329300                                                                          
329400     MOVE '  ' TO GODK-STATUSKODER                                        
329500     CALL CBLTDLI USING REPL 4111-PCB DLI-IO-AREA4                        
329600     MOVE 4111-STATUS-CODE TO STATUS-WS                                   
329700     PERFORM IMS-STATUSKONTROLL                                           
329800     .                                                                    
329900     EJECT                                                                
330000*IMS-GHU-RETA01-FSEQ SECTION.                                             
330100*                                                                         
330200*    STRING 'WLRETA01(WDA3FSEQ =' W-WDA3FSEQ-X ')'                        
330300*           DELIMITED BY SIZE INTO SSA1                                   
330400*    MOVE '  ' TO GODK-STATUSKODER                                        
330500*    CALL CBLTDLI USING GHU RETA3-PCB RET-WDA301 SSA1                     
330600*    MOVE RETA3-STATUS-CODE TO STATUS-WS                                  
330700*    PERFORM IMS-STATUSKONTROLL                                           
330800*    .                                                                    
330900*    EJECT                                                                
331000*IMS-GHN-RETA01-FSEQ SECTION.                                             
331100*                                                                         
331200*    STRING 'WLRETA01(WDA3FSEQ =' W-WDA3FSEQ-X ')'                        
331300*           DELIMITED BY SIZE INTO SSA1                                   
331400*    MOVE '  GEGB' TO GODK-STATUSKODER                                    
331500*    CALL CBLTDLI USING GHN RETA3-PCB RET-WDA301 SSA1                     
331600*    MOVE RETA3-STATUS-CODE TO STATUS-WS                                  
331700*    PERFORM IMS-STATUSKONTROLL                                           
331800*    .                                                                    
331900*    EJECT                                                                
332000*IMS-REPL-RETA-FSEQ  SECTION.                                             
332100*                                                                         
332200*    MOVE '  ' TO GODK-STATUSKODER                                        
332300*    CALL CBLTDLI USING REPL RETA3-PCB RET-WDA301                         
332400*    MOVE RETA3-STATUS-CODE TO STATUS-WS                                  
332500*    PERFORM IMS-STATUSKONTROLL                                           
332600*    .                                                                    
332700*    EJECT                                                                
332800 IMS-GU-W6KVAH11     SECTION.                                             
332900                                                                          
333000     STRING 'W6KVAH01(IDARTNR  =' W-IDARTNR-X ')'                         
333100            DELIMITED BY SIZE INTO SSA1                                   
333200     STRING 'W6KVAH11(KDKVAINF =' W-KDKVAINF-X ')'                        
333300            DELIMITED BY SIZE INTO SSA2                                   
333400     MOVE '  GE' TO GODK-STATUSKODER                                      
333500     CALL CBLTDLI USING GU  KVAH-PCB INFO-W6D211 SSA1 SSA2                
333600     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
333700     PERFORM IMS-STATUSKONTROLL                                           
333800     .                                                                    
333900     EJECT                                                                
334000 IMS-GU-WDB601    SECTION.                                                
334100     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
334200          DELIMITED BY SIZE INTO SSA1                                     
334300     MOVE '  GE' TO GODK-STATUSKODER                                      
334400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
334500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
334600     PERFORM IMS-STATUSKONTROLL                                           
334700     IF SEGMENT-SAKNAS                                                    
334800        MOVE SPACE TO DCS-KDDC                                            
334900     END-IF                                                               
335000     .                                                                    
335100     EJECT                                                                
335110 IMS-GU-WDR501    SECTION.                                                
335120                                                                          
335130     STRING 'WDR501  (WDGXKEY >=' W-WDGXKEY-X ')'                         
335140          DELIMITED BY SIZE INTO SSA1                                     
335150     MOVE '    ' TO GODK-STATUSKODER                                      
335160     CALL CBLTDLI USING GU WDR5-PCB DLI-IO-WDGX01DC SSA1                  
335170     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
335180     PERFORM IMS-STATUSKONTROLL                                           
335190     .                                                                    
335191     EJECT                                                                
335192 IMS-ISRT-WDR501    SECTION.                                              
335193                                                                          
335194     MOVE '    '           TO GODK-STATUSKODER                            
335195     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-WDGX01DC SSA1                
335196     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
335197     PERFORM IMS-STATUSKONTROLL                                           
335198     .                                                                    
335199     EJECT                                                                
335200 IMS-GHU-WDR5-4704 SECTION.                                               
335201                                                                          
335202     STRING 'WDR501  (WDGXKEY >=' W-WDGXKEY-X ')'                         
335203          DELIMITED BY SIZE INTO SSA1                                     
335204     STRING 'WDGX4704(IDILIST  =' W-IDILIST-4704-X ')'                    
335205          DELIMITED BY SIZE INTO SSA2                                     
335206     MOVE '    ' TO GODK-STATUSKODER                                      
335207     CALL CBLTDLI USING GHU WDR5-PCB DLI-IO-WDGX4704 SSA1 SSA2            
335208     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
335209     PERFORM IMS-STATUSKONTROLL                                           
335210     .                                                                    
335211     EJECT                                                                
335212 IMS-REPL-WDR5-4704 SECTION.                                              
335213                                                                          
335214     MOVE '    '           TO GODK-STATUSKODER                            
335215     CALL CBLTDLI USING REPL WDR5-PCB DLI-IO-WDGX4704                     
335216     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
335217     PERFORM IMS-STATUSKONTROLL                                           
335218     .                                                                    
335219     EJECT                                                                
335220 IMS-ISRT-WDR5-4704 SECTION.                                              
335221                                                                          
335222     STRING 'WDR501  (WDGXKEY >=' W-WDGXKEY-X ')'                         
335223          DELIMITED BY SIZE INTO SSA1                                     
335224     MOVE 'WDGX4704 '         TO SSA2                                     
335225     MOVE '    ' TO GODK-STATUSKODER                                      
335226     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-WDGX4704 SSA1 SSA2           
335227     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
335228     PERFORM IMS-STATUSKONTROLL                                           
335229     .                                                                    
335230     EJECT                                                                
335240 IMS-STATUSKONTROLL SECTION.                                              
335300                                                                          
335400     SET STATUS-IX TO 1                                                   
335500     SEARCH GODK-STATUS                                                   
335600       AT END                                                             
335700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
335800         DELIMITED BY SIZE INTO FELTEXT                                   
335900         CALL FELLOG                                                      
336000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
336100         CONTINUE                                                         
336200     END-SEARCH                                                           
337000     .                                                                    
