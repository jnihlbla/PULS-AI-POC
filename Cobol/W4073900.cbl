000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4073900.                                                
000300 AUTHOR.         SUSANNE OLSSON.                                          
000400 DATE-WRITTEN.   99/07/05.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        MPP-PROGRAM SOM BÅDE GER MÖJLIGHET ATT FRÅGA PÅ VISS             
000900*        LEVERANSANMÄRKNING OCH ATT UPPDATERA DEN ARTIKEL SOM             
001000*        KOM HIT SOM ART.NR 100 (OKÄND ARTIKEL) PÅ ANGIVEN RETUR-         
001100*        RAD. ENBART ART.NR 100 FÅR UPPDATERAS OCH RADEN MÅSTE HA         
001200*        ETT INL.DATUM FÖR ART.NR 100.FRÅN INL.DAT HAR MAN 14 DGR         
001300*        PÅ SIG ATT RAPPORTERA IN ARTIKELN ENLIGT G-B STARMAN.            
001400*                                                                         
001500*        PROGRAMMET UPPDATERAR WLKREE (WDA2)                              
001600*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
001700*        PROGRAMMET UPPDATERAR WLARTS (WDK7)                              
001800*        PROGRAMMET UPPDATERAR WLLOGA (WDL9)                              
001900*        PROGRAMMET UPPDATERAR WLINVA (WDH1)                              
002000*        PROGRAMMET UPPDATERAR WLINLE (WDL2)                              
002100*        PROGRAMMET UPPDATERAR WLINLC (WDL6)                              
002200*        PROGRAMMET UPPDATERAR WLSAPA (WDR9)                              
002300*        PROGRAMMET UPPDATERAR WDR6                                       
002400*        PROGRAMMET LÄSER      WL4111 (WDR1)                              
002500*        PROGRAMMET LÄSER              WDD3                               
002600*                                                                         
002700*        PROGRAMMET SKAPAR SKROTORDER VIA DISPATCHEN                      
002800*                                                                         
002900*    E-TRACKER : 1658417 DATED 2006-03-10                                 
003000*    E-TRACKER : 5165899 DATED 2007-06-12                                 
003100*                                                                         
003200                                                                          
003300*    INDATA.                                                              
003400*        TRANSAKTION: W4T739                                              
003500*        MID:         W4I73901                                            
003600*                                                                         
003700*    UTDATA.                                                              
003800*        MOD:         W4O73901                                            
003900                                                                          
004000     SKIP3                                                                
004100 ENVIRONMENT DIVISION.                                                    
004200     EJECT                                                                
004300 DATA DIVISION.                                                           
004400 WORKING-STORAGE SECTION.                                                 
004500                                                                          
004600*    -- CHECKED BY WY2000                                                 
004700 77  IDPGM                       PIC X(08)   VALUE 'W4073900'.            
004800                                                                          
004900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
005000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005100                                                                          
005200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   VALUE +16 COMP SYNC.         
005300                                                                          
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005600                                                                          
005700                                                                          
005800 77  VISA-INFO-SW                PIC X       VALUE 'J'.                   
005900     88  VISA-INFO-OK                        VALUE 'J'.                   
006000     88  VISA-INFO-FEL                       VALUE 'N'.                   
006100                                                                          
006200                                                                          
006300 77  WS-ANTAL                    PIC S9(7)   VALUE +0 COMP-3.             
006400 77  WS-ANTAL-REKL               PIC S9(7)   VALUE +0 COMP-3.             
006500 77  INV-FINNS                   PIC X       VALUE 'N'.                   
006600 77  WS-FLSALDOJUST              PIC X       VALUE 'N'.                   
006700 77  WS-ART-KDERS-UTG            PIC S9(3)   VALUE +0 COMP-3.             
006800 77  WS-ART-IDLEVNR              PIC  X(5)   VALUE SPACE.                 
006900 77  WS-ART-KDSORT               PIC X(2)    VALUE SPACE.                 
007000 77  WS-ART-KDPRODSL             PIC S9(3)   VALUE +0 COMP-3.             
007100 77  WS-CLAG-KDVVKL              PIC S9      VALUE +0 COMP-3.             
007200 77  WS-CLAG-KVKP                PIC S9(7)   VALUE +0 COMP-3.             
007300 77  WS-CLAG-PRARTSTD            PIC S9(7)V9(2) VALUE +0 COMP-3.          
007400                                                                          
007500 77  W-IDRT-KEY              PIC X(2)    VALUE SPACE.                     
007600                                                                          
007700*    --- ARBETSFÄLT FÖR BERÄKNING AV DAT./TID                             
007800 77  WS-AAAAMMDD                 PIC 9(8)    VALUE ZERO.                  
007900 77  WS-TTMMSSTH                 PIC 9(8)    VALUE ZERO.                  
008000                                                                          
008100 01  WS-INV-DAREGDAT-AREA.                                                
008200     03  WS-INV-DAREGDAT     PIC 9(9) VALUE ZERO.                         
008300     03  FILLER REDEFINES WS-INV-DAREGDAT.                                
008400       05  WS-INV-NOLL         PIC 9(1).                                  
008500       05  WS-INV-AAAAMMDD     PIC 9(8).                                  
008600                                                                          
008700 01  WS-TISEGKEYAREA.                                                     
008800     03  WS-TIAAAAMMDDL      PIC 9(9) VALUE ZERO.                         
008900     03  FILLER REDEFINES WS-TIAAAAMMDDL.                                 
009000         05  WS-TIAAAAMMDD   PIC 9(8).                                    
009100         05  WS-LOPNR        PIC 9(1).                                    
009200     03  WS-TISEGKEY         PIC S9(9) VALUE ZERO COMP-3.                 
009300     EJECT                                                                
009400                                                                          
009500*     -- DATE + TIME                                                      
009600 01      WS-TIAAAAMMDDTTMMSSTH     PIC 9(16)   VALUE ZERO.                
009700 01       FILLER                REDEFINES WS-TIAAAAMMDDTTMMSSTH.          
009800     03    WS-TISEKEL               PIC 9(2).                             
009900     03    WS-TIAAMMDDTTMMSSTH-DATE PIC 9(6).                             
010000     03    WS-TIAAMMDDTTMMSSTH-TIME PIC 9(8).                             
010100     EJECT                                                                
010200                                                                          
010300 01  W-SKROT-FAELT.                                                       
010400     03  W-KVSKROT-6-X.                                                   
010500         05  W-KVSKROT-6         PIC 9(6).                                
010600                                                                          
010700     03  W-IDORDNR-X.                                                     
010800       05 FILLER          PIC 9(2).                                       
010900       05 W-IDORDNR.                                                      
011000         10  W-IDORDNR-VV    PIC 9(2).                                    
011100         10  W-IDORDNR-LLL   PIC 9(3).                                    
011200                                                                          
011300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
011400 01  FILLER REDEFINES DAGENS-DATUM.                                       
011500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
011600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
011700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
011800     EJECT                                                                
011900                                                                          
012000*    --- INDEX FÖR BLÄDDRINGSRADER                                        
012100 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
012200 77  MAX-INDX                    PIC S9(4)  VALUE +10   COMP SYNC.        
012300 77  ORAD-IX                     PIC S9(9)  VALUE +0    COMP SYNC.        
012400 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
012500                                                                          
012600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
012700                                                                          
012800 77  INDATA-SW                   PIC X       VALUE 'J'.                   
012900     88  INDATA-OK                           VALUE 'J'.                   
013000     88  INDATA-FEL                          VALUE 'N'.                   
013100                                                                          
013200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
013300     88  NYCKLAR-OK                          VALUE 'J'.                   
013400     88  NYCKLAR-FEL                         VALUE 'N'.                   
013500                                                                          
013600 77  TRANS-OHUVUD-DAM-SKAPAD-SW  PIC X       VALUE 'N'.                   
013700     88  TRANS-OHUVUD-DAM-SKAPAD             VALUE 'J'.                   
013800                                                                          
013900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
014000     88  EGEN-MID                            VALUE '4739'.                
014100     88  GODK-MID                            VALUE '4712' '4713'          
014200                                                   '4714' '4715'          
014300                                                   '4716' '4722'          
014400                                                   '4723' '4737'          
014500                                                   '4739' '4724'.         
014600     88  HELP-MID                            VALUE '0551'.                
014700                                                                          
014800 77  SW-STARTA-ANNAN-BILD        PIC X       VALUE 'N'.                   
014900     88 STARTA-ANNAN-BILD                    VALUE 'J'.                   
015000                                                                          
015100     EJECT                                                                
015200 01  FILLER.                                                              
015300     03  W-IDLEVNR-PIC9          PIC 9(5).                                
015400                                                                          
015500 01  W-IDLOPNRM                  PIC 9(9)    VALUE ZERO.                  
015600                                                                          
015700 01  W-0VVDLLLLK  REDEFINES W-IDLOPNRM.                                   
015800     03 FILLER                   PIC 9(1).                                
015900     03 W-VVD                    PIC 9(3).                                
016000     03 W-LLLL                   PIC 9(4).                                
016100     03 W-K                      PIC 9(1).                                
016200                                                                          
016300     EJECT                                                                
016400                                                                          
016500 01  WS-KOMMENTAR.                                                        
016600     03  INV-K-IDDISTR           PIC 9(4).                                
016700     03  FILLER                  PIC X.                                   
016800     03  INV-K-IDKUNDNR          PIC 9(6).                                
016900     03  FILLER                  PIC X.                                   
017000     03  INV-K-IDRAPPNR          PIC 9(7).                                
017100     03  FILLER                  PIC X.                                   
017200     03  INV-K-KVANTAL           PIC 9(3).                                
017300     03  FILLER                  PIC X.                                   
017400     03  INV-K-FLSALDOJUST       PIC X.                                   
017500                                                                          
017600     EJECT                                                                
017700 01  BILD-HOPP-AREOR.                                                     
017800                                                                          
017900   03  FILLER            PIC X(16)   VALUE 'P-TO-P-IO-AREA'.              
018000   03  P-TO-P-IO-AREA.                                                    
018100     05  FILLER                  PIC S9(4)   VALUE +117 COMP SYNC.        
018200     05  FILLER                  PIC X(1)    VALUE LOW-VALUE.             
018300     05  FILLER                  PIC X(1)    VALUE LOW-VALUE.             
018400     05  P-TO-P-KDTRANS          PIC X(8).                                
018500     05  FILLER                  PIC X(4)    VALUE '4739'.                
018600     05  FILLER                  PIC X(1)    VALUE '2'.                   
018700     05  FILLER                  PIC X(100)  VALUE ALL '+'.               
018800                                                                          
018900     EJECT                                                                
019000 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA2'.        
019100*  AREA FÖR DISPATCHEN                                                    
019200 01  P-TO-P-AREA2.                                                        
019300     03  P-TO-P2-LL              PIC S9(4)            COMP SYNC.          
019400     03  P-TO-P2-Z1              PIC  X(1)   VALUE LOW-VALUE.             
019500     03  P-TO-P2-Z2              PIC  X(1)   VALUE LOW-VALUE.             
019600     03  P-TO-P2-TRANSKOD        PIC  X(7).                               
019700     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
019800     03  P-TO-P2-FROM-MID        PIC  X(4).                               
019900     03  P-TO-P2-KDMFSFOR        PIC  X(1).                               
020000     03  P-TO-P2-DATA            PIC  X(1000).                            
020100                                                                          
020200     EJECT                                                                
020300                                                                          
020400     EJECT                                                                
020500*      --- VALID IDDC CODES                                               
020600*                                                                         
020700*01    -COPY WWDCKONS -PRE RET-                                           
020800*01    -COPY WWDC99 -PRE RET-                                             
020900*01    -COPY WWDCKONS                                                     
021000*01    -COPY WWDC99                                                       
021100     EJECT                                                                
021200                                                                          
021300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
021400 01  GENERELLA-SUBPROGRAM.                                                
021500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
021600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
021700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
021800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
021900     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
022000     03  W009KSIF                PIC X(8)    VALUE 'W009KSIF'.            
022100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
022200     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
022300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
022400     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
022500     03  CHECK                   PIC X(8)    VALUE 'CHECK   '.            
022600     03  W009CIA                 PIC X(8)    VALUE 'W009CIA'.             
022700     EJECT                                                                
022800                                                                          
022900*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
023000*01 -COPY WMEDAREA                                                        
023100     SKIP3                                                                
023200 01  MESSAGE-CODES.                                                       
023300     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
023400     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
023500     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
023600     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
023700     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
023800     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
023900     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
024000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
024100     03  ERR-KEY-MISSING         PIC X(3)    VALUE '005'.                 
024200     03  ERR-PART-MISS-ON-FILE   PIC X(3)    VALUE '769'.                 
024300     03  ERR-WRONG-LINE-NUMBER   PIC X(3)    VALUE '014'.                 
024400     03  ERR-UPDATE-NOT-OK       PIC X(3)    VALUE '007'.                 
024500     03  ERR-ALREADY-RECEIVED    PIC X(3)    VALUE '120'.                 
024600     03  ERR-WRONG-LINES         PIC X(3)    VALUE '751'.                 
024700     03  ERR-WRONG-STATUS        PIC X(3)    VALUE '079'.                 
024800     03  ERR-USER-NOT-AUTHORIZED PIC X(3)    VALUE '405'.                 
024900     EJECT                                                                
025000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
025100*                                                                         
025200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
025300     SKIP3                                                                
025400*01 -COPY WMSGINIT                                                        
025500     EJECT                                                                
025600                                                                          
025700 01  KONTROLL-SIFFRA.                                                     
025800     03  REK-IDARTNR             PIC 9(9)    VALUE 0.                     
025900     03  REK-LNGD                PIC 9(1)    VALUE 9.                     
026000     03  REK-REKSIFFR            PIC 9(1)    VALUE 0.                     
026100                                                                          
026200     EJECT                                                                
026300                                                                          
026400*    --- PARAMETRAR TILL SUBPROGRAM WORKDAY                               
026500 01  FILLER                      PIC X(16)   VALUE 'WORKAREA'.            
026600*01  -COPY WORKAREA                                                       
026700     EJECT                                                                
026800                                                                          
026900*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
027000 01  FILLER                      PIC X(16)   VALUE 'DAT-AREA'.            
027100     SKIP3                                                                
027200 01  DAT-IO-AREA.                                                         
027300*    03  -COPY WDATAREA                                                   
027400     EJECT                                                                
027500                                                                          
027600*    --- PARAMETRAR TILL SUBPROGRAM W009CIA                               
027700*01  -COPY W009CIA                                                        
027800     EJECT                                                                
027900*****************************************************************         
028000*    --- AREOR FÖR W006KOM SUBMODUL                                       
028100*                                                                         
028200 01  FILLER                      PIC X(16)   VALUE 'MSG-KOM-AREA'.        
028300*01  -COPY WMSGKOM                                                        
028400     EJECT                                                                
028500*                                                                         
028600 01  FILLER                      PIC X(16)   VALUE 'W4I25101'.            
028700 01  FILLER.                                                              
028800   03  OHUV-KOM-AREA.                                                     
028900*    05      -COPY W4I25101   -PRE OHUV-                                  
029000     EJECT                                                                
029100 01  FILLER                      PIC X(16)   VALUE 'W4I25201'.            
029200 01  FILLER.                                                              
029300   03  ORAD-KOM-AREA.                                                     
029400*    05      -COPY W4I25201   -PRE ORAD-                                  
029500     EJECT                                                                
029600******************************************************************        
029700                                                                          
029800*    --- PARAMETRAR TILL SUBPROGRAM W005WDK7                              
029900*        TILLDELA DATA WDK711                                             
030000 01 FILLER                       PIC X(8)    VALUE 'W005WDK7'.            
030100*   -COPY W005WDK7                                                        
030200     EJECT                                                                
030300                                                                          
030400*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
030500*                                                                         
030600 01  SPAR-AREA.                                                           
030700     03  SPAR-IDTRANS           PIC X(4)    VALUE '4739'.                 
030800     03  SPAR-IDARTNR-ENTER       PIC S9(9)        COMP-3.                
030900     03  SPAR-IDARTNR-NEXT        PIC S9(9)        COMP-3.                
031000     03  SPAR-IDRADNR-ENTER       PIC S9(5)        COMP-3.                
031100     03  SPAR-IDRADNR-NEXT        PIC S9(5)        COMP-3.                
031200     EJECT                                                                
031300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
031400*                                                                         
031500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
031600     SKIP3                                                                
031700*01  MID -COPY W4I73901                                                   
031800     EJECT                                                                
031900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
032000     SKIP3                                                                
032100*01  -COPY WMSGAREA                                                       
032200     EJECT                                                                
032300     03  MOD REDEFINES MSG-AREA.                                          
032400*      05  -COPY W4O73901                                                 
032500     EJECT                                                                
032600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
032700     SKIP3                                                                
032800*01  -COPY WMFSAREA                                                       
032900     EJECT                                                                
033000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
033100*                                                                         
033200     EJECT                                                                
033300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
033400     SKIP3                                                                
033500 01  NYCKLAR-TILL-DLI.                                                    
033600*    --- VÄRDE PÅ BLÄDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN             
033700                                                                          
033800     03  W-IDLEVANM-X.                                                    
033900         05  W-IDDISTR           PIC S9(5)   COMP-3 VALUE ZERO.           
034000         05  W-IDKUNDNR          PIC S9(7)   COMP-3 VALUE ZERO.           
034100         05  W-IDRAPPNR          PIC  9(7)          VALUE ZERO.           
034200                                                                          
034300     03  W-WDA211KY-X.                                                    
034400         05  W-IDARTNR-WDA2      PIC S9(9)   COMP-3 VALUE ZERO.           
034500         05  W-IDRADNR-WDA2      PIC S9(5)   COMP-3 VALUE ZERO.           
034600                                                                          
034700     03  W-IDARTNR-X.                                                     
034800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
034900                                                                          
035000     03  W-KDSEGKEY-X.                                                    
035100         05  W-KDSEGKEY          PIC X(1)    VALUE SPACE.                 
035200                                                                          
035300     03  W-IDDC-X.                                                        
035400         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
035500                                                                          
035600     03  W-DAINLEV-X.                                                     
035700         05  W-DAINLEV           PIC 9(16)   VALUE ZERO.                  
035800                                                                          
035900     03  W-IDFTG-X.                                                       
036000         05  W-IDFTG             PIC  9(2)   VALUE ZERO.                  
036100                                                                          
036200     03  W-IDSKYLT-X.                                                     
036300         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
036400                                                                          
036500                                                                          
036600     03  W-WDGXKEY-4111-X.                                                
036700         05  FILLER              PIC  X(4)   VALUE '4111'.                
036800         05  W-IDRT-4111         PIC  X(3)   VALUE 'CDC'.                 
036900         05  FILLER              PIC  X(23)  VALUE LOW-VALUE.             
037000                                                                          
037100     SKIP2                                                                
037200                                                                          
037300 01  W-WDH111KY-MIN.                                                      
037400     03  IDDC-SEARCH-MIN       PIC X(2).                                  
037500     03  KDINVKAT-SEARCH-MIN   PIC S9(3) VALUE ZERO       COMP-3.         
037600     03  TISEGKEY-SEARCH-MIN   PIC S9(9) VALUE ZERO       COMP-3.         
037700     03  DAREGDAT-SORT-SEARCH-MIN    PIC 9(8)  VALUE ZERO.                
037800                                                                          
037900 01  W-WDH111KY-MAX.                                                      
038000     03  IDDC-SEARCH-MAX       PIC X(2).                                  
038100     03  KDINVKAT-SEARCH-MAX   PIC S9(3) VALUE +999       COMP-3.         
038200     03  TISEGKEY-SEARCH-MAX   PIC S9(9) VALUE +999999999 COMP-3.         
038300     03  DAREGDAT-SORT-SEARCH-MAX    PIC 9(8)  VALUE 99999999.            
038400                                                                          
038500     SKIP2                                                                
038600*    --- STATUS-KOD FRÅN IMS                                              
038700 01  STATUS-WS                   PIC XX.                                  
038800     88  STATUS-OK                           VALUE '  '.                  
038900     88  SEGMENT-FINNS                       VALUE '  '.                  
039000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
039100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
039200     88  TRANSKOD-FEL                        VALUE 'A1'.                  
039300     88  SECURITY-FEL                        VALUE 'A4'.                  
039400     SKIP2                                                                
039500 01  GODK-STATUSKODER.                                                    
039600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
039700     SKIP3                                                                
039800 01  SSA1                        PIC X(128).                              
039900 01  SSA2                        PIC X(128).                              
040000 01  SSA3                        PIC X(128).                              
040100     EJECT                                                                
040200*    --- IMS FUNKTIONSKODER                                               
040300*01  -COPY W0003                                                          
040400     EJECT                                                                
040500*    ---  DLI INPUT-OUTPUT AREA                                           
040600                                                                          
040700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA201'.                      
040800 01  DLI-IO-WDA201.                                                       
040900*    03  -COPY WDA201                                                     
041000     EJECT                                                                
041100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA211'.                      
041200 01  DLI-IO-WDA211.                                                       
041300*    03  -COPY WDA211                                                     
041400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
041500 01  DLI-IO-WDK601.                                                       
041600*    03  -COPY WDK601                                                     
041700     EJECT                                                                
041800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
041900 01  DLI-IO-WDK611.                                                       
042000*    03  -COPY WDK611                                                     
042100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
042200 01  DLI-IO-WDK711.                                                       
042300*    03  -COPY WDK711                                                     
042400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL901'.                      
042500 01  DLI-IO-WDL901.                                                       
042600*    03  -COPY WDL901                                                     
042700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH101'.                      
042800 01  DLI-IO-WDH101.                                                       
042900*    03  -COPY WDH101   -PRE INV-                                         
043000     EJECT                                                                
043100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH111'.                      
043200 01  DLI-IO-WDH111.                                                       
043300*    03  -COPY WDH111                                                     
043400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH121'.                      
043500 01  DLI-IO-WDH121.                                                       
043600*    03  -COPY WDH121                                                     
043700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL201'.                      
043800 01  DLI-IO-WDL201.                                                       
043900*    03  -COPY WDL201  -PRE INLE-                                         
044000     EJECT                                                                
044100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL211'.                      
044200 01  DLI-IO-WDL211.                                                       
044300*    03  -COPY WDL211  -PRE INLE-                                         
044400     EJECT                                                                
044500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL222'.                      
044600 01  DLI-IO-WDL222.                                                       
044700*    03  -COPY WDL222  -PRE INLE-                                         
044800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL601'.                      
044900 01  DLI-IO-WDL601.                                                       
045000*    03  -COPY WDL601  -PRE INLC-                                         
045100     EJECT                                                                
045200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL611'.                      
045300 01  DLI-IO-WDL611.                                                       
045400*    03  -COPY WDL611  -PRE INLC-                                         
045500     EJECT                                                                
045600     SKIP2                                                                
045700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4112'.                    
045800 01  DLI-IO-WDGX4112.                                                     
045900*    03  -COPY WDGX4112                                                   
046000     EJECT                                                                
046100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR901'.                      
046200 01  DLI-IO-WDR901.                                                       
046300*    03  WDR901  -COPY WDR901                                             
046400*    07  -COPY W510EKHA -RED FIL-WDR901-DATA                              
046500     EJECT                                                                
046600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR601'.                      
046700 01  DLI-IO-WDR601.                                                       
046800*    03  -COPY WDR601 -PRE WDR6-                                          
046900*    05  LOGG  -COPY W414100A          -RED WDR6-FIL-WDR601-DATA.         
047000     EJECT                                                                
047100                                                                          
047200 01  FILLER         PIC X(24) VALUE 'DLI-IO-BENA01'.                      
047300 01  DLI-IO-BENA01.                                                       
047400*    03  -COPY WDD301                                                     
047500     EJECT                                                                
047600                                                                          
047700 01  FILLER         PIC X(24) VALUE 'DLI-IO-BENA11'.                      
047800 01  DLI-IO-BENA11.                                                       
047900*    03  -COPY WDD311                                                     
048000     EJECT                                                                
048100                                                                          
048200 LINKAGE SECTION.                                                         
048300*01  -COPY W0009   -PRE MSG-                                              
048400*01  -COPY W0009   -PRE ALT-                                              
048500     EJECT                                                                
048600*01  -COPY W0009   -PRE DISP-                                             
048700     EJECT                                                                
048800*01  -COPY W0008   -PRE USEA-                                             
048900     05  FILLER                  PIC X.                                   
049000                                                                          
049100*01  -COPY W0008  -PRE WDA2-                                              
049200     05  FILLER                  PIC X.                                   
049300                                                                          
049400*01  -COPY W0008  -PRE WDK6-                                              
049500     05  FILLER                  PIC X.                                   
049600                                                                          
049700*01  -COPY W0008  -PRE WDK7-                                              
049800     05  FILLER                  PIC X.                                   
049900                                                                          
050000*01  -COPY W0008  -PRE WDL9-                                              
050100     05  FILLER                  PIC X.                                   
050200                                                                          
050300*01  -COPY W0008  -PRE WDH1-                                              
050400     05  FILLER                  PIC X.                                   
050500                                                                          
050600*01  -COPY W0008  -PRE WDL2-                                              
050700     05  FILLER                  PIC X.                                   
050800                                                                          
050900*01  -COPY W0008  -PRE WDL6-                                              
051000     05  FILLER                  PIC X.                                   
051100     EJECT                                                                
051200*01  -COPY W0008  -PRE 4111-                                              
051300     05  FILLER                  PIC X.                                   
051400     EJECT                                                                
051500*01  -COPY W0008  -PRE KOMA-                                              
051600     05  FILLER                  PIC X.                                   
051700     EJECT                                                                
051800*01  -COPY W0008  -PRE WDR9-                                              
051900     05  FILLER                  PIC X.                                   
052000     EJECT                                                                
052100*01  -COPY W0008  -PRE WDR6-                                              
052200     05  FILLER                  PIC X.                                   
052300     EJECT                                                                
052400*01  -COPY W0008  -PRE BENA-                                              
052500     05  FILLER                  PIC X.                                   
052600     EJECT                                                                
052700*01  -COPY W0008  -PRE WDB6-                                              
052800     05  FILLER                  PIC X.                                   
052900     EJECT                                                                
053000 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB DISP-PCB USEA-PCB              
053100     WDA2-PCB WDK6-PCB WDK7-PCB WDL9-PCB WDH1-PCB WDL2-PCB                
053200     WDL6-PCB 4111-PCB KOMA-PCB WDR9-PCB WDR6-PCB BENA-PCB                
053300     WDB6-PCB.                                                            
053400 MAIN SECTION.                                                            
053500     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB DISP-PCB USEA-PCB              
053600     WDA2-PCB WDK6-PCB WDK7-PCB WDL9-PCB WDH1-PCB WDL2-PCB                
053700     WDL6-PCB 4111-PCB KOMA-PCB WDR9-PCB WDR6-PCB BENA-PCB                
053800     WDB6-PCB.                                                            
053900                                                                          
054000     PERFORM IMS-GET-MSG                                                  
054100     IF SEGMENT-FINNS                                                     
054200       PERFORM A-INIT                                                     
054300       PERFORM B-KOLLA-NYCKLAR                                            
054400       IF NYCKLAR-OK                                                      
054500         IF MFS-UPDATE                                                    
054600           PERFORM G-KOLLA-INPUT                                          
054700           IF INDATA-OK                                                   
054800             PERFORM H-UPPDATERA                                          
054900           ELSE                                                           
055000             MOVE NEJ TO VISA-INFO-SW                                     
055100           END-IF                                                         
055200         ELSE                                                             
055300           IF MFS-FIRST                                                   
055400             PERFORM C-FOERSTA-SIDA                                       
055500           ELSE                                                           
055600             IF MFS-NEXT                                                  
055700               PERFORM D-NAESTA-SIDA                                      
055800             ELSE                                                         
055900               PERFORM E-SAMMA-SIDA                                       
056000             END-IF                                                       
056100           END-IF                                                         
056200         END-IF                                                           
056300         IF STARTA-ANNAN-BILD                                             
056400           CONTINUE                                                       
056500         ELSE                                                             
056600           IF VISA-INFO-OK                                                
056700             PERFORM F-LAES-VISA-INFO                                     
056800           END-IF                                                         
056900         END-IF                                                           
057000       END-IF                                                             
057100       IF STARTA-ANNAN-BILD                                               
057200         CONTINUE                                                         
057300       ELSE                                                               
057400         COMPUTE MSG-KVLL = LENGTH OF MOD-W4O73901 + 4                    
057500         PERFORM IMS-INSERT-MSG                                           
057600       END-IF                                                             
057700     END-IF                                                               
057800                                                                          
057900     MOVE ZERO TO RETURN-CODE                                             
058000     GOBACK                                                               
058100     .                                                                    
058200     EJECT                                                                
058300 A-INIT SECTION.                                                          
058400                                                                          
058500     IF MSG-DUBBLA-TRANSKODER                                             
058600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I73901                 
058700       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
058800       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
058900     ELSE                                                                 
059000       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I73901                  
059100       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
059200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
059300     END-IF                                                               
059400                                                                          
059500     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
059600     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
059700     MOVE MFS-IDTRANS TO W-IDTRANS                                        
059800                                                                          
059900     MOVE LOW-VALUE TO MSG-AREA                                           
060000     MOVE 'W4O739N1' TO MFS-IDMOD                                         
060100     MOVE '4739' TO MOD-IDTRANS                                           
060200     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
060300                                                                          
060400     MOVE SPACE                       TO MED-IDMFSINF                     
060500     MOVE SPACE                       TO MED-IDMFSFEL                     
060600                                                                          
060700     IF EGEN-MID OR HELP-MID                                              
060800       CONTINUE                                                           
060900     ELSE                                                                 
061000       MOVE SPACE TO MFS-KDTRTYP                                          
061100       MOVE '7' TO MFS-IDPFK                                              
061200     END-IF                                                               
061300                                                                          
061400     ACCEPT DAGENS-DATUM FROM DATE                                        
061500                                                                          
061600     MOVE 'IDAG ' TO DAT-KDDATFORM                                        
061700     MOVE ZERO    TO DAT-I-TIDATUM                                        
061800                     DAT-O-TIDATUM                                        
061900     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
062000                         DAT-O-TIDATUM DAT-KDSVAR                         
062100     IF DAT-KDSVAR-FEL                                                    
062200        MOVE 'FEL FRÅN DATKONV' TO FELTEXT                                
062300        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
062400     END-IF                                                               
062500     MOVE IDPGM                TO  WDR6-FIL-IDPGM                         
062600     ACCEPT WDR6-FIL-TIREGDAT       FROM  DATE                            
062700     ACCEPT WDR6-FIL-TIKLOCK        FROM  TIME                            
062800     MOVE ZERO                 TO  WDR6-FIL-IDSEKVNR                      
062900     MOVE 'W414'               TO  WDR6-FIL-CT-IDSYSTEM                   
063000     MOVE '100'                TO  WDR6-FIL-CT-IDPTYP                     
063100     MOVE 'A'                  TO  WDR6-FIL-CT-IDVTYP                     
063200     .                                                                    
063300     EJECT                                                                
063400 B-KOLLA-NYCKLAR SECTION.                                                 
063500                                                                          
063600     MOVE ALL '+'           TO MSGI-WMSGINIT                              
063700     MOVE '001'             TO MSGI-KDCALL                                
063800     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
063900     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
064000     MOVE '4739'            TO MSGI-IDTRANS                               
064100     IF EGEN-MID                                                          
064200        MOVE MID-IDDISTR-IN     TO MSGI-IDDISTR                           
064300        MOVE MID-IDKUNDNR-IN    TO MSGI-IDKUNDNR                          
064400        MOVE MID-IDRAPPNR-IN    TO MSGI-IDRAPPNR                          
064500        MOVE MID-IDARTNR-IN     TO MSGI-IDARTNR                           
064600        MOVE MID-IDRADNR-IN     TO MSGI-IDRADNR                           
064700     END-IF                                                               
064800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
064900     MOVE MSGI-SPAR-AREA    TO SPAR-AREA                                  
065000                                                                          
065100     MOVE 'GB'              TO MED-IDSKYLT                                
065200     MOVE '2'               TO MFS-KDMFSFOR                               
065300                                                                          
065400     MOVE JA TO NYCKLAR-SW                                                
065500     MOVE JA TO VISA-INFO-SW                                              
065600                                                                          
065700                                                                          
065800*    -- KONTROLL AV IDDISTR                                               
065900     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
066000                                                                          
066100     IF MID-IDDISTR-IN  NOT = ALL '+'                                     
066200       MOVE '7'         TO MFS-IDPFK                                      
066300       MOVE SPACE       TO MFS-KDTRTYP                                    
066400     END-IF                                                               
066500     INSPECT MSGI-IDDISTR REPLACING LEADING SPACE BY ZERO                 
066600     IF MSGI-IDDISTR NUMERIC                                              
066700       MOVE MSGI-IDDISTR TO W-IDDISTR                                     
066800     ELSE                                                                 
066900       MOVE NEJ TO NYCKLAR-SW                                             
067000     END-IF                                                               
067100                                                                          
067200*    -- KONTROLL AV IDKUNDNR                                              
067300     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-IN                              
067400                                                                          
067500     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
067600       MOVE '7'         TO MFS-IDPFK                                      
067700       MOVE SPACE       TO MFS-KDTRTYP                                    
067800     END-IF                                                               
067900     INSPECT MSGI-IDKUNDNR REPLACING LEADING SPACE BY ZERO                
068000     IF MSGI-IDKUNDNR NUMERIC                                             
068100       MOVE MSGI-IDKUNDNR TO W-IDKUNDNR                                   
068200     ELSE                                                                 
068300       MOVE NEJ TO NYCKLAR-SW                                             
068400     END-IF                                                               
068500                                                                          
068600*    -- KONTROLL AV IDRAPPNR                                              
068700     MOVE MFS-RENSA-FAELT TO MOD-IDRAPPNR-IN                              
068800                                                                          
068900     IF MID-IDRAPPNR-IN NOT = ALL '+'                                     
069000       MOVE '7'         TO MFS-IDPFK                                      
069100       MOVE SPACE       TO MFS-KDTRTYP                                    
069200     END-IF                                                               
069300     INSPECT MSGI-IDRAPPNR REPLACING LEADING SPACE BY ZERO                
069400     IF MSGI-IDRAPPNR NUMERIC                                             
069500       MOVE MSGI-IDRAPPNR TO W-IDRAPPNR                                   
069600     ELSE                                                                 
069700       MOVE NEJ TO NYCKLAR-SW                                             
069800     END-IF                                                               
069900                                                                          
070000*    -- KONTROLL AV IDARTNR                                               
070100     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
070200                                                                          
070300     IF MID-IDARTNR-IN NOT = ALL '+'                                      
070400       MOVE '7'         TO MFS-IDPFK                                      
070500       MOVE SPACE       TO MFS-KDTRTYP                                    
070600     END-IF                                                               
070700     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
070800     IF MSGI-IDARTNR NUMERIC                                              
070900       MOVE MSGI-IDARTNR TO W-IDARTNR-WDA2                                
071000     END-IF                                                               
071100                                                                          
071200*    -- KONTROLL AV IDRADNR                                               
071300     MOVE MFS-RENSA-FAELT TO MOD-IDRADNR-IN                               
071400                                                                          
071500     IF MID-IDRADNR-IN NOT = ALL '+'                                      
071600       MOVE '7'         TO MFS-IDPFK                                      
071700       MOVE SPACE       TO MFS-KDTRTYP                                    
071800     END-IF                                                               
071900     INSPECT MSGI-IDRADNR REPLACING LEADING SPACE BY ZERO                 
072000     IF MSGI-IDRADNR NUMERIC                                              
072100       MOVE MSGI-IDRADNR TO W-IDRADNR-WDA2                                
072200     END-IF                                                               
072300                                                                          
072400     MOVE MSGI-IDDC            TO WS-IDDC                                 
072500     IF CDC-SE OR NDC-US OR NDC-CA OR NDC-JP OR NDC-AU                    
072600       MOVE MSGI-IDRT-KEY        TO W-IDRT-4111                           
072700     END-IF                                                               
072800     MOVE MSGI-IDFTG             TO W-IDFTG                               
072900                                                                          
073000     MOVE MSGI-IDRT-KEY          TO W-IDRT-KEY                            
073100                                                                          
073200     IF GODK-MID OR NYCKLAR-OK                                            
073300       MOVE MSGI-IDDISTR         TO MOD-IDDISTR-UT                        
073400       INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
073500       MOVE MSGI-IDKUNDNR        TO MOD-IDKUNDNR-UT                       
073600       INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE            
073700       MOVE MSGI-IDRAPPNR        TO MOD-IDRAPPNR-UT                       
073800       INSPECT MOD-IDRAPPNR-UT REPLACING LEADING ZERO BY SPACE            
073900       IF EGEN-MID                                                        
074000         IF  MID-IDARTNR-IN = ALL '+'   AND                               
074100             MID-IDRADNR-IN = ALL '+'                                     
074200           MOVE SPACE                TO MOD-IDARTNR-UT                    
074300           MOVE SPACE                TO MOD-IDRADNR-UT                    
074400         ELSE                                                             
074500           IF MSGI-IDARTNR NOT = ALL '+'                                  
074600             IF MSGI-IDARTNR > 0                                          
074700               MOVE MSGI-IDARTNR     TO MOD-IDARTNR-UT                    
074800               INSPECT MOD-IDARTNR-UT REPLACING                           
074900                                     LEADING ZERO BY SPACE                
075000             END-IF                                                       
075100           ELSE                                                           
075200             MOVE SPACE              TO MOD-IDARTNR-UT                    
075300           END-IF                                                         
075400           IF MSGI-IDRADNR  NOT = ALL '+'                                 
075500             IF MSGI-IDRADNR > 0                                          
075600              MOVE MSGI-IDRADNR      TO MOD-IDRADNR-UT                    
075700              INSPECT MOD-IDRADNR-UT REPLACING                            
075800                                     LEADING ZERO BY SPACE                
075900             END-IF                                                       
076000           ELSE                                                           
076100             MOVE SPACE              TO MOD-IDRADNR-UT                    
076200           END-IF                                                         
076300         END-IF                                                           
076400       ELSE                                                               
076500         MOVE SPACE                  TO MOD-IDARTNR-UT                    
076600         MOVE SPACE                  TO MOD-IDRADNR-UT                    
076700       END-IF                                                             
076800     ELSE                                                                 
076900       MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                             
077000                               MOD-IDKUNDNR-UT                            
077100                               MOD-IDRAPPNR-UT                            
077200                               MOD-IDARTNR-UT                             
077300                               MOD-IDRADNR-UT                             
077400     END-IF                                                               
077500                                                                          
077600     IF NYCKLAR-FEL                                                       
077700       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
077800       CALL WMEDKONV USING MED-WMEDAREA                                   
077900       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
078000       PERFORM MFS-RENSA-FAELT-IN                                         
078100       PERFORM MFS-RENSA-FAELT-UT                                         
078200     END-IF                                                               
078300     .                                                                    
078400     EJECT                                                                
078500 C-FOERSTA-SIDA SECTION.                                                  
078600                                                                          
078700     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
078800     CALL WMEDKONV USING MED-WMEDAREA                                     
078900     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
079000                                                                          
079100     PERFORM MFS-RENSA-FAELT-IN                                           
079200     .                                                                    
079300     EJECT                                                                
079400 D-NAESTA-SIDA SECTION.                                                   
079500                                                                          
079600     IF SPAR-IDTRANS = '4739'                                             
079700       MOVE SPAR-IDARTNR-NEXT TO W-IDARTNR-WDA2                           
079800       MOVE SPAR-IDRADNR-NEXT TO W-IDRADNR-WDA2                           
079900     ELSE                                                                 
080000       PERFORM MFS-RENSA-FAELT-IN                                         
080100     END-IF                                                               
080200     .                                                                    
080300     EJECT                                                                
080400 E-SAMMA-SIDA SECTION.                                                    
080500                                                                          
080600     IF EGEN-MID OR HELP-MID                                              
080700       IF SPAR-IDTRANS = '4739' OR '0551'                                 
080800         MOVE SPAR-IDARTNR-ENTER TO W-IDARTNR-WDA2                        
080900         MOVE SPAR-IDRADNR-ENTER TO W-IDRADNR-WDA2                        
081000       END-IF                                                             
081100                                                                          
081200       IF MID-INPUT NOT = ALL '+'                                         
081300         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
081400         CALL WMEDKONV USING MED-WMEDAREA                                 
081500         MOVE MED-MFSINF TO MOD-TEMFSFEL                                  
081600         PERFORM EA-MID-INDATA-TILL-MOD                                   
081700       ELSE                                                               
081800         MOVE MFS-RENSA-FAELT  TO MOD-IDARTNR-UPD                         
081900         MOVE MFS-RENSA-FAELT  TO MOD-IDRADNR-UPD                         
082000                                                                          
082100         MOVE +1 TO INDX                                                  
082200         PERFORM UNTIL INDX > MAX-INDX                                    
082300           IF MID-IDTRANS-HOPP (INDX)  =  ALL '+'                         
082400             MOVE MFS-RENSA-FAELT  TO MOD-IDTRANS-HOPP (INDX)             
082500           ELSE                                                           
082600             IF MID-IDTRANS-HOPP (INDX) NUMERIC                           
082700               PERFORM EB-STARTA-ANNAN-BILD                               
082800               MOVE JA                  TO SW-STARTA-ANNAN-BILD           
082900               MOVE MAX-INDX TO INDX                                      
083000             ELSE                                                         
083100               MOVE MFS-ALFA-FAELT-FEL TO                                 
083200                                   MOD-IDTRANS-ATTR (INDX)                
083300               MOVE MFS-ROER-EJ-FAELT  TO                                 
083400                                   MOD-IDTRANS-HOPP (INDX)                
083500               MOVE NEJ                TO SW-STARTA-ANNAN-BILD            
083600               MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                  
083700               CALL WMEDKONV USING MED-WMEDAREA                           
083800               MOVE MED-MFSFEL TO MOD-TEMFSFEL                            
083900             END-IF                                                       
084000           END-IF                                                         
084100           ADD +1 TO INDX                                                 
084200         END-PERFORM                                                      
084300       END-IF                                                             
084400     ELSE                                                                 
084500       PERFORM MFS-RENSA-FAELT-IN                                         
084600     END-IF                                                               
084700     .                                                                    
084800     EJECT                                                                
084900 EA-MID-INDATA-TILL-MOD SECTION.                                          
085000                                                                          
085100     IF MID-IDARTNR-UPD = ALL '+'                                         
085200       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UPD                            
085300     ELSE                                                                 
085400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDARTNR-UPD-ATTR                 
085500       MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-UPD                          
085600     END-IF                                                               
085700                                                                          
085800     IF MID-IDRADNR-UPD = ALL '+'                                         
085900       MOVE MFS-RENSA-FAELT TO MOD-IDRADNR-UPD                            
086000     ELSE                                                                 
086100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDRADNR-UPD-ATTR                 
086200       MOVE MFS-ROER-EJ-FAELT TO MOD-IDRADNR-UPD                          
086300     END-IF                                                               
086400                                                                          
086500     .                                                                    
086600     EJECT                                                                
086700 EB-STARTA-ANNAN-BILD SECTION.                                            
086800                                                                          
086900     MOVE MID-IDARTNR (INDX)             TO MSGI-IDARTNR                  
087000     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
087100     MOVE MID-IDRADNR (INDX)             TO MSGI-IDRADNR                  
087200     INSPECT MSGI-IDRADNR REPLACING LEADING SPACE BY ZERO                 
087300                                                                          
087400     MOVE '001'                          TO MSGI-KDCALL                   
087500     MOVE MSG-SIGNON-USERID              TO MSGI-IDUSER                   
087600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
087700                                                                          
087800     STRING 'W' MID-IDTRANS-HOPP(INDX) (1:1)                              
087900            'T' MID-IDTRANS-HOPP(INDX) (2:3) '  '                         
088000                DELIMITED BY SIZE INTO P-TO-P-KDTRANS                     
088100                                                                          
088200     PERFORM S01-INSERT-ALTMSG                                            
088300     .                                                                    
088400     EJECT                                                                
088500 F-LAES-VISA-INFO SECTION.                                                
088600                                                                          
088700     PERFORM IMS-GU-WDA201                                                
088800                                                                          
088900     IF SEGMENT-SAKNAS                                                    
089000        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
089100        CALL WMEDKONV USING MED-WMEDAREA                                  
089200        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
089300        PERFORM MFS-RENSA-FAELT-UT                                        
089400     ELSE                                                                 
089500                                                                          
089600       IF ANM-DARETILL > 0                                                
089700          MOVE ANM-DARETILL(3:6) TO MOD-TIRETILL                          
089800       ELSE                                                               
089900          MOVE MFS-RENSA-FAELT   TO MOD-TIRETILL                          
090000       END-IF                                                             
090100                                                                          
090200       IF ANM-DARETANK > 0                                                
090300          MOVE ANM-DARETANK(3:6) TO MOD-TIRETANK                          
090400       ELSE                                                               
090500          MOVE MFS-RENSA-FAELT   TO MOD-TIRETANK                          
090600       END-IF                                                             
090700                                                                          
090800       MOVE ANM-KDLEVANM    TO MOD-KDLEVANM                               
090900                                                                          
091000*-- NÄR MAN KOMMER FRÅN ANNAN BILD SKALL ALLTID ALLA RADER VISAS          
091100*-- ENLIGT SUSSI 990927.                                                  
091200       IF EGEN-MID                                                        
091300         IF MID-IDARTNR-IN = ALL '+' AND                                  
091400            MID-IDRADNR-IN = ALL '+'                                      
091500            PERFORM FA-LAES-ALLA-RADER                                    
091600         ELSE                                                             
091700           PERFORM IMS-GNP-WDA211-KVAL                                    
091800           IF SEGMENT-FINNS                                               
091900             MOVE +1 TO INDX                                              
092000             MOVE LEV-IDARTNR      TO MOD-IDARTNR  (INDX)                 
092100             MOVE LEV-IDRADNR      TO MOD-IDRADNR  (INDX)                 
092200             MOVE LEV-IDDC         TO MOD-IDDC     (INDX)                 
092300             MOVE LEV-IDORDNR5     TO MOD-IDORDNR5 (INDX)                 
092400             MOVE LEV-KVLEVANM     TO MOD-KVLEVANM (INDX)                 
092500             MOVE LEV-TIINLINL     TO MOD-TIINLINL (INDX)                 
092600             MOVE LEV-KVRETINL     TO MOD-KVRETINL (INDX)                 
092700             MOVE LEV-KVRETINL-SKR TO MOD-KVRETINL-SKR (INDX)             
092800             MOVE LEV-KVAVV-KVANT  TO MOD-KVAVV-KVANT  (INDX)             
092900             MOVE LEV-KVAVV-KVAL   TO MOD-KVAVV-KVAL   (INDX)             
093000             MOVE LEV-FLTEXT       TO MOD-FLTEXT       (INDX)             
093100             MOVE LEV-IDARTNR-DEL  TO MOD-IDARTNR-DEL  (INDX)             
093200                                                                          
093300             IF LEV-IDANSTNR-RET > ZERO                                   
093400               MOVE LEV-IDANSTNR-RET TO MOD-IDANSTNR-RET                  
093500             END-IF                                                       
093600           ELSE                                                           
093700             MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                           
093800             CALL WMEDKONV USING MED-WMEDAREA                             
093900             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
094000             PERFORM MFS-RENSA-FAELT-UT                                   
094100             PERFORM MFS-STAENG-FAELT-IN                                  
094200           END-IF                                                         
094300         END-IF                                                           
094400       ELSE                                                               
094500         PERFORM FA-LAES-ALLA-RADER                                       
094600       END-IF                                                             
094700     END-IF                                                               
094800     .                                                                    
094900     EJECT                                                                
095000 FA-LAES-ALLA-RADER SECTION.                                              
095100                                                                          
095200     IF MFS-FIRST                                                         
095300       PERFORM IMS-GNP-WDA211                                             
095400     ELSE                                                                 
095500       PERFORM IMS-GNP-WDA211-KVAL                                        
095600     END-IF                                                               
095700                                                                          
095800     IF SEGMENT-SAKNAS                                                    
095900       MOVE W-IDARTNR-WDA2    TO SPAR-IDARTNR-ENTER                       
096000       MOVE W-IDRADNR-WDA2    TO SPAR-IDRADNR-ENTER                       
096100       MOVE ERR-KEY-MISSING TO MED-IDMFSFEL                               
096200       CALL WMEDKONV USING MED-WMEDAREA                                   
096300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
096400       PERFORM MFS-RENSA-FAELT-UT                                         
096500     ELSE                                                                 
096600       MOVE LEV-IDARTNR TO SPAR-IDARTNR-ENTER                             
096700       MOVE LEV-IDRADNR TO SPAR-IDRADNR-ENTER                             
096800                                                                          
096900       MOVE +1 TO INDX                                                    
097000                                                                          
097100       PERFORM UNTIL INDX > MAX-INDX                                      
097200         IF SEGMENT-FINNS                                                 
097300           PERFORM FB-FLYTTA-DATA-TILL-MOD                                
097400           PERFORM IMS-GNP-WDA211                                         
097500         ELSE                                                             
097600           PERFORM MFS-RENSA-RAD-FAELT-UT                                 
097700           MOVE MFS-STAENG-FAELT-NOMOD  TO MOD-IDTRANS-ATTR (INDX)        
097800         END-IF                                                           
097900         ADD 1 TO INDX                                                    
098000       END-PERFORM                                                        
098100                                                                          
098200*--OM DET FINNS ETT 11:E SEGMENT.                                         
098300                                                                          
098400       IF SEGMENT-FINNS                                                   
098500         MOVE LEV-IDARTNR TO SPAR-IDARTNR-NEXT                            
098600         MOVE LEV-IDRADNR TO SPAR-IDRADNR-NEXT                            
098700         IF MED-IDMFSINF = SPACE                                          
098800           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
098900           CALL WMEDKONV USING MED-WMEDAREA                               
099000           MOVE MED-MFSINF TO MOD-TEMFSINF                                
099100         END-IF                                                           
099200       ELSE                                                               
099300         MOVE LEV-IDARTNR TO SPAR-IDARTNR-NEXT                            
099400         MOVE LEV-IDRADNR TO SPAR-IDRADNR-NEXT                            
099500         IF MED-IDMFSINF = SPACE                                          
099600           MOVE INF-LAST-PAGE  TO MED-IDMFSINF                            
099700           CALL WMEDKONV USING MED-WMEDAREA                               
099800           MOVE MED-MFSINF TO MOD-TEMFSINF                                
099900         END-IF                                                           
100000       END-IF                                                             
100100                                                                          
100200       MOVE '002'      TO MSGI-KDCALL                                     
100300       MOVE '4739'     TO SPAR-IDTRANS                                    
100400       MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                  
100500       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
100600                                                                          
100700     END-IF                                                               
100800     .                                                                    
100900     EJECT                                                                
101000 FB-FLYTTA-DATA-TILL-MOD SECTION.                                         
101100                                                                          
101200     MOVE LEV-IDARTNR   TO MOD-IDARTNR  (INDX)                            
101300     MOVE LEV-IDRADNR   TO MOD-IDRADNR  (INDX)                            
101400     MOVE LEV-IDDC      TO MOD-IDDC     (INDX)                            
101500     MOVE LEV-IDORDNR5  TO MOD-IDORDNR5 (INDX)                            
101600     MOVE LEV-KVLEVANM  TO MOD-KVLEVANM (INDX)                            
101700     MOVE LEV-TIINLINL  TO MOD-TIINLINL (INDX)                            
101800     MOVE LEV-KVRETINL  TO MOD-KVRETINL (INDX)                            
101900     MOVE LEV-KVRETINL-SKR TO MOD-KVRETINL-SKR (INDX)                     
102000     MOVE LEV-KVAVV-KVANT  TO MOD-KVAVV-KVANT  (INDX)                     
102100     MOVE LEV-KVAVV-KVAL   TO MOD-KVAVV-KVAL   (INDX)                     
102200     MOVE LEV-FLTEXT       TO MOD-FLTEXT       (INDX)                     
102300     MOVE LEV-IDARTNR-DEL  TO MOD-IDARTNR-DEL  (INDX)                     
102400                                                                          
102500     IF LEV-IDANSTNR-RET > ZERO                                           
102600       MOVE LEV-IDANSTNR-RET TO MOD-IDANSTNR-RET                          
102700     END-IF                                                               
102800     .                                                                    
102900     EJECT                                                                
103000 G-KOLLA-INPUT SECTION.                                                   
103100                                                                          
103200     MOVE JA  TO INDATA-SW                                                
103300                                                                          
103400     MOVE MSGI-IDDC            TO WS-IDDC                                 
103500     IF CDC-SE OR NDC-US OR NDC-CA OR NDC-JP OR NDC-AU                    
103600       PERFORM GA-KOLLA-INPUT-DATA                                        
103700     ELSE                                                                 
103800       MOVE ERR-USER-NOT-AUTHORIZED TO MED-IDMFSFEL                       
103900       CALL WMEDKONV USING MED-WMEDAREA                                   
104000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
104100       PERFORM MFS-ROER-EJ-FAELT-IN                                       
104200       PERFORM MFS-ROER-EJ-FAELT-UT                                       
104300       MOVE NEJ TO INDATA-SW                                              
104400     END-IF                                                               
104500     .                                                                    
104600     EJECT                                                                
104700 GA-KOLLA-INPUT-DATA SECTION.                                             
104800                                                                          
104900     IF MID-INPUT = ALL '+'                                               
105000       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
105100       CALL WMEDKONV USING MED-WMEDAREA                                   
105200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
105300       PERFORM MFS-ROER-EJ-FAELT-IN                                       
105400       PERFORM MFS-ROER-EJ-FAELT-UT                                       
105500       MOVE NEJ TO INDATA-SW                                              
105600     ELSE                                                                 
105700                                                                          
105800       IF MID-IDARTNR-UPD  = ALL '+'                                      
105900          MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-UPD-ATTR                  
106000          MOVE NEJ TO INDATA-SW                                           
106100       ELSE                                                               
106200          MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-UPD-ATTR                
106300       END-IF                                                             
106400                                                                          
106500       IF MID-IDRADNR-UPD  = ALL '+'                                      
106600          MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-UPD-ATTR                  
106700          MOVE NEJ TO INDATA-SW                                           
106800       ELSE                                                               
106900          MOVE MFS-NUM-FAELT-RAETT TO MOD-IDRADNR-UPD-ATTR                
107000       END-IF                                                             
107100                                                                          
107200       IF MID-IDARTNR-UPD NOT = ALL '+'                                   
107300         IF MID-IDARTNR-UPD NOT NUMERIC                                   
107400           MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-UPD-ATTR                 
107500           MOVE NEJ TO INDATA-SW                                          
107600         ELSE                                                             
107700           IF MID-IDARTNR-UPD = 00000100                                  
107800             MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-UPD-ATTR               
107900             MOVE NEJ TO INDATA-SW                                        
108000           ELSE                                                           
108100             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-UPD-ATTR             
108200           END-IF                                                         
108300         END-IF                                                           
108400       END-IF                                                             
108500                                                                          
108600       IF MID-IDRADNR-UPD NOT = ALL '+'                                   
108700         IF MID-IDRADNR-UPD NOT NUMERIC                                   
108800           MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-UPD-ATTR                 
108900           MOVE NEJ TO INDATA-SW                                          
109000         ELSE                                                             
109100           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDRADNR-UPD-ATTR               
109200         END-IF                                                           
109300       END-IF                                                             
109400                                                                          
109500* - LÄSER ARTIKELREGISTRET SÅ ATT ARTIKELN FINNS (WDK6)                   
109600                                                                          
109700       IF MID-IDARTNR-UPD NOT = ALL '+'                                   
109800         MOVE MID-IDARTNR-UPD TO W-IDARTNR                                
109900         PERFORM IMS-GU-WDK601                                            
110000         IF SEGMENT-SAKNAS                                                
110100           MOVE ERR-PART-MISS-ON-FILE TO MED-IDMFSFEL                     
110200           MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-UPD-ATTR                 
110300           MOVE NEJ TO INDATA-SW                                          
110400         ELSE                                                             
110500           MOVE ART-KDERS-UTG    TO WS-ART-KDERS-UTG                      
110600           MOVE ART-IDLEVNR      TO WS-ART-IDLEVNR                        
110700           MOVE ART-KDSORT       TO WS-ART-KDSORT                         
110800           MOVE ART-KDPRODSL     TO WS-ART-KDPRODSL                       
110900*---KOLLA SÅ ATT 11-SEGM FINNS, SAKNAS IBLAND FÖR 29-MÄRKT ART            
111000           PERFORM IMS-GNP-WDK611                                         
111100           IF SEGMENT-SAKNAS                                              
111200             MOVE ERR-PART-MISS-ON-FILE TO MED-IDMFSFEL                   
111300             MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-UPD-ATTR               
111400             MOVE NEJ TO INDATA-SW                                        
111500           ELSE                                                           
111600             MOVE CLAG-KDVVKL      TO WS-CLAG-KDVVKL                      
111700             MOVE CLAG-KVKP        TO WS-CLAG-KVKP                        
111800             MOVE CLAG-PRARTSTD    TO WS-CLAG-PRARTSTD                    
111900             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-UPD-ATTR             
112000           END-IF                                                         
112100         END-IF                                                           
112200       END-IF                                                             
112300                                                                          
112400       IF INDATA-FEL                                                      
112500         IF MED-IDMFSFEL = SPACE                                          
112600           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
112700         END-IF                                                           
112800         CALL WMEDKONV USING MED-WMEDAREA                                 
112900         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
113000         PERFORM MFS-ROER-EJ-FAELT-UT                                     
113100         PERFORM MFS-ROER-EJ-FAELT-IN                                     
113200       ELSE                                                               
113300         PERFORM GB-KOLLA-OM-OK-UPPDATERA                                 
113400       END-IF                                                             
113500     END-IF                                                               
113600     .                                                                    
113700     EJECT                                                                
113800 GB-KOLLA-OM-OK-UPPDATERA SECTION.                                        
113900                                                                          
114000*-KOLLA SÅ ATT ANGIVET RADNR HAR ARTNR 100. LEV.ANM HAR STATUS > 5        
114100* OCH ATT DAGENS DATUM ÄR MAX 14 DGR FRÅN INL.DATUM AV ART. 100.          
114200* IDDC OCH IDDC-RET MÅSTE VARA OLIKA.                                     
114300                                                                          
114400     PERFORM IMS-GU-WDA201                                                
114500                                                                          
114600     IF SEGMENT-FINNS                                                     
114700                                                                          
114800       MOVE 100             TO W-IDARTNR-WDA2                             
114900       MOVE MID-IDRADNR-UPD TO W-IDRADNR-WDA2                             
115000                                                                          
115100       IF ANM-KDLEVANM  <  '6'                                            
115200         MOVE ERR-WRONG-STATUS TO MED-IDMFSFEL                            
115300         PERFORM S02-FELMEDDELANDE                                        
115400       ELSE                                                               
115500         PERFORM IMS-GNP-WDA211-KVAL                                      
115600         IF SEGMENT-FINNS                                                 
115700                                                                          
115800           COMPUTE WS-ANTAL  = LEV-KVRETINL + LEV-KVRETINL-SKR            
115900                                                                          
116000           END-COMPUTE                                                    
116100                                                                          
116200           COMPUTE WS-ANTAL-REKL = LEV-KVRETINL     +                     
116300                                   LEV-KVRETINL-SKR +                     
116400                                   LEV-KVAVV-KVAL   +                     
116500                                   LEV-KVAVV-KVANT                        
116600           END-COMPUTE                                                    
116700                                                                          
116800           IF LEV-IDDC NOT = LEV-IDDC-RET                                 
116900             CONTINUE                                                     
117000           ELSE                                                           
117100             MOVE ERR-UPDATE-NOT-OK TO MED-IDMFSFEL                       
117200             PERFORM S02-FELMEDDELANDE                                    
117300           END-IF                                                         
117400                                                                          
117500           IF LEV-TIINLINL > ZERO                                         
117600             PERFORM GC-KOLLA-TIDSINTERVALL                               
117700           ELSE                                                           
117800             MOVE ERR-UPDATE-NOT-OK TO MED-IDMFSFEL                       
117900             PERFORM S02-FELMEDDELANDE                                    
118000           END-IF                                                         
118100                                                                          
118200           IF LEV-IDARTNR-DEL = ZERO                                      
118300             CONTINUE                                                     
118400           ELSE                                                           
118500             MOVE ERR-ALREADY-RECEIVED TO MED-IDMFSFEL                    
118600             PERFORM S02-FELMEDDELANDE                                    
118700           END-IF                                                         
118800         ELSE                                                             
118900           MOVE ERR-WRONG-LINE-NUMBER TO MED-IDMFSFEL                     
119000           PERFORM S02-FELMEDDELANDE                                      
119100         END-IF                                                           
119200       END-IF                                                             
119300     ELSE                                                                 
119400       PERFORM S02-FELMEDDELANDE                                          
119500     END-IF                                                               
119600     .                                                                    
119700     EJECT                                                                
119800 GC-KOLLA-TIDSINTERVALL SECTION.                                          
119900                                                                          
120000     MOVE +001                TO WORK-KDCALL                              
120100     MOVE LEV-IDDC-RET        TO WORK-IDDC                                
120200     MOVE LEV-TIINLINL        TO WORK-TIAAMMDD-FOM                        
120300     MOVE DAGENS-DATUM        TO WORK-TIAAMMDD-TOM                        
120400                                                                          
120500     CALL WORKDAY  USING WORK-KDCALL                                      
120600                         WORK-DATE-AREA                                   
120700                         WORK-KDSVAR                                      
120800                                                                          
120900     IF WORK-KDSVAR-FEL                                                   
121000       MOVE 'FEL FRÅN WORKDAY' TO FELTEXT                                 
121100****** KAN BERO PÅ SKRÄP PÅ WDA211 - LEV-TIINLINL                         
121200       MOVE ERR-WRONG-LINES TO MED-IDMFSFEL                               
121300       PERFORM S02-FELMEDDELANDE                                          
121400     ELSE                                                                 
121500       IF WORK-KVWORKD > 14                                               
121600         MOVE ERR-UPDATE-NOT-OK TO MED-IDMFSFEL                           
121700         PERFORM S02-FELMEDDELANDE                                        
121800       END-IF                                                             
121900     END-IF                                                               
122000     .                                                                    
122100     EJECT                                                                
122200 H-UPPDATERA SECTION.                                                     
122300                                                                          
122400     PERFORM IMS-GHU-WDA211                                               
122500     IF SEGMENT-FINNS                                                     
122600       MOVE LEV-IDDC-RET        TO RET-WS-IDDC                            
122700                                   W-IDDC                                 
122800       MOVE MID-IDARTNR-UPD TO LEV-IDARTNR-DEL                            
122900       PERFORM IMS-REPL-WDA211                                            
123000***LOGGAR FÖR UPPFÖLJNINGSLISTA                                           
123100       MOVE LEV-IDARTNR-DEL     TO 100-IDARTNR                            
123200       MOVE LEV-IDDC            TO 100-IDDC-REC                           
123300       MOVE SPACE               TO 100-IDDC-SEND                          
123400*      MOVE LEV-KVLEVANM        TO 100-KVANTAL                            
123500       COMPUTE 100-KVANTAL = LEV-KVLEVANM * (-1)                          
123600       MOVE SPACE               TO 100-AVVIKELSETYP                       
123700                                   100-BEART                              
123800       MOVE ZERO                TO 100-ADLAGOMR                           
123900                                   100-ADGANG                             
124000                                   100-ADPLATS                            
124100       ADD +1                   TO WDR6-FIL-IDSEKVNR                      
124200       PERFORM S50-BEART-LAGERPLATS                                       
124300       PERFORM IMS-ISRT-WDR601                                            
124400************************************************                          
124500                                                                          
124600       IF RET-CDC-SE                                                      
124700         PERFORM HA-UPPDATERA-CDC                                         
124800       ELSE                                                               
124900         IF RET-NDC-US                                                    
125000           PERFORM HB-UPPDATERA-USA                                       
125100         ELSE                                                             
125200           IF RET-NDC-PACIFIC                                             
125300             PERFORM HC-UPPDATERA-JAP-AUS                                 
125400           END-IF                                                         
125500         END-IF                                                           
125600       END-IF                                                             
125700                                                                          
125800       MOVE LEV-IDDC          TO WS-IDDC                                  
125900                                 W-IDDC                                   
126000       IF LEV-FLDIRLEV = JA  OR  GOOD-DDC                                 
126100         CONTINUE                                                         
126200       ELSE                                                               
126300         IF CDC-SE                                                        
126400           IF WS-ART-KDERS-UTG = +0                                       
126500             IF WS-CLAG-KDVVKL NOT = +4 AND +5                            
126600               PERFORM HD-UPPDATERA-WDH1                                  
126700             END-IF                                                       
126800           ELSE                                                           
126900             PERFORM HD-UPPDATERA-WDH1                                    
127000           END-IF                                                         
127100         ELSE                                                             
127200           PERFORM IMS-GHU-WDK711                                         
127300           IF SEGMENT-SAKNAS                                              
127400             MOVE ALL '+'      TO WDK7-W005WDK7                           
127500             MOVE 'WDK711'     TO WDK7-IDSEGM                             
127600             MOVE W-IDARTNR    TO WDK7-IDARTNR-KFB                        
127700             MOVE W-IDDC       TO WDK7-IDDC-KFB                           
127800                                  WDK7-IDDC                               
127900                                                                          
128000             CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB WDK6-PCB          
128100                                               WDK7-PCB                   
128200           END-IF                                                         
128300           PERFORM HD-UPPDATERA-WDH1                                      
128400         END-IF                                                           
128500       END-IF                                                             
128600                                                                          
128700       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
128800       CALL WMEDKONV USING MED-WMEDAREA                                   
128900       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
129000       PERFORM MFS-FORM-ATTR                                              
129100       PERFORM MFS-RENSA-FAELT-IN                                         
129200     END-IF                                                               
129300     .                                                                    
129400     EJECT                                                                
129500 HA-UPPDATERA-CDC SECTION.                                                
129600                                                                          
129700*----ART.REGISTRET,CDC SKALL JUSTERAS UPP MED MOTTAGET ANTAL.             
129800*----FÖR SKR SKAPAS SKROTORDER.                                           
129900                                                                          
130000     PERFORM IMS-GHU-WDK611                                               
130100                                                                          
130200     ADD WS-ANTAL-REKL           TO CLAG-KVLS                             
130300     PERFORM IMS-REPL-WDK611                                              
130400                                                                          
130500     MOVE RET-WC-CDC-SE          TO LOGG-IDDC                             
130600     MOVE '+'                    TO LOGG-IDTECKEN-KVLS                    
130700     MOVE WS-ANTAL-REKL          TO LOGG-KVART-SALDO                      
130800     PERFORM S09-SKAPA-SALDOLOGG                                          
130900                                                                          
131000     IF LEV-KVRETINL-SKR > 0                                              
131100        PERFORM S03-SKAPA-SKROTORDER                                      
131200     END-IF                                                               
131300                                                                          
131400     MOVE LEV-KVRETINL           TO INLE-DIR-KVAVIS                       
131500     PERFORM HE-UPPDATERA-WDL2                                            
131600                                                                          
131700*----SÄNDANDE DC'S LAGER SKALL BOKAS NER MED REKLAMERAT ANTAL.            
131800*----DVS TOTALA ANTALET INL+SKR+KVA+ANT, MEN EJ OM KVLS BLIR < 0.         
131900*----DET FÅR EJ BLI MINUSSALDO.                                           
132000*----2006-03-10 KOD 22 = 27 MEN KOD 27 SKALL EJ BOKA SALDO.               
132100                                                                          
132200     MOVE LEV-IDDC               TO W-IDDC                                
132300                                                                          
132400     IF GOOD-DDC OR ( LEV-KDANMORS = '27' )                               
132500*----- BOKAR EJ NER SALDOT PÅ SÄNDANDE DC                                 
132600       MOVE 403                    TO EKH-KDEKHHT                         
132700       MOVE 408                    TO EKH-KDEKSHT                         
132800       MOVE 'J'                    TO EKH-FLLSBOK                         
132900       MOVE WS-ANTAL-REKL          TO EKH-KVANTAL                         
133000                                                                          
133100       PERFORM S40-EKONOMITRANS-WDR901                                    
133200                                                                          
133300       IF LEV-KVAVV-KVANT > 0 OR LEV-KVAVV-KVAL > 0                       
133400         MOVE 404                    TO EKH-KDEKHHT                       
133500         MOVE 401                    TO EKH-KDEKSHT                       
133600                                                                          
133700         PERFORM S41-EKONOMITR-SUM-WDR901                                 
133800         PERFORM S42-EKONOMITR-DET-WDR901                                 
133900       END-IF                                                             
134000     ELSE                                                                 
134100       PERFORM IMS-GHU-WDK711                                             
134200       IF SEGMENT-SAKNAS                                                  
134300*-----   BOKAR EJ NER SALDOT PÅ SÄNDANDE DC                               
134400         MOVE 403                    TO EKH-KDEKHHT                       
134500         MOVE 408                    TO EKH-KDEKSHT                       
134600         MOVE 'J'                    TO EKH-FLLSBOK                       
134700         MOVE WS-ANTAL-REKL          TO EKH-KVANTAL                       
134800                                                                          
134900         PERFORM S40-EKONOMITRANS-WDR901                                  
135000                                                                          
135100         IF LEV-KVAVV-KVANT > 0 OR LEV-KVAVV-KVAL > 0                     
135200           MOVE 404                    TO EKH-KDEKHHT                     
135300           MOVE 401                    TO EKH-KDEKSHT                     
135400                                                                          
135500           PERFORM S41-EKONOMITR-SUM-WDR901                               
135600           PERFORM S42-EKONOMITR-DET-WDR901                               
135700         END-IF                                                           
135800       ELSE                                                               
135900         COMPUTE SLAG-KVLS = SLAG-KVLS - WS-ANTAL-REKL                    
136000         END-COMPUTE                                                      
136100                                                                          
136200         IF SLAG-KVLS < 0                                                 
136300*--------  BOKAR EJ NER SALDOT PÅ SÄNDANDE DC                             
136400           MOVE 403                    TO EKH-KDEKHHT                     
136500           MOVE 408                    TO EKH-KDEKSHT                     
136600           MOVE 'J'                    TO EKH-FLLSBOK                     
136700           MOVE WS-ANTAL-REKL          TO EKH-KVANTAL                     
136800                                                                          
136900           PERFORM S40-EKONOMITRANS-WDR901                                
137000                                                                          
137100           IF LEV-KVAVV-KVANT > 0 OR LEV-KVAVV-KVAL > 0                   
137200             MOVE 404                    TO EKH-KDEKHHT                   
137300             MOVE 401                    TO EKH-KDEKSHT                   
137400                                                                          
137500             PERFORM S41-EKONOMITR-SUM-WDR901                             
137600             PERFORM S42-EKONOMITR-DET-WDR901                             
137700           END-IF                                                         
137800         ELSE                                                             
137900           PERFORM IMS-REPL-WDK711                                        
138000           MOVE '-'                    TO LOGG-IDTECKEN-KVLS              
138100           MOVE WS-ANTAL-REKL          TO LOGG-KVART-SALDO                
138200           PERFORM S10-SKAPA-SALDOLOGG                                    
138300           MOVE JA                     TO WS-FLSALDOJUST                  
138400                                                                          
138500           MOVE ZERO                   TO INLC-INL-ADLAGOMR               
138600                                          INLC-INL-ADGANG                 
138700                                          INLC-INL-ADPLATS                
138800                                          INLC-INL-KVART-SKROT            
138900           COMPUTE  INLC-INL-KVANTMOT  =  WS-ANTAL-REKL  *  (-1)          
139000           END-COMPUTE                                                    
139100           PERFORM  HG-UPPDATERA-WDL6                                     
139200                                                                          
139300           MOVE 502                    TO EKH-KDEKHHT                     
139400           MOVE 502                    TO EKH-KDEKSHT                     
139500           MOVE 'J'                    TO EKH-FLLSBOK                     
139600           MOVE WS-ANTAL-REKL          TO EKH-KVANTAL                     
139700                                                                          
139800           PERFORM S40-EKONOMITRANS-WDR901                                
139900                                                                          
140000           IF LEV-KVAVV-KVANT > 0 OR LEV-KVAVV-KVAL > 0                   
140100             MOVE 404                    TO EKH-KDEKHHT                   
140200             MOVE 401                    TO EKH-KDEKSHT                   
140300                                                                          
140400             PERFORM S41-EKONOMITR-SUM-WDR901                             
140500             PERFORM S42-EKONOMITR-DET-WDR901                             
140600           END-IF                                                         
140700                                                                          
140800         END-IF                                                           
140900       END-IF                                                             
141000     END-IF                                                               
141100     .                                                                    
141200     EJECT                                                                
141300 HB-UPPDATERA-USA SECTION.                                                
141400                                                                          
141500*----ART.REGISTRET,NDC SKALL JUSTERAS UPP MED MOTTAGET ANTAL.             
141600                                                                          
141700     PERFORM IMS-GHU-WDK711                                               
141800     IF SEGMENT-SAKNAS                                                    
141900       MOVE ALL '+'      TO WDK7-W005WDK7                                 
142000       MOVE 'WDK711'     TO WDK7-IDSEGM                                   
142100       MOVE W-IDARTNR    TO WDK7-IDARTNR-KFB                              
142200       MOVE W-IDDC       TO WDK7-IDDC-KFB                                 
142300                            WDK7-IDDC                                     
142400       MOVE LEV-KVRETINL TO WDK7-KVLS                                     
142500                                                                          
142600       CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB WDK6-PCB                
142700                                         WDK7-PCB                         
142800       MOVE WDK7-WDK711  TO SLAG-WDK711                                   
142900     ELSE                                                                 
143000       ADD LEV-KVRETINL            TO SLAG-KVLS                           
143100       PERFORM IMS-REPL-WDK711                                            
143200     END-IF                                                               
143300                                                                          
143400     MOVE '+'                      TO LOGG-IDTECKEN-KVLS                  
143500     MOVE LEV-KVRETINL             TO LOGG-KVART-SALDO                    
143600     PERFORM S10-SKAPA-SALDOLOGG                                          
143700                                                                          
143800     MOVE SLAG-ADLAGOMR            TO INLC-INL-ADLAGOMR                   
143900     MOVE SLAG-ADGANG              TO INLC-INL-ADGANG                     
144000     MOVE SLAG-ADPLATS             TO INLC-INL-ADPLATS                    
144100     MOVE LEV-KVRETINL             TO INLC-INL-KVANTMOT                   
144200     MOVE LEV-KVRETINL-SKR         TO INLC-INL-KVART-SKROT                
144300     PERFORM HG-UPPDATERA-WDL6                                            
144400                                                                          
144500*----SÄNDANDE DC'S LAGER SKALL BOKAS NER MED REKLAMERAT ANTAL.            
144600*----DVS TOTALA ANTALET INL+SKR+KVA+ANT, MEN EJ OM KVLS BLIR < 0.         
144700*----DET FÅR EJ BLI MINUSSALDO.                                           
144800                                                                          
144900     MOVE LEV-IDDC               TO W-IDDC                                
145000                                    WS-IDDC                               
145100                                                                          
145200     IF GOOD-DDC                                                          
145300       CONTINUE                                                           
145400     ELSE                                                                 
145500       IF CDC-SE                                                          
145600         PERFORM IMS-GHU-WDK611                                           
145700                                                                          
145800         COMPUTE CLAG-KVLS = CLAG-KVLS - WS-ANTAL-REKL                    
145900         END-COMPUTE                                                      
146000                                                                          
146100         IF CLAG-KVLS < 0                                                 
146200           CONTINUE                                                       
146300         ELSE                                                             
146400           PERFORM IMS-REPL-WDK611                                        
146500                                                                          
146600           MOVE WC-CDC-SE              TO LOGG-IDDC                       
146700           MOVE '-'                    TO LOGG-IDTECKEN-KVLS              
146800           MOVE WS-ANTAL-REKL          TO LOGG-KVART-SALDO                
146900           PERFORM S09-SKAPA-SALDOLOGG                                    
147000           MOVE JA                     TO WS-FLSALDOJUST                  
147100                                                                          
147200           COMPUTE  INLE-DIR-KVAVIS      = WS-ANTAL-REKL * (-1)           
147300           END-COMPUTE                                                    
147400           PERFORM HE-UPPDATERA-WDL2                                      
147500                                                                          
147600         END-IF                                                           
147700       ELSE                                                               
147800         PERFORM IMS-GHU-WDK711                                           
147900         IF SEGMENT-SAKNAS                                                
148000           CONTINUE                                                       
148100         ELSE                                                             
148200           COMPUTE SLAG-KVLS = SLAG-KVLS - WS-ANTAL-REKL                  
148300           END-COMPUTE                                                    
148400                                                                          
148500           IF SLAG-KVLS < 0                                               
148600             CONTINUE                                                     
148700           ELSE                                                           
148800             PERFORM IMS-REPL-WDK711                                      
148900             MOVE '-'                    TO LOGG-IDTECKEN-KVLS            
149000             MOVE WS-ANTAL-REKL          TO LOGG-KVART-SALDO              
149100             PERFORM S10-SKAPA-SALDOLOGG                                  
149200             MOVE JA                     TO WS-FLSALDOJUST                
149300                                                                          
149400             MOVE ZERO                   TO INLC-INL-ADLAGOMR             
149500                                            INLC-INL-ADGANG               
149600                                            INLC-INL-ADPLATS              
149700                                            INLC-INL-KVART-SKROT          
149800             COMPUTE  INLC-INL-KVANTMOT    = WS-ANTAL-REKL * (-1)         
149900             END-COMPUTE                                                  
150000             PERFORM HG-UPPDATERA-WDL6                                    
150100           END-IF                                                         
150200         END-IF                                                           
150300       END-IF                                                             
150400     END-IF                                                               
150500     .                                                                    
150600     EJECT                                                                
150700 HC-UPPDATERA-JAP-AUS SECTION.                                            
150800                                                                          
150900*----ART.REGISTRET,NDC SKALL JUSTERAS UPP MED MOTTAGET ANTAL.             
151000                                                                          
151100     PERFORM IMS-GHU-WDK711                                               
151200     IF SEGMENT-SAKNAS                                                    
151300       MOVE ALL '+'       TO WDK7-W005WDK7                                
151400       MOVE 'WDK711'      TO WDK7-IDSEGM                                  
151500       MOVE W-IDARTNR     TO WDK7-IDARTNR-KFB                             
151600       MOVE W-IDDC        TO WDK7-IDDC-KFB                                
151700                             WDK7-IDDC                                    
151800       MOVE WS-ANTAL-REKL TO WDK7-KVLS                                    
151900                                                                          
152000       CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB WDK6-PCB                
152100                                         WDK7-PCB                         
152200       MOVE WDK7-WDK711  TO SLAG-WDK711                                   
152300     ELSE                                                                 
152400       ADD WS-ANTAL-REKL         TO SLAG-KVLS                             
152500       PERFORM IMS-REPL-WDK711                                            
152600     END-IF                                                               
152700                                                                          
152800     MOVE '+'                    TO LOGG-IDTECKEN-KVLS                    
152900     MOVE WS-ANTAL-REKL          TO LOGG-KVART-SALDO                      
153000     PERFORM S10-SKAPA-SALDOLOGG                                          
153100                                                                          
153200     MOVE SLAG-ADLAGOMR            TO INLC-INL-ADLAGOMR                   
153300     MOVE SLAG-ADGANG              TO INLC-INL-ADGANG                     
153400     MOVE SLAG-ADPLATS             TO INLC-INL-ADPLATS                    
153500     MOVE LEV-KVRETINL             TO INLC-INL-KVANTMOT                   
153600     MOVE LEV-KVRETINL-SKR         TO INLC-INL-KVART-SKROT                
153700     PERFORM HG-UPPDATERA-WDL6                                            
153800                                                                          
153900     IF LEV-KVRETINL-SKR > 0                                              
154000        PERFORM S03-SKAPA-SKROTORDER                                      
154100     END-IF                                                               
154200                                                                          
154300*----CDC SKALL BOKAS NER MED REKLAMERAT ANTAL.MEN EJ OM KVLS < 0          
154400*----DET FÅR EJ BLI MINUSSALDO.JAP/AUS FÅR ENBART LEV FRÅN EGET DC        
154500*----OCH CDC.                                                             
154600*----2006-03-10 KOD 22 = 27 MEN KOD 27 SKALL EJ BOKA SALDO.               
154700                                                                          
154800                                                                          
154900     MOVE LEV-IDDC               TO W-IDDC                                
155000     IF GOOD-DDC OR ( LEV-KDANMORS = '27' )                               
155100*----- BOKAR EJ NER SALDOT PÅ SÄNDANDE DC                                 
155200       MOVE 403                    TO EKH-KDEKHHT                         
155300       MOVE 408                    TO EKH-KDEKSHT                         
155400       MOVE 'J'                    TO EKH-FLLSBOK                         
155500       MOVE WS-ANTAL-REKL          TO EKH-KVANTAL                         
155600                                                                          
155700       PERFORM S40-EKONOMITRANS-WDR901                                    
155800                                                                          
155900       IF LEV-KVAVV-KVANT > 0 OR LEV-KVAVV-KVAL > 0                       
156000         MOVE 404                    TO EKH-KDEKHHT                       
156100         MOVE 401                    TO EKH-KDEKSHT                       
156200                                                                          
156300         PERFORM S41-EKONOMITR-SUM-WDR901                                 
156400         PERFORM S42-EKONOMITR-DET-WDR901                                 
156500       END-IF                                                             
156600     ELSE                                                                 
156700       PERFORM IMS-GHU-WDK611                                             
156800                                                                          
156900       COMPUTE CLAG-KVLS = CLAG-KVLS - WS-ANTAL-REKL                      
157000       END-COMPUTE                                                        
157100                                                                          
157200       IF CLAG-KVLS < 0                                                   
157300*------- BOKAR EJ NER SALDOT PÅ SÄNDANDE DC                               
157400         MOVE 403                    TO EKH-KDEKHHT                       
157500         MOVE 408                    TO EKH-KDEKSHT                       
157600         MOVE 'J'                    TO EKH-FLLSBOK                       
157700         MOVE WS-ANTAL-REKL          TO EKH-KVANTAL                       
157800                                                                          
157900         PERFORM S40-EKONOMITRANS-WDR901                                  
158000                                                                          
158100         IF LEV-KVAVV-KVANT > 0 OR LEV-KVAVV-KVAL > 0                     
158200           MOVE 404                    TO EKH-KDEKHHT                     
158300           MOVE 401                    TO EKH-KDEKSHT                     
158400                                                                          
158500           PERFORM S41-EKONOMITR-SUM-WDR901                               
158600           PERFORM S42-EKONOMITR-DET-WDR901                               
158700         END-IF                                                           
158800       ELSE                                                               
158900         PERFORM IMS-REPL-WDK611                                          
159000                                                                          
159100         MOVE LEV-IDDC               TO WS-IDDC                           
159200                                                                          
159300         MOVE WC-CDC-SE              TO LOGG-IDDC                         
159400         MOVE '-'                    TO LOGG-IDTECKEN-KVLS                
159500         MOVE WS-ANTAL-REKL          TO LOGG-KVART-SALDO                  
159600         PERFORM S09-SKAPA-SALDOLOGG                                      
159700         MOVE JA                     TO WS-FLSALDOJUST                    
159800                                                                          
159900         COMPUTE  INLE-DIR-KVAVIS      = WS-ANTAL-REKL * (-1)             
160000         END-COMPUTE                                                      
160100         PERFORM HE-UPPDATERA-WDL2                                        
160200                                                                          
160300         MOVE 502                    TO EKH-KDEKHHT                       
160400         MOVE 502                    TO EKH-KDEKSHT                       
160500         MOVE 'J'                    TO EKH-FLLSBOK                       
160600         MOVE WS-ANTAL-REKL          TO EKH-KVANTAL                       
160700                                                                          
160800         PERFORM S40-EKONOMITRANS-WDR901                                  
160900                                                                          
161000         IF LEV-KVAVV-KVANT > 0 OR LEV-KVAVV-KVAL > 0                     
161100           MOVE 404                    TO EKH-KDEKHHT                     
161200           MOVE 401                    TO EKH-KDEKSHT                     
161300                                                                          
161400           PERFORM S41-EKONOMITR-SUM-WDR901                               
161500           PERFORM S42-EKONOMITR-DET-WDR901                               
161600         END-IF                                                           
161700                                                                          
161800       END-IF                                                             
161900     END-IF                                                               
162000     .                                                                    
162100     EJECT                                                                
162200 HD-UPPDATERA-WDH1 SECTION.                                               
162300                                                                          
162400*----INVENTERINGSKÖN SKALL UPPDATERAS MED KATEGORI 4, UTAN HÄNSYN         
162500*----TILL BELOPPSGRÄNSER.                                                 
162600                                                                          
162700     PERFORM IMS-GU-WDH101                                                
162800     IF SEGMENT-SAKNAS                                                    
162900       PERFORM HDA-SKAPA-NY-ROT                                           
163000       PERFORM HDB-SKAPA-KAT4-SEGMENT                                     
163100     ELSE                                                                 
163200**** SÖK OM DET FINNS NÅGON EJ BEHANDLAD KATEGORI      ***                
163300**** OM EJ, LÄGG UPP EN NY KATEGORI 4 (HDB-)           ***                
163400       MOVE LEV-IDDC                      TO IDDC-SEARCH-MIN              
163500                                             IDDC-SEARCH-MAX              
163600       MOVE NEJ                           TO INV-FINNS                    
163700       PERFORM IMS-GNP-WDH111                                             
163800       PERFORM UNTIL SEGMENT-SAKNAS                                       
163900         IF INV-FLINVBEH = NEJ                                            
164000           MOVE JA                        TO INV-FINNS                    
164100         END-IF                                                           
164200         PERFORM IMS-GNP-WDH111                                           
164300       END-PERFORM                                                        
164400                                                                          
164500       IF INV-FINNS = NEJ                                                 
164600**** ALLA KATEGORIER PÅ DENNA ART/DC HAR FLINVBEH = JA ***                
164700         PERFORM HDB-SKAPA-KAT4-SEGMENT                                   
164800       END-IF                                                             
164900     END-IF                                                               
165000     .                                                                    
165100     EJECT                                                                
165200 HDA-SKAPA-NY-ROT SECTION.                                                
165300                                                                          
165400     MOVE W-IDARTNR                    TO INV-ART-IDARTNR                 
165500                                                                          
165600*** SOFTWARE ARTIKLAR SKALL INTE INVENTERAS                               
165700     PERFORM IMS-GU-WDK601                                                
165800     IF ART-KDSORT = 'SW'                                                 
165900       CONTINUE                                                           
166000     ELSE                                                                 
166100       PERFORM IMS-ISRT-WDH101                                            
166200     END-IF                                                               
166300     .                                                                    
166400     SKIP3                                                                
166500 HDB-SKAPA-KAT4-SEGMENT SECTION.                                          
166600                                                                          
166700     PERFORM IMS-GU-WDK601                                                
166800     MOVE ART-IDFKNGRP                 TO INV-IDFKNGRP                    
166900     MOVE ART-KDPRODSL                 TO INV-KDPRODSL                    
167000                                                                          
167100     PERFORM IMS-GNP-WDK611                                               
167200     MOVE CLAG-KDPSLLOC                TO INV-KDPSLLOC                    
167300     MOVE CLAG-KDVVKL                  TO INV-KDVVKL                      
167400                                                                          
167500     MOVE LEV-IDDC                     TO WS-IDDC                         
167600     IF CDC-SE                                                            
167700       MOVE CLAG-ADLAGOMR              TO INV-ADLAGOMR                    
167800       MOVE CLAG-ADGANG                TO INV-ADGANG                      
167900       MOVE CLAG-ADPLATS               TO INV-ADPLATS                     
168000     ELSE                                                                 
168100       MOVE LEV-IDDC                   TO W-IDDC                          
168200       PERFORM IMS-GU-WDK711                                              
168300       IF SEGMENT-SAKNAS                                                  
168400         MOVE ZERO                     TO INV-ADLAGOMR                    
168500                                          INV-ADGANG                      
168600                                          INV-ADPLATS                     
168700       ELSE                                                               
168800         MOVE SLAG-ADLAGOMR            TO INV-ADLAGOMR                    
168900         MOVE SLAG-ADGANG              TO INV-ADGANG                      
169000         MOVE SLAG-ADPLATS             TO INV-ADPLATS                     
169100       END-IF                                                             
169200     END-IF                                                               
169300                                                                          
169400     MOVE NEJ                            TO INV-FLINVBEH                  
169500     MOVE NEJ                            TO INV-FLINVSKR                  
169600     MOVE NEJ                            TO INV-FLINV2B                   
169700     MOVE NEJ                            TO INV-FLINV2C                   
169800     MOVE NEJ                            TO INV-FLINV2D                   
169900     MOVE NEJ                            TO INV-FLINV3E                   
170000     MOVE NEJ                            TO INV-FLINV4N                   
170100     MOVE NEJ                            TO INV-FLINV4P                   
170200     MOVE NEJ                            TO INV-FLINV4R                   
170300     MOVE ZERO                           TO INV-KDINVKAT-OLD              
170400     MOVE NEJ                            TO INV-FLINV85                   
170500     MOVE SPACE                          TO INV-FILLER1                   
170600                                          INV-FILLER2                     
170700     MOVE LEV-IDDC                       TO INV-IDDC                      
170800     MOVE +2                             TO INV-KDINVPRIO                 
170900     MOVE +4                             TO INV-KDINVKAT                  
171000     MOVE +0                             TO INV-KVJUSTKV                  
171100                                                                          
171200*--- INV-K-  = FÄLT I WORKING-STORAGE                                     
171300     MOVE SPACE                           TO INV-TEINVANM                 
171400     MOVE SPACE                           TO WS-KOMMENTAR                 
171500     MOVE W-IDDISTR                       TO INV-K-IDDISTR                
171600     MOVE W-IDKUNDNR                      TO INV-K-IDKUNDNR               
171700     MOVE W-IDRAPPNR                      TO INV-K-IDRAPPNR               
171800     MOVE WS-ANTAL-REKL                   TO INV-K-KVANTAL                
171900     MOVE WS-FLSALDOJUST                  TO INV-K-FLSALDOJUST            
172000     MOVE WS-KOMMENTAR                    TO INV-TEINVANM                 
172100     MOVE ZERO                            TO INV-IDPRTOMG                 
172200                                           INV-IDLOPNR                    
172300                                           INV-KVAKS-OLD                  
172400                                           INV-KVEFRS-OLD                 
172500                                           INV-KVLS-OLD                   
172600                                           INV-DAREGDAT-PR1               
172700                                           INV-DAREGDAT-PR2               
172800                                           INV-DAREGDAT-PR3               
172900                                                                          
173000     MOVE WS-AAAAMMDD                    TO WS-TIAAAAMMDD                 
173100                                          WS-INV-AAAAMMDD                 
173200     MOVE WS-INV-DAREGDAT                TO INV-DAREGDAT-CRE              
173300                                          INV-DAREGDAT                    
173400***  COMPUTE INV-DAREGDAT-SORT =                                          
173500***   99999999 - WS-INV-AAAAMMDD                                          
173600     MOVE 99999999                       TO INV-DAREGDAT-SORT             
173700     MOVE 0                              TO WS-LOPNR                      
173800     MOVE WS-TIAAAAMMDDL                 TO INV-TISEGKEY                  
173900                                                                          
174000*** SOFTWARE ARTIKLAR SKALL INTE INVENTERAS                               
174100     IF ART-KDSORT = 'SW'                                                 
174200       CONTINUE                                                           
174300     ELSE                                                                 
174400       PERFORM IMS-ISRT-WDH111                                            
174500       IF SEGMENT-FINNS                                                   
174600*** OM SEGMENT-FINNS GICK INSERTEN BRA ***                                
174700*** INSERT PÅ WDH121 SEGMENTET ***                                        
174800         MOVE MSGI-IDUSER TO INVL-IDUSER                                  
174900         MOVE '0'         TO INVL-KDSEGKEY                                
175000         PERFORM IMS-INSERT-WDH121                                        
175100         MOVE SPACE       TO INVL-IDUSER                                  
175200         MOVE '1'         TO INVL-KDSEGKEY                                
175300         PERFORM IMS-INSERT-WDH121                                        
175400         MOVE SPACE       TO INVL-IDUSER                                  
175500         MOVE '2'         TO INVL-KDSEGKEY                                
175600         PERFORM IMS-INSERT-WDH121                                        
175700         MOVE SPACE       TO INVL-IDUSER                                  
175800         MOVE '3'         TO INVL-KDSEGKEY                                
175900         PERFORM IMS-INSERT-WDH121                                        
176000       END-IF                                                             
176100     END-IF                                                               
176200**** SLUT PÅ INSERT PÅ WDH121 SEGMENT                                     
176300     .                                                                    
176400     EJECT                                                                
176500 HE-UPPDATERA-WDL2 SECTION.                                               
176600                                                                          
176700     PERFORM IMS-GU-WDL201                                                
176800     IF  SEGMENT-SAKNAS                                                   
176900       MOVE W-IDARTNR   TO INLE-ART-IDARTNR                               
177000       PERFORM IMS-ISRT-WDL201                                            
177100     END-IF                                                               
177200                                                                          
177300*    -- SKAPA IDINLEV                                                     
177400     ACCEPT WS-TIAAMMDDTTMMSSTH-DATE FROM DATE                            
177500     ACCEPT WS-TIAAMMDDTTMMSSTH-TIME FROM TIME                            
177600     MOVE FUNCTION CURRENT-DATE (1:2) TO WS-TISEKEL                       
177700     COMPUTE W-DAINLEV          = 9999999999999999                        
177800                                - WS-TIAAAAMMDDTTMMSSTH                   
177900     END-COMPUTE                                                          
178000                                                                          
178100     MOVE W-DAINLEV TO INLE-INL-DAINLEV                                   
178200     PERFORM IMS-ISRT-WDL211                                              
178300     PERFORM UNTIL SEGMENT-FINNS                                          
178400        SUBTRACT 1 FROM W-DAINLEV                                         
178500        MOVE W-DAINLEV TO INLE-INL-DAINLEV                                
178600        PERFORM IMS-ISRT-WDL211                                           
178700     END-PERFORM                                                          
178800                                                                          
178900                                                                          
179000     MOVE 'R34'            TO INLE-DIR-IDPTYP                             
179100                                                                          
179200     MOVE DAT-TIAAVVD(3:3) TO W-VVD                                       
179300     MOVE ZERO             TO W-LLLL                                      
179400                              W-K                                         
179500     MOVE W-IDLOPNRM       TO INLE-DIR-IDLOPNRM                           
179600                                                                          
179700***  MOVE W-IDDISTR        TO INLE-DIR-IDLEVNR                            
179800     MOVE W-IDDISTR        TO W-IDLEVNR-PIC9                              
179900     MOVE ZERO TO TALLY                                                   
180000     INSPECT W-IDLEVNR-PIC9 TALLYING TALLY FOR LEADING ZEROES             
180100     IF TALLY = 5                                                         
180200         MOVE SPACE TO INLE-DIR-IDLEVNR                                   
180300     ELSE                                                                 
180400        MOVE W-IDLEVNR-PIC9(TALLY + 1:) TO INLE-DIR-IDLEVNR               
180500     END-IF                                                               
180600     MOVE W-IDRAPPNR       TO INLE-DIR-IDAVINR                            
180700     MOVE DAGENS-DATUM     TO INLE-DIR-TIAVSDAT                           
180800     MOVE WC-CDC-SE        TO INLE-DIR-IDDC                               
180900     MOVE +7               TO INLE-DIR-KDRT                               
181000     MOVE SPACE            TO INLE-DIR-IDKST                              
181100     MOVE SPACE            TO INLE-DIR-IDANALYS                           
181200     MOVE W-IDKUNDNR       TO INLE-DIR-IDKONTO                            
181300     MOVE W-IDDISTR        TO INLE-DIR-IDDISTR                            
181400     MOVE W-IDKUNDNR       TO INLE-DIR-IDKUNDNR                           
181500     MOVE ZERO             TO INLE-DIR-IDKUNDRF                           
181600                              INLE-DIR-IDPRODNR                           
181700                              INLE-DIR-IDFAKT                             
181800                                                                          
181900     PERFORM IMS-ISRT-WDL222                                              
182000     .                                                                    
182100     EJECT                                                                
182200 HG-UPPDATERA-WDL6 SECTION.                                               
182300                                                                          
182400     PERFORM IMS-GU-WDL601                                                
182500     IF SEGMENT-SAKNAS                                                    
182600       MOVE W-IDARTNR       TO INLC-ART-IDARTNR                           
182700       PERFORM IMS-ISRT-WDL601                                            
182800     END-IF                                                               
182900                                                                          
183000*    -- SKAPA IDINLEV                                                     
183100     ACCEPT WS-TIAAMMDDTTMMSSTH-DATE FROM DATE                            
183200     ACCEPT WS-TIAAMMDDTTMMSSTH-TIME FROM TIME                            
183300     MOVE FUNCTION CURRENT-DATE (1:2) TO WS-TISEKEL                       
183400     COMPUTE W-DAINLEV          = 9999999999999999                        
183500                                - WS-TIAAAAMMDDTTMMSSTH                   
183600     END-COMPUTE                                                          
183700                                                                          
183800     MOVE W-DAINLEV         TO INLC-INL-DAINLEV                           
183900     MOVE SPACE             TO INLC-INL-FLMAKUL                           
184000                               INLC-INL-FLSKAKOL                          
184100     MOVE 'N'               TO INLC-INL-FLPRIO                            
184200     MOVE W-IDDC            TO INLC-INL-IDDC                              
184300***  MOVE W-IDDISTR         TO INLC-INL-IDLEVNR                           
184400     MOVE W-IDDISTR        TO W-IDLEVNR-PIC9                              
184500     MOVE ZERO TO TALLY                                                   
184600     INSPECT W-IDLEVNR-PIC9 TALLYING TALLY FOR LEADING ZEROES             
184700     IF TALLY = 5                                                         
184800         MOVE SPACE TO INLC-INL-IDLEVNR                                   
184900     ELSE                                                                 
185000        MOVE W-IDLEVNR-PIC9(TALLY + 1:) TO INLC-INL-IDLEVNR               
185100     END-IF                                                               
185200     MOVE ZERO              TO INLC-INL-IDLOPNRM                          
185300     MOVE ZERO              TO INLC-INL-IDFAKT                            
185400     MOVE W-IDDISTR         TO INLC-INL-IDDISTR                           
185500     MOVE W-IDKUNDNR        TO INLC-INL-IDKUNDNR                          
185600     MOVE W-IDRAPPNR        TO INLC-INL-IDKUNDRF                          
185700     MOVE ZERO              TO INLC-INL-IDKOLLI                           
185800     MOVE 'R34'             TO INLC-INL-IDPTYP                            
185900     MOVE ZERO              TO INLC-INL-KDFRAKT                           
186000     MOVE SPACE             TO INLC-INL-KDKOLLI                           
186100                               INLC-INL-KDVALISO                          
186200                               INLC-INL-ADINLOMR                          
186300                               INLC-INL-IDANALYS                          
186400                               INLC-INL-IDUSER-003                        
186500                               INLC-INL-IDDC-LEV                          
186600     MOVE ZERO              TO INLC-INL-KVAVIS                            
186700     MOVE +7                TO INLC-INL-KDRT                              
186800     MOVE ZERO              TO INLC-INL-IDKONTO                           
186900     MOVE SPACE             TO INLC-INL-IDKST                             
187000     MOVE ZERO              TO INLC-INL-PRARTNTO                          
187100                               INLC-INL-PRKURS                            
187200                               INLC-INL-TIBERANK                          
187300                               INLC-INL-TIINLMTI                          
187400                               INLC-INL-TIINLITI                          
187500                               INLC-INL-KVRETUR                           
187600                               INLC-INL-KDAVVANT                          
187700                               INLC-INL-TIAVIDAT                          
187800                               INLC-INL-KVTULRET                          
187900     ACCEPT INLC-INL-TIINLMOT FROM DATE                                   
188000     ACCEPT INLC-INL-TIINLINL FROM DATE                                   
188100                                                                          
188200     PERFORM IMS-ISRT-WDL611                                              
188300                                                                          
188400     PERFORM UNTIL SEGMENT-FINNS                                          
188500       SUBTRACT 1 FROM W-DAINLEV                                          
188600       MOVE W-DAINLEV TO INLC-INL-DAINLEV                                 
188700       PERFORM IMS-ISRT-WDL611                                            
188800     END-PERFORM                                                          
188900     .                                                                    
189000     EJECT                                                                
189100 S01-INSERT-ALTMSG SECTION.                                               
189200                                                                          
189300     PERFORM IMS-CHANGE-ALTMSG                                            
189400     IF STATUS-OK                                                         
189500       PERFORM IMS-INSERT-ALTMSG                                          
189600     ELSE                                                                 
189700       IF SECURITY-FEL                                                    
189800         STRING 'NOT AUTHORIZED TO USE '                                  
189900                MID-IDTRANS-HOPP(INDX)                                    
190000                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
190100       ELSE                                                               
190200         STRING 'WRONG PICTURE '                                          
190300                MID-IDTRANS-HOPP(INDX)                                    
190400                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
190500       END-IF                                                             
190600       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O73901 + 4                      
190700       PERFORM MFS-ROER-EJ-FAELT-IN                                       
190800       PERFORM MFS-ROER-EJ-FAELT-UT                                       
190900       PERFORM IMS-INSERT-MSG                                             
191000     END-IF                                                               
191100     .                                                                    
191200     EJECT                                                                
191300 S02-FELMEDDELANDE SECTION.                                               
191400                                                                          
191500     IF MED-IDMFSFEL = SPACE                                              
191600       MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                          
191700     END-IF                                                               
191800     CALL WMEDKONV USING MED-WMEDAREA                                     
191900     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
192000     MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-UPD-ATTR                       
192100     PERFORM MFS-ROER-EJ-FAELT-UT                                         
192200     PERFORM MFS-ROER-EJ-FAELT-IN                                         
192300     MOVE NEJ TO INDATA-SW                                                
192400     .                                                                    
192500     EJECT                                                                
192600 S03-SKAPA-SKROTORDER SECTION.                                            
192700                                                                          
192800     IF  TRANS-OHUVUD-DAM-SKAPAD                                          
192900         CONTINUE                                                         
193000     ELSE                                                                 
193100         PERFORM S04-SKAPA-TRANS-ORDERHUVUD                               
193200                                                                          
193300         PERFORM S05-SKAPA-HUVUD-ORDERRADER                               
193400                                                                          
193500         MOVE 1           TO ORAD-IX                                      
193600     END-IF                                                               
193700                                                                          
193800     PERFORM S06-EDIT-TRANS-ORDERRADER                                    
193900                                                                          
194000     MOVE ORAD-KOM-AREA    TO P-TO-P2-DATA                                
194100     PERFORM S08-CALL-W006KOM                                             
194200     .                                                                    
194300     EJECT                                                                
194400 S04-SKAPA-TRANS-ORDERHUVUD SECTION.                                      
194500                                                                          
194600     MOVE JA              TO TRANS-OHUVUD-DAM-SKAPAD-SW                   
194700                                                                          
194800     MOVE SPACE           TO MSG-KOM-WMSGKOM                              
194900     MOVE +54             TO MSG-KOM-KVLL                                 
195000     MOVE LOW-VALUE       TO MSG-KOM-KDZ1                                 
195100     MOVE LOW-VALUE       TO MSG-KOM-KDZ2                                 
195200     MOVE SPACE           TO MSG-KOM-KDTRANS                              
195300     MOVE 'W4I25101'      TO MSG-KOM-IDCPYTXT                             
195400     MOVE 'RET-RET '      TO MSG-KOM-IDSNDNOD                             
195500     MOVE 'W4073900'      TO MSG-KOM-IDSNDJOB                             
195600                                                                          
195700     ACCEPT MSG-KOM-TIREGDAT  FROM DATE                                   
195800                                                                          
195900     ACCEPT MSG-KOM-TIKLOCK   FROM TIME                                   
196000                                                                          
196100     MOVE SPACE           TO MSG-KOM-IDMFSMED                             
196200                             MSG-KOM-KDSVAR                               
196300                                                                          
196400     COMPUTE P-TO-P2-LL   =  LNG-P-TO-P-PREFIX +                          
196500                             LENGTH OF OHUV-MID-W4I25101                  
196600                                                                          
196700     MOVE LOW-VALUE              TO P-TO-P2-Z1                            
196800     MOVE LOW-VALUE              TO P-TO-P2-Z2                            
196900     MOVE 'W4T251X'              TO P-TO-P2-TRANSKOD                      
197000     MOVE '4251'                 TO P-TO-P2-FROM-MID                      
197100     MOVE MFS-KDMFSFOR           TO P-TO-P2-KDMFSFOR                      
197200                                                                          
197300                                                                          
197400     MOVE SPACE                  TO OHUV-KOM-AREA                         
197500                                                                          
197600     MOVE 'W407'                 TO OHUV-MID-IDSYSTEM                     
197700                                                                          
197800     MOVE LEV-IDDC-RET           TO RET-WS-IDDC                           
197900     EVALUATE TRUE                                                        
198000       WHEN RET-NDC-JP                                                    
198100         MOVE '0090'                 TO OHUV-MID-IDDISTR                  
198200         MOVE '057538'               TO OHUV-MID-IDKUNDNR                 
198300       WHEN RET-NDC-AU                                                    
198400         MOVE '0090'                 TO OHUV-MID-IDDISTR                  
198500         MOVE '057539'               TO OHUV-MID-IDKUNDNR                 
198600       WHEN RET-CDC-SE                                                    
198700         MOVE '0070'                 TO OHUV-MID-IDDISTR                  
198800         MOVE '057561'               TO OHUV-MID-IDKUNDNR                 
198900     END-EVALUATE                                                         
199000                                                                          
199100     PERFORM S07-LAES-HOEGSTA-IDORDNR                                     
199200     MOVE W-IDORDNR-X           TO OHUV-MID-IDORDNR                       
199300     MOVE '1'                   TO OHUV-MID-KDORDKL                       
199400                                                                          
199500     MOVE SPACE           TO OHUV-MID-KDFRAKT                             
199600                             OHUV-MID-TIRFS                               
199700     MOVE SPACE           TO OHUV-MID-BEKUNDRF                            
199800     MOVE 'N'             TO OHUV-MID-KDFAKTYP                            
199900     MOVE NEJ             TO OHUV-MID-FLRESTN                             
200000     MOVE SPACE           TO OHUV-MID-KDTPOTYP                            
200100                             OHUV-MID-TITPO                               
200200                             OHUV-MID-BELAGINS                            
200300                             OHUV-MID-BEGMT                               
200400                             OHUV-MID-ADGMT-GATA                          
200500                             OHUV-MID-ADGMT-PADR                          
200600                             OHUV-MID-KDROPACK                            
200700                             OHUV-MID-BEVARREF                            
200800                             OHUV-MID-KDTULLVE                            
200900                             OHUV-MID-KDNOTES                             
201000*SAP EJ KONTO HÄR I DETTA FALLET, ENL. BOSSE H. 981026.EFTERSOM           
201100*GULL-BRITT EJ HAR SVARAT,IFALL HON VILL HA ETT EGET ANALYSNR FÖR         
201200*DESSA SKROTNINGAR, HAMNAR ALLT PÅ 'ALLMÄNNA SKROTNINGAR' MED             
201300*INTERNTABELL. DETTA GÄLLER FÖR DC11.                                     
201400     MOVE ZERO            TO OHUV-MID-IDKONTO                             
201500     MOVE SPACE           TO OHUV-MID-IDANALYS                            
201600                             OHUV-MID-IDKST                               
201700*** OVAN SKALL BELASTA RETURAVD. ENL. G-B STARMAN 980904, MEN SAP         
201800*** SKALL EJ HA KOSTN.STÄLLE ENL TUULA 981026.                            
201900     MOVE JA              TO OHUV-MID-FLAUTFAK                            
202000     MOVE JA              TO OHUV-MID-FLAUTPAC                            
202100     MOVE NEJ             TO OHUV-MID-FLEMBORD                            
202200     MOVE NEJ             TO OHUV-MID-FLOVRLEV                            
202300     MOVE '57'            TO OHUV-MID-IDFTG                               
202400     MOVE SPACE           TO OHUV-MID-IDKAMPRF                            
202500                             OHUV-MID-ADBET                               
202600                             OHUV-MID-BEBET                               
202700                             OHUV-MID-IDSKYLT                             
202800                             OHUV-MID-FLLSBOK                             
202900                             OHUV-MID-IDBILREG                            
203000                             OHUV-MID-IDVIN                               
203100                             OHUV-MID-IDCISNR                             
203200     MOVE LEV-IDDC-RET    TO OHUV-MID-IDDC                                
203300     MOVE NEJ             TO OHUV-MID-FLORDTIL                            
203400     MOVE ZERO            TO OHUV-MID-IDGROSS                             
203500                                                                          
203600     MOVE OHUV-KOM-AREA   TO P-TO-P2-DATA                                 
203700                                                                          
203800     PERFORM S08-CALL-W006KOM                                             
203900     .                                                                    
204000     EJECT                                                                
204100 S05-SKAPA-HUVUD-ORDERRADER SECTION.                                      
204200                                                                          
204300     COMPUTE P-TO-P2-LL = LNG-P-TO-P-PREFIX +                             
204400                          LENGTH OF ORAD-MID-W4I25201                     
204500                                                                          
204600     MOVE LOW-VALUE        TO P-TO-P2-Z1                                  
204700     MOVE LOW-VALUE        TO P-TO-P2-Z2                                  
204800     MOVE 'W4T252X'        TO P-TO-P2-TRANSKOD                            
204900     MOVE '4252'           TO P-TO-P2-FROM-MID                            
205000     MOVE MFS-KDMFSFOR     TO P-TO-P2-KDMFSFOR                            
205100                                                                          
205200     MOVE SPACE            TO ORAD-KOM-AREA                               
205300                                                                          
205400     MOVE 'W407'           TO ORAD-MID-IDSYSTEM                           
205500                                                                          
205600     MOVE LEV-IDDC-RET     TO RET-WS-IDDC                                 
205700     EVALUATE TRUE                                                        
205800       WHEN RET-NDC-JP                                                    
205900         MOVE '0090'       TO ORAD-MID-IDDISTR                            
206000         MOVE '057538'     TO ORAD-MID-IDKUNDNR                           
206100       WHEN RET-NDC-AU                                                    
206200         MOVE '0090'       TO ORAD-MID-IDDISTR                            
206300         MOVE '057539'     TO ORAD-MID-IDKUNDNR                           
206400       WHEN RET-CDC-SE                                                    
206500         MOVE '0070'       TO ORAD-MID-IDDISTR                            
206600         MOVE '057561'     TO ORAD-MID-IDKUNDNR                           
206700     END-EVALUATE                                                         
206800                                                                          
206900     MOVE W-IDORDNR-X      TO ORAD-MID-IDORDNR                            
207000     MOVE SPACE            TO ORAD-MID-BEVOLREF                           
207100     MOVE SPACE            TO ORAD-MID-IDKUNDRF-RO                        
207200     MOVE 'J'              TO ORAD-MID-FLSLUT                             
207300                                                                          
207400     .                                                                    
207500     EJECT                                                                
207600 S06-EDIT-TRANS-ORDERRADER SECTION.                                       
207700                                                                          
207800     MOVE W-IDARTNR       TO ORAD-MID-IDARTNR      (ORAD-IX)              
207900                             REK-IDARTNR                                  
208000     MOVE 9               TO REK-LNGD                                     
208100     MOVE 0               TO REK-REKSIFFR                                 
208200                                                                          
208300     CALL W009KSIF        USING REK-IDARTNR                               
208400                                REK-LNGD                                  
208500                                REK-REKSIFFR                              
208600                                                                          
208700     MOVE REK-REKSIFFR     TO ORAD-MID-REKSIFFR  (ORAD-IX)                
208800     MOVE LEV-KVRETINL-SKR TO W-KVSKROT-6                                 
208900     MOVE W-KVSKROT-6-X    TO ORAD-MID-KVBEART   (ORAD-IX)                
209000     MOVE SPACE            TO ORAD-MID-PRARTNTO  (ORAD-IX)                
209100                              ORAD-MID-TITPO     (ORAD-IX)                
209200                              ORAD-MID-FLRESTN   (ORAD-IX)                
209300                              ORAD-MID-KDKVBRYT  (ORAD-IX)                
209400                              ORAD-MID-FLINVEST  (ORAD-IX)                
209500     MOVE ZERO             TO ORAD-MID-KDVRINFO  (ORAD-IX)                
209600**** MAN FLYTTAR EJ KONTO PÅ RADNIVÅ TILL SAP.                            
209700     MOVE ZERO             TO ORAD-MID-IDKONTO   (ORAD-IX)                
209800     MOVE SPACE            TO ORAD-MID-BERADREF  (ORAD-IX)                
209900                              ORAD-MID-IDBIL     (ORAD-IX)                
210000                              ORAD-MID-IDKST     (ORAD-IX)                
210100     MOVE 2                TO ORAD-MID-KDDSP     (ORAD-IX)                
210200     MOVE NEJ              TO ORAD-MID-FLSLATT   (ORAD-IX)                
210300     MOVE SPACE         TO ORAD-MID-PRARTNTO-LOC (ORAD-IX)                
210400     MOVE SPACE         TO ORAD-MID-PRARTBTO-LOC (ORAD-IX)                
210500     MOVE SPACE         TO     ORAD-MID-KDVALISO (ORAD-IX)                
210600     MOVE SPACE         TO     ORAD-MID-KDVAT    (ORAD-IX)                
210700     MOVE 0             TO     ORAD-MID-RERAB    (ORAD-IX)                
210800     MOVE SPACE         TO     ORAD-MID-KDRAB    (ORAD-IX)                
210900     MOVE SPACE         TO ORAD-MID-BEART-VIPS   (ORAD-IX)                
211000     MOVE ZERO          TO ORAD-MID-ADLAGOMR-CD  (ORAD-IX)                
211100                           ORAD-MID-ADGANG-CD    (ORAD-IX)                
211200                           ORAD-MID-ADPLATS-CD   (ORAD-IX)                
211300     .                                                                    
211400     EJECT                                                                
211500 S07-LAES-HOEGSTA-IDORDNR  SECTION.                                       
211600                                                                          
211700     PERFORM IMS-GHU-WL411111                                             
211800     MOVE 4112-IDORDNR7 TO W-IDORDNR-X                                    
211900     IF W-IDORDNR-VV = DAT-TIVV                                           
212000       ADD  +1           TO W-IDORDNR-LLL                                 
212100     ELSE                                                                 
212200       MOVE DAT-TIVV     TO W-IDORDNR-VV                                  
212300       MOVE +1           TO W-IDORDNR-LLL                                 
212400     END-IF                                                               
212500     MOVE W-IDORDNR-X  TO 4112-IDORDNR7                                   
212600     PERFORM IMS-REPL-WL4111                                              
212700     .                                                                    
212800     EJECT                                                                
212900 S08-CALL-W006KOM        SECTION.                                         
213000                                                                          
213100     CALL W006KOM         USING MSG-PCB                                   
213200                                DISP-PCB                                  
213300                                KOMA-PCB                                  
213400                                MSG-KOM-WMSGKOM                           
213500                                P-TO-P-AREA2                              
213600                                                                          
213700     .                                                                    
213800     EJECT                                                                
213900 S09-SKAPA-SALDOLOGG SECTION.                                             
214000                                                                          
214100     MOVE W-IDARTNR                TO LOGG-IDARTNR                        
214200                                                                          
214300     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-AAAAMMDD                      
214400     COMPUTE LOGG-DAREGDAT-9KOMPL  = 99999999                             
214500                                   - WS-AAAAMMDD                          
214600     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-TTMMSSTH                      
214700     COMPUTE LOGG-TIKLOCK-9KOMPL   = 999999999                            
214800                                   - WS-TTMMSSTH                          
214900     MOVE 9                        TO LOGG-IDSEKVNR                       
215000     MOVE 'DISC'                   TO LOGG-IDHUVTYP                       
215100     MOVE 'RET'                    TO LOGG-IDSUBTYP                       
215200     MOVE 'W4073900'               TO LOGG-IDPGM                          
215300     MOVE MFS-IDTRANS              TO LOGG-IDTRANS                        
215400     MOVE MSG-SIGNON-USERID        TO LOGG-IDUSER                         
215500     MOVE SPACE                    TO LOGG-REF                            
215600     MOVE W-IDDISTR                TO LOGG-IDDISTR                        
215700     MOVE W-IDKUNDNR               TO LOGG-IDKUNDNR                       
215800     MOVE W-IDRAPPNR               TO LOGG-IDRAPPNR                       
215900     MOVE ' '                      TO LOGG-IDTECKEN-KVAKS-PAV             
216000     MOVE ' '                      TO LOGG-IDTECKEN-KVEFRS                
216100                                                                          
216200     COMPUTE LOGG-KVAKS = CLAG-KVAKS-CDC                                  
216300                        + CLAG-KVAKS-T                                    
216400                                                                          
216500     MOVE CLAG-KVAKS-PAV           TO LOGG-KVAKS-PAV                      
216600     MOVE CLAG-KVEFRS              TO LOGG-KVEFRS                         
216700     MOVE CLAG-KVLS                TO LOGG-KVLS                           
216800     MOVE 000000                   TO LOGG-DAREGDAT-LADD                  
216900                                                                          
217000     PERFORM IMS-ISRT-WDL901                                              
217100     IF SEGMENT-FINNS-REDAN                                               
217200       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
217300         SUBTRACT 1       FROM LOGG-IDSEKVNR                              
217400         PERFORM IMS-ISRT-WDL901                                          
217500       END-PERFORM                                                        
217600     END-IF                                                               
217700     .                                                                    
217800     EJECT                                                                
217900 S10-SKAPA-SALDOLOGG SECTION.                                             
218000                                                                          
218100     MOVE W-IDARTNR                TO LOGG-IDARTNR                        
218200                                                                          
218300     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-AAAAMMDD                      
218400     COMPUTE LOGG-DAREGDAT-9KOMPL  = 99999999                             
218500                                   - WS-AAAAMMDD                          
218600     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-TTMMSSTH                      
218700     COMPUTE LOGG-TIKLOCK-9KOMPL   = 999999999                            
218800                                   - WS-TTMMSSTH                          
218900     MOVE 9                        TO LOGG-IDSEKVNR                       
219000     MOVE SLAG-IDDC                TO LOGG-IDDC                           
219100     MOVE 'DISC'                   TO LOGG-IDHUVTYP                       
219200     MOVE 'RET'                    TO LOGG-IDSUBTYP                       
219300     MOVE 'W4073900'               TO LOGG-IDPGM                          
219400     MOVE MFS-IDTRANS              TO LOGG-IDTRANS                        
219500     MOVE MSG-SIGNON-USERID        TO LOGG-IDUSER                         
219600     MOVE SPACE                    TO LOGG-REF                            
219700     MOVE W-IDDISTR                TO LOGG-IDDISTR                        
219800     MOVE W-IDKUNDNR               TO LOGG-IDKUNDNR                       
219900     MOVE W-IDRAPPNR               TO LOGG-IDRAPPNR                       
220000     MOVE ' '                      TO LOGG-IDTECKEN-KVAKS-PAV             
220100     MOVE ' '                      TO LOGG-IDTECKEN-KVEFRS                
220200     MOVE SLAG-KVAKS-SDC           TO LOGG-KVAKS                          
220300     MOVE SLAG-KVAKS-PAV           TO LOGG-KVAKS-PAV                      
220400     MOVE SLAG-KVEFRS              TO LOGG-KVEFRS                         
220500     MOVE SLAG-KVLS                TO LOGG-KVLS                           
220600     MOVE 000000                   TO LOGG-DAREGDAT-LADD                  
220700                                                                          
220800     PERFORM IMS-ISRT-WDL901                                              
220900     IF SEGMENT-FINNS-REDAN                                               
221000       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
221100         SUBTRACT 1       FROM LOGG-IDSEKVNR                              
221200         PERFORM IMS-ISRT-WDL901                                          
221300       END-PERFORM                                                        
221400     END-IF                                                               
221500     .                                                                    
221600     EJECT                                                                
221700 S40-EKONOMITRANS-WDR901 SECTION.                                         
221800                                                                          
221900     MOVE 'W4073900'             TO FIL-IDPGM                             
222000     MOVE FUNCTION CURRENT-DATE(1:8)  TO FIL-DAREGDAT                     
222100                                         EKH-DAVERDAT                     
222200     ACCEPT FIL-TIKLOCK FROM TIME                                         
222300                                                                          
222400     MOVE 1                      TO FIL-IDSEKVNR                          
222500     MOVE 'W510EKHA'             TO FIL-IDCPYTXT                          
222600     MOVE MSG-SIGNON-USERID      TO FIL-IDUSER                            
222700     MOVE 'DET'                  TO EKH-KDEKNIVA                          
222800     MOVE W-IDDISTR              TO EKH-IDDISTR                           
222900     MOVE W-IDKUNDNR             TO EKH-IDKUNDNR                          
223000                                                                          
223100     MOVE 'VO'                   TO CIA-IDARTPRE-IN                       
223200     MOVE W-IDRAPPNR             TO CIA-IDARTBET-IN                       
223300     CALL W009CIA USING             CIA-W009CIA                           
223400     MOVE CIA-IDARTBET-UT        TO EKH-IDVERGL                           
223500                                                                          
223600     MOVE WS-ART-KDPRODSL        TO EKH-KDPRODSL                          
223700     MOVE 0                      TO EKH-KDPSLLOC                          
223800     MOVE W-IDARTNR              TO EKH-IDARTNR                           
223900     MOVE LEV-IDDC               TO EKH-IDDC-SEND                         
224000     MOVE LEV-IDDC-RET           TO EKH-IDDC-REC                          
224100     MOVE W-IDARTNR              TO EKH-IDARTNR                           
224200     MOVE 'SEK'                  TO EKH-KDVALISO                          
224300     MOVE 1.00                   TO EKH-PRKURS                            
224400     MOVE 0                      TO EKH-PRARTNTO                          
224500     MOVE 0                      TO EKH-PRARTSJK                          
224600     MOVE 0                      TO EKH-PRHEMTAG                          
224700     MOVE WS-CLAG-PRARTSTD       TO EKH-PRARTSTD                          
224800     MOVE 0                      TO EKH-PRLANDCO                          
224900     MOVE 0                      TO EKH-PRINK                             
225000     MOVE 0                      TO EKH-PRDIRLON                          
225100     MOVE 0                      TO EKH-PRDMTRL                           
225200     MOVE 0                      TO EKH-PROVRPAL                          
225300     MOVE 0                      TO EKH-SUBEL                             
225400                                                                          
225500     MOVE LEV-KDANMORS           TO EKH-KDANMORS                          
225600     MOVE LEV-IDANALYS           TO EKH-IDANALYS                          
225700     MOVE LEV-IDKONTO            TO EKH-IDKONTO                           
225800     MOVE LEV-IDKST              TO EKH-IDKST                             
225900     MOVE ZERO                   TO EKH-BEVAT                             
226000                                    EKH-KDFRAKT                           
226100                                    EKH-SUVAT                             
226200     MOVE ZERO                   TO EKH-DAAVIDAT                          
226300                                    EKH-IDAVINR                           
226400                                    EKH-KDAVVTYP                          
226500                                    EKH-KDRT                              
226600                                    EKH-KVANTMOT                          
226700                                    EKH-KVAVIS                            
226800                                                                          
226900     MOVE MFS-IDTRANS            TO EKH-IDTRANS                           
227000     MOVE WS-ART-KDSORT          TO EKH-KDSORT                            
227100     MOVE SPACE                  TO EKH-KDTRADP                           
227200     IF W-IDRT-KEY = 'ET'                                                 
227300       MOVE JA             TO EKH-FLDCET                                  
227400     ELSE                                                                 
227500       MOVE NEJ            TO EKH-FLDCET                                  
227600     END-IF                                                               
227700     MOVE SPACE                  TO EKH-FLOVRLEV                          
227800                                    EKH-IDLEVNR                           
227900                                    EKH-IDKUNDRF                          
228000                                    EKH-IDFAKT-EXP                        
228100                                                                          
228200     PERFORM IMS-ISRT-WDR901                                              
228300     PERFORM UNTIL SEGMENT-FINNS                                          
228400       ADD +1  TO FIL-IDSEKVNR                                            
228500       PERFORM IMS-ISRT-WDR901                                            
228600     END-PERFORM                                                          
228700     .                                                                    
228800     EJECT                                                                
228900 S41-EKONOMITR-SUM-WDR901 SECTION.                                        
229000                                                                          
229100     MOVE 'W4073900'             TO FIL-IDPGM                             
229200     MOVE FUNCTION CURRENT-DATE(1:8)  TO FIL-DAREGDAT                     
229300                                         EKH-DAVERDAT                     
229400     ACCEPT FIL-TIKLOCK FROM TIME                                         
229500                                                                          
229600     MOVE 1                      TO FIL-IDSEKVNR                          
229700     MOVE 'W510EKHA'             TO FIL-IDCPYTXT                          
229800     MOVE MSG-SIGNON-USERID      TO FIL-IDUSER                            
229900     MOVE 'SUM'                  TO EKH-KDEKNIVA                          
230000     MOVE W-IDDISTR              TO EKH-IDDISTR                           
230100     MOVE W-IDKUNDNR             TO EKH-IDKUNDNR                          
230200                                                                          
230300     MOVE 'VO'                   TO CIA-IDARTPRE-IN                       
230400     MOVE W-IDRAPPNR             TO CIA-IDARTBET-IN                       
230500     CALL W009CIA USING             CIA-W009CIA                           
230600     MOVE CIA-IDARTBET-UT        TO EKH-IDVERGL                           
230700                                                                          
230800     MOVE 0                      TO EKH-KDPRODSL                          
230900     MOVE 0                      TO EKH-KDPSLLOC                          
231000     MOVE LEV-IDDC               TO EKH-IDDC-SEND                         
231100     MOVE LEV-IDDC-RET           TO EKH-IDDC-REC                          
231200     MOVE 0                      TO EKH-IDARTNR                           
231300     MOVE ' '                    TO EKH-FLLSBOK                           
231400                                                                          
231500     MOVE 'SEK'                  TO EKH-KDVALISO                          
231600     MOVE 1.00                   TO EKH-PRKURS                            
231700     MOVE 0                      TO EKH-PRARTNTO                          
231800     MOVE 0                      TO EKH-PRARTSJK                          
231900     MOVE 0                      TO EKH-PRHEMTAG                          
232000     MOVE 0                      TO EKH-PRARTSTD                          
232100     MOVE 0                      TO EKH-PRLANDCO                          
232200     MOVE 0                      TO EKH-PRINK                             
232300     MOVE 0                      TO EKH-PRDIRLON                          
232400     MOVE 0                      TO EKH-PRDMTRL                           
232500     MOVE 0                      TO EKH-PROVRPAL                          
232600                                                                          
232700     COMPUTE EKH-SUBEL = EKH-KVANTAL * EKH-PRARTSTD                       
232800                                                                          
232900     MOVE 0                      TO EKH-KVANTAL                           
233000     MOVE 0                      TO EKH-KDANMORS                          
233100     MOVE SPACE                  TO EKH-IDANALYS                          
233200     MOVE 0                      TO EKH-IDKONTO                           
233300     MOVE SPACE                  TO EKH-IDKST                             
233400     MOVE ZERO                   TO EKH-BEVAT                             
233500                                    EKH-KDFRAKT                           
233600                                    EKH-SUVAT                             
233700     MOVE ZERO                   TO EKH-DAAVIDAT                          
233800                                    EKH-IDAVINR                           
233900                                    EKH-KDAVVTYP                          
234000                                    EKH-KDRT                              
234100                                    EKH-KVANTMOT                          
234200                                    EKH-KVAVIS                            
234300                                                                          
234400     MOVE MFS-IDTRANS            TO EKH-IDTRANS                           
234500     MOVE SPACE                  TO EKH-KDSORT                            
234600     MOVE SPACE                  TO EKH-KDTRADP                           
234700     IF W-IDRT-KEY = 'ET'                                                 
234800       MOVE JA             TO EKH-FLDCET                                  
234900     ELSE                                                                 
235000       MOVE NEJ            TO EKH-FLDCET                                  
235100     END-IF                                                               
235200     MOVE SPACE                  TO EKH-FLOVRLEV                          
235300                                    EKH-IDLEVNR                           
235400                                    EKH-IDKUNDRF                          
235500                                    EKH-IDFAKT-EXP                        
235600                                                                          
235700     PERFORM IMS-ISRT-WDR901                                              
235800     PERFORM UNTIL SEGMENT-FINNS                                          
235900       ADD +1  TO FIL-IDSEKVNR                                            
236000       PERFORM IMS-ISRT-WDR901                                            
236100     END-PERFORM                                                          
236200     .                                                                    
236300     EJECT                                                                
236400 S42-EKONOMITR-DET-WDR901 SECTION.                                        
236500                                                                          
236600     MOVE 'W4073900'             TO FIL-IDPGM                             
236700     MOVE FUNCTION CURRENT-DATE(1:8)  TO FIL-DAREGDAT                     
236800                                         EKH-DAVERDAT                     
236900     ACCEPT FIL-TIKLOCK FROM TIME                                         
237000                                                                          
237100     MOVE 1                      TO FIL-IDSEKVNR                          
237200     MOVE 'W510EKHA'             TO FIL-IDCPYTXT                          
237300     MOVE MSG-SIGNON-USERID      TO FIL-IDUSER                            
237400     MOVE 'DET'                  TO EKH-KDEKNIVA                          
237500     MOVE W-IDDISTR              TO EKH-IDDISTR                           
237600     MOVE W-IDKUNDNR             TO EKH-IDKUNDNR                          
237700                                                                          
237800     MOVE 'VO'                   TO CIA-IDARTPRE-IN                       
237900     MOVE W-IDRAPPNR             TO CIA-IDARTBET-IN                       
238000     CALL W009CIA USING             CIA-W009CIA                           
238100     MOVE CIA-IDARTBET-UT        TO EKH-IDVERGL                           
238200                                                                          
238300     MOVE WS-ART-KDPRODSL        TO EKH-KDPRODSL                          
238400     MOVE 0                      TO EKH-KDPSLLOC                          
238500     MOVE LEV-IDDC               TO EKH-IDDC-SEND                         
238600     MOVE LEV-IDDC-RET           TO EKH-IDDC-REC                          
238700     MOVE W-IDARTNR              TO EKH-IDARTNR                           
238800     MOVE 'J'                    TO EKH-FLLSBOK                           
238900                                                                          
239000     MOVE 'SEK'                  TO EKH-KDVALISO                          
239100     MOVE 1.00                   TO EKH-PRKURS                            
239200     MOVE 0                      TO EKH-PRARTNTO                          
239300     MOVE 0                      TO EKH-PRARTSJK                          
239400     MOVE 0                      TO EKH-PRHEMTAG                          
239500     MOVE WS-CLAG-PRARTSTD       TO EKH-PRARTSTD                          
239600     MOVE 0                      TO EKH-PRLANDCO                          
239700     MOVE 0                      TO EKH-PRINK                             
239800     MOVE 0                      TO EKH-PRDIRLON                          
239900     MOVE 0                      TO EKH-PRDMTRL                           
240000     MOVE 0                      TO EKH-PROVRPAL                          
240100                                                                          
240200     COMPUTE EKH-SUBEL = EKH-KVANTAL * EKH-PRARTSTD                       
240300                                                                          
240400     COMPUTE EKH-KVANTAL = LEV-KVAVV-KVAL +                               
240500                                 LEV-KVAVV-KVANT                          
240600                                                                          
240700     MOVE LEV-KDANMORS           TO EKH-KDANMORS                          
240800     MOVE SPACE                  TO EKH-IDANALYS                          
240900     MOVE ZERO                   TO EKH-IDKONTO                           
241000     MOVE SPACE                  TO EKH-IDKST                             
241100     MOVE ZERO                   TO EKH-BEVAT                             
241200                                    EKH-KDFRAKT                           
241300                                    EKH-SUVAT                             
241400     MOVE ZERO                   TO EKH-DAAVIDAT                          
241500                                    EKH-IDAVINR                           
241600                                    EKH-KDAVVTYP                          
241700                                    EKH-KDRT                              
241800                                    EKH-KVANTMOT                          
241900                                    EKH-KVAVIS                            
242000                                                                          
242100     MOVE MFS-IDTRANS            TO EKH-IDTRANS                           
242200     MOVE WS-ART-KDSORT          TO EKH-KDSORT                            
242300     MOVE SPACE                  TO EKH-KDTRADP                           
242400     IF W-IDRT-KEY = 'ET'                                                 
242500       MOVE JA             TO EKH-FLDCET                                  
242600     ELSE                                                                 
242700       MOVE NEJ            TO EKH-FLDCET                                  
242800     END-IF                                                               
242900     MOVE SPACE                  TO EKH-FLOVRLEV                          
243000                                    EKH-IDLEVNR                           
243100                                    EKH-IDKUNDRF                          
243200                                    EKH-IDFAKT-EXP                        
243300                                                                          
243400     PERFORM IMS-ISRT-WDR901                                              
243500     PERFORM UNTIL SEGMENT-FINNS                                          
243600       ADD +1  TO FIL-IDSEKVNR                                            
243700       PERFORM IMS-ISRT-WDR901                                            
243800     END-PERFORM                                                          
243900     .                                                                    
244000     EJECT                                                                
244100                                                                          
244200 S50-BEART-LAGERPLATS SECTION.                                            
244300                                                                          
244400     PERFORM IMS-GU-BENA01-BSEQ                                           
244500     IF SEGMENT-FINNS                                                     
244600       MOVE 'GB' TO W-IDSKYLT                                             
244700       PERFORM IMS-GNP-BENA11                                             
244800       IF SEGMENT-FINNS                                                   
244900         MOVE TEXT-BEART        TO 100-BEART                              
245000       ELSE                                                               
245100         MOVE SPACE             TO 100-BEART                              
245200       END-IF                                                             
245300     END-IF                                                               
245400*******LAGERPLATS                                                         
245500     MOVE LEV-IDDC                     TO W-IDDC                          
245600     PERFORM IMS-GU-WDK711                                                
245700     IF SEGMENT-SAKNAS                                                    
245800       MOVE ZERO                       TO 100-ADLAGOMR                    
245900                                          100-ADGANG                      
246000                                          100-ADPLATS                     
246100     ELSE                                                                 
246200       MOVE SLAG-ADLAGOMR              TO 100-ADLAGOMR                    
246300       MOVE SLAG-ADGANG                TO 100-ADGANG                      
246400       MOVE SLAG-ADPLATS               TO 100-ADPLATS                     
246500     END-IF                                                               
246600                                                                          
246700     .                                                                    
246800     EJECT                                                                
246900 MFS-RENSA-FAELT-UT SECTION.                                              
247000                                                                          
247100*    --- ALLA UTDATA-FÄLT                                                 
247200*    --- INKL. BLÄDDRINGSNYCKLAR                                          
247300                                                                          
247400     MOVE MFS-RENSA-FAELT TO MOD-TIRETILL                                 
247500                             MOD-TIRETANK                                 
247600                             MOD-KDLEVANM                                 
247700                             MOD-IDANSTNR-RET                             
247800                                                                          
247900     MOVE +1 TO INDX                                                      
248000     PERFORM UNTIL INDX > MAX-INDX                                        
248100       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR (INDX)                         
248200                               MOD-IDRADNR  (INDX)                        
248300                               MOD-IDDC     (INDX)                        
248400                               MOD-IDORDNR5 (INDX)                        
248500                               MOD-KVLEVANM (INDX)                        
248600                               MOD-TIINLINL (INDX)                        
248700                               MOD-KVRETINL (INDX)                        
248800                               MOD-KVRETINL-SKR (INDX)                    
248900                               MOD-KVAVV-KVANT  (INDX)                    
249000                               MOD-KVAVV-KVAL   (INDX)                    
249100                               MOD-FLTEXT   (INDX)                        
249200                               MOD-IDARTNR-DEL (INDX)                     
249300       ADD +1 TO INDX                                                     
249400     END-PERFORM                                                          
249500     .                                                                    
249600     SKIP3                                                                
249700 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
249800                                                                          
249900*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
250000     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR  (INDX)                          
250100                             MOD-IDRADNR  (INDX)                          
250200                             MOD-IDDC     (INDX)                          
250300                             MOD-IDORDNR5 (INDX)                          
250400                             MOD-KVLEVANM (INDX)                          
250500                             MOD-TIINLINL (INDX)                          
250600                             MOD-KVRETINL (INDX)                          
250700                             MOD-KVRETINL-SKR (INDX)                      
250800                             MOD-KVAVV-KVANT  (INDX)                      
250900                             MOD-KVAVV-KVAL   (INDX)                      
251000                             MOD-FLTEXT   (INDX)                          
251100                             MOD-IDARTNR-DEL (INDX)                       
251200     .                                                                    
251300     SKIP3                                                                
251400 MFS-RENSA-FAELT-IN SECTION.                                              
251500                                                                          
251600*    --- ALLA INDATA-FÄLT                                                 
251700     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UPD                              
251800                                MOD-IDRADNR-UPD                           
251900                                                                          
252000     MOVE +1 TO INDX                                                      
252100     PERFORM UNTIL INDX > MAX-INDX                                        
252200       MOVE MFS-RENSA-FAELT TO MOD-IDTRANS-HOPP (INDX)                    
252300       ADD +1 TO INDX                                                     
252400     END-PERFORM                                                          
252500     .                                                                    
252600     EJECT                                                                
252700 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
252800                                                                          
252900*    --- ALLA UTDATA-FÄLT                                                 
253000*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
253100                                                                          
253200     MOVE MFS-ROER-EJ-FAELT TO MOD-TIRETILL                               
253300                               MOD-TIRETANK                               
253400                               MOD-KDLEVANM                               
253500                               MOD-IDANSTNR-RET                           
253600                                                                          
253700     MOVE +1 TO INDX                                                      
253800     PERFORM UNTIL INDX > MAX-INDX                                        
253900       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
254000       ADD +1 TO INDX                                                     
254100     END-PERFORM                                                          
254200     .                                                                    
254300     SKIP2                                                                
254400 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
254500                                                                          
254600*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
254700     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR (INDX)                         
254800                               MOD-IDRADNR (INDX)                         
254900                               MOD-IDDC     (INDX)                        
255000                               MOD-IDORDNR5 (INDX)                        
255100                               MOD-KVLEVANM (INDX)                        
255200                               MOD-TIINLINL (INDX)                        
255300                               MOD-KVRETINL (INDX)                        
255400                               MOD-KVRETINL-SKR (INDX)                    
255500                               MOD-KVAVV-KVANT  (INDX)                    
255600                               MOD-KVAVV-KVAL   (INDX)                    
255700                               MOD-FLTEXT   (INDX)                        
255800                               MOD-IDARTNR-DEL (INDX)                     
255900     .                                                                    
256000     SKIP3                                                                
256100 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
256200                                                                          
256300*    --- ALLA INDATA-FÄLT                                                 
256400     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-UPD                            
256500                                 MOD-IDRADNR-UPD                          
256600                                                                          
256700     MOVE +1 TO INDX                                                      
256800     PERFORM UNTIL INDX > MAX-INDX                                        
256900       MOVE MFS-ROER-EJ-FAELT TO MOD-IDTRANS-HOPP (INDX)                  
257000       ADD +1 TO INDX                                                     
257100     END-PERFORM                                                          
257200     .                                                                    
257300     EJECT                                                                
257400 MFS-STAENG-FAELT-IN  SECTION.                                            
257500                                                                          
257600*    --- ALLA INDATA-FÄLT                                                 
257700     MOVE MFS-STAENG-FAELT-NOMOD   TO MOD-IDARTNR-UPD-ATTR                
257800                                      MOD-IDRADNR-UPD-ATTR                
257900                                                                          
258000     MOVE +1 TO INDX                                                      
258100     PERFORM UNTIL INDX > MAX-INDX                                        
258200       MOVE MFS-STAENG-FAELT-NOMOD TO MOD-IDTRANS-ATTR (INDX)             
258300       ADD +1 TO INDX                                                     
258400     END-PERFORM                                                          
258500     .                                                                    
258600     EJECT                                                                
258700 MFS-FORM-ATTR SECTION.                                                   
258800                                                                          
258900*    --- ALLA INDATA-FÄLT                                                 
259000     MOVE MFS-FORMATETS-ATTR TO MOD-IDARTNR-UPD-ATTR                      
259100                                MOD-IDRADNR-UPD-ATTR                      
259200     .                                                                    
259300     SKIP2                                                                
259400 IMS-GET-MSG SECTION.                                                     
259500                                                                          
259600     MOVE '  QC' TO GODK-STATUSKODER                                      
259700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
259800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
259900     PERFORM IMS-STATUSKONTROLL                                           
260000     .                                                                    
260100     SKIP3                                                                
260200 IMS-INSERT-MSG SECTION.                                                  
260300                                                                          
260400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
260500     MOVE SPACE TO GODK-STATUSKODER                                       
260600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
260700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
260800     PERFORM IMS-STATUSKONTROLL                                           
260900     .                                                                    
261000     EJECT                                                                
261100 IMS-CHANGE-ALTMSG SECTION.                                               
261200     MOVE '  A1A4' TO GODK-STATUSKODER                                    
261300     CALL CBLTDLI USING CHNG ALT-PCB P-TO-P-KDTRANS                       
261400     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
261500     PERFORM IMS-STATUSKONTROLL                                           
261600     .                                                                    
261700     SKIP3                                                                
261800 IMS-INSERT-ALTMSG SECTION.                                               
261900     MOVE SPACE TO GODK-STATUSKODER                                       
262000     CALL CBLTDLI USING ISRT ALT-PCB P-TO-P-IO-AREA                       
262100     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
262200     PERFORM IMS-STATUSKONTROLL                                           
262300     .                                                                    
262400     EJECT                                                                
262500 IMS-GU-WDA201 SECTION.                                                   
262600                                                                          
262700     STRING 'WDA201  (IDLEVANM =' W-IDLEVANM-X                            
262800                    '&IDFTG    =' W-IDFTG-X    ')'                        
262900          DELIMITED BY SIZE INTO SSA1                                     
263000     MOVE '  GE' TO GODK-STATUSKODER                                      
263100     CALL CBLTDLI USING GU WDA2-PCB DLI-IO-WDA201 SSA1                    
263200     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
263300     PERFORM IMS-STATUSKONTROLL                                           
263400     .                                                                    
263500     SKIP3                                                                
263600 IMS-GNP-WDA211-KVAL SECTION.                                             
263700                                                                          
263800     STRING 'WDA211  (WDA211KY =' W-WDA211KY-X ')'                        
263900          DELIMITED BY SIZE INTO SSA1                                     
264000     MOVE '  GE' TO GODK-STATUSKODER                                      
264100     CALL CBLTDLI USING GNP WDA2-PCB DLI-IO-WDA211 SSA1                   
264200     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
264300     PERFORM IMS-STATUSKONTROLL                                           
264400     .                                                                    
264500     SKIP3                                                                
264600 IMS-GNP-WDA211 SECTION.                                                  
264700                                                                          
264800     MOVE 'WDA211   ' TO SSA1                                             
264900     MOVE '  GE' TO GODK-STATUSKODER                                      
265000     CALL CBLTDLI USING GNP WDA2-PCB DLI-IO-WDA211 SSA1                   
265100     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
265200     PERFORM IMS-STATUSKONTROLL                                           
265300     .                                                                    
265400     SKIP3                                                                
265500 IMS-REPL-WDA211 SECTION.                                                 
265600                                                                          
265700     MOVE '  ' TO GODK-STATUSKODER                                        
265800     CALL CBLTDLI USING REPL WDA2-PCB DLI-IO-WDA211                       
265900     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
266000     PERFORM IMS-STATUSKONTROLL                                           
266100     .                                                                    
266200     EJECT                                                                
266300 IMS-GHU-WDA211 SECTION.                                                  
266400                                                                          
266500     STRING 'WDA201  (IDLEVANM =' W-IDLEVANM-X                            
266600                    '&IDFTG    =' W-IDFTG-X    ')'                        
266700          DELIMITED BY SIZE INTO SSA1                                     
266800     STRING 'WDA211  (WDA211KY =' W-WDA211KY-X ')'                        
266900          DELIMITED BY SIZE INTO SSA2                                     
267000     MOVE '  GE' TO GODK-STATUSKODER                                      
267100     CALL CBLTDLI USING GHU WDA2-PCB DLI-IO-WDA211 SSA1 SSA2              
267200     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
267300     PERFORM IMS-STATUSKONTROLL                                           
267400     .                                                                    
267500     SKIP3                                                                
267600 IMS-GU-WDK601 SECTION.                                                   
267700                                                                          
267800     STRING 'WDK601  (IDARTNR = ' W-IDARTNR-X ')'                         
267900          DELIMITED BY SIZE INTO SSA1                                     
268000     MOVE '  GE' TO GODK-STATUSKODER                                      
268100     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
268200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
268300     PERFORM IMS-STATUSKONTROLL                                           
268400     .                                                                    
268500     SKIP3                                                                
268600 IMS-GU-WDK611 SECTION.                                                   
268700                                                                          
268800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
268900          DELIMITED BY SIZE INTO SSA1                                     
269000     MOVE 'WDK611   ' TO SSA2                                             
269100     MOVE '  ' TO GODK-STATUSKODER                                        
269200     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
269300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
269400     PERFORM IMS-STATUSKONTROLL                                           
269500     .                                                                    
269600     SKIP3                                                                
269700 IMS-GHU-WDK611 SECTION.                                                  
269800                                                                          
269900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
270000          DELIMITED BY SIZE INTO SSA1                                     
270100     MOVE 'WDK611   ' TO SSA2                                             
270200     MOVE '  ' TO GODK-STATUSKODER                                        
270300     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
270400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
270500     PERFORM IMS-STATUSKONTROLL                                           
270600     .                                                                    
270700     SKIP3                                                                
270800 IMS-GNP-WDK611 SECTION.                                                  
270900                                                                          
271000     MOVE 'WDK611   ' TO SSA1                                             
271100     MOVE '  GE' TO GODK-STATUSKODER                                      
271200     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
271300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
271400     PERFORM IMS-STATUSKONTROLL                                           
271500     .                                                                    
271600     SKIP3                                                                
271700 IMS-REPL-WDK611 SECTION.                                                 
271800                                                                          
271900     MOVE '  ' TO GODK-STATUSKODER                                        
272000     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
272100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
272200     PERFORM IMS-STATUSKONTROLL                                           
272300     .                                                                    
272400     EJECT                                                                
272500 IMS-GHU-WDK711 SECTION.                                                  
272600                                                                          
272700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
272800          DELIMITED BY SIZE INTO SSA1                                     
272900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
273000          DELIMITED BY SIZE INTO SSA2                                     
273100     MOVE '  GE' TO GODK-STATUSKODER                                      
273200     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
273300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
273400     PERFORM IMS-STATUSKONTROLL                                           
273500     .                                                                    
273600     SKIP3                                                                
273700 IMS-GU-WDK711 SECTION.                                                   
273800                                                                          
273900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
274000          DELIMITED BY SIZE INTO SSA1                                     
274100     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
274200          DELIMITED BY SIZE INTO SSA2                                     
274300     MOVE '  GE' TO GODK-STATUSKODER                                      
274400     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
274500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
274600     PERFORM IMS-STATUSKONTROLL                                           
274700     .                                                                    
274800     SKIP3                                                                
274900 IMS-REPL-WDK711 SECTION.                                                 
275000                                                                          
275100     MOVE '  ' TO GODK-STATUSKODER                                        
275200     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
275300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
275400     PERFORM IMS-STATUSKONTROLL                                           
275500     .                                                                    
275600     EJECT                                                                
275700 IMS-ISRT-WDL901 SECTION.                                                 
275800                                                                          
275900     MOVE 'WDL901   ' TO SSA1                                             
276000     MOVE '  II' TO GODK-STATUSKODER                                      
276100     CALL CBLTDLI USING ISRT WDL9-PCB DLI-IO-WDL901 SSA1                  
276200     MOVE WDL9-STATUS-CODE TO STATUS-WS                                   
276300     PERFORM IMS-STATUSKONTROLL                                           
276400     .                                                                    
276500     EJECT                                                                
276600 IMS-GU-WDH101 SECTION.                                                   
276700*    DISPLAY '*** IMS-GU-WDH101'                                          
276800                                                                          
276900     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
277000          DELIMITED BY SIZE INTO SSA1                                     
277100     MOVE '  GE' TO GODK-STATUSKODER                                      
277200     CALL CBLTDLI USING GU WDH1-PCB DLI-IO-WDH101 SSA1                    
277300     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
277400     PERFORM IMS-STATUSKONTROLL                                           
277500     .                                                                    
277600     SKIP3                                                                
277700 IMS-ISRT-WDH101 SECTION.                                                 
277800*    DISPLAY '***** IMS-ISRT-WDH101'                                      
277900                                                                          
278000     MOVE 'WDH101   ' TO SSA1                                             
278100     MOVE '  II' TO GODK-STATUSKODER                                      
278200     CALL CBLTDLI USING ISRT WDH1-PCB DLI-IO-WDH101 SSA1                  
278300     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
278400     PERFORM IMS-STATUSKONTROLL                                           
278500     .                                                                    
278600     EJECT                                                                
278700 IMS-GNP-WDH111 SECTION.                                                  
278800*    DISPLAY '*** IMS-GNP-WDH111'                                         
278900                                                                          
279000     STRING  'WDH111  (WDH111KY>=' W-WDH111KY-MIN                         
279100                     '&WDH111KY<=' W-WDH111KY-MAX ')'                     
279200              DELIMITED BY SIZE INTO SSA1                                 
279300     MOVE '  GE' TO GODK-STATUSKODER                                      
279400     CALL CBLTDLI USING GNP WDH1-PCB DLI-IO-WDH111 SSA1                   
279500     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
279600     PERFORM IMS-STATUSKONTROLL                                           
279700     .                                                                    
279800 IMS-ISRT-WDH111 SECTION.                                                 
279900*    DISPLAY '**** IMS-ISRT-WDH111'                                       
280000                                                                          
280100     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
280200          DELIMITED BY SIZE INTO SSA1                                     
280300     MOVE 'WDH111   ' TO SSA2                                             
280400     MOVE '  II' TO GODK-STATUSKODER                                      
280500     CALL CBLTDLI USING ISRT WDH1-PCB DLI-IO-WDH111 SSA1 SSA2             
280600     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
280700     PERFORM IMS-STATUSKONTROLL                                           
280800     .                                                                    
280900     EJECT                                                                
281000 IMS-INSERT-WDH121 SECTION.                                               
281100                                                                          
281200     MOVE 'WDH121  ' TO SSA1                                              
281300     MOVE '  II' TO GODK-STATUSKODER                                      
281400     CALL CBLTDLI USING ISRT WDH1-PCB                                     
281500     DLI-IO-WDH121 SSA1                                                   
281600                                                                          
281700     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
281800     PERFORM IMS-STATUSKONTROLL                                           
281900     .                                                                    
282000     SKIP3                                                                
282100 IMS-GU-WDL201 SECTION.                                                   
282200                                                                          
282300     STRING 'WDL201  (IDARTNR  =' W-IDARTNR-X ')'                         
282400          DELIMITED BY SIZE INTO SSA1                                     
282500     MOVE '  GE' TO GODK-STATUSKODER                                      
282600     CALL CBLTDLI USING GU   WDL2-PCB DLI-IO-WDL201 SSA1                  
282700     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
282800     PERFORM IMS-STATUSKONTROLL                                           
282900     .                                                                    
283000     SKIP3                                                                
283100 IMS-ISRT-WDL201 SECTION.                                                 
283200                                                                          
283300     MOVE 'WDL201   ' TO SSA1                                             
283400     MOVE '  ' TO GODK-STATUSKODER                                        
283500     CALL CBLTDLI USING ISRT WDL2-PCB DLI-IO-WDL201 SSA1                  
283600     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
283700     PERFORM IMS-STATUSKONTROLL                                           
283800     .                                                                    
283900     EJECT                                                                
284000 IMS-ISRT-WDL211 SECTION.                                                 
284100                                                                          
284200     STRING 'WDL201  (IDARTNR  =' W-IDARTNR-X ')'                         
284300          DELIMITED BY SIZE INTO SSA1                                     
284400     MOVE 'WDL211   ' TO SSA2                                             
284500     MOVE '  II' TO GODK-STATUSKODER                                      
284600     CALL CBLTDLI USING ISRT WDL2-PCB DLI-IO-WDL211 SSA1 SSA2             
284700     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
284800     PERFORM IMS-STATUSKONTROLL                                           
284900     .                                                                    
285000     EJECT                                                                
285100 IMS-ISRT-WDL222 SECTION.                                                 
285200                                                                          
285300     STRING 'WDL201  (IDARTNR  =' W-IDARTNR-X ')'                         
285400          DELIMITED BY SIZE INTO SSA1                                     
285500     STRING 'WDL211  (DAINLEV  =' W-DAINLEV-X ')'                         
285600          DELIMITED BY SIZE INTO SSA2                                     
285700     MOVE 'WDL222   ' TO SSA3                                             
285800     MOVE '  ' TO GODK-STATUSKODER                                        
285900     CALL CBLTDLI USING ISRT WDL2-PCB DLI-IO-WDL222                       
286000                        SSA1 SSA2 SSA3                                    
286100     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
286200     PERFORM IMS-STATUSKONTROLL                                           
286300     .                                                                    
286400     EJECT                                                                
286500 IMS-GU-WDL601      SECTION.                                              
286600                                                                          
286700     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
286800          DELIMITED BY SIZE INTO SSA1                                     
286900     MOVE '  GE' TO GODK-STATUSKODER                                      
287000     CALL CBLTDLI USING GU   WDL6-PCB DLI-IO-WDL601 SSA1                  
287100     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
287200     PERFORM IMS-STATUSKONTROLL                                           
287300     .                                                                    
287400     SKIP3                                                                
287500 IMS-ISRT-WDL601 SECTION.                                                 
287600                                                                          
287700     MOVE 'WDL601   ' TO SSA1                                             
287800     MOVE '  II' TO GODK-STATUSKODER                                      
287900     CALL CBLTDLI USING ISRT WDL6-PCB DLI-IO-WDL601 SSA1                  
288000     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
288100     PERFORM IMS-STATUSKONTROLL                                           
288200     .                                                                    
288300     EJECT                                                                
288400 IMS-ISRT-WDL611      SECTION.                                            
288500                                                                          
288600     MOVE 'WDL611   ' TO SSA1                                             
288700     MOVE '  II' TO GODK-STATUSKODER                                      
288800     CALL CBLTDLI USING ISRT WDL6-PCB DLI-IO-WDL611 SSA1                  
288900     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
289000     PERFORM IMS-STATUSKONTROLL                                           
289100     .                                                                    
289200     SKIP3                                                                
289300 IMS-GHU-WL411111    SECTION.                                             
289400                                                                          
289500     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-4111-X ')'                    
289600            DELIMITED BY SIZE INTO SSA1                                   
289700     MOVE 'WDGX4112 ' TO SSA2                                             
289800     MOVE '  ' TO GODK-STATUSKODER                                        
289900     CALL CBLTDLI USING GHU 4111-PCB DLI-IO-WDGX4112 SSA1 SSA2            
290000     MOVE 4111-STATUS-CODE TO STATUS-WS                                   
290100     PERFORM IMS-STATUSKONTROLL                                           
290200     .                                                                    
290300     EJECT                                                                
290400 IMS-REPL-WL4111    SECTION.                                              
290500                                                                          
290600     MOVE '  ' TO GODK-STATUSKODER                                        
290700     CALL CBLTDLI USING REPL 4111-PCB DLI-IO-WDGX4112                     
290800     MOVE 4111-STATUS-CODE TO STATUS-WS                                   
290900     PERFORM IMS-STATUSKONTROLL                                           
291000     .                                                                    
291100     EJECT                                                                
291200 IMS-ISRT-WDR901 SECTION.                                                 
291300                                                                          
291400     MOVE 'WDR901   ' TO SSA1                                             
291500     MOVE '  II' TO GODK-STATUSKODER                                      
291600     CALL CBLTDLI USING ISRT WDR9-PCB WDR901 SSA1                         
291700     MOVE WDR9-STATUS-CODE TO STATUS-WS                                   
291800     PERFORM IMS-STATUSKONTROLL                                           
291900     .                                                                    
292000     EJECT                                                                
292100 IMS-ISRT-WDR601 SECTION.                                                 
292200                                                                          
292300     MOVE 'WDR601 ' TO SSA1                                               
292400     MOVE '    ' TO GODK-STATUSKODER                                      
292500     CALL CBLTDLI USING ISRT WDR6-PCB DLI-IO-WDR601 SSA1                  
292600     MOVE WDR6-STATUS-CODE TO STATUS-WS                                   
292700     PERFORM IMS-STATUSKONTROLL                                           
292800     .                                                                    
292900     SKIP3                                                                
293000 IMS-GU-BENA01-BSEQ SECTION.                                              
293100                                                                          
293200     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
293300          DELIMITED BY SIZE INTO SSA1                                     
293400     MOVE '  GE' TO GODK-STATUSKODER                                      
293500     CALL CBLTDLI USING GU BENA-PCB DLI-IO-BENA01 SSA1                    
293600     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
293700     PERFORM IMS-STATUSKONTROLL                                           
293800     .                                                                    
293900     SKIP3                                                                
294000 IMS-GNP-BENA11 SECTION.                                                  
294100                                                                          
294200     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
294300          DELIMITED BY SIZE INTO SSA1                                     
294400     MOVE '  GE' TO GODK-STATUSKODER                                      
294500     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-BENA11 SSA1                   
294600     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
294700     PERFORM IMS-STATUSKONTROLL                                           
294800     .                                                                    
294900     EJECT                                                                
295000                                                                          
295100 IMS-STATUSKONTROLL SECTION.                                              
295200                                                                          
295300     SET STATUS-IX TO 1                                                   
295400     SEARCH GODK-STATUS                                                   
295500       AT END                                                             
295600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
295700         DELIMITED BY SIZE INTO FELTEXT                                   
295800         CALL FELLOG                                                      
295900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
296000         CONTINUE                                                         
296100     END-SEARCH                                                           
296200     .                                                                    
