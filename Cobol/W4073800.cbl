000100*                                                                         
000200******************************************************************        
000300*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0153      *        
000400******************************************************************        
000500*                                                                         
000600 ID DIVISION.                                                             
000700                                                                          
000800 PROGRAM-ID.     W4073800.                                                
000900 AUTHOR.         LARS THELL.                                              
001000 DATE-WRITTEN.   95/07/07.                                                
001100 DATE-COMPILED.                                                           
001200                                                                          
001300*    FUNKTION:                                                            
001400*        VISA RETURTILLSTÅNDS.                                            
001500*        ANVÄNDS FÖR RAPPORTERING AV INLÄGGNING AV ILISTA.                
001600*                                                                         
001700*        PROGRAMMET UPPDATERAR WLKREE (WDA2)                              
001800*                                      WDR5                               
001900*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
002000*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
002100*                                                                         
002200*    ÄNDRAT: 2004-06 E-TRACKER SCR-ID 1334504                             
002300*            2004-09 E-TRACKER SCD-ID 829496                              
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W4T738                                              
002700*        MID:         W4I73801                                            
002800*                                                                         
002900*    UTDATA.                                                              
003000*        MOD:         W4O73801                                            
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600 WORKING-STORAGE SECTION.                                                 
003700                                                                          
003800*    -- CHECKED BY WY2000                                                 
003900 77  IDPGM                       PIC X(08)   VALUE 'W4073800'.            
004000                                                                          
004100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004300 77  PGM-POS                     PIC X(12)   VALUE SPACE.                 
004400                                                                          
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  YES                         PIC X       VALUE 'Y'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800 77  W-ILI                       PIC X(3)    VALUE 'ILI'.                 
004900 77  W-PLUS                      PIC X       VALUE '+'.                   
005000*  INNEHÅLLER X'3F'                                                       
005100 77  W-X3F                       PIC X       VALUE ''.                   
005200                                                                          
005300*    --- INDEX FÖR BLÄDDRINGSRADER                                        
005400 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005500 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
005600 77  4797-INDX                   PIC S9(4)  VALUE +0    COMP SYNC.        
005700 77  4797-MAX-INDX               PIC S9(4)  VALUE +16   COMP SYNC.        
005800 77  4795-IX                     PIC S9(4)  VALUE +0    COMP SYNC.        
005900                                                                          
006000 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
006100 77  W-ANM-AVIS                  PIC  X(1)  VALUE '4'.                    
006200 77  W-ANM-MOT                   PIC  X(1)  VALUE '5'.                    
006300 77  W-ANM-PAAB                  PIC  X(1)  VALUE '6'.                    
006400 77  W-KVLEVANM-KVAR             PIC S9(7)   VALUE 0   COMP-3.            
006500 77  W-KVRETINL-R32              PIC S9(6)   VALUE 0.                     
006600 77  W-KVRADER-BEH               PIC S9(3)   VALUE 0   COMP-3.            
006700 77  W-KVANTAL                   PIC S9(6)   VALUE 0.                     
006800 77  W-IDILIST                   PIC  9(5)   VALUE 0.                     
006900 77  W-SPAR-IDDISTR              PIC S9(5)   VALUE 0   COMP-3.            
007000 77  W-SPAR-IDKUNDNR             PIC S9(7)   VALUE 0   COMP-3.            
007100 77  W-SPAR-IDRAPPNR             PIC  9(7)   VALUE 0   COMP-3.            
007200 77  WS-ADGANG                   PIC  9(2)   VALUE 0.                     
007300                                                                          
007400 77  TEST-KDCMDVAL               PIC X(3)    VALUE SPACE.                 
007500     88  GODK-KDCMDVAL                       VALUE 'INL'                  
007600                                                   'BIN'.                 
007700                                                                          
007800 77  SW-IDANSTNR                 PIC X       VALUE 'N'.                   
007900     88  IDANSTNR-IFYLLT                     VALUE 'J'.                   
008000                                                                          
008100 77  SW-ALLT-INLAGT              PIC X       VALUE 'N'.                   
008200     88  ALLT-INLAGT                         VALUE 'J'.                   
008300                                                                          
008400 77  SW-MAKULERA-ILISTA          PIC X       VALUE 'N'.                   
008500     88  MAKULERA-ILISTA                     VALUE 'J'.                   
008600                                                                          
008700 77  SW-RAD-INPUT                PIC X       VALUE 'N'.                   
008800     88  RAD-INPUT                           VALUE 'J'.                   
008900                                                                          
009000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
009100     88  INDATA-OK                           VALUE 'J'.                   
009200     88  INDATA-FEL                          VALUE 'N'.                   
009300                                                                          
009400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
009500     88  NYCKLAR-OK                          VALUE 'J'.                   
009600     88  NYCKLAR-FEL                         VALUE 'N'.                   
009700                                                                          
009800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
009900     88  EGEN-MID                            VALUE '4738'.                
010000     88  GODK-MID                            VALUE '4738'.                
010100     88  HELP-MID                            VALUE '0551'.                
010200     EJECT                                                                
010300*      --- VALID IDDC CODES                                               
010400*                                                                         
010500*01    -COPY WWDC99                                                       
010600       EJECT                                                              
010700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
010800 01  GENERELLA-SUBPROGRAM.                                                
010900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
011000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
011100     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
011200     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
011300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011500     EJECT                                                                
011600*01  -COPY W006PRT                                                        
011700     EJECT                                                                
011800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
011900*01 -COPY WMEDAREA                                                        
012000     SKIP3                                                                
012100 01  MESSAGE-CODES.                                                       
012200     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
012300     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
012400     03  ERR-FORBIDDEN-UPDATE    PIC X(3)    VALUE '007'.                 
012500     03  INF-PRESS-PF4           PIC X(3)    VALUE '081'.                 
012600     03  ERR-INFO-MISSING        PIC X(3)    VALUE '005'.                 
012700     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
012800     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
012900     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
013000     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
013100     03  INF-PRINT-BEG           PIC X(3)    VALUE '118'.                 
013200     03  ERR-NOTHING-PRINTED     PIC X(3)    VALUE '167'.                 
013300     03  ERR-NO-LINE-CHOSEN      PIC X(3)    VALUE '231'.                 
013400     03  ERR-WRONG-COMBINATION   PIC X(3)    VALUE '238'.                 
013500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
013600     03  ERR-WRONG-PRINTER       PIC X(3)    VALUE '772'.                 
013610     03  ERR-LAGER-SAKNAS        PIC X(3)    VALUE '764'.                 
013700     EJECT                                                                
013800*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
013900*                                                                         
014000 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
014100     SKIP3                                                                
014200*01 -COPY WMSGINIT                                                        
014300     EJECT                                                                
014400*                                                                         
014500 01  FILLER              PIC X(16)   VALUE 'P-TO-P-AREA2'.                
014600*                                                                         
014700 01  P-TO-P-T95.                                                          
014800*----TILL W40795                                                          
014900     03  P-TO-P2-LL              PIC S9(4)   COMP SYNC.                   
015000     03  P-TO-P2-Z1              PIC X(1)    VALUE LOW-VALUE.             
015100     03  P-TO-P2-Z2              PIC X(1)    VALUE LOW-VALUE.             
015200     03  P-TO-P2-TRANSKOD        PIC X(7)    VALUE 'W4T795X'.             
015300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
015400     03  P-TO-P2-IDTRANS         PIC X(4)    VALUE '4738'.                
015500     03  P-TO-P2-KDMFSFOR        PIC X(1)    VALUE SPACE.                 
015600*    03  MID -COPY W4I79501 -PRE MOD4795-                                 
015700     EJECT                                                                
015800*  AREA FÖR DISPATCHEN                                                    
015900 01  P-TO-P-AREA1.                                                        
016000     03  P-TO-P1-LL              PIC S9(4)            COMP SYNC.          
016100     03  P-TO-P1-Z1              PIC  X(1)   VALUE LOW-VALUE.             
016200     03  P-TO-P1-Z2              PIC  X(1)   VALUE LOW-VALUE.             
016300     03  P-TO-P1-TRANSKOD        PIC  X(7).                               
016400     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
016500     03  P-TO-P1-FROM-MID        PIC  X(4).                               
016600     03  P-TO-P1-KDMFSFOR        PIC  X(1).                               
016700     03  P-TO-P1-DATA            PIC  X(1000).                            
016800                                                                          
016900     EJECT                                                                
017000*                                                                         
017100*    --- AREOR FÖR W006KOM SUBMODUL                                       
017200*                                                                         
017300 01  FILLER                      PIC X(16)   VALUE 'MSG-KOM-AREA'.        
017400*01  -COPY WMSGKOM                                                        
017500     EJECT                                                                
017600*                                                                         
017700 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
017800 01  KOM-IO-AREA.                                                         
017900   03  KOM-AREA                     PIC X(1000) VALUE SPACE.              
018000    03 R32      REDEFINES KOM-AREA.                                       
018100      05    -COPY W4I79701 -PRE MOD4797-                                  
018200     EJECT                                                                
018300*                                                                         
018400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
018500*                                                                         
018600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
018700     SKIP3                                                                
018800*01  MID -COPY W4I73801                                                   
018900     EJECT                                                                
019000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
019100     SKIP3                                                                
019200*01  -COPY WMSGAREA                                                       
019300     EJECT                                                                
019400     03  MOD REDEFINES MSG-AREA.                                          
019500*      05  -COPY W4O73801    -PRE MOD-                                    
019600     EJECT                                                                
019700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
019800     SKIP3                                                                
019900*01  -COPY WMFSAREA                                                       
020000     EJECT                                                                
020100                                                                          
020200     SKIP2                                                                
020300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
020400*                                                                         
020500     EJECT                                                                
020600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
020700     SKIP3                                                                
020800 01  W-MINKEY-X.                                                          
020900     03  W-MINKEY-IDTRANS          PIC  X(4)   VALUE '4738'.              
021000     03  W-MINKEY-WDA2E1KY-ENTER.                                         
021100         05  W-MINKEY-IDDC         PIC  X(2)          VALUE SPACE.        
021200         05  W-MINKEY-IDILIST      PIC  9(5)          VALUE ZERO.         
021300         05  W-MINKEY-ADLAGOMR     PIC S9(3)   COMP-3 VALUE ZERO.         
021400         05  W-MINKEY-ADGANG       PIC S9(3)   COMP-3 VALUE ZERO.         
021500         05  W-MINKEY-ADPLATS      PIC S9(5)   COMP-3 VALUE ZERO.         
021600         05  W-MINKEY-IDDISTR      PIC S9(5)   COMP-3 VALUE ZERO.         
021700         05  W-MINKEY-IDKUNDNR     PIC S9(7)   COMP-3 VALUE ZERO.         
021800         05  W-MINKEY-IDRAPPNR     PIC  9(7)          VALUE ZERO.         
021900         05  W-MINKEY-IDARTNR      PIC S9(9)   COMP-3 VALUE ZERO.         
022000         05  W-MINKEY-IDRADNR      PIC S9(5)   COMP-3 VALUE ZERO.         
022100     03  W-MINKEY-WDA2E1KY-NEXT.                                          
022200         05  W-MINKEY-IDDC-NEXT     PIC  X(2)         VALUE SPACE.        
022300         05  W-MINKEY-IDILIST-NEXT  PIC  9(5)          VALUE ZERO.        
022400         05  W-MINKEY-ADLAGOMR-NEXT PIC S9(3)   COMP-3 VALUE ZERO.        
022500         05  W-MINKEY-ADGANG-NEXT   PIC S9(3)   COMP-3 VALUE ZERO.        
022600         05  W-MINKEY-ADPLATS-NEXT  PIC S9(5)   COMP-3 VALUE ZERO.        
022700         05  W-MINKEY-IDDISTR-NEXT  PIC S9(5)   COMP-3 VALUE ZERO.        
022800         05  W-MINKEY-IDKUNDNR-NEXT PIC S9(7)   COMP-3 VALUE ZERO.        
022900         05  W-MINKEY-IDRAPPNR-NEXT PIC  9(7)          VALUE ZERO.        
023000         05  W-MINKEY-IDARTNR-NEXT  PIC S9(9)   COMP-3 VALUE ZERO.        
023100         05  W-MINKEY-IDRADNR-NEXT  PIC S9(5)   COMP-3 VALUE ZERO.        
023200     SKIP3                                                                
023300                                                                          
023400 01  NYCKLAR-TILL-DLI.                                                    
023500                                                                          
023600     03  W-IDLEVANM-X.                                                    
023700         05  W-IDDISTR           PIC S9(5)   COMP-3 VALUE ZERO.           
023800         05  W-IDKUNDNR          PIC S9(7)   COMP-3 VALUE ZERO.           
023900         05  W-IDRAPPNR          PIC  9(7)          VALUE ZERO.           
024000                                                                          
024100     03  W-WDA211KY-X.                                                    
024200         05  W-IDARTNR-A2        PIC S9(9)   COMP-3 VALUE ZERO.           
024300         05  W-IDRADNR-A2        PIC S9(5)   COMP-3 VALUE ZERO.           
024400                                                                          
024500     03  W-WDA2E1KY-MIN-X.                                                
024600         05  W-IDDC-E1-MIN       PIC  X(2)          VALUE SPACE.          
024700         05  W-IDILIST-E1-MIN    PIC  9(5)          VALUE ZERO.           
024800         05  W-ADLAGOMR-E1-MIN   PIC S9(3)   COMP-3 VALUE ZERO.           
024900         05  W-ADGANG-E1-MIN     PIC S9(3)   COMP-3 VALUE ZERO.           
025000         05  W-ADPLATS-E1-MIN    PIC S9(5)   COMP-3 VALUE ZERO.           
025100         05  W-IDDISTR-E1-MIN    PIC S9(5)   COMP-3 VALUE ZERO.           
025200         05  W-IDKUNDNR-E1-MIN   PIC S9(7)   COMP-3 VALUE ZERO.           
025300         05  W-IDRAPPNR-E1-MIN   PIC  9(7)          VALUE ZERO.           
025400         05  W-IDARTNR-E1-MIN    PIC S9(9)   COMP-3 VALUE ZERO.           
025500         05  W-IDRADNR-E1-MIN    PIC S9(5)   COMP-3 VALUE ZERO.           
025600                                                                          
025700     03  W-WDA2E1KY-MAX-X.                                                
025800         05  W-IDDC-E1-MAX       PIC  X(2)          VALUE SPACE.          
025900         05  W-IDILIST-E1-MAX    PIC  9(5)          VALUE ZERO.           
026000         05  W-ADLAGOMR-E1-MAX   PIC S9(3)   COMP-3 VALUE ZERO.           
026100         05  W-ADGANG-E1-MAX     PIC S9(3)   COMP-3 VALUE ZERO.           
026200         05  W-ADPLATS-E1-MAX    PIC S9(5)   COMP-3 VALUE ZERO.           
026300         05  W-IDDISTR-E1-MAX    PIC S9(5)   COMP-3 VALUE ZERO.           
026400         05  W-IDKUNDNR-E1-MAX   PIC S9(7)   COMP-3 VALUE ZERO.           
026500         05  W-IDRAPPNR-E1-MAX   PIC  9(7)          VALUE ZERO.           
026600         05  W-IDARTNR-E1-MAX    PIC S9(9)   COMP-3 VALUE ZERO.           
026700         05  W-IDRADNR-E1-MAX    PIC S9(5)   COMP-3 VALUE ZERO.           
026800                                                                          
026900     03  W-WDA2ESEQ-MIN-X.                                                
027000         05  W-IDDC-ESEQ-MIN     PIC  X(2)          VALUE SPACE.          
027100         05  W-IDILIST-ESEQ-MIN  PIC  9(5)          VALUE ZERO.           
027200         05  W-ADLAGOMR-ESEQ-MIN PIC S9(3)   COMP-3 VALUE ZERO.           
027300         05  W-ADGANG-ESEQ-MIN   PIC S9(3)   COMP-3 VALUE ZERO.           
027400         05  W-ADPLATS-ESEQ-MIN  PIC S9(5)   COMP-3 VALUE ZERO.           
027500                                                                          
027600     03  W-WDA2ESEQ-MAX-X.                                                
027700         05  W-IDDC-ESEQ-MAX     PIC  X(2)          VALUE SPACE.          
027800         05  W-IDILIST-ESEQ-MAX  PIC  9(5)          VALUE ZERO.           
027900         05  W-ADLAGOMR-ESEQ-MAX PIC S9(3)   COMP-3 VALUE ZERO.           
028000         05  W-ADGANG-ESEQ-MAX   PIC S9(3)   COMP-3 VALUE ZERO.           
028100         05  W-ADPLATS-ESEQ-MAX  PIC S9(5)   COMP-3 VALUE ZERO.           
028200                                                                          
028300     03  W-IDARTNR-X.                                                     
028400         05  W-IDARTNR           PIC S9(9)   COMP-3 VALUE ZERO.           
028500                                                                          
028600     03  W-IDDC-X.                                                        
028700         05  W-IDDC              PIC  X(2)          VALUE SPACE.          
028800                                                                          
028900     03  W-IDRADNR-X.                                                     
029000         05  W-IDRADNR           PIC S9(5)   COMP-3 VALUE ZERO.           
029100                                                                          
029200     03  W-IDSKYLT-X.                                                     
029300         05  W-IDSKYLT           PIC  X(3)   VALUE SPACE.                 
029400                                                                          
029500     03  W-WDGXKEY-X.                                                     
029600         05  W-IDHTYP            PIC  X(4)    VALUE '4703'.               
029700         05  W-IDDC-4703         PIC  X(2)    VALUE SPACE.                
029800         05  FILLER              PIC  X(24)   VALUE LOW-VALUE.            
029900                                                                          
030000     03  W-IDILIST-4704-X.                                                
030100         05  W-IDILIST-4704      PIC  9(5)          VALUE ZERO.           
030200                                                                          
030300     SKIP2                                                                
030400*    --- STATUS-KOD FRÅN IMS                                              
030500 01  STATUS-WS                   PIC XX.                                  
030600     88  STATUS-OK                           VALUE '  '.                  
030700     88  SEGMENT-FINNS                       VALUE '  '.                  
030800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
030900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
031000     88  SEGMENT-SLUT                        VALUE 'GB'.                  
031100     88  TRANSKOD-FEL                        VALUE 'A1'.                  
031200     88  SECURITY-FEL                        VALUE 'A4'.                  
031300     SKIP2                                                                
031400 01  GODK-STATUSKODER.                                                    
031500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
031600     SKIP3                                                                
031700 01  SSA1                        PIC X(192).                              
031800 01  SSA2                        PIC X(64).                               
031900     EJECT                                                                
032000*    --- IMS FUNKTIONSKODER                                               
032100*01  -COPY W0003                                                          
032200     EJECT                                                                
032300*    ---  DLI INPUT-OUTPUT AREA                                           
032400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
032500     SKIP3                                                                
032600 01  DLI-IO-AREA.                                                         
032700     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
032800     SKIP3                                                                
032900     03  WLKREJ01 REDEFINES IO-AREA.                                      
033000*        05  -COPY WDA2E1                                                 
033100     EJECT                                                                
033200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
033300     SKIP3                                                                
033400 01  DLI-IO-AREA2.                                                        
033500     03  IO-AREA2                PIC X(900)  VALUE SPACE.                 
033600     SKIP3                                                                
033700     03  WLKREE01 REDEFINES IO-AREA2.                                     
033800*        05  -COPY WDA201                                                 
033900     EJECT                                                                
034000     03  WLKREE11 REDEFINES IO-AREA2.                                     
034100*        05  -COPY WDA211                                                 
034200     EJECT                                                                
034300     03  WLBENA11 REDEFINES IO-AREA2.                                     
034400*        05  -COPY WDD311                                                 
034500     EJECT                                                                
034600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-K611'.         
034700 01  DLI-IO-K611.                                                         
034800*    03  -COPY WDK611                                                     
034900     EJECT                                                                
035000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-K711'.         
035100 01  DLI-IO-K711.                                                         
035200*    03  -COPY WDK711                                                     
035300                                                                          
035400 01  FILLER               PIC X(16)   VALUE 'WDGX4704 AREA'.              
035500 01   DLI-IO-WDGX4704.                                                    
035600*     03  -COPY WDGX4704                                                  
035700     EJECT                                                                
035800 LINKAGE SECTION.                                                         
035900                                                                          
036000*01  -COPY W0009   -PRE MSG-                                              
036100*01  -COPY W0009   -PRE DISP-                                             
036200     EJECT                                                                
036300*01  -COPY W0009   -PRE W4795-                                            
036400     EJECT                                                                
036500*01  -COPY W0008   -PRE USEA-                                             
036600     05  FILLER                  PIC X.                                   
036700     EJECT                                                                
036800*01  -COPY W0008  -PRE KREE1-                                             
036900     05  FILLER                  PIC X.                                   
037000     EJECT                                                                
037100*01  -COPY W0008  -PRE KREE2-                                             
037200     05  FILLER                  PIC X.                                   
037300     EJECT                                                                
037400*01  -COPY W0008  -PRE KREJ-                                              
037500     05  FILLER                  PIC X.                                   
037600     EJECT                                                                
037700*01  -COPY W0008  -PRE BENA-                                              
037800     05  FILLER                  PIC X.                                   
037900     EJECT                                                                
038000*01  -COPY W0008  -PRE ARTC-                                              
038100     EJECT                                                                
038200     05  FILLER                  PIC X.                                   
038300*01  -COPY W0008  -PRE ARTS-                                              
038400     EJECT                                                                
038500     05  FILLER                  PIC X.                                   
038600*01  -COPY W0008  -PRE RETA1-                                             
038700     05  FILLER                  PIC X.                                   
038800     EJECT                                                                
038900*01  -COPY W0008  -PRE RETA2-                                             
039000     05  FILLER                  PIC X.                                   
039100     EJECT                                                                
039200*01  -COPY W0008  -PRE KOMA-                                              
039300     05  FILLER                  PIC X.                                   
039400     EJECT                                                                
039500*01  -COPY W0008  -PRE WDR5-                                              
039600     05  FILLER                  PIC X.                                   
039700     EJECT                                                                
039800 PROCEDURE DIVISION  USING MSG-PCB  DISP-PCB W4795-PCB                    
039900                           USEA-PCB                                       
040000                           KREE1-PCB KREE2-PCB KREJ-PCB BENA-PCB          
040100                           ARTC-PCB ARTS-PCB                              
040200                           RETA1-PCB RETA2-PCB KOMA-PCB WDR5-PCB.         
040300     ENTRY 'DLITCBL' USING MSG-PCB   DISP-PCB W4795-PCB                   
040400                           USEA-PCB                                       
040500                           KREE1-PCB KREE2-PCB KREJ-PCB BENA-PCB          
040600                           ARTC-PCB ARTS-PCB                              
040700                           RETA1-PCB RETA2-PCB KOMA-PCB WDR5-PCB.         
040800                                                                          
040900     PERFORM IMS-GET-MSG                                                  
041000     IF SEGMENT-FINNS                                                     
041100       PERFORM A-INIT                                                     
041200       PERFORM B-KOLLA-NYCKLAR                                            
041300       IF NYCKLAR-OK                                                      
041400         MOVE MID-MODFAELT-IN         TO MOD-INPUT                        
041500         INSPECT MOD-INPUT REPLACING ALL W-PLUS BY W-X3F                  
041600         IF (MFS-ENTER AND EGEN-MID) OR HELP-MID OR MFS-UPDATE OR         
041700             MFS-PRINT                                                    
041800             PERFORM G-KOLLA-INPUT                                        
041900         END-IF                                                           
042000                                                                          
042100         IF (MFS-PRINT OR MFS-UPDATE)                                     
042200             IF INDATA-OK                                                 
042300                PERFORM H-UPPDATERA-SKRIV-UT                              
042400             END-IF                                                       
042500         ELSE                                                             
042600            IF MFS-FIRST                                                  
042700               PERFORM C-FOERSTA-SIDA                                     
042800            ELSE                                                          
042900               IF MFS-NEXT                                                
043000                  PERFORM D-NAESTA-SIDA                                   
043100               ELSE                                                       
043200                  PERFORM E-SAMMA-SIDA                                    
043300               END-IF                                                     
043400            END-IF                                                        
043500         END-IF                                                           
043600         IF INDATA-OK                                                     
043700            PERFORM F-LAES-VISA-INFO                                      
043800         END-IF                                                           
043900       END-IF                                                             
044000       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O73801 + 4                      
044100       PERFORM IMS-INSERT-MSG                                             
044200     END-IF                                                               
044300                                                                          
044400     MOVE ZERO TO RETURN-CODE                                             
044500     GOBACK                                                               
044600     .                                                                    
044700     EJECT                                                                
044800 A-INIT SECTION.                                                          
044900                                                                          
045000     IF MSG-DUBBLA-TRANSKODER                                             
045100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I73801                 
045200       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
045300       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
045400     ELSE                                                                 
045500       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I73801                 
045600       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
045700       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
045800     END-IF                                                               
045900                                                                          
046000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
046100     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
046200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
046300                                                                          
046400     MOVE LOW-VALUE TO MSG-AREA                                           
046500     MOVE 'W4O73801' TO MFS-IDMOD                                         
046600     MOVE '4738' TO MOD-IDTRANS                                           
046700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
046800                                                                          
046900     IF EGEN-MID OR HELP-MID                                              
047000       CONTINUE                                                           
047100     ELSE                                                                 
047200       MOVE SPACE TO MFS-KDTRTYP                                          
047300       MOVE '7' TO MFS-IDPFK                                              
047400     END-IF                                                               
047500                                                                          
047600     MOVE LOW-VALUE             TO W-WDA2E1KY-MIN-X                       
047700                                                                          
047800     MOVE HIGH-VALUE            TO W-WDA2E1KY-MAX-X                       
047900                                   W-WDA2ESEQ-MAX-X                       
048000                                                                          
048100     .                                                                    
048200     EJECT                                                                
048300 B-KOLLA-NYCKLAR SECTION.                                                 
048400                                                                          
048500     MOVE ALL '+'              TO MSGI-WMSGINIT                           
048600     MOVE '001'                TO MSGI-KDCALL                             
048700     IF EGEN-MID                                                          
048800        MOVE MID-IDILIST-IN    TO MSGI-IDILIST                            
048900     END-IF                                                               
049000     MOVE MSG-SIGNON-USERID    TO MSGI-IDUSER                             
049100     MOVE '4738'               TO MSGI-IDTRANS                            
049200     MOVE MSG-LTERM-NAME       TO MSGI-IDLTERM-USER                       
049300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
049400                                                                          
049500     IF MSGI-IDLAND-SPR = 'GB'                                            
049600       MOVE 'GB'                  TO MED-IDSKYLT                          
049700                                       W-IDSKYLT                          
049800     ELSE                                                                 
049900       MOVE 'S '                  TO MED-IDSKYLT                          
050000                                       W-IDSKYLT                          
050100     END-IF                                                               
050200                                                                          
050300     MOVE JA TO NYCKLAR-SW                                                
050400                                                                          
050500     PERFORM BA-KOLLA-IDILIST                                             
050600     MOVE MSGI-IDDC               TO W-IDDC-E1-MIN                        
050700                                     W-IDDC-E1-MAX                        
050800                                     W-IDDC-ESEQ-MIN                      
050900                                     W-IDDC-ESEQ-MAX                      
051000                                     W-IDDC                               
051100                                     WS-IDDC                              
051200                                                                          
051300     IF GODK-MID OR NYCKLAR-OK                                            
051400        MOVE MSGI-IDILIST         TO MOD-IDILIST-UT                       
051500        INSPECT MOD-IDILIST-UT REPLACING LEADING ZERO BY SPACE            
051600     ELSE                                                                 
051700        MOVE MFS-RENSA-FAELT      TO MOD-IDILIST-UT                       
051800     END-IF                                                               
051900                                                                          
052000     IF NYCKLAR-FEL                                                       
052100       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
052200       CALL WMEDKONV USING MED-WMEDAREA                                   
052300       MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                 
052400       PERFORM MFS-RENSA-FAELT-IN                                         
052500       PERFORM MFS-RENSA-FAELT-UT                                         
052600     END-IF                                                               
052700     .                                                                    
052800     EJECT                                                                
052900                                                                          
053000 BA-KOLLA-IDILIST  SECTION.                                               
053100                                                                          
053200     MOVE MFS-RENSA-FAELT       TO MOD-IDILIST-IN                         
053300                                                                          
053400     IF MID-IDILIST-IN          NOT = ALL '+'                             
053500       MOVE '7'                 TO MFS-IDPFK                              
053600       MOVE SPACE               TO MFS-KDTRTYP                            
053700     END-IF                                                               
053800                                                                          
053900     IF MSGI-IDILIST NUMERIC AND MSGI-IDILIST > ZERO                      
054000       MOVE MSGI-IDILIST        TO W-IDILIST-E1-MIN                       
054100                                   W-IDILIST-ESEQ-MIN                     
054200                                   W-IDILIST-E1-MAX                       
054300                                   W-IDILIST-ESEQ-MAX                     
054400     ELSE                                                                 
054500       MOVE NEJ                 TO NYCKLAR-SW                             
054600     END-IF                                                               
054700                                                                          
054800     .                                                                    
054900     EJECT                                                                
055000                                                                          
055100 C-FOERSTA-SIDA SECTION.                                                  
055200                                                                          
055300     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
055400     CALL WMEDKONV USING MED-WMEDAREA                                     
055500     MOVE MED-MFSINF     TO MOD-TEMFSFEL                                  
055600                                                                          
055700*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
055800     PERFORM MFS-RENSA-FAELT-IN                                           
055900     .                                                                    
056000     EJECT                                                                
056100 D-NAESTA-SIDA SECTION.                                                   
056200                                                                          
056300      MOVE MSGI-SPAR-AREA         TO W-MINKEY-X                           
056400      IF W-MINKEY-IDTRANS = '4738'                                        
056500        MOVE W-MINKEY-WDA2E1KY-NEXT TO W-WDA2E1KY-MIN-X                   
056600     ELSE                                                                 
056700        MOVE LOW-VALUE              TO W-WDA2E1KY-MIN-X                   
056800        MOVE MSGI-IDDC              TO W-IDDC-E1-MIN                      
056900                                       W-IDDC-E1-MAX                      
057000        PERFORM MFS-RENSA-FAELT-IN                                        
057100     END-IF                                                               
057200     .                                                                    
057300     EJECT                                                                
057400 E-SAMMA-SIDA SECTION.                                                    
057500                                                                          
057600     MOVE MSGI-SPAR-AREA         TO W-MINKEY-X                            
057700     IF W-MINKEY-IDTRANS = '4738'                                         
057800       MOVE W-MINKEY-WDA2E1KY-ENTER TO W-WDA2E1KY-MIN-X                   
057900       IF MID-INPUT                =  ALL '+'                             
058000          PERFORM MFS-RENSA-FAELT-IN                                      
058100       ELSE                                                               
058200          MOVE INF-PRESS-PF11      TO MED-IDMFSINF                        
058300          CALL WMEDKONV USING MED-WMEDAREA                                
058400          MOVE MED-MFSINF          TO MOD-TEMFSFEL                        
058500       END-IF                                                             
058600     ELSE                                                                 
058700        MOVE LOW-VALUE           TO W-WDA2E1KY-MIN-X                      
058800        MOVE MSGI-IDDC           TO W-IDDC-E1-MIN                         
058900                                    W-IDDC-E1-MAX                         
059000        PERFORM MFS-RENSA-FAELT-IN                                        
059100     END-IF                                                               
059200     .                                                                    
059300     EJECT                                                                
059400 F-LAES-VISA-INFO SECTION.                                                
059500                                                                          
059600     PERFORM IMS-GU-WLKREJ01-OKVAL                                        
059700     PERFORM FA-FIXA-ENTER-KEY                                            
059800                                                                          
059900     IF SEGMENT-SAKNAS                                                    
060000       MOVE ERR-INFO-MISSING   TO MED-IDMFSFEL                            
060100       CALL WMEDKONV USING MED-WMEDAREA                                   
060200       MOVE MED-MFSFEL         TO MOD-TEMFSFEL                            
060300       PERFORM MFS-RENSA-FAELT-IN                                         
060400       PERFORM MFS-RENSA-FAELT-UT                                         
060500     ELSE                                                                 
060600       MOVE +1                  TO INDX                                   
060700                                                                          
060800       PERFORM UNTIL INDX        > MAX-INDX                               
060900         IF SEGMENT-FINNS                                                 
061000            MOVE SEQE-IDDISTR       TO W-IDDISTR                          
061100            MOVE SEQE-IDKUNDNR      TO W-IDKUNDNR                         
061200            MOVE SEQE-IDRAPPNR      TO W-IDRAPPNR                         
061300            MOVE SEQE-IDARTNR       TO W-IDARTNR-A2                       
061400            MOVE SEQE-IDRADNR       TO W-IDRADNR-A2                       
061500                                                                          
061600            PERFORM IMS-GHU-WLKREE11                                      
061700            PERFORM FB-REDIGERA-RAD-UPPGIFTER                             
061800                                                                          
061900            PERFORM IMS-GN-WLKREJ01-OKVAL                                 
062000            ADD +1               TO INDX                                  
062100         ELSE                                                             
062200            PERFORM FC-RENSA-RAD                                          
062300            ADD +1               TO INDX                                  
062400         END-IF                                                           
062500       END-PERFORM                                                        
062600                                                                          
062700                                                                          
062800     END-IF                                                               
062900                                                                          
063000     PERFORM FD-FIXA-NEXT-KEY                                             
063100                                                                          
063200     MOVE '002'                TO MSGI-KDCALL                             
063300     MOVE '4738'               TO MSGI-IDTRANS                            
063400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
063500     .                                                                    
063600     EJECT                                                                
063700                                                                          
063800 FA-FIXA-ENTER-KEY        SECTION.                                        
063900                                                                          
064000     IF SEGMENT-FINNS                                                     
064100        MOVE SEQE-IDDC-RET          TO W-MINKEY-IDDC                      
064200        MOVE SEQE-IDILIST           TO W-MINKEY-IDILIST                   
064300        MOVE SEQE-ADLAGOMR          TO W-MINKEY-ADLAGOMR                  
064400        MOVE SEQE-ADGANG            TO W-MINKEY-ADGANG                    
064500        MOVE SEQE-ADPLATS           TO W-MINKEY-ADPLATS                   
064600        MOVE SEQE-IDDISTR           TO W-MINKEY-IDDISTR                   
064700        MOVE SEQE-IDKUNDNR          TO W-MINKEY-IDKUNDNR                  
064800        MOVE SEQE-IDRAPPNR          TO W-MINKEY-IDRAPPNR                  
064900        MOVE SEQE-IDDISTR           TO W-MINKEY-IDDISTR                   
065000        MOVE SEQE-IDARTNR           TO W-MINKEY-IDARTNR                   
065100        MOVE SEQE-IDRADNR           TO W-MINKEY-IDRADNR                   
065200     ELSE                                                                 
065300        MOVE MSGI-IDDC              TO W-MINKEY-IDDC                      
065400        MOVE MSGI-IDILIST           TO W-MINKEY-IDILIST                   
065500        MOVE ZERO                   TO W-MINKEY-ADLAGOMR                  
065600                                       W-MINKEY-ADGANG                    
065700                                       W-MINKEY-ADPLATS                   
065800                                       W-MINKEY-IDDISTR                   
065900                                       W-MINKEY-IDKUNDNR                  
066000                                       W-MINKEY-IDRAPPNR                  
066100                                       W-MINKEY-IDDISTR                   
066200                                       W-MINKEY-IDARTNR                   
066300                                       W-MINKEY-IDRADNR                   
066400     END-IF                                                               
066500     MOVE '4738'                    TO W-MINKEY-IDTRANS                   
066600     MOVE W-MINKEY-X                TO MSGI-SPAR-AREA                     
066700                                                                          
066800     .                                                                    
066900     EJECT                                                                
067000                                                                          
067100 FB-REDIGERA-RAD-UPPGIFTER  SECTION.                                      
067200                                                                          
067300     MOVE LEV-KVANTAL-ILI      TO MOD-KVANTAL-KVAR (INDX)                 
067400                                                                          
067500     MOVE LEV-IDARTNR          TO MOD-IDARTNR  (INDX)                     
067600     MOVE LEV-KDANMORS         TO MOD-KDANMORS (INDX)                     
067700     MOVE LEV-IDARTNR          TO MOD-IDARTNR  (INDX)                     
067800     MOVE LEV-IDRADNR          TO MOD-IDRADNR  (INDX)                     
067900                                                                          
068000     PERFORM FBA-FIXA-ART-UPPGIFTER                                       
068100     .                                                                    
068200     EJECT                                                                
068300                                                                          
068400 FBA-FIXA-ART-UPPGIFTER  SECTION.                                         
068500                                                                          
068600     MOVE LEV-IDARTNR          TO W-IDARTNR                               
068700                                                                          
068800     PERFORM IMS-GU-WLBENA11                                              
068900     IF SEGMENT-FINNS                                                     
069000        MOVE TEXT-BEART        TO MOD-BEART (INDX)                        
069100     ELSE                                                                 
069200        MOVE MFS-RENSA-FAELT   TO MOD-BEART (INDX)                        
069300     END-IF                                                               
069400                                                                          
069500     IF CDC-SE                                                            
069600       PERFORM IMS-GU-WLARTC11                                            
069700       IF SEGMENT-FINNS                                                   
069800         MOVE CLAG-ADLAGOMR         TO MOD-ADLAGOMR (INDX)                
069900         MOVE CLAG-ADGANG           TO WS-ADGANG                          
070000         MOVE WS-ADGANG             TO MOD-ADGANG   (INDX)                
070100         MOVE CLAG-ADPLATS          TO MOD-ADPLATS  (INDX)                
070200       ELSE                                                               
070300         MOVE MFS-RENSA-FAELT       TO MOD-ADLAGOMR (INDX)                
070400                                       MOD-ADGANG   (INDX)                
070500                                       MOD-ADPLATS  (INDX)                
070600       END-IF                                                             
070700     ELSE                                                                 
070800       PERFORM IMS-GU-WLARTS11                                            
070900       IF SEGMENT-FINNS                                                   
071000         MOVE SLAG-ADLAGOMR         TO MOD-ADLAGOMR (INDX)                
071100         MOVE SLAG-ADGANG           TO WS-ADGANG                          
071200         MOVE WS-ADGANG             TO MOD-ADGANG   (INDX)                
071300         MOVE SLAG-ADPLATS          TO MOD-ADPLATS  (INDX)                
071400       ELSE                                                               
071500         MOVE MFS-RENSA-FAELT       TO MOD-ADLAGOMR (INDX)                
071600                                       MOD-ADGANG   (INDX)                
071700                                       MOD-ADPLATS  (INDX)                
071800       END-IF                                                             
071900     END-IF                                                               
072000     .                                                                    
072100     EJECT                                                                
072200                                                                          
072300                                                                          
072400 FC-RENSA-RAD SECTION.                                                    
072500                                                                          
072600     MOVE MFS-STAENG-FAELT TO MOD-KDCMDVAL-ATTR (INDX)                    
072700                              MOD-KVANTAL-ATTR  (INDX)                    
072800                                                                          
072900     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR      (INDX)                      
073000                             MOD-BEART        (INDX)                      
073100                             MOD-KVANTAL-KVAR (INDX)                      
073200                             MOD-KDANMORS     (INDX)                      
073300                             MOD-ADLAGOMR     (INDX)                      
073400                             MOD-ADGANG       (INDX)                      
073500                             MOD-ADPLATS      (INDX)                      
073600                             MOD-IDRADNR      (INDX)                      
073700     .                                                                    
073800     EJECT                                                                
073900                                                                          
074000 FD-FIXA-NEXT-KEY        SECTION.                                         
074100                                                                          
074200     IF SEGMENT-FINNS                                                     
074300        MOVE INF-MORE-INFO-EXISTS   TO MED-IDMFSINF                       
074400        CALL WMEDKONV USING MED-WMEDAREA                                  
074500        MOVE MED-TEMFSINF           TO MOD-TEMFSINF                       
074600                                                                          
074700        MOVE SEQE-IDDC-RET          TO W-MINKEY-IDDC-NEXT                 
074800        MOVE SEQE-IDILIST           TO W-MINKEY-IDILIST-NEXT              
074900        MOVE SEQE-ADLAGOMR          TO W-MINKEY-ADLAGOMR-NEXT             
075000        MOVE SEQE-ADGANG            TO W-MINKEY-ADGANG-NEXT               
075100        MOVE SEQE-ADPLATS           TO W-MINKEY-ADPLATS-NEXT              
075200        MOVE SEQE-IDDISTR           TO W-MINKEY-IDDISTR-NEXT              
075300        MOVE SEQE-IDKUNDNR          TO W-MINKEY-IDKUNDNR-NEXT             
075400        MOVE SEQE-IDRAPPNR          TO W-MINKEY-IDRAPPNR-NEXT             
075500        MOVE SEQE-IDDISTR           TO W-MINKEY-IDDISTR-NEXT              
075600        MOVE SEQE-IDARTNR           TO W-MINKEY-IDARTNR-NEXT              
075700        MOVE SEQE-IDRADNR           TO W-MINKEY-IDRADNR-NEXT              
075800     ELSE                                                                 
075900        MOVE MSGI-IDDC              TO W-MINKEY-IDDC-NEXT                 
076000        MOVE MSGI-IDILIST           TO W-MINKEY-IDILIST-NEXT              
076100        MOVE ZERO                   TO W-MINKEY-ADLAGOMR-NEXT             
076200                                       W-MINKEY-ADGANG-NEXT               
076300                                       W-MINKEY-ADPLATS-NEXT              
076400                                       W-MINKEY-IDDISTR-NEXT              
076500                                       W-MINKEY-IDKUNDNR-NEXT             
076600                                       W-MINKEY-IDRAPPNR-NEXT             
076700                                       W-MINKEY-IDDISTR-NEXT              
076800                                       W-MINKEY-IDARTNR-NEXT              
076900                                       W-MINKEY-IDRADNR-NEXT              
077000     END-IF                                                               
077100     MOVE '4738'                    TO W-MINKEY-IDTRANS                   
077200     MOVE W-MINKEY-X                TO MSGI-SPAR-AREA                     
077300                                                                          
077400     .                                                                    
077500     EJECT                                                                
077600 G-KOLLA-INPUT SECTION.                                                   
077700                                                                          
077800     MOVE JA                      TO INDATA-SW                            
077900     MOVE ZERO                    TO MED-IDMFSFEL                         
078000                                                                          
078100     MOVE MSGI-IDILIST         TO W-IDILIST-ESEQ-MIN                      
078200                                  W-IDILIST-ESEQ-MAX                      
078300                                                                          
078400     PERFORM IMS-GHU-SEQE-WLKREE11                                        
078500     IF SEGMENT-FINNS                                                     
078600       PERFORM GA-FORMELL-KONTROLL                                        
078700       IF INDATA-OK AND (MFS-UPDATE OR MFS-PRINT)                         
078800          PERFORM GB-LOGISK-KONTROLL                                      
078900       END-IF                                                             
079000                                                                          
079100       IF INDATA-FEL                                                      
079200          IF MED-IDMFSFEL               = ZERO                            
079300             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
079400          END-IF                                                          
079500          CALL WMEDKONV USING MED-WMEDAREA                                
079600          MOVE MED-MFSFEL              TO MOD-TEMFSFEL                    
079700          PERFORM MFS-ROER-EJ-FAELT-UT                                    
079800          PERFORM MFS-ROER-EJ-FAELT-IN                                    
079900       END-IF                                                             
080000     ELSE                                                                 
080100       MOVE NEJ                  TO INDATA-SW                             
080200       MOVE ERR-INFO-MISSING     TO MED-IDMFSFEL                          
080300       CALL WMEDKONV USING MED-WMEDAREA                                   
080400       MOVE MED-MFSFEL              TO MOD-TEMFSFEL                       
080500       PERFORM MFS-RENSA-FAELT-UT                                         
080600       PERFORM MFS-RENSA-FAELT-IN                                         
080700     END-IF                                                               
080800                                                                          
080900     .                                                                    
081000     EJECT                                                                
081100 GA-FORMELL-KONTROLL SECTION.                                             
081200                                                                          
081300     IF MID-INPUT                = ALL '+' AND MFS-UPDATE                 
081400       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
081500       CALL WMEDKONV USING MED-WMEDAREA                                   
081600       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
081700       PERFORM MFS-ROER-EJ-FAELT-IN                                       
081800       PERFORM MFS-ROER-EJ-FAELT-UT                                       
081900       MOVE NEJ                  TO INDATA-SW                             
082000     ELSE                                                                 
082100       PERFORM GAA-KOLLA-IDPRT                                            
082200       PERFORM GAB-KOLLA-IDANSTNR                                         
082300       PERFORM GAC-KOLLA-RADINFO                                          
082400       PERFORM GAD-KOLLA-FLKLAR                                           
082500       PERFORM GAE-KOLLA-FLMAK                                            
082600     END-IF                                                               
082700                                                                          
082800     .                                                                    
082900     EJECT                                                                
083000                                                                          
083100 GAA-KOLLA-IDPRT      SECTION.                                            
083200                                                                          
083300     MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPRT-ATTR                          
083400                                                                          
083500     .                                                                    
083600     EJECT                                                                
083700                                                                          
083800 GAB-KOLLA-IDANSTNR   SECTION.                                            
083900                                                                          
084000     MOVE NEJ                        TO SW-IDANSTNR                       
084100     IF MID-IDANSTNR                 NOT = ALL '+'                        
084200        IF MID-IDANSTNR NUMERIC                                           
084300           MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDANSTNR-ATTR                 
084400           MOVE JA                   TO SW-IDANSTNR                       
084500        ELSE                                                              
084600           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDANSTNR-ATTR                 
084700           MOVE NEJ                  TO INDATA-SW                         
084800        END-IF                                                            
084900     ELSE                                                                 
085000        IF MID-FLMAK = JA OR YES                                          
085100          CONTINUE                                                        
085200        ELSE                                                              
085300          MOVE MFS-NUM-FAELT-FEL    TO MOD-IDANSTNR-ATTR                  
085400          MOVE NEJ                  TO INDATA-SW                          
085500        END-IF                                                            
085600     END-IF                                                               
085700                                                                          
085800     .                                                                    
085900     EJECT                                                                
086000                                                                          
086100 GAC-KOLLA-RADINFO    SECTION.                                            
086200                                                                          
086300     MOVE +1                           TO INDX                            
086400                                                                          
086500     PERFORM UNTIL INDX                 >  MAX-INDX                       
086600        IF MID-KDCMDVAL(INDX)           NOT = ALL '+'                     
086700                                                                          
086800           MOVE JA                      TO SW-RAD-INPUT                   
086900                                                                          
087000           MOVE MID-KDCMDVAL(INDX)      TO TEST-KDCMDVAL                  
087100           IF GODK-KDCMDVAL                                               
087200              MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMDVAL-ATTR(INDX)        
087300           ELSE                                                           
087400              MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMDVAL-ATTR(INDX)        
087500              MOVE NEJ                  TO INDATA-SW                      
087600           END-IF                                                         
087700                                                                          
087800           IF MID-KVANTAL(INDX)           NOT = ALL '+'                   
087900              IF MID-KVANTAL(INDX)        NUMERIC                         
088000                MOVE MFS-NUM-FAELT-RAETT  TO                              
088100                                          MOD-KVANTAL-ATTR(INDX)          
088200              ELSE                                                        
088300                MOVE MFS-NUM-FAELT-FEL    TO                              
088400                                          MOD-KVANTAL-ATTR(INDX)          
088500                MOVE NEJ                  TO INDATA-SW                    
088600              END-IF                                                      
088700           END-IF                                                         
088800        ELSE                                                              
088900           IF MID-KVANTAL(INDX)           NOT = ALL '+'                   
089000             MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMDVAL-ATTR(INDX)         
089100             MOVE MFS-NUM-FAELT-FEL    TO MOD-KVANTAL-ATTR(INDX)          
089200             MOVE NEJ                  TO INDATA-SW                       
089300             MOVE JA                   TO SW-RAD-INPUT                    
089400           ELSE                                                           
089500             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMDVAL-ATTR(INDX)         
089600             MOVE MFS-NUM-FAELT-RAETT  TO MOD-KVANTAL-ATTR(INDX)          
089700           END-IF                                                         
089800        END-IF                                                            
089900        ADD +1                          TO INDX                           
090000     END-PERFORM                                                          
090100     .                                                                    
090200     EJECT                                                                
090300                                                                          
090400 GAD-KOLLA-FLKLAR     SECTION.                                            
090500                                                                          
090600     MOVE NEJ                         TO SW-ALLT-INLAGT                   
090700                                                                          
090800     IF MID-FLKLAR                    NOT = ALL '+'                       
090900        IF MID-FLKLAR                 =  JA OR NEJ OR YES                 
091000           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-FLKLAR-ATTR                  
091100           IF MID-FLKLAR              =  JA OR YES                        
091200               MOVE JA                TO SW-ALLT-INLAGT                   
091300           END-IF                                                         
091400        ELSE                                                              
091500           MOVE MFS-ALFA-FAELT-FEL    TO MOD-FLKLAR-ATTR                  
091600        END-IF                                                            
091700     ELSE                                                                 
091800        MOVE MFS-ALFA-FAELT-RAETT  TO MOD-FLKLAR-ATTR                     
091900     END-IF                                                               
092000                                                                          
092100     .                                                                    
092200     EJECT                                                                
092300                                                                          
092400 GAE-KOLLA-FLMAK      SECTION.                                            
092500                                                                          
092600     MOVE NEJ                         TO SW-MAKULERA-ILISTA               
092700                                                                          
092800     IF MID-FLMAK                     NOT = ALL '+'                       
092900        IF MID-FLMAK                  =  JA OR NEJ OR YES                 
093000           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-FLMAK-ATTR                   
093100           IF MID-FLMAK               =  JA OR YES                        
093200               MOVE JA                TO SW-MAKULERA-ILISTA               
093300           END-IF                                                         
093400        ELSE                                                              
093500           MOVE MFS-ALFA-FAELT-FEL    TO MOD-FLMAK-ATTR                   
093600        END-IF                                                            
093700     ELSE                                                                 
093800        MOVE MFS-ALFA-FAELT-RAETT  TO MOD-FLMAK-ATTR                      
093900     END-IF                                                               
094000                                                                          
094100     .                                                                    
094200     EJECT                                                                
094300                                                                          
094400 GB-LOGISK-KONTROLL SECTION.                                              
094500                                                                          
094600     IF MFS-PRINT                                                         
094700        PERFORM GBA-KOLLA-IDPRT                                           
094800        IF ALLT-INLAGT OR MAKULERA-ILISTA OR                              
094900          RAD-INPUT                                                       
095000          MOVE NEJ     TO INDATA-SW                                       
095100          MOVE ERR-WRONG-COMBINATION TO MED-IDMFSFEL                      
095200        END-IF                                                            
095300     ELSE                                                                 
095400        PERFORM GBB-KOLLA-RADINFO                                         
095500        IF RAD-INPUT                                                      
095600           IF ALLT-INLAGT OR MAKULERA-ILISTA                              
095700              MOVE NEJ     TO INDATA-SW                                   
095800              MOVE ERR-WRONG-COMBINATION TO MED-IDMFSFEL                  
095900           END-IF                                                         
096000        ELSE                                                              
096100           IF ALLT-INLAGT AND MAKULERA-ILISTA                             
096200              MOVE NEJ     TO INDATA-SW                                   
096300              MOVE ERR-WRONG-COMBINATION TO MED-IDMFSFEL                  
096400           ELSE                                                           
096500              IF ALLT-INLAGT OR MAKULERA-ILISTA                           
096510                 IF ALLT-INLAGT                                           
096520                   PERFORM GBC-KOLLA-LAGERPLATS                           
096530                 END-IF                                                   
096700              ELSE                                                        
096800                 MOVE NEJ     TO INDATA-SW                                
096900                 MOVE INF-PRESS-PF4      TO MED-IDMFSFEL                  
097000              END-IF                                                      
097100           END-IF                                                         
097200        END-IF                                                            
097300     END-IF                                                               
097400                                                                          
097500     .                                                                    
097600     EJECT                                                                
097700                                                                          
097800                                                                          
097900 GBA-KOLLA-IDPRT                  SECTION.                                
098000                                                                          
098100     MOVE SPACE                TO PRT-IDPRTLST                            
098200     MOVE '4RT'                TO PRT-IDPRTLST(1:3)                       
098300                                                                          
098400     MOVE MID-IDPRT            TO PRT-IDPRTLST(4:3)                       
098500     MOVE 1                    TO PRT-KDCALL                              
098600     CALL W006PRT USING PRT-W006PRT                                       
098700                                                                          
098800     IF PRT-KDSVAR                     = 'F'                              
098900         MOVE ERR-WRONG-PRINTER        TO MED-IDMFSFEL                    
099000         MOVE PRT-IDPRTLST             TO MED-TEMFSINF                    
099100         MOVE NEJ                      TO INDATA-SW                       
099200         MOVE MFS-ALFA-FAELT-FEL       TO MOD-IDPRT-ATTR                  
099300     END-IF                                                               
099400     .                                                                    
099500     EJECT                                                                
099600                                                                          
099700 GBB-KOLLA-RADINFO    SECTION.                                            
099800                                                                          
099900     MOVE +1                           TO INDX                            
100000                                                                          
100100     PERFORM UNTIL INDX                >  MAX-INDX                        
100200        IF MID-KDCMDVAL(INDX)          NOT = ALL '+'                      
100300                                                                          
100400           IF IDANSTNR-IFYLLT                                             
100500              CONTINUE                                                    
100600           ELSE                                                           
100700              MOVE MFS-NUM-FAELT-FEL    TO MOD-IDANSTNR-ATTR              
100800           END-IF                                                         
100900           MOVE MSGI-IDILIST             TO W-IDILIST-E1-MIN              
101000                                            W-IDILIST-E1-MAX              
101100                                                                          
101200           MOVE MID-IDARTNR(INDX)      TO W-IDARTNR                       
101300                                          W-IDARTNR-A2                    
101400           MOVE MID-IDRADNR(INDX)      TO W-IDRADNR                       
101500                                          W-IDRADNR-A2                    
101600           PERFORM IMS-GU-WLKREJ01-KVAL                                   
101700           IF SEGMENT-FINNS                                               
101800             MOVE SEQE-IDDISTR           TO W-IDDISTR                     
101900             MOVE SEQE-IDKUNDNR          TO W-IDKUNDNR                    
102000             MOVE SEQE-IDRAPPNR          TO W-IDRAPPNR                    
102100             PERFORM IMS-GHU-WLKREE11                                     
102200                                                                          
102300             IF SEGMENT-FINNS                                             
102400                IF MID-KVANTAL (INDX) NOT = ALL '+'                       
102500                  MOVE MID-KVANTAL (INDX)  TO W-KVANTAL                   
102600                                                                          
102700*--- INLAGT ANTAL FÅR EJ VARA STÖRRE ÄN ANTALET PÅ I-LISTAN.              
102800                  IF LEV-KVANTAL-ILI   <  W-KVANTAL                       
102900                                                                          
103000                     MOVE MFS-NUM-FAELT-FEL TO                            
103100                                 MOD-KVANTAL-ATTR(INDX)                   
103200                     MOVE NEJ               TO INDATA-SW                  
103210                  ELSE                                                    
103220                     IF LEV-KVANTAL-ILI > ZERO                            
103230                       IF LEV-IDARTNR NOT = 100                           
103250                         PERFORM GBBA-KOLLA-LAGERPLATS                    
103251                       END-IF                                             
103260                     END-IF                                               
103300                  END-IF                                                  
103310                ELSE                                                      
103320                  IF LEV-KVANTAL-ILI > ZERO                               
103321                    IF LEV-IDARTNR NOT = 100                              
103330                      PERFORM GBBA-KOLLA-LAGERPLATS                       
103340                    END-IF                                                
103350                  END-IF                                                  
103400                END-IF                                                    
103500             ELSE                                                         
103600                MOVE MFS-NUM-FAELT-FEL  TO MOD-KVANTAL-ATTR(INDX)         
103700                MOVE NEJ                TO INDATA-SW                      
103800             END-IF                                                       
103900           ELSE                                                           
104000              MOVE MFS-NUM-FAELT-FEL  TO MOD-KVANTAL-ATTR(INDX)           
104100              MOVE NEJ                TO INDATA-SW                        
104200           END-IF                                                         
104300        END-IF                                                            
104400        ADD +1                          TO INDX                           
104500     END-PERFORM                                                          
104600     .                                                                    
104610                                                                          
104620 GBBA-KOLLA-LAGERPLATS  SECTION.                                          
104621     MOVE 'STA-GBBA-SECT'        TO PGM-POS                               
104622                                                                          
104640     MOVE LEV-IDARTNR          TO W-IDARTNR                               
104650                                                                          
104660     IF CDC-SE                                                            
104670       PERFORM IMS-GU-WLARTC11                                            
104694       IF SEGMENT-FINNS                                                   
104695         IF CLAG-ADLAGOMR > ZERO OR                                       
104696           CLAG-ADGANG   > ZERO OR                                        
104697           CLAG-ADPLATS  > ZERO                                           
104698           CONTINUE                                                       
104699         ELSE                                                             
104700           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMDVAL-ATTR(INDX)             
104701           MOVE ERR-LAGER-SAKNAS   TO MED-IDMFSFEL                        
104702           MOVE NEJ                TO INDATA-SW                           
104703         END-IF                                                           
104704       ELSE                                                               
104705         MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDCMDVAL-ATTR(INDX)              
104706         MOVE ERR-LAGER-SAKNAS    TO MED-IDMFSFEL                         
104707         MOVE NEJ                 TO INDATA-SW                            
104708       END-IF                                                             
104709     ELSE                                                                 
104711       PERFORM IMS-GU-WLARTS11                                            
104712       IF SEGMENT-FINNS                                                   
104713         IF SLAG-ADLAGOMR > ZERO OR                                       
104714           SLAG-ADGANG   > ZERO OR                                        
104715           SLAG-ADPLATS  > ZERO                                           
104716           CONTINUE                                                       
104717         ELSE                                                             
104718           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMDVAL-ATTR(INDX)             
104719           MOVE ERR-LAGER-SAKNAS   TO MED-IDMFSFEL                        
104720           MOVE NEJ                TO INDATA-SW                           
104721         END-IF                                                           
104722       ELSE                                                               
104723         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMDVAL-ATTR(INDX)             
104724         MOVE ERR-LAGER-SAKNAS     TO MED-IDMFSFEL                        
104725         MOVE NEJ                  TO INDATA-SW                           
104726       END-IF                                                             
104727     END-IF                                                               
104728                                                                          
104729     .                                                                    
104730     EJECT                                                                
104740                                                                          
104801 GBC-KOLLA-LAGERPLATS  SECTION.                                           
104802     MOVE 'STA-GBC-SECT'        TO PGM-POS                                
104803                                                                          
104810     PERFORM IMS-GU-WLKREJ01-OKVAL                                        
104830                                                                          
104840     IF SEGMENT-FINNS                                                     
104895       PERFORM UNTIL SEGMENT-SAKNAS OR INDATA-FEL                         
104896          MOVE SEQE-IDDISTR         TO W-IDDISTR                          
104897          MOVE SEQE-IDKUNDNR        TO W-IDKUNDNR                         
104898          MOVE SEQE-IDRAPPNR        TO W-IDRAPPNR                         
104899          MOVE SEQE-IDARTNR         TO W-IDARTNR-A2                       
104900          MOVE SEQE-IDRADNR         TO W-IDRADNR-A2                       
104901                                                                          
104902          PERFORM IMS-GHU-WLKREE11                                        
104903                                                                          
104904          IF LEV-IDARTNR NOT = 100                                        
104905                                                                          
104906*---  KOLLA ATT LAGERPLATS FINNS FÖR ARTIKEL                              
104907            MOVE LEV-IDARTNR   TO W-IDARTNR                               
104908                                                                          
104909            IF CDC-SE                                                     
104910              PERFORM IMS-GU-WLARTC11                                     
104911              IF SEGMENT-FINNS                                            
104912                IF CLAG-ADLAGOMR > ZERO OR                                
104913                  CLAG-ADGANG > ZERO OR                                   
104914                  CLAG-ADPLATS > ZERO                                     
104915                  CONTINUE                                                
104916                ELSE                                                      
104917                  MOVE ERR-LAGER-SAKNAS TO MED-IDMFSFEL                   
104918                  MOVE NEJ         TO INDATA-SW                           
104919                END-IF                                                    
104920              ELSE                                                        
104921                MOVE ERR-LAGER-SAKNAS TO MED-IDMFSFEL                     
104922                MOVE NEJ          TO INDATA-SW                            
104923              END-IF                                                      
104924            ELSE                                                          
104925              PERFORM IMS-GU-WLARTS11                                     
104926              IF SEGMENT-FINNS                                            
104927                IF SLAG-ADLAGOMR > ZERO OR                                
104928                  SLAG-ADGANG > ZERO OR                                   
104929                  SLAG-ADPLATS > ZERO                                     
104930                  CONTINUE                                                
104931                ELSE                                                      
104932                  MOVE ERR-LAGER-SAKNAS TO MED-IDMFSFEL                   
104933                  MOVE NEJ         TO INDATA-SW                           
104934                END-IF                                                    
104935              ELSE                                                        
104936                MOVE ERR-LAGER-SAKNAS TO MED-IDMFSFEL                     
104937                MOVE NEJ           TO INDATA-SW                           
104938              END-IF                                                      
104939            END-IF                                                        
104941          END-IF                                                          
104942                                                                          
104943          PERFORM IMS-GN-WLKREJ01-OKVAL                                   
104944       END-PERFORM                                                        
104945     END-IF                                                               
104946     .                                                                    
104947     EJECT                                                                
104970                                                                          
105000 H-UPPDATERA-SKRIV-UT SECTION.                                            
105100                                                                          
105200     MOVE +1                     TO 4797-INDX                             
105300                                                                          
105400     IF MFS-PRINT                                                         
105500        PERFORM HA-SKRIV-UT-ILISTA                                        
105600        MOVE INF-PRINT-BEG       TO MED-IDMFSINF                          
105700     ELSE                                                                 
105800        IF ALLT-INLAGT                                                    
105900           PERFORM HB-UPPDATERA-ALLT-INLAGT                               
106000        ELSE                                                              
106100           IF MAKULERA-ILISTA                                             
106200              PERFORM HC-MAKULERA-ILISTA                                  
106300           ELSE                                                           
106400              PERFORM HD-UPPDATERA-VALDA-RADER                            
106500           END-IF                                                         
106600        END-IF                                                            
106700                                                                          
106800        MOVE INF-UPDATE-DONE           TO MED-IDMFSINF                    
106900                                                                          
107000     END-IF                                                               
107100                                                                          
107200     IF 4797-INDX              >  +1                                      
107300        PERFORM S04-STARTA-R32-RAPPORTERING                               
107400     END-IF                                                               
107500                                                                          
107600     CALL WMEDKONV USING MED-WMEDAREA                                     
107700     MOVE MED-MFSINF             TO MOD-TEMFSINF                          
107800     PERFORM MFS-FORM-ATTR                                                
107900     PERFORM MFS-RENSA-FAELT-IN                                           
108000     .                                                                    
108100     EJECT                                                                
108200                                                                          
108300 HA-SKRIV-UT-ILISTA    SECTION.                                           
108400     MOVE 'STA-HA-SECT'        TO PGM-POS                                 
108500                                                                          
108600     MOVE +1                   TO 4795-IX                                 
108700                                                                          
108800     MOVE MSGI-IDDC            TO MOD4795-MID-IDDC                        
108900     MOVE MSGI-IDILIST         TO MOD4795-MID-IDILIST(4795-IX)            
109000                                  W-IDILIST-ESEQ-MIN                      
109100                                  W-IDILIST-ESEQ-MAX                      
109200                                                                          
109300     PERFORM IMS-GHU-SEQE-WLKREE11                                        
109400     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
109500                                                                          
109600         PERFORM HAA-KOLLA-PLATS                                          
109700                                                                          
109800         ACCEPT LEV-TIUTSKR    FROM DATE                                  
109900         PERFORM IMS-REPL-WLKREE-SEQE                                     
110000                                                                          
110100         PERFORM IMS-GHN-SEQE-WLKREE11                                    
110200                                                                          
110300     END-PERFORM                                                          
110400                                                                          
110500     PERFORM S07-UPPDATERA-WDR5-4704P                                     
110600     PERFORM HAB-STARTA-4795                                              
110700     .                                                                    
110800     EJECT                                                                
110900                                                                          
111000 HAA-KOLLA-PLATS  SECTION.                                                
111100     MOVE 'STA-HAA-SECT'        TO PGM-POS                                
111200                                                                          
111300     MOVE LEV-IDARTNR          TO W-IDARTNR                               
111400     IF CDC-SE                                                            
111500       PERFORM IMS-GU-WLARTC11                                            
111600       IF (CLAG-ADLAGOMR = LEV-ADLAGOMR    AND                            
111700          CLAG-ADGANG   = LEV-ADGANG       AND                            
111800          CLAG-ADPLATS  = LEV-ADPLATS)     OR                             
111900         (CLAG-ADLAGOMR = 22               AND                            
112000          LEV-ADLAGOMR  = 21               AND                            
112100          CLAG-ADGANG   = LEV-ADGANG       AND                            
112200          CLAG-ADPLATS  = LEV-ADPLATS)     OR                             
112300         (CLAG-ADLAGOMR = 31               AND                            
112400          LEV-ADLAGOMR  = 30               AND                            
112500          CLAG-ADGANG   = LEV-ADGANG       AND                            
112600          CLAG-ADPLATS  = LEV-ADPLATS)                                    
112700         CONTINUE                                                         
112800       ELSE                                                               
112900          MOVE CLAG-ADLAGOMR      TO LEV-ADLAGOMR                         
113000          IF LEV-ADLAGOMR = 22                                            
113100            MOVE 21               TO LEV-ADLAGOMR                         
113200          END-IF                                                          
113300          IF LEV-ADLAGOMR = 31                                            
113400            MOVE 30               TO LEV-ADLAGOMR                         
113500          END-IF                                                          
113600          MOVE CLAG-ADGANG        TO LEV-ADGANG                           
113700          MOVE CLAG-ADPLATS       TO LEV-ADPLATS                          
113800       END-IF                                                             
113900     ELSE                                                                 
114000       PERFORM IMS-GU-WLARTS11                                            
114100       IF (SLAG-ADLAGOMR = LEV-ADLAGOMR    AND                            
114200          SLAG-ADGANG   = LEV-ADGANG       AND                            
114300          SLAG-ADPLATS  = LEV-ADPLATS)     OR                             
114400         (SLAG-ADLAGOMR = 22               AND                            
114500          LEV-ADLAGOMR  = 21               AND                            
114600          SLAG-ADGANG   = LEV-ADGANG       AND                            
114700          SLAG-ADPLATS  = LEV-ADPLATS)                                    
114800         CONTINUE                                                         
114900       ELSE                                                               
115000          MOVE SLAG-ADLAGOMR      TO LEV-ADLAGOMR                         
115100          IF LEV-ADLAGOMR = 22                                            
115200            MOVE 21               TO LEV-ADLAGOMR                         
115300          END-IF                                                          
115400          MOVE SLAG-ADGANG        TO LEV-ADGANG                           
115500          MOVE SLAG-ADPLATS       TO LEV-ADPLATS                          
115600       END-IF                                                             
115700     END-IF                                                               
115800     .                                                                    
115900     EJECT                                                                
116000                                                                          
116100 HAB-STARTA-4795  SECTION.                                                
116200     MOVE 'STA-HAB-SECT'        TO PGM-POS                                
116300                                                                          
116400     MOVE MFS-KDMFSFOR          TO P-TO-P2-KDMFSFOR                       
116500                                                                          
116600     MOVE 'W40738'              TO MOD4795-MID-IDPGM                      
116700     MOVE PRT-IDPRTLST          TO MOD4795-MID-IDPRTLST                   
116800     MOVE +1                    TO MOD4795-MID-KVPOST                     
116900                                                                          
117000     COMPUTE P-TO-P2-LL = LENGTH OF MOD4795-MID  + 17                     
117100                                                                          
117200     PERFORM IMS-ISRT-MSG-ALT-4795                                        
117300     .                                                                    
117400     EJECT                                                                
117500                                                                          
117600 HB-UPPDATERA-ALLT-INLAGT            SECTION.                             
117700     MOVE 'STA-HB-SECT'        TO PGM-POS                                 
117800                                                                          
117900     MOVE ZERO                     TO W-KVRADER-BEH                       
118000                                      W-SPAR-IDDISTR                      
118100                                      W-SPAR-IDKUNDNR                     
118200                                      W-SPAR-IDRAPPNR                     
118300                                                                          
118400     MOVE MSGI-IDILIST             TO W-IDILIST-E1-MIN                    
118500                                      W-IDILIST-E1-MAX                    
118600                                                                          
118700     PERFORM IMS-GU-WLKREJ01-OKVAL                                        
118800                                                                          
118900     PERFORM UNTIL SEGMENT-SAKNAS                                         
119000                                                                          
119100       PERFORM S01-UPPDATERA-EV-WDA201                                    
119200       PERFORM S03-UPPDATERA-WDA211                                       
119300                                                                          
119400       PERFORM IMS-GN-WLKREJ01-OKVAL                                      
119500     END-PERFORM                                                          
119600                                                                          
119700     PERFORM S08-DELETE-WDR5-4704                                         
119800     PERFORM S02-UPPDATERA-WDA201                                         
119900     .                                                                    
120000     EJECT                                                                
120100                                                                          
120200 HC-MAKULERA-ILISTA         SECTION.                                      
120300     MOVE 'STA-HC-SECT'        TO PGM-POS                                 
120400                                                                          
120500     MOVE MSGI-IDILIST         TO W-IDILIST-ESEQ-MIN                      
120600                                  W-IDILIST-ESEQ-MAX                      
120700                                                                          
120800     PERFORM IMS-GHU-SEQE-WLKREE11                                        
120900     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
121000                                                                          
121100         MOVE ZERO             TO LEV-IDILIST                             
121200                                  LEV-TIUTSKR                             
121300                                  LEV-KVANTAL-ILI                         
121400                                  LEV-TIUPPDAT-ILI                        
121500         PERFORM IMS-REPL-WLKREE-SEQE                                     
121600                                                                          
121700         PERFORM IMS-GHN-SEQE-WLKREE11                                    
121800                                                                          
121900     END-PERFORM                                                          
122000                                                                          
122100     PERFORM S08-DELETE-WDR5-4704                                         
122200     .                                                                    
122300     EJECT                                                                
122400                                                                          
122500 HD-UPPDATERA-VALDA-RADER  SECTION.                                       
122600     MOVE 'STA-HD-SECT'        TO PGM-POS                                 
122700                                                                          
122800     MOVE ZERO                     TO W-KVRADER-BEH                       
122900                                      W-SPAR-IDDISTR                      
123000                                      W-SPAR-IDKUNDNR                     
123100                                      W-SPAR-IDRAPPNR                     
123200                                                                          
123300     MOVE MSGI-IDILIST             TO W-IDILIST-E1-MIN                    
123400                                      W-IDILIST-E1-MAX                    
123500                                                                          
123600     MOVE +1                       TO INDX                                
123700     PERFORM UNTIL INDX            >  MAX-INDX                            
123800                                                                          
123900        IF MID-KDCMDVAL(INDX)      = ALL '+' OR SPACE                     
124000           CONTINUE                                                       
124100        ELSE                                                              
124200                                                                          
124300           MOVE MID-IDARTNR(INDX)      TO W-IDARTNR                       
124400           MOVE MID-IDRADNR(INDX)      TO W-IDRADNR                       
124500           PERFORM IMS-GU-WLKREJ01-KVAL                                   
124600                                                                          
124700           PERFORM S01-UPPDATERA-EV-WDA201                                
124800                                                                          
124900           PERFORM S03-UPPDATERA-WDA211                                   
125010           PERFORM S06-UPPDATERA-WDR5-4704U                               
125020                                                                          
125100           MOVE LOW-VALUE          TO W-WDA2ESEQ-MIN-X                    
125200           MOVE MSGI-IDDC          TO W-IDDC-ESEQ-MIN                     
125300           MOVE MSGI-IDILIST       TO W-IDILIST-ESEQ-MIN                  
125400                                                                          
125500           MOVE HIGH-VALUE         TO W-WDA2ESEQ-MAX-X                    
125600           MOVE MSGI-IDDC          TO W-IDDC-ESEQ-MAX                     
125700           MOVE MSGI-IDILIST       TO W-IDILIST-ESEQ-MAX                  
125800                                                                          
125900           PERFORM IMS-GHU-SEQE-WLKREE11                                  
126000           IF SEGMENT-SAKNAS                                              
126300             PERFORM S08-DELETE-WDR5-4704                                 
126400           END-IF                                                         
126500                                                                          
126600        END-IF                                                            
126700                                                                          
126800        ADD +1                         TO INDX                            
126900     END-PERFORM                                                          
127000                                                                          
127100     PERFORM S02-UPPDATERA-WDA201                                         
127200                                                                          
127300     .                                                                    
127400     EJECT                                                                
127500                                                                          
127600 S01-UPPDATERA-EV-WDA201     SECTION.                                     
127700                                                                          
127800     IF SEQE-IDDISTR               =  W-SPAR-IDDISTR  AND                 
127900        SEQE-IDKUNDNR              =  W-SPAR-IDKUNDNR AND                 
128000        SEQE-IDRAPPNR              =  W-SPAR-IDRAPPNR                     
128100        CONTINUE                                                          
128200     ELSE                                                                 
128300        IF SEQE-IDDISTR            =  ZERO                                
128400           CONTINUE                                                       
128500        ELSE                                                              
128600          IF W-SPAR-IDDISTR        =  ZERO                                
128700            MOVE SEQE-IDDISTR      TO W-SPAR-IDDISTR                      
128800            MOVE SEQE-IDKUNDNR     TO W-SPAR-IDKUNDNR                     
128900            MOVE SEQE-IDRAPPNR     TO W-SPAR-IDRAPPNR                     
129000          ELSE                                                            
129100             PERFORM S02-UPPDATERA-WDA201                                 
129200                                                                          
129300             MOVE SEQE-IDDISTR       TO W-SPAR-IDDISTR                    
129400             MOVE SEQE-IDKUNDNR      TO W-SPAR-IDKUNDNR                   
129500             MOVE SEQE-IDRAPPNR      TO W-SPAR-IDRAPPNR                   
129600                                                                          
129700             MOVE ZERO               TO W-KVRADER-BEH                     
129800                                                                          
129900          END-IF                                                          
130000        END-IF                                                            
130100     END-IF                                                               
130200                                                                          
130300     .                                                                    
130400     EJECT                                                                
130500                                                                          
130600 S02-UPPDATERA-WDA201     SECTION.                                        
130700                                                                          
130800     MOVE W-SPAR-IDDISTR           TO W-IDDISTR                           
130900     MOVE W-SPAR-IDKUNDNR          TO W-IDKUNDNR                          
131000     MOVE W-SPAR-IDRAPPNR          TO W-IDRAPPNR                          
131100                                                                          
131200     PERFORM IMS-GHU-WLKREE01                                             
131300                                                                          
131400     IF ANM-KDLEVANM = W-ANM-MOT                                          
131500       MOVE W-ANM-PAAB               TO ANM-KDLEVANM                      
131600     END-IF                                                               
131700     COMPUTE ANM-KVRADER-OBEH      =  ANM-KVRADER-OBEH -                  
131800                                      W-KVRADER-BEH                       
131900                                                                          
132000     PERFORM IMS-REPL-WLKREE01                                            
132100                                                                          
132200     .                                                                    
132300     EJECT                                                                
132400                                                                          
132500 S03-UPPDATERA-WDA211     SECTION.                                        
132600                                                                          
132700     MOVE SEQE-IDDISTR             TO W-IDDISTR                           
132800     MOVE SEQE-IDKUNDNR            TO W-IDKUNDNR                          
132900     MOVE SEQE-IDRAPPNR            TO W-IDRAPPNR                          
133000     MOVE SEQE-IDARTNR             TO W-IDARTNR-A2                        
133100     MOVE SEQE-IDRADNR             TO W-IDRADNR-A2                        
133200                                                                          
133300     PERFORM IMS-GHU-WLKREE11                                             
133400                                                                          
133500     IF ALLT-INLAGT OR                                                    
133600        MID-KVANTAL(INDX)          NOT NUMERIC                            
133700                                                                          
133800        COMPUTE LEV-KVRETINL       =  LEV-KVRETINL +                      
133900                                      LEV-KVANTAL-ILI                     
134000        MOVE LEV-KVANTAL-ILI       TO W-KVRETINL-R32                      
134100     ELSE                                                                 
134200        MOVE MID-KVANTAL(INDX)     TO W-KVANTAL                           
134300                                      W-KVRETINL-R32                      
134400        COMPUTE LEV-KVRETINL       =  LEV-KVRETINL +                      
134500                                      W-KVANTAL                           
134600     END-IF                                                               
134700                                                                          
134800     IF MID-IDANSTNR NUMERIC                                              
134900       MOVE MID-IDANSTNR           TO LEV-IDANSTNR-ILIU                   
135000     END-IF                                                               
135100                                                                          
135200     COMPUTE W-KVLEVANM-KVAR       =  LEV-KVLEVANM-BEKR -                 
135300                                      LEV-KVRETINL -                      
135400                                      LEV-KVAVV-KVANT -                   
135500                                      LEV-KVRETINL-SKR -                  
135600                                      LEV-KVAVV-KVAL                      
135700                                                                          
135800     MOVE +0                       TO LEV-IDILIST                         
135900                                      LEV-TIUTSKR                         
136000                                      LEV-KVANTAL-ILI                     
136100                                      LEV-TIUPPDAT-ILI                    
136200                                                                          
136300                                                                          
136400     IF W-KVLEVANM-KVAR            =  ZERO                                
136500       ACCEPT LEV-TIINLINL FROM DATE                                      
136600       ADD +1                     TO W-KVRADER-BEH                        
136700     END-IF                                                               
136800                                                                          
136900     PERFORM IMS-REPL-WLKREE11                                            
137000                                                                          
137100     PERFORM S05-FYLL-I-R32-MID                                           
137200     .                                                                    
137300     EJECT                                                                
137400                                                                          
137500 S04-STARTA-R32-RAPPORTERING     SECTION.                                 
137600                                                                          
137700     MOVE SPACE                TO MSG-KOM-WMSGKOM                         
137800     COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM                     
137900     MOVE LOW-VALUE            TO MSG-KOM-KDZ1                            
138000     MOVE LOW-VALUE            TO MSG-KOM-KDZ2                            
138100     MOVE SPACE                TO MSG-KOM-KDTRANS                         
138200     MOVE 'W4I79701'           TO MSG-KOM-IDCPYTXT                        
138300     MOVE 'INLEVRET'           TO MSG-KOM-IDSNDNOD                        
138400     MOVE 'W4073800'           TO MSG-KOM-IDSNDJOB                        
138500     ACCEPT MSG-KOM-TIREGDAT   FROM DATE                                  
138600     ACCEPT MSG-KOM-TIKLOCK    FROM TIME                                  
138700                                                                          
138800     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
138900                                                                          
139000     COMPUTE P-TO-P1-LL        =  LNG-P-TO-P-PREFIX +                     
139100                                  LENGTH OF MOD4797-MID-W4I79701          
139200                                                                          
139300     MOVE 'W4T797X '           TO P-TO-P1-TRANSKOD                        
139400     MOVE '4738'               TO P-TO-P1-FROM-MID                        
139500     MOVE MFS-KDMFSFOR         TO P-TO-P1-KDMFSFOR                        
139600                                                                          
139700     COMPUTE MOD4797-MID-KVPOST  = 4797-INDX - 1                          
139800                                                                          
139900     MOVE KOM-AREA        TO P-TO-P1-DATA                                 
140000     CALL W006KOM         USING MSG-PCB                                   
140100                                DISP-PCB                                  
140200                                KOMA-PCB                                  
140300                                MSG-KOM-WMSGKOM                           
140400                                P-TO-P-AREA1                              
140500                                                                          
140600     .                                                                    
140700     EJECT                                                                
140800                                                                          
140900 S05-FYLL-I-R32-MID SECTION.                                              
141000                                                                          
141100     MOVE SEQE-IDDISTR      TO MOD4797-MID-IDDISTR     (4797-INDX)        
141200     MOVE SEQE-IDKUNDNR     TO MOD4797-MID-IDKUNDNR    (4797-INDX)        
141300     MOVE SEQE-IDRAPPNR     TO MOD4797-MID-IDRAPPNR    (4797-INDX)        
141400     MOVE LEV-IDARTNR       TO MOD4797-MID-IDARTNR     (4797-INDX)        
141500     MOVE LEV-IDRADNR       TO MOD4797-MID-IDRADNR     (4797-INDX)        
141600     MOVE W-KVRETINL-R32    TO MOD4797-MID-KVRETINL    (4797-INDX)        
141700     MOVE ZERO              TO MOD4797-MID-KVAVV-KVANT (4797-INDX)        
141800                               MOD4797-MID-KVRETINL-TRP(4797-INDX)        
141900                               MOD4797-MID-KVRETINL-SKR(4797-INDX)        
142000                                                                          
142100     ADD +1                    TO 4797-INDX                               
142200                                                                          
142300     IF 4797-INDX              >  4797-MAX-INDX                           
142400        PERFORM S04-STARTA-R32-RAPPORTERING                               
142500        MOVE +1                TO 4797-INDX                               
142600     END-IF                                                               
142700     .                                                                    
142800     EJECT                                                                
142900                                                                          
143000 S06-UPPDATERA-WDR5-4704U SECTION.                                        
143100                                                                          
143200     MOVE SEQE-IDDC-RET    TO W-IDDC-4703                                 
143300     MOVE SEQE-IDILIST     TO W-IDILIST-4704                              
143400                                                                          
143500     PERFORM IMS-GHU-WDR5-4704                                            
143600     MOVE LEV-IDANSTNR-ILIU TO 4704-IDANSTNR-ILIU                         
143700     ACCEPT 4704-TIUPPDAT-ILI FROM DATE                                   
143800     PERFORM IMS-REPL-WDR5-4704                                           
143900     .                                                                    
144000     EJECT                                                                
144100                                                                          
144200 S07-UPPDATERA-WDR5-4704P SECTION.                                        
144300                                                                          
144400     MOVE MSGI-IDDC        TO W-IDDC-4703                                 
144500     MOVE MSGI-IDILIST     TO W-IDILIST-4704                              
144600                                                                          
144700     PERFORM IMS-GHU-WDR5-4704                                            
144800     MOVE MID-IDANSTNR  TO 4704-IDANSTNR-ILIP                             
144900     ACCEPT 4704-TIUTSKR FROM DATE                                        
145000     PERFORM IMS-REPL-WDR5-4704                                           
145100     .                                                                    
145200     EJECT                                                                
145300                                                                          
145400 S08-DELETE-WDR5-4704  SECTION.                                           
145500                                                                          
145600     MOVE MSGI-IDDC        TO W-IDDC-4703                                 
145700     MOVE MSGI-IDILIST     TO W-IDILIST-4704                              
145800                                                                          
145900     PERFORM IMS-GHU-WDR5-4704                                            
146000     PERFORM IMS-DLET-WDR5-4704                                           
146100     .                                                                    
146200     EJECT                                                                
146300                                                                          
146400 MFS-RENSA-FAELT-UT SECTION.                                              
146500                                                                          
146600     MOVE +1                    TO INDX                                   
146700     PERFORM UNTIL INDX         >  MAX-INDX                               
146800        PERFORM MFS-RENSA-RAD-FAELT-UT                                    
146900        ADD +1                  TO INDX                                   
147000     END-PERFORM                                                          
147100     .                                                                    
147200     SKIP3                                                                
147300 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
147400                                                                          
147500     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR      (INDX)                      
147600                             MOD-BEART        (INDX)                      
147700                             MOD-KVANTAL-KVAR (INDX)                      
147800                             MOD-KDANMORS     (INDX)                      
147900                             MOD-ADLAGOMR     (INDX)                      
148000                             MOD-ADGANG       (INDX)                      
148100                             MOD-ADPLATS      (INDX)                      
148200                             MOD-IDRADNR      (INDX)                      
148300     .                                                                    
148400     SKIP3                                                                
148500 MFS-RENSA-FAELT-IN SECTION.                                              
148600                                                                          
148700*    --- ALLA INDATA-FÄLT (UTOM MOD-IDPRT )                               
148800     MOVE MFS-RENSA-FAELT       TO MOD-FLMAK                              
148900                                   MOD-FLKLAR                             
149000                                   MOD-IDANSTNR                           
149100                                                                          
149200     MOVE +1 TO INDX                                                      
149300     PERFORM UNTIL INDX         >  MAX-INDX                               
149400       MOVE MFS-RENSA-FAELT     TO MOD-KDCMDVAL (INDX)                    
149500                                   MOD-KVANTAL  (INDX)                    
149600       ADD +1                   TO INDX                                   
149700     END-PERFORM                                                          
149800     .                                                                    
149900     EJECT                                                                
150000 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
150100                                                                          
150200     MOVE +1                  TO INDX                                     
150300     PERFORM UNTIL INDX       >  MAX-INDX                                 
150400       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
150500       ADD +1                 TO INDX                                     
150600     END-PERFORM                                                          
150700     .                                                                    
150800                                                                          
150900 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
151000                                                                          
151100     MOVE MFS-ROER-EJ-FAELT  TO MOD-IDARTNR      (INDX)                   
151200                                MOD-BEART        (INDX)                   
151300                                MOD-KVANTAL-KVAR (INDX)                   
151400                                MOD-KDANMORS   (INDX)                     
151500                                MOD-ADLAGOMR   (INDX)                     
151600                                MOD-ADGANG     (INDX)                     
151700                                MOD-ADPLATS    (INDX)                     
151800                                MOD-IDRADNR    (INDX)                     
151900     .                                                                    
152000                                                                          
152100 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
152200                                                                          
152300*    --- ALLA INDATA-FÄLT  (UTOM MOD-IDPRT)                               
152400     MOVE MFS-ROER-EJ-FAELT     TO MOD-FLMAK                              
152500                                   MOD-IDANSTNR                           
152600                                   MOD-FLKLAR                             
152700                                                                          
152800     MOVE +1 TO INDX                                                      
152900     PERFORM UNTIL INDX         >  MAX-INDX                               
153000       MOVE MFS-ROER-EJ-FAELT   TO MOD-KDCMDVAL(INDX)                     
153100                                   MOD-KVANTAL(INDX)                      
153200       ADD +1                   TO INDX                                   
153300     END-PERFORM                                                          
153400     .                                                                    
153500     EJECT                                                                
153600 MFS-FORM-ATTR SECTION.                                                   
153700                                                                          
153800*    --- ALLA INDATA-FÄLT                                                 
153900     MOVE MFS-FORMATETS-ATTR    TO MOD-IDPRT-ATTR                         
154000                                   MOD-IDANSTNR-ATTR                      
154100                                   MOD-FLKLAR-ATTR                        
154200                                   MOD-FLMAK-ATTR                         
154300                                                                          
154400     MOVE +1 TO INDX                                                      
154500     PERFORM UNTIL INDX         >  MAX-INDX                               
154600       MOVE MFS-FORMATETS-ATTR  TO MOD-KDCMDVAL-ATTR(INDX)                
154700                                   MOD-KVANTAL-ATTR(INDX)                 
154800       ADD +1                   TO INDX                                   
154900     END-PERFORM                                                          
155000     .                                                                    
155100     EJECT                                                                
155200                                                                          
155300* --- IMS SEKTIONER ---                                                   
155400     SKIP3                                                                
155500 IMS-GET-MSG SECTION.                                                     
155600                                                                          
155700     MOVE '  QC' TO GODK-STATUSKODER                                      
155800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
155900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
156000     PERFORM IMS-STATUSKONTROLL                                           
156100     .                                                                    
156200     SKIP3                                                                
156300 IMS-INSERT-MSG SECTION.                                                  
156400                                                                          
156500     IF MSGI-IDLAND-SPR = 'GB'                                            
156600       MOVE 'N' TO MFS-KDHUVOMR                                           
156700     END-IF                                                               
156800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
156900     MOVE SPACE TO GODK-STATUSKODER                                       
157000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
157100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
157200     PERFORM IMS-STATUSKONTROLL                                           
157300     .                                                                    
157400     EJECT                                                                
157500                                                                          
157600 IMS-ISRT-MSG-ALT-4795 SECTION.                                           
157700                                                                          
157800     MOVE SPACE              TO GODK-STATUSKODER                          
157900     CALL CBLTDLI USING      ISRT W4795-PCB                               
158000                                  P-TO-P-T95                              
158100     MOVE W4795-STATUS-CODE   TO STATUS-WS                                
158200     PERFORM IMS-STATUSKONTROLL                                           
158300     .                                                                    
158400     SKIP3                                                                
158500 IMS-GHU-WLKREE01       SECTION.                                          
158600                                                                          
158700     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
158800          DELIMITED BY SIZE INTO SSA1                                     
158900     MOVE '  GE'           TO GODK-STATUSKODER                            
159000     CALL CBLTDLI USING GHU KREE1-PCB DLI-IO-AREA2 SSA1                   
159100     MOVE KREE1-STATUS-CODE TO STATUS-WS                                  
159200     PERFORM IMS-STATUSKONTROLL                                           
159300     .                                                                    
159400                                                                          
159500 IMS-REPL-WLKREE01      SECTION.                                          
159600                                                                          
159700     MOVE '    '           TO GODK-STATUSKODER                            
159800     CALL CBLTDLI USING REPL KREE1-PCB DLI-IO-AREA2                       
159900     MOVE KREE1-STATUS-CODE TO STATUS-WS                                  
160000     PERFORM IMS-STATUSKONTROLL                                           
160100     .                                                                    
160200     EJECT                                                                
160300                                                                          
160400 IMS-GHU-WLKREE11       SECTION.                                          
160500                                                                          
160600     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
160700          DELIMITED BY SIZE INTO SSA1                                     
160800     STRING 'WLKREE11(WDA211KY =' W-WDA211KY-X ')'                        
160900          DELIMITED BY SIZE INTO SSA2                                     
161000     MOVE '  '           TO GODK-STATUSKODER                              
161100     CALL CBLTDLI USING GHU KREE1-PCB DLI-IO-AREA2 SSA1 SSA2              
161200     MOVE KREE1-STATUS-CODE TO STATUS-WS                                  
161300     PERFORM IMS-STATUSKONTROLL                                           
161400     .                                                                    
161500                                                                          
161600 IMS-REPL-WLKREE11      SECTION.                                          
161700                                                                          
161800     MOVE '    '           TO GODK-STATUSKODER                            
161900     CALL CBLTDLI USING REPL KREE1-PCB DLI-IO-AREA2                       
162000     MOVE KREE1-STATUS-CODE TO STATUS-WS                                  
162100     PERFORM IMS-STATUSKONTROLL                                           
162200     .                                                                    
162300     EJECT                                                                
162400                                                                          
163600 IMS-GHU-SEQE-WLKREE11       SECTION.                                     
163610                                                                          
163620     STRING 'WLKREE11(WDA2ESEQ>=' W-WDA2ESEQ-MIN-X                        
163630                    '&WDA2ESEQ<=' W-WDA2ESEQ-MAX-X ')'                    
163640          DELIMITED BY SIZE INTO SSA1                                     
163650     MOVE '  GE'           TO GODK-STATUSKODER                            
163660     CALL CBLTDLI USING GHU KREE2-PCB DLI-IO-AREA2 SSA1                   
163670     MOVE KREE2-STATUS-CODE TO STATUS-WS                                  
163680     PERFORM IMS-STATUSKONTROLL                                           
163690     .                                                                    
163691                                                                          
163700 IMS-GHN-SEQE-WLKREE11       SECTION.                                     
163800                                                                          
163900     STRING 'WLKREE11(WDA2ESEQ>=' W-WDA2ESEQ-MIN-X                        
164000                    '&WDA2ESEQ<=' W-WDA2ESEQ-MAX-X ')'                    
164100          DELIMITED BY SIZE INTO SSA1                                     
164200     MOVE '  GEGB'           TO GODK-STATUSKODER                          
164300     CALL CBLTDLI USING GHN KREE2-PCB DLI-IO-AREA2 SSA1                   
164400     MOVE KREE2-STATUS-CODE TO STATUS-WS                                  
164500     PERFORM IMS-STATUSKONTROLL                                           
164600     .                                                                    
164700                                                                          
164800 IMS-REPL-WLKREE-SEQE   SECTION.                                          
164900                                                                          
165000     MOVE '    '           TO GODK-STATUSKODER                            
165100     CALL CBLTDLI USING REPL KREE2-PCB DLI-IO-AREA2                       
165200     MOVE KREE2-STATUS-CODE TO STATUS-WS                                  
165300     PERFORM IMS-STATUSKONTROLL                                           
165400     .                                                                    
165500     EJECT                                                                
165600                                                                          
165700 IMS-GU-WLKREJ01-KVAL SECTION.                                            
165800                                                                          
165900     STRING 'WLKREJ01(WDA2E1KY>=' W-WDA2E1KY-MIN-X                        
166000                    '&WDA2E1KY<=' W-WDA2E1KY-MAX-X                        
166100                    '&IDARTNR  =' W-IDARTNR-X                             
166200                    '&IDRADNR  =' W-IDRADNR-X ')'                         
166300          DELIMITED BY SIZE INTO SSA1                                     
166400     MOVE '  GE'           TO GODK-STATUSKODER                            
166500     CALL CBLTDLI USING GU KREJ-PCB DLI-IO-AREA SSA1                      
166600     MOVE KREJ-STATUS-CODE TO STATUS-WS                                   
166700     PERFORM IMS-STATUSKONTROLL                                           
166800     .                                                                    
166900                                                                          
167000 IMS-GU-WLKREJ01-OKVAL SECTION.                                           
167100                                                                          
167200     STRING 'WLKREJ01(WDA2E1KY>=' W-WDA2E1KY-MIN-X                        
167300                    '&WDA2E1KY<=' W-WDA2E1KY-MAX-X ')'                    
167400          DELIMITED BY SIZE INTO SSA1                                     
167500     MOVE '  GE'           TO GODK-STATUSKODER                            
167600     CALL CBLTDLI USING GU KREJ-PCB DLI-IO-AREA SSA1                      
167700     MOVE KREJ-STATUS-CODE TO STATUS-WS                                   
167800     PERFORM IMS-STATUSKONTROLL                                           
167900     .                                                                    
168000                                                                          
168100 IMS-GN-WLKREJ01-OKVAL  SECTION.                                          
168200                                                                          
168300     STRING 'WLKREJ01(WDA2E1KY>=' W-WDA2E1KY-MIN-X                        
168400                    '&WDA2E1KY<=' W-WDA2E1KY-MAX-X ')'                    
168500          DELIMITED BY SIZE INTO SSA1                                     
168600     MOVE '  GE'           TO GODK-STATUSKODER                            
168700     CALL CBLTDLI USING GN KREJ-PCB DLI-IO-AREA SSA1                      
168800     MOVE KREJ-STATUS-CODE TO STATUS-WS                                   
168900     PERFORM IMS-STATUSKONTROLL                                           
169000     .                                                                    
169100                                                                          
169200 IMS-GU-WLBENA11     SECTION.                                             
169300                                                                          
169400     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
169500            DELIMITED BY SIZE INTO SSA1                                   
169600     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
169700            DELIMITED BY SIZE INTO SSA2                                   
169800     MOVE '  ' TO GODK-STATUSKODER                                        
169900     CALL CBLTDLI USING GU  BENA-PCB DLI-IO-AREA2 SSA1 SSA2               
170000     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
170100     PERFORM IMS-STATUSKONTROLL                                           
170200     .                                                                    
170300     EJECT                                                                
170400 IMS-GU-WLARTC11     SECTION.                                             
170500                                                                          
170600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
170700            DELIMITED BY SIZE INTO SSA1                                   
170800     MOVE 'WLARTC11 '    TO SSA2                                          
170900     MOVE '  ' TO GODK-STATUSKODER                                        
171000     CALL CBLTDLI USING GU  ARTC-PCB DLI-IO-K611 SSA1 SSA2                
171100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
171200     PERFORM IMS-STATUSKONTROLL                                           
171300     .                                                                    
171400     EJECT                                                                
171500 IMS-GU-WLARTS11     SECTION.                                             
171600                                                                          
171700     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
171800            DELIMITED BY SIZE INTO SSA1                                   
171900     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
172000            DELIMITED BY SIZE INTO SSA2                                   
172100     MOVE '  ' TO GODK-STATUSKODER                                        
172200     CALL CBLTDLI USING GU  ARTS-PCB DLI-IO-K711 SSA1 SSA2                
172300     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
172400     PERFORM IMS-STATUSKONTROLL                                           
172500     .                                                                    
172600     EJECT                                                                
172700 IMS-GHU-WDR5-4704 SECTION.                                               
172800                                                                          
172900     STRING 'WDR501  (WDGXKEY >=' W-WDGXKEY-X ')'                         
173000          DELIMITED BY SIZE INTO SSA1                                     
173100     STRING 'WDGX4704(IDILIST  =' W-IDILIST-4704-X ')'                    
173200          DELIMITED BY SIZE INTO SSA2                                     
173300     MOVE '    ' TO GODK-STATUSKODER                                      
173400     CALL CBLTDLI USING GHU WDR5-PCB DLI-IO-WDGX4704 SSA1 SSA2            
173500     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
173600     PERFORM IMS-STATUSKONTROLL                                           
173700     .                                                                    
173800     EJECT                                                                
173900 IMS-DLET-WDR5-4704 SECTION.                                              
174000                                                                          
174100     MOVE '    '           TO GODK-STATUSKODER                            
174200     CALL CBLTDLI USING DLET WDR5-PCB DLI-IO-WDGX4704                     
174300     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
174400     PERFORM IMS-STATUSKONTROLL                                           
174500     .                                                                    
174600     EJECT                                                                
174700 IMS-REPL-WDR5-4704 SECTION.                                              
174800                                                                          
174900     MOVE '    '           TO GODK-STATUSKODER                            
175000     CALL CBLTDLI USING REPL WDR5-PCB DLI-IO-WDGX4704                     
175100     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
175200     PERFORM IMS-STATUSKONTROLL                                           
175300     .                                                                    
175400     EJECT                                                                
175500 IMS-STATUSKONTROLL SECTION.                                              
175600                                                                          
175700     SET STATUS-IX TO 1                                                   
175800     SEARCH GODK-STATUS                                                   
175900       AT END                                                             
176000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
176100         DELIMITED BY SIZE INTO FELTEXT                                   
176200         CALL FELLOG                                                      
176300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
176400         CONTINUE                                                         
176500     END-SEARCH                                                           
176600     .                                                                    
