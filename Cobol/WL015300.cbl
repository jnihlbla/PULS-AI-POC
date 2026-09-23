000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     WL015300.                                                
000400 AUTHOR.         TAPAS KUMAR GHOSH.                                       
000500 DATE-WRITTEN.   2004/09/13.                                              
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        VISA RETURTILLSTÅND.                                             
001000*        ANVÄNDS FÖR RAPPORTERING AV INLÄGGNING AV ILISTA.                
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WLKREE (WDA2)                              
001300*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001400*                                                                         
001500*    ÄNDRAT: 2004-06 E-TRACKER SCR-ID 1334504                             
001600*    RÄTTAT: 2008-02 E-TRACKER SCR-ID 6414251                             
001700*                                                                         
001800*        WL015300 PROGRAM IS A REPLICA OF W4073800 PROGRAM                
001900*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
002000*                                                                         
002100*                                                                         
002200* ADDRESS: 'CARPARTS.LDC.REPORTBINNINGLIST'                               
002300*                                                                         
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: WL0153U                                             
002700*        REQUEST:     WZ01REQ2                                            
002800*                     WL0153I1                                            
002900*                                                                         
003000*    UTDATA.                                                              
003100*        RESPONSE:    WZ01RES2                                            
003200*                     WL0153O1                                            
003300*                     WZ0155I1                                            
003400                                                                          
003500     SKIP3                                                                
003600 ENVIRONMENT DIVISION.                                                    
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(08)   VALUE 'WL015300'.            
004300                                                                          
004400*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004500 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
004600 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
004700 77  KDRC-DISPLAY                PIC Z(5).                                
004800 77  PGM-POS                     PIC X(12)   VALUE SPACE.                 
004900                                                                          
005000 77  JA                          PIC X       VALUE 'J'.                   
005100 77  YES                         PIC X       VALUE 'Y'.                   
005200 77  NEJ                         PIC X       VALUE 'N'.                   
005300 77  NOO                         PIC X       VALUE 'N'.                   
005400                                                                          
005500 77  KEYS-SW                   PIC X      VALUE 'J'.                      
005600     88  KEYS-OK                          VALUE 'J'.                      
005700     88  KEYS-WRONG                       VALUE 'N'.                      
005800                                                                          
005900*    --- INDEX FÖR BLÄDDRINGSRADER                                        
006000 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
006100 77  MSG-IX                      PIC S9(9)  VALUE +0   COMP SYNC.         
006200 77  WS-COUNT                    PIC  9(4)  VALUE  0    COMP SYNC.        
006300 77  MAX-INDX                    PIC S9(4)  VALUE +500  COMP SYNC.        
006400 77  4797-INDX                   PIC S9(4)  VALUE +0    COMP SYNC.        
006500 77  4797-MAX-INDX               PIC S9(4)  VALUE +16   COMP SYNC.        
006600 77  WS-INDX-REC                 PIC S9(4)  VALUE +0    COMP SYNC.        
006700                                                                          
006800 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
006900 77  W-ANM-AVIS                  PIC  X(1)  VALUE '4'.                    
007000 77  W-ANM-MOT                   PIC  X(1)  VALUE '5'.                    
007100 77  W-ANM-PAAB                  PIC  X(1)  VALUE '6'.                    
007200 77  W-KVLEVANM-KVAR             PIC S9(7)   VALUE 0   COMP-3.            
007300 77  W-KVRETINL-R32              PIC S9(6)   VALUE 0.                     
007400 77  W-KVRADER-BEH               PIC S9(3)   VALUE 0   COMP-3.            
007500 77  W-KVANTAL                   PIC S9(6)   VALUE 0.                     
007600 77  W-SPAR-IDDISTR              PIC S9(5)   VALUE 0   COMP-3.            
007700 77  W-SPAR-IDKUNDNR             PIC S9(7)   VALUE 0   COMP-3.            
007800 77  W-SPAR-IDRAPPNR             PIC  9(7)   VALUE 0   COMP-3.            
007900 77  WS-ADGANG                   PIC  9(2)   VALUE 0.                     
008000 77  WS-IDSKYLT-CN               PIC X(3)  VALUE 'RCN'.                   
008100 77  WS-IDSKYLT-GB               PIC X(3)  VALUE 'GB '.                   
008200 77  WS-CP-UTF8                  PIC X(4)  VALUE 'UTF8'.                  
008300 77  WS-CP-278                   PIC X(3)  VALUE '278'.                   
008400                                                                          
008500 77  WS-REC-LIMIT                PIC X       VALUE 'N'.                   
008600     88  REC-LIMIT                           VALUE 'J'.                   
008700                                                                          
008800 77  SW-IDANSTNR                 PIC X       VALUE 'N'.                   
008900     88  IDANSTNR-IFYLLT                     VALUE 'J'.                   
009000                                                                          
009100 77  SW-ALLT-INLAGT              PIC X       VALUE 'N'.                   
009200     88  ALLT-INLAGT                         VALUE 'J'.                   
009300                                                                          
009400 77  SW-MAKULERA-ILISTA          PIC X       VALUE 'N'.                   
009500     88  MAKULERA-ILISTA                     VALUE 'J'.                   
009600                                                                          
009700 77  SW-RAD-INPUT                PIC X       VALUE 'N'.                   
009800     88  RAD-INPUT                           VALUE 'J'.                   
009900                                                                          
010000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
010100     88  INDATA-OK                           VALUE 'J'.                   
010200     88  INDATA-FEL                          VALUE 'N'.                   
010300                                                                          
010400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
010500     88  NYCKLAR-OK                          VALUE 'J'.                   
010600     88  NYCKLAR-FEL                         VALUE 'N'.                   
010700                                                                          
010800*    --- VALID DC CODES                                                   
010900*01  -COPY WWDC99                                                         
011000     EJECT                                                                
011100                                                                          
011200 77  WS-IDELMT-ERROR             PIC X(16).                               
011300 77  WS-IDMSG-ERROR              PIC X(03).                               
011400 77  WS-IDMSG-INFO               PIC X(03).                               
011500                                                                          
011600*    --- PARAMETERS TO ABEND                                              
011700                                                                          
011800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
012000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
012100       EJECT                                                              
012200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
012300 01  GENERELLA-SUBPROGRAM.                                                
012400     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
012500     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
012600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
012900     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
013000     03  WMSGCONV                PIC X(8)    VALUE 'WMSGCONV'.            
013100     03  WZ01AUTH                PIC X(8)    VALUE 'WZ01AUTH'.            
013200     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
013300     EJECT                                                                
013400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
013500*01 -COPY WMEDAREA                                                        
013600     SKIP3                                                                
013700 01  MESSAGE-CODES.                                                       
013800     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
013900     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
014000     03  ERR-FORBIDDEN-UPDATE    PIC X(3)    VALUE '007'.                 
014100     03  INF-PRESS-PF4           PIC X(3)    VALUE '081'.                 
014200     03  ERR-INFO-MISSING        PIC X(3)    VALUE '005'.                 
014300     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
014400     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
014500     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
014600     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
014700     03  INF-PRINT-BEG           PIC X(3)    VALUE '118'.                 
014800     03  ERR-NOTHING-PRINTED     PIC X(3)    VALUE '167'.                 
014900     03  ERR-NO-LINE-CHOSEN      PIC X(3)    VALUE '231'.                 
015000     03  ERR-WRONG-COMBINATION   PIC X(3)    VALUE '238'.                 
015100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
015200     03  ERR-WRONG-PRINTER       PIC X(3)    VALUE '772'.                 
015300     03  ERR-LAGER-SAKNAS        PIC X(3)    VALUE '706'.                 
015400     03  ERR-UNAUTHORIZED        PIC X(3)    VALUE '00A'.                 
015500     EJECT                                                                
015600*                                                                         
015700*  AREA FÖR DISPATCHEN                                                    
015800 01  P-TO-P-AREA1.                                                        
015900     03  P-TO-P1-LL              PIC S9(4)            COMP SYNC.          
016000     03  P-TO-P1-Z1              PIC  X(1)   VALUE LOW-VALUE.             
016100     03  P-TO-P1-Z2              PIC  X(1)   VALUE LOW-VALUE.             
016200     03  P-TO-P1-TRANSKOD        PIC  X(7).                               
016300     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
016400     03  P-TO-P1-FROM-MID        PIC  X(4).                               
016500     03  P-TO-P1-KDMFSFOR        PIC  X(1).                               
016600     03  P-TO-P1-DATA            PIC  X(1000).                            
016700                                                                          
016800     EJECT                                                                
016900                                                                          
017000 01  FILLER                      PIC X(16) VALUE 'WTRAUTF8-AREA'.         
017100*01  -COPY WTRAUTF8                                                       
017200     EJECT                                                                
017300*                                                                         
017400*    --- AREOR FÖR W006KOM SUBMODUL                                       
017500*                                                                         
017600 01  FILLER                      PIC X(16)   VALUE 'MSG-KOM-AREA'.        
017700*01  -COPY WMSGKOM                                                        
017800     EJECT                                                                
017900*                                                                         
018000 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
018100 01  KOM-IO-AREA.                                                         
018200   03  KOM-AREA                     PIC X(1000) VALUE SPACE.              
018300    03 R32      REDEFINES KOM-AREA.                                       
018400      05    -COPY W4I79701 -PRE MOD4797-                                  
018500     EJECT                                                                
018600*                                                                         
018700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
018800*                                                                         
018900 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
019000     SKIP3                                                                
019100*01  -COPY WZ01SUB                                                        
019200     EJECT                                                                
019300 01  FILLER                      PIC X(16)   VALUE 'WZ01AUTH'.            
019400*01  -COPY WZ01AUTH                                                       
019500     EJECT                                                                
019600 01  FILLER                      PIC X(16)   VALUE 'WMSGCONV'.            
019700*01  -COPY WMSGCONV                                                       
019800     EJECT                                                                
019900 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
020000     SKIP3                                                                
020100 01  REQU-AREA.                                                           
020200*    03  -COPY WZ01REQ2                                                   
020300*    03  -COPY WL0153I1                                                   
020400     EJECT                                                                
020500 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
020600     SKIP3                                                                
020700 01  RESP-AREA.                                                           
020800*    03  -COPY WZ01RES2                                                   
020900*    03  -COPY WL0153O1                                                   
021000     EJECT                                                                
021100     SKIP2                                                                
021200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021300*                                                                         
021400     EJECT                                                                
021500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
021600     SKIP3                                                                
021700                                                                          
021800 01  NYCKLAR-TILL-DLI.                                                    
021900                                                                          
022000     03  W-IDLEVANM-X.                                                    
022100         05  W-IDDISTR           PIC S9(5)   COMP-3 VALUE ZERO.           
022200         05  W-IDKUNDNR          PIC S9(7)   COMP-3 VALUE ZERO.           
022300         05  W-IDRAPPNR          PIC  9(7)          VALUE ZERO.           
022400                                                                          
022500     03  W-WDA211KY-X.                                                    
022600         05  W-IDARTNR-A2        PIC S9(9)   COMP-3 VALUE ZERO.           
022700         05  W-IDRADNR-A2        PIC S9(5)   COMP-3 VALUE ZERO.           
022800                                                                          
022900     03  W-WDA2E1KY-MIN-X.                                                
023000         05  W-IDDC-E1-MIN       PIC  X(2)          VALUE SPACE.          
023100         05  W-IDILIST-E1-MIN    PIC  9(5)          VALUE ZERO.           
023200         05  W-ADLAGOMR-E1-MIN   PIC S9(3)   COMP-3 VALUE ZERO.           
023300         05  W-ADGANG-E1-MIN     PIC S9(3)   COMP-3 VALUE ZERO.           
023400         05  W-ADPLATS-E1-MIN    PIC S9(5)   COMP-3 VALUE ZERO.           
023500         05  W-IDDISTR-E1-MIN    PIC S9(5)   COMP-3 VALUE ZERO.           
023600         05  W-IDKUNDNR-E1-MIN   PIC S9(7)   COMP-3 VALUE ZERO.           
023700         05  W-IDRAPPNR-E1-MIN   PIC  9(7)          VALUE ZERO.           
023800         05  W-IDARTNR-E1-MIN    PIC S9(9)   COMP-3 VALUE ZERO.           
023900         05  W-IDRADNR-E1-MIN    PIC S9(5)   COMP-3 VALUE ZERO.           
024000                                                                          
024100     03  W-WDA2E1KY-MAX-X.                                                
024200         05  W-IDDC-E1-MAX       PIC  X(2)          VALUE SPACE.          
024300         05  W-IDILIST-E1-MAX    PIC  9(5)          VALUE ZERO.           
024400         05  W-ADLAGOMR-E1-MAX   PIC S9(3)   COMP-3 VALUE ZERO.           
024500         05  W-ADGANG-E1-MAX     PIC S9(3)   COMP-3 VALUE ZERO.           
024600         05  W-ADPLATS-E1-MAX    PIC S9(5)   COMP-3 VALUE ZERO.           
024700         05  W-IDDISTR-E1-MAX    PIC S9(5)   COMP-3 VALUE ZERO.           
024800         05  W-IDKUNDNR-E1-MAX   PIC S9(7)   COMP-3 VALUE ZERO.           
024900         05  W-IDRAPPNR-E1-MAX   PIC  9(7)          VALUE ZERO.           
025000         05  W-IDARTNR-E1-MAX    PIC S9(9)   COMP-3 VALUE ZERO.           
025100         05  W-IDRADNR-E1-MAX    PIC S9(5)   COMP-3 VALUE ZERO.           
025200                                                                          
025300     03  W-WDA2ESEQ-MIN-X.                                                
025400         05  W-IDDC-ESEQ-MIN     PIC  X(2)          VALUE SPACE.          
025500         05  W-IDILIST-ESEQ-MIN  PIC  9(5)          VALUE ZERO.           
025600         05  W-ADLAGOMR-ESEQ-MIN PIC S9(3)   COMP-3 VALUE ZERO.           
025700         05  W-ADGANG-ESEQ-MIN   PIC S9(3)   COMP-3 VALUE ZERO.           
025800         05  W-ADPLATS-ESEQ-MIN  PIC S9(5)   COMP-3 VALUE ZERO.           
025900                                                                          
026000     03  W-WDA2ESEQ-MAX-X.                                                
026100         05  W-IDDC-ESEQ-MAX     PIC  X(2)          VALUE SPACE.          
026200         05  W-IDILIST-ESEQ-MAX  PIC  9(5)          VALUE ZERO.           
026300         05  W-ADLAGOMR-ESEQ-MAX PIC S9(3)   COMP-3 VALUE ZERO.           
026400         05  W-ADGANG-ESEQ-MAX   PIC S9(3)   COMP-3 VALUE ZERO.           
026500         05  W-ADPLATS-ESEQ-MAX  PIC S9(5)   COMP-3 VALUE ZERO.           
026600                                                                          
026700     03  W-IDARTNR-X.                                                     
026800         05  W-IDARTNR           PIC S9(9)   COMP-3 VALUE ZERO.           
026900                                                                          
027000     03  W-IDDC-X.                                                        
027100         05  W-IDDC              PIC  X(2)          VALUE SPACE.          
027200                                                                          
027300     03  W-IDRADNR-X.                                                     
027400         05  W-IDRADNR           PIC S9(5)   COMP-3 VALUE ZERO.           
027500                                                                          
027600     03  W-IDSKYLT-X.                                                     
027700         05  W-IDSKYLT           PIC  X(3)   VALUE SPACE.                 
027800     03  W-IDDC-B6-X.                                                     
027900         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
028000                                                                          
028100     SKIP2                                                                
028200*    --- STATUS-KOD FRÅN IMS                                              
028300 01  STATUS-WS                   PIC XX.                                  
028400     88  STATUS-OK                           VALUE '  '.                  
028500     88  SEGMENT-FINNS                       VALUE '  '.                  
028600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
028700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
028800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
028900     88  TRANSKOD-FEL                        VALUE 'A1'.                  
029000     88  SECURITY-FEL                        VALUE 'A4'.                  
029100     SKIP2                                                                
029200 01  GODK-STATUSKODER.                                                    
029300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
029400     SKIP3                                                                
029500 01  SSA1                        PIC X(192).                              
029600 01  SSA2                        PIC X(64).                               
029700     EJECT                                                                
029800*    --- IMS FUNKTIONSKODER                                               
029900*01  -COPY W0003                                                          
030000     EJECT                                                                
030100*    ---  DLI INPUT-OUTPUT AREA                                           
030200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
030300     SKIP3                                                                
030400 01  DLI-IO-AREA.                                                         
030500     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
030600     SKIP3                                                                
030700     03  WLKREJ01 REDEFINES IO-AREA.                                      
030800*        05  -COPY WDA2E1                                                 
030900     EJECT                                                                
031000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
031100     SKIP3                                                                
031200 01  DLI-IO-AREA2.                                                        
031300     03  IO-AREA2                PIC X(900)  VALUE SPACE.                 
031400     SKIP3                                                                
031500     03  WLKREE01 REDEFINES IO-AREA2.                                     
031600*        05  -COPY WDA201                                                 
031700     EJECT                                                                
031800     03  WLKREE11 REDEFINES IO-AREA2.                                     
031900*        05  -COPY WDA211                                                 
032000     EJECT                                                                
032100     03  WLBENA11 REDEFINES IO-AREA2.                                     
032200*        05  -COPY WDD311                                                 
032300     EJECT                                                                
032400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA3'.        
032500     SKIP3                                                                
032600 01  DLI-IO-AREA3.                                                        
032700     03  WLARTS11.                                                        
032800*        05  -COPY WDK711                                                 
032900                                                                          
033000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-K611'.         
033100 01  DLI-IO-K611.                                                         
033200*    03  -COPY WDK611                                                     
033300     EJECT                                                                
033400 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
033500 01  DLI-IO-AREA-B601.                                                    
033600*    03  -COPY WDB601                                                     
033700     EJECT                                                                
033800 LINKAGE SECTION.                                                         
033900                                                                          
034000 01  MSG-PCB                     PIC X.                                   
034100*01  -COPY W0009   -PRE DISP-                                             
034200     EJECT                                                                
034300 01  ATAB-PCB                    PIC X.                                   
034400     EJECT                                                                
034500*01  -COPY W0008  -PRE KREE1-                                             
034600     05  FILLER                  PIC X.                                   
034700     EJECT                                                                
034800*01  -COPY W0008  -PRE KREE2-                                             
034900     05  FILLER                  PIC X.                                   
035000     EJECT                                                                
035100*01  -COPY W0008  -PRE KREJ-                                              
035200     05  FILLER                  PIC X.                                   
035300     EJECT                                                                
035400*01  -COPY W0008  -PRE BENA-                                              
035500     05  FILLER                  PIC X.                                   
035600     EJECT                                                                
035700*01  -COPY W0008  -PRE ARTS-                                              
035800     EJECT                                                                
035900     05  FILLER                  PIC X.                                   
036000*01  -COPY W0008  -PRE KOMA-                                              
036100     05  FILLER                  PIC X.                                   
036200     EJECT                                                                
036300*01  -COPY W0008  -PRE ARTC-                                              
036400     05  FILLER                  PIC X.                                   
036500     EJECT                                                                
036600*01  -COPY W0008  -PRE WDB6-                                              
036700     05  FILLER                  PIC X.                                   
036800     EJECT                                                                
036900 PROCEDURE DIVISION  USING MSG-PCB  DISP-PCB ATAB-PCB                     
037000                           KREE1-PCB KREE2-PCB KREJ-PCB BENA-PCB          
037100                           ARTS-PCB  KOMA-PCB                             
037200                           ARTC-PCB  WDB6-PCB.                            
037300                                                                          
037400     ENTRY 'DLITCBL' USING MSG-PCB   DISP-PCB ATAB-PCB                    
037500                           KREE1-PCB KREE2-PCB KREJ-PCB BENA-PCB          
037600                           ARTS-PCB KOMA-PCB                              
037700                           ARTC-PCB WDB6-PCB.                             
037800                                                                          
037900     PERFORM S06-FETCH-REQUEST-ARGUMENT                                   
038000     IF SUB-KDRC = 0                                                      
038100       PERFORM A-INIT                                                     
038200       PERFORM B-KOLLA-NYCKLAR                                            
038300       IF NYCKLAR-OK                                                      
038400         IF REQU-KDPGMACT = 'E'                                           
038500             PERFORM G-KOLLA-INPUT                                        
038600         END-IF                                                           
038700                                                                          
038800         IF REQU-KDPGMACT = 'E'                                           
038900             IF INDATA-OK                                                 
039000                PERFORM H-UPPDATERA-SKRIV-UT                              
039100             END-IF                                                       
039200         END-IF                                                           
039300         IF INDATA-OK                                                     
039400            PERFORM F-LAES-VISA-INFO                                      
039500         END-IF                                                           
039600       END-IF                                                             
039700                                                                          
039800          MOVE RESP-IDMSG-INFO    TO WS-IDMSG-INFO                        
039900          MOVE RESP-IDMSG-ERROR   TO WS-IDMSG-ERROR                       
040000          MOVE RESP-IDELMT-ERROR  TO WS-IDELMT-ERROR                      
040100          IF WS-IDMSG-INFO NOT = SPACE                                    
040200             MOVE SPACE            TO RESP-IDMSG-ERROR                    
040300             MOVE SPACE            TO RESP-IDELMT-ERROR                   
040400          ELSE                                                            
040500            IF WS-IDMSG-ERROR NOT = SPACE                                 
040600                MOVE ALL '+' TO RESP-WL0153O1(1:15)                       
040700                MOVE WS-IDMSG-ERROR   TO RESP-IDMSG-ERROR                 
040800                MOVE WS-IDELMT-ERROR  TO RESP-IDELMT-ERROR                
040900                MOVE WS-IDMSG-INFO    TO RESP-IDMSG-INFO                  
041000                MOVE  001             TO RESP-IDRESVER                    
041100                IF  REQU-KDPGMACT = 'S'                                   
041200                   MOVE ZERO             TO RESP-KVRADER                  
041300                ELSE                                                      
041400                  IF REQU-KVRADER NUMERIC                                 
041500                    MOVE REQU-KVRADER     TO RESP-KVRADER                 
041600                  ELSE                                                    
041700                    MOVE ZERO             TO RESP-KVRADER                 
041800                  END-IF                                                  
041900                END-IF                                                    
042000            END-IF                                                        
042100          END-IF                                                          
042200          IF SUB-KDTRANS(1:6) = 'WLA153'                                  
042300            PERFORM S11-MSG-CONV                                          
042400          END-IF                                                          
042500          PERFORM S07-RETURN-RESPONSE                                     
042600     END-IF                                                               
042700                                                                          
042800     MOVE ZERO TO RETURN-CODE                                             
042900     GOBACK                                                               
043000     .                                                                    
043100     EJECT                                                                
043200 A-INIT SECTION.                                                          
043300                                                                          
043400     MOVE ALL '+' TO RESP-AREA                                            
043500     MOVE SPACE   TO RESP-IDMSG-INFO                                      
043600                     RESP-IDMSG-ERROR                                     
043700                     RESP-IDELMT-ERROR                                    
043800     MOVE 001     TO RESP-IDRESVER                                        
043900     MOVE ZERO    TO RESP-KVRADER                                         
044000     MOVE +1      TO INDX                                                 
044100     PERFORM UNTIL INDX        > MAX-INDX                                 
044200                                                                          
044300*    -- BEART SKA VARA SPACE I UNICODE                                    
044400       MOVE ALL X'20'  TO RESP-BEART(INDX)                                
044500                                                                          
044600       ADD +1               TO INDX                                       
044700     END-PERFORM                                                          
044800                                                                          
044900     MOVE LOW-VALUE             TO W-WDA2E1KY-MIN-X                       
045000                                                                          
045100     MOVE HIGH-VALUE            TO W-WDA2E1KY-MAX-X                       
045200                                   W-WDA2ESEQ-MAX-X                       
045300     IF SUB-KDTRANS(1:6) = 'WLA153'                                       
045400       MOVE 001                  TO AUTH-KDCALL                           
045500       CALL WZ01AUTH          USING AUTH-WZ01AUTH                         
045600                                    REQU-WZ01REQ2                         
045700       IF AUTH-KDRC > 0                                                   
045800         MOVE ERR-UNAUTHORIZED   TO RESP-IDMSG-ERROR                      
045900         MOVE NOO                TO KEYS-SW                               
046000       END-IF                                                             
046100       MOVE FUNCTION UPPER-CASE (REQU-IDDC-KEY) TO                        
046200                                 REQU-IDDC-KEY                            
046300       MOVE FUNCTION UPPER-CASE (REQU-FLKLAR) TO                          
046400                                 REQU-FLKLAR                              
046500       MOVE FUNCTION UPPER-CASE (REQU-FLSKRIV) TO                         
046600                                 REQU-FLSKRIV                             
046700       MOVE FUNCTION UPPER-CASE (REQU-FLMAK) TO                           
046800                                 REQU-FLMAK                               
046900       MOVE +1 TO INDX                                                    
047000       PERFORM UNTIL INDX > MAX-INDX                                      
047100        MOVE FUNCTION UPPER-CASE (REQU-FLCMD(INDX)) TO                    
047200                                  REQU-FLCMD(INDX)                        
047300        ADD +1 TO INDX                                                    
047400       END-PERFORM                                                        
047500     END-IF                                                               
047600     .                                                                    
047700     EJECT                                                                
047800 B-KOLLA-NYCKLAR SECTION.                                                 
047900                                                                          
048000     MOVE JA TO NYCKLAR-SW                                                
048100                                                                          
048200*    MOVE 'GB'                    TO W-IDSKYLT                            
048300                                                                          
048400     PERFORM BA-KOLLA-IDILIST                                             
048500     MOVE REQU-IDDC-KEY           TO W-IDDC-E1-MIN                        
048600                                     W-IDDC-E1-MAX                        
048700                                     W-IDDC-ESEQ-MIN                      
048800                                     W-IDDC-ESEQ-MAX                      
048900                                     W-IDDC                               
049000                                     RESP-IDDC-KEY                        
049100                                     WS-IDDC                              
049200                                     W-IDDC-B6                            
049300     PERFORM IMS-GU-WDB601                                                
049400                                                                          
049500     MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
049600     IF DCS-UNICODE-IDSKYLT                                               
049700        MOVE 'UTF8'             TO TRAUTF8-KDCP                           
049800     ELSE                                                                 
049900        MOVE '278 '             TO TRAUTF8-KDCP                           
050000     END-IF                                                               
050100     .                                                                    
050200     EJECT                                                                
050300                                                                          
050400 BA-KOLLA-IDILIST  SECTION.                                               
050500                                                                          
050600     IF REQU-IDILIST-KEY NUMERIC AND REQU-IDILIST-KEY > ZERO              
050700       MOVE REQU-IDILIST-KEY    TO W-IDILIST-E1-MIN                       
050800                                   W-IDILIST-ESEQ-MIN                     
050900                                   W-IDILIST-E1-MAX                       
051000                                   W-IDILIST-ESEQ-MAX                     
051100                                   RESP-IDILIST-KEY                       
051200     ELSE                                                                 
051300       MOVE NEJ                 TO NYCKLAR-SW                             
051400       IF REQU-IDILIST-KEY NOT NUMERIC                                    
051500          MOVE 'IDILIST'           TO RESP-IDELMT-ERROR                   
051600          MOVE '024'               TO RESP-IDMSG-ERROR                    
051700       ELSE                                                               
051800          MOVE 'IDILIST'           TO RESP-IDELMT-ERROR                   
051900          MOVE '126'               TO RESP-IDMSG-ERROR                    
052000       END-IF                                                             
052100     END-IF                                                               
052200                                                                          
052300     .                                                                    
052400     EJECT                                                                
052500                                                                          
052600     EJECT                                                                
052700 F-LAES-VISA-INFO SECTION.                                                
052800                                                                          
052900     PERFORM IMS-GU-WLKREJ01-OKVAL                                        
053000                                                                          
053100     IF SEGMENT-SAKNAS                                                    
053200       MOVE '027'              TO RESP-IDMSG-ERROR                        
053300     ELSE                                                                 
053400       MOVE +1                  TO INDX                                   
053500       MOVE  0                  TO WS-COUNT                               
053600                                                                          
053700       PERFORM UNTIL INDX        > MAX-INDX                               
053800         IF SEGMENT-FINNS                                                 
053900            MOVE SEQE-IDDISTR       TO W-IDDISTR                          
054000            MOVE SEQE-IDKUNDNR      TO W-IDKUNDNR                         
054100            MOVE SEQE-IDRAPPNR      TO W-IDRAPPNR                         
054200            MOVE SEQE-IDARTNR       TO W-IDARTNR-A2                       
054300            MOVE SEQE-IDRADNR       TO W-IDRADNR-A2                       
054400                                                                          
054500            PERFORM IMS-GHU-WLKREE11                                      
054600            PERFORM FB-REDIGERA-RAD-UPPGIFTER                             
054700                                                                          
054800            PERFORM IMS-GN-WLKREJ01-OKVAL                                 
054900            ADD +1               TO WS-COUNT                              
055000         END-IF                                                           
055100         ADD +1               TO INDX                                     
055200       END-PERFORM                                                        
055300       MOVE WS-COUNT          TO RESP-KVRADER                             
055400                                                                          
055500       IF WS-COUNT = 500                                                  
055600          MOVE '028'  TO RESP-IDMSG-INFO                                  
055700       END-IF                                                             
055800                                                                          
055900       IF REQU-KDPGMACT = 'S'                                             
056000          MOVE 'N'            TO RESP-FLKLAR                              
056100                                 RESP-FLMAK                               
056200                                 RESP-FLSKRIV                             
056300       ELSE                                                               
056400          MOVE REQU-FLKLAR    TO RESP-FLKLAR                              
056500          MOVE REQU-FLMAK     TO RESP-FLMAK                               
056600          MOVE REQU-FLSKRIV   TO RESP-FLSKRIV                             
056700       END-IF                                                             
056800     END-IF                                                               
056900                                                                          
057000     .                                                                    
057100     EJECT                                                                
057200                                                                          
057300 FB-REDIGERA-RAD-UPPGIFTER  SECTION.                                      
057400                                                                          
057500     MOVE LEV-KVANTAL-ILI      TO RESP-KVANTAL-KVAR (INDX)                
057600                                                                          
057700     MOVE LEV-KDANMORS         TO RESP-KDANMORS (INDX)                    
057800     MOVE LEV-IDARTNR          TO RESP-IDARTNR  (INDX)                    
057900     MOVE LEV-IDRADNR          TO RESP-IDRADNR  (INDX)                    
058000                                                                          
058100     PERFORM FBA-FIXA-ART-UPPGIFTER                                       
058200     .                                                                    
058300     EJECT                                                                
058400                                                                          
058500 FBA-FIXA-ART-UPPGIFTER  SECTION.                                         
058600                                                                          
058700     MOVE LEV-IDARTNR          TO W-IDARTNR                               
058800                                                                          
058900     PERFORM IMS-GU-WLBENA11                                              
059000     IF SEGMENT-FINNS                                                     
059100      MOVE TEXT-BEART           TO TRAUTF8-TECONV-FROM                    
059200     ELSE                                                                 
059300      MOVE SPACE TO TRAUTF8-TECONV-FROM                                   
059400     END-IF                                                               
059500     IF TRAUTF8-TECONV-FROM = SPACES                                      
059600      MOVE 'GB'  TO W-IDSKYLT                                             
059700      MOVE '278' TO TRAUTF8-KDCP                                          
059800      PERFORM IMS-GU-WLBENA11                                             
059900      MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                           
060000     END-IF                                                               
060100                                                                          
060200*    -- STRIP SPACE OR CONVERT TO UNICODE                                 
060300     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
060400                                                                          
060500*    -- MOVE CONVERTED DESCRIPTION TO THE RESPONSE                        
060600     MOVE TRAUTF8-TECONV-TO    TO RESP-BEART (INDX)                       
060700                                                                          
060800     PERFORM IMS-GU-WLARTS11                                              
060900     IF SEGMENT-FINNS                                                     
061000       MOVE SLAG-ADLAGOMR         TO RESP-ADLAGOMR (INDX)                 
061100       MOVE SLAG-ADGANG           TO WS-ADGANG                            
061200       MOVE WS-ADGANG             TO RESP-ADGANG   (INDX)                 
061300       MOVE SLAG-ADPLATS          TO RESP-ADPLATS  (INDX)                 
061400     END-IF                                                               
061500     .                                                                    
061600     EJECT                                                                
061700                                                                          
061800                                                                          
061900     EJECT                                                                
062000 G-KOLLA-INPUT SECTION.                                                   
062100                                                                          
062200     MOVE JA                      TO INDATA-SW                            
062300                                                                          
062400     MOVE REQU-IDILIST-KEY     TO W-IDILIST-ESEQ-MIN                      
062500                                  W-IDILIST-ESEQ-MAX                      
062600                                                                          
062700     PERFORM IMS-GHU-SEQE-WLKREE11                                        
062800     IF SEGMENT-FINNS                                                     
062900       PERFORM GA-FORMELL-KONTROLL                                        
063000       IF INDATA-OK AND (REQU-KDPGMACT = 'E' OR                           
063100                         REQU-FLSKRIV  = 'J'   )                          
063200          PERFORM GB-LOGISK-KONTROLL                                      
063300       END-IF                                                             
063400                                                                          
063500     ELSE                                                                 
063600       MOVE NEJ                  TO INDATA-SW                             
063700       MOVE '041'                TO RESP-IDMSG-ERROR                      
063800       MOVE 'IDRADNR'            TO RESP-IDELMT-ERROR                     
063900     END-IF                                                               
064000                                                                          
064100     .                                                                    
064200     EJECT                                                                
064300 GA-FORMELL-KONTROLL SECTION.                                             
064400                                                                          
064500     IF REQU-INPUT  = ALL '+' AND REQU-KDPGMACT = 'E'                     
064600       MOVE '192'     TO RESP-IDMSG-ERROR                                 
064700       MOVE NEJ                  TO INDATA-SW                             
064800     ELSE                                                                 
064900       PERFORM GAB-KOLLA-IDANSTNR                                         
065000       PERFORM GAC-KOLLA-RADINFO                                          
065100       PERFORM GAD-KOLLA-FLKLAR                                           
065200       PERFORM GAE-KOLLA-FLMAK                                            
065300     END-IF                                                               
065400                                                                          
065500     .                                                                    
065600     EJECT                                                                
065700                                                                          
065800     EJECT                                                                
065900                                                                          
066000 GAB-KOLLA-IDANSTNR   SECTION.                                            
066100                                                                          
066200     MOVE NEJ                        TO SW-IDANSTNR                       
066300     IF REQU-IDANSTNR                 NOT = ALL '+'                       
066400        IF REQU-IDANSTNR NUMERIC                                          
066500           MOVE JA                   TO SW-IDANSTNR                       
066600           MOVE REQU-IDANSTNR        TO RESP-IDANSTNR                     
066700        ELSE                                                              
066800           MOVE NEJ                  TO INDATA-SW                         
066900           MOVE 'IDANSTNR'           TO RESP-IDELMT-ERROR                 
067000           MOVE '024'                TO RESP-IDMSG-ERROR                  
067100        END-IF                                                            
067200     ELSE                                                                 
067300        IF REQU-FLMAK = JA OR YES                                         
067400          CONTINUE                                                        
067500        ELSE                                                              
067600          MOVE NEJ                  TO INDATA-SW                          
067700          MOVE '023'                TO RESP-IDMSG-ERROR                   
067800          MOVE 'IDANSTNR'           TO RESP-IDELMT-ERROR                  
067900        END-IF                                                            
068000     END-IF                                                               
068100                                                                          
068200     .                                                                    
068300     EJECT                                                                
068400                                                                          
068500 GAC-KOLLA-RADINFO    SECTION.                                            
068600                                                                          
068700     IF REQU-KVRADER NUMERIC AND REQU-KVRADER > 0                         
068800       MOVE +1                            TO INDX                         
068900       MOVE REQU-KVRADER                  TO WS-INDX-REC                  
069000       MOVE NEJ                           TO WS-REC-LIMIT                 
069100                                                                          
069200       PERFORM UNTIL INDX     >  MAX-INDX OR REC-LIMIT                    
069300         IF REQU-FLCMD(INDX)     NOT = '+' AND SPACE AND 'N'              
069400           MOVE JA                     TO SW-RAD-INPUT                    
069500                                                                          
069600           IF REQU-FLCMD(INDX) NOT = 'J' AND 'Y'                          
069700              MOVE NEJ                 TO INDATA-SW                       
069800              MOVE 'CMD'               TO RESP-IDELMT-ERROR               
069900              MOVE '023'               TO RESP-IDMSG-ERROR                
070000                                  RESP-IDMSG-ERROR-LINE(INDX)             
070100           END-IF                                                         
070200                                                                          
070300           IF REQU-KVANTAL(INDX)         NOT = ALL '+'                    
070400              IF REQU-KVANTAL(INDX)      NUMERIC                          
070500                 CONTINUE                                                 
070600              ELSE                                                        
070700                MOVE NEJ               TO INDATA-SW                       
070800                MOVE 'KVANTAL'         TO RESP-IDELMT-ERROR               
070900                MOVE '024'             TO RESP-IDMSG-ERROR                
071000                                     RESP-IDMSG-ERROR-LINE(INDX)          
071100              END-IF                                                      
071200           END-IF                                                         
071300         ELSE                                                             
071400           IF REQU-KVANTAL(INDX)  NOT = ALL '+' AND                       
071500              REQU-KVANTAL(INDX)  NOT = LOW-VALUES                        
071600             MOVE NEJ                  TO INDATA-SW                       
071700             MOVE 'KVANTAL'            TO RESP-IDELMT-ERROR               
071800             MOVE '033'                TO RESP-IDMSG-ERROR                
071900                                       RESP-IDMSG-ERROR-LINE(INDX)        
072000             MOVE JA                   TO SW-RAD-INPUT                    
072100           END-IF                                                         
072200         END-IF                                                           
072300         IF INDX = WS-INDX-REC                                            
072400            MOVE JA TO WS-REC-LIMIT                                       
072500         ELSE                                                             
072600            ADD +1                       TO INDX                          
072700         END-IF                                                           
072800       END-PERFORM                                                        
072900     ELSE                                                                 
073000       MOVE NEJ           TO INDATA-SW                                    
073100       IF REQU-KVRADER = 0                                                
073200          MOVE 'KVRADER' TO RESP-IDELMT-ERROR                             
073300          MOVE '126'     TO RESP-IDMSG-ERROR                              
073400       ELSE                                                               
073500          MOVE 'KVRADER' TO RESP-IDELMT-ERROR                             
073600          MOVE '024'     TO RESP-IDMSG-ERROR                              
073700       END-IF                                                             
073800     END-IF                                                               
073900     .                                                                    
074000     EJECT                                                                
074100                                                                          
074200 GAD-KOLLA-FLKLAR     SECTION.                                            
074300                                                                          
074400     MOVE NEJ                         TO SW-ALLT-INLAGT                   
074500                                                                          
074600     IF REQU-FLKLAR                    NOT = ALL '+'                      
074700        IF REQU-FLKLAR                 =  JA OR NEJ OR YES                
074800           IF REQU-FLKLAR              =  JA OR YES                       
074900               MOVE JA                TO SW-ALLT-INLAGT                   
075000           END-IF                                                         
075100        END-IF                                                            
075200     END-IF                                                               
075300                                                                          
075400     .                                                                    
075500     EJECT                                                                
075600                                                                          
075700 GAE-KOLLA-FLMAK      SECTION.                                            
075800                                                                          
075900     MOVE NEJ                         TO SW-MAKULERA-ILISTA               
076000                                                                          
076100     IF REQU-FLMAK                     NOT = ALL '+'                      
076200        IF REQU-FLMAK                  =  JA OR NEJ OR YES                
076300           IF REQU-FLMAK               =  JA OR YES                       
076400               MOVE JA                TO SW-MAKULERA-ILISTA               
076500           END-IF                                                         
076600        END-IF                                                            
076700     END-IF                                                               
076800                                                                          
076900     .                                                                    
077000     EJECT                                                                
077100                                                                          
077200 GB-LOGISK-KONTROLL SECTION.                                              
077300                                                                          
077400     IF REQU-FLSKRIV = 'J'                                                
077500        IF ALLT-INLAGT OR MAKULERA-ILISTA OR                              
077600          RAD-INPUT                                                       
077700          MOVE NEJ     TO INDATA-SW                                       
077800          MOVE '187'                 TO RESP-IDMSG-ERROR                  
077900        END-IF                                                            
078000     ELSE                                                                 
078100        PERFORM GBB-KOLLA-RADINFO                                         
078200        IF RAD-INPUT                                                      
078300           IF ALLT-INLAGT OR MAKULERA-ILISTA                              
078400              MOVE NEJ     TO INDATA-SW                                   
078500              MOVE '187'                 TO RESP-IDMSG-ERROR              
078600           END-IF                                                         
078700        ELSE                                                              
078800           IF ALLT-INLAGT AND MAKULERA-ILISTA                             
078900              MOVE NEJ     TO INDATA-SW                                   
079000              MOVE '187'                 TO RESP-IDMSG-ERROR              
079100           ELSE                                                           
079200              IF ALLT-INLAGT OR MAKULERA-ILISTA                           
079300                 IF ALLT-INLAGT                                           
079400                   PERFORM GBC-KOLLA-LAGERPLATS                           
079500                 END-IF                                                   
079600              ELSE                                                        
079700                 MOVE NEJ     TO INDATA-SW                                
079800                 MOVE '293'              TO RESP-IDMSG-ERROR              
079900              END-IF                                                      
080000           END-IF                                                         
080100        END-IF                                                            
080200     END-IF                                                               
080300                                                                          
080400     .                                                                    
080500     EJECT                                                                
080600                                                                          
080700 GBB-KOLLA-RADINFO    SECTION.                                            
080800                                                                          
080900     MOVE +1                            TO INDX                           
081000     MOVE REQU-KVRADER                  TO WS-INDX-REC                    
081100     MOVE NEJ                           TO WS-REC-LIMIT                   
081200                                                                          
081300     PERFORM UNTIL INDX     >  MAX-INDX OR REC-LIMIT                      
081400        IF REQU-FLCMD(INDX) = 'J' OR 'Y'                                  
081500                                                                          
081600           MOVE REQU-IDILIST-KEY         TO W-IDILIST-E1-MIN              
081700                                            W-IDILIST-E1-MAX              
081800                                                                          
081900           MOVE REQU-IDARTNR(INDX)     TO W-IDARTNR                       
082000                                          W-IDARTNR-A2                    
082100           MOVE REQU-IDRADNR(INDX)     TO W-IDRADNR                       
082200                                          W-IDRADNR-A2                    
082300           PERFORM IMS-GU-WLKREJ01-KVAL                                   
082400           IF SEGMENT-FINNS                                               
082500             MOVE SEQE-IDDISTR           TO W-IDDISTR                     
082600             MOVE SEQE-IDKUNDNR          TO W-IDKUNDNR                    
082700             MOVE SEQE-IDRAPPNR          TO W-IDRAPPNR                    
082800             PERFORM IMS-GHU-WLKREE11                                     
082900                                                                          
083000             IF SEGMENT-FINNS                                             
083100                IF REQU-KVANTAL (INDX) NOT = ALL '+'                      
083200                  MOVE REQU-KVANTAL (INDX)  TO W-KVANTAL                  
083300                                                                          
083400*--- INLAGT ANTAL FÅR EJ VARA STÖRRE ÄN ANTALET PÅ I-LISTAN.              
083500                  IF LEV-KVANTAL-ILI   <  W-KVANTAL                       
083600                                                                          
083700                     MOVE NEJ               TO INDATA-SW                  
083800                     MOVE 'KVANTAL'         TO RESP-IDELMT-ERROR          
083900                     MOVE '246'     TO RESP-IDMSG-ERROR                   
084000                                       RESP-IDMSG-ERROR-LINE(INDX)        
084100                  ELSE                                                    
084200                     IF LEV-KVANTAL-ILI > ZERO                            
084300                       IF LEV-IDARTNR NOT = 100                           
084400                         PERFORM GBBA-KOLLA-LAGERPLATS                    
084500                       END-IF                                             
084600                     END-IF                                               
084700                  END-IF                                                  
084800                ELSE                                                      
084900                  IF LEV-KVANTAL-ILI > ZERO                               
085000                    IF LEV-IDARTNR NOT = 100                              
085100                      PERFORM GBBA-KOLLA-LAGERPLATS                       
085200                    END-IF                                                
085300                  END-IF                                                  
085400                END-IF                                                    
085500                                                                          
085600             ELSE                                                         
085700                MOVE NEJ                TO INDATA-SW                      
085800                MOVE 'IDRADNR'            TO RESP-IDELMT-ERROR            
085900                MOVE '041'        TO RESP-IDMSG-ERROR                     
086000                                     RESP-IDMSG-ERROR-LINE(INDX)          
086100             END-IF                                                       
086200           ELSE                                                           
086300              MOVE NEJ                TO INDATA-SW                        
086400              MOVE '027'            TO RESP-IDMSG-ERROR                   
086500                                       RESP-IDMSG-ERROR-LINE(INDX)        
086600           END-IF                                                         
086700        END-IF                                                            
086800        IF INDX = WS-INDX-REC                                             
086900           MOVE JA TO WS-REC-LIMIT                                        
087000        ELSE                                                              
087100           ADD +1                       TO INDX                           
087200        END-IF                                                            
087300     END-PERFORM                                                          
087400     .                                                                    
087500     EJECT                                                                
087600                                                                          
087700                                                                          
087800 GBBA-KOLLA-LAGERPLATS  SECTION.                                          
087900                                                                          
088000     MOVE LEV-IDARTNR          TO W-IDARTNR                               
088100                                                                          
088200     IF CDC-SE                                                            
088300       PERFORM IMS-GU-WLARTC11                                            
088400       IF SEGMENT-FINNS                                                   
088500         IF CLAG-ADLAGOMR > ZERO OR                                       
088600           CLAG-ADGANG   > ZERO OR                                        
088700           CLAG-ADPLATS  > ZERO                                           
088800           CONTINUE                                                       
088900         ELSE                                                             
089000           MOVE NEJ                     TO INDATA-SW                      
089100           MOVE 'IDRADNR'               TO RESP-IDELMT-ERROR              
089200           MOVE ERR-LAGER-SAKNAS        TO RESP-IDMSG-ERROR               
089300                                RESP-IDMSG-ERROR-LINE(INDX)               
089400         END-IF                                                           
089500       ELSE                                                               
089600         MOVE NEJ                       TO INDATA-SW                      
089700         MOVE 'IDRADNR'                 TO RESP-IDELMT-ERROR              
089800         MOVE ERR-LAGER-SAKNAS          TO RESP-IDMSG-ERROR               
089900                              RESP-IDMSG-ERROR-LINE(INDX)                 
090000       END-IF                                                             
090100     ELSE                                                                 
090200       PERFORM IMS-GU-WLARTS11                                            
090300       IF SEGMENT-FINNS                                                   
090400         IF SLAG-ADLAGOMR > ZERO OR                                       
090500           SLAG-ADGANG   > ZERO OR                                        
090600           SLAG-ADPLATS  > ZERO                                           
090700           CONTINUE                                                       
090800         ELSE                                                             
090900           MOVE NEJ                     TO INDATA-SW                      
091000           MOVE 'IDRADNR'               TO RESP-IDELMT-ERROR              
091100           MOVE ERR-LAGER-SAKNAS        TO RESP-IDMSG-ERROR               
091200                                RESP-IDMSG-ERROR-LINE(INDX)               
091300         END-IF                                                           
091400       ELSE                                                               
091500         MOVE NEJ                       TO INDATA-SW                      
091600         MOVE 'IDRADNR'                 TO RESP-IDELMT-ERROR              
091700         MOVE ERR-LAGER-SAKNAS          TO RESP-IDMSG-ERROR               
091800                              RESP-IDMSG-ERROR-LINE(INDX)                 
091900       END-IF                                                             
092000     END-IF                                                               
092100                                                                          
092200     .                                                                    
092300     EJECT                                                                
092400                                                                          
092500 GBC-KOLLA-LAGERPLATS  SECTION.                                           
092600                                                                          
092700     PERFORM IMS-GU-WLKREJ01-OKVAL                                        
092800                                                                          
092900     IF SEGMENT-FINNS                                                     
093000       PERFORM UNTIL SEGMENT-SAKNAS OR INDATA-FEL                         
093100          MOVE SEQE-IDDISTR         TO W-IDDISTR                          
093200          MOVE SEQE-IDKUNDNR        TO W-IDKUNDNR                         
093300          MOVE SEQE-IDRAPPNR        TO W-IDRAPPNR                         
093400          MOVE SEQE-IDARTNR         TO W-IDARTNR-A2                       
093500          MOVE SEQE-IDRADNR         TO W-IDRADNR-A2                       
093600                                                                          
093700          PERFORM IMS-GHU-WLKREE11                                        
093800                                                                          
093900*---  KOLLA ATT LAGERPLATS FINNS FÖR ARTIKEL                              
094000          IF LEV-IDARTNR NOT = 100                                        
094100            MOVE LEV-IDARTNR   TO W-IDARTNR                               
094200                                                                          
094300            IF CDC-SE                                                     
094400              PERFORM IMS-GU-WLARTC11                                     
094500              IF SEGMENT-FINNS                                            
094600                IF CLAG-ADLAGOMR > ZERO OR                                
094700                  CLAG-ADGANG > ZERO OR                                   
094800                  CLAG-ADPLATS > ZERO                                     
094900                  CONTINUE                                                
095000                ELSE                                                      
095100                  MOVE NEJ                   TO INDATA-SW                 
095200                  MOVE ERR-LAGER-SAKNAS      TO RESP-IDMSG-ERROR          
095300                END-IF                                                    
095400              ELSE                                                        
095500                MOVE NEJ                   TO INDATA-SW                   
095600                MOVE ERR-LAGER-SAKNAS      TO RESP-IDMSG-ERROR            
095700              END-IF                                                      
095800            ELSE                                                          
095900              PERFORM IMS-GU-WLARTS11                                     
096000              IF SEGMENT-FINNS                                            
096100                IF SLAG-ADLAGOMR > ZERO OR                                
096200                  SLAG-ADGANG > ZERO OR                                   
096300                  SLAG-ADPLATS > ZERO                                     
096400                  CONTINUE                                                
096500                ELSE                                                      
096600                  MOVE NEJ                   TO INDATA-SW                 
096700                  MOVE ERR-LAGER-SAKNAS      TO RESP-IDMSG-ERROR          
096800                END-IF                                                    
096900              ELSE                                                        
097000                MOVE NEJ                   TO INDATA-SW                   
097100                MOVE ERR-LAGER-SAKNAS      TO RESP-IDMSG-ERROR            
097200              END-IF                                                      
097300            END-IF                                                        
097400          END-IF                                                          
097500                                                                          
097600          PERFORM IMS-GN-WLKREJ01-OKVAL                                   
097700       END-PERFORM                                                        
097800     END-IF                                                               
097900     .                                                                    
098000     EJECT                                                                
098100                                                                          
098200                                                                          
098300 H-UPPDATERA-SKRIV-UT SECTION.                                            
098400                                                                          
098500     MOVE +1                     TO 4797-INDX                             
098600                                                                          
098700     IF REQU-FLSKRIV = 'J'                                                
098800        PERFORM HA-SKRIV-UT-ILISTA                                        
098900        MOVE '237'               TO RESP-IDMSG-INFO                       
099000     ELSE                                                                 
099100        IF ALLT-INLAGT                                                    
099200           PERFORM HB-UPPDATERA-ALLT-INLAGT                               
099300        ELSE                                                              
099400           IF MAKULERA-ILISTA                                             
099500              PERFORM HC-MAKULERA-ILISTA                                  
099600           ELSE                                                           
099700              PERFORM HD-UPPDATERA-VALDA-RADER                            
099800           END-IF                                                         
099900        END-IF                                                            
100000                                                                          
100100        MOVE '001'                     TO RESP-IDMSG-INFO                 
100200                                                                          
100300     END-IF                                                               
100400                                                                          
100500     IF 4797-INDX              >  +1                                      
100600        PERFORM S04-STARTA-R32-RAPPORTERING                               
100700     END-IF                                                               
100800                                                                          
100900     .                                                                    
101000     EJECT                                                                
101100                                                                          
101200 HA-SKRIV-UT-ILISTA    SECTION.                                           
101300                                                                          
101400     MOVE REQU-IDILIST-KEY     TO W-IDILIST-ESEQ-MIN                      
101500                                  W-IDILIST-ESEQ-MAX                      
101600                                                                          
101700     PERFORM IMS-GHU-SEQE-WLKREE11                                        
101800     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
101900                                                                          
102000         PERFORM HAA-KOLLA-PLATS                                          
102100                                                                          
102200         IF REQU-IDANSTNR NUMERIC                                         
102300           MOVE REQU-IDANSTNR           TO LEV-IDANSTNR-RET               
102400         END-IF                                                           
102500                                                                          
102600         ACCEPT LEV-TIUTSKR    FROM DATE                                  
102700         PERFORM IMS-REPL-WLKREE-SEQE                                     
102800                                                                          
102900         PERFORM IMS-GHN-SEQE-WLKREE11                                    
103000                                                                          
103100     END-PERFORM                                                          
103200                                                                          
103300     PERFORM HAB-STARTA-4795                                              
103400     .                                                                    
103500     EJECT                                                                
103600                                                                          
103700 HAA-KOLLA-PLATS  SECTION.                                                
103800                                                                          
103900     MOVE LEV-IDARTNR          TO W-IDARTNR                               
104000       PERFORM IMS-GU-WLARTS11                                            
104100       IF (SLAG-ADLAGOMR = LEV-ADLAGOMR    AND                            
104200          SLAG-ADGANG   = LEV-ADGANG       AND                            
104300          SLAG-ADPLATS  = LEV-ADPLATS)     OR                             
104400         (SLAG-ADLAGOMR = 22               AND                            
104500          LEV-ADLAGOMR  = 21               AND                            
104600          SLAG-ADGANG   = LEV-ADGANG       AND                            
104700          SLAG-ADPLATS  = LEV-ADPLATS)                                    
104800         CONTINUE                                                         
104900       ELSE                                                               
105000          MOVE SLAG-ADLAGOMR      TO LEV-ADLAGOMR                         
105100          IF LEV-ADLAGOMR = 22                                            
105200            MOVE 21               TO LEV-ADLAGOMR                         
105300          END-IF                                                          
105400          MOVE SLAG-ADGANG        TO LEV-ADGANG                           
105500          MOVE SLAG-ADPLATS       TO LEV-ADPLATS                          
105600       END-IF                                                             
105700     .                                                                    
105800     EJECT                                                                
105900                                                                          
106000 HAB-STARTA-4795  SECTION.                                                
106100                                                                          
106200     CONTINUE                                                             
106300                                                                          
106400     .                                                                    
106500     EJECT                                                                
106600                                                                          
106700 HB-UPPDATERA-ALLT-INLAGT            SECTION.                             
106800     MOVE 'STA-HB-SECT'        TO PGM-POS                                 
106900                                                                          
107000     MOVE ZERO                     TO W-KVRADER-BEH                       
107100                                      W-SPAR-IDDISTR                      
107200                                      W-SPAR-IDKUNDNR                     
107300                                      W-SPAR-IDRAPPNR                     
107400                                                                          
107500     MOVE REQU-IDILIST-KEY         TO W-IDILIST-E1-MIN                    
107600                                      W-IDILIST-E1-MAX                    
107700                                                                          
107800     PERFORM IMS-GU-WLKREJ01-OKVAL                                        
107900                                                                          
108000     PERFORM UNTIL SEGMENT-SAKNAS                                         
108100                                                                          
108200       PERFORM S01-UPPDATERA-EV-WDA201                                    
108300       PERFORM S03-UPPDATERA-WDA211                                       
108400                                                                          
108500       PERFORM IMS-GN-WLKREJ01-OKVAL                                      
108600     END-PERFORM                                                          
108700                                                                          
108800     PERFORM S02-UPPDATERA-WDA201                                         
108900     .                                                                    
109000     EJECT                                                                
109100                                                                          
109200 HC-MAKULERA-ILISTA         SECTION.                                      
109300     MOVE 'STA-HC-SECT'        TO PGM-POS                                 
109400                                                                          
109500     MOVE REQU-IDILIST-KEY     TO W-IDILIST-ESEQ-MIN                      
109600                                  W-IDILIST-ESEQ-MAX                      
109700                                                                          
109800     PERFORM IMS-GHU-SEQE-WLKREE11                                        
109900     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
110000                                                                          
110100         MOVE ZERO             TO LEV-IDILIST                             
110200                                  LEV-TIUTSKR                             
110300                                  LEV-KVANTAL-ILI                         
110400                                  LEV-TIUPPDAT-ILI                        
110500         PERFORM IMS-REPL-WLKREE-SEQE                                     
110600                                                                          
110700         PERFORM IMS-GHN-SEQE-WLKREE11                                    
110800                                                                          
110900     END-PERFORM                                                          
111000                                                                          
111100     .                                                                    
111200     EJECT                                                                
111300                                                                          
111400 HD-UPPDATERA-VALDA-RADER  SECTION.                                       
111500     MOVE 'STA-HD-SECT'        TO PGM-POS                                 
111600                                                                          
111700     MOVE ZERO                     TO W-KVRADER-BEH                       
111800                                      W-SPAR-IDDISTR                      
111900                                      W-SPAR-IDKUNDNR                     
112000                                      W-SPAR-IDRAPPNR                     
112100                                                                          
112200     MOVE REQU-IDILIST-KEY         TO W-IDILIST-E1-MIN                    
112300                                      W-IDILIST-E1-MAX                    
112400                                                                          
112500     MOVE +1                            TO INDX                           
112600     MOVE REQU-KVRADER                  TO WS-INDX-REC                    
112700     MOVE NEJ                           TO WS-REC-LIMIT                   
112800                                                                          
112900     PERFORM UNTIL INDX       >  MAX-INDX OR REC-LIMIT                    
113000        IF REQU-FLCMD(INDX)      = '+' OR SPACE OR 'N'                    
113100           CONTINUE                                                       
113200        ELSE                                                              
113300           MOVE REQU-IDARTNR(INDX)      TO W-IDARTNR                      
113400           MOVE REQU-IDRADNR(INDX)      TO W-IDRADNR                      
113500           PERFORM IMS-GU-WLKREJ01-KVAL                                   
113600                                                                          
113700           PERFORM S01-UPPDATERA-EV-WDA201                                
113800                                                                          
113900           PERFORM S03-UPPDATERA-WDA211                                   
114000        END-IF                                                            
114100                                                                          
114200        IF INDX = WS-INDX-REC                                             
114300           MOVE JA TO WS-REC-LIMIT                                        
114400        ELSE                                                              
114500           ADD +1                       TO INDX                           
114600        END-IF                                                            
114700     END-PERFORM                                                          
114800                                                                          
114900     PERFORM S02-UPPDATERA-WDA201                                         
115000                                                                          
115100     .                                                                    
115200     EJECT                                                                
115300                                                                          
115400 S01-UPPDATERA-EV-WDA201     SECTION.                                     
115500                                                                          
115600     IF SEQE-IDDISTR               =  W-SPAR-IDDISTR  AND                 
115700        SEQE-IDKUNDNR              =  W-SPAR-IDKUNDNR AND                 
115800        SEQE-IDRAPPNR              =  W-SPAR-IDRAPPNR                     
115900        CONTINUE                                                          
116000     ELSE                                                                 
116100        IF SEQE-IDDISTR            =  ZERO                                
116200           CONTINUE                                                       
116300        ELSE                                                              
116400          IF W-SPAR-IDDISTR        =  ZERO                                
116500            MOVE SEQE-IDDISTR      TO W-SPAR-IDDISTR                      
116600            MOVE SEQE-IDKUNDNR     TO W-SPAR-IDKUNDNR                     
116700            MOVE SEQE-IDRAPPNR     TO W-SPAR-IDRAPPNR                     
116800          ELSE                                                            
116900             PERFORM S02-UPPDATERA-WDA201                                 
117000                                                                          
117100             MOVE SEQE-IDDISTR       TO W-SPAR-IDDISTR                    
117200             MOVE SEQE-IDKUNDNR      TO W-SPAR-IDKUNDNR                   
117300             MOVE SEQE-IDRAPPNR      TO W-SPAR-IDRAPPNR                   
117400                                                                          
117500             MOVE ZERO               TO W-KVRADER-BEH                     
117600                                                                          
117700          END-IF                                                          
117800        END-IF                                                            
117900     END-IF                                                               
118000                                                                          
118100     .                                                                    
118200     EJECT                                                                
118300                                                                          
118400 S02-UPPDATERA-WDA201     SECTION.                                        
118500                                                                          
118600     MOVE W-SPAR-IDDISTR           TO W-IDDISTR                           
118700     MOVE W-SPAR-IDKUNDNR          TO W-IDKUNDNR                          
118800     MOVE W-SPAR-IDRAPPNR          TO W-IDRAPPNR                          
118900                                                                          
119000     PERFORM IMS-GHU-WLKREE01                                             
119100                                                                          
119200     IF ANM-KDLEVANM = W-ANM-MOT                                          
119300       MOVE W-ANM-PAAB               TO ANM-KDLEVANM                      
119400     END-IF                                                               
119500     COMPUTE ANM-KVRADER-OBEH      =  ANM-KVRADER-OBEH -                  
119600                                      W-KVRADER-BEH                       
119700                                                                          
119800     PERFORM IMS-REPL-WLKREE01                                            
119900                                                                          
120000     .                                                                    
120100     EJECT                                                                
120200                                                                          
120300 S03-UPPDATERA-WDA211     SECTION.                                        
120400                                                                          
120500     MOVE SEQE-IDDISTR             TO W-IDDISTR                           
120600     MOVE SEQE-IDKUNDNR            TO W-IDKUNDNR                          
120700     MOVE SEQE-IDRAPPNR            TO W-IDRAPPNR                          
120800     MOVE SEQE-IDARTNR             TO W-IDARTNR-A2                        
120900     MOVE SEQE-IDRADNR             TO W-IDRADNR-A2                        
121000                                                                          
121100     PERFORM IMS-GHU-WLKREE11                                             
121200                                                                          
121300     IF ALLT-INLAGT OR                                                    
121400        REQU-KVANTAL(INDX)          NOT NUMERIC                           
121500                                                                          
121600        COMPUTE LEV-KVRETINL       =  LEV-KVRETINL +                      
121700                                      LEV-KVANTAL-ILI                     
121800        MOVE LEV-KVANTAL-ILI       TO W-KVRETINL-R32                      
121900     ELSE                                                                 
122000        MOVE REQU-KVANTAL(INDX)    TO W-KVANTAL                           
122100                                      W-KVRETINL-R32                      
122200        COMPUTE LEV-KVRETINL       =  LEV-KVRETINL +                      
122300                                      W-KVANTAL                           
122400     END-IF                                                               
122500                                                                          
122600     IF REQU-IDANSTNR NUMERIC                                             
122700       MOVE REQU-IDANSTNR           TO LEV-IDANSTNR-RET                   
122800     END-IF                                                               
122900                                                                          
123000     COMPUTE W-KVLEVANM-KVAR       =  LEV-KVLEVANM-BEKR -                 
123100                                      LEV-KVRETINL -                      
123200                                      LEV-KVAVV-KVANT -                   
123300                                      LEV-KVRETINL-SKR -                  
123400                                      LEV-KVAVV-KVAL                      
123500                                                                          
123600     MOVE +0                       TO LEV-IDILIST                         
123700                                      LEV-TIUTSKR                         
123800                                      LEV-KVANTAL-ILI                     
123900                                      LEV-TIUPPDAT-ILI                    
124000                                                                          
124100                                                                          
124200     IF W-KVLEVANM-KVAR            =  ZERO                                
124300       ACCEPT LEV-TIINLINL FROM DATE                                      
124400       ADD +1                     TO W-KVRADER-BEH                        
124500     END-IF                                                               
124600                                                                          
124700     PERFORM IMS-REPL-WLKREE11                                            
124800                                                                          
124900     PERFORM S05-FYLL-I-R32-MID                                           
125000     .                                                                    
125100     EJECT                                                                
125200                                                                          
125300 S04-STARTA-R32-RAPPORTERING     SECTION.                                 
125400                                                                          
125500     MOVE SPACE                TO MSG-KOM-WMSGKOM                         
125600     COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM                     
125700     MOVE LOW-VALUE            TO MSG-KOM-KDZ1                            
125800     MOVE LOW-VALUE            TO MSG-KOM-KDZ2                            
125900     MOVE SPACE                TO MSG-KOM-KDTRANS                         
126000     MOVE 'W4I79701'           TO MSG-KOM-IDCPYTXT                        
126100     MOVE 'INLEVRET'           TO MSG-KOM-IDSNDNOD                        
126200     MOVE 'WL015300'           TO MSG-KOM-IDSNDJOB                        
126300     ACCEPT MSG-KOM-TIREGDAT   FROM DATE                                  
126400     ACCEPT MSG-KOM-TIKLOCK    FROM TIME                                  
126500                                                                          
126600     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
126700                                                                          
126800     COMPUTE P-TO-P1-LL        =  LNG-P-TO-P-PREFIX +                     
126900                                  LENGTH OF MOD4797-MID-W4I79701          
127000                                                                          
127100     MOVE 'W4T797X '           TO P-TO-P1-TRANSKOD                        
127200     MOVE 'L153'               TO P-TO-P1-FROM-MID                        
127300     MOVE '2'                  TO P-TO-P1-KDMFSFOR                        
127400                                                                          
127500     COMPUTE MOD4797-MID-KVPOST  = 4797-INDX - 1                          
127600                                                                          
127700     MOVE KOM-AREA        TO P-TO-P1-DATA                                 
127800     CALL W006KOM         USING MSG-PCB                                   
127900                                DISP-PCB                                  
128000                                KOMA-PCB                                  
128100                                MSG-KOM-WMSGKOM                           
128200                                P-TO-P-AREA1                              
128300                                                                          
128400     .                                                                    
128500     EJECT                                                                
128600                                                                          
128700 S05-FYLL-I-R32-MID SECTION.                                              
128800                                                                          
128900     MOVE SEQE-IDDISTR      TO MOD4797-MID-IDDISTR     (4797-INDX)        
129000     MOVE SEQE-IDKUNDNR     TO MOD4797-MID-IDKUNDNR    (4797-INDX)        
129100     MOVE SEQE-IDRAPPNR     TO MOD4797-MID-IDRAPPNR    (4797-INDX)        
129200     MOVE LEV-IDARTNR       TO MOD4797-MID-IDARTNR     (4797-INDX)        
129300     MOVE LEV-IDRADNR       TO MOD4797-MID-IDRADNR     (4797-INDX)        
129400     MOVE W-KVRETINL-R32    TO MOD4797-MID-KVRETINL    (4797-INDX)        
129500     MOVE ZERO              TO MOD4797-MID-KVAVV-KVANT (4797-INDX)        
129600                               MOD4797-MID-KVRETINL-TRP(4797-INDX)        
129700                               MOD4797-MID-KVRETINL-SKR(4797-INDX)        
129800                                                                          
129900     ADD +1                    TO 4797-INDX                               
130000                                                                          
130100     IF 4797-INDX              >  4797-MAX-INDX                           
130200        PERFORM S04-STARTA-R32-RAPPORTERING                               
130300        MOVE +1                TO 4797-INDX                               
130400     END-IF                                                               
130500     .                                                                    
130600     EJECT                                                                
130700*    --- DISPATCHER SECTIONS                                              
130800 S06-FETCH-REQUEST-ARGUMENT SECTION.                                      
130900                                                                          
131000     MOVE 'GETARG'               TO SUB-KDFUNC                            
131100     MOVE 'CARPARTS.LDC.REPORTBINNINGLIST'  TO SUB-ADDISPABS              
131200     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
131300                                                                          
131400     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
131500                                                                          
131600     IF SUB-KDRC > 0                                                      
131700       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
131800       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
131900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
132000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
132100     END-IF                                                               
132200     .                                                                    
132300     SKIP3                                                                
132400 S07-RETURN-RESPONSE SECTION.                                             
132500                                                                          
132600     MOVE 'RETURN'                   TO SUB-KDFUNC                        
132700     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
132800                                                                          
132900     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
133000                                                                          
133100     IF SUB-KDRC > 0                                                      
133200       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
133300       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
133400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
133500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
133600     END-IF                                                               
133700     .                                                                    
133800     SKIP3                                                                
133900 S11-MSG-CONV SECTION.                                                    
134000     MOVE SPACES                  TO RESP-MESSAGES (1)                    
134100                                     RESP-MESSAGES (2)                    
134200     MOVE 1                       TO MSG-IX                               
134300*    REQUEST OK                                                           
134400     MOVE 200                     TO RESP-KDSTATUS-API                    
134500     IF RESP-IDMSG-INFO > SPACE                                           
134600       MOVE SPACES                TO MSG-CONV-AREA                        
134700       MOVE RESP-IDMSG-INFO       TO MSG-CONV-IDMSG-IN                    
134800       CALL WMSGCONV           USING MSG-CONV-AREA                        
134900       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
135000       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
135100       ADD 1                      TO MSG-IX                               
135200     END-IF                                                               
135300     IF RESP-IDMSG-ERROR > SPACE                                          
135400*      BAD REQUEST                                                        
135500       MOVE 400                   TO RESP-KDSTATUS-API                    
135600       MOVE SPACES                TO MSG-CONV-AREA                        
135700       MOVE RESP-IDMSG-ERROR      TO MSG-CONV-IDMSG-IN                    
135800       MOVE RESP-IDELMT-ERROR     TO MSG-CONV-IDELMT                      
135900       CALL WMSGCONV           USING MSG-CONV-AREA                        
136000       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
136100       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
136200     END-IF                                                               
136300     .                                                                    
136400 IMS-GHU-WLKREE01       SECTION.                                          
136500                                                                          
136600     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
136700          DELIMITED BY SIZE INTO SSA1                                     
136800     MOVE '  GE'           TO GODK-STATUSKODER                            
136900     CALL CBLTDLI USING GHU KREE1-PCB DLI-IO-AREA2 SSA1                   
137000     MOVE KREE1-STATUS-CODE TO STATUS-WS                                  
137100     PERFORM IMS-STATUSKONTROLL                                           
137200     .                                                                    
137300                                                                          
137400 IMS-REPL-WLKREE01      SECTION.                                          
137500                                                                          
137600     MOVE '    '           TO GODK-STATUSKODER                            
137700     CALL CBLTDLI USING REPL KREE1-PCB DLI-IO-AREA2                       
137800     MOVE KREE1-STATUS-CODE TO STATUS-WS                                  
137900     PERFORM IMS-STATUSKONTROLL                                           
138000     .                                                                    
138100     EJECT                                                                
138200                                                                          
138300 IMS-GHU-WLKREE11       SECTION.                                          
138400                                                                          
138500     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
138600          DELIMITED BY SIZE INTO SSA1                                     
138700     STRING 'WLKREE11(WDA211KY =' W-WDA211KY-X ')'                        
138800          DELIMITED BY SIZE INTO SSA2                                     
138900     MOVE '  '           TO GODK-STATUSKODER                              
139000     CALL CBLTDLI USING GHU KREE1-PCB DLI-IO-AREA2 SSA1 SSA2              
139100     MOVE KREE1-STATUS-CODE TO STATUS-WS                                  
139200     PERFORM IMS-STATUSKONTROLL                                           
139300     .                                                                    
139400                                                                          
139500 IMS-REPL-WLKREE11      SECTION.                                          
139600                                                                          
139700     MOVE '    '           TO GODK-STATUSKODER                            
139800     CALL CBLTDLI USING REPL KREE1-PCB DLI-IO-AREA2                       
139900     MOVE KREE1-STATUS-CODE TO STATUS-WS                                  
140000     PERFORM IMS-STATUSKONTROLL                                           
140100     .                                                                    
140200     EJECT                                                                
140300                                                                          
140400 IMS-GHU-SEQE-WLKREE11       SECTION.                                     
140500                                                                          
140600     STRING 'WLKREE11(WDA2ESEQ>=' W-WDA2ESEQ-MIN-X                        
140700                    '&WDA2ESEQ<=' W-WDA2ESEQ-MAX-X ')'                    
140800          DELIMITED BY SIZE INTO SSA1                                     
140900     MOVE '  GE'           TO GODK-STATUSKODER                            
141000     CALL CBLTDLI USING GHU KREE2-PCB DLI-IO-AREA2 SSA1                   
141100     MOVE KREE2-STATUS-CODE TO STATUS-WS                                  
141200     PERFORM IMS-STATUSKONTROLL                                           
141300     .                                                                    
141400                                                                          
141500 IMS-GHN-SEQE-WLKREE11       SECTION.                                     
141600                                                                          
141700     STRING 'WLKREE11(WDA2ESEQ>=' W-WDA2ESEQ-MIN-X                        
141800                    '&WDA2ESEQ<=' W-WDA2ESEQ-MAX-X ')'                    
141900          DELIMITED BY SIZE INTO SSA1                                     
142000     MOVE '  GEGB'           TO GODK-STATUSKODER                          
142100     CALL CBLTDLI USING GHN KREE2-PCB DLI-IO-AREA2 SSA1                   
142200     MOVE KREE2-STATUS-CODE TO STATUS-WS                                  
142300     PERFORM IMS-STATUSKONTROLL                                           
142400     .                                                                    
142500                                                                          
142600 IMS-REPL-WLKREE-SEQE   SECTION.                                          
142700                                                                          
142800     MOVE '    '           TO GODK-STATUSKODER                            
142900     CALL CBLTDLI USING REPL KREE2-PCB DLI-IO-AREA2                       
143000     MOVE KREE2-STATUS-CODE TO STATUS-WS                                  
143100     PERFORM IMS-STATUSKONTROLL                                           
143200     .                                                                    
143300     EJECT                                                                
143400                                                                          
143500 IMS-GU-WLKREJ01-KVAL SECTION.                                            
143600                                                                          
143700     STRING 'WLKREJ01(WDA2E1KY>=' W-WDA2E1KY-MIN-X                        
143800                    '&WDA2E1KY<=' W-WDA2E1KY-MAX-X                        
143900                    '&IDARTNR  =' W-IDARTNR-X                             
144000                    '&IDRADNR  =' W-IDRADNR-X ')'                         
144100          DELIMITED BY SIZE INTO SSA1                                     
144200     MOVE '  GE'           TO GODK-STATUSKODER                            
144300     CALL CBLTDLI USING GU KREJ-PCB DLI-IO-AREA SSA1                      
144400     MOVE KREJ-STATUS-CODE TO STATUS-WS                                   
144500     PERFORM IMS-STATUSKONTROLL                                           
144600     .                                                                    
144700                                                                          
144800 IMS-GU-WLKREJ01-OKVAL SECTION.                                           
144900                                                                          
145000     STRING 'WLKREJ01(WDA2E1KY>=' W-WDA2E1KY-MIN-X                        
145100                    '&WDA2E1KY<=' W-WDA2E1KY-MAX-X ')'                    
145200          DELIMITED BY SIZE INTO SSA1                                     
145300     MOVE '  GE'           TO GODK-STATUSKODER                            
145400     CALL CBLTDLI USING GU KREJ-PCB DLI-IO-AREA SSA1                      
145500     MOVE KREJ-STATUS-CODE TO STATUS-WS                                   
145600     PERFORM IMS-STATUSKONTROLL                                           
145700     .                                                                    
145800                                                                          
145900 IMS-GN-WLKREJ01-OKVAL  SECTION.                                          
146000                                                                          
146100     STRING 'WLKREJ01(WDA2E1KY>=' W-WDA2E1KY-MIN-X                        
146200                    '&WDA2E1KY<=' W-WDA2E1KY-MAX-X ')'                    
146300          DELIMITED BY SIZE INTO SSA1                                     
146400     MOVE '  GE'           TO GODK-STATUSKODER                            
146500     CALL CBLTDLI USING GN KREJ-PCB DLI-IO-AREA SSA1                      
146600     MOVE KREJ-STATUS-CODE TO STATUS-WS                                   
146700     PERFORM IMS-STATUSKONTROLL                                           
146800     .                                                                    
146900                                                                          
147000 IMS-GU-WLBENA11     SECTION.                                             
147100                                                                          
147200     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
147300            DELIMITED BY SIZE INTO SSA1                                   
147400     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
147500            DELIMITED BY SIZE INTO SSA2                                   
147600     MOVE '  ' TO GODK-STATUSKODER                                        
147700     CALL CBLTDLI USING GU  BENA-PCB DLI-IO-AREA2 SSA1 SSA2               
147800     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
147900     PERFORM IMS-STATUSKONTROLL                                           
148000     .                                                                    
148100     EJECT                                                                
148200 IMS-GU-WLARTS11     SECTION.                                             
148300                                                                          
148400     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
148500            DELIMITED BY SIZE INTO SSA1                                   
148600     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
148700            DELIMITED BY SIZE INTO SSA2                                   
148800     MOVE '  ' TO GODK-STATUSKODER                                        
148900     CALL CBLTDLI USING GU  ARTS-PCB DLI-IO-AREA3 SSA1 SSA2               
149000     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
149100     PERFORM IMS-STATUSKONTROLL                                           
149200     .                                                                    
149300     EJECT                                                                
149400 IMS-GU-WLARTC11     SECTION.                                             
149500                                                                          
149600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
149700            DELIMITED BY SIZE INTO SSA1                                   
149800     MOVE 'WLARTC11 '    TO SSA2                                          
149900     MOVE '  ' TO GODK-STATUSKODER                                        
150000     CALL CBLTDLI USING GU  ARTC-PCB DLI-IO-K611 SSA1 SSA2                
150100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
150200     PERFORM IMS-STATUSKONTROLL                                           
150300     .                                                                    
150400 IMS-GU-WDB601 SECTION.                                                   
150500                                                                          
150600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
150700          DELIMITED BY SIZE INTO SSA1                                     
150800     MOVE '  ' TO GODK-STATUSKODER                                        
150900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
151000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
151100     PERFORM IMS-STATUSKONTROLL                                           
151200     .                                                                    
151300     SKIP3                                                                
151400 IMS-STATUSKONTROLL SECTION.                                              
151500                                                                          
151600     SET STATUS-IX TO 1                                                   
151700     SEARCH GODK-STATUS                                                   
151800       AT END                                                             
151900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
152000         DELIMITED BY SIZE INTO FELTEXT                                   
152100         CALL FELLOG                                                      
152200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
152300         CONTINUE                                                         
152400     END-SEARCH                                                           
152500     .                                                                    
