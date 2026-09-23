000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6011500.                                                
000400*AUTHOR.         LARS THELL                                               
000500*DATE-WRITTEN.   92/04/01.                                                
000600*                                                                         
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        AKTIVERAR FÖLJESEDLAR VID MOTTAGNING                             
001100*                                                                         
001200*        THIS IS A DRIVER PGM FOR TRANSACTIONS W6T115, W6T115U            
001300*                                                                         
001400*        IT TAKES CARE OF ALL TECHNICAL DETAILS RELATED TO WHELP          
001500*        AND 3270 FORMATS AND CALLS SUBPROGRAM W6011510 WHICH             
001600*        CONTAINS ALL BUSINESS LOGIC FOR THESE TRANSACTIONS.              
001700*                                                                         
001800*        A SIMILAR DRIVER PROGRAM FOR INVOCATIONS FROM THE WEB            
001900*        EXISTS - W6W14400 (TRANSACTIONS W6T115, W6T115U)                 
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSAKTION: W6T115                                              
002300*        MID:         W6I11501                                            
002400*                                                                         
002500*    UTDATA.                                                              
002600*        MOD:         W6O11501                                            
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400 77  IDPGM                       PIC X(08)   VALUE 'W6011500'.            
003500                                                                          
003600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003700 77  FILLER                      PIC X(8)  VALUE 'ERRORTEX'.              
003800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003900                                                                          
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  YES                         PIC X       VALUE 'Y'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004400 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
004500 01  WS-IDDC-LOCAL.                                                       
004600     03  FILLER                  PIC X(5)   VALUE 'WIDDC'.                
004700     03  WS-IDDC-LOCAL-DATE      PIC X(2).                                
004800     03  FILLER                  PIC X(1)   VALUE SPACE.                  
004900 01  WS-DAT-TIVVD                PIC 9(3)   VALUE ZERO.                   
005000                                                                          
005100*    --- INDEX FÖR BLÄDDRINGSRADER                                        
005200 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005300 77  MAX-INDX                    PIC S9(4)  VALUE +10   COMP SYNC.        
005400 77  RAD-IX                      PIC S9(4)  VALUE +0    COMP SYNC.        
005500 77  RAD-MAX-IX                  PIC S9(4)  VALUE +12   COMP SYNC.        
005600 77  MAX-ADINLOMR-IX             PIC S9(4)  VALUE +50   COMP SYNC.        
005700 77  MAX-LPL-IX                  PIC S9(4)  VALUE +10   COMP SYNC.        
005800 77  MAX-MOD-LPL-IX              PIC S9(4)  VALUE +5    COMP SYNC.        
005900 77  MOD-LPL-IX                  PIC S9(4)  VALUE +0    COMP SYNC.        
006000 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
006100 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +927  COMP SYNC.        
006200 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
006300 77  MAX-REQU-INDX               PIC S9(4)  VALUE +500  COMP SYNC.        
006400                                                                          
006500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006600 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
006700 77  WS-IDFS                     PIC X(8)    VALUE SPACE.                 
006800 77  WS-TIAVIDAT                 PIC X(6)    VALUE SPACE.                 
006900 77  WS-IDLBBET                  PIC X(12)   VALUE SPACE.                 
007000 77  WS-ADINLOMR-PRT             PIC X(4)    VALUE SPACE.                 
007100                                                                          
007200 77  WS-IDLEVNR-2                PIC X(5)    VALUE SPACE.                 
007300 77  W-TIAVIDAT                  PIC S9(7)   VALUE ZERO COMP-3.           
007400 77  W-IDFS                      PIC  X(8)   VALUE SPACE.                 
007500 77  W-KVPARTI                   PIC S9(7)   VALUE ZERO COMP-3.           
007600 77  W-TIAVIDAT-RAD              PIC 9(7)    VALUE ZERO.                  
007700 77  W-KDVVKL                    PIC S9(1)   VALUE ZERO COMP-3.           
007800 77  W-KVPB-SEP-CL               PIC S9(6)V9 VALUE ZERO COMP-3.           
007900 77  W-KVPB-SATS-CL              PIC S9(6)V9 VALUE ZERO COMP-3.           
008000 77  W-KVPB-TPO-CL               PIC S9(6)V9 VALUE ZERO COMP-3.           
008100 77  W-FLFSP-CL                  PIC X(1)    VALUE 'N'.                   
008200 77  W-FLEJBUFF                  PIC X(1)    VALUE 'N'.                   
008300 77  FLSLUT                      PIC X(1)    VALUE 'N'.                   
008400 77  FL-SVS                      PIC X(1)    VALUE 'N'.                   
008500 77  HELP-FLSLUT                 PIC X(1)    VALUE 'N'.                   
008600 77  W-OFOERAEDLAT               PIC S9(7)   VALUE ZERO COMP-3.           
008700 77  WS-OFOERAEDLAT              PIC S9(7)   VALUE ZERO COMP-3.           
008800 77  WS-SATSBEHOV                PIC S9(7)   VALUE ZERO COMP-3.           
008900 77  W-SUMMA-PRIO-OFR            PIC S9(7)   VALUE ZERO COMP-3.           
009000 77  W-PROCENT-SATS              PIC S9(1)V9(2) VALUE ZERO COMP-3.        
009100 77  W-MAX-PROCENT-LPL           PIC S9(1)V9(2) VALUE ZERO COMP-3.        
009200 77  W-TOT-KVPB-CL               PIC S9(6)V9 VALUE ZERO COMP-3.           
009300 77  W-KVKVAR-ATT-STYRA          PIC S9(7)   VALUE ZERO COMP-3.           
009400 77  W-KVAVIS-FB                 PIC S9(7)   VALUE ZERO COMP-3.           
009500 77  W-KVAVIS-FP                 PIC S9(7)   VALUE ZERO COMP-3.           
009600 77  W-KVAVIS-LO                 PIC S9(7)   VALUE ZERO COMP-3.           
009700 77  W-KVAVIS-BO                 PIC S9(7)   VALUE ZERO COMP-3.           
009800 77  W-KVAVIS-OVR                PIC S9(7)   VALUE ZERO COMP-3.           
009900 77  W-KVAVIS-PRIO-EGET          PIC S9(7)   VALUE ZERO COMP-3.           
010000 77  W-KVAVIS-KVAR               PIC S9(7)   VALUE ZERO COMP-3.           
010100 77  WS-KVANTAL-CD               PIC S9(7)   VALUE ZERO COMP-3.           
010200 77  W-LPL-FB                    PIC  X(4)   VALUE SPACE.                 
010300 77  W-LPL-FP                    PIC  X(4)   VALUE SPACE.                 
010400 77  W-LPL-LO                    PIC  X(4)   VALUE SPACE.                 
010500 77  W-LPL-BO                    PIC  X(4)   VALUE SPACE.                 
010600 77  W-ADINLOMR-BO               PIC  X(4)   VALUE SPACE.                 
010700 77  W-ADINLOMR-OVR              PIC  X(4)   VALUE SPACE.                 
010800 77  W-TOT-VLARTNTO              PIC S9(9)V9(1) COMP-3 VALUE ZERO.        
010900 77  W-BIL-VLARTNTO              PIC S9(9)V9(1) COMP-3 VALUE ZERO.        
011000 77  W-SPAR-LPL                  PIC  X(4)   VALUE SPACE.                 
011100 77  W-SPAR-ADLAGOMR             PIC 9(2)     VALUE ZEROS.                
011200 77  W-JFR-ADLAGOMR              PIC S9(3)    VALUE ZEROS COMP-3.         
011300 77  W-ADLAGOMR                  PIC 9(2)    VALUE ZERO.                  
011400 77  W-IDILIST                   PIC 9(5)     VALUE ZEROS.                
011500 77  W-KVRADER                   PIC 9(5)     VALUE ZEROS.                
011600 77  W-SPAR-TORG                 PIC X(4)     VALUE SPACE.                
011700 77  W-KVPOST-I-LISTA            PIC 9(7)     VALUE ZERO.                 
011800 77  W-IDPRTLST                  PIC X(8)     VALUE SPACE.                
011900 77  W-IDPRTLST-VAL              PIC X(2)     VALUE SPACE.                
012000 77  W-KVUTSKR-AR                PIC S9(3)    VALUE ZERO.                 
012100 77  TMO-ARTIKEL                 PIC X        VALUE 'N'.                  
012200 77  DATUM-PASSERAT              PIC X        VALUE 'N'.                  
012300 77  X-DAGAR                     PIC S9(2)    VALUE 20.                   
012400 77  WS-REDAN-UTTAGET            PIC S9(7)    VALUE ZERO COMP-3.          
012500 77  FL-UTTAG                    PIC X(1)    VALUE 'J'.                   
012600 77  WS-KVBEHOV-CD               PIC S9(7)V9(2) VALUE ZERO COMP-3.        
012700 77  WS-IDARTNR-DISP             PIC 9(9)     VALUE ZERO.                 
012800 01  WS-CD.                                                               
012900    03  WS-AK-CD                 OCCURS 4 TIMES                           
013000                                 PIC S9(7)   VALUE ZERO COMP-3.           
013100                                                                          
013200 01  SPAR-6110-KVAVIS            PIC S9(7)    VALUE +0 COMP-3.            
013300 01  SPAR-6110-KVAVIS-PRIO       PIC S9(7)    VALUE +0 COMP-3.            
013400 01  SPAR-6110-KVAVIS-KIT        PIC S9(7)    VALUE +0 COMP-3.            
013500 01  SPAR-IDRADNR-INL            PIC S9(5)    VALUE ZERO COMP-3.          
013600 01  HELP-IDRADNR                PIC S9(5)    VALUE ZERO COMP-3.          
013700 01  HELP-IDRADNR-NY             PIC S9(5)    VALUE ZERO COMP-3.          
013800 01  HELP-KVINLART               PIC S9(7)    VALUE ZERO COMP-3.          
013900                                                                          
014000 01  W-IDLOPNRM                  PIC 9(9)    VALUE ZERO.                  
014100                                                                          
014200 01  WS-TILLDATUM                PIC 9(4).                                
014300                                                                          
014400 01  WS-TILLDATUM-GRP REDEFINES WS-TILLDATUM.                             
014500  03 WS-TILLD-AA                 PIC 9(2).                                
014600  03 WS-TILLD-VV                 PIC 9(2).                                
014700                                                                          
014800 01  W-IDLOPNRM-LOPA             PIC 9(9)    VALUE ZERO.                  
014900                                                                          
015000 01  W-0VVDLLLLK  REDEFINES W-IDLOPNRM-LOPA.                              
015100  03 FILLER                      PIC 9(1).                                
015200  03 W-VVD                       PIC 9(3).                                
015300  03 W-LLLL                      PIC 9(4).                                
015400  03 W-K                         PIC 9(1).                                
015500                                                                          
015600 01  FLT-FOR-BER-AV-IDLOPNRM.                                             
015700     03  FLT-LGD                 PIC S9  COMP SYNC   VALUE +7.            
015800     03  VAEGNINGSTAL            PIC 9(7)        VALUE 2121212.           
015900     03  VAEGNTAL-LGD            PIC S9  COMP SYNC   VALUE +7.            
016000     03  MODUL-10-11             PIC 9(2)            VALUE 10.            
016100     03  ALT-A-B                 PIC X(1)            VALUE 'B'.           
016200                                                                          
016300*    --- TABELL FÖR ADINLOMR                                              
016400 01  FILLER.                                                              
016500  03 W-ADINLOMR-TAB     OCCURS 50 INDEXED BY ADINLOMR-IX.                 
016600     05  W-ADINLOMR              PIC X(4)         VALUE SPACE.            
016700     05  W-VLARTNTO-ADINLOMR     PIC S9(9)V9(1) COMP-3 VALUE ZERO.        
016800     SKIP2                                                                
016900*    --- TABELL FÖR LPL                                                   
017000 01  LPL-TABELL.                                                          
017100  03 W-LPL-TAB          OCCURS 10 INDEXED BY LPL-IX.                      
017200     05  W-SUPROC-LPL           PIC S9(1)V9(2)  COMP-3 VALUE ZERO.        
017300     05  W-VLARTNTO-LPL         PIC S9(10)V9(1) COMP-3 VALUE ZERO.        
017400     05  W-LPL                  PIC X(4)        VALUE SPACE.              
017500     SKIP2                                                                
017600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
017700     88  INDATA-OK                           VALUE 'J'.                   
017800     88  INDATA-FEL                          VALUE 'N'.                   
017900                                                                          
018000 77  INL-LAES-SW                 PIC X       VALUE '0'.                   
018100     88  INLB-LAES                           VALUE '1'.                   
018200     88  INLA-LAES                           VALUE '2'.                   
018300     88  INLA-SEQ-LAES                       VALUE '3'.                   
018400                                                                          
018500 77  FOERSTA-RAD-UPD-SW          PIC X       VALUE 'J'.                   
018600     88  FOERSTA-RAD-UPD                     VALUE 'J'.                   
018700                                                                          
018800 77  OMSTART-SW                  PIC X       VALUE 'N'.                   
018900     88  OMSTART                             VALUE 'J'.                   
019000                                                                          
019100 77  LOSSLISTA-SW                PIC X       VALUE 'J'.                   
019200     88  LOSSLISTA                           VALUE 'J'.                   
019300                                                                          
019400 77  ILISTA-SW                   PIC X       VALUE 'N'.                   
019500     88  ILISTA                              VALUE 'J'.                   
019600                                                                          
019700 77  TAB-TRAEFF-SW               PIC X       VALUE 'N'.                   
019800     88  TAB-TRAEFF                          VALUE 'J'.                   
019900     88  EJ-TAB-TRAEFF                       VALUE 'N'.                   
020000                                                                          
020100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
020200     88  EGEN-MID                            VALUE '6115'.                
020300     88  GODK-MID                            VALUE '6111' '6112'          
020400                                                   '6113' '6114'          
020500                                                   '6115' '6116'          
020600                                                   '6118' '6119'.         
020700     88  HELP-MID                            VALUE '0551'.                
020800     EJECT                                                                
020900*      --- VALID IDDC CODES                                               
021000*                                                                         
021100*01    -COPY WWDCKONS                                                     
021200*01    -COPY WWDC99                                                       
021300*01    -COPY WWDC99 -PRE SW-                                              
021400       EJECT                                                              
021500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
021600 01  GENERELLA-SUBPROGRAM.                                                
021700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
021800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
021900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
022000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
022100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
022200     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR '.            
022300     03  CHECK                   PIC X(8)    VALUE 'CHECK   '.            
022400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
022500     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
022600     03  W6011510                PIC X(8)    VALUE 'W6011510'.            
022700     EJECT                                                                
022800*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
022900*01 -COPY WDATAREA                                                        
023000     EJECT                                                                
023100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
023200*01 -COPY WMEDAREA                                                        
023300     EJECT                                                                
023400*01  -COPY WMSGINIT                                                       
023500     EJECT                                                                
023600 01  FILLER             PIC X(16) VALUE 'WORKAREA     '.                  
023700*01   -COPY WORKAREA.                                                     
023800     EJECT                                                                
023900*                                                                         
024000 77  RKOD-ABEND                  PIC S9(4)   VALUE +33 COMP SYNC.         
024100*                                                                         
024200 01  TABENTRY-PARM.                                                       
024300     03  TABENTRY-LNGD           PIC S9(9)   COMP.                        
024400     03  ANTAL-ENTRY             PIC S9(9)   COMP.                        
024500     03  SORTBGP-LNGD            PIC S9(9)   COMP.                        
024600                                                                          
024700*----TABELL FÖR SKAPANDE AV INLÄGGNINGSLISTA                              
024800 01  INL-TAB.                                                             
024900     03  INL-POST  OCCURS 150.                                            
025000         05  I-TAB-SORTNYCKEL.                                            
025100           07  I-TAB-ADLAGOMR     PIC S9(3)   VALUE ZERO COMP-3.          
025200           07  I-TAB-TORG         PIC  X(4)   VALUE '0000'.               
025300           07  I-TAB-ADGANG       PIC S9(3)   VALUE ZERO COMP-3.          
025400           07  I-TAB-ADPLATS      PIC S9(5)   VALUE ZERO COMP-3.          
025500           07  I-TAB-IDARTNR      PIC S9(9)   VALUE ZERO COMP-3.          
025600           07  I-TAB-IDLEVNR-KOLLI PIC X(5)   VALUE SPACE.                
025700           07  I-TAB-IDOKOLLI     PIC  9(9)   VALUE ZERO.                 
025800         05  I-TAB-IDLEVNR        PIC  X(5)   VALUE SPACE.                
025900         05  I-TAB-IDFS           PIC  X(8)   VALUE SPACE.                
026000         05  I-TAB-TIAVIDAT       PIC S9(7)   VALUE ZERO COMP-3.          
026100         05  I-TAB-IDRADNR-INL    PIC S9(5)   VALUE ZERO COMP-3.          
026200         05  I-TAB-IDRADNR        PIC S9(5)   VALUE ZERO COMP-3.          
026300         05  I-TAB-IDILIST        PIC  9(5)   VALUE ZERO.                 
026400         05  I-TAB-IDILIRAD       PIC S9(5)   VALUE ZERO COMP-3.          
026500         05  I-TAB-IDPRTLST       PIC  X(8)   VALUE SPACE.                
026600*                                                                         
026700 SKIP2                                                                    
026800*----TOMTABELL FÖR INLÄGGNINGSLISTETABELL                                 
026900 01  INL-TAB-TOM.                                                         
027000     03  INL-TOM-POST  OCCURS 150.                                        
027100         05  I-TOM-SORTNYCKEL.                                            
027200           07  I-TOM-ADLAGOMR     PIC S9(3)   VALUE ZERO COMP-3.          
027300           07  I-TOM-TORG         PIC  X(4)   VALUE '0000'.               
027400           07  I-TOM-ADGANG       PIC S9(3)   VALUE ZERO COMP-3.          
027500           07  I-TOM-ADPLATS      PIC S9(5)   VALUE ZERO COMP-3.          
027600           07  I-TOM-IDARTNR      PIC S9(9)   VALUE ZERO COMP-3.          
027700           07  I-TOM-IDLEVNR-KOLLI PIC X(5)   VALUE SPACE.                
027800           07  I-TOM-IDOKOLLI     PIC  9(9)   VALUE ZERO.                 
027900         05  I-TOM-IDLEVNR        PIC X(5)    VALUE SPACE.                
028000         05  I-TOM-IDFS           PIC  X(8)   VALUE SPACE.                
028100         05  I-TOM-TIAVIDAT       PIC S9(7)   VALUE ZERO COMP-3.          
028200         05  I-TOM-IDRADNR-INL    PIC S9(5)   VALUE ZERO COMP-3.          
028300         05  I-TOM-IDRADNR        PIC S9(5)   VALUE ZERO COMP-3.          
028400         05  I-TOM-IDILIST        PIC  9(5)   VALUE ZERO.                 
028500         05  I-TOM-IDILIRAD       PIC S9(5)   VALUE ZERO COMP-3.          
028600         05  I-TOM-IDPRTLST       PIC  X(8)   VALUE SPACE.                
028700*                                                                         
028800 EJECT                                                                    
028900 01  ANTAL-I-TABELL              PIC S9(9)   COMP VALUE ZERO.             
029000     EJECT                                                                
029100 01  FILLER                      PIC X(16) VALUE 'MEDDELANDEN'.           
029200     SKIP2                                                                
029300 01  MESSAGE-CODES.                                                       
029400     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
029500     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
029600     03  ERR-KEYS-MISSING        PIC X(3)    VALUE '005'.                 
029700     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
029800     03  ERR-NO-UPDATE           PIC X(3)    VALUE '007'.                 
029900     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
030000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
030100     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
030200     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
030300     03  INF-ALREADY-LAST        PIC X(3)    VALUE '115'.                 
030400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
030500     03  ERR-INVALID-VALUE       PIC X(3)    VALUE '492'.                 
030600     03  ERR-WRONG-PRINTER       PIC X(3)    VALUE '772'.                 
030700                                                                          
030800 01 WS-IDMSG-ERROR                 PIC X(3).                              
030900    88 R-ERR-UPDATE-NOT-ALLOWED    VALUE '007'.                           
031000    88 R-CORR-HILITE-FLDS          VALUE '020'.                           
031100    88 R-ERR-WRONG-KEY             VALUE '022'.                           
031200    88 R-ERR-NOT-FOUND             VALUE '025'.                           
031300    88 R-ERR-WRONG-PRINTER         VALUE '347'.                           
031400    88 R-ERR-KEYS-MISSING          VALUE '348'.                           
031500    88 PF11-AND-NO-DATA            VALUE '014'.                           
031600                                                                          
031700 01 WS-IDMSG-INFO                  PIC X(3).                              
031800    88 FIRST-PAGE                  VALUE '010'.                           
031900    88 INF-MORE-LINES-EXISTS       VALUE '011'.                           
032000    88 PRESS-PF11                  VALUE '013'.                           
032100    88 NO-MORE-LINES               VALUE '316'.                           
032200    88 UPDATE-DONE                 VALUE '001'.                           
032300    88 R-INF-ALREADY-LAST-PAGE     VALUE '346'.                           
032400                                                                          
032500     EJECT                                                                
032600                                                                          
032700 01  INF-UTSKR-TEXT.                                                      
032800     03  FILLER                  PIC X(12) VALUE 'AR UTSKRIVNA'.          
032900     03  FILLER                  PIC X(12) VALUE 'RR PRINTED  '.          
033000 01  FILLER REDEFINES INF-UTSKR-TEXT.                                     
033100     03  UTSKR-TEXT OCCURS 2     PIC X(12).                               
033200                                                                          
033300     EJECT                                                                
033400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
033500*                                                                         
033600 01  FILLER                    PIC X(16) VALUE 'SPAR-W6D111-AREA'.        
033700     SKIP3                                                                
033800*01  -COPY W6D111 -PRE SPAR-                                              
033900     EJECT                                                                
034000 01  FILLER                    PIC X(16) VALUE 'SPAR-W6D121-AREA'.        
034100     SKIP3                                                                
034200*01  -COPY W6D121 -PRE SPAR-                                              
034300     EJECT                                                                
034400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
034500     SKIP3                                                                
034600*01  MID -COPY W6I11501                                                   
034700     EJECT                                                                
034800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
034900     SKIP3                                                                
035000*01  -COPY WMSGAREA                                                       
035100     EJECT                                                                
035200     03  MOD REDEFINES MSG-AREA.                                          
035300*      05  -COPY W6O11501                                                 
035400     EJECT                                                                
035500 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
035600     SKIP3                                                                
035700******************************************************************        
035800*                                                                         
035900*                AREOR FÖR ANROP TILL W6011510                            
036000*                                                                         
036100 01  FILLER                    PIC X(16) VALUE 'REQU-AREA'.               
036200 01  REQU-AREA.                                                           
036300     03 -COPY WZ01REQU                                                    
036400     03 -COPY W60115I1                                                    
036500                                                                          
036600 01  FILLER                    PIC X(16) VALUE 'RESP-AREA'.               
036700 01  RESP-AREA.                                                           
036800     03 -COPY WZ01RESP                                                    
036900     03 -COPY W60115O1                                                    
037000                                                                          
037100 77  MAX-KVRADER                 PIC S9(4)   VALUE +10 COMP.              
037200                                                                          
037300******************************************************************        
037400     SKIP3                                                                
037500 01  KOM-MSG-IO-AREA.                                                     
037600*03  -COPY WMSGKOM                                                        
037700     EJECT                                                                
038800******************************************************************        
038900     SKIP3                                                                
039000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
039100     SKIP3                                                                
039200*01  -COPY WMFSAREA                                                       
039300     EJECT                                                                
039400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
039500*                                                                         
039600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
039700     SKIP3                                                                
039800*    --- STATUS-KOD FRÅN IMS                                              
039900 01  STATUS-WS                   PIC XX.                                  
040000     88  SEGMENT-FINNS                       VALUE '  '.                  
040100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
040200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
040300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
040400     SKIP2                                                                
040500 01  GODK-STATUSKODER.                                                    
040600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
040700     SKIP3                                                                
040800 01  SSA1                        PIC X(192).                              
040900 01  SSA2                        PIC X(64).                               
041000 01  SSA3                        PIC X(64).                               
041100     EJECT                                                                
041200*    --- IMS FUNKTIONSKODER                                               
041300*01  -COPY W0003                                                          
041400     EJECT                                                                
041500 LINKAGE SECTION.                                                         
041600                                                                          
041700                                                                          
041800 01 -COPY W0009     -PRE MSG-                                             
042100                                                                          
042200 01  DISP-PCB                    PIC X.                                   
042300                                                                          
042400 01  STAT-PCB                    PIC X.                                   
042500                                                                          
042600 01  AR-PCB                      PIC X.                                   
042700                                                                          
042800 01  FR-PCB                      PIC X.                                   
042900                                                                          
043000 01  ILIST-PCB                   PIC X.                                   
                                                                                
       01  SYNQ-PCB                    PIC X.                                   
043100                                                                          
043200 01  USEA-PCB                    PIC X.                                   
043300                                                                          
043400 01  ARTC-PCB                    PIC X.                                   
043500                                                                          
043600 01  WDB6-PCB                    PIC X.                                   
043700                                                                          
043800 01  INLA1-PCB                   PIC X.                                   
043900                                                                          
044000 01  INLA2-PCB                   PIC X.                                   
044100                                                                          
044200 01  INLB-PCB                    PIC X.                                   
044300                                                                          
044400 01  INLA3-PCB                   PIC X.                                   
044500                                                                          
044600 01  INLA-D-PCB                  PIC X.                                   
044700                                                                          
044800 01  W6D1-I-PCB                  PIC X.                                   
044900                                                                          
045000 01  PLAA-PCB                    PIC X.                                   
045100                                                                          
045200 01  LASA-PCB                    PIC X.                                   
045300                                                                          
045400 01  LOPA-PCB                    PIC X.                                   
045500                                                                          
045600 01  ARTS-PCB                    PIC X.                                   
045700                                                                          
045800 01  WDJ1-PCB                    PIC X.                                   
045900                                                                          
046000 01  WDJ2-PCB                    PIC X.                                   
046100                                                                          
046200 01  INLA4-PCB                   PIC X.                                   
046300                                                                          
046400 01  WDK9-PCB                    PIC X.                                   
046500                                                                          
046600 01  WDL2-PCB                    PIC X.                                   
046700                                                                          
046800 01  WDD9-PCB                    PIC X.                                   
046900                                                                          
047000 01  WDD8-PCB                    PIC X.                                   
047100                                                                          
047200*    PCB'ER FÖR SUBPGM                                                    
047300                                                                          
047400 01 STYR-HANB-PCB                PIC X.                                   
047500                                                                          
047600 01 STYR-PLAA-PCB                PIC X.                                   
047700                                                                          
047800 01 PRIO-INLA-PCB                PIC X.                                   
047900                                                                          
048000 01 PRIO-ARTC-PCB                PIC X.                                   
048100                                                                          
048200 01 PRIO-ARTM-PCB                PIC X.                                   
048300                                                                          
048400 01 PRIO-ORDQ-PCB                PIC X.                                   
048500                                                                          
048600 01 PRIO-ARTS-PCB                PIC X.                                   
048700                                                                          
048800 01 PRIO-KVAI-PCB                PIC X.                                   
048900                                                                          
049000 01 PRIO-INLI1-PCB               PIC X.                                   
049100                                                                          
049200 01 PRIO-KVAE-PCB                PIC X.                                   
049300                                                                          
049400 01 KOM-KOMA-PCB                 PIC X.                                   
049500                                                                          
049600 01 KVAL-ARTC-PCB                PIC X.                                   
049700                                                                          
049800 01 KVAL-KVAH1-PCB               PIC X.                                   
049900                                                                          
050000 01 KVAL-KVAH2-PCB               PIC X.                                   
050100                                                                          
050200 01 KVAL-KVAG-PCB                PIC X.                                   
050300                                                                          
050400 01 KVAL-LEVA-PCB                PIC X.                                   
050500                                                                          
050600 01 KVAL-UPFA-PCB                PIC X.                                   
050700                                                                          
050800 01 KVAL-PROA-PCB                PIC X.                                   
050900                                                                          
051000 01 KVAL-XXLA-PCB                PIC X.                                   
051100                                                                          
051200 01 KVAL-KODA-PCB                PIC X.                                   
051300                                                                          
051400 01 BEHOV-WDK6-PCB               PIC X.                                   
051500                                                                          
051600 01 BEHOV-WDK7-PCB               PIC X.                                   
051700                                                                          
051800 01 BEHOV-WDR2-PCB               PIC X.                                   
051900                                                                          
052000 01 BEHOV-WDE3-PCB               PIC X.                                   
052100                                                                          
052200 01 BEHOV-WDK6-2-PCB             PIC X.                                   
052300                                                                          
052400 01 BEHOV-WDK7-2-PCB             PIC X.                                   
052500                                                                          
052600 01 BEHOV-WDL6-PCB               PIC X.                                   
052700                                                                          
052800 01 BEHOV-WDD7A-PCB              PIC X.                                   
052900                                                                          
053000 01 BEHOV-WDB6-PCB               PIC X.                                   
053100                                                                          
053200 01 W22222-WDK6-PCB              PIC X.                                   
053300                                                                          
053400 01 W22222-WDK7-PCB              PIC X.                                   
053500                                                                          
053600 01 W22222-ARTM-PCB              PIC X.                                   
053700                                                                          
053800 01 W22222-2501-PCB              PIC X.                                   
053900                                                                          
054000 01 W22222-WDB6-PCB              PIC X.                                   
054100                                                                          
054200 01 W22222-WDD7-PCB              PIC X.                                   
054300                                                                          
054400 01 W22222-WDK7E-PCB             PIC X.                                   
054500                                                                          
054600 01 W6D2-PCB                     PIC X.                                   
                                                                                
       01 SYNQ-ATAB-PCB                PIC X.                                   
                                                                                
       01 WDQ3-PCB                     PIC X.                                   
054700                                                                          
054800     EJECT                                                                
054900                                                                          
055000 PROCEDURE DIVISION  USING MSG-PCB DISP-PCB STAT-PCB                      
055100                           AR-PCB FR-PCB ILIST-PCB SYNQ-PCB               
                                 USEA-PCB                                       
055200                           ARTC-PCB WDB6-PCB                              
055300                           INLA1-PCB INLA2-PCB INLB-PCB INLA3-PCB         
055400                           INLA-D-PCB W6D1-I-PCB                          
055500                           PLAA-PCB LASA-PCB LOPA-PCB ARTS-PCB            
055600                           WDJ1-PCB WDJ2-PCB INLA4-PCB                    
055700                           WDK9-PCB WDL2-PCB WDD9-PCB WDD8-PCB            
055800                           STYR-HANB-PCB STYR-PLAA-PCB                    
055900                           PRIO-INLA-PCB PRIO-ARTC-PCB                    
056000                           PRIO-ARTM-PCB PRIO-ORDQ-PCB                    
056100                           PRIO-ARTS-PCB PRIO-KVAI-PCB                    
056200                           PRIO-INLI1-PCB                                 
056300                           PRIO-KVAE-PCB                                  
056400                           KOM-KOMA-PCB                                   
056500                           KVAL-ARTC-PCB  KVAL-KVAH1-PCB                  
056600                           KVAL-KVAH2-PCB KVAL-KVAG-PCB                   
056700                           KVAL-LEVA-PCB  KVAL-UPFA-PCB                   
056800                           KVAL-PROA-PCB  KVAL-XXLA-PCB                   
056900                           KVAL-KODA-PCB                                  
057000                           BEHOV-WDK6-PCB BEHOV-WDK7-PCB                  
057100                           BEHOV-WDR2-PCB BEHOV-WDE3-PCB                  
057200                           BEHOV-WDK6-2-PCB BEHOV-WDK7-2-PCB              
057300                           BEHOV-WDL6-PCB BEHOV-WDD7A-PCB                 
057400                           BEHOV-WDB6-PCB                                 
057500                           W22222-WDK6-PCB W22222-WDK7-PCB                
057600                           W22222-ARTM-PCB W22222-2501-PCB                
057700                           W22222-WDB6-PCB W22222-WDD7-PCB                
057800                           W22222-WDK7E-PCB W6D2-PCB                      
                                 SYNQ-ATAB-PCB WDQ3-PCB.                        
057900                                                                          
058000     ENTRY 'DLITCBL' USING MSG-PCB DISP-PCB STAT-PCB                      
058100                           AR-PCB FR-PCB ILIST-PCB SYNQ-PCB               
058200                           USEA-PCB ARTC-PCB WDB6-PCB                     
058300                           INLA1-PCB INLA2-PCB INLB-PCB INLA3-PCB         
058400                           INLA-D-PCB W6D1-I-PCB                          
058500                           PLAA-PCB LASA-PCB LOPA-PCB ARTS-PCB            
058600                           WDJ1-PCB WDJ2-PCB INLA4-PCB                    
058700                           WDK9-PCB WDL2-PCB WDD9-PCB WDD8-PCB            
058800                           STYR-HANB-PCB STYR-PLAA-PCB                    
058900                           PRIO-INLA-PCB PRIO-ARTC-PCB                    
059000                           PRIO-ARTM-PCB PRIO-ORDQ-PCB                    
059100                           PRIO-ARTS-PCB PRIO-KVAI-PCB                    
059200                           PRIO-INLI1-PCB                                 
059300                           PRIO-KVAE-PCB                                  
059400                           KOM-KOMA-PCB                                   
059500                           KVAL-ARTC-PCB  KVAL-KVAH1-PCB                  
059600                           KVAL-KVAH2-PCB KVAL-KVAG-PCB                   
059700                           KVAL-LEVA-PCB  KVAL-UPFA-PCB                   
059800                           KVAL-PROA-PCB  KVAL-XXLA-PCB                   
059900                           KVAL-KODA-PCB                                  
060000                           BEHOV-WDK6-PCB BEHOV-WDK7-PCB                  
060100                           BEHOV-WDR2-PCB BEHOV-WDE3-PCB                  
060200                           BEHOV-WDK6-2-PCB BEHOV-WDK7-2-PCB              
060300                           BEHOV-WDL6-PCB BEHOV-WDD7A-PCB                 
060400                           BEHOV-WDB6-PCB                                 
060500                           W22222-WDK6-PCB W22222-WDK7-PCB                
060600                           W22222-ARTM-PCB W22222-2501-PCB                
060700                           W22222-WDB6-PCB W22222-WDD7-PCB                
060800                           W22222-WDK7E-PCB W6D2-PCB                      
                                 SYNQ-ATAB-PCB WDQ3-PCB.                        
060900                                                                          
061000     PERFORM IMS-GET-MSG                                                  
061100     IF SEGMENT-FINNS                                                     
061200       PERFORM A-INIT                                                     
061300       PERFORM B-INIT-KEYS                                                
061400       PERFORM C-INIT-REQU                                                
                                                                                
             IF MFS-UPD-X                                                       
                SET REQU-UPD-X TO TRUE                                          
             ELSE                                                               
061500         IF MFS-UPDATE                                                    
061600           SET REQU-UPDATE TO TRUE                                        
061700         ELSE                                                             
061800           IF MFS-FIRST                                                   
061900             SET REQU-FIRST TO TRUE                                       
062000             PERFORM D-FIRST-PAGE                                         
062100           ELSE                                                           
062200             IF MFS-NEXT                                                  
062300               SET REQU-NEXT TO TRUE                                      
062400               PERFORM E-NEXT-PAGE                                        
062500             ELSE                                                         
062600               SET REQU-QUERY TO TRUE                                     
062700               PERFORM F-SAME-PAGE                                        
062800             END-IF                                                       
062900           END-IF                                                         
063000         END-IF                                                           
             END-IF                                                             
                                                                                
063100       PERFORM G-CALL-BIZ-LOGIC-W6011510                                  
063200                                                                          
             IF NOT REQU-UPD-X                                                  
064300          COMPUTE MSG-KVLL = MAX-MOD-LAENGD +                             
064400                             LNG-P-TO-P-PREFIX                            
064500          PERFORM IMS-INSERT-MSG                                          
             END-IF                                                             
                                                                                
064700     END-IF                                                               
064800                                                                          
064900     MOVE ZERO TO RETURN-CODE                                             
065000     GOBACK                                                               
065100     .                                                                    
065200     EJECT                                                                
065300 A-INIT SECTION.                                                          
065400                                                                          
065500     ACCEPT DAGENS-DATUM       FROM DATE                                  
065600     ACCEPT DAGENS-TID         FROM TIME                                  
065700                                                                          
065800     IF MSG-DUBBLA-TRANSKODER                                             
065900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I11501                 
066000       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
066100       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
066200     ELSE                                                                 
066300       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W6I11501                 
066400       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
066500       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
066600     END-IF                                                               
066700                                                                          
066800     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
066900     MOVE MSG-IDPFK            TO MFS-IDPFK                               
067000     MOVE MFS-IDTRANS          TO W-IDTRANS                               
067100                                                                          
067200     MOVE LOW-VALUE            TO MSG-AREA                                
067300     MOVE 'W6O115N1'           TO MFS-IDMOD                               
067400     MOVE '6115'               TO MOD-IDTRANS                             
067500     MOVE MFS-RENSA-FAELT      TO MOD-TEMFSFEL MOD-TEMFSINF               
067600     MOVE +10                  TO MAX-KVRADER                             
067700                                                                          
067800     IF EGEN-MID OR HELP-MID OR MFS-UPD-X                                 
067900       CONTINUE                                                           
068000     ELSE                                                                 
068100       MOVE SPACE              TO MFS-KDTRTYP                             
068200       MOVE '7'                TO MFS-IDPFK                               
068300     END-IF                                                               
068400                                                                          
068500     MOVE 'IDAG'               TO DAT-KDDATFORM                           
068600     CALL WDATKONV USING          DAT-KDDATFORM                           
068700                                  DAT-I-TIDATUM                           
068800                                  DAT-O-TIDATUM                           
068900                                  DAT-KDSVAR                              
069000                                                                          
069100     SET ADINLOMR-IX           TO 1                                       
069200     PERFORM UNTIL ADINLOMR-IX >  MAX-ADINLOMR-IX                         
069300       MOVE SPACE              TO W-ADINLOMR        (ADINLOMR-IX)         
069400       MOVE ZERO               TO W-VLARTNTO-ADINLOMR(ADINLOMR-IX)        
069500       SET ADINLOMR-IX UP BY 1                                            
069600     END-PERFORM                                                          
069700                                                                          
069800     MOVE JA                   TO INDATA-SW                               
069900                                                                          
070000     MOVE NEJ                  TO OMSTART-SW                              
070100     PERFORM AA-INIT-NYCKLAR                                              
070200                                                                          
070300     MOVE MSGI-IDTIDZON        TO REQU-IDTIDZON                           
070400                                                                          
070500     IF MSGI-IDLAND-SPR = 'GB'                                            
070600       MOVE +2 TO SPRAK-IX REQU-IDLAND-SPR                                
070700       MOVE 'GB ' TO MED-IDSKYLT                                          
070800     ELSE                                                                 
070900       MOVE +1 TO SPRAK-IX REQU-IDLAND-SPR                                
071000       MOVE 'S  ' TO MED-IDSKYLT                                          
071100     END-IF                                                               
071200     .                                                                    
071300     EJECT                                                                
071400 AA-INIT-NYCKLAR SECTION.                                                 
071500                                                                          
071600     MOVE ALL '+' TO MSGI-WMSGINIT                                        
071700     MOVE '001'                  TO MSGI-KDCALL                           
071800     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
071900     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
072000     MOVE '6115'                 TO MSGI-IDTRANS                          
072100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
072200     .                                                                    
072300     EJECT                                                                
072400 B-INIT-KEYS     SECTION.                                                 
072500     PERFORM BA-KOLLA-IDLEVNR                                             
072600                                                                          
072700     PERFORM BB-KOLLA-IDFS                                                
072800     PERFORM BC-KOLLA-TIAVIDAT                                            
072900     PERFORM BD-KOLLA-IDLBBET                                             
073000                                                                          
073100     PERFORM BE-KOLLA-ADINLOMR-PRT                                        
073200     PERFORM BF-KOLLA-FLYTTA-IDDC                                         
073300                                                                          
073400     IF MID-KVUTSKR-AR = ALL '+'                                          
073500       MOVE ZERO           TO W-KVUTSKR-AR                                
073600     ELSE                                                                 
073700       IF EGEN-MID                                                        
073800         MOVE MID-KVUTSKR-AR TO W-KVUTSKR-AR                              
073900       ELSE                                                               
074000         MOVE ZERO         TO W-KVUTSKR-AR                                
074100       END-IF                                                             
074200     END-IF                                                               
074300                                                                          
074400*FÖR NDC:ERNAS SKULL HÄMTAR MAN LOKAL TID MHA ETT ANROP TILL              
074500*W005INIT. DETTA SKA KUNNA GÄLLA FÖR SAMTLIGA DC:N ÄVEN CDC               
074600                                                                          
074700     PERFORM BG-FLYTTA-OEVRIGA-NYCKLAR                                    
074800                                                                          
074900     IF GODK-MID OR HELP-MID                                              
075000       MOVE WS-IDLEVNR         TO MOD-IDLEVNR-UT                          
075100       MOVE WS-IDFS            TO MOD-IDFS-UT                             
075200       MOVE WS-TIAVIDAT        TO MOD-TIAVIDAT-UT                         
075300       INSPECT MOD-TIAVIDAT-UT REPLACING LEADING ZERO BY SPACE            
075400       MOVE WS-IDLBBET         TO MOD-IDLBBET-UT                          
075500       MOVE WS-ADINLOMR-PRT    TO MOD-ADINLOMR-PRT-UT                     
075600       MOVE WS-IDDC            TO MOD-IDDC-UT                             
075700     ELSE                                                                 
075800       MOVE MFS-RENSA-FAELT    TO MOD-IDLEVNR-UT                          
075900                                  MOD-IDFS-UT                             
076000                                  MOD-TIAVIDAT-UT                         
076100                                  MOD-IDLBBET-UT                          
076200                                  MOD-ADINLOMR-PRT-UT                     
076300                                  MOD-IDDC-UT                             
076400**     + ÖVRIGA NYCKLAR (ANVÄNDS INTE LÄNGRE)                             
076500                                  MOD-FLKLIVIS-UT                         
076600                                  MOD-KDRT-UT                             
076700                                  MOD-IDLOPNRM-UT                         
076800     END-IF                                                               
076900*                                                                         
077000     IF MID-IDLBBET-IN NOT = ALL '+'                                      
077100       MOVE ZERO                 TO REQU-IDLEVNR-KEY                      
077200       MOVE ZERO                 TO REQU-IDFS-KEY                         
077300       MOVE SPACE                TO REQU-TIAVIDAT-KEY                     
077400       MOVE MFS-RENSA-FAELT      TO MOD-IDLEVNR-UT                        
077500                                    MOD-IDFS-UT                           
077600                                    MOD-TIAVIDAT-UT                       
077700     ELSE                                                                 
077800       IF MID-TIAVIDAT-IN NOT = ALL '+'                                   
077900         IF WS-IDLEVNR NOT = SPACE AND WS-IDFS NOT = SPACE                
078000           MOVE MFS-RENSA-FAELT   TO MOD-IDLBBET-UT                       
078100           MOVE SPACE             TO REQU-IDLBBET-KEY                     
078200         END-IF                                                           
078300       ELSE                                                               
078400         IF MID-IDFS-IN NOT = ALL '+'                                     
078500           IF WS-IDLEVNR NOT = SPACE                                      
078600             MOVE MFS-RENSA-FAELT TO MOD-IDLBBET-UT                       
078700                                     MOD-TIAVIDAT-UT                      
078800             MOVE ZERO            TO REQU-TIAVIDAT-KEY                    
078900             MOVE SPACE           TO REQU-IDLBBET-KEY                     
079000           END-IF                                                         
079100         ELSE                                                             
079200           IF MID-IDLEVNR-IN NOT = ALL '+'                                
079300             MOVE MFS-RENSA-FAELT      TO MOD-IDLBBET-UT                  
079400                                        MOD-TIAVIDAT-UT                   
079500                                        MOD-IDFS-UT                       
079600             MOVE ZERO            TO REQU-TIAVIDAT-KEY                    
079700             MOVE SPACE           TO REQU-IDLBBET-KEY                     
079800             MOVE ZERO            TO REQU-IDFS-KEY                        
079900           END-IF                                                         
080000         END-IF                                                           
080100       END-IF                                                             
080200     END-IF                                                               
080300     .                                                                    
080400     EJECT                                                                
080500 BA-KOLLA-IDLEVNR   SECTION.                                              
080600                                                                          
080700     MOVE MFS-RENSA-FAELT      TO MOD-IDLEVNR-IN                          
080800                                                                          
080900     IF MID-IDLEVNR-IN         = ALL '+'                                  
081000       MOVE MID-IDLEVNR-UT     TO WS-IDLEVNR                              
081100                                  REQU-IDLEVNR-KEY                        
081200     ELSE                                                                 
081300       MOVE MID-IDLEVNR-IN     TO WS-IDLEVNR                              
081400                                  REQU-IDLEVNR-KEY                        
081500       MOVE '7'                TO MFS-IDPFK                               
081600       MOVE SPACE              TO MFS-KDTRTYP                             
081700     END-IF                                                               
081800     .                                                                    
081900     EJECT                                                                
082000 BB-KOLLA-IDFS      SECTION.                                              
082100                                                                          
082200     MOVE MFS-RENSA-FAELT      TO MOD-IDFS-IN                             
082300                                                                          
082400     IF MID-IDFS-IN            =  ALL '+'                                 
082500       MOVE MID-IDFS-UT        TO WS-IDFS                                 
082600                                  REQU-IDFS-KEY                           
082700     ELSE                                                                 
082800       MOVE MID-IDFS-IN        TO WS-IDFS                                 
082900                                  REQU-IDFS-KEY                           
083000       MOVE '7'                TO MFS-IDPFK                               
083100       MOVE SPACE              TO MFS-KDTRTYP                             
083200     END-IF                                                               
083300     .                                                                    
083400     EJECT                                                                
083500 BC-KOLLA-TIAVIDAT  SECTION.                                              
083600                                                                          
083700     MOVE MFS-RENSA-FAELT      TO MOD-TIAVIDAT-IN                         
083800                                                                          
083900     IF MID-TIAVIDAT-IN        = ALL '+'                                  
084000       MOVE MID-TIAVIDAT-UT    TO WS-TIAVIDAT                             
084100                                  REQU-TIAVIDAT-KEY                       
084200       INSPECT REQU-TIAVIDAT-KEY REPLACING                                
084300               LEADING SPACE BY ZERO                                      
084400     ELSE                                                                 
084500       MOVE MID-TIAVIDAT-IN    TO WS-TIAVIDAT                             
084600                                  REQU-TIAVIDAT-KEY                       
084700       MOVE '7'                TO MFS-IDPFK                               
084800       MOVE SPACE              TO MFS-KDTRTYP                             
084900     END-IF                                                               
085000     .                                                                    
085100     EJECT                                                                
085200                                                                          
085300 BD-KOLLA-IDLBBET   SECTION.                                              
085400                                                                          
085500     IF EGEN-MID OR HELP-MID OR MFS-UPD-X                                 
085600         MOVE MFS-RENSA-FAELT  TO MOD-IDLBBET-IN                          
085700                                                                          
085800         IF MID-IDLBBET-IN     = ALL '+'                                  
085900           IF MID-IDLBBET-UT > SPACE                                      
086000             MOVE MID-IDLBBET-UT TO WS-IDLBBET                            
086100                                    REQU-IDLBBET-KEY                      
086200           ELSE                                                           
086300             MOVE SPACE          TO WS-IDLBBET                            
086400                                    REQU-IDLBBET-KEY                      
086500           END-IF                                                         
086600         ELSE                                                             
086700           MOVE MID-IDLBBET-IN TO WS-IDLBBET                              
086800                                  REQU-IDLBBET-KEY                        
086900           MOVE '7'            TO MFS-IDPFK                               
087000           MOVE SPACE          TO MFS-KDTRTYP                             
087100         END-IF                                                           
087200                                                                          
087300      ELSE                                                                
087400         MOVE SPACE            TO WS-IDLBBET                              
087500                                  REQU-IDLBBET-KEY                        
087600         MOVE ALL '+'          TO MID-IDLBBET-IN                          
087700     END-IF                                                               
087800     .                                                                    
087900     EJECT                                                                
088000 BE-KOLLA-ADINLOMR-PRT  SECTION.                                          
088100                                                                          
088200     MOVE MFS-RENSA-FAELT           TO MOD-ADINLOMR-PRT-IN                
088300                                                                          
088400     IF MID-ADINLOMR-PRT-IN         =  ALL '+'                            
088500       MOVE MID-ADINLOMR-PRT-UT     TO WS-ADINLOMR-PRT                    
088600                                       REQU-ADINLOMR-PRT-KEY              
088700     ELSE                                                                 
088800       MOVE MID-ADINLOMR-PRT-IN     TO WS-ADINLOMR-PRT                    
088900                                       REQU-ADINLOMR-PRT-KEY              
089000     END-IF                                                               
089100     .                                                                    
089200     EJECT                                                                
089300 BF-KOLLA-FLYTTA-IDDC  SECTION.                                           
089400     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
089500     MOVE     SPACE       TO WS-IDDC                                      
089600                                                                          
089700     IF EGEN-MID OR HELP-MID OR MFS-UPD-X                                 
089800       IF MID-IDDC-IN = ALL '+'                                           
089900         MOVE MSGI-IDDC   TO WS-IDDC                                      
090000                             REQU-IDDC-KEY                                
090100       ELSE                                                               
090200         MOVE MID-IDDC-IN TO WS-IDDC                                      
090300                             REQU-IDDC-KEY                                
090400         MOVE     '7'     TO MFS-IDPFK                                    
090500         MOVE    SPACE    TO MFS-KDTRTYP                                  
090600       END-IF                                                             
090700     ELSE                                                                 
090800       IF GODK-MID                                                        
090900         MOVE MID-IDDC-UT TO WS-IDDC                                      
091000                             REQU-IDDC-KEY                                
091100         IF GOOD-DC                                                       
091200           CONTINUE                                                       
091300         ELSE                                                             
091400           MOVE MSGI-IDDC   TO WS-IDDC                                    
091500                             REQU-IDDC-KEY                                
091600         END-IF                                                           
091700       ELSE                                                               
091800         MOVE MSGI-IDDC   TO WS-IDDC                                      
091900                             REQU-IDDC-KEY                                
092000       END-IF                                                             
092100     END-IF                                                               
092200     .                                                                    
092300     EJECT                                                                
092400                                                                          
092500 BG-FLYTTA-OEVRIGA-NYCKLAR  SECTION.                                      
092600     SKIP2                                                                
092700*    -- DESSA FÄLT ANVÄNDS INTE NUMERA                                    
092800     MOVE MFS-RENSA-FAELT      TO MOD-KDRT-IN                             
092900                                  MOD-FLKLIVIS-IN                         
093000                                  MOD-IDLOPNRM-IN                         
093100     .                                                                    
093200     EJECT                                                                
093300 C-INIT-REQU   SECTION.                                                   
093400     MOVE '001'                   TO REQU-IDMSGVER                        
093500     MOVE MSG-SIGNON-USERID       TO REQU-IDUSER                          
093600     MOVE MAX-KVRADER             TO REQU-KVRADER                         
093700     MOVE MID-OMSTART-INDX        TO REQU-OMSTART-INDX                    
093800*                                                                         
093900     IF EGEN-MID OR HELP-MID OR MFS-UPD-X                                 
094000       MOVE MID-IDLBBET-UPD     TO REQU-IDLBBET-UPD                       
094100     ELSE                                                                 
094200       MOVE SPACE               TO REQU-IDLBBET-UPD                       
094300     END-IF                                                               
094400*                                                                         
094500     MOVE W-KVUTSKR-AR            TO REQU-KVUTSKR-AR                      
094600     MOVE MID-FLKLAR-UPD          TO REQU-FLKLAR-UPD                      
094700     MOVE MID-ADINLOMR-LPL-UPD    TO REQU-ADINLOMR-LPL-UPD                
094800                                                                          
094900     MOVE +1               TO INDX                                        
095000     PERFORM UNTIL INDX > MAX-INDX                                        
095100       MOVE MID-KDCMDVAL-RAD(INDX)  TO REQU-KDCMDVAL-LINE(INDX)           
095200       MOVE MID-IDLEVNR-RAD (INDX)  TO REQU-IDLEVNR-LINE(INDX)            
095300       MOVE MID-IDFS-RAD (INDX)     TO REQU-IDFS-LINE(INDX)               
095400       MOVE MID-TIAVIDAT-RAD (INDX) TO REQU-TIAVIDAT-LINE(INDX)           
095500       ADD +1 TO INDX                                                     
095600     END-PERFORM                                                          
095700                                                                          
095800*THE REST OF THE REQU FIELDS.                                             
095900     PERFORM UNTIL INDX > MAX-REQU-INDX                                   
096000       MOVE ALL '+'        TO REQU-KDCMDVAL-LINE(INDX)                    
096100                              REQU-IDLEVNR-LINE(INDX)                     
096200                              REQU-IDFS-LINE(INDX)                        
096300                              REQU-TIAVIDAT-LINE(INDX)                    
096400       ADD +1 TO INDX                                                     
096500     END-PERFORM                                                          
096600     .                                                                    
096700     EJECT                                                                
096800                                                                          
096900 D-FIRST-PAGE SECTION.                                                    
097000     MOVE INF-FIRST-PAGE       TO MED-IDMFSINF                            
097100     CALL WMEDKONV USING MED-WMEDAREA                                     
097200     MOVE MED-TEMFSINF         TO MOD-TEMFSINF                            
097300                                                                          
097400     PERFORM MFS-RENSA-FAELT-IN                                           
097500     .                                                                    
097600     EJECT                                                                
097700 E-NEXT-PAGE SECTION.                                                     
097800     MOVE MID-IDLEVNR-NEXT        TO REQU-IDLEVNR-START                   
097900     MOVE MID-IDFS-NEXT           TO REQU-IDFS-START                      
098000     MOVE MID-TIAVIDAT-NEXT       TO REQU-TIAVIDAT-START                  
098100     MOVE MID-IDLBBET-NEXT        TO REQU-IDLBBET-START                   
098200                                                                          
098300     PERFORM MFS-RENSA-FAELT-IN                                           
098400     .                                                                    
098500     EJECT                                                                
098600 F-SAME-PAGE SECTION.                                                     
098700     MOVE MID-IDLEVNR-ENTER       TO REQU-IDLEVNR-START                   
098800     MOVE MID-IDFS-ENTER          TO REQU-IDFS-START                      
098900     MOVE MID-TIAVIDAT-ENTER      TO REQU-TIAVIDAT-START                  
099000     MOVE MID-IDLBBET-ENTER       TO REQU-IDLBBET-START                   
099100                                                                          
099200     .                                                                    
099300     EJECT                                                                
099400                                                                          
099500 G-CALL-BIZ-LOGIC-W6011510    SECTION.                                    
099600     CALL W6011510 USING                                                  
099700          REQU-AREA RESP-AREA MAX-KVRADER                                 
099800          MSG-PCB DISP-PCB STAT-PCB                                       
099900          AR-PCB FR-PCB ILIST-PCB SYNQ-PCB                                
100000          ARTC-PCB WDB6-PCB                                               
100100          INLA1-PCB INLA2-PCB INLB-PCB INLA3-PCB                          
100200          INLA-D-PCB W6D1-I-PCB                                           
100300          PLAA-PCB LASA-PCB LOPA-PCB ARTS-PCB                             
100400          WDJ1-PCB WDJ2-PCB INLA4-PCB                                     
100500          WDK9-PCB WDL2-PCB WDD9-PCB WDD8-PCB                             
100600          STYR-HANB-PCB STYR-PLAA-PCB                                     
100700          PRIO-INLA-PCB PRIO-ARTC-PCB                                     
100800          PRIO-ARTM-PCB PRIO-ORDQ-PCB                                     
100900          PRIO-ARTS-PCB PRIO-KVAI-PCB                                     
101000          PRIO-INLI1-PCB                                                  
101100          PRIO-KVAE-PCB                                                   
101200          KOM-KOMA-PCB                                                    
101300          KVAL-ARTC-PCB  KVAL-KVAH1-PCB                                   
101400          KVAL-KVAH2-PCB KVAL-KVAG-PCB                                    
101500          KVAL-LEVA-PCB  KVAL-UPFA-PCB                                    
101600          KVAL-PROA-PCB  KVAL-XXLA-PCB                                    
101700          KVAL-KODA-PCB                                                   
101800          BEHOV-WDK6-PCB BEHOV-WDK7-PCB                                   
101900          BEHOV-WDR2-PCB BEHOV-WDE3-PCB                                   
102000          BEHOV-WDK6-2-PCB BEHOV-WDK7-2-PCB                               
102100          BEHOV-WDL6-PCB BEHOV-WDD7A-PCB                                  
102200          BEHOV-WDB6-PCB                                                  
102300          W22222-WDK6-PCB W22222-WDK7-PCB                                 
102400          W22222-ARTM-PCB W22222-2501-PCB                                 
102500          W22222-WDB6-PCB W22222-WDD7-PCB                                 
102600          W22222-WDK7E-PCB W6D2-PCB                                       
                SYNQ-ATAB-PCB WDQ3-PCB                                          
102700                                                                          
102800     PERFORM GA-SET-MSG-AND-HILIGHT                                       
102900                                                                          
103000     PERFORM GB-MOVE-RESP-TO-MOD                                          
103100     .                                                                    
103200     EJECT                                                                
103300                                                                          
103400 GA-SET-MSG-AND-HILIGHT   SECTION.                                        
103500     MOVE RESP-IDMSG-ERROR TO WS-IDMSG-ERROR                              
103600     MOVE RESP-IDMSG-INFO  TO WS-IDMSG-INFO                               
103700*                                                                         
103800     MOVE SPACE            TO MED-IDMFSFEL                                
103900     MOVE SPACE            TO MED-IDMFSINF                                
104000*                                                                         
104100     IF R-ERR-WRONG-KEY                                                   
104200       MOVE NEJ                   TO INDATA-SW                            
104300       MOVE ERR-WRONG-KEY         TO MED-IDMFSFEL                         
104400     END-IF                                                               
104500                                                                          
104600     IF R-ERR-WRONG-PRINTER                                               
104700       MOVE NEJ                   TO INDATA-SW                            
104800       MOVE ERR-WRONG-PRINTER     TO MED-IDMFSFEL                         
104900     END-IF                                                               
105000                                                                          
105100     IF R-ERR-KEYS-MISSING                                                
105200       MOVE NEJ                   TO INDATA-SW                            
105300       MOVE ERR-KEYS-MISSING      TO MED-IDMFSFEL                         
105400     END-IF                                                               
105500                                                                          
105600     IF R-ERR-UPDATE-NOT-ALLOWED                                          
105700       MOVE NEJ                   TO INDATA-SW                            
105800       MOVE ERR-NO-UPDATE         TO MED-IDMFSFEL                         
105900     END-IF                                                               
106000                                                                          
106100     IF PF11-AND-NO-DATA                                                  
106200       MOVE ERR-PF11-AND-NO-DATA  TO MED-IDMFSFEL                         
106300     END-IF                                                               
106400                                                                          
106500     IF R-CORR-HILITE-FLDS                                                
106600       MOVE NEJ                   TO INDATA-SW                            
106700       MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                         
106800     END-IF                                                               
106900*                                                                         
107000     CALL WMEDKONV USING MED-WMEDAREA                                     
107100     MOVE MED-TEMFSFEL    TO MOD-TEMFSFEL                                 
107200     PERFORM MFS-ROER-EJ-FAELT-UT                                         
107300     PERFORM MFS-ROER-EJ-FAELT-IN                                         
107400*                                                                         
107500     IF FIRST-PAGE                                                        
107600       MOVE INF-FIRST-PAGE      TO MED-IDMFSINF                           
107700     END-IF                                                               
107800                                                                          
107900     IF NO-MORE-LINES                                                     
108000       MOVE INF-LAST-PAGE       TO MED-IDMFSINF                           
108100     END-IF                                                               
108200                                                                          
108300     IF R-INF-ALREADY-LAST-PAGE                                           
108400       MOVE INF-ALREADY-LAST    TO MED-IDMFSINF                           
108500     END-IF                                                               
108600                                                                          
108700     IF INF-MORE-LINES-EXISTS                                             
108800       MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                          
108900     END-IF                                                               
109000                                                                          
109100     IF PRESS-PF11                                                        
109200       MOVE INF-PRESS-PF11      TO MED-IDMFSINF                           
109300     END-IF                                                               
109400                                                                          
109500     IF UPDATE-DONE                                                       
109600       MOVE INF-UPDATE-DONE     TO MED-IDMFSINF                           
109700     END-IF                                                               
109800*                                                                         
109900     CALL WMEDKONV USING MED-WMEDAREA                                     
110000     MOVE MED-TEMFSINF  TO MOD-TEMFSINF                                   
110100*                                                                         
110200     IF INDATA-OK                                                         
110300       MOVE ALL '+'       TO MSGI-WMSGINIT                                
110400       MOVE '001'         TO MSGI-KDCALL                                  
110500       MOVE WS-IDDC       TO WS-IDDC-LOCAL-DATE                           
110600       MOVE WS-IDDC-LOCAL TO MSGI-IDUSER                                  
110700       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
110800     END-IF                                                               
110900     .                                                                    
111000     EJECT                                                                
111100                                                                          
111200 GB-MOVE-RESP-TO-MOD      SECTION.                                        
111300     MOVE RESP-IDLEVNR-START    TO MOD-IDLEVNR-ENTER                      
111400     MOVE RESP-IDLEVNR-NEXT     TO MOD-IDLEVNR-NEXT                       
111500*                                                                         
111600     MOVE RESP-IDFS-START       TO MOD-IDFS-ENTER                         
111700     MOVE RESP-IDFS-NEXT        TO MOD-IDFS-NEXT                          
111800*                                                                         
111900     MOVE RESP-TIAVIDAT-START   TO MOD-TIAVIDAT-ENTER                     
112000     MOVE RESP-TIAVIDAT-NEXT    TO MOD-TIAVIDAT-NEXT                      
112100*                                                                         
112200     MOVE RESP-IDLBBET-START    TO MOD-IDLBBET-ENTER                      
112300     MOVE RESP-IDLBBET-NEXT     TO MOD-IDLBBET-NEXT                       
112400*                                                                         
112500     MOVE RESP-OMSTART-INDX     TO MOD-OMSTART-INDX                       
112600*                                                                         
112700     MOVE +1 TO INDX                                                      
112800     IF NOT R-ERR-WRONG-KEY                                               
112900*                                                                         
113000     MOVE RESP-IDLBBET-UPD-ATTR TO MOD-IDLBBET-UPD-ATTR                   
113100     IF RESP-IDLBBET-UPD = SPACES                                         
113200         MOVE MFS-RENSA-FAELT   TO MOD-IDLBBET-UPD                        
113300     ELSE                                                                 
113400       IF RESP-IDLBBET-UPD =  ALL '+'                                     
113500         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDLBBET-UPD                       
113600       ELSE                                                               
113700         MOVE RESP-IDLBBET-UPD   TO MOD-IDLBBET-UPD                       
113800       END-IF                                                             
113900     END-IF                                                               
114000*                                                                         
114100     IF RESP-FLKLAR-UPD = ALL '+'                                         
114200       MOVE MFS-ROER-EJ-FAELT  TO MOD-FLKLAR-UPD                          
114300     ELSE                                                                 
114400       MOVE RESP-FLKLAR-UPD    TO MOD-FLKLAR-UPD                          
114500     END-IF                                                               
114600*                                                                         
114700     IF RESP-FLKLAR-UPD-ATTR > SPACE                                      
114800       MOVE RESP-FLKLAR-UPD-ATTR TO MOD-FLKLAR-UPD-ATTR                   
114900     END-IF                                                               
115000*                                                                         
115100     IF RESP-ADINLOMR-LPL-UPD = ALL '+'                                   
115200       MOVE MFS-ROER-EJ-FAELT     TO MOD-ADINLOMR-LPL-UPD                 
115300     ELSE                                                                 
115400       MOVE RESP-ADINLOMR-LPL-UPD TO MOD-ADINLOMR-LPL-UPD                 
115500     END-IF                                                               
115600*                                                                         
115700     IF RESP-ADINLOMR-LPL-UPD-ATTR > SPACE                                
115800      MOVE RESP-ADINLOMR-LPL-UPD-ATTR TO MOD-ADINLOMR-LPL-UPD-ATTR        
115900     END-IF                                                               
116000*                                                                         
116100     MOVE +1 TO INDX                                                      
116200     PERFORM UNTIL INDX > 5                                               
116300       IF RESP-ADINLOMR-LPL(INDX) = ALL '+'                               
116400         MOVE MFS-ROER-EJ-FAELT     TO MOD-ADINLOMR-LPL(INDX)             
116500       ELSE                                                               
116600         MOVE RESP-ADINLOMR-LPL(INDX) TO MOD-ADINLOMR-LPL(INDX)           
116700       END-IF                                                             
116800                                                                          
116900       IF RESP-SUPROC-LPL (INDX) = ALL '+'                                
117000         MOVE MFS-ROER-EJ-FAELT      TO MOD-SUPROC-LPL(INDX)              
117100       ELSE                                                               
117200         MOVE RESP-SUPROC-LPL (INDX) TO MOD-SUPROC-LPL(INDX)              
117300       END-IF                                                             
117400*                                                                         
117500       ADD +1  TO INDX                                                    
117600     END-PERFORM                                                          
117700*                                                                         
117800*                                                                         
117900     IF RESP-TEMFSINF > SPACE                                             
118000       MOVE RESP-TEMFSINF       TO MOD-TEMFSINF                           
118100     END-IF                                                               
118200*                                                                         
118300     IF RESP-TEMFSFEL > SPACE                                             
118400       MOVE RESP-TEMFSFEL       TO MOD-TEMFSFEL                           
118500     END-IF                                                               
118600*                                                                         
118700     IF RESP-IDELMT-ERROR > SPACE                                         
118800       MOVE RESP-IDELMT-ERROR   TO MOD-TEMFSINF                           
118900     END-IF                                                               
119000*                                                                         
119100     IF RESP-KVRADER > MAX-KVRADER                                        
119200       MOVE 10                  TO RESP-KVRADER                           
119300     END-IF                                                               
119400*                                                                         
119500     MOVE +1 TO INDX                                                      
119600     PERFORM UNTIL INDX > RESP-KVRADER                                    
119700       MOVE RESP-KDCMDVAL-LINE-ATTR (INDX)                                
119800                                TO MOD-KDCMDVAL-RAD-ATTR (INDX)           
119900       IF RESP-KDCMDVAL-LINE (INDX) = SPACES                              
120000         MOVE MFS-RENSA-FAELT   TO MOD-KDCMDVAL-RAD (INDX)                
120100       ELSE                                                               
120200         IF RESP-KDCMDVAL-LINE (INDX) = ALL '+'                           
120300           MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMDVAL-RAD (INDX)              
120400         ELSE                                                             
120500           MOVE RESP-KDCMDVAL-LINE (INDX)                                 
120600                                  TO MOD-KDCMDVAL-RAD (INDX)              
120700         END-IF                                                           
120800       END-IF                                                             
120900*                                                                         
121000       MOVE RESP-IDLEVNR-LINE-ATTR (INDX)                                 
121100                                TO MOD-IDLEVNR-RAD-ATTR (INDX)            
121200       IF RESP-IDLEVNR-LINE (INDX) = SPACES                               
121300         MOVE MFS-RENSA-FAELT   TO MOD-IDLEVNR-RAD (INDX)                 
121400       ELSE                                                               
121500         IF RESP-IDLEVNR-LINE (INDX) = ALL '+'                            
121600           MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR-RAD (INDX)               
121700         ELSE                                                             
121800           MOVE RESP-IDLEVNR-LINE (INDX)                                  
121900                                  TO MOD-IDLEVNR-RAD (INDX)               
122000         END-IF                                                           
122100       END-IF                                                             
122200*                                                                         
122300       MOVE RESP-IDFS-LINE-ATTR (INDX)                                    
122400                                TO MOD-IDFS-RAD-ATTR (INDX)               
122500       IF RESP-IDFS-LINE (INDX) = SPACES                                  
122600         MOVE MFS-RENSA-FAELT   TO MOD-IDFS-RAD (INDX)                    
122700       ELSE                                                               
122800         IF RESP-IDFS-LINE (INDX) = ALL '+'                               
122900           MOVE MFS-ROER-EJ-FAELT TO MOD-IDFS-RAD (INDX)                  
123000         ELSE                                                             
123100           MOVE RESP-IDFS-LINE (INDX)                                     
123200                                  TO MOD-IDFS-RAD (INDX)                  
123300         END-IF                                                           
123400       END-IF                                                             
123500*                                                                         
123600       MOVE RESP-FLFEL-LINE-ATTR (INDX)                                   
123700                                TO MOD-FLFEL-RAD-ATTR (INDX)              
123800       IF RESP-FLFEL-LINE (INDX) = SPACES                                 
123900         MOVE MFS-RENSA-FAELT   TO MOD-FLFEL-RAD (INDX)                   
124000       ELSE                                                               
124100         IF RESP-FLFEL-LINE (INDX) = ALL '+'                              
124200           MOVE MFS-ROER-EJ-FAELT TO MOD-FLFEL-RAD (INDX)                 
124300         ELSE                                                             
124400           MOVE RESP-FLFEL-LINE (INDX)                                    
124500                                  TO MOD-FLFEL-RAD (INDX)                 
124600         END-IF                                                           
124700       END-IF                                                             
124800*                                                                         
124900       IF RESP-TIAVIDAT-LINE (INDX) = SPACES                              
125000         MOVE MFS-RENSA-FAELT   TO MOD-TIAVIDAT-RAD (INDX)                
125100       ELSE                                                               
125200         IF RESP-TIAVIDAT-LINE (INDX) = ALL '+'                           
125300           MOVE MFS-ROER-EJ-FAELT TO MOD-TIAVIDAT-RAD (INDX)              
125400         ELSE                                                             
125500           MOVE RESP-TIAVIDAT-LINE (INDX)                                 
125600                                  TO MOD-TIAVIDAT-RAD (INDX)              
125700           INSPECT MOD-TIAVIDAT-RAD(INDX)                                 
125800           REPLACING LEADING SPACE BY ZERO                                
125900         END-IF                                                           
126000       END-IF                                                             
126100*                                                                         
126200       IF RESP-IDLBBET-LINE (INDX) = SPACES                               
126300         MOVE MFS-RENSA-FAELT   TO MOD-IDLBBET-RAD (INDX)                 
126400       ELSE                                                               
126500         IF RESP-IDLBBET-LINE (INDX) = ALL '+'                            
126600           MOVE MFS-ROER-EJ-FAELT TO MOD-IDLBBET-RAD (INDX)               
126700         ELSE                                                             
126800           MOVE RESP-IDLBBET-LINE (INDX)                                  
126900                                  TO MOD-IDLBBET-RAD (INDX)               
127000         END-IF                                                           
127100       END-IF                                                             
127200*                                                                         
127300       IF RESP-KVPARTI-LINE (INDX) = SPACES                               
127400         MOVE MFS-RENSA-FAELT   TO MOD-KVPARTI-RAD (INDX)                 
127500       ELSE                                                               
127600         IF RESP-KVPARTI-LINE (INDX) = ALL '+'                            
127700           MOVE MFS-ROER-EJ-FAELT TO MOD-KVPARTI-RAD (INDX)               
127800         ELSE                                                             
127900           MOVE RESP-KVPARTI-LINE (INDX)                                  
128000                                  TO MOD-KVPARTI-RAD (INDX)               
128100         END-IF                                                           
128200       END-IF                                                             
128300*                                                                         
128400       IF RESP-ADINLOMR-FB-LINE (INDX) = SPACES                           
128500         MOVE MFS-RENSA-FAELT   TO MOD-ADINLOMR-FB (INDX)                 
128600       ELSE                                                               
128700         IF RESP-ADINLOMR-FB-LINE (INDX) = ALL '+'                        
128800           MOVE MFS-ROER-EJ-FAELT TO MOD-ADINLOMR-FB (INDX)               
128900         ELSE                                                             
129000           MOVE RESP-ADINLOMR-FB-LINE (INDX)                              
129100                                  TO MOD-ADINLOMR-FB (INDX)               
129200         END-IF                                                           
129300       END-IF                                                             
129400*                                                                         
129500       IF RESP-IDSHIPM-LINE (INDX) = SPACES                               
129600         MOVE MFS-RENSA-FAELT   TO MOD-IDSHIPM-RAD (INDX)                 
129700       ELSE                                                               
129800         IF RESP-IDSHIPM-LINE (INDX) = ALL '+'                            
129900           MOVE MFS-ROER-EJ-FAELT TO MOD-IDSHIPM-RAD (INDX)               
130000         ELSE                                                             
130100           MOVE RESP-IDSHIPM-LINE (INDX)                                  
130200                                  TO MOD-IDSHIPM-RAD (INDX)               
130300         END-IF                                                           
130400       END-IF                                                             
130500*                                                                         
130600       ADD +1 TO INDX                                                     
130700     END-PERFORM                                                          
130800     END-IF                                                               
130900                                                                          
131000*                                                                         
131100* CLOSE/CLEAR THE REMAINING LINES ON THE SCREEN                           
131200     PERFORM UNTIL INDX    >  MAX-INDX                                    
131300       MOVE MFS-STAENG-FAELT TO MOD-KDCMDVAL-RAD-ATTR(INDX)               
131400                                                                          
131500       MOVE MFS-RENSA-FAELT TO MOD-KDCMDVAL-RAD      (INDX)               
131600                               MOD-IDLEVNR-RAD       (INDX)               
131700                               MOD-IDFS-RAD          (INDX)               
131800                               MOD-TIAVIDAT-RAD      (INDX)               
131900                               MOD-IDLBBET-RAD       (INDX)               
132000                               MOD-KVPARTI-RAD       (INDX)               
132100                               MOD-FLFEL-RAD         (INDX)               
132200                               MOD-IDSHIPM-RAD       (INDX)               
132300                               MOD-ADINLOMR-FB       (INDX)               
132400       ADD +1 TO INDX                                                     
132500     END-PERFORM                                                          
132600                                                                          
132700     IF UPDATE-DONE                                                       
132800       PERFORM MFS-FORM-ATTR                                              
132900       PERFORM MFS-RENSA-FAELT-IN                                         
133000     END-IF                                                               
133100     .                                                                    
133200     EJECT                                                                
133300* --- MFS SEKTIONER ---                                                   
133400 MFS-RENSA-FAELT-IN SECTION.                                              
133500                                                                          
133600     MOVE MFS-RENSA-FAELT TO MOD-IDLBBET-UPD                              
133700                             MOD-FLKLAR-UPD                               
133800                             MOD-ADINLOMR-LPL-UPD                         
133900     PERFORM MFS-RENSA-RAD-FAELT-IN                                       
134000     .                                                                    
134100     SKIP2                                                                
134200 MFS-RENSA-RAD-FAELT-IN SECTION.                                          
134300     MOVE +1                   TO INDX                                    
134400     PERFORM UNTIL INDX        >  MAX-INDX                                
134500         MOVE MFS-RENSA-FAELT  TO MOD-KDCMDVAL-RAD (INDX)                 
134600         ADD +1                TO INDX                                    
134700     END-PERFORM                                                          
134800     .                                                                    
134900     EJECT                                                                
135000     .                                                                    
135100     EJECT                                                                
135200 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
135300                                                                          
135400     MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR-ENTER                          
135500                               MOD-IDLEVNR-NEXT                           
135600                               MOD-IDFS-ENTER                             
135700                               MOD-IDFS-NEXT                              
135800                               MOD-TIAVIDAT-ENTER                         
135900                               MOD-TIAVIDAT-NEXT                          
136000                               MOD-IDLBBET-ENTER                          
136100                               MOD-IDLBBET-NEXT                           
136200                               MOD-OMSTART-INDX                           
136300     MOVE +1 TO INDX                                                      
136400     PERFORM UNTIL INDX > MAX-INDX                                        
136500       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
136600       ADD +1 TO INDX                                                     
136700     END-PERFORM                                                          
136800     .                                                                    
136900     SKIP2                                                                
137000 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
137100                                                                          
137200     MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMDVAL-RAD (INDX)                    
137300                               MOD-IDLEVNR-RAD  (INDX)                    
137400                               MOD-IDFS-RAD     (INDX)                    
137500                               MOD-TIAVIDAT-RAD (INDX)                    
137600                               MOD-IDLBBET-RAD  (INDX)                    
137700                               MOD-KVPARTI-RAD  (INDX)                    
137800                               MOD-FLFEL-RAD    (INDX)                    
137900                               MOD-IDSHIPM-RAD  (INDX)                    
138000                               MOD-ADINLOMR-FB  (INDX)                    
138100     .                                                                    
138200     SKIP2                                                                
138300 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
138400                                                                          
138500     MOVE MFS-ROER-EJ-FAELT TO MOD-IDLBBET-UPD                            
138600                               MOD-FLKLAR-UPD                             
138700                               MOD-ADINLOMR-LPL-UPD                       
138800     MOVE +1 TO INDX                                                      
138900     PERFORM UNTIL INDX > MAX-INDX                                        
139000       PERFORM MFS-ROER-EJ-RAD-FAELT-IN                                   
139100       ADD +1 TO INDX                                                     
139200     END-PERFORM                                                          
139300     .                                                                    
139400     SKIP2                                                                
139500 MFS-ROER-EJ-RAD-FAELT-IN  SECTION.                                       
139600                                                                          
139700     MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMDVAL-RAD (INDX)                    
139800     .                                                                    
139900     EJECT                                                                
140000 MFS-FORM-ATTR SECTION.                                                   
140100                                                                          
140200     MOVE MFS-FORMATETS-ATTR   TO MOD-IDLBBET-UPD-ATTR                    
140300                                  MOD-FLKLAR-UPD-ATTR                     
140400                                  MOD-ADINLOMR-LPL-UPD-ATTR               
140500     MOVE +1                   TO INDX                                    
140600     PERFORM UNTIL INDX        >  MAX-INDX                                
140700       PERFORM MFS-FORM-ATTR-RAD                                          
140800       ADD +1                  TO INDX                                    
140900     END-PERFORM                                                          
141000     .                                                                    
141100     SKIP2                                                                
141200 MFS-FORM-ATTR-RAD         SECTION.                                       
141300                                                                          
141400     MOVE MFS-FORMATETS-ATTR   TO MOD-KDCMDVAL-RAD-ATTR (INDX)            
141500     .                                                                    
141600     EJECT                                                                
141700 MFS-LAES-IN-IGEN SECTION.                                                
141800                                                                          
141900     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLBBET-UPD-ATTR                   
142000                                   MOD-FLKLAR-UPD-ATTR                    
142100                                   MOD-ADINLOMR-LPL-UPD-ATTR              
142200     MOVE +1                    TO INDX                                   
142300     PERFORM UNTIL INDX         >  MAX-INDX                               
142400       MOVE MFS-ADD-LAES-IN-FAELT                                         
142500                                TO MOD-KDCMDVAL-RAD-ATTR (INDX)           
142600       ADD +1                   TO INDX                                   
142700     END-PERFORM                                                          
142800     .                                                                    
142900     EJECT                                                                
143400* --- IMS SEKTIONER ---                                                   
143500     SKIP3                                                                
143600 IMS-GET-MSG SECTION.                                                     
143700                                                                          
143800     MOVE '  QC' TO GODK-STATUSKODER                                      
143900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
144000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
144100     PERFORM IMS-STATUSKONTROLL                                           
144200     .                                                                    
144300     SKIP3                                                                
144400 IMS-INSERT-MSG SECTION.                                                  
144500                                                                          
144600     IF NOT ENGLISH-TEXT                                                  
144700       MOVE '0' TO MFS-KDHUVOMR                                           
144800     END-IF                                                               
144900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
145000     MOVE SPACE TO GODK-STATUSKODER                                       
145100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
145200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
145300     PERFORM IMS-STATUSKONTROLL                                           
145400     .                                                                    
145500     EJECT                                                                
146300 IMS-STATUSKONTROLL SECTION.                                              
146400                                                                          
146500     SET STATUS-IX TO 1                                                   
146600     SEARCH GODK-STATUS                                                   
146700       AT END                                                             
146800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
146900         DELIMITED BY SIZE INTO FELTEXT                                   
147000         CALL FELLOG                                                      
147100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
147200         CONTINUE                                                         
147300     END-SEARCH                                                           
147400     .                                                                    
