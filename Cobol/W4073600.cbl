000100*                                                                         
000200******************************************************************        
000300*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0151      *        
000400******************************************************************        
000500*                                                                         
000600 ID DIVISION.                                                             
000700                                                                          
000800 PROGRAM-ID.     W4073600.                                                
000900 AUTHOR.         LARS THELL.                                              
001000 DATE-WRITTEN.   95/06/26.                                                
001100 DATE-COMPILED.                                                           
001200                                                                          
001300*    FUNKTION:                                                            
001400*        VISAR RETURTILLSTÅNDSKÖ.                                         
001500*        ANVÄNDS FÖR UTSKRIFT AV RETURTILLSTÅND.                          
001600*                                                                         
001700*        PROGRAMMET LÄSER      WLRETA (WDA3)                              
001800*        PROGRAMMET LÄSER      WLKREE (WDA2)                              
001900*                                                                         
002000*    E-TRACKER: 4230251  2007-02  LDC-3 SUSANNE OLSSON                    
002100*    E-TRACKER: 6426835  2008-03  RÄTTA FEL                               
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSAKTION: W4T736                                              
002500*        MID:         W4I73601                                            
002600*                                                                         
002700*    UTDATA.                                                              
002800*        MOD:         W4O73601                                            
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400 WORKING-STORAGE SECTION.                                                 
003500*    -- CHECKED BY WY2000                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'W4073600'.            
003700                                                                          
003800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004000                                                                          
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  YES                         PIC X       VALUE 'Y'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400 77  W-IDDISTR-SEC               PIC 9(4).                                
004500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  VALUE +1000 COMP SYNC.        
004600                                                                          
004700 77  W-UTSKRIFT                  PIC X       VALUE 'X'.                   
004800                                                                          
004900*    --- INDEX FÖR BLÄDDRINGSRADER                                        
005000 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005100 77  4794-IX                     PIC S9(4)  VALUE +0    COMP SYNC.        
005200 77  MAX-INDX                    PIC S9(4)  VALUE +11   COMP SYNC.        
005300                                                                          
005400*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005500 77  WS-IDDISTR                  PIC  X(4)  VALUE SPACE.                  
005600 77  WS-IDKUNDNR                 PIC  X(6)  VALUE SPACE.                  
005700 77  WS-KDLEVANM                 PIC  X(1)  VALUE SPACE.                  
005800 77  WS-FLSUM                    PIC  X(1)  VALUE SPACE.                  
005900 77  WS-IDANSV                   PIC  X(4)  VALUE SPACE.                  
006000                                                                          
006100 77  W-KVRT                      PIC S9(5)  VALUE ZERO COMP-3.            
006200 77  W-KVRADER                   PIC S9(5)  VALUE ZERO COMP-3.            
006300 77  W-IDPERSON                  PIC  9(3)  VALUE ZERO.                   
006400 77  W-ANM-AVIS                  PIC X(1)   VALUE '4'.                    
006500 77  W-ANM-MOT                   PIC X(1)   VALUE '5'.                    
006600 77  W-ANM-PAAB                  PIC X(1)   VALUE '6'.                    
006700                                                                          
006800 77  SW-KDCMD                    PIC X       VALUE 'N'.                   
006900     88  KDCMD-IFYLLT                        VALUE 'J'.                   
006700                                                                          
006800 77  SW-KDLEVANM                 PIC X       VALUE 'N'.                   
006900     88  KDLEVANM-INTERVALL                  VALUE 'J'.                   
007000                                                                          
007100 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007200     88  INDATA-OK                           VALUE 'J'.                   
007300     88  INDATA-FEL                          VALUE 'N'.                   
007400                                                                          
007500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007600     88  NYCKLAR-OK                          VALUE 'J'.                   
007700     88  NYCKLAR-FEL                         VALUE 'N'.                   
007800                                                                          
007900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008000     88  EGEN-MID                            VALUE '4736'.                
008100     88  GODK-MID                            VALUE '4736'.                
008200     88  HELP-MID                            VALUE '0551'.                
008300*      --- VALID IDDC CODES                                               
008400*                                                                         
008500*01    -COPY WWDC99                                                       
008600       EJECT                                                              
008700*    --- DATUM                                                            
008800                                                                          
008900 01  W-TIAADDD.                                                           
009000     03  FILLER                  PIC 9(1)   VALUE ZERO.                   
009100     03  W-TIAA                  PIC 9(2)   VALUE ZERO.                   
009200     03  W-TIDDD                 PIC 9(3)   VALUE ZERO.                   
009300                                                                          
009400 01  W-TIAADDD-IDAG.                                                      
009500     03  W-TIAA-IDAG             PIC 9(2).                                
009600     03  W-TIDDD-IDAG            PIC 9(3).                                
009700                                                                          
009800 01  W-TIAAAAMMDD-RTM.                                                    
009900     03  W-TISEKEL-RTM           PIC 9(2).                                
010000     03  W-TIAAMMDD-RTM          PIC 9(6).                                
010100 01  W-TIAAAAMMDD-RTP.                                                    
010200     03  W-TISEKEL-RTP           PIC 9(2).                                
010300     03  W-TIAAMMDD-RTP          PIC 9(6).                                
010400 01  W-TIAAAAMMDD-RTAOSEA.                                                
010500     03  W-TISEKEL-RTAOSEA       PIC 9(2).                                
010600     03  W-TIAAMMDD-RTAOSEA      PIC 9(6).                                
010700 01  W-TIAAAAMMDD-RTAOVR.                                                 
010800     03  W-TISEKEL-RTAOVR        PIC 9(2).                                
010900     03  W-TIAAMMDD-RTAOVR       PIC 9(6).                                
011000                                                                          
011100     EJECT                                                                
011200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011300 01  GENERELLA-SUBPROGRAM.                                                
011400     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
011500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
011600     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
011700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
012000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
012100     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
012200     EJECT                                                                
012300*01  -COPY W006PRT                                                        
012400     EJECT                                                                
012500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
012600*01 -COPY WMEDAREA                                                        
012700     SKIP3                                                                
012800*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
012900*01 -COPY WDATAREA                                                        
013000     SKIP3                                                                
013100                                                                          
013200*    --- PARAMETRAR TILL SUBPROGRAM WSECURIT                              
013300*   -COPY WSECAREA                                                        
013400     EJECT                                                                
013500 01  MESSAGE-CODES.                                                       
013600     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
013700     03  INF-PRESS-PF4           PIC X(3)    VALUE '081'.                 
013800     03  ERR-INFO-MISSING        PIC X(3)    VALUE '005'.                 
013900     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
014000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
014100     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
014200     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
014300     03  INF-PRINT-BEG           PIC X(3)    VALUE '118'.                 
014400     03  ERR-NOTHING-PRINTED     PIC X(3)    VALUE '167'.                 
014500     03  ERR-NO-LINE-CHOSEN      PIC X(3)    VALUE '231'.                 
014600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
014700     03  ERR-WRONG-PRINTER       PIC X(3)    VALUE '772'.                 
014800     03  ERR-USER-NOT-AUTHORIZED PIC X(3)    VALUE '405'.                 
014900     EJECT                                                                
015000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
015100*                                                                         
015200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
015300     SKIP3                                                                
015400*01 -COPY WMSGINIT                                                        
015500     EJECT                                                                
015600*    --- FÄLT FÖR HOPP TILL ANDRA BILDER                                  
015700   77  SW-STARTA-ANNAN-BILD        PIC X       VALUE 'N'.                 
015800     88  STARTA-ANNAN-BILD                     VALUE 'J'.                 
015900                                                                          
016000 01  BILD-HOPP-AREOR.                                                     
016100                                                                          
016200   03    W-BILD               PIC X(4)    VALUE SPACE.                    
016300   03    W-HOPP-IDTRANS.                                                  
016400     05  FILLER               PIC X(1)    VALUE 'W'.                      
016500     05  W-HOPP-IDTRANS-2     PIC X(1).                                   
016600     05  FILLER               PIC X(1)    VALUE 'T'.                      
016700     05  W-HOPP-IDTRANS-4-6   PIC X(3).                                   
016800     05  FILLER               PIC X(2)    VALUE SPACE.                    
016900                                                                          
017000                                                                          
017100   03 FILLER             PIC X(16)   VALUE 'P-TO-P-AREA'.                 
017200   03      P-TO-P-SW.                                                     
017300                                                                          
017400     05  P-TO-P-KVLL             PIC S9(4) VALUE +119 COMP SYNC.          
017500     05  P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
017600     05  P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
017700     05  P-TO-P-KDTRANS          PIC X(8).                                
017800     05  P-TO-P-IDTRANS          PIC X(4).                                
017900     05  P-TO-P-KDMFSFOR         PIC X(1).                                
018000     05  P-TO-P-DATA             PIC X(100) VALUE ALL '+'.                
018100                                                                          
018200   03 FILLER             PIC X(16)   VALUE 'P-TO-P-AREA2'.                
018300 01  P-TO-P-T94.                                                          
018400*----TILL W40794                                                          
018500     03  P-TO-P2-LL              PIC S9(4)   COMP SYNC.                   
018600     03  P-TO-P2-Z1              PIC X(1)    VALUE LOW-VALUE.             
018700     03  P-TO-P2-Z2              PIC X(1)    VALUE LOW-VALUE.             
018800     03  P-TO-P2-TRANSKOD        PIC X(7)    VALUE 'W4T794X'.             
018900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
019000     03  P-TO-P2-IDTRANS         PIC X(4)    VALUE '4736'.                
019100     03  P-TO-P2-KDMFSFOR        PIC X(1)    VALUE SPACE.                 
019200*    03  MID -COPY W4I79401 -PRE MOD4794-                                 
019300     EJECT                                                                
019400*                                                                         
019500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
019600*                                                                         
019700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
019800     SKIP3                                                                
019900*01  MID -COPY W4I73601                                                   
020000     EJECT                                                                
020100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
020200     SKIP3                                                                
020300*01  -COPY WMSGAREA                                                       
020400     EJECT                                                                
020500     03  MOD REDEFINES MSG-AREA.                                          
020600*      05  -COPY W4O73601    -PRE MOD-                                    
020700     EJECT                                                                
020800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
020900     SKIP3                                                                
021000*01  -COPY WMFSAREA                                                       
021100     EJECT                                                                
021200                                                                          
021300     SKIP2                                                                
021400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021500*                                                                         
021600     EJECT                                                                
021700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
021800     SKIP3                                                                
021900 01  W-MINKEY-X.                                                          
022000     03  W-MINKEY-IDTRANS          PIC  X(4)   VALUE '4736'.              
022100     03  W-MINKEY-WDA2C1KY-ENTER.                                         
022200         05  W-MINKEYC1-IDFTG      PIC  9(2)          VALUE ZERO.         
022300         05  W-MINKEYC1-KDLEVANM   PIC  X(1)          VALUE ZERO.         
022400         05  W-MINKEYC1-KDARBTYP   PIC  X(8)          VALUE SPACE.        
022500         05  W-MINKEYC1-IDPERSON   PIC S9(3)   COMP-3 VALUE ZERO.         
022600         05  W-MINKEYC1-DARETANK   PIC  9(8)          VALUE ZERO.         
022700         05  W-MINKEYC1-DARETILL   PIC  9(8)          VALUE ZERO.         
022800         05  W-MINKEYC1-IDDISTR    PIC S9(5)   COMP-3 VALUE ZERO.         
022900         05  W-MINKEYC1-IDKUNDNR   PIC S9(7)   COMP-3 VALUE ZERO.         
023000         05  W-MINKEYC1-IDRAPPNR   PIC  9(7)          VALUE ZERO.         
023100     03  W-MINKEY-WDA2C1KY-NEXT.                                          
023200         05  W-MINKEYC1-IDFTG-NEXT    PIC  9(2)        VALUE ZERO.        
023300         05  W-MINKEYC1-KDLEVANM-NEXT PIC  X(1)        VALUE ZERO.        
023400         05  W-MINKEYC1-KDARBTYP-NEXT PIC  X(8)       VALUE SPACE.        
023500         05  W-MINKEYC1-IDPERSON-NEXT PIC S9(3) COMP-3 VALUE ZERO.        
023600         05  W-MINKEYC1-DARETANK-NEXT PIC  9(8)        VALUE ZERO.        
023700         05  W-MINKEYC1-DARETILL-NEXT PIC  9(8)        VALUE ZERO.        
023800         05  W-MINKEYC1-IDDISTR-NEXT  PIC S9(5) COMP-3 VALUE ZERO.        
023900         05  W-MINKEYC1-IDKUNDNR-NEXT PIC S9(7) COMP-3 VALUE ZERO.        
024000         05  W-MINKEYC1-IDRAPPNR-NEXT PIC  9(7)        VALUE ZERO.        
021900 01  W-MINKEYG1-X.                                                        
022000     03  W-MINKEYG1-IDTRANS        PIC  X(4)   VALUE '4736'.              
022100     03  W-MINKEYG1-WDA2G1KY-ENTER.                                       
022200         05  W-MINKEYG1-IDFTG      PIC  9(2)          VALUE ZERO.         
022400         05  W-MINKEYG1-KDARBTYP   PIC  X(8)          VALUE SPACE.        
022500         05  W-MINKEYG1-IDPERSON   PIC S9(3)   COMP-3 VALUE ZERO.         
022600         05  W-MINKEYG1-DARETANK   PIC  9(8)          VALUE ZERO.         
022700         05  W-MINKEYG1-DARETILL   PIC  9(8)          VALUE ZERO.         
022800         05  W-MINKEYG1-IDDISTR    PIC S9(5)   COMP-3 VALUE ZERO.         
022900         05  W-MINKEYG1-IDKUNDNR   PIC S9(7)   COMP-3 VALUE ZERO.         
023000         05  W-MINKEYG1-IDRAPPNR   PIC  9(7)          VALUE ZERO.         
023100     03  W-MINKEY-WDA2G1KY-NEXT.                                          
023200         05  W-MINKEYG1-IDFTG-NEXT    PIC  9(2)        VALUE ZERO.        
023400         05  W-MINKEYG1-KDARBTYP-NEXT PIC  X(8)       VALUE SPACE.        
023500         05  W-MINKEYG1-IDPERSON-NEXT PIC S9(3) COMP-3 VALUE ZERO.        
023600         05  W-MINKEYG1-DARETANK-NEXT PIC  9(8)        VALUE ZERO.        
023700         05  W-MINKEYG1-DARETILL-NEXT PIC  9(8)        VALUE ZERO.        
023800         05  W-MINKEYG1-IDDISTR-NEXT  PIC S9(5) COMP-3 VALUE ZERO.        
023900         05  W-MINKEYG1-IDKUNDNR-NEXT PIC S9(7) COMP-3 VALUE ZERO.        
024000         05  W-MINKEYG1-IDRAPPNR-NEXT PIC  9(7)        VALUE ZERO.        
024100     SKIP3                                                                
024200                                                                          
024300 01  NYCKLAR-TILL-DLI.                                                    
024400                                                                          
024500     03  W-IDLEVANM-X.                                                    
024600         05  W-IDDISTR           PIC S9(5)   COMP-3 VALUE ZERO.           
024700         05  W-IDKUNDNR          PIC S9(7)   COMP-3 VALUE ZERO.           
024800         05  W-IDRAPPNR          PIC  9(7)          VALUE ZERO.           
024900                                                                          
025000     03  W-WDA3BSEQ-X.                                                    
025100         05  W-IDRT-BSEQ         PIC  X(3)          VALUE SPACE.          
025200         05  W-IDDC-BSEQ         PIC  X(2)          VALUE SPACE.          
025300         05  W-IDRTLOP-BSEQ      PIC  9(3)          VALUE ZERO.           
025400         05  W-IDKOLLI-BSEQ      PIC S9(5)   COMP-3 VALUE ZERO.           
025500                                                                          
025600     03  W-WDA2C1KY-MIN-X.                                                
025700         05  W-IDFTG-C1-MIN      PIC  9(2)          VALUE ZERO.           
025800         05  W-KDLEVANM-C1-MIN   PIC  X(1)          VALUE ZERO.           
025900         05  W-KDARBTYP-C1-MIN   PIC  X(8)          VALUE SPACE.          
026000         05  W-IDPERSON-C1-MIN   PIC S9(3)   COMP-3 VALUE ZERO.           
026100         05  W-DARETANK-C1-MIN   PIC  9(8)          VALUE ZERO.           
026200         05  W-DARETILL-C1-MIN   PIC  9(8)          VALUE ZERO.           
026300         05  W-IDDISTR-C1-MIN    PIC S9(5)   COMP-3 VALUE ZERO.           
026400         05  W-IDKUNDNR-C1-MIN   PIC S9(7)   COMP-3 VALUE ZERO.           
026500         05  W-IDRAPPNR-C1-MIN   PIC  9(7)          VALUE ZERO.           
026600                                                                          
026700     03  W-WDA2C1KY-MAX-X.                                                
026800         05  W-IDFTG-C1-MAX      PIC  9(2)          VALUE ZERO.           
026900         05  W-KDLEVANM-C1-MAX   PIC  X(1)          VALUE ZERO.           
027000         05  W-KDARBTYP-C1-MAX   PIC  X(8)          VALUE SPACE.          
027100         05  W-IDPERSON-C1-MAX   PIC S9(3)   COMP-3 VALUE ZERO.           
027200         05  W-DARETANK-C1-MAX   PIC  9(8)          VALUE ZERO.           
027300         05  W-DARETILL-C1-MAX   PIC  9(8)          VALUE ZERO.           
027400         05  W-IDDISTR-C1-MAX    PIC S9(5)   COMP-3 VALUE ZERO.           
027500         05  W-IDKUNDNR-C1-MAX   PIC S9(7)   COMP-3 VALUE ZERO.           
027600         05  W-IDRAPPNR-C1-MAX   PIC  9(7)          VALUE ZERO.           
                                                                                
025600     03  W-WDA2G1KY-MIN-X.                                                
025700         05  W-IDFTG-G1-MIN      PIC  9(2)          VALUE ZERO.           
025900         05  W-KDARBTYP-G1-MIN   PIC  X(8)          VALUE SPACE.          
026000         05  W-IDPERSON-G1-MIN   PIC S9(3)   COMP-3 VALUE ZERO.           
026100         05  W-DARETANK-G1-MIN   PIC  9(8)          VALUE ZERO.           
026200         05  W-DARETILL-G1-MIN   PIC  9(8)          VALUE ZERO.           
026300         05  W-IDDISTR-G1-MIN    PIC S9(5)   COMP-3 VALUE ZERO.           
026400         05  W-IDKUNDNR-G1-MIN   PIC S9(7)   COMP-3 VALUE ZERO.           
026500         05  W-IDRAPPNR-G1-MIN   PIC  9(7)          VALUE ZERO.           
026600                                                                          
026700     03  W-WDA2G1KY-MAX-X.                                                
026800         05  W-IDFTG-G1-MAX      PIC  9(2)          VALUE ZERO.           
027000         05  W-KDARBTYP-G1-MAX   PIC  X(8)          VALUE SPACE.          
027100         05  W-IDPERSON-G1-MAX   PIC S9(3)   COMP-3 VALUE ZERO.           
027200         05  W-DARETANK-G1-MAX   PIC  9(8)          VALUE ZERO.           
027300         05  W-DARETILL-G1-MAX   PIC  9(8)          VALUE ZERO.           
027400         05  W-IDDISTR-G1-MAX    PIC S9(5)   COMP-3 VALUE ZERO.           
027500         05  W-IDKUNDNR-G1-MAX   PIC S9(7)   COMP-3 VALUE ZERO.           
027600         05  W-IDRAPPNR-G1-MAX   PIC  9(7)          VALUE ZERO.           
027700                                                                          
027800     03  W-WDA3FSEQ-MIN-X.                                                
027900         05  W-IDDC-FSEQ-MIN     PIC  X(2)          VALUE SPACE.          
028000         05  W-IDDISTR-FSEQ-MIN  PIC S9(5)   COMP-3 VALUE ZERO.           
028100         05  W-IDKUNDNR-FSEQ-MIN PIC S9(7)   COMP-3 VALUE ZERO.           
028200         05  W-IDRAPPNR-FSEQ-MIN PIC  9(7)   VALUE ZERO.                  
028300                                                                          
028400     03  W-WDA3FSEQ-MAX-X.                                                
028500         05  W-IDDC-FSEQ-MAX     PIC  X(2)          VALUE SPACE.          
028600         05  W-IDDISTR-FSEQ-MAX  PIC S9(5)   COMP-3 VALUE ZERO.           
028700         05  W-IDKUNDNR-FSEQ-MAX PIC S9(7)   COMP-3 VALUE ZERO.           
028800         05  W-IDRAPPNR-FSEQ-MAX PIC  9(7)   VALUE ZERO.                  
028900                                                                          
029000     03  W-IDDISTR-MIN-X.                                                 
029100         05  W-IDDISTR-MIN       PIC S9(5)   COMP-3 VALUE ZERO.           
029200                                                                          
029300     03  W-IDDISTR-MAX-X.                                                 
029400         05  W-IDDISTR-MAX       PIC S9(5)   COMP-3 VALUE ZERO.           
029500                                                                          
029600     03  W-IDKUNDNR-MIN-X.                                                
029700         05  W-IDKUNDNR-MIN      PIC S9(7)   COMP-3 VALUE ZERO.           
029800                                                                          
029900     03  W-IDKUNDNR-MAX-X.                                                
030000         05  W-IDKUNDNR-MAX      PIC S9(7)   COMP-3 VALUE ZERO.           
030100                                                                          
030200     03  W-WDGX4107-X.                                                    
030300         05  W-IDHTYP-4107       PIC X(4)        VALUE '4107'.            
030400         05  FILLER              PIC X(26)       VALUE LOW-VALUE.         
030500                                                                          
030600     03  W-KDSEGKEY-X.                                                    
030700         05  W-KDSEGKEY          PIC X(1)        VALUE '1'.               
030800                                                                          
030900     SKIP2                                                                
031000*    --- STATUS-KOD FRÅN IMS                                              
031100 01  STATUS-WS                   PIC XX.                                  
031200     88  STATUS-OK                           VALUE '  '.                  
031300     88  SEGMENT-FINNS                       VALUE '  '.                  
031400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
031500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
031600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
031700     88  TRANSKOD-FEL                        VALUE 'A1'.                  
031800     88  SECURITY-FEL                        VALUE 'A4'.                  
031900     SKIP2                                                                
032000 01  GODK-STATUSKODER.                                                    
032100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
032200     SKIP3                                                                
032300 01  SSA1                        PIC X(192).                              
032400 01  SSA2                        PIC X(32).                               
032500     EJECT                                                                
032600*    --- IMS FUNKTIONSKODER                                               
032700*01  -COPY W0003                                                          
032800     EJECT                                                                
032900*    ---  DLI INPUT-OUTPUT AREA                                           
033000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
033100     SKIP3                                                                
033200 01  DLI-IO-AREA.                                                         
033300     03  IO-AREA                 PIC X(192)  VALUE SPACE.                 
033400     SKIP3                                                                
033500     03  WLRETA01 REDEFINES IO-AREA.                                      
033600*        05  -COPY WDA301                                                 
033700     EJECT                                                                
033800     03  WLKREE01 REDEFINES IO-AREA.                                      
033900*        05  -COPY WDA201                                                 
034000     EJECT                                                                
034100     03  WLKREH01 REDEFINES IO-AREA.                                      
034200*        05  -COPY WDA2C1                                                 
034300     EJECT                                                                
034400     03  WL410711 REDEFINES IO-AREA.                                      
034500*        05  -COPY WDGX4108                                               
034000     EJECT                                                                
034100     03  WDA2G    REDEFINES IO-AREA.                                      
034200*        05  -COPY WDA2G1                                                 
034600     EJECT                                                                
034700 LINKAGE SECTION.                                                         
034800                                                                          
034900*01  -COPY W0009   -PRE MSG-                                              
035000*01  -COPY W0009   -PRE ALT-                                              
035100     EJECT                                                                
035200*01  -COPY W0009   -PRE W4794-                                            
035300     EJECT                                                                
035400*01  -COPY W0008   -PRE USEA-                                             
035500     05  FILLER                  PIC X.                                   
035600     EJECT                                                                
035700*01  -COPY W0008   -PRE KREH-                                             
035800     05  FILLER                  PIC X.                                   
035900     EJECT                                                                
036000*01  -COPY W0008  -PRE KREE-                                              
036100     05  FILLER                  PIC X.                                   
036200     EJECT                                                                
036300*01  -COPY W0008  -PRE RETA-                                              
036400     05  FILLER                  PIC X.                                   
036200     EJECT                                                                
036300*01  -COPY W0008  -PRE WDA2G-                                             
036400     05  FILLER                  PIC X.                                   
036500     EJECT                                                                
036600*01  -COPY W0008  -PRE RETA2-                                             
036700     05  FILLER                  PIC X.                                   
036800     EJECT                                                                
036900*01  -COPY W0008  -PRE RETA4-                                             
037000     05  FILLER                  PIC X.                                   
037100     EJECT                                                                
037200*01  -COPY W0008  -PRE 4107-                                              
037300     05  FILLER                  PIC X.                                   
037400     EJECT                                                                
037500 PROCEDURE DIVISION  USING MSG-PCB  ALT-PCB W4794-PCB USEA-PCB            
037600                           KREH-PCB KREE-PCB  RETA-PCB WDA2G-PCB          
037600                           RETA2-PCB RETA4-PCB 4107-PCB.                  
037800     ENTRY 'DLITCBL' USING MSG-PCB  ALT-PCB W4794-PCB USEA-PCB            
037900                           KREH-PCB KREE-PCB  RETA-PCB WDA2G-PCB          
037900                           RETA2-PCB RETA4-PCB 4107-PCB.                  
038100                                                                          
038200     PERFORM IMS-GET-MSG                                                  
038300     IF SEGMENT-FINNS                                                     
038400       PERFORM A-INIT                                                     
038500       PERFORM B-KOLLA-NYCKLAR                                            
038600       IF NYCKLAR-OK                                                      
038700         IF MFS-PRINT                                                     
038800           PERFORM G-KOLLA-INPUT                                          
038900           IF INDATA-OK                                                   
039000             PERFORM H-UPPDATERA-SKRIV-UT                                 
039100           END-IF                                                         
039200         ELSE                                                             
039300           IF MFS-FIRST                                                   
039400             PERFORM C-FOERSTA-SIDA                                       
039500           ELSE                                                           
039600             IF MFS-NEXT                                                  
039700               PERFORM D-NAESTA-SIDA                                      
039800             ELSE                                                         
039900               PERFORM E-SAMMA-SIDA                                       
040000             END-IF                                                       
040100           END-IF                                                         
040200         END-IF                                                           
040300         IF STARTA-ANNAN-BILD                                             
040400            CONTINUE                                                      
040500         ELSE                                                             
040600            IF INDATA-OK                                                  
040700               PERFORM F-LAES-VISA-INFO                                   
040800            END-IF                                                        
040900         END-IF                                                           
041000       END-IF                                                             
041100       IF STARTA-ANNAN-BILD                                               
041200          CONTINUE                                                        
041300       ELSE                                                               
041400          COMPUTE MSG-KVLL = LENGTH OF MOD-W4O73601 + 4                   
041500          PERFORM IMS-INSERT-MSG                                          
041600       END-IF                                                             
041700     END-IF                                                               
041800                                                                          
041900     MOVE ZERO TO RETURN-CODE                                             
042000     GOBACK                                                               
042100     .                                                                    
042200     EJECT                                                                
042300 A-INIT SECTION.                                                          
042400                                                                          
042500     IF MSG-DUBBLA-TRANSKODER                                             
042600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I73601                 
042700       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
042800       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
042900     ELSE                                                                 
043000       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I73601                 
043100       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
043200       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
043300     END-IF                                                               
043400                                                                          
043500     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
043600     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
043700     MOVE MFS-IDTRANS TO W-IDTRANS                                        
043800                                                                          
043900     MOVE LOW-VALUE TO MSG-AREA                                           
044000     MOVE 'W4O73601' TO MFS-IDMOD                                         
044100     MOVE '4736' TO MOD-IDTRANS                                           
044200     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
044300                                                                          
044400     IF EGEN-MID OR HELP-MID                                              
044500       CONTINUE                                                           
044600     ELSE                                                                 
044700       MOVE SPACE TO MFS-KDTRTYP                                          
044800       MOVE '7' TO MFS-IDPFK                                              
044900     END-IF                                                               
045000                                                                          
045100     MOVE LOW-VALUE         TO W-WDA2C1KY-MIN-X                           
045100                               W-WDA2G1KY-MIN-X                           
045200                               W-IDDISTR-MIN-X                            
045300                               W-IDKUNDNR-MIN-X                           
045400                               W-WDA3FSEQ-MIN-X                           
045500                                                                          
045600     MOVE HIGH-VALUE        TO W-WDA2C1KY-MAX-X                           
045600                               W-WDA2G1KY-MAX-X                           
045700                               W-IDDISTR-MAX-X                            
045800                               W-IDKUNDNR-MAX-X                           
045900                               W-WDA3FSEQ-MAX-X                           
046000                                                                          
046100     .                                                                    
046200     EJECT                                                                
046300 B-KOLLA-NYCKLAR SECTION.                                                 
046400                                                                          
046500     MOVE ALL '+'              TO MSGI-WMSGINIT                           
046600     MOVE '001'                TO MSGI-KDCALL                             
046700     MOVE MSG-SIGNON-USERID    TO MSGI-IDUSER                             
046800     MOVE '4736'               TO MSGI-IDTRANS                            
046900     MOVE MSG-LTERM-NAME       TO MSGI-IDLTERM-USER                       
047000     IF EGEN-MID                                                          
047100        IF MID-IDANSV-IN           = ALL '+'                              
047200           MOVE '++++++++'         TO MSGI-KDARBTYP                       
047300           MOVE '+++'              TO MSGI-IDPERSON                       
047400           IF MID-IDDISTR-IN NOT = ALL '+'                                
047500              MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                        
047600              IF MID-IDKUNDNR-IN NOT = ALL '+'                            
047700                MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                     
047800              ELSE                                                        
047900                MOVE ZERO            TO MSGI-IDKUNDNR                     
048000              END-IF                                                      
048100           ELSE                                                           
048200              MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                       
048300           END-IF                                                         
048400        ELSE                                                              
048500           MOVE MID-IDANSV-IN(1:3) TO MSGI-KDARBTYP                       
048600           MOVE MID-IDANSV-IN(4:3) TO MSGI-IDPERSON                       
048700           MOVE ZERO               TO MSGI-IDDISTR                        
048800                                      MSGI-IDKUNDNR                       
048900        END-IF                                                            
              IF MID-KDLEVANM-FOM-IN NOT = ALL '+' AND                          
                 MID-KDLEVANM-TOM-IN = ALL '+'                                  
049000           MOVE MID-KDLEVANM-FOM-IN   TO MSGI-KDLEVANM-FOM                
049000           MOVE SPACE                 TO MSGI-KDLEVANM-TOM                
              ELSE                                                              
049000           MOVE MID-KDLEVANM-FOM-IN   TO MSGI-KDLEVANM-FOM                
049000           MOVE MID-KDLEVANM-TOM-IN   TO MSGI-KDLEVANM-TOM                
              END-IF                                                            
049100     ELSE                                                                 
049200        MOVE SPACE                 TO MSGI-KDARBTYP                       
049300        MOVE ZERO                  TO MSGI-IDPERSON                       
049400     END-IF                                                               
049500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
049600                                                                          
049700     IF MSGI-IDLAND-SPR = 'GB'                                            
049800       MOVE 'GB'                  TO MED-IDSKYLT                          
049900     ELSE                                                                 
050000       MOVE 'S '                  TO MED-IDSKYLT                          
050100     END-IF                                                               
050200                                                                          
050300     MOVE JA TO NYCKLAR-SW                                                
050400                                                                          
050500     PERFORM BA-KOLLA-IDANSV                                              
050600     PERFORM BB-KOLLA-IDDISTR                                             
050700     PERFORM BC-KOLLA-IDKUNDNR                                            
050800     PERFORM BD-KOLLA-KDLEVANM                                            
050900     PERFORM BE-KOLLA-FLSUM                                               
051000     PERFORM BF-KOLLA-IDDC                                                
051100                                                                          
051200     IF GODK-MID OR NYCKLAR-OK                                            
051300        MOVE MSGI-KDARBTYP        TO MOD-IDANSV-UT(1:3)                   
051400        MOVE MSGI-IDPERSON        TO MOD-IDANSV-UT(4:3)                   
051500        MOVE MSGI-IDDISTR         TO MOD-IDDISTR-UT                       
051600        INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE            
051700        MOVE MSGI-IDKUNDNR        TO MOD-IDKUNDNR-UT                      
051800        INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE           
051900        MOVE MSGI-KDLEVANM-FOM    TO MOD-KDLEVANM-FOM-UT                  
051900        MOVE MSGI-KDLEVANM-TOM    TO MOD-KDLEVANM-TOM-UT                  
052000        MOVE WS-FLSUM             TO MOD-FLSUM-UT                         
052100        MOVE WS-IDDC              TO MOD-IDDC-UT                          
052200     ELSE                                                                 
052300        MOVE MFS-RENSA-FAELT      TO MOD-IDANSV-UT                        
052400                                     MOD-IDDISTR-UT                       
052500                                     MOD-IDKUNDNR-UT                      
052600                                     MOD-KDLEVANM-FOM-UT                  
052600                                     MOD-KDLEVANM-TOM-UT                  
052700                                     MOD-FLSUM-UT                         
052800                                     MOD-IDDC-UT                          
052900     END-IF                                                               
053000                                                                          
053100     IF NYCKLAR-FEL                                                       
053200       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
053300       CALL WMEDKONV USING MED-WMEDAREA                                   
053400       MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                 
053500       PERFORM MFS-RENSA-FAELT-IN                                         
053600       PERFORM MFS-RENSA-FAELT-UT                                         
053700     END-IF                                                               
053800                                                                          
053900     IF MSGI-IDDISTR NUMERIC AND MSGI-IDDISTR > ZERO                      
054000        MOVE MSGI-IDUSER          TO SEC-IDUSER                           
054100        MOVE '4736'               TO SEC-IDTRANS                          
054200        MOVE MSGI-IDDISTR         TO WS-IDDISTR                           
054300        MOVE WS-IDDISTR           TO SEC-IDKEY                            
054400                                                                          
054500        CALL WSECURIT USING SEC-IDUSER                                    
054600                            SEC-IDTRANS                                   
054700                            SEC-IDKEY                                     
054800                            SEC-KDSVAR                                    
054900                                                                          
055000        IF SEC-KDSVAR = 'F'                                               
055100           MOVE ERR-USER-NOT-AUTHORIZED  TO MED-IDMFSFEL                  
055200           CALL WMEDKONV USING MED-WMEDAREA                               
055300           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
055400           PERFORM MFS-RENSA-FAELT-IN                                     
055500           PERFORM MFS-RENSA-FAELT-UT                                     
055600           MOVE NEJ   TO NYCKLAR-SW                                       
055700        END-IF                                                            
055800     END-IF                                                               
055900     .                                                                    
056000     EJECT                                                                
056100                                                                          
056200 BA-KOLLA-IDANSV    SECTION.                                              
056300                                                                          
056400*    -- KONTROLL AV IDANSV DVS KDARBTYP OCH IDPERSON                      
056500                                                                          
056600     MOVE MFS-RENSA-FAELT TO MOD-IDANSV-IN                                
056700                                                                          
056800     IF MID-IDANSV-IN     NOT =  ALL '+'                                  
056900       MOVE '7'           TO MFS-IDPFK                                    
057000       MOVE SPACE         TO MFS-KDTRTYP                                  
057100     END-IF                                                               
057200                                                                          
057300     IF MSGI-KDARBTYP             NOT = SPACE                             
057400        IF MSGI-IDPERSON          NUMERIC                                 
057500           MOVE MSGI-KDARBTYP      TO  W-KDARBTYP-C1-MIN                  
057600                                       W-KDARBTYP-C1-MAX                  
057500                                       W-KDARBTYP-G1-MIN                  
057600                                       W-KDARBTYP-G1-MAX                  
057700           MOVE MSGI-IDPERSON      TO  W-IDPERSON-C1-MIN                  
057800                                       W-IDPERSON-C1-MAX                  
057700                                       W-IDPERSON-G1-MIN                  
057800                                       W-IDPERSON-G1-MAX                  
057900        ELSE                                                              
058000           MOVE NEJ                TO NYCKLAR-SW                          
058100        END-IF                                                            
058200     END-IF                                                               
058300     .                                                                    
058400     EJECT                                                                
058500                                                                          
058600 BB-KOLLA-IDDISTR  SECTION.                                               
058700                                                                          
058800     MOVE MFS-RENSA-FAELT       TO MOD-IDDISTR-IN                         
058900                                                                          
059000     IF MID-IDDISTR-IN          NOT = ALL '+'                             
059100       MOVE '7'                 TO MFS-IDPFK                              
059200       MOVE SPACE               TO MFS-KDTRTYP                            
059300     END-IF                                                               
059400                                                                          
059500     IF MSGI-IDDISTR NUMERIC AND MSGI-IDDISTR > ZERO                      
059600       MOVE MSGI-IDDISTR        TO W-IDDISTR-MIN                          
059700                                   W-IDDISTR-MAX                          
059800     END-IF                                                               
059900                                                                          
060000     .                                                                    
060100     EJECT                                                                
060200                                                                          
060300 BC-KOLLA-IDKUNDNR   SECTION.                                             
060400                                                                          
060500     MOVE MFS-RENSA-FAELT       TO MOD-IDKUNDNR-IN                        
060600                                                                          
060700     IF MID-IDKUNDNR-IN         NOT = ALL '+'                             
060800       MOVE '7'                 TO MFS-IDPFK                              
060900       MOVE SPACE               TO MFS-KDTRTYP                            
061000     END-IF                                                               
061100                                                                          
061200     IF MSGI-IDKUNDNR           NUMERIC AND MSGI-IDKUNDNR > ZERO          
061300       MOVE MSGI-IDKUNDNR       TO W-IDKUNDNR-MIN                         
061400                                   W-IDKUNDNR-MAX                         
061500     END-IF                                                               
061600                                                                          
061700     .                                                                    
061800     EJECT                                                                
061900 BD-KOLLA-KDLEVANM   SECTION.                                             
062000                                                                          
           MOVE NEJ TO SW-KDLEVANM                                              
062100     MOVE MFS-RENSA-FAELT       TO MOD-KDLEVANM-FOM-IN                    
062100                                   MOD-KDLEVANM-TOM-IN                    
062200                                                                          
062300     IF MID-KDLEVANM-FOM-IN     NOT = ALL '+' OR                          
062300        MID-KDLEVANM-TOM-IN     NOT = ALL '+'                             
062400       MOVE '7'                 TO MFS-IDPFK                              
062500       MOVE SPACE               TO MFS-KDTRTYP                            
062600     END-IF                                                               
062700                                                                          
062800     IF MSGI-KDLEVANM-FOM        NUMERIC AND                              
062900        MSGI-KDLEVANM-FOM        NOT = ZERO                               
063000        IF MSGI-KDLEVANM-FOM     > '3' AND                                
063100           MSGI-KDLEVANM-FOM     < '7'                                    
063200            MOVE MSGI-KDLEVANM-FOM TO W-KDLEVANM-C1-MIN                   
062800            IF MSGI-KDLEVANM-TOM        NUMERIC AND                       
062900               MSGI-KDLEVANM-TOM        NOT = ZERO                        
063000               IF MSGI-KDLEVANM-TOM     > '3' AND                         
063100                  MSGI-KDLEVANM-TOM     < '7' AND                         
063100                  MSGI-KDLEVANM-FOM <=  MSGI-KDLEVANM-TOM                 
                        IF  MSGI-KDLEVANM-FOM <  MSGI-KDLEVANM-TOM              
                            IF  MSGI-KDLEVANM-FOM = '5' AND                     
                                MSGI-KDLEVANM-TOM = '6'                         
063200                          MOVE MSGI-KDLEVANM-TOM TO                       
                                     W-KDLEVANM-C1-MAX                          
                                MOVE JA TO SW-KDLEVANM                          
                            ELSE                                                
063500                          MOVE NEJ TO NYCKLAR-SW                          
                            END-IF                                              
                        ELSE                                                    
063200                      MOVE MSGI-KDLEVANM-TOM TO W-KDLEVANM-C1-MAX         
                        END-IF                                                  
                     ELSE                                                       
063500                  MOVE NEJ TO NYCKLAR-SW                                  
                     END-IF                                                     
                  ELSE                                                          
063200               MOVE MSGI-KDLEVANM-FOM TO W-KDLEVANM-C1-MAX                
                  END-IF                                                        
063400        ELSE                                                              
063500            MOVE NEJ            TO NYCKLAR-SW                             
063600        END-IF                                                            
063700     ELSE                                                                 
062800        IF MSGI-KDLEVANM-TOM    NOT NUMERIC OR                            
062900           MSGI-KDLEVANM-TOM    = ZERO                                    
063800           MOVE W-ANM-MOT          TO W-KDLEVANM-C1-MIN                   
063900                                      W-KDLEVANM-C1-MAX                   
064000                                      MSGI-KDLEVANM-FOM                   
064000                                      MSGI-KDLEVANM-TOM                   
              ELSE                                                              
063500            MOVE NEJ               TO NYCKLAR-SW                          
              END-IF                                                            
064100     END-IF                                                               
064200                                                                          
064300     .                                                                    
064400     EJECT                                                                
064500 BE-KOLLA-FLSUM      SECTION.                                             
064600                                                                          
064700     MOVE MFS-RENSA-FAELT       TO MOD-FLSUM-IN                           
064800                                                                          
064900     IF MID-FLSUM-IN            = ALL '+'                                 
065000       MOVE MID-FLSUM-UT        TO WS-FLSUM                               
065100     ELSE                                                                 
065200       MOVE MID-FLSUM-IN        TO WS-FLSUM                               
065300       MOVE '7'                 TO MFS-IDPFK                              
065400       MOVE SPACE               TO MFS-KDTRTYP                            
065500     END-IF                                                               
065600     IF WS-FLSUM = JA OR YES                                              
065700       CONTINUE                                                           
065800     ELSE                                                                 
065900       MOVE NEJ TO WS-FLSUM                                               
066000     END-IF                                                               
066100                                                                          
066200     .                                                                    
066300     EJECT                                                                
066400 BF-KOLLA-IDDC       SECTION.                                             
066500                                                                          
066600     MOVE MFS-RENSA-FAELT       TO MOD-IDDC-IN                            
066700                                                                          
066800     IF MID-IDDC-IN            = ALL '+'                                  
066900       MOVE MSGI-IDDC           TO WS-IDDC                                
067000     ELSE                                                                 
067100       MOVE MID-IDDC-IN         TO WS-IDDC                                
067200       MOVE '7'                 TO MFS-IDPFK                              
067300       MOVE SPACE               TO MFS-KDTRTYP                            
067400     END-IF                                                               
067500     IF WS-IDDC(1:1) = MSGI-IDDC(1:1)                                     
067600       MOVE WS-IDDC        TO W-IDDC-BSEQ                                 
067700                              W-IDDC-FSEQ-MIN                             
067800                              W-IDDC-FSEQ-MAX                             
067900       MOVE MSGI-IDFTG     TO W-IDFTG-C1-MIN                              
068000                              W-IDFTG-C1-MAX                              
067900                              W-IDFTG-G1-MIN                              
068000                              W-IDFTG-G1-MAX                              
068100     ELSE                                                                 
068200       MOVE NEJ            TO NYCKLAR-SW                                  
068300     END-IF                                                               
068400                                                                          
068500     .                                                                    
068600     EJECT                                                                
068700 C-FOERSTA-SIDA SECTION.                                                  
068800                                                                          
068900     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
069000     CALL WMEDKONV USING MED-WMEDAREA                                     
069100     MOVE MED-MFSINF     TO MOD-TEMFSFEL                                  
069200                                                                          
069300*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
069400     PERFORM MFS-RENSA-FAELT-IN                                           
069500     .                                                                    
069600     EJECT                                                                
069700 D-NAESTA-SIDA SECTION.                                                   
069800                                                                          
           IF KDLEVANM-INTERVALL                                                
069900        MOVE MSGI-SPAR-AREA           TO W-MINKEYG1-X                     
070000        IF W-MINKEYG1-IDTRANS = '4736'                                    
070100           MOVE W-MINKEY-WDA2G1KY-NEXT TO W-WDA2G1KY-MIN-X                
070200        ELSE                                                              
070300           MOVE LOW-VALUE             TO W-WDA2G1KY-MIN-X                 
070400           MOVE MSGI-IDFTG            TO W-IDFTG-G1-MIN                   
070500                                         W-IDFTG-G1-MAX                   
070600           PERFORM MFS-RENSA-FAELT-IN                                     
              END-IF                                                            
           ELSE                                                                 
069900        MOVE MSGI-SPAR-AREA           TO W-MINKEY-X                       
070000        IF W-MINKEY-IDTRANS = '4736'                                      
070100           MOVE W-MINKEY-WDA2C1KY-NEXT TO W-WDA2C1KY-MIN-X                
070200        ELSE                                                              
070300           MOVE LOW-VALUE             TO W-WDA2C1KY-MIN-X                 
070400           MOVE MSGI-IDFTG            TO W-IDFTG-C1-MIN                   
070500                                         W-IDFTG-C1-MAX                   
070600           PERFORM MFS-RENSA-FAELT-IN                                     
070700        END-IF                                                            
070700     END-IF                                                               
070800     .                                                                    
070900     EJECT                                                                
071000 E-SAMMA-SIDA SECTION.                                                    
071100                                                                          
            IF KDLEVANM-INTERVALL                                               
071200         MOVE MSGI-SPAR-AREA            TO W-MINKEYG1-X                   
071300         IF W-MINKEYG1-IDTRANS = '4736'                                   
071400            MOVE W-MINKEYG1-WDA2G1KY-ENTER TO W-WDA2G1KY-MIN-X            
071500            IF MID-INPUT                =  ALL '+'                        
071600               PERFORM MFS-RENSA-FAELT-IN                                 
071700            ELSE                                                          
071800               MOVE +1                  TO INDX                           
071900               PERFORM UNTIL INDX       >  MAX-INDX                       
072000                  IF MID-KDCMD(INDX) NUMERIC                              
072100                     PERFORM EA-STARTA-ANNAN-BILD                         
072200                     MOVE JA            TO SW-STARTA-ANNAN-BILD           
072300                     MOVE MAX-INDX      TO INDX                           
072400                  END-IF                                                  
072500                  ADD +1                TO INDX                           
072600               END-PERFORM                                                
072700               IF STARTA-ANNAN-BILD                                       
072800                  CONTINUE                                                
072900               ELSE                                                       
073000                  MOVE INF-PRESS-PF4    TO MED-IDMFSINF                   
073100                  CALL WMEDKONV USING MED-WMEDAREA                        
073200                  MOVE MED-MFSINF       TO MOD-TEMFSFEL                   
073300                  PERFORM EB-MID-INDATA-TILL-MOD                          
073400               END-IF                                                     
073500            END-IF                                                        
073600         ELSE                                                             
073700            MOVE LOW-VALUE           TO W-WDA2G1KY-MIN-X                  
073800            MOVE MSGI-IDFTG          TO W-IDFTG-G1-MIN                    
073900                                        W-IDFTG-G1-MAX                    
074000            PERFORM MFS-RENSA-FAELT-IN                                    
074100         END-IF                                                           
            ELSE                                                                
071200         MOVE MSGI-SPAR-AREA            TO W-MINKEY-X                     
071300         IF W-MINKEY-IDTRANS = '4736'                                     
071400            MOVE W-MINKEY-WDA2C1KY-ENTER TO W-WDA2C1KY-MIN-X              
071500            IF MID-INPUT                =  ALL '+'                        
071600               PERFORM MFS-RENSA-FAELT-IN                                 
071700            ELSE                                                          
071800               MOVE +1                  TO INDX                           
071900               PERFORM UNTIL INDX       >  MAX-INDX                       
072000                  IF MID-KDCMD(INDX) NUMERIC                              
072100                     PERFORM EA-STARTA-ANNAN-BILD                         
072200                     MOVE JA            TO SW-STARTA-ANNAN-BILD           
072300                     MOVE MAX-INDX      TO INDX                           
072400                  END-IF                                                  
072500                  ADD +1                TO INDX                           
072600               END-PERFORM                                                
072700               IF STARTA-ANNAN-BILD                                       
072800                  CONTINUE                                                
072900               ELSE                                                       
073000                  MOVE INF-PRESS-PF4    TO MED-IDMFSINF                   
073100                  CALL WMEDKONV USING MED-WMEDAREA                        
073200                  MOVE MED-MFSINF       TO MOD-TEMFSFEL                   
073300                  PERFORM EB-MID-INDATA-TILL-MOD                          
073400               END-IF                                                     
073500            END-IF                                                        
073600         ELSE                                                             
073700            MOVE LOW-VALUE           TO W-WDA2C1KY-MIN-X                  
073800            MOVE MSGI-IDFTG          TO W-IDFTG-C1-MIN                    
073900                                        W-IDFTG-C1-MAX                    
074000            PERFORM MFS-RENSA-FAELT-IN                                    
074100         END-IF                                                           
074100     END-IF                                                               
074200     .                                                                    
074300     EJECT                                                                
074400 EA-STARTA-ANNAN-BILD  SECTION.                                           
074500                                                                          
074600     INSPECT MID-IDDISTR(INDX) REPLACING LEADING SPACE BY ZERO            
074700     MOVE MID-IDDISTR(INDX)      TO MSGI-IDDISTR                          
074800     INSPECT MID-IDKUNDNR(INDX) REPLACING LEADING SPACE BY ZERO           
074900     MOVE MID-IDKUNDNR(INDX)     TO MSGI-IDKUNDNR                         
075000     INSPECT MID-IDRAPPNR(INDX) REPLACING LEADING SPACE BY ZERO           
075100     MOVE MID-IDRAPPNR(INDX)     TO MSGI-IDRAPPNR                         
075200     MOVE '001'                  TO MSGI-KDCALL                           
075300     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
075400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
075500                                                                          
075600     MOVE LOW-VALUE              TO P-TO-P-KDZ1                           
075700     MOVE LOW-VALUE              TO P-TO-P-KDZ2                           
075800     MOVE MID-KDCMD(INDX) (1:1)  TO W-HOPP-IDTRANS-2                      
075900     MOVE MID-KDCMD(INDX) (2:3)  TO W-HOPP-IDTRANS-4-6                    
076000     MOVE W-HOPP-IDTRANS         TO P-TO-P-KDTRANS                        
076100     MOVE '4736'                 TO P-TO-P-IDTRANS                        
076200     MOVE MFS-KDMFSFOR           TO P-TO-P-KDMFSFOR                       
076300                                                                          
076400     PERFORM S01-INSERT-ALTMSG                                            
076500     .                                                                    
076600     EJECT                                                                
076700 EB-MID-INDATA-TILL-MOD SECTION.                                          
076800                                                                          
076900     IF MID-IDPRT                     NOT = ALL '+'                       
077000        MOVE MID-IDPRT                TO MOD-IDPRT                        
077100        MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-IDPRT-ATTR                   
077200     END-IF                                                               
077300                                                                          
077400     MOVE +1                          TO INDX                             
077500     PERFORM UNTIL INDX               >  MAX-INDX                         
077600        IF MID-KDCMD(INDX)            NOT = ALL '+'                       
077700           MOVE MID-KDCMD(INDX)       TO MOD-KDCMD(INDX)                  
077800           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-ATTR(INDX)             
077900        ELSE                                                              
078000           MOVE MFS-RENSA-FAELT       TO MOD-KDCMD(INDX)                  
078100        END-IF                                                            
078200        ADD +1                        TO INDX                             
078300     END-PERFORM                                                          
078400     .                                                                    
078500     EJECT                                                                
078600 F-LAES-VISA-INFO SECTION.                                                
078700                                                                          
078800     PERFORM S05-BERAKNA-DATUM                                            
078900                                                                          
079000     MOVE ZERO         TO W-KVRT                                          
079100                          W-KVRADER                                       
079200                                                                          
           IF KDLEVANM-INTERVALL                                                
              PERFORM FF-LAES-STATUS-INTERVALL                                  
           ELSE                                                                 
079300        PERFORM IMS-GU-WLKREH01                                           
079400        IF SEGMENT-FINNS                                                  
079500           PERFORM S02-KOLLA-BEHORIGHET                                   
079600        END-IF                                                            
079700        PERFORM FA-FIXA-ENTER-KEY                                         
079800                                                                          
079900        IF SEGMENT-SAKNAS                                                 
080000           PERFORM FD-FIXA-NEXT-KEY                                       
080100           MOVE ERR-INFO-MISSING   TO MED-IDMFSFEL                        
080200           CALL WMEDKONV USING MED-WMEDAREA                               
080300           MOVE MED-MFSFEL         TO MOD-TEMFSFEL                        
080400           PERFORM MFS-RENSA-FAELT-UT                                     
080500        ELSE                                                              
080600           MOVE +1                  TO INDX                               
080700                                                                          
080800           PERFORM UNTIL INDX        > MAX-INDX                           
080900              IF SEGMENT-FINNS                                            
081000                                                                          
081100                 ADD +1               TO W-KVRT                           
081200                 COMPUTE W-KVRADER    =  W-KVRADER                        
081300                                      +  SEQC-KVRADER-OBEH                
081400                                                                          
081500                 PERFORM FB-REDIGERA-MOD                                  
081600                                                                          
081700                 PERFORM IMS-GN-WLKREH01                                  
081800                 IF SEGMENT-FINNS                                         
081900                    PERFORM S02-KOLLA-BEHORIGHET                          
082000                 END-IF                                                   
082100                 ADD +1               TO INDX                             
082200              ELSE                                                        
082300                 PERFORM FC-RENSA-RAD                                     
082400                 ADD +1               TO INDX                             
082500              END-IF                                                      
082600           END-PERFORM                                                    
082700                                                                          
082800           PERFORM FD-FIXA-NEXT-KEY                                       
082900           IF SEGMENT-FINNS AND (WS-FLSUM = JA OR YES)                    
083000              PERFORM FE-ADDERA-RT                                        
083100           END-IF                                                         
083200                                                                          
083300           IF WS-FLSUM             =  JA OR YES                           
083400              MOVE W-KVRADER       TO MOD-KVRADER-RT                      
083500              MOVE W-KVRT          TO MOD-KVANT-RT                        
083600           END-IF                                                         
083700                                                                          
083800        END-IF                                                            
083900        MOVE '002'                TO MSGI-KDCALL                          
084000        MOVE '4736'               TO MSGI-IDTRANS                         
084100        CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                        
           END-IF                                                               
084200     .                                                                    
084300     EJECT                                                                
084400                                                                          
084500 FA-FIXA-ENTER-KEY        SECTION.                                        
084600                                                                          
084700     IF SEGMENT-FINNS                                                     
084800        MOVE SEQC-IDFTG             TO W-MINKEYC1-IDFTG                   
084900        MOVE SEQC-KDARBTYP          TO W-MINKEYC1-KDARBTYP                
085000        MOVE SEQC-IDPERSON          TO W-MINKEYC1-IDPERSON                
085100        MOVE SEQC-KDLEVANM          TO W-MINKEYC1-KDLEVANM                
085200        MOVE SEQC-DARETANK          TO W-MINKEYC1-DARETANK                
085300        MOVE SEQC-DARETILL          TO W-MINKEYC1-DARETILL                
085400        MOVE SEQC-IDDISTR           TO W-MINKEYC1-IDDISTR                 
085500        MOVE SEQC-IDKUNDNR          TO W-MINKEYC1-IDKUNDNR                
085600        MOVE SEQC-IDRAPPNR          TO W-MINKEYC1-IDRAPPNR                
085700     ELSE                                                                 
085800        MOVE MSGI-IDFTG             TO W-MINKEYC1-IDFTG                   
085900        MOVE MSGI-KDARBTYP          TO W-MINKEYC1-KDARBTYP                
086000        MOVE MSGI-IDPERSON          TO W-MINKEYC1-IDPERSON                
086100        MOVE MSGI-KDLEVANM-FOM      TO W-MINKEYC1-KDLEVANM                
086200        MOVE ZERO                   TO W-MINKEYC1-IDDISTR                 
086300                                       W-MINKEYC1-IDKUNDNR                
086400                                       W-MINKEYC1-IDRAPPNR                
086500                                       W-MINKEYC1-DARETANK                
086600                                       W-MINKEYC1-DARETILL                
086700     END-IF                                                               
086800     MOVE '4736'                    TO W-MINKEY-IDTRANS                   
086900     MOVE W-MINKEY-X                TO MSGI-SPAR-AREA                     
087000                                                                          
087100     .                                                                    
087200     EJECT                                                                
087300                                                                          
087400 FB-REDIGERA-MOD         SECTION.                                         
087500                                                                          
087600     MOVE SEQC-KDARBTYP     TO MOD-IDANSV   (INDX) (1:3)                  
087700     MOVE SEQC-IDPERSON     TO W-IDPERSON                                 
087800     MOVE W-IDPERSON        TO MOD-IDANSV   (INDX) (4:3)                  
087900     MOVE SEQC-DARETILL(3:6) TO MOD-TIRETILL (INDX)                       
088000                                                                          
088100     IF SEQC-KDLEVANM        =  W-ANM-AVIS                                
088200        MOVE MFS-RENSA-FAELT TO MOD-TIRETANK (INDX)                       
088300        IF SEQC-IDDISTR > 2399                                            
088400          IF CDC-SE                                                       
088500            IF SEQC-DARETILL < W-TIAAAAMMDD-RTAOSEA                       
088600               PERFORM MFS-LYS-UPP-UTRAD                                  
088700            ELSE                                                          
088800               PERFORM MFS-FORM-ATTR-UTRAD                                
088900            END-IF                                                        
089000          ELSE                                                            
089100            IF SEQC-DARETILL < W-TIAAAAMMDD-RTAOVR                        
089200               PERFORM MFS-LYS-UPP-UTRAD                                  
089300            ELSE                                                          
089400               PERFORM MFS-FORM-ATTR-UTRAD                                
089500            END-IF                                                        
089600          END-IF                                                          
089700        ELSE                                                              
089800          IF SEQC-DARETILL < W-TIAAAAMMDD-RTAOVR                          
089900             PERFORM MFS-LYS-UPP-UTRAD                                    
090000          ELSE                                                            
090100             PERFORM MFS-FORM-ATTR-UTRAD                                  
090200          END-IF                                                          
090300        END-IF                                                            
090400     ELSE                                                                 
090500        MOVE SEQC-DARETANK(3:6)   TO MOD-TIRETANK (INDX)                  
090600        IF SEQC-KDLEVANM = W-ANM-MOT                                      
090700          IF SEQC-DARETANK < W-TIAAAAMMDD-RTM                             
090800             PERFORM MFS-LYS-UPP-UTRAD                                    
090900          ELSE                                                            
091000             PERFORM MFS-FORM-ATTR-UTRAD                                  
091100          END-IF                                                          
091200        ELSE                                                              
091300          IF SEQC-DARETANK < W-TIAAAAMMDD-RTP                             
091400             PERFORM MFS-LYS-UPP-UTRAD                                    
091500          ELSE                                                            
091600             PERFORM MFS-FORM-ATTR-UTRAD                                  
091700          END-IF                                                          
091800        END-IF                                                            
091900     END-IF                                                               
092000                                                                          
092100     MOVE SEQC-IDDISTR      TO MOD-IDDISTR  (INDX)                        
092200     MOVE SEQC-IDKUNDNR     TO MOD-IDKUNDNR (INDX)                        
092300     MOVE SEQC-IDRAPPNR     TO MOD-IDRAPPNR (INDX)                        
092400     INSPECT MOD-IDRAPPNR (INDX) REPLACING LEADING ZERO BY SPACE          
092500     MOVE SEQC-KDLEVANM     TO MOD-KDLEVANM (INDX)                        
092500     MOVE SEQC-KVRADER-RT   TO MOD-KVRADER  (INDX)                        
092600     MOVE SEQC-KVRADER-OBEH TO MOD-KVRADER-OBEH  (INDX)                   
092700                                                                          
092800     IF SEQC-KDLEVANM       =  W-ANM-MOT                                  
092900        PERFORM FBA-LAES-ANTAL-KOLLI                                      
093000     ELSE                                                                 
093100        MOVE MFS-RENSA-FAELT TO MOD-KVKOLLI (INDX)                        
093200     END-IF                                                               
093300     .                                                                    
093400     EJECT                                                                
093500                                                                          
093600 FBA-LAES-ANTAL-KOLLI     SECTION.                                        
093700                                                                          
093800     MOVE SEQC-IDDISTR    TO W-IDDISTR-FSEQ-MIN                           
093900                             W-IDDISTR-FSEQ-MAX                           
094000     MOVE SEQC-IDKUNDNR   TO W-IDKUNDNR-FSEQ-MIN                          
094100                             W-IDKUNDNR-FSEQ-MAX                          
094200     MOVE SEQC-IDRAPPNR   TO W-IDRAPPNR-FSEQ-MIN                          
094300                             W-IDRAPPNR-FSEQ-MAX                          
094400                                                                          
094500     PERFORM IMS-GU-WLRETA01                                              
094600                                                                          
094700     IF SEGMENT-FINNS                                                     
094800         MOVE RET-KVKOLLI-AAF  TO MOD-KVKOLLI(INDX)                       
094900     ELSE                                                                 
095000         MOVE ZERO            TO MOD-KVKOLLI(INDX)                        
095100     END-IF                                                               
095200                                                                          
095300     .                                                                    
095400     EJECT                                                                
095500                                                                          
095600 FC-RENSA-RAD SECTION.                                                    
095700                                                                          
095800     MOVE MFS-RENSA-FAELT TO MOD-IDANSV   (INDX)                          
095900                             MOD-TIRETILL (INDX)                          
096000                             MOD-TIRETANK (INDX)                          
096100                             MOD-IDDISTR  (INDX)                          
096200                             MOD-IDKUNDNR (INDX)                          
096300                             MOD-IDRAPPNR (INDX)                          
096400                             MOD-KDLEVANM (INDX)                          
096400                             MOD-KVRADER  (INDX)                          
096500                             MOD-KVRADER-OBEH  (INDX)                     
096600                             MOD-KVKOLLI  (INDX)                          
096700     PERFORM MFS-FORM-ATTR-UTRAD                                          
096800     .                                                                    
096900     EJECT                                                                
097000                                                                          
097100 FD-FIXA-NEXT-KEY        SECTION.                                         
097200                                                                          
097300     IF SEGMENT-FINNS                                                     
097400        MOVE INF-MORE-INFO-EXISTS   TO MED-IDMFSINF                       
097500        CALL WMEDKONV USING MED-WMEDAREA                                  
097600        MOVE MED-MFSINF             TO MOD-TEMFSINF                       
097700                                                                          
097800        MOVE SEQC-IDFTG             TO W-MINKEYC1-IDFTG-NEXT              
097900        MOVE SEQC-KDARBTYP          TO W-MINKEYC1-KDARBTYP-NEXT           
098000        MOVE SEQC-IDPERSON          TO W-MINKEYC1-IDPERSON-NEXT           
098100        MOVE SEQC-KDLEVANM          TO W-MINKEYC1-KDLEVANM-NEXT           
098200        MOVE SEQC-DARETANK          TO W-MINKEYC1-DARETANK-NEXT           
098300        MOVE SEQC-DARETILL          TO W-MINKEYC1-DARETILL-NEXT           
098400        MOVE SEQC-IDDISTR           TO W-MINKEYC1-IDDISTR-NEXT            
098500        MOVE SEQC-IDKUNDNR          TO W-MINKEYC1-IDKUNDNR-NEXT           
098600        MOVE SEQC-IDRAPPNR          TO W-MINKEYC1-IDRAPPNR-NEXT           
098700     ELSE                                                                 
098800        MOVE MSGI-IDFTG             TO W-MINKEYC1-IDFTG-NEXT              
098900        MOVE MSGI-KDARBTYP          TO W-MINKEYC1-KDARBTYP-NEXT           
099000        MOVE MSGI-IDPERSON          TO W-MINKEYC1-IDPERSON-NEXT           
099100        MOVE MSGI-KDLEVANM-FOM      TO W-MINKEYC1-KDLEVANM-NEXT           
099200        MOVE ZERO                   TO W-MINKEYC1-IDDISTR-NEXT            
099300                                       W-MINKEYC1-IDKUNDNR-NEXT           
099400                                       W-MINKEYC1-IDRAPPNR-NEXT           
099500                                       W-MINKEYC1-DARETANK-NEXT           
099600                                       W-MINKEYC1-DARETILL-NEXT           
099700     END-IF                                                               
099800     MOVE '4736'                    TO W-MINKEY-IDTRANS                   
099900     MOVE W-MINKEY-X                TO MSGI-SPAR-AREA                     
100000                                                                          
100100     .                                                                    
100200     EJECT                                                                
100300 FE-ADDERA-RT  SECTION.                                                   
100400                                                                          
100500     PERFORM UNTIL SEGMENT-SAKNAS                                         
100600        ADD +1               TO W-KVRT                                    
100700        COMPUTE W-KVRADER    =  W-KVRADER + SEQC-KVRADER-OBEH             
100800                                                                          
100900        PERFORM IMS-GN-WLKREH01                                           
101000     END-PERFORM                                                          
101100                                                                          
101200     .                                                                    
100200     EJECT                                                                
100300 FF-LAES-STATUS-INTERVALL SECTION.                                        
           PERFORM IMS-GU-WDA2G1                                                
079400     IF SEGMENT-FINNS                                                     
079500        PERFORM S03-KOLLA-BEHORIGHET                                      
079600     END-IF                                                               
079700     PERFORM FG-FIXA-ENTER-KEY                                            
079800                                                                          
079900     IF SEGMENT-SAKNAS                                                    
080000        PERFORM FH-FIXA-NEXT-KEY                                          
080100        MOVE ERR-INFO-MISSING   TO MED-IDMFSFEL                           
080200        CALL WMEDKONV USING MED-WMEDAREA                                  
080300        MOVE MED-MFSFEL         TO MOD-TEMFSFEL                           
080400        PERFORM MFS-RENSA-FAELT-UT                                        
080500     ELSE                                                                 
080600        MOVE +1                  TO INDX                                  
080700                                                                          
080800        PERFORM UNTIL INDX        > MAX-INDX                              
080900           IF SEGMENT-FINNS                                               
081000                                                                          
081100              ADD +1               TO W-KVRT                              
081200              COMPUTE W-KVRADER    =  W-KVRADER                           
081300                                   +  SEQG-KVRADER-OBEH                   
081400                                                                          
081500              PERFORM FI-REDIGERA-MOD                                     
081600                                                                          
081700              PERFORM IMS-GN-WDA2G1                                       
081800              IF SEGMENT-FINNS                                            
081900                 PERFORM S03-KOLLA-BEHORIGHET                             
082000              END-IF                                                      
082100              ADD +1               TO INDX                                
082200           ELSE                                                           
082300              PERFORM FC-RENSA-RAD                                        
082400              ADD +1               TO INDX                                
082500           END-IF                                                         
082600        END-PERFORM                                                       
082700                                                                          
082800        PERFORM FH-FIXA-NEXT-KEY                                          
082900        IF SEGMENT-FINNS AND (WS-FLSUM = JA OR YES)                       
083000           PERFORM FJ-ADDERA-RT                                           
083100        END-IF                                                            
083200                                                                          
083300        IF WS-FLSUM             =  JA OR YES                              
083400           MOVE W-KVRADER       TO MOD-KVRADER-RT                         
083500           MOVE W-KVRT          TO MOD-KVANT-RT                           
083600        END-IF                                                            
083700                                                                          
083800     END-IF                                                               
083900     MOVE '002'                TO MSGI-KDCALL                             
084000     MOVE '4736'               TO MSGI-IDTRANS                            
084100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
101200     .                                                                    
084300     EJECT                                                                
084400                                                                          
084500 FG-FIXA-ENTER-KEY        SECTION.                                        
084600                                                                          
084700     IF SEGMENT-FINNS                                                     
084800        MOVE SEQG-IDFTG             TO W-MINKEYG1-IDFTG                   
084900        MOVE SEQG-KDARBTYP          TO W-MINKEYG1-KDARBTYP                
085000        MOVE SEQG-IDPERSON          TO W-MINKEYG1-IDPERSON                
085200        MOVE SEQG-DARETANK          TO W-MINKEYG1-DARETANK                
085300        MOVE SEQG-DARETILL          TO W-MINKEYG1-DARETILL                
085400        MOVE SEQG-IDDISTR           TO W-MINKEYG1-IDDISTR                 
085500        MOVE SEQG-IDKUNDNR          TO W-MINKEYG1-IDKUNDNR                
085600        MOVE SEQG-IDRAPPNR          TO W-MINKEYG1-IDRAPPNR                
085700     ELSE                                                                 
085800        MOVE MSGI-IDFTG             TO W-MINKEYG1-IDFTG                   
085900        MOVE MSGI-KDARBTYP          TO W-MINKEYG1-KDARBTYP                
086000        MOVE MSGI-IDPERSON          TO W-MINKEYG1-IDPERSON                
086200        MOVE ZERO                   TO W-MINKEYG1-IDDISTR                 
086300                                       W-MINKEYG1-IDKUNDNR                
086400                                       W-MINKEYG1-IDRAPPNR                
086500                                       W-MINKEYG1-DARETANK                
086600                                       W-MINKEYG1-DARETILL                
086700     END-IF                                                               
086800     MOVE '4736'                    TO W-MINKEYG1-IDTRANS                 
086900     MOVE W-MINKEYG1-X              TO MSGI-SPAR-AREA                     
087100     .                                                                    
           EJECT                                                                
097000                                                                          
097100 FH-FIXA-NEXT-KEY        SECTION.                                         
097200                                                                          
097300     IF SEGMENT-FINNS                                                     
097400        MOVE INF-MORE-INFO-EXISTS   TO MED-IDMFSINF                       
097500        CALL WMEDKONV USING MED-WMEDAREA                                  
097600        MOVE MED-MFSINF             TO MOD-TEMFSINF                       
097700                                                                          
097800        MOVE SEQG-IDFTG             TO W-MINKEYG1-IDFTG-NEXT              
097900        MOVE SEQG-KDARBTYP          TO W-MINKEYG1-KDARBTYP-NEXT           
098000        MOVE SEQG-IDPERSON          TO W-MINKEYG1-IDPERSON-NEXT           
098200        MOVE SEQG-DARETANK          TO W-MINKEYG1-DARETANK-NEXT           
098300        MOVE SEQG-DARETILL          TO W-MINKEYG1-DARETILL-NEXT           
098400        MOVE SEQG-IDDISTR           TO W-MINKEYG1-IDDISTR-NEXT            
098500        MOVE SEQG-IDKUNDNR          TO W-MINKEYG1-IDKUNDNR-NEXT           
098600        MOVE SEQG-IDRAPPNR          TO W-MINKEYG1-IDRAPPNR-NEXT           
098700     ELSE                                                                 
098800        MOVE MSGI-IDFTG             TO W-MINKEYG1-IDFTG-NEXT              
098900        MOVE MSGI-KDARBTYP          TO W-MINKEYG1-KDARBTYP-NEXT           
099000        MOVE MSGI-IDPERSON          TO W-MINKEYG1-IDPERSON-NEXT           
099200        MOVE ZERO                   TO W-MINKEYG1-IDDISTR-NEXT            
099300                                       W-MINKEYG1-IDKUNDNR-NEXT           
099400                                       W-MINKEYG1-IDRAPPNR-NEXT           
099500                                       W-MINKEYG1-DARETANK-NEXT           
099600                                       W-MINKEYG1-DARETILL-NEXT           
099700     END-IF                                                               
099800     MOVE '4736'                    TO W-MINKEYG1-IDTRANS                 
099900     MOVE W-MINKEYG1-X              TO MSGI-SPAR-AREA                     
100100     .                                                                    
087200     EJECT                                                                
087300                                                                          
087400 FI-REDIGERA-MOD         SECTION.                                         
087500                                                                          
087600     MOVE SEQG-KDARBTYP     TO MOD-IDANSV   (INDX) (1:3)                  
087700     MOVE SEQG-IDPERSON     TO W-IDPERSON                                 
087800     MOVE W-IDPERSON        TO MOD-IDANSV   (INDX) (4:3)                  
087900     MOVE SEQG-DARETILL(3:6) TO MOD-TIRETILL (INDX)                       
088000                                                                          
088100     IF SEQG-KDLEVANM        =  W-ANM-AVIS                                
088200        MOVE MFS-RENSA-FAELT TO MOD-TIRETANK (INDX)                       
088300        IF SEQG-IDDISTR > 2399                                            
088400          IF CDC-SE                                                       
088500            IF SEQG-DARETILL < W-TIAAAAMMDD-RTAOSEA                       
088600               PERFORM MFS-LYS-UPP-UTRAD                                  
088700            ELSE                                                          
088800               PERFORM MFS-FORM-ATTR-UTRAD                                
088900            END-IF                                                        
089000          ELSE                                                            
089100            IF SEQG-DARETILL < W-TIAAAAMMDD-RTAOVR                        
089200               PERFORM MFS-LYS-UPP-UTRAD                                  
089300            ELSE                                                          
089400               PERFORM MFS-FORM-ATTR-UTRAD                                
089500            END-IF                                                        
089600          END-IF                                                          
089700        ELSE                                                              
089800          IF SEQG-DARETILL < W-TIAAAAMMDD-RTAOVR                          
089900             PERFORM MFS-LYS-UPP-UTRAD                                    
090000          ELSE                                                            
090100             PERFORM MFS-FORM-ATTR-UTRAD                                  
090200          END-IF                                                          
090300        END-IF                                                            
090400     ELSE                                                                 
090500        MOVE SEQG-DARETANK(3:6)   TO MOD-TIRETANK (INDX)                  
090600        IF SEQG-KDLEVANM = W-ANM-MOT                                      
090700          IF SEQG-DARETANK < W-TIAAAAMMDD-RTM                             
090800             PERFORM MFS-LYS-UPP-UTRAD                                    
090900          ELSE                                                            
091000             PERFORM MFS-FORM-ATTR-UTRAD                                  
091100          END-IF                                                          
091200        ELSE                                                              
091300          IF SEQG-DARETANK < W-TIAAAAMMDD-RTP                             
091400             PERFORM MFS-LYS-UPP-UTRAD                                    
091500          ELSE                                                            
091600             PERFORM MFS-FORM-ATTR-UTRAD                                  
091700          END-IF                                                          
091800        END-IF                                                            
091900     END-IF                                                               
092000                                                                          
092100     MOVE SEQG-IDDISTR      TO MOD-IDDISTR  (INDX)                        
092200     MOVE SEQG-IDKUNDNR     TO MOD-IDKUNDNR (INDX)                        
092300     MOVE SEQG-IDRAPPNR     TO MOD-IDRAPPNR (INDX)                        
092400     INSPECT MOD-IDRAPPNR (INDX) REPLACING LEADING ZERO BY SPACE          
092500     MOVE SEQG-KDLEVANM     TO MOD-KDLEVANM (INDX)                        
092500     MOVE SEQG-KVRADER-RT   TO MOD-KVRADER  (INDX)                        
092600     MOVE SEQG-KVRADER-OBEH TO MOD-KVRADER-OBEH  (INDX)                   
092700                                                                          
092800     IF SEQG-KDLEVANM       =  W-ANM-MOT                                  
092900        PERFORM FIA-LAES-ANTAL-KOLLI                                      
093000     ELSE                                                                 
093100        MOVE MFS-RENSA-FAELT TO MOD-KVKOLLI (INDX)                        
093200     END-IF                                                               
093300     .                                                                    
093400     EJECT                                                                
093500                                                                          
093600 FIA-LAES-ANTAL-KOLLI     SECTION.                                        
093700                                                                          
093800     MOVE SEQG-IDDISTR    TO W-IDDISTR-FSEQ-MIN                           
093900                             W-IDDISTR-FSEQ-MAX                           
094000     MOVE SEQG-IDKUNDNR   TO W-IDKUNDNR-FSEQ-MIN                          
094100                             W-IDKUNDNR-FSEQ-MAX                          
094200     MOVE SEQG-IDRAPPNR   TO W-IDRAPPNR-FSEQ-MIN                          
094300                             W-IDRAPPNR-FSEQ-MAX                          
094400                                                                          
094500     PERFORM IMS-GU-WLRETA01                                              
094600                                                                          
094700     IF SEGMENT-FINNS                                                     
094800         MOVE RET-KVKOLLI-AAF  TO MOD-KVKOLLI(INDX)                       
094900     ELSE                                                                 
095000         MOVE ZERO            TO MOD-KVKOLLI(INDX)                        
095100     END-IF                                                               
095200                                                                          
095300     .                                                                    
100200     EJECT                                                                
100300 FJ-ADDERA-RT  SECTION.                                                   
100400                                                                          
100500     PERFORM UNTIL SEGMENT-SAKNAS                                         
100600        ADD +1               TO W-KVRT                                    
100700        COMPUTE W-KVRADER    =  W-KVRADER + SEQG-KVRADER-OBEH             
100800                                                                          
100900        PERFORM IMS-GN-WDA2G1                                             
101000     END-PERFORM                                                          
101100                                                                          
101200     .                                                                    
101300     EJECT                                                                
101400 G-KOLLA-INPUT SECTION.                                                   
101500                                                                          
101600     MOVE JA                      TO INDATA-SW                            
101700     MOVE ZERO                    TO MED-IDMFSFEL                         
101800                                                                          
101900     PERFORM GA-FORMELL-KONTROLL                                          
102000     IF INDATA-OK                                                         
102100        PERFORM GB-LOGISK-KONTROLL                                        
102200     END-IF                                                               
102300                                                                          
102400     IF INDATA-FEL                                                        
102500        IF MED-IDMFSFEL           = ZERO                                  
102600           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
102700        END-IF                                                            
102800        CALL WMEDKONV USING MED-WMEDAREA                                  
102900        MOVE MED-MFSFEL           TO MOD-TEMFSFEL                         
103000        PERFORM MFS-ROER-EJ-FAELT-UT                                      
103100        PERFORM MFS-ROER-EJ-FAELT-IN                                      
103200     END-IF                                                               
103300                                                                          
103400     .                                                                    
103500     EJECT                                                                
103600 GA-FORMELL-KONTROLL SECTION.                                             
103700                                                                          
103800     IF MID-INPUT                = ALL '+'                                
103900       MOVE ERR-NOTHING-PRINTED  TO MED-IDMFSFEL                          
104000       CALL WMEDKONV USING MED-WMEDAREA                                   
104100       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
104200       PERFORM MFS-ROER-EJ-FAELT-IN                                       
104300       PERFORM MFS-ROER-EJ-FAELT-UT                                       
104400       MOVE NEJ                  TO INDATA-SW                             
104500     ELSE                                                                 
104600       PERFORM GAA-KOLLA-KDCMD                                            
104700       PERFORM GAB-KOLLA-IDPRT                                            
104800     END-IF                                                               
104900                                                                          
105000     .                                                                    
105100     EJECT                                                                
105200 GAA-KOLLA-KDCMD      SECTION.                                            
105300                                                                          
105400     MOVE +1                           TO INDX                            
105500                                                                          
105600     MOVE NEJ                           TO SW-KDCMD                       
105700                                                                          
105800     PERFORM UNTIL INDX                 >  MAX-INDX                       
105900        IF MID-KDCMD(INDX)              NOT = ALL '+'                     
106000           IF MID-KDCMD(INDX)           = 'X'                             
106100              MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-ATTR(INDX)           
106200              MOVE JA                   TO SW-KDCMD                       
106300           ELSE                                                           
106400              MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-ATTR(INDX)           
106500              MOVE NEJ                  TO INDATA-SW                      
106600           END-IF                                                         
106700        END-IF                                                            
106800        ADD +1                          TO INDX                           
106900     END-PERFORM                                                          
107000                                                                          
107100     IF KDCMD-IFYLLT                                                      
107200        CONTINUE                                                          
107300     ELSE                                                                 
107400        MOVE NEJ                        TO INDATA-SW                      
107500        MOVE ERR-NO-LINE-CHOSEN         TO MED-IDMFSFEL                   
107600     END-IF                                                               
107700                                                                          
107800     .                                                                    
107900     EJECT                                                                
108000 GAB-KOLLA-IDPRT      SECTION.                                            
108100                                                                          
108200     IF MID-IDPRT                       NOT = ALL '+'                     
108300        MOVE MFS-ALFA-FAELT-RAETT       TO MOD-IDPRT-ATTR                 
108400     ELSE                                                                 
108500        MOVE ERR-WRONG-PRINTER          TO MED-IDMFSFEL                   
108600        MOVE MFS-ALFA-FAELT-FEL         TO MOD-IDPRT-ATTR                 
108700        MOVE NEJ                        TO INDATA-SW                      
108800     END-IF                                                               
108900                                                                          
109000     .                                                                    
109100     EJECT                                                                
109200 GB-LOGISK-KONTROLL SECTION.                                              
109300                                                                          
109400     PERFORM GBA-KOLLA-IDPRT                                              
109500     .                                                                    
109600     EJECT                                                                
109700                                                                          
109800 GBA-KOLLA-IDPRT                  SECTION.                                
109900                                                                          
110000     MOVE SPACE                TO PRT-IDPRTLST                            
110100     MOVE '4RT'                TO PRT-IDPRTLST(1:3)                       
110200                                                                          
110300     MOVE MID-IDPRT            TO PRT-IDPRTLST(4:3)                       
110400     MOVE 1                    TO PRT-KDCALL                              
110500     CALL W006PRT USING PRT-W006PRT                                       
110600                                                                          
110700     IF PRT-KDSVAR                     = 'F'                              
110800         MOVE ERR-WRONG-PRINTER        TO MED-IDMFSFEL                    
110900         MOVE NEJ                      TO INDATA-SW                       
111000         MOVE MFS-ALFA-FAELT-FEL       TO MOD-IDPRT-ATTR                  
111100     END-IF                                                               
111200     .                                                                    
111300     EJECT                                                                
111400                                                                          
111500 H-UPPDATERA-SKRIV-UT SECTION.                                            
111600                                                                          
111700     PERFORM HA-BEHANDLA-VALDA-TILLSTAND                                  
111800                                                                          
111900     MOVE INF-PRINT-BEG          TO MED-IDMFSINF                          
112000     CALL WMEDKONV USING MED-WMEDAREA                                     
112100     MOVE MED-MFSINF             TO MOD-TEMFSINF                          
112200     PERFORM MFS-FORM-ATTR                                                
112300     PERFORM MFS-RENSA-FAELT-IN                                           
112400     .                                                                    
112500     EJECT                                                                
112600                                                                          
112700 HA-BEHANDLA-VALDA-TILLSTAND SECTION.                                     
112800                                                                          
112900     MOVE +1                       TO INDX                                
113000                                      4794-IX                             
113100     PERFORM UNTIL INDX            >  MAX-INDX                            
113200                                                                          
113300        IF MID-KDCMD(INDX)         = W-UTSKRIFT                           
113400                                                                          
113410           INSPECT MID-IDDISTR(INDX) REPLACING                            
113420                   LEADING SPACE BY ZERO                                  
113430           INSPECT MID-IDKUNDNR(INDX) REPLACING                           
113440                   LEADING SPACE BY ZERO                                  
113450           INSPECT MID-IDRAPPNR(INDX) REPLACING                           
113460                   LEADING SPACE BY ZERO                                  
113470                                                                          
113500           MOVE MID-IDDISTR(INDX)  TO W-IDDISTR                           
113600                                      W-IDDISTR-FSEQ-MIN                  
113700                                      MOD4794-MID-IDDISTR(4794-IX)        
113800           MOVE MID-IDKUNDNR(INDX) TO W-IDKUNDNR                          
113900                                      W-IDKUNDNR-FSEQ-MIN                 
114000                                     MOD4794-MID-IDKUNDNR(4794-IX)        
114100           MOVE MID-IDRAPPNR(INDX) TO W-IDRAPPNR                          
114200                                      W-IDRAPPNR-FSEQ-MIN                 
114300                                     MOD4794-MID-IDRAPPNR(4794-IX)        
114400           ADD +1                 TO 4794-IX                              
114500           PERFORM IMS-GHU-WLKREE01                                       
114600           IF SEGMENT-FINNS AND ANM-KDLEVANM = W-ANM-MOT                  
114700              MOVE W-ANM-PAAB      TO ANM-KDLEVANM                        
114800              PERFORM IMS-REPL-WLKREE01                                   
114900**** UPPDATERINGEN BORTTAGEN DÅ DEN STÖR SORTORDNINGEN I KOLLIKÖN         
115000***           PERFORM HAD-UPPDATERA-STATUS                                
115100           END-IF                                                         
115200        END-IF                                                            
115300        ADD +1                     TO INDX                                
115400     END-PERFORM                                                          
115500                                                                          
115600     IF 4794-IX                > +1                                       
115700         PERFORM HAB-STARTA-4794                                          
115800     END-IF                                                               
115900     .                                                                    
116000     EJECT                                                                
116100                                                                          
116200 HAB-STARTA-4794  SECTION.                                                
116300                                                                          
116400     MOVE MFS-KDMFSFOR          TO P-TO-P2-KDMFSFOR                       
116500                                                                          
116600     MOVE 'W40736'              TO MOD4794-MID-IDPGM                      
116700     MOVE PRT-IDPRTLST          TO MOD4794-MID-IDPRTLST                   
116800     MOVE WS-IDDC               TO MOD4794-MID-IDDC                       
116900     COMPUTE MOD4794-MID-KVPOST = 4794-IX - 1                             
117000                                                                          
117100     COMPUTE P-TO-P2-LL = (MOD4794-MID-KVPOST * 17) + 17 + 25             
117200                                                                          
117300     PERFORM IMS-ISRT-MSG-ALT-4794                                        
117400     .                                                                    
117500     EJECT                                                                
117600*HAD-UPPDATERA-STATUS SECTION.                                            
117700*                                                                         
117800*    PERFORM IMS-GHU-RETA4-FSEQ                                           
117900*    PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
118000*       MOVE '5'        TO RET-KDRETSTA                                   
118100*       PERFORM IMS-REPL-RETA4                                            
118200*       PERFORM IMS-GHN-RETA4-FSEQ                                        
118300*    END-PERFORM                                                          
118400*                                                                         
118500*    .                                                                    
118600*    EJECT                                                                
118700 S01-INSERT-ALTMSG SECTION.                                               
118800                                                                          
118900     MOVE P-TO-P-SW            TO MSG-IO-AREA                             
119000     PERFORM IMS-CHANGE-ALTMSG                                            
119100     IF STATUS-OK                                                         
119200       PERFORM IMS-INSERT-ALTMSG                                          
119300     ELSE                                                                 
119400       MOVE LOW-VALUE          TO MSG-AREA                                
119500       MOVE 'W4O73601'         TO MFS-IDMOD                               
119600       MOVE '4736'             TO MOD-IDTRANS                             
119700       MOVE P-TO-P-KDTRANS (2:1) TO W-BILD (1:1)                          
119800       MOVE P-TO-P-KDTRANS (4:3) TO W-BILD (2:3)                          
119900       IF SECURITY-FEL                                                    
120000         STRING 'NOT AUTHORIZED TO USE '                                  
120100                W-BILD                                                    
120200                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
120300       ELSE                                                               
120400         STRING 'WRONG PICTURE '                                          
120500                 W-BILD                                                   
120600                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
120700       END-IF                                                             
120800       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O73601 + 4                      
120900       PERFORM MFS-ROER-EJ-FAELT-IN                                       
121000       PERFORM MFS-ROER-EJ-FAELT-UT                                       
121100       PERFORM IMS-INSERT-MSG                                             
121200     END-IF                                                               
121300     .                                                                    
121400     EJECT                                                                
121500 S02-KOLLA-BEHORIGHET SECTION.                                            
121600                                                                          
121700     MOVE 'F'                     TO SEC-KDSVAR                           
121800     PERFORM UNTIL SEC-KDSVAR NOT = 'F'    OR                             
121900                   SEGMENT-SAKNAS          OR                             
122000                   SEGMENT-SLUT                                           
122100        MOVE MSGI-IDUSER          TO SEC-IDUSER                           
122200        MOVE '4736'               TO SEC-IDTRANS                          
122300        MOVE SEQC-IDDISTR         TO W-IDDISTR-SEC                        
122400        MOVE W-IDDISTR-SEC        TO SEC-IDKEY                            
122500                                                                          
122600        CALL WSECURIT USING SEC-IDUSER                                    
122700                            SEC-IDTRANS                                   
122800                            SEC-IDKEY                                     
122900                            SEC-KDSVAR                                    
123000                                                                          
123100        IF SEC-KDSVAR = 'F'                                               
123200           PERFORM IMS-GN-WLKREH01                                        
123300        END-IF                                                            
123400     END-PERFORM                                                          
123500     .                                                                    
121400     EJECT                                                                
121500 S03-KOLLA-BEHORIGHET SECTION.                                            
121600                                                                          
121700     MOVE 'F'                     TO SEC-KDSVAR                           
121800     PERFORM UNTIL SEC-KDSVAR NOT = 'F'    OR                             
121900                   SEGMENT-SAKNAS          OR                             
122000                   SEGMENT-SLUT                                           
122100        MOVE MSGI-IDUSER          TO SEC-IDUSER                           
122200        MOVE '4736'               TO SEC-IDTRANS                          
122300        MOVE SEQG-IDDISTR         TO W-IDDISTR-SEC                        
122400        MOVE W-IDDISTR-SEC        TO SEC-IDKEY                            
122500                                                                          
122600        CALL WSECURIT USING SEC-IDUSER                                    
122700                            SEC-IDTRANS                                   
122800                            SEC-IDKEY                                     
122900                            SEC-KDSVAR                                    
123000                                                                          
123100        IF SEC-KDSVAR = 'F'                                               
123200           PERFORM IMS-GN-WDA2G1                                          
123300        END-IF                                                            
123400     END-PERFORM                                                          
123500     .                                                                    
123600     EJECT                                                                
123700 S05-BERAKNA-DATUM        SECTION.                                        
123800                                                                          
123900     PERFORM IMS-GU-WL410711                                              
124000                                                                          
124100     MOVE 'IDAG'      TO DAT-KDDATFORM                                    
124200                                                                          
124300     CALL WDATKONV USING DAT-KDDATFORM                                    
124400                         DAT-I-TIDATUM                                    
124500                         DAT-O-TIDATUM                                    
124600                         DAT-KDSVAR                                       
124700     IF DAT-KDSVAR-FEL                                                    
124800        MOVE 'FEL FRÅN DATKONV'   TO FELTEXT                              
124900        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
125000     END-IF                                                               
125100                                                                          
125200     MOVE DAT-TIAADDD     TO W-TIAADDD-IDAG                               
125300                                                                          
125400     IF 4108-KVDAGAR-RTAOSEA >= W-TIDDD-IDAG                              
125500        IF W-TIAA-IDAG          = 00                                      
125600          MOVE 99               TO W-TIAA                                 
125700        ELSE                                                              
125800          COMPUTE W-TIAA        = W-TIAA-IDAG  - 1                        
125900        END-IF                                                            
126000        COMPUTE W-TIDDD         =  365                  -                 
126100                                   4108-KVDAGAR-RTAOSEA +                 
126200                                   W-TIDDD-IDAG                           
126300        MOVE W-TIAADDD          TO DAT-I-TIDATUM                          
126400        MOVE 'AADDD'            TO DAT-KDDATFORM                          
126500                                                                          
126600        CALL WDATKONV USING DAT-KDDATFORM                                 
126700                            DAT-I-TIDATUM                                 
126800                            DAT-O-TIDATUM                                 
126900                            DAT-KDSVAR                                    
127000        IF DAT-KDSVAR-FEL                                                 
127100           MOVE 'FEL FRÅN DATKONV'   TO FELTEXT                           
127200           CALL ABEND USING RKOD-ABEND-MED-DUMP                           
127300        END-IF                                                            
127400                                                                          
127500        MOVE DAT-TIAAMMDD       TO W-TIAAMMDD-RTAOSEA                     
127600        MOVE DAT-TISEKEL        TO W-TISEKEL-RTAOSEA                      
127700     ELSE                                                                 
127800        MOVE W-TIAA-IDAG        TO W-TIAA                                 
127900        COMPUTE W-TIDDD         =  W-TIDDD-IDAG -                         
128000                                   4108-KVDAGAR-RTAOSEA                   
128100        MOVE W-TIAADDD          TO DAT-I-TIDATUM                          
128200        MOVE 'AADDD'            TO DAT-KDDATFORM                          
128300                                                                          
128400        CALL WDATKONV USING DAT-KDDATFORM                                 
128500                            DAT-I-TIDATUM                                 
128600                            DAT-O-TIDATUM                                 
128700                            DAT-KDSVAR                                    
128800        IF DAT-KDSVAR-FEL                                                 
128900           MOVE 'FEL FRÅN DATKONV'   TO FELTEXT                           
129000           CALL ABEND USING RKOD-ABEND-MED-DUMP                           
129100        END-IF                                                            
129200                                                                          
129300        MOVE DAT-TIAAMMDD       TO W-TIAAMMDD-RTAOSEA                     
129400        MOVE DAT-TISEKEL        TO W-TISEKEL-RTAOSEA                      
129500     END-IF                                                               
129600                                                                          
129700     IF 4108-KVDAGAR-RTAOVR >= W-TIDDD-IDAG                               
129800        IF W-TIAA-IDAG          = 00                                      
129900          MOVE 99               TO W-TIAA                                 
130000        ELSE                                                              
130100          COMPUTE W-TIAA        = W-TIAA-IDAG  - 1                        
130200        END-IF                                                            
130300        COMPUTE W-TIDDD         =  365                 -                  
130400                                   4108-KVDAGAR-RTAOVR +                  
130500                                   W-TIDDD-IDAG                           
130600        MOVE W-TIAADDD          TO DAT-I-TIDATUM                          
130700        MOVE 'AADDD'            TO DAT-KDDATFORM                          
130800                                                                          
130900        CALL WDATKONV USING DAT-KDDATFORM                                 
131000                            DAT-I-TIDATUM                                 
131100                            DAT-O-TIDATUM                                 
131200                            DAT-KDSVAR                                    
131300        IF DAT-KDSVAR-FEL                                                 
131400           MOVE 'FEL FRÅN DATKONV'   TO FELTEXT                           
131500           CALL ABEND USING RKOD-ABEND-MED-DUMP                           
131600        END-IF                                                            
131700                                                                          
131800        MOVE DAT-TIAAMMDD       TO W-TIAAMMDD-RTAOVR                      
131900        MOVE DAT-TISEKEL        TO W-TISEKEL-RTAOVR                       
132000     ELSE                                                                 
132100        MOVE W-TIAA-IDAG        TO W-TIAA                                 
132200        COMPUTE W-TIDDD         =  W-TIDDD-IDAG -                         
132300                                   4108-KVDAGAR-RTAOVR                    
132400        MOVE W-TIAADDD          TO DAT-I-TIDATUM                          
132500        MOVE 'AADDD'            TO DAT-KDDATFORM                          
132600                                                                          
132700        CALL WDATKONV USING DAT-KDDATFORM                                 
132800                            DAT-I-TIDATUM                                 
132900                            DAT-O-TIDATUM                                 
133000                            DAT-KDSVAR                                    
133100        IF DAT-KDSVAR-FEL                                                 
133200           MOVE 'FEL FRÅN DATKONV'   TO FELTEXT                           
133300           CALL ABEND USING RKOD-ABEND-MED-DUMP                           
133400        END-IF                                                            
133500                                                                          
133600        MOVE DAT-TIAAMMDD       TO W-TIAAMMDD-RTAOVR                      
133700        MOVE DAT-TISEKEL        TO W-TISEKEL-RTAOVR                       
133800     END-IF                                                               
133900                                                                          
134000     IF 4108-KVDAGAR-RTM >= W-TIDDD-IDAG                                  
134100        IF W-TIAA-IDAG          = 00                                      
134200          MOVE 99               TO W-TIAA                                 
134300        ELSE                                                              
134400          COMPUTE W-TIAA        = W-TIAA-IDAG  - 1                        
134500        END-IF                                                            
134600        COMPUTE W-TIDDD         =  365                 -                  
134700                                   4108-KVDAGAR-RTM    +                  
134800                                   W-TIDDD-IDAG                           
134900        MOVE W-TIAADDD          TO DAT-I-TIDATUM                          
135000        MOVE 'AADDD'            TO DAT-KDDATFORM                          
135100                                                                          
135200        CALL WDATKONV USING DAT-KDDATFORM                                 
135300                            DAT-I-TIDATUM                                 
135400                            DAT-O-TIDATUM                                 
135500                            DAT-KDSVAR                                    
135600        IF DAT-KDSVAR-FEL                                                 
135700           MOVE 'FEL FRÅN DATKONV'   TO FELTEXT                           
135800           CALL ABEND USING RKOD-ABEND-MED-DUMP                           
135900        END-IF                                                            
136000                                                                          
136100        MOVE DAT-TIAAMMDD       TO W-TIAAMMDD-RTM                         
136200        MOVE DAT-TISEKEL        TO W-TISEKEL-RTM                          
136300     ELSE                                                                 
136400        MOVE W-TIAA-IDAG        TO W-TIAA                                 
136500        COMPUTE W-TIDDD         =  W-TIDDD-IDAG -                         
136600                                   4108-KVDAGAR-RTM                       
136700        MOVE W-TIAADDD          TO DAT-I-TIDATUM                          
136800        MOVE 'AADDD'            TO DAT-KDDATFORM                          
136900                                                                          
137000        CALL WDATKONV USING DAT-KDDATFORM                                 
137100                            DAT-I-TIDATUM                                 
137200                            DAT-O-TIDATUM                                 
137300                            DAT-KDSVAR                                    
137400        IF DAT-KDSVAR-FEL                                                 
137500           MOVE 'FEL FRÅN DATKONV'   TO FELTEXT                           
137600           CALL ABEND USING RKOD-ABEND-MED-DUMP                           
137700        END-IF                                                            
137800                                                                          
137900        MOVE DAT-TIAAMMDD       TO W-TIAAMMDD-RTM                         
138000        MOVE DAT-TISEKEL        TO W-TISEKEL-RTM                          
138100     END-IF                                                               
138200                                                                          
138300     IF 4108-KVDAGAR-RTP >= W-TIDDD-IDAG                                  
138400        IF W-TIAA-IDAG          = 00                                      
138500          MOVE 99               TO W-TIAA                                 
138600        ELSE                                                              
138700          COMPUTE W-TIAA        = W-TIAA-IDAG  - 1                        
138800        END-IF                                                            
138900        COMPUTE W-TIDDD         =  365                 -                  
139000                                   4108-KVDAGAR-RTP    +                  
139100                                   W-TIDDD-IDAG                           
139200        MOVE W-TIAADDD          TO DAT-I-TIDATUM                          
139300        MOVE 'AADDD'            TO DAT-KDDATFORM                          
139400                                                                          
139500        CALL WDATKONV USING DAT-KDDATFORM                                 
139600                            DAT-I-TIDATUM                                 
139700                            DAT-O-TIDATUM                                 
139800                            DAT-KDSVAR                                    
139900        IF DAT-KDSVAR-FEL                                                 
140000           MOVE 'FEL FRÅN DATKONV'   TO FELTEXT                           
140100           CALL ABEND USING RKOD-ABEND-MED-DUMP                           
140200        END-IF                                                            
140300                                                                          
140400        MOVE DAT-TIAAMMDD       TO W-TIAAMMDD-RTP                         
140500        MOVE DAT-TISEKEL        TO W-TISEKEL-RTP                          
140600     ELSE                                                                 
140700        MOVE W-TIAA-IDAG        TO W-TIAA                                 
140800        COMPUTE W-TIDDD         =  W-TIDDD-IDAG -                         
140900                                   4108-KVDAGAR-RTP                       
141000        MOVE W-TIAADDD          TO DAT-I-TIDATUM                          
141100        MOVE 'AADDD'            TO DAT-KDDATFORM                          
141200                                                                          
141300        CALL WDATKONV USING DAT-KDDATFORM                                 
141400                            DAT-I-TIDATUM                                 
141500                            DAT-O-TIDATUM                                 
141600                            DAT-KDSVAR                                    
141700        IF DAT-KDSVAR-FEL                                                 
141800           MOVE 'FEL FRÅN DATKONV'   TO FELTEXT                           
141900           CALL ABEND USING RKOD-ABEND-MED-DUMP                           
142000        END-IF                                                            
142100                                                                          
142200        MOVE DAT-TIAAMMDD       TO W-TIAAMMDD-RTP                         
142300        MOVE DAT-TISEKEL        TO W-TISEKEL-RTP                          
142400     END-IF                                                               
142500     .                                                                    
142600     EJECT                                                                
142700 MFS-RENSA-FAELT-UT SECTION.                                              
142800                                                                          
142900     MOVE MFS-RENSA-FAELT       TO MOD-KVANT-RT                           
143000                                   MOD-KVRADER-RT                         
143100                                                                          
143200     MOVE +1                    TO INDX                                   
143300     PERFORM UNTIL INDX         >  MAX-INDX                               
143400        PERFORM MFS-RENSA-RAD-FAELT-UT                                    
143500        ADD +1                  TO INDX                                   
143600     END-PERFORM                                                          
143700     .                                                                    
143800     SKIP3                                                                
143900 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
144000                                                                          
144100     MOVE MFS-RENSA-FAELT TO MOD-IDANSV   (INDX)                          
144200                             MOD-TIRETILL (INDX)                          
144300                             MOD-TIRETANK (INDX)                          
144400                             MOD-IDDISTR  (INDX)                          
144500                             MOD-IDKUNDNR (INDX)                          
144600                             MOD-IDRAPPNR (INDX)                          
144700                             MOD-KDLEVANM (INDX)                          
144700                             MOD-KVRADER  (INDX)                          
144800                             MOD-KVRADER-OBEH  (INDX)                     
144900                             MOD-KVKOLLI  (INDX)                          
145000     .                                                                    
145100     SKIP3                                                                
145200 MFS-RENSA-FAELT-IN SECTION.                                              
145300                                                                          
145400*    --- ALLA INDATA-FÄLT                                                 
145500     MOVE MFS-RENSA-FAELT       TO MOD-IDPRT                              
145600                                                                          
145700     MOVE +1 TO INDX                                                      
145800     PERFORM UNTIL INDX         >  MAX-INDX                               
145900       MOVE MFS-RENSA-FAELT     TO MOD-KDCMD(INDX)                        
146000       ADD +1                   TO INDX                                   
146100     END-PERFORM                                                          
146200     .                                                                    
146300     EJECT                                                                
146400 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
146500                                                                          
146600     MOVE MFS-ROER-EJ-FAELT     TO MOD-KVANT-RT                           
146700                                   MOD-KVRADER-RT                         
146800                                                                          
146900     MOVE +1                  TO INDX                                     
147000     PERFORM UNTIL INDX       >  MAX-INDX                                 
147100       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
147200       ADD +1                 TO INDX                                     
147300     END-PERFORM                                                          
147400     .                                                                    
147500                                                                          
147600                                                                          
147700 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
147800                                                                          
147900     MOVE MFS-ROER-EJ-FAELT  TO MOD-IDANSV   (INDX)                       
148000                                MOD-TIRETILL (INDX)                       
148100                                MOD-TIRETANK (INDX)                       
148200                                MOD-IDDISTR (INDX)                        
148300                                MOD-IDKUNDNR (INDX)                       
148400                                MOD-IDRAPPNR (INDX)                       
148500                                MOD-KDLEVANM (INDX)                       
148500                                MOD-KVRADER (INDX)                        
148600                                MOD-KVRADER-OBEH (INDX)                   
148700                                MOD-KVKOLLI (INDX)                        
148800     .                                                                    
148900                                                                          
149000 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
149100                                                                          
149200*    --- ALLA INDATA-FÄLT                                                 
149300     MOVE MFS-ROER-EJ-FAELT     TO MOD-IDPRT                              
149400                                                                          
149500     MOVE +1 TO INDX                                                      
149600     PERFORM UNTIL INDX         >  MAX-INDX                               
149700       MOVE MFS-ROER-EJ-FAELT   TO MOD-KDCMD(INDX)                        
149800       ADD +1                   TO INDX                                   
149900     END-PERFORM                                                          
150000     .                                                                    
150100     EJECT                                                                
150200 MFS-FORM-ATTR SECTION.                                                   
150300                                                                          
150400*    --- ALLA INDATA-FÄLT                                                 
150500     MOVE MFS-FORMATETS-ATTR    TO MOD-IDPRT-ATTR                         
150600                                                                          
150700     MOVE +1 TO INDX                                                      
150800     PERFORM UNTIL INDX         >  MAX-INDX                               
150900       MOVE MFS-FORMATETS-ATTR  TO MOD-KDCMD-ATTR(INDX)                   
151000                                   MOD-IDANSV-ATTR(INDX)                  
151100                                   MOD-TIRETILL-ATTR(INDX)                
151200                                   MOD-TIRETANK-ATTR(INDX)                
151300                                   MOD-IDDISTR-ATTR(INDX)                 
151400                                   MOD-IDKUNDNR-ATTR(INDX)                
151500                                   MOD-IDRAPPNR-ATTR(INDX)                
151600                                   MOD-KDLEVANM-ATTR(INDX)                
151600                                   MOD-KVRADER-ATTR(INDX)                 
151700                                   MOD-KVRADER-OBEH-ATTR(INDX)            
151800                                   MOD-KVKOLLI-ATTR(INDX)                 
151900       ADD +1                   TO INDX                                   
152000     END-PERFORM                                                          
152100     .                                                                    
152200     EJECT                                                                
152300 MFS-FORM-ATTR-UTRAD SECTION.                                             
152400                                                                          
152500*    --- EJ UPPLYST RAD                                                   
152600     MOVE MFS-FORMATETS-ATTR  TO MOD-KDCMD-ATTR(INDX)                     
152700                                 MOD-IDANSV-ATTR(INDX)                    
152800                                 MOD-TIRETILL-ATTR(INDX)                  
152900                                 MOD-TIRETANK-ATTR(INDX)                  
153000                                 MOD-IDDISTR-ATTR(INDX)                   
153100                                 MOD-IDKUNDNR-ATTR(INDX)                  
153200                                 MOD-IDRAPPNR-ATTR(INDX)                  
153300                                 MOD-KDLEVANM-ATTR(INDX)                  
153300                                 MOD-KVRADER-ATTR(INDX)                   
153400                                 MOD-KVRADER-OBEH-ATTR(INDX)              
153500                                 MOD-KVKOLLI-ATTR(INDX)                   
153600                                                                          
153700     .                                                                    
153800     EJECT                                                                
153900 MFS-LYS-UPP-UTRAD SECTION.                                               
154000                                                                          
154100*    --- EJ UPPLYST RAD                                                   
154200     MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDCMD-ATTR(INDX)                   
154300                                   MOD-IDANSV-ATTR(INDX)                  
154400                                   MOD-TIRETILL-ATTR(INDX)                
154500                                   MOD-TIRETANK-ATTR(INDX)                
154600                                   MOD-IDDISTR-ATTR(INDX)                 
154700                                   MOD-IDKUNDNR-ATTR(INDX)                
154800                                   MOD-IDRAPPNR-ATTR(INDX)                
154900                                   MOD-KDLEVANM-ATTR(INDX)                
154900                                   MOD-KVRADER-ATTR(INDX)                 
155000                                   MOD-KVRADER-OBEH-ATTR(INDX)            
155100                                   MOD-KVKOLLI-ATTR(INDX)                 
155200                                                                          
155300     .                                                                    
155400     EJECT                                                                
155500* --- IMS SEKTIONER ---                                                   
155600     SKIP3                                                                
155700 IMS-GET-MSG SECTION.                                                     
155800                                                                          
155900     MOVE '  QC' TO GODK-STATUSKODER                                      
156000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
156100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
156200     PERFORM IMS-STATUSKONTROLL                                           
156300     .                                                                    
156400     SKIP3                                                                
156500 IMS-INSERT-MSG SECTION.                                                  
156600                                                                          
156700     IF MSGI-IDLAND-SPR = 'GB'                                            
156800       MOVE 'N' TO MFS-KDHUVOMR                                           
156900     END-IF                                                               
157000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
157100     MOVE SPACE TO GODK-STATUSKODER                                       
157200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
157300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
157400     PERFORM IMS-STATUSKONTROLL                                           
157500     .                                                                    
157600     EJECT                                                                
157700                                                                          
157800 IMS-ISRT-MSG-ALT-4794 SECTION.                                           
157900                                                                          
158000     MOVE SPACE              TO GODK-STATUSKODER                          
158100     CALL CBLTDLI USING      ISRT W4794-PCB                               
158200                                  P-TO-P-T94                              
158300     MOVE W4794-STATUS-CODE   TO STATUS-WS                                
158400     PERFORM IMS-STATUSKONTROLL                                           
158500     .                                                                    
158600     SKIP3                                                                
158700 IMS-CHANGE-ALTMSG SECTION.                                               
158800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
158900     MOVE '  A1A4' TO GODK-STATUSKODER                                    
159000     CALL CBLTDLI USING CHNG ALT-PCB MSG-KDTRANS-1                        
159100     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
159200     PERFORM IMS-STATUSKONTROLL                                           
159300     .                                                                    
159400     SKIP3                                                                
159500 IMS-INSERT-ALTMSG SECTION.                                               
159600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
159700     MOVE SPACE TO GODK-STATUSKODER                                       
159800     CALL CBLTDLI USING ISRT ALT-PCB MSG-IO-AREA                          
159900     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
160000     PERFORM IMS-STATUSKONTROLL                                           
160100     .                                                                    
160200     EJECT                                                                
160300 IMS-GU-WL410711            SECTION.                                      
160400                                                                          
160500     STRING 'WL410701(WDGXKEY  =' W-WDGX4107-X ')'                        
160600          DELIMITED BY SIZE INTO SSA1                                     
160700     STRING 'WL410711(KDSEGKEY =' W-KDSEGKEY-X ')'                        
160800          DELIMITED BY SIZE INTO SSA2                                     
160900     MOVE '  '           TO GODK-STATUSKODER                              
161000     CALL CBLTDLI USING GU 4107-PCB DLI-IO-AREA SSA1 SSA2                 
161100     MOVE 4107-STATUS-CODE TO STATUS-WS                                   
161200     PERFORM IMS-STATUSKONTROLL                                           
161300     .                                                                    
161400 IMS-GHU-WLKREE01       SECTION.                                          
161500                                                                          
161600     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
161700          DELIMITED BY SIZE INTO SSA1                                     
161800     MOVE '  GE'           TO GODK-STATUSKODER                            
161900     CALL CBLTDLI USING GHU KREE-PCB DLI-IO-AREA SSA1                     
162000     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
162100     PERFORM IMS-STATUSKONTROLL                                           
162200     .                                                                    
162300                                                                          
162400 IMS-REPL-WLKREE01      SECTION.                                          
162500                                                                          
162600     MOVE '    '           TO GODK-STATUSKODER                            
162700     CALL CBLTDLI USING REPL KREE-PCB DLI-IO-AREA                         
162800     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
162900     PERFORM IMS-STATUSKONTROLL                                           
163000     .                                                                    
163100     EJECT                                                                
163200                                                                          
163300 IMS-GU-WLKREH01       SECTION.                                           
163400                                                                          
163500     STRING 'WLKREH01(WDA2C1KY>=' W-WDA2C1KY-MIN-X                        
163600                    '&WDA2C1KY<=' W-WDA2C1KY-MAX-X                        
163700                    '&IDDISTR >=' W-IDDISTR-MIN-X                         
163800                    '&IDDISTR <=' W-IDDISTR-MAX-X                         
163900                    '&IDKUNDNR>=' W-IDKUNDNR-MIN-X                        
164000                    '&IDKUNDNR<=' W-IDKUNDNR-MAX-X ')'                    
164100          DELIMITED BY SIZE INTO SSA1                                     
164200     MOVE '  GE'           TO GODK-STATUSKODER                            
164300     CALL CBLTDLI USING GU KREH-PCB DLI-IO-AREA SSA1                      
164400     MOVE KREH-STATUS-CODE TO STATUS-WS                                   
164500     PERFORM IMS-STATUSKONTROLL                                           
164600     .                                                                    
164700                                                                          
164800 IMS-GN-WLKREH01       SECTION.                                           
164900                                                                          
165000     STRING 'WLKREH01(WDA2C1KY>=' W-WDA2C1KY-MIN-X                        
165100                    '&WDA2C1KY<=' W-WDA2C1KY-MAX-X                        
165200                    '&IDDISTR >=' W-IDDISTR-MIN-X                         
165300                    '&IDDISTR <=' W-IDDISTR-MAX-X                         
165400                    '&IDKUNDNR>=' W-IDKUNDNR-MIN-X                        
165500                    '&IDKUNDNR<=' W-IDKUNDNR-MAX-X ')'                    
165600          DELIMITED BY SIZE INTO SSA1                                     
165700     MOVE '  GEGB'           TO GODK-STATUSKODER                          
165800     CALL CBLTDLI USING GN KREH-PCB DLI-IO-AREA SSA1                      
165900     MOVE KREH-STATUS-CODE TO STATUS-WS                                   
166000     PERFORM IMS-STATUSKONTROLL                                           
166100     .                                                                    
163100     EJECT                                                                
163200                                                                          
163300 IMS-GU-WDA2G1         SECTION.                                           
163400                                                                          
163500     STRING 'WDA2G1  (WDA2G1KY>=' W-WDA2G1KY-MIN-X                        
163600                    '&WDA2G1KY<=' W-WDA2G1KY-MAX-X                        
163700                    '&IDDISTR >=' W-IDDISTR-MIN-X                         
163800                    '&IDDISTR <=' W-IDDISTR-MAX-X                         
163900                    '&IDKUNDNR>=' W-IDKUNDNR-MIN-X                        
164000                    '&IDKUNDNR<=' W-IDKUNDNR-MAX-X ')'                    
164100          DELIMITED BY SIZE INTO SSA1                                     
164200     MOVE '  GE'           TO GODK-STATUSKODER                            
164300     CALL CBLTDLI USING GU WDA2G-PCB DLI-IO-AREA SSA1                     
164400     MOVE WDA2G-STATUS-CODE TO STATUS-WS                                  
164500     PERFORM IMS-STATUSKONTROLL                                           
164600     .                                                                    
164700                                                                          
164800 IMS-GN-WDA2G1         SECTION.                                           
164900                                                                          
165000     STRING 'WDA2G1  (WDA2G1KY>=' W-WDA2G1KY-MIN-X                        
165100                    '&WDA2G1KY<=' W-WDA2G1KY-MAX-X                        
165200                    '&IDDISTR >=' W-IDDISTR-MIN-X                         
165300                    '&IDDISTR <=' W-IDDISTR-MAX-X                         
165400                    '&IDKUNDNR>=' W-IDKUNDNR-MIN-X                        
165500                    '&IDKUNDNR<=' W-IDKUNDNR-MAX-X ')'                    
165600          DELIMITED BY SIZE INTO SSA1                                     
165700     MOVE '  GEGB'           TO GODK-STATUSKODER                          
165800     CALL CBLTDLI USING GN WDA2G-PCB DLI-IO-AREA SSA1                     
165900     MOVE WDA2G-STATUS-CODE TO STATUS-WS                                  
166000     PERFORM IMS-STATUSKONTROLL                                           
166100     .                                                                    
166200     EJECT                                                                
166300                                                                          
166400 IMS-GU-WLRETA01       SECTION.                                           
166500                                                                          
166600     STRING 'WLRETA01(WDA3FSEQ>=' W-WDA3FSEQ-MIN-X                        
166700                    '&WDA3FSEQ<=' W-WDA3FSEQ-MAX-X ')'                    
166800          DELIMITED BY SIZE INTO SSA1                                     
166900     MOVE '  GE'           TO GODK-STATUSKODER                            
167000     CALL CBLTDLI USING GU RETA-PCB DLI-IO-AREA SSA1                      
167100     MOVE RETA-STATUS-CODE TO STATUS-WS                                   
167200     PERFORM IMS-STATUSKONTROLL                                           
167300     .                                                                    
167400                                                                          
167500                                                                          
167600*IMS-GHU-RETA4-FSEQ SECTION.                                              
167700*                                                                         
167800*    STRING 'WLRETA01(WDA3FSEQ>=' W-WDA3FSEQ-MIN-X                        
167900*                   '&WDA3FSEQ<=' W-WDA3FSEQ-MAX-X ')'                    
168000*         DELIMITED BY SIZE INTO SSA1                                     
168100*    MOVE '  GE' TO GODK-STATUSKODER                                      
168200*    CALL CBLTDLI USING GHU RETA4-PCB DLI-IO-AREA SSA1                    
168300*    MOVE RETA4-STATUS-CODE TO STATUS-WS                                  
168400*    PERFORM IMS-STATUSKONTROLL                                           
168500*    .                                                                    
168600*    SKIP2                                                                
168700*IMS-GHN-RETA4-FSEQ SECTION.                                              
168800*                                                                         
168900*    STRING 'WLRETA01(WDA3FSEQ>=' W-WDA3FSEQ-MIN-X                        
169000*                   '&WDA3FSEQ<=' W-WDA3FSEQ-MAX-X ')'                    
169100*         DELIMITED BY SIZE INTO SSA1                                     
169200*    MOVE '  GEGB' TO GODK-STATUSKODER                                    
169300*    CALL CBLTDLI USING GHN RETA4-PCB DLI-IO-AREA SSA1                    
169400*    MOVE RETA4-STATUS-CODE TO STATUS-WS                                  
169500*    PERFORM IMS-STATUSKONTROLL                                           
169600*    .                                                                    
169700*    SKIP2                                                                
169800*IMS-REPL-RETA4     SECTION.                                              
169900*                                                                         
170000*    MOVE '  ' TO GODK-STATUSKODER                                        
170100*    CALL CBLTDLI USING REPL RETA4-PCB DLI-IO-AREA                        
170200*    MOVE RETA4-STATUS-CODE TO STATUS-WS                                  
170300*    PERFORM IMS-STATUSKONTROLL                                           
170400*    .                                                                    
170500*    SKIP2                                                                
170600 IMS-STATUSKONTROLL SECTION.                                              
170700                                                                          
170800     SET STATUS-IX TO 1                                                   
170900     SEARCH GODK-STATUS                                                   
171000       AT END                                                             
171100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
171200         DELIMITED BY SIZE INTO FELTEXT                                   
171300         CALL FELLOG                                                      
171400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
171500         CONTINUE                                                         
171600     END-SEARCH                                                           
171700     .                                                                    
