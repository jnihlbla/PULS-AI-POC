000100*                                                                         
000200******************************************************************        
000300*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0150      *        
000400******************************************************************        
000500*                                                                         
000600 ID DIVISION.                                                             
000700 PROGRAM-ID.     W4073500.                                                
000800 AUTHOR.         EVA LUNDELL.                                             
000900 DATE-WRITTEN.   95/07/11.                                                
001000 DATE-COMPILED.                                                           
001100                                                                          
001200*                                                                         
001300*    FUNKTION:                                                            
001400*        PROGRAMMET VISAR VAD SOM INGÅR I ETT ELLER FLERA                 
001500*        KOLLIN.                                                          
001600*                                                                         
001700*        PROGRAMMET UPPDATERAR WLKREE (WDA2)                              
001800*        PROGRAMMET UPPDATERAR WLRETA (WDA3)                              
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSAKTION: W4T735                                              
002200*        MID:         W4I73501                                            
002300*                                                                         
002400*    UTDATA.                                                              
002500*        MOD:         W4O73501                                            
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900 DATA DIVISION.                                                           
003000     EJECT                                                                
003100 WORKING-STORAGE SECTION.                                                 
003200                                                                          
003300*    -- CHECKED BY WY2000                                                 
003400 77  IDPGM                       PIC X(08)   VALUE 'W4073500'.            
003500                                                                          
003600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003700 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003800                                                                          
003900 77  JA                          PIC X       VALUE 'J'.                   
004000 77  YES                         PIC X       VALUE 'Y'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200 01  WS-IDSNDNNR.                                                         
004300     03  WS-IDRT                 PIC X(3)    VALUE SPACE.                 
004400     03  WS-IDRTLOP              PIC X(3)    VALUE SPACE.                 
004500 77  W-ANM-UTF                   PIC X(1)    VALUE '4'.                   
004600 77  W-ANM-MOT                   PIC X(1)    VALUE '5'.                   
004700 77  W-ANM-PAAB                  PIC X(1)    VALUE '6'.                   
004800 77  W-UTSKRIFT                  PIC X(1)    VALUE 'X'.                   
004900 77  W-BORTTAG                   PIC X(1)    VALUE 'B'.                   
005000 77  W-PRINT                     PIC X(1)    VALUE 'X'.                   
005100 77  W-DELETE                    PIC X(1)    VALUE 'D'.                   
005200 77  W-ETIKETT                   PIC X(2)    VALUE 'BC'.                  
005300 77  W-TEXT                      PIC X(1)    VALUE 'T'.                   
005400 77  RAD-IX                      PIC S9(4)   VALUE ZERO.                  
005500 77  4794-IX                     PIC S9(4)   VALUE ZERO.                  
005600 77  4794-MAX-IX                 PIC S9(4)   VALUE +13.                   
005700 77  MAX-IX                      PIC S9(5)   VALUE +13.                   
005800 77  SPARA-IDRAPPNR              PIC 9(7)    VALUE ZERO.                  
005900 77  SPARA-IDKUNDNR              PIC S9(7) COMP-3 VALUE ZERO.             
006000 77  SPARA-IDDISTR               PIC S9(5) COMP-3 VALUE ZERO.             
006100 77  ANTAL-KOLLIN                PIC S9(3) COMP-3 VALUE ZERO.             
006200 77  WS-IDDISTR-4                PIC 9(4)    VALUE ZERO.                  
006300 77  WS-IDKUNDNR-6               PIC 9(6)    VALUE ZERO.                  
006400 77  W-IDKOLLI-SPAR              PIC S9(5) COMP-3 VALUE ZERO.             
006500                                                                          
006600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006700                                                                          
006800 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006900     88  INDATA-OK                           VALUE 'J'.                   
007000     88  INDATA-FEL                          VALUE 'N'.                   
007100                                                                          
007200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007300     88  NYCKLAR-OK                          VALUE 'J'.                   
007400     88  NYCKLAR-FEL                         VALUE 'N'.                   
007500                                                                          
007600 77  IDSNDNNR-SW                 PIC X       VALUE 'J'.                   
007700     88  IDSNDNNR-FINNS                      VALUE 'J'.                   
007800     88  IDSNDNNR-FEL                        VALUE 'N'.                   
007900                                                                          
008000 77  PRINTAT-SW                  PIC X       VALUE 'N'.                   
008100     88  PRINTAT                             VALUE 'J'.                   
008200                                                                          
008300 77  IDDISTR-SW                  PIC X       VALUE 'J'.                   
008400     88  IDDISTR-FINNS                       VALUE 'J'.                   
008500     88  IDDISTR-FEL                         VALUE 'N'.                   
008600                                                                          
008700 77  IDKUNDNR-SW                 PIC X       VALUE 'J'.                   
008800     88  IDKUNDNR-FINNS                      VALUE 'J'.                   
008900     88  IDKUNDNR-FEL                        VALUE 'N'.                   
009000                                                                          
009100 77  IDRAPPNR-SW                 PIC X       VALUE 'J'.                   
009200     88  IDRAPPNR-FINNS                      VALUE 'J'.                   
009300     88  IDRAPPNR-FEL                        VALUE 'N'.                   
009400                                                                          
009500 77  IDKOLLI-SW                  PIC X       VALUE 'J'.                   
009600     88  IDKOLLI-FINNS                       VALUE 'J'.                   
009700     88  IDKOLLI-FEL                         VALUE 'N'.                   
009800                                                                          
009900 77  SW-STARTA-ANNAN-BILD        PIC X       VALUE 'N'.                   
010000     88  STARTA-ANNAN-BILD                   VALUE 'J'.                   
010100                                                                          
010200 77  SW-KDCMD                    PIC X       VALUE 'N'.                   
010300     88  KDCMD-IFYLLT                        VALUE 'J'.                   
010400     88  KDCMD-SAKNAS                        VALUE 'N'.                   
010500                                                                          
010600 77  SW-KDCMD-RAETT-IFYLLD       PIC X       VALUE 'N'.                   
010700     88  KDCMD-RAETT                         VALUE 'J'.                   
010800     88  KDCMD-FEL                           VALUE 'N'.                   
010900                                                                          
011000 77  SW-IDANSTNR-RAETT-IFYLLD    PIC X       VALUE 'J'.                   
011100     88  IDANSTNR-RAETT                      VALUE 'J'.                   
011200     88  IDANSTNR-FEL                        VALUE 'N'.                   
011300                                                                          
011400 77  SW-IDPRT-RAETT-IFYLLD       PIC X       VALUE 'J'.                   
011500     88  IDPRT-RAETT                         VALUE 'J'.                   
011600     88  IDPRT-FEL                           VALUE 'N'.                   
011700                                                                          
011800 77  SW-KDCMD-BORTTAG            PIC X       VALUE 'N'.                   
011900     88  KDCMD-BORTTAG                       VALUE 'J'.                   
012000                                                                          
012100 77  SW-KDCMD-UTSKRIFT           PIC X       VALUE 'N'.                   
012200     88  KDCMD-UTSKRIFT                      VALUE 'J'.                   
012300                                                                          
012400 77  SW-KDCMD-ETIKETT            PIC X       VALUE 'N'.                   
012500     88  KDCMD-ETIKETT                       VALUE 'J'.                   
012600                                                                          
012700 77  SW-KDCMD-TEXT               PIC X       VALUE 'N'.                   
012800     88  KDCMD-TEXT                          VALUE 'J'.                   
012900                                                                          
013000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
013100     88  EGEN-MID                            VALUE '4735'.                
013200     88  GODK-MID                            VALUE '4731' '4732'          
013300                                                   '4733' '4734'          
013400                                                   '4735' '4736'          
013500                                                   '4737' '4738'          
013600                                                   '4739'.                
013700     88  HELP-MID                            VALUE '0551'.                
013800     EJECT                                                                
013900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
014000 01  GENERELLA-SUBPROGRAM.                                                
014100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
014200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
014300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014500     03  W006PRT                 PIC X(8)    VALUE 'W006PRT'.             
014600     03  W418OKOD                PIC X(8)    VALUE 'W418OKOD'.            
014700     EJECT                                                                
014800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
014900*01 -COPY WMEDAREA                                                        
015000     SKIP3                                                                
015100*    ---  LÄNKAREA TILL W418OKOD                                          
015200 01  FILLER                      PIC X(16)   VALUE 'W418OKOD'.            
015300                                                                          
015400*01 -COPY W418OKOD           -PRE OKOD-.                                  
015500     EJECT                                                                
015600 01  MESSAGE-CODES.                                                       
015700     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
015800     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
015900     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
016000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
016100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
016200     03  ERR-URVAL-SAKNAS        PIC X(3)    VALUE '005'.                 
016300     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
016400     03  INF-PRESS-PF4           PIC X(3)    VALUE '081'.                 
016500     03  INF-PRINT-BEG           PIC X(3)    VALUE '118'.                 
016600     03  INF-MORE-INFO-FINNS     PIC X(3)    VALUE '105'.                 
016700     03  ERR-NOTHING-PRINTED     PIC X(3)    VALUE '167'.                 
016800     03  ERR-NO-LINE-CHOSEN      PIC X(3)    VALUE '231'.                 
016900     03  ERR-WRONG-PRINTER       PIC X(3)    VALUE '772'.                 
017000     03  ERR-FLERA-FUNKTIONER    PIC X(3)    VALUE '097'.                 
017100     03  ERR-UPPGIFTER-SAKNAS    PIC X(3)    VALUE '760'.                 
017200     03  ERR-ADD-CASE-INFO       PIC X(3)    VALUE '728'.                 
017300     03  ERR-WRONG-COMMAND-CODE  PIC X(3)    VALUE '304'.                 
017400     EJECT                                                                
017500 01  PROG-TO-PROG-SW.                                                     
017600*    03  -COPY WMSGSOP                                                    
017700     SKIP3                                                                
017800 01  WS-BC-PARAMETRAR.                                                    
017900     03  WS-BC.                                                           
018000         05  BC-URV-IDDISTR      PIC 9(5)  VALUE ZERO.                    
018100         05  BC-URV-IDKUNDNR     PIC 9(7)  VALUE ZERO.                    
018200         05  BC-URV-IDRAPPNR     PIC 9(7)  VALUE ZERO.                    
018300     EJECT                                                                
018400 01  BILD-HOPP-AREOR.                                                     
018500   03    W-BILD               PIC X(4)    VALUE SPACE.                    
018600   03    W-HOPP-IDTRANS.                                                  
018700     05  FILLER               PIC X(1)    VALUE 'W'.                      
018800     05  W-HOPP-IDTRANS-2     PIC X(1).                                   
018900     05  FILLER               PIC X(1)    VALUE 'T'.                      
019000     05  W-HOPP-IDTRANS-4-6   PIC X(3).                                   
019100     05  FILLER               PIC X(2)    VALUE SPACE.                    
019200                                                                          
019300**--- PARAMETRAR TILL SUBPROGRAM W006PRT                                  
019400 01  FILLER                      PIC X(16)   VALUE 'W006PRT-AREA'.        
019500*01  -COPY W006PRT                                                        
019600     EJECT                                                                
019700   03 FILLER             PIC X(16)   VALUE 'P-TO-P-AREA'.                 
019800   03      P-TO-P-SW.                                                     
019900                                                                          
020000     05  P-TO-P-KVLL             PIC S9(4) VALUE +117 COMP SYNC.          
020100     05  P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
020200     05  P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
020300     05  P-TO-P-KDTRANS          PIC X(8).                                
020400     05  P-TO-P-IDTRANS          PIC X(4).                                
020500     05  P-TO-P-KDMFSFOR         PIC X(1).                                
020600     05  P-TO-P-DATA             PIC X(100) VALUE ALL '+'.                
020700                                                                          
020800   03 FILLER             PIC X(16)   VALUE 'P-TO-P-AREA2'.                
020900*----TILL W40794                                                          
021000 01  P-TO-P-T94.                                                          
021100     03  P-TO-P2-LL              PIC S9(4)   COMP SYNC.                   
021200     03  P-TO-P2-Z1              PIC X(1)    VALUE LOW-VALUE.             
021300     03  P-TO-P2-Z2              PIC X(1)    VALUE LOW-VALUE.             
021400     03  P-TO-P2-TRANSKOD        PIC X(7)    VALUE 'W4T794X'.             
021500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
021600     03  P-TO-P2-IDTRANS         PIC X(4)    VALUE '4735'.                
021700     03  P-TO-P2-KDMFSFOR        PIC X(1)    VALUE SPACE.                 
021800*    03  MID -COPY W4I79401 -PRE MOD4794-                                 
021900     EJECT                                                                
022000*                                                                         
022100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
022200*                                                                         
022300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
022400     SKIP3                                                                
022500*01 -COPY WMSGINIT                                                        
022600     SKIP3                                                                
022700*                                                                         
022800*    --- VALID IDDC CODES                                                 
022900*01 -COPY WWDC99                                                          
023000*                                                                         
023100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
023200*                                                                         
023300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
023400     SKIP3                                                                
023500*01  MID -COPY W4I73501                                                   
023600     EJECT                                                                
023700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
023800     SKIP3                                                                
023900*01  -COPY WMSGAREA                                                       
024000     EJECT                                                                
024100     03  MOD REDEFINES MSG-AREA.                                          
024200*      05  -COPY W4O73501  -PRE MOD-                                      
024300     EJECT                                                                
024400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
024500     SKIP3                                                                
024600*01  -COPY WMFSAREA                                                       
024700     EJECT                                                                
024800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
024900*                                                                         
025000     EJECT                                                                
025100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
025200 01  NYCKLAR-TILL-BLAEDDRING.                                             
025300     SKIP3                                                                
025400   03  W-MINKEY-WDA3.                                                     
025500     05  W-MINKEY-IDTRANS          PIC X(4)    VALUE '4735'.              
025600     05  W-MINKEY-WDA3B1-ENTER.                                           
025800         07  W-MINKEYB1-IDRT       PIC X(3)    VALUE SPACE.               
025900         07  W-MINKEYB1-IDDC       PIC X(2)    VALUE SPACE.               
026000         07  W-MINKEYB1-IDRTLOP    PIC 9(3).                              
026100         07  W-MINKEYB1-IDKOLLI    PIC S9(5)   COMP-3 VALUE ZERO.         
026200         07  W-MINKEYB1-DAREGDAT   PIC  9(8)          VALUE ZERO.         
026300         07  W-MINKEYB1-TIKLOCK    PIC S9(9)   COMP-3 VALUE ZERO.         
026400     05  W-MINKEY-WDA3FSEQ-ENTER.                                         
026500         07  W-MINKEYF1-IDDC       PIC  X(2)          VALUE SPACE.        
026600         07  W-MINKEYF1-IDDISTR    PIC S9(5)   COMP-3 VALUE ZERO.         
026700         07  W-MINKEYF1-IDKUNDNR   PIC S9(7)   COMP-3 VALUE ZERO.         
026800         07  W-MINKEYF1-IDRAPPNR   PIC  9(7)          VALUE ZERO.         
026900     05  W-MINKEY-WDA3B1-NEXT.                                            
027100         07  W-MINKEYB1-IDRT-NEXT     PIC X(3)  VALUE SPACE.              
027110         07  W-MINKEYB1-IDDC-NEXT     PIC X(2)  VALUE SPACE.              
027200         07  W-MINKEYB1-IDRTLOP-NEXT  PIC 9(3).                           
027300         07  W-MINKEYB1-IDKOLLI-NEXT  PIC S9(5) COMP-3 VALUE ZERO.        
027400         07  W-MINKEYB1-DAREGDAT-NEXT PIC  9(8)        VALUE ZERO.        
027500         07  W-MINKEYB1-TIKLOCK-NEXT  PIC S9(9) COMP-3 VALUE ZERO.        
027600     05  W-MINKEY-WDA3FSEQ-NEXT.                                          
027700         07  W-MINKEYF1-IDDC-NEXT    PIC  X(2)        VALUE SPACE.        
027800         07  W-MINKEYF1-IDDISTR-NEXT  PIC S9(5) COMP-3 VALUE ZERO.        
027900         07  W-MINKEYF1-IDKUNDNR-NEXT PIC S9(7) COMP-3 VALUE ZERO.        
028000         07  W-MINKEYF1-IDRAPPNR-NEXT PIC  9(7)        VALUE ZERO.        
028100                                                                          
028200                                                                          
028300 01  NYCKLAR-TILL-DLI.                                                    
028400     03  W-IDLEVANM-X.                                                    
028500         05  W-IDDISTR           PIC S9(5)   COMP-3 VALUE ZERO.           
028600         05  W-IDKUNDNR          PIC S9(7)   COMP-3 VALUE ZERO.           
028700         05  W-IDRAPPNR          PIC  9(7)          VALUE ZERO.           
028800                                                                          
028900     03  W-WDA3B1-MIN-X.                                                  
029000         05  W-IDRT-MIN          PIC X(3)            VALUE SPACE.         
029100         05  W-IDDC-MIN          PIC X(2)            VALUE SPACE.         
029200         05  W-IDRTLOP-MIN       PIC 9(3)            VALUE ZERO.          
029300         05  W-IDKOLLI-MIN       PIC S9(5)   COMP-3  VALUE ZERO.          
029400         05  W-DAREGDAT-MIN      PIC  9(8)           VALUE ZERO.          
029500         05  W-TIKLOCK-MIN       PIC S9(9)   COMP-3  VALUE ZERO.          
029600                                                                          
029700     03  W-WDA3B1-MAX-X.                                                  
029800         05  W-IDRT-MAX          PIC X(3)            VALUE SPACE.         
029900         05  W-IDDC-MAX          PIC X(2)            VALUE SPACE.         
030000         05  W-IDRTLOP-MAX       PIC 9(3)            VALUE ZERO.          
030100         05  W-IDKOLLI-MAX       PIC S9(5)   COMP-3  VALUE ZERO.          
030200         05  W-DAREGDAT-MAX      PIC  9(8)           VALUE ZERO.          
030300         05  W-TIKLOCK-MAX       PIC S9(9)   COMP-3  VALUE ZERO.          
030400                                                                          
030500     03  W-WDA301KY-X.                                                    
030600         05  W-IDDC              PIC  X(2)           VALUE SPACE.         
030700         05  W-DAREGDAT          PIC  9(8)           VALUE ZERO.          
030800         05  W-TIKLOCK           PIC S9(9)   COMP-3  VALUE ZERO.          
030900     SKIP2                                                                
031000     03  W-WDA3BSEQ-X.                                                    
031100         05  W-IDRT-BSEQ         PIC  X(3)          VALUE SPACE.          
031200         05  W-IDDC-BSEQ         PIC  X(2)          VALUE SPACE.          
031300         05  W-IDRTLOP-BSEQ      PIC  9(3)          VALUE ZERO.           
031400         05  W-IDKOLLI-BSEQ      PIC S9(5)   COMP-3 VALUE ZERO.           
031500                                                                          
031600     03  W-WDA3FSEQ-MIN-X.                                                
031700         05  W-IDDC-FSEQ-MIN     PIC  X(2)          VALUE SPACE.          
031800         05  W-IDDISTR-FSEQ-MIN  PIC S9(5)   COMP-3 VALUE ZERO.           
031900         05  W-IDKUNDNR-FSEQ-MIN PIC S9(7)   COMP-3 VALUE ZERO.           
032000         05  W-IDRAPPNR-FSEQ-MIN PIC  9(7)   VALUE ZERO.                  
032100                                                                          
032200     03  W-WDA3FSEQ-MAX-X.                                                
032300         05  W-IDDC-FSEQ-MAX     PIC  X(2)          VALUE SPACE.          
032400         05  W-IDDISTR-FSEQ-MAX  PIC S9(5)   COMP-3 VALUE ZERO.           
032500         05  W-IDKUNDNR-FSEQ-MAX PIC S9(7)   COMP-3 VALUE ZERO.           
032600         05  W-IDRAPPNR-FSEQ-MAX PIC  9(7)   VALUE ZERO.                  
032700                                                                          
032800     03  W-WDA3F1KY-MIN-X.                                                
032900         05  W-IDDC-F1KY-MIN     PIC  X(2)          VALUE SPACE.          
033000         05  W-IDDISTR-F1KY-MIN  PIC S9(5)   COMP-3 VALUE ZERO.           
033100         05  W-IDKUNDNR-F1KY-MIN PIC S9(7)   COMP-3 VALUE ZERO.           
033200         05  W-IDRAPPNR-F1KY-MIN PIC  9(7)          VALUE ZERO.           
033300         05  W-IDRT-F1KY-MIN     PIC  X(3)          VALUE SPACE.          
033400         05  W-IDRTLOP-F1KY-MIN  PIC  9(3)          VALUE ZERO.           
033500         05  W-IDKOLLI-F1KY-MIN  PIC S9(5)   COMP-3 VALUE ZERO.           
033600         05  W-DAREGDAT-F1KY-MIN PIC  9(8)          VALUE ZERO.           
033700         05  W-TIKLOCK-F1KY-MIN  PIC S9(9)   COMP-3 VALUE ZERO.           
033800                                                                          
033900     03  W-WDA3F1KY-MAX-X.                                                
034000         05  W-IDDC-F1KY-MAX     PIC  X(2)          VALUE SPACE.          
034100         05  W-IDDISTR-F1KY-MAX  PIC S9(5)   COMP-3 VALUE ZERO.           
034200         05  W-IDKUNDNR-F1KY-MAX PIC S9(7)   COMP-3 VALUE ZERO.           
034300         05  W-IDRAPPNR-F1KY-MAX PIC  9(7)          VALUE ZERO.           
034400         05  W-IDRT-F1KY-MAX     PIC  X(3)          VALUE SPACE.          
034500         05  W-IDRTLOP-F1KY-MAX  PIC  9(3)          VALUE ZERO.           
034600         05  W-IDKOLLI-F1KY-MAX  PIC S9(5)   COMP-3 VALUE ZERO.           
034700         05  W-DAREGDAT-F1KY-MAX PIC  9(8)          VALUE ZERO.           
034800         05  W-TIKLOCK-F1KY-MAX  PIC S9(9)   COMP-3 VALUE ZERO.           
034900     EJECT                                                                
035000*    --- STATUS-KOD FRÅN IMS                                              
035100 01  STATUS-WS                   PIC XX.                                  
035200     88  STATUS-OK                           VALUE '  '.                  
035300     88  SEGMENT-FINNS                       VALUE '  '.                  
035400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
035500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
035600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
035700     88  TRANSKOD-FEL                        VALUE 'A1'.                  
035800     88  SECURITY-FEL                        VALUE 'A4'.                  
035900     SKIP2                                                                
036000 01  GODK-STATUSKODER.                                                    
036100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
036200     SKIP3                                                                
036300 01  SSA1                        PIC X(128).                              
036400 01  SSA2                        PIC X(128).                              
036500     EJECT                                                                
036600*    --- IMS FUNKTIONSKODER                                               
036700*01  -COPY W0003                                                          
036800     EJECT                                                                
036900*    ---  DLI INPUT-OUTPUT AREA                                           
037000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-A201'.         
037100                                                                          
037200 01  DLI-IO-A201.                                                         
037300*  03  -COPY WDA201                                                       
037400                                                                          
037500     SKIP3                                                                
037600                                                                          
037700 01  FILLER                      PIC X(16)   VALUE 'WDA211-AREA'.         
037800 01  WDA211-AREA.                                                         
037900     03 -COPY WDA211                                                      
038000                                                                          
038100     EJECT                                                                
038200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-A301'.         
038300                                                                          
038400 01  DLI-IO-A301.                                                         
038500*  03  -COPY WDA301                                                       
038600                                                                          
038700     EJECT                                                                
038800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-A3B1'.         
038900                                                                          
039000 01  DLI-IO-A3B1.                                                         
039100*  03  -COPY WDA3B1                                                       
039200                                                                          
039300     EJECT                                                                
039400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-A3F1'.         
039500                                                                          
039600 01  DLI-IO-A3F1.                                                         
039700*  03  -COPY WDA3F1                                                       
039800                                                                          
039900     EJECT                                                                
040000 LINKAGE SECTION.                                                         
040100                                                                          
040200*01  -COPY W0009   -PRE MSG-                                              
040300     EJECT                                                                
040400*01  -COPY W0009   -PRE ALT-                                              
040500     EJECT                                                                
040600*01  -COPY W0009   -PRE W4794-                                            
040700     EJECT                                                                
040800*01  -COPY W0009   -PRE ALT2-                                             
040900     EJECT                                                                
041000*01  -COPY W0008   -PRE USEA-                                             
041100     05  FILLER                  PIC X.                                   
041200     EJECT                                                                
041300*01  -COPY W0008  -PRE KREE-                                              
041400     05  FILLER                  PIC X.                                   
041500     EJECT                                                                
041600*01  -COPY W0008  -PRE RETA1-                                             
041700     05  FILLER                  PIC X.                                   
041800     EJECT                                                                
041900*01  -COPY W0008  -PRE RETC-                                              
042000     05  FILLER                  PIC X.                                   
042100     EJECT                                                                
042200*01  -COPY W0008  -PRE RETA2-                                             
042300     05  FILLER                  PIC X.                                   
042400     EJECT                                                                
042500*01  -COPY W0008  -PRE RETG-                                              
042600     05  FILLER                  PIC X.                                   
042700     EJECT                                                                
042800*01  -COPY W0008  -PRE RETA3-                                             
042900     05  FILLER                  PIC X.                                   
043000     EJECT                                                                
043100*01  -COPY W0008  -PRE RETA4-                                             
043200     05  FILLER                  PIC X.                                   
043300                                                                          
043400     EJECT                                                                
043500 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB W4794-PCB ALT2-PCB             
043600                           USEA-PCB                                       
043700                           KREE-PCB RETA1-PCB RETC-PCB RETA2-PCB          
043800                           RETG-PCB RETA3-PCB RETA4-PCB.                  
043900 MAIN SECTION.                                                            
044000     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB W4794-PCB ALT2-PCB             
044100                           USEA-PCB                                       
044200                           KREE-PCB RETA1-PCB RETC-PCB RETA2-PCB          
044300                           RETG-PCB RETA3-PCB RETA4-PCB.                  
044400                                                                          
044500                                                                          
044600     PERFORM IMS-GET-MSG                                                  
044700     IF SEGMENT-FINNS                                                     
044800       PERFORM A-INIT                                                     
044900       PERFORM B-KOLLA-NYCKLAR                                            
045000       IF NYCKLAR-OK                                                      
045100         IF MFS-PRINT OR MFS-UPDATE                                       
045200           PERFORM G-KOLLA-INPUT                                          
045300           IF INDATA-OK                                                   
045400             PERFORM H-UPPDATERA-SKRIV-UT                                 
045500           END-IF                                                         
045600         ELSE                                                             
045700           IF MFS-FIRST                                                   
045800             PERFORM C-FOERSTA-SIDA                                       
045900           ELSE                                                           
046000             IF MFS-NEXT                                                  
046100               PERFORM D-NAESTA-SIDA                                      
046200             ELSE                                                         
046300               PERFORM E-SAMMA-SIDA                                       
046400             END-IF                                                       
046500           END-IF                                                         
046600         END-IF                                                           
046700                                                                          
046800         IF STARTA-ANNAN-BILD OR MFS-PRINT OR MFS-UPDATE                  
046900           CONTINUE                                                       
047000         ELSE                                                             
047100           IF INDATA-OK                                                   
047200              PERFORM F-LAES-VISA-INFO                                    
047300           END-IF                                                         
047400         END-IF                                                           
047500                                                                          
047600       END-IF                                                             
047700     END-IF                                                               
047800     IF STARTA-ANNAN-BILD                                                 
047900         CONTINUE                                                         
048000     ELSE                                                                 
048100         COMPUTE MSG-KVLL = LENGTH OF MOD-W4O73501 + 4                    
048200         PERFORM IMS-INSERT-MSG                                           
048300     END-IF                                                               
048400                                                                          
048500     MOVE ZERO TO RETURN-CODE                                             
048600     GOBACK                                                               
048700     .                                                                    
048800     EJECT                                                                
048900 A-INIT SECTION.                                                          
049000                                                                          
049100     IF MSG-DUBBLA-TRANSKODER                                             
049200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I73501                 
049300       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
049400       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
049500     ELSE                                                                 
049600       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I73501                  
049700       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
049800       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
049900     END-IF                                                               
050000                                                                          
050100     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
050200     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
050300     MOVE MFS-IDTRANS TO W-IDTRANS                                        
050400                                                                          
050500     MOVE LOW-VALUE TO MSG-AREA                                           
050600     MOVE 'W4O73501' TO MFS-IDMOD                                         
050700     MOVE '4735' TO MOD-IDTRANS                                           
050800     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
050900                                                                          
051000     MOVE SPACE                       TO MED-IDMFSINF                     
051100                                                                          
051200     IF EGEN-MID OR HELP-MID                                              
051300       CONTINUE                                                           
051400     ELSE                                                                 
051500       MOVE SPACE TO MFS-KDTRTYP                                          
051600       MOVE '7' TO MFS-IDPFK                                              
051700     END-IF                                                               
051800                                                                          
051900     MOVE SPACE TO MOD4794-MID-W4I79401                                   
052000     MOVE LOW-VALUE TO  W-WDA3B1-MIN-X                                    
052100                        W-WDA3F1KY-MIN-X                                  
052200     MOVE HIGH-VALUE TO W-WDA3B1-MAX-X                                    
052300                        W-WDA3F1KY-MAX-X                                  
052400     .                                                                    
052500     EJECT                                                                
052600 B-KOLLA-NYCKLAR SECTION.                                                 
052700                                                                          
052800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
052900     MOVE '001'             TO MSGI-KDCALL                                
053000     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
053100     MOVE '4735'            TO MSGI-IDTRANS                               
053200     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
053300     IF EGEN-MID                                                          
053400       IF MID-IDSNDNNR-IN NOT = ALL '+'                                   
053500         MOVE MID-IDSNDNNR-IN     TO WS-IDSNDNNR                          
053600         MOVE WS-IDRT             TO MSGI-IDRT                            
053700         MOVE WS-IDRTLOP          TO MSGI-IDRTLOP                         
053800         IF MID-IDKOLLI-IN NOT = ALL '+'                                  
053900           MOVE MID-IDKOLLI-IN      TO MSGI-IDKOLLI                       
054000         ELSE                                                             
054100           MOVE ZERO                TO MSGI-IDKOLLI                       
054200         END-IF                                                           
054300         MOVE ZERO                  TO MSGI-IDDISTR                       
054400                                       MSGI-IDKUNDNR                      
054500                                       MSGI-IDRAPPNR                      
054600       ELSE                                                               
054700         IF MID-IDKOLLI-IN NOT = ALL '+'                                  
054800           MOVE MID-IDKOLLI-IN      TO MSGI-IDKOLLI                       
054900           MOVE ZERO                TO MSGI-IDDISTR                       
055000                                       MSGI-IDKUNDNR                      
055100                                       MSGI-IDRAPPNR                      
055200         ELSE                                                             
055300           IF MID-IDDISTR-IN NOT = ALL '+'                                
055400             MOVE MID-IDDISTR-IN      TO MSGI-IDDISTR                     
055500             IF MID-IDKUNDNR-IN NOT = ALL '+'                             
055600               MOVE MID-IDKUNDNR-IN   TO MSGI-IDKUNDNR                    
055700             ELSE                                                         
055800               MOVE ZERO              TO MSGI-IDKUNDNR                    
055900             END-IF                                                       
056000             IF MID-IDRAPPNR-IN NOT = ALL '+'                             
056100               MOVE MID-IDRAPPNR-IN   TO MSGI-IDRAPPNR                    
056200             ELSE                                                         
056300               MOVE ZERO              TO MSGI-IDRAPPNR                    
056400             END-IF                                                       
056500             MOVE SPACE               TO MSGI-IDRT                        
056600             MOVE ZERO                TO MSGI-IDRTLOP                     
056700                                         MSGI-IDKOLLI                     
056800           ELSE                                                           
056900             IF MID-IDKUNDNR-IN NOT = ALL '+'                             
057000               MOVE MID-IDKUNDNR-IN     TO MSGI-IDKUNDNR                  
057100               IF MID-IDRAPPNR-IN NOT = ALL '+'                           
057200                 MOVE MID-IDRAPPNR-IN   TO MSGI-IDRAPPNR                  
057300               ELSE                                                       
057400                 MOVE ZERO              TO MSGI-IDRAPPNR                  
057500               END-IF                                                     
057600               MOVE SPACE               TO MSGI-IDRT                      
057700               MOVE ZERO                TO MSGI-IDRTLOP                   
057800                                           MSGI-IDKOLLI                   
057900             ELSE                                                         
058000               IF MID-IDRAPPNR-IN NOT = ALL '+'                           
058100                 MOVE MID-IDRAPPNR-IN   TO MSGI-IDRAPPNR                  
058200                 MOVE SPACE               TO MSGI-IDRT                    
058300                 MOVE ZERO                TO MSGI-IDRTLOP                 
058400                                             MSGI-IDKOLLI                 
058500               END-IF                                                     
058600             END-IF                                                       
058700           END-IF                                                         
058800         END-IF                                                           
058900       END-IF                                                             
059000     ELSE                                                                 
059100       MOVE ZERO                          TO MSGI-IDDISTR                 
059200                                             MSGI-IDKUNDNR                
059300                                             MSGI-IDRAPPNR                
059400     END-IF                                                               
059500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
059600     MOVE MSGI-SPAR-AREA TO  W-MINKEY-WDA3                                
059700                                                                          
059800     IF MSGI-IDLAND-SPR = 'GB'                                            
059900       MOVE 'GB'                  TO MED-IDSKYLT                          
060000     ELSE                                                                 
060100       MOVE 'S '                  TO MED-IDSKYLT                          
060200     END-IF                                                               
060300                                                                          
060400     MOVE JA TO NYCKLAR-SW                                                
060500     MOVE MSGI-IDDC                       TO W-IDDC                       
060600                                             W-IDDC-MIN                   
060700                                             W-IDDC-MAX                   
060800                                             W-IDDC-BSEQ                  
060900                                             W-IDDC-FSEQ-MIN              
061000                                             W-IDDC-FSEQ-MAX              
061100                                             W-IDDC-F1KY-MIN              
061200                                             W-IDDC-F1KY-MAX              
061300                                             WS-IDDC                      
061400     PERFORM BA-KOLLA-ISNDNNR-IDKOLLI                                     
061500     PERFORM BB-KOLLA-IDDISTR                                             
061600     PERFORM BC-KOLLA-IDKUNDNR                                            
061700     PERFORM BD-KOLLA-IDRAPPNR                                            
061800                                                                          
061900                                                                          
062000     IF GODK-MID OR NYCKLAR-OK                                            
062100        MOVE MSGI-IDRT             TO WS-IDRT                             
062200        MOVE MSGI-IDRTLOP          TO WS-IDRTLOP                          
062300        IF WS-IDRTLOP = ZERO                                              
062400          MOVE MFS-RENSA-FAELT       TO MOD-IDSNDNNR-UT                   
062500        ELSE                                                              
062600          MOVE WS-IDSNDNNR           TO MOD-IDSNDNNR-UT                   
062700        END-IF                                                            
062800        MOVE MSGI-IDKOLLI          TO MOD-IDKOLLI-UT                      
062900        INSPECT MOD-IDKOLLI-UT REPLACING LEADING ZERO BY SPACE            
063000        MOVE MSGI-IDDISTR          TO MOD-IDDISTR-UT                      
063100        INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE            
063200        MOVE MSGI-IDKUNDNR         TO MOD-IDKUNDNR-UT                     
063300        INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE           
063400        MOVE MSGI-IDRAPPNR         TO MOD-IDRAPPNR-UT                     
063500        INSPECT MOD-IDRAPPNR-UT REPLACING LEADING ZERO BY SPACE           
063600     ELSE                                                                 
063700        MOVE MFS-RENSA-FAELT       TO MOD-IDSNDNNR-UT                     
063800                                      MOD-IDKOLLI-UT                      
063900                                      MOD-IDDISTR-UT                      
064000                                      MOD-IDKUNDNR-UT                     
064100                                      MOD-IDRAPPNR-UT                     
064200     END-IF                                                               
064300                                                                          
064400     IF NYCKLAR-FEL                                                       
064500       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
064600       CALL WMEDKONV USING MED-WMEDAREA                                   
064700       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
064800       PERFORM MFS-RENSA-FAELT-IN                                         
064900       PERFORM MFS-RENSA-FAELT-UT                                         
065000     END-IF                                                               
065100     .                                                                    
065200     EJECT                                                                
065300 BA-KOLLA-ISNDNNR-IDKOLLI SECTION.                                        
065400     SKIP2                                                                
065500     MOVE MFS-RENSA-FAELT TO MOD-IDSNDNNR-IN                              
065600     MOVE MFS-RENSA-FAELT TO MOD-IDKOLLI-IN                               
065700                                                                          
065800     IF MID-IDSNDNNR-IN NOT = ALL '+' OR                                  
065900        MID-IDKOLLI-IN NOT = ALL '+'                                      
066000         MOVE '7'         TO MFS-IDPFK                                    
066100         MOVE SPACE       TO MFS-KDTRTYP                                  
066200     END-IF                                                               
066300                                                                          
066400     IF MSGI-IDRT       NOT = SPACE                                       
066500         MOVE MSGI-IDRT   TO W-IDRT-MIN                                   
066600                             W-IDRT-MAX                                   
066700     ELSE                                                                 
066800         MOVE NEJ TO IDSNDNNR-SW                                          
066900     END-IF                                                               
067000                                                                          
067100     IF MSGI-IDRTLOP  NUMERIC                                             
067200     AND MSGI-IDRTLOP > 0                                                 
067300         MOVE MSGI-IDRTLOP  TO W-IDRTLOP-MIN                              
067400                               W-IDRTLOP-MAX                              
067500     ELSE                                                                 
067600         MOVE NEJ TO IDSNDNNR-SW                                          
067700     END-IF                                                               
067800     MOVE MSGI-IDRT         TO WS-IDRT                                    
067900     MOVE MSGI-IDRTLOP      TO WS-IDRTLOP                                 
068000     MOVE WS-IDSNDNNR       TO MOD-IDSNDNNR-UT                            
068100                                                                          
068200     IF MSGI-IDKOLLI NUMERIC AND                                          
068300         MSGI-IDKOLLI > ZERO                                              
068400         MOVE MSGI-IDKOLLI  TO W-IDKOLLI-MIN                              
068500                               W-IDKOLLI-MAX                              
068600     ELSE                                                                 
068700         MOVE ZERO          TO W-IDKOLLI-MIN                              
068800         MOVE 99999         TO W-IDKOLLI-MAX                              
068900         MOVE NEJ TO IDKOLLI-SW                                           
069000     END-IF                                                               
069100     .                                                                    
069200     EJECT                                                                
069300 BB-KOLLA-IDDISTR SECTION.                                                
069400     SKIP2                                                                
069500     MOVE MFS-RENSA-FAELT     TO MOD-IDDISTR-IN                           
069600     IF MID-IDDISTR-IN = ALL '+'                                          
069700         MOVE NEJ TO IDDISTR-SW                                           
069800     ELSE                                                                 
069900         MOVE '7'     TO MFS-IDPFK                                        
070000         MOVE SPACE   TO MFS-KDTRTYP                                      
070100     END-IF                                                               
070200                                                                          
070300     IF MSGI-IDDISTR NUMERIC                                              
070400     AND MSGI-IDDISTR > 0                                                 
070500         MOVE MSGI-IDDISTR   TO W-IDDISTR-FSEQ-MIN                        
070600                                W-IDDISTR-FSEQ-MAX                        
070700     ELSE                                                                 
070800         MOVE NEJ TO IDDISTR-SW                                           
070900     END-IF                                                               
071000                                                                          
071100     .                                                                    
071200     EJECT                                                                
071300 BC-KOLLA-IDKUNDNR SECTION.                                               
071400     SKIP2                                                                
071500     MOVE MFS-RENSA-FAELT     TO MOD-IDKUNDNR-IN                          
071600     IF MID-IDKUNDNR-IN = ALL '+'                                         
071700         MOVE NEJ TO IDKUNDNR-SW                                          
071800     ELSE                                                                 
071900         MOVE '7'     TO MFS-IDPFK                                        
072000         MOVE SPACE   TO MFS-KDTRTYP                                      
072100     END-IF                                                               
072200                                                                          
072300     IF MSGI-IDKUNDNR NUMERIC                                             
072400     AND MSGI-IDKUNDNR > 0                                                
072500         MOVE MSGI-IDKUNDNR  TO W-IDKUNDNR-FSEQ-MIN                       
072600                                W-IDKUNDNR-FSEQ-MAX                       
072700     ELSE                                                                 
072800         MOVE NEJ TO IDKUNDNR-SW                                          
072900     END-IF                                                               
073000     .                                                                    
073100     EJECT                                                                
073200 BD-KOLLA-IDRAPPNR SECTION.                                               
073300     SKIP2                                                                
073400     MOVE MFS-RENSA-FAELT     TO MOD-IDRAPPNR-IN                          
073500     IF MID-IDRAPPNR-IN = ALL '+'                                         
073600         MOVE NEJ TO IDRAPPNR-SW                                          
073700     ELSE                                                                 
073800         MOVE '7'     TO MFS-IDPFK                                        
073900         MOVE SPACE   TO MFS-KDTRTYP                                      
074000     END-IF                                                               
074100                                                                          
074200     IF MSGI-IDRAPPNR NUMERIC                                             
074300     AND MSGI-IDRAPPNR > 0                                                
074400         MOVE MSGI-IDRAPPNR  TO W-IDRAPPNR-FSEQ-MIN                       
074500                                W-IDRAPPNR-FSEQ-MAX                       
074600     ELSE                                                                 
074700         MOVE NEJ TO IDRAPPNR-SW                                          
074800     END-IF                                                               
074900     .                                                                    
075000     EJECT                                                                
075100 C-FOERSTA-SIDA SECTION.                                                  
075200                                                                          
075300     MOVE INF-FIRST-PAGE       TO MED-IDMFSINF                            
075400     CALL WMEDKONV   USING MED-WMEDAREA                                   
075500     MOVE MED-MFSINF  TO MOD-TEMFSFEL                                     
075600                                                                          
075700     PERFORM MFS-RENSA-FAELT-IN                                           
075800     .                                                                    
075900     EJECT                                                                
076000 D-NAESTA-SIDA SECTION.                                                   
076100     SKIP2                                                                
076200     IF W-MINKEY-IDTRANS = '4735'                                         
076300       IF W-MINKEYB1-IDRT = SPACE AND                                     
076400          W-MINKEYF1-IDDISTR = ZERO                                       
076500         CONTINUE                                                         
076600       ELSE                                                               
076700         MOVE W-MINKEY-WDA3B1-NEXT   TO W-WDA3B1-MIN-X                    
076800         MOVE W-MINKEY-WDA3FSEQ-NEXT TO W-WDA3FSEQ-MIN-X                  
076900       END-IF                                                             
077000        PERFORM MFS-RENSA-FAELT-IN                                        
077100     ELSE                                                                 
077200        PERFORM MFS-RENSA-FAELT-IN                                        
077300     END-IF                                                               
077400     .                                                                    
077500     EJECT                                                                
077600 E-SAMMA-SIDA SECTION.                                                    
077700                                                                          
077800     IF W-MINKEY-IDTRANS = '4735'                                         
077900       MOVE W-MINKEY-WDA3B1-ENTER    TO W-WDA3B1-MIN-X                    
078000       MOVE W-MINKEY-WDA3FSEQ-ENTER  TO W-WDA3FSEQ-MIN-X                  
078100                                                                          
078200       IF MID-INPUT = ALL '+'                                             
078300         PERFORM MFS-RENSA-FAELT-IN                                       
078400       ELSE                                                               
078500         MOVE +1 TO RAD-IX                                                
078600         PERFORM UNTIL RAD-IX > MAX-IX                                    
078700           IF MID-KDCMD (RAD-IX) NUMERIC                                  
078800             PERFORM EA-STARTA-ANNAN-BILD                                 
078900             MOVE JA TO SW-STARTA-ANNAN-BILD                              
079000             MOVE MAX-IX TO RAD-IX                                        
079100           END-IF                                                         
079200           ADD +1 TO RAD-IX                                               
079300         END-PERFORM                                                      
079400                                                                          
079500         IF STARTA-ANNAN-BILD                                             
079600           CONTINUE                                                       
079700         ELSE                                                             
079800           MOVE +1                        TO RAD-IX                       
079900           PERFORM UNTIL RAD-IX         >  MAX-IX                         
080000             IF MID-KDCMD(RAD-IX) NOT = ALL '+'                           
080100               IF MID-KDCMD(RAD-IX) = W-UTSKRIFT OR W-PRINT               
080200                                      OR W-ETIKETT                        
080300                 MOVE INF-PRESS-PF4         TO MED-IDMFSINF               
080400                 CALL WMEDKONV USING MED-WMEDAREA                         
080500                 MOVE MED-TEMFSINF           TO MOD-TEMFSFEL              
080600                 MOVE MAX-IX TO RAD-IX                                    
080700                                                                          
080800               ELSE                                                       
080900                 IF MID-KDCMD(RAD-IX) = W-BORTTAG OR W-DELETE OR          
081000                                        W-TEXT                            
081100                   MOVE INF-PRESS-PF11       TO MED-IDMFSINF              
081200                   CALL WMEDKONV USING MED-WMEDAREA                       
081300                   MOVE MED-TEMFSINF         TO MOD-TEMFSFEL              
081400                   MOVE MAX-IX TO RAD-IX                                  
081500                                                                          
081600                 ELSE                                                     
081700                   MOVE ERR-WRONG-COMMAND-CODE TO MED-IDMFSFEL            
081800                   CALL WMEDKONV USING MED-WMEDAREA                       
081900                   MOVE MED-MFSFEL TO MOD-TEMFSFEL                        
082000                   MOVE MAX-IX TO RAD-IX                                  
082100                 END-IF                                                   
082200               END-IF                                                     
082300             END-IF                                                       
082400             ADD +1 TO RAD-IX                                             
082500           END-PERFORM                                                    
082600                                                                          
082700           IF MID-IDPRT NOT = ALL '+'                                     
082800             MOVE INF-PRESS-PF4         TO MED-IDMFSINF                   
082900             CALL WMEDKONV USING MED-WMEDAREA                             
083000             MOVE MED-TEMFSINF           TO MOD-TEMFSFEL                  
083100           END-IF                                                         
083200                                                                          
083300           PERFORM EB-MID-INDATA-TILL-MOD                                 
083400         END-IF                                                           
083500       END-IF                                                             
083600     END-IF                                                               
083700     .                                                                    
083800     EJECT                                                                
083900 EA-STARTA-ANNAN-BILD SECTION.                                            
084000     SKIP2                                                                
084100     MOVE MID-IDKOLLI(RAD-IX)    TO MSGI-IDKOLLI                          
084200     INSPECT MSGI-IDKOLLI  REPLACING LEADING SPACE BY ZERO                
084300     MOVE MID-IDDISTR(RAD-IX)    TO MSGI-IDDISTR                          
084400     INSPECT MSGI-IDDISTR  REPLACING LEADING SPACE BY ZERO                
084500     MOVE MID-IDKUNDNR(RAD-IX)   TO MSGI-IDKUNDNR                         
084600     INSPECT MSGI-IDKUNDNR REPLACING LEADING SPACE BY ZERO                
084700     MOVE MID-IDRAPPNR(RAD-IX)   TO MSGI-IDRAPPNR                         
084800     INSPECT MSGI-IDRAPPNR REPLACING LEADING SPACE BY ZERO                
084900     MOVE '001'                  TO MSGI-KDCALL                           
085000     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
085100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
085200                                                                          
085300     MOVE LOW-VALUE                 TO P-TO-P-KDZ1                        
085400     MOVE LOW-VALUE                 TO P-TO-P-KDZ2                        
085500     MOVE MID-KDCMD(RAD-IX) (1:1)   TO W-HOPP-IDTRANS-2                   
085600     MOVE MID-KDCMD(RAD-IX) (2:3)   TO W-HOPP-IDTRANS-4-6                 
085700     MOVE W-HOPP-IDTRANS            TO P-TO-P-KDTRANS                     
085800     MOVE '4735'                    TO P-TO-P-IDTRANS                     
085900     MOVE MFS-KDMFSFOR              TO P-TO-P-KDMFSFOR                    
086000                                                                          
086100     PERFORM S01-INSERT-ALTMSG                                            
086200     .                                                                    
086300     EJECT                                                                
086400 EB-MID-INDATA-TILL-MOD SECTION.                                          
086500     SKIP2                                                                
086600                                                                          
086700     IF MID-IDPRT = ALL '+'                                               
086800       MOVE MFS-RENSA-FAELT TO MOD-IDPRT-UPD                              
086900     ELSE                                                                 
087000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPRT-ATTR                       
087100       MOVE MFS-ROER-EJ-FAELT TO MOD-IDPRT-UPD                            
087200     END-IF                                                               
087300                                                                          
087400     IF MID-IDANSTNR = ALL '+'                                            
087500       MOVE MFS-RENSA-FAELT TO MOD-IDANSTNR                               
087600     ELSE                                                                 
087700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDANSTNR-ATTR                    
087800       MOVE MFS-ROER-EJ-FAELT TO MOD-IDANSTNR                             
087900     END-IF                                                               
088000                                                                          
088100     MOVE +1 TO RAD-IX                                                    
088200     PERFORM UNTIL RAD-IX > MAX-IX                                        
088300         IF MID-KDCMD (RAD-IX) NOT = ALL '+'                              
088400            MOVE MID-KDCMD (RAD-IX)    TO MOD-KDCMD (RAD-IX)              
088500            MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-ATTR (RAD-IX)         
088600         ELSE                                                             
088700             MOVE MFS-RENSA-FAELT       TO MOD-KDCMD (RAD-IX)             
088800         END-IF                                                           
088900                                                                          
089000         IF MID-TERETNOT (RAD-IX) NOT = ALL '+'                           
089100            MOVE MID-TERETNOT (RAD-IX)   TO MOD-TERETNOT (RAD-IX)         
089200            MOVE MFS-ADD-LAES-IN-FAELT   TO                               
089300                                       MOD-TERETNOT-ATTR (RAD-IX)         
089400         END-IF                                                           
089500                                                                          
089600         ADD +1 TO RAD-IX                                                 
089700     END-PERFORM                                                          
089800     .                                                                    
089900     EJECT                                                                
090000 F-LAES-VISA-INFO SECTION.                                                
090100                                                                          
090200     IF IDSNDNNR-FINNS                                                    
090300         PERFORM FA-VISA-IDSNDNNR-IDKOLLI                                 
090400     ELSE                                                                 
090500         PERFORM FB-VISA-DISTR-KUND-RAPPNR                                
090600     END-IF                                                               
090700     .                                                                    
090800     EJECT                                                                
090900 FA-VISA-IDSNDNNR-IDKOLLI SECTION.                                        
091000     SKIP2                                                                
091100     PERFORM IMS-GU-WLRETC01                                              
091200     IF SEGMENT-FINNS                                                     
091300         PERFORM FAA-VISA-IDKOLLI-LEVANM                                  
091400     ELSE                                                                 
091500         MOVE ERR-URVAL-SAKNAS TO MED-IDMFSFEL                            
091600         CALL WMEDKONV USING MED-WMEDAREA                                 
091700         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
091800         PERFORM MFS-RENSA-FAELT-UT                                       
091900     END-IF                                                               
092000     .                                                                    
092100     EJECT                                                                
092200 FAA-VISA-IDKOLLI-LEVANM SECTION.                                         
092300     SKIP2                                                                
092400***  WDA2 LÄSES BARA VID VARJE NY FÖREKOMST AV DISTRIKT/KUND/             
092500***    RAPPORTNUMMER (FÖR ATT MINSKA ANTALET LÄSNINGAR)                   
092600***                                                                       
092700     MOVE SEQB-DAREGDAT TO W-DAREGDAT                                     
092800     MOVE SEQB-TIKLOCK  TO W-TIKLOCK                                      
092900     PERFORM IMS-GU-WLRETA01                                              
093000     IF SEGMENT-FINNS AND RET-IDDISTR > ZERO                              
093100       MOVE RET-IDDISTR  TO W-IDDISTR                                     
093200                            SPARA-IDDISTR                                 
093300                            W-IDDISTR-F1KY-MIN                            
093400                            W-IDDISTR-F1KY-MAX                            
093500       MOVE RET-IDKUNDNR TO W-IDKUNDNR                                    
093600                            SPARA-IDKUNDNR                                
093700                            W-IDKUNDNR-F1KY-MIN                           
093800                            W-IDKUNDNR-F1KY-MAX                           
093900       MOVE RET-IDRAPPNR TO W-IDRAPPNR                                    
094000                            SPARA-IDRAPPNR                                
094100                            W-IDRAPPNR-F1KY-MIN                           
094200                            W-IDRAPPNR-F1KY-MAX                           
094300       PERFORM IMS-GU-WLKREE01                                            
094400                                                                          
094500       IF SEGMENT-FINNS                                                   
094600         PERFORM S03-FIXA-ENTER-KEY                                       
094700                                                                          
094800         MOVE +1 TO RAD-IX                                                
094900         PERFORM UNTIL RAD-IX > MAX-IX                                    
095000           IF SEGMENT-FINNS                                               
095100             PERFORM FAAC-REDIGERA-MOD                                    
095200             PERFORM S02-KOLLA-OM-FLERA-KOLLIN                            
095300             IF ANTAL-KOLLIN > +1                                         
095400                 MOVE ANTAL-KOLLIN TO MOD-KVKOLLI-DEL (RAD-IX)            
095500             END-IF                                                       
095600             PERFORM IMS-GN-WLRETC01                                      
095700             IF SEGMENT-FINNS                                             
095800                 MOVE SEQB-DAREGDAT TO W-DAREGDAT                         
095900                 MOVE SEQB-TIKLOCK  TO W-TIKLOCK                          
096000                 PERFORM IMS-GU-WLRETA01                                  
096100                 IF SEGMENT-FINNS                                         
096200                     IF RET-IDDISTR = SPARA-IDDISTR                       
096300                     AND RET-IDKUNDNR = SPARA-IDKUNDNR                    
096400                     AND RET-IDRAPPNR = SPARA-IDRAPPNR                    
096500                         CONTINUE                                         
096600                     ELSE                                                 
096700                         MOVE RET-IDDISTR  TO W-IDDISTR                   
096800                                              SPARA-IDDISTR               
096900                                              W-IDDISTR-F1KY-MIN          
097000                                              W-IDDISTR-F1KY-MAX          
097100                         MOVE RET-IDKUNDNR TO W-IDKUNDNR                  
097200                                              SPARA-IDKUNDNR              
097300                                              W-IDKUNDNR-F1KY-MIN         
097400                                              W-IDKUNDNR-F1KY-MAX         
097500                         MOVE RET-IDRAPPNR TO W-IDRAPPNR                  
097600                                              SPARA-IDRAPPNR              
097700                                              W-IDRAPPNR-F1KY-MIN         
097800                                              W-IDRAPPNR-F1KY-MAX         
097900                         PERFORM IMS-GU-WLKREE01                          
098000                     END-IF                                               
098100                 END-IF                                                   
098200             END-IF                                                       
098300           ELSE                                                           
098400             PERFORM FAAD-RENSA-RADER                                     
098500             MOVE MFS-STAENG-FAELT-NOMOD  TO                              
098600                                      MOD-TERETNOT-ATTR (RAD-IX)          
098700                                      MOD-KDCMD-ATTR (RAD-IX)             
098800           END-IF                                                         
098900           ADD +1 TO RAD-IX                                               
099000         END-PERFORM                                                      
099100                                                                          
099200         PERFORM S04-FIXA-NEXT-KEY                                        
099300                                                                          
099400       ELSE                                                               
099500         MOVE ERR-URVAL-SAKNAS TO MED-IDMFSFEL                            
099600         CALL WMEDKONV USING MED-WMEDAREA                                 
099700         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
099800         PERFORM MFS-RENSA-FAELT-UT                                       
099900       END-IF                                                             
100000     ELSE                                                                 
100100       MOVE ERR-URVAL-SAKNAS TO MED-IDMFSFEL                              
100200       CALL WMEDKONV USING MED-WMEDAREA                                   
100300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
100400       PERFORM MFS-RENSA-FAELT-UT                                         
100500     END-IF                                                               
100600     .                                                                    
100700     EJECT                                                                
100800                                                                          
100900 FAAC-REDIGERA-MOD SECTION.                                               
101000     SKIP2                                                                
101100     MOVE RET-IDKOLLI         TO MOD-IDKOLLI (RAD-IX)                     
101200     MOVE RET-IDDISTR         TO MOD-IDDISTR (RAD-IX)                     
101300     MOVE RET-IDKUNDNR        TO MOD-IDKUNDNR(RAD-IX)                     
101400     MOVE RET-IDRAPPNR        TO MOD-IDRAPPNR (RAD-IX)                    
101500     MOVE RET-KVKOLLI-AAF     TO MOD-KVKOLLI (RAD-IX)                     
101600     IF MSGI-IDLAND-SPR = 'GB '                                           
101700       IF RET-FLFARLIG = JA                                               
101800         MOVE YES               TO MOD-FLFARLIG (RAD-IX)                  
101900       ELSE                                                               
102000         MOVE NEJ               TO MOD-FLFARLIG (RAD-IX)                  
102100       END-IF                                                             
102200     ELSE                                                                 
102300       MOVE RET-FLFARLIG        TO MOD-FLFARLIG (RAD-IX)                  
102400     END-IF                                                               
102500     MOVE RET-IDFRASED-AAF    TO MOD-IDFRASED-AAF (RAD-IX)                
102600                                                                          
102700*TEST FÖR ATT EJ RENSA IFYLLD RAD VID ENTER IST. FÖR PF11                 
102800     IF MFS-ENTER                                                         
102900       IF MID-TERETNOT (RAD-IX) = ALL '+'                                 
103000         MOVE RET-TERETNOT    TO MOD-TERETNOT (RAD-IX)                    
103100       END-IF                                                             
103200     ELSE                                                                 
103300       MOVE RET-TERETNOT      TO MOD-TERETNOT (RAD-IX)                    
103400     END-IF                                                               
103500                                                                          
103600     MOVE ANM-KDLEVANM        TO MOD-KDLEVANM (RAD-IX)                    
103700     IF ANM-KDLEVANM > 5                                                  
103800       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-KDTRSTAT-ATTR(RAD-IX)          
103900     END-IF                                                               
104000     .                                                                    
104100     EJECT                                                                
104200 FAAD-RENSA-RADER SECTION.                                                
104300     SKIP2                                                                
104400     MOVE MFS-RENSA-FAELT     TO MOD-IDKOLLI (RAD-IX)                     
104500                                 MOD-IDDISTR (RAD-IX)                     
104600                                 MOD-IDRAPPNR (RAD-IX)                    
104700                                 MOD-KVKOLLI (RAD-IX)                     
104800                                 MOD-FLFARLIG (RAD-IX)                    
104900                                 MOD-IDFRASED-AAF (RAD-IX)                
105000                                 MOD-TERETNOT (RAD-IX)                    
105100                                 MOD-KDLEVANM (RAD-IX)                    
105200     .                                                                    
105300     EJECT                                                                
105400 FB-VISA-DISTR-KUND-RAPPNR SECTION.                                       
105500     SKIP2                                                                
105600     PERFORM IMS-GU-SEQF-WLRETA01                                         
105700     IF SEGMENT-FINNS AND RET-IDDISTR > ZERO AND                          
105800        W-IDDISTR-FSEQ-MIN  > ZERO                                        
105900***** DISTR > ZERO TAR HAND OM SÄNDNINGAR UTAN RAPPORTER                  
106000         PERFORM S03-FIXA-ENTER-KEY                                       
106100         PERFORM FBB-VISA-DIST-KUND-RAPP                                  
106200         PERFORM S04-FIXA-NEXT-KEY                                        
106300     ELSE                                                                 
106400         MOVE ERR-URVAL-SAKNAS TO MED-IDMFSFEL                            
106500         CALL WMEDKONV USING MED-WMEDAREA                                 
106600         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
106700         PERFORM MFS-RENSA-FAELT-UT                                       
106800     END-IF                                                               
106900     .                                                                    
107000     EJECT                                                                
107100 FBB-VISA-DIST-KUND-RAPP SECTION.                                         
107200     SKIP2                                                                
107300     MOVE RET-IDRETSND        TO MOD-IDSNDNNR-UT                          
107400     MOVE RET-IDDISTR         TO W-IDDISTR                                
107500     MOVE RET-IDKUNDNR        TO W-IDKUNDNR                               
107600     MOVE RET-IDRAPPNR        TO W-IDRAPPNR                               
107700     PERFORM IMS-GU-WLKREE01                                              
107800                                                                          
107900     MOVE +1 TO RAD-IX                                                    
108000     PERFORM UNTIL RAD-IX > MAX-IX                                        
108100       IF SEGMENT-FINNS                                                   
108200                                                                          
108300         MOVE RET-IDKOLLI         TO MOD-IDKOLLI (RAD-IX)                 
108400         MOVE RET-IDDISTR         TO MOD-IDDISTR (RAD-IX)                 
108500         MOVE RET-IDKUNDNR        TO MOD-IDKUNDNR(RAD-IX)                 
108600         MOVE RET-IDRAPPNR        TO MOD-IDRAPPNR (RAD-IX)                
108700         MOVE RET-KVKOLLI-AAF     TO MOD-KVKOLLI (RAD-IX)                 
108800         IF MSGI-IDLAND-SPR = 'GB '                                       
108900           IF RET-FLFARLIG = JA                                           
109000             MOVE YES             TO MOD-FLFARLIG (RAD-IX)                
109100           ELSE                                                           
109200             MOVE NEJ             TO MOD-FLFARLIG (RAD-IX)                
109300           END-IF                                                         
109400         ELSE                                                             
109500           MOVE RET-FLFARLIG      TO MOD-FLFARLIG (RAD-IX)                
109600         END-IF                                                           
109700         MOVE RET-IDFRASED-AAF    TO MOD-IDFRASED-AAF (RAD-IX)            
109800                                                                          
109900         IF MFS-ENTER                                                     
110000           IF MID-TERETNOT (RAD-IX) = ALL '+'                             
110100             MOVE RET-TERETNOT    TO MOD-TERETNOT (RAD-IX)                
110200           END-IF                                                         
110300         ELSE                                                             
110400           MOVE RET-TERETNOT      TO MOD-TERETNOT (RAD-IX)                
110500         END-IF                                                           
110600                                                                          
110700         MOVE ANM-KDLEVANM        TO MOD-KDLEVANM (RAD-IX)                
110800         IF ANM-KDLEVANM > 5                                              
110900           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDTRSTAT-ATTR(RAD-IX)        
111000         END-IF                                                           
111100                                                                          
111200         PERFORM IMS-GN-SEQF-WLRETA01                                     
111300       ELSE                                                               
111400         MOVE MFS-RENSA-FAELT     TO MOD-IDKOLLI (RAD-IX)                 
111500                                     MOD-IDDISTR (RAD-IX)                 
111600                                     MOD-IDRAPPNR (RAD-IX)                
111700                                     MOD-KVKOLLI (RAD-IX)                 
111800                                     MOD-FLFARLIG (RAD-IX)                
111900                                     MOD-IDFRASED-AAF (RAD-IX)            
112000                                     MOD-TERETNOT (RAD-IX)                
112100                                     MOD-KDLEVANM (RAD-IX)                
112200         MOVE MFS-STAENG-FAELT-NOMOD  TO                                  
112300                                     MOD-KDCMD-ATTR (RAD-IX)              
112400                                     MOD-TERETNOT-ATTR (RAD-IX)           
112500       END-IF                                                             
112600       ADD +1 TO RAD-IX                                                   
112700     END-PERFORM                                                          
112800     .                                                                    
112900     EJECT                                                                
113000 G-KOLLA-INPUT SECTION.                                                   
113100                                                                          
113200     MOVE JA  TO INDATA-SW                                                
113300                 SW-IDANSTNR-RAETT-IFYLLD                                 
113400                 SW-IDPRT-RAETT-IFYLLD                                    
113500     MOVE NEJ TO SW-KDCMD                                                 
113600                 SW-KDCMD-BORTTAG                                         
113700                 SW-KDCMD-UTSKRIFT                                        
113800                 SW-KDCMD-ETIKETT                                         
113900                 SW-KDCMD-TEXT                                            
114000                 SW-KDCMD-RAETT-IFYLLD                                    
114100                                                                          
114200     MOVE ZERO                    TO MED-IDMFSFEL                         
114300                                                                          
114400     IF MID-INPUT                = ALL '+' AND                            
114500        MFS-UPDATE                                                        
114600       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
114700       CALL WMEDKONV USING MED-WMEDAREA                                   
114800       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
114900       PERFORM MFS-ROER-EJ-FAELT-IN                                       
115000       PERFORM MFS-ROER-EJ-FAELT-UT                                       
115100       MOVE NEJ                  TO INDATA-SW                             
115200     ELSE                                                                 
115300       PERFORM GA-FORMELL-KONTROLL                                        
115400       IF INDATA-OK                                                       
115500          IF KDCMD-ETIKETT                                                
115600             CONTINUE                                                     
115700          ELSE                                                            
115800             PERFORM GB-LOGISK-KONTROLL                                   
115900          END-IF                                                          
116000       END-IF                                                             
116100                                                                          
116200       IF INDATA-FEL                                                      
116300          IF MED-IDMFSFEL           = ZERO                                
116400             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
116500          END-IF                                                          
116600          CALL WMEDKONV USING MED-WMEDAREA                                
116700          MOVE MED-MFSFEL           TO MOD-TEMFSFEL                       
116800          PERFORM MFS-ROER-EJ-FAELT-UT                                    
116900          PERFORM MFS-ROER-EJ-FAELT-IN                                    
117000                                                                          
117100          IF KDCMD-RAETT                                                  
117200            PERFORM MFS-LAES-IN-IGEN                                      
117300          END-IF                                                          
117400          IF IDPRT-RAETT                                                  
117500            MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPRT-ATTR                  
117600          END-IF                                                          
117700          IF IDANSTNR-RAETT                                               
117800            MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDANSTNR-ATTR               
117900          END-IF                                                          
118000       END-IF                                                             
118100     END-IF                                                               
118200                                                                          
118300     .                                                                    
118400     EJECT                                                                
118500 GA-FORMELL-KONTROLL SECTION.                                             
118600                                                                          
118700     PERFORM GAA-KOLLA-KDCMD                                              
118800     IF INDATA-OK                                                         
118900       MOVE JA   TO SW-KDCMD-RAETT-IFYLLD                                 
119000     END-IF                                                               
119100                                                                          
119200     PERFORM GAAA-KOLLA-IDANSTNR                                          
119300                                                                          
119400     IF MFS-UPDATE                                                        
119500        CONTINUE                                                          
119600     ELSE                                                                 
119700        IF KDCMD-ETIKETT                                                  
119800           CONTINUE                                                       
119900        ELSE                                                              
120000           PERFORM GAB-KOLLA-IDPRT                                        
120100        END-IF                                                            
120200     END-IF                                                               
120300                                                                          
120400     IF (KDCMD-UTSKRIFT AND KDCMD-BORTTAG)                                
120500     OR (KDCMD-UTSKRIFT AND KDCMD-ETIKETT)                                
120600     OR (KDCMD-UTSKRIFT AND KDCMD-TEXT)                                   
120700     OR (KDCMD-BORTTAG  AND KDCMD-ETIKETT)                                
120800     OR (KDCMD-BORTTAG  AND KDCMD-TEXT)                                   
120900     OR (KDCMD-ETIKETT  AND KDCMD-TEXT)                                   
121000        MOVE NEJ                  TO INDATA-SW                            
121100        MOVE ERR-FLERA-FUNKTIONER TO MED-IDMFSFEL                         
121200     END-IF                                                               
121300     IF KDCMD-SAKNAS   AND MFS-UPDATE                                     
121400        MOVE NEJ                  TO INDATA-SW                            
121500        MOVE INF-PRESS-PF4        TO MED-IDMFSFEL                         
121600     END-IF                                                               
121700                                                                          
121800     IF KDCMD-SAKNAS   AND MFS-PRINT                                      
121900       IF IDSNDNNR-FINNS AND IDKOLLI-FINNS                                
122000         CONTINUE                                                         
122100       ELSE                                                               
122200         MOVE MFS-ALFA-FAELT-FEL         TO MOD-IDPRT-ATTR                
122300         MOVE NEJ                        TO INDATA-SW                     
122400         MOVE ERR-UPPGIFTER-SAKNAS       TO MED-IDMFSFEL                  
122500         MOVE NEJ  TO SW-IDPRT-RAETT-IFYLLD                               
122600       END-IF                                                             
122700     END-IF                                                               
122800     .                                                                    
122900     EJECT                                                                
123000 GAA-KOLLA-KDCMD      SECTION.                                            
123100                                                                          
123200     MOVE +1                           TO RAD-IX                          
123300                                                                          
123400     PERFORM UNTIL RAD-IX               >  MAX-IX                         
123500        IF MID-KDCMD(RAD-IX)            NOT = ALL '+'                     
123600           IF MID-KDCMD(RAD-IX)         = (W-UTSKRIFT OR W-PRINT)         
123700                                      AND MFS-PRINT                       
123800              MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-ATTR(RAD-IX)         
123900              MOVE JA                   TO SW-KDCMD                       
124000                                           SW-KDCMD-UTSKRIFT              
124100           ELSE                                                           
124200             IF MID-KDCMD(RAD-IX)       = (W-BORTTAG OR W-DELETE)         
124300                                      AND MFS-UPDATE                      
124400                MOVE MFS-ALFA-FAELT-RAETT TO                              
124500                                    MOD-KDCMD-ATTR(RAD-IX)                
124600                MOVE JA                   TO SW-KDCMD                     
124700                                             SW-KDCMD-BORTTAG             
124800             ELSE                                                         
124900               IF MID-KDCMD(RAD-IX)   = W-TEXT AND MFS-UPDATE             
125000                                                                          
125100                  IF MID-TERETNOT(RAD-IX)  = ALL '+'                      
125200                    MOVE MFS-ALFA-FAELT-FEL   TO                          
125300                                       MOD-KDCMD-ATTR(RAD-IX)             
125400                    MOVE ERR-ADD-CASE-INFO    TO MED-IDMFSFEL             
125500                    MOVE NEJ                  TO INDATA-SW                
125600                  ELSE                                                    
125700                    MOVE MFS-ALFA-FAELT-RAETT TO                          
125800                                      MOD-KDCMD-ATTR(RAD-IX)              
125900                  END-IF                                                  
126000                  MOVE JA                     TO SW-KDCMD                 
126100                                                 SW-KDCMD-TEXT            
126200               ELSE                                                       
126300                 IF MID-KDCMD(RAD-IX)      = W-ETIKETT AND                
126400                    MFS-PRINT                                             
126500                    MOVE MFS-ALFA-FAELT-RAETT TO                          
126600                                        MOD-KDCMD-ATTR(RAD-IX)            
126700                    MOVE JA                   TO SW-KDCMD                 
126800                                                 SW-KDCMD-ETIKETT         
126900                 ELSE                                                     
127000                   MOVE MFS-ALFA-FAELT-FEL   TO                           
127100                                       MOD-KDCMD-ATTR(RAD-IX)             
127200                   MOVE NEJ                  TO INDATA-SW                 
127300                   MOVE JA                   TO SW-KDCMD                  
127400                 END-IF                                                   
127500               END-IF                                                     
127600             END-IF                                                       
127700           END-IF                                                         
127800        END-IF                                                            
127900        ADD +1                          TO RAD-IX                         
128000     END-PERFORM                                                          
128100                                                                          
128200     .                                                                    
128300     EJECT                                                                
128400 GAAA-KOLLA-IDANSTNR SECTION.                                             
128500                                                                          
128600     IF CDC-SE                                                            
128700       IF MID-IDANSTNR = ALL '+'                                          
128800         MOVE MFS-NUM-FAELT-FEL           TO MOD-IDANSTNR-ATTR            
128900         MOVE NEJ                         TO INDATA-SW                    
129000         MOVE ERR-UPPGIFTER-SAKNAS        TO MED-IDMFSFEL                 
129100         MOVE NEJ  TO SW-IDANSTNR-RAETT-IFYLLD                            
129200       ELSE                                                               
129300         IF MID-IDANSTNR NUMERIC                                          
129400           MOVE MID-IDANSTNR              TO MOD-IDANSTNR                 
129500           MOVE MFS-NUM-FAELT-RAETT       TO MOD-IDANSTNR-ATTR            
129600         ELSE                                                             
129700           MOVE MFS-NUM-FAELT-FEL         TO MOD-IDANSTNR-ATTR            
129800           MOVE NEJ                       TO INDATA-SW                    
129900           MOVE NEJ  TO SW-IDANSTNR-RAETT-IFYLLD                          
130000         END-IF                                                           
130100       END-IF                                                             
130200     ELSE                                                                 
130300       IF MID-IDANSTNR NOT = ALL '+'                                      
130400         IF MID-IDANSTNR NOT NUMERIC                                      
130500           MOVE MFS-NUM-FAELT-FEL         TO MOD-IDANSTNR-ATTR            
130600           MOVE NEJ                       TO INDATA-SW                    
130700           MOVE NEJ  TO SW-IDANSTNR-RAETT-IFYLLD                          
130800         ELSE                                                             
130900           MOVE MID-IDANSTNR              TO MOD-IDANSTNR                 
131000           MOVE MFS-NUM-FAELT-RAETT       TO MOD-IDANSTNR-ATTR            
131100         END-IF                                                           
131200       END-IF                                                             
131300     END-IF                                                               
131400     .                                                                    
131500     EJECT                                                                
131600 GAB-KOLLA-IDPRT      SECTION.                                            
131700                                                                          
131800     IF MID-IDPRT                       NOT = ALL '+'                     
131900        MOVE MFS-ALFA-FAELT-RAETT       TO MOD-IDPRT-ATTR                 
132000     ELSE                                                                 
132100        MOVE ERR-WRONG-PRINTER          TO MED-IDMFSFEL                   
132200        MOVE MFS-ALFA-FAELT-FEL         TO MOD-IDPRT-ATTR                 
132300        MOVE NEJ                        TO INDATA-SW                      
132400        MOVE NEJ  TO SW-IDPRT-RAETT-IFYLLD                                
132500     END-IF                                                               
132600                                                                          
132700     .                                                                    
132800     EJECT                                                                
132900 GB-LOGISK-KONTROLL SECTION.                                              
133000                                                                          
133100     IF MFS-PRINT                                                         
133200        PERFORM GBA-KOLLA-IDPRT                                           
133300     END-IF                                                               
133400     IF MFS-UPDATE                                                        
133500        PERFORM GBB-KOLLA-STATUS                                          
133600     END-IF                                                               
133700     .                                                                    
133800     EJECT                                                                
133900                                                                          
134000 GBA-KOLLA-IDPRT                  SECTION.                                
134100                                                                          
134200     MOVE SPACE                TO PRT-IDPRTLST                            
134300     MOVE '4RT'                TO PRT-IDPRTLST(1:3)                       
134400                                                                          
134500     MOVE MID-IDPRT            TO PRT-IDPRTLST(4:3)                       
134600     MOVE 1                    TO PRT-KDCALL                              
134700     CALL W006PRT USING PRT-W006PRT                                       
134800                                                                          
134900     IF PRT-KDSVAR                     = 'F'                              
135000         MOVE ERR-WRONG-KEY            TO MED-IDMFSFEL                    
135100         MOVE PRT-IDPRTLST             TO MED-TEMFSINF                    
135200         MOVE NEJ                      TO INDATA-SW                       
135300         MOVE MFS-ALFA-FAELT-FEL       TO MOD-IDPRT-ATTR                  
135400         MOVE NEJ  TO SW-IDPRT-RAETT-IFYLLD                               
135500     END-IF                                                               
135600     .                                                                    
135700     EJECT                                                                
135800 GBB-KOLLA-STATUS                 SECTION.                                
135900                                                                          
136000     MOVE +1        TO RAD-IX                                             
136100     PERFORM UNTIL RAD-IX               >  MAX-IX                         
136200       IF MID-KDCMD (RAD-IX) = W-BORTTAG OR W-DELETE                      
136300           INSPECT MID-IDDISTR(RAD-IX) REPLACING                          
136400                   LEADING SPACE BY ZERO                                  
136500           INSPECT MID-IDKUNDNR(RAD-IX) REPLACING                         
136600                   LEADING SPACE BY ZERO                                  
136700           INSPECT MID-IDRAPPNR(RAD-IX) REPLACING                         
136800                   LEADING SPACE BY ZERO                                  
136900           IF  MID-IDDISTR (RAD-IX) NUMERIC                  AND          
137000               MID-IDDISTR (RAD-IX) > ZERO                   AND          
137100               MID-IDKUNDNR(RAD-IX) NUMERIC                  AND          
137200               MID-IDRAPPNR(RAD-IX) NUMERIC                               
137300             MOVE MID-IDDISTR(RAD-IX) TO W-IDDISTR                        
137400             MOVE MID-IDKUNDNR(RAD-IX) TO W-IDKUNDNR                      
137500             MOVE MID-IDRAPPNR(RAD-IX) TO W-IDRAPPNR                      
137600                                                                          
137700             PERFORM IMS-GU-WLKREE01                                      
137800             IF ANM-KDLEVANM = W-ANM-UTF                                  
137900                CONTINUE                                                  
138000             ELSE                                                         
138100                MOVE MFS-ALFA-FAELT-FEL   TO                              
138200                                    MOD-KDCMD-ATTR(RAD-IX)                
138300                MOVE NEJ                  TO INDATA-SW                    
138400                MOVE NEJ  TO SW-KDCMD-RAETT-IFYLLD                        
138500             END-IF                                                       
138600           ELSE                                                           
138700              MOVE MFS-ALFA-FAELT-FEL   TO                                
138800                                  MOD-KDCMD-ATTR(RAD-IX)                  
138900              MOVE NEJ                  TO INDATA-SW                      
139000              MOVE NEJ  TO SW-KDCMD-RAETT-IFYLLD                          
139100           END-IF                                                         
139200       END-IF                                                             
139300       ADD +1           TO RAD-IX                                         
139400     END-PERFORM                                                          
139500     .                                                                    
139600     EJECT                                                                
139700 H-UPPDATERA-SKRIV-UT SECTION.                                            
139800                                                                          
139900     IF KDCMD-SAKNAS AND MFS-PRINT                                        
140000       PERFORM HB-PRINTA-TILLSTAND-KOLLI                                  
140100     ELSE                                                                 
140200       PERFORM HA-BEHANDLA-VALDA-TILLSTAND                                
140300     END-IF                                                               
140400                                                                          
140500     IF PRINTAT                                                           
140600       MOVE INF-PRINT-BEG          TO MED-IDMFSINF                        
140700     ELSE                                                                 
140800       IF MFS-UPDATE                                                      
140900         MOVE  INF-UPDATE-DONE      TO MED-IDMFSINF                       
141000       ELSE                                                               
141100         MOVE  ERR-NOTHING-PRINTED   TO MED-IDMFSINF                      
141200       END-IF                                                             
141300     END-IF                                                               
141400     CALL WMEDKONV USING MED-WMEDAREA                                     
141500     MOVE MED-MFSINF             TO MOD-TEMFSINF                          
141600     PERFORM MFS-FORM-ATTR                                                
141700     PERFORM MFS-RENSA-FAELT-IN                                           
141800     PERFORM MFS-ROER-EJ-RADFAELT-UT                                      
141900     .                                                                    
142000     EJECT                                                                
142100                                                                          
142200 HA-BEHANDLA-VALDA-TILLSTAND SECTION.                                     
142300                                                                          
142400     MOVE +1                       TO RAD-IX                              
142500                                      4794-IX                             
142600     PERFORM UNTIL RAD-IX          >  MAX-IX                              
142700                                                                          
142800        IF MID-KDCMD(RAD-IX)       = W-UTSKRIFT OR W-BORTTAG OR           
142900                                     W-PRINT    OR W-DELETE  OR           
143000                                     W-ETIKETT  OR W-TEXT    OR           
143100                                     KDCMD-SAKNAS                         
143200           INSPECT MID-IDDISTR(RAD-IX) REPLACING                          
143300                   LEADING SPACE BY ZERO                                  
143400           INSPECT MID-IDKUNDNR(RAD-IX) REPLACING                         
143500                   LEADING SPACE BY ZERO                                  
143600           INSPECT MID-IDRAPPNR(RAD-IX) REPLACING                         
143700                   LEADING SPACE BY ZERO                                  
143800           INSPECT MID-IDKOLLI (RAD-IX) REPLACING                         
143900                   LEADING SPACE BY ZERO                                  
144000                                                                          
144100           IF  MID-IDDISTR (RAD-IX) NUMERIC                  AND          
144200               MID-IDDISTR (RAD-IX) > ZERO                   AND          
144300               MID-IDKUNDNR(RAD-IX) NUMERIC                  AND          
144400               MID-IDRAPPNR(RAD-IX) NUMERIC                  AND          
144500               MID-IDKOLLI (RAD-IX) NUMERIC                               
144600                                                                          
144700             MOVE MID-IDDISTR(RAD-IX) TO W-IDDISTR                        
144800                                         W-IDDISTR-FSEQ-MIN               
144900                                         W-IDDISTR-FSEQ-MAX               
145000             MOVE MID-IDKUNDNR(RAD-IX) TO W-IDKUNDNR                      
145100                                          W-IDKUNDNR-FSEQ-MIN             
145200                                          W-IDKUNDNR-FSEQ-MAX             
145300             MOVE MID-IDRAPPNR(RAD-IX) TO W-IDRAPPNR                      
145400                                          W-IDRAPPNR-FSEQ-MIN             
145500                                          W-IDRAPPNR-FSEQ-MAX             
145600             MOVE MID-IDKOLLI (RAD-IX) TO W-IDKOLLI-SPAR                  
145700                                                                          
145800             IF MID-KDCMD(RAD-IX) = W-BORTTAG OR W-DELETE OR              
145900                                    W-TEXT                                
146000               IF MID-KDCMD(RAD-IX) = W-BORTTAG OR W-DELETE               
146100                 PERFORM IMS-GU-SEQF-WLRETA01                             
146200                 PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT             
146300                   IF RET-IDKOLLI = W-IDKOLLI-SPAR                        
146400                     MOVE RET-DAREGDAT TO W-DAREGDAT                      
146500                     MOVE RET-TIKLOCK  TO W-TIKLOCK                       
146600                     PERFORM IMS-GHU-WLRETA01                             
146700                     PERFORM IMS-DLET-WLRETA01                            
146800                   END-IF                                                 
146900                   PERFORM IMS-GN-SEQF-WLRETA01                           
147000                 END-PERFORM                                              
147100               ELSE                                                       
147200                 PERFORM IMS-GHU-RETA4-FSEQ                               
147300                 PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT             
147400                   IF RET-IDKOLLI = W-IDKOLLI-SPAR                        
147500                     MOVE SPACE                TO RET-TERETNOT            
147600                     MOVE MID-TERETNOT(RAD-IX) TO RET-TERETNOT            
147700                     PERFORM IMS-REPL-RETA4                               
147800                   END-IF                                                 
147900                   PERFORM IMS-GHN-RETA4-FSEQ                             
148000                 END-PERFORM                                              
148100               END-IF                                                     
148200             ELSE                                                         
148300               PERFORM IMS-GHU-WLKREE01                                   
148400               IF SEGMENT-FINNS                                           
148500                IF (MID-KDCMD (RAD-IX) = (W-UTSKRIFT OR W-DELETE))        
148600                 OR (KDCMD-SAKNAS AND ANM-KDLEVANM < 7)                   
148700                   MOVE MID-IDDISTR (RAD-IX)   TO                         
148800                                   MOD4794-MID-IDDISTR(4794-IX)           
148900                   MOVE MID-IDKUNDNR(RAD-IX)  TO                          
149000                                   MOD4794-MID-IDKUNDNR(4794-IX)          
149100                   MOVE MID-IDRAPPNR(RAD-IX)  TO                          
149200                                   MOD4794-MID-IDRAPPNR(4794-IX)          
149300                   ADD +1             TO 4794-IX                          
149400                                                                          
149500                   IF ANM-KDLEVANM = W-ANM-MOT                            
149600                      MOVE W-ANM-PAAB      TO ANM-KDLEVANM                
149700                      PERFORM IMS-REPL-WLKREE01                           
149800                                                                          
149900                      IF CDC-SE                                           
150000                        PERFORM S05-LAES-WLKREE11                         
150100                        PERFORM UNTIL SEGMENT-SAKNAS                      
150200                          MOVE MID-IDANSTNR    TO LEV-IDANSTNR-RET        
150300                          PERFORM IMS-REPL-WLKREE11                       
150400                          PERFORM S05-LAES-WLKREE11                       
150500                        END-PERFORM                                       
150600                      END-IF                                              
150700                   END-IF                                                 
150800                                                                          
150900                   PERFORM HAC-REDIGERA-UTSKRIFTS-NYCKLAR                 
151000                   PERFORM HAD-UPPDATERA-STATUS                           
151100                   MOVE JA          TO PRINTAT-SW                         
151200                 ELSE                                                     
151300                   IF MID-KDCMD(RAD-IX) = W-ETIKETT                       
151400                      PERFORM HAE-SKAPA-BC-TRANS                          
151500                      MOVE JA       TO PRINTAT-SW                         
151600                   END-IF                                                 
151700                 END-IF                                                   
151800               END-IF                                                     
151900             END-IF                                                       
152000           END-IF                                                         
152100        END-IF                                                            
152200        ADD +1                     TO RAD-IX                              
152300     END-PERFORM                                                          
152400                                                                          
152500     IF 4794-IX                > +1                                       
152600         PERFORM HAB-STARTA-4794                                          
152700     END-IF                                                               
152800     .                                                                    
152900     EJECT                                                                
153000                                                                          
153100 HAB-STARTA-4794  SECTION.                                                
153200                                                                          
153300     MOVE MFS-KDMFSFOR          TO P-TO-P2-KDMFSFOR                       
153400                                                                          
153500     MOVE 'W40735'              TO MOD4794-MID-IDPGM                      
153600     MOVE MSGI-IDDC             TO MOD4794-MID-IDDC                       
153700     MOVE PRT-IDPRTLST          TO MOD4794-MID-IDPRTLST                   
153800     COMPUTE MOD4794-MID-KVPOST = 4794-IX - 1                             
153900                                                                          
154000     COMPUTE P-TO-P2-LL = LENGTH OF MOD4794-MID-W4I79401 + 17             
154100                                                                          
154200     PERFORM IMS-ISRT-MSG-ALT-4794                                        
154300     MOVE SPACE TO MOD4794-MID-W4I79401                                   
154400     .                                                                    
154500     EJECT                                                                
154600 HAC-REDIGERA-UTSKRIFTS-NYCKLAR SECTION.                                  
154700     SKIP2                                                                
154800     IF MSGI-IDRT NOT = SPACE                                             
154900         MOVE MSGI-IDRT    TO W-IDRT-MIN                                  
155000                              W-IDRT-MAX                                  
155100     END-IF                                                               
155200     IF MSGI-IDRTLOP NOT = SPACE                                          
155300         IF MSGI-IDRTLOP NUMERIC AND MSGI-IDRTLOP > ZERO                  
155400             MOVE MSGI-IDRTLOP  TO W-IDRTLOP-MIN                          
155500                                   W-IDRTLOP-MAX                          
155600         END-IF                                                           
155700     END-IF                                                               
155800                                                                          
155900     MOVE MID-IDKOLLI (RAD-IX)   TO W-IDKOLLI-MIN                         
156000                                    W-IDKOLLI-MAX                         
156100                                    W-IDKOLLI-BSEQ                        
156200     MOVE MID-IDDISTR (RAD-IX)   TO W-IDDISTR-FSEQ-MIN                    
156300                                    W-IDDISTR-FSEQ-MAX                    
156400     MOVE MID-IDKUNDNR (RAD-IX)  TO W-IDKUNDNR-FSEQ-MIN                   
156500                                    W-IDKUNDNR-FSEQ-MAX                   
156600     MOVE MID-IDRAPPNR (RAD-IX)  TO W-IDRAPPNR-FSEQ-MIN                   
156700                                    W-IDRAPPNR-FSEQ-MAX                   
156800     .                                                                    
156900     EJECT                                                                
157000 HAD-UPPDATERA-STATUS SECTION.                                            
157100                                                                          
157200***** UPPDATERINGEN BORTTAGEN PGA ATT DEN STÖR                            
157300***** SORTORDNINGEN I KOLLIKÖN                                            
157400*****                                                                     
157500*    PERFORM IMS-GHU-RETA4-FSEQ                                           
157600*    MOVE RET-IDRT           TO W-IDRT-BSEQ                               
157700*    MOVE RET-IDRTLOP        TO W-IDRTLOP-BSEQ                            
157800*    PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
157900*       MOVE '5'        TO RET-KDRETSTA                                   
158000*       PERFORM IMS-REPL-RETA4                                            
158100*       PERFORM IMS-GHN-RETA4-FSEQ                                        
158200*    END-PERFORM                                                          
158300                                                                          
158400     PERFORM IMS-GHU-RETA3-BSEQ                                           
158500     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
158600        MOVE '8'        TO RET-KDKOLSTA                                   
158700        PERFORM IMS-REPL-RETA3                                            
158800        PERFORM IMS-GHN-RETA3-BSEQ                                        
158900     END-PERFORM                                                          
159000     .                                                                    
159100     EJECT                                                                
159200 HAE-SKAPA-BC-TRANS SECTION.                                              
159300                                                                          
159400     MOVE ZERO TO BC-URV-IDDISTR                                          
159500                  BC-URV-IDKUNDNR                                         
159600                  BC-URV-IDRAPPNR                                         
159700                                                                          
159800     INSPECT MID-IDDISTR(RAD-IX) REPLACING LEADING SPACE BY ZERO          
159900     INSPECT MID-IDKUNDNR(RAD-IX) REPLACING LEADING SPACE BY ZERO         
160000     INSPECT MID-IDRAPPNR(RAD-IX) REPLACING LEADING SPACE BY ZERO         
160100     MOVE MID-IDDISTR(RAD-IX)  TO BC-URV-IDDISTR                          
160200     MOVE MID-IDKUNDNR(RAD-IX) TO BC-URV-IDKUNDNR                         
160300     MOVE MID-IDRAPPNR(RAD-IX) TO BC-URV-IDRAPPNR                         
160400                                                                          
160500                                                                          
160600     MOVE '4735'   TO MSGSOP-IDTRANS                                      
160700     MOVE '1'      TO MSGSOP-KDMFSFOR                                     
160800     MOVE 'W418S1' TO MSGSOP-IDPROCESS                                    
160900     MOVE 'O'      TO MSGSOP-KDSOPFUNK                                    
161000                                                                          
161100     STRING 'URVAL(' WS-BC ')'                                            
161200             DELIMITED BY SIZE INTO MSGSOP-TESYMBV                        
161300                                                                          
161400     PERFORM IMS-INSERT-ALTMSG-SOP                                        
161500     .                                                                    
161600     EJECT                                                                
161700 HB-PRINTA-TILLSTAND-KOLLI SECTION.                                       
161800                                                                          
161900     MOVE +1                      TO 4794-IX                              
162000                                                                          
162100     MOVE MSGI-IDRT           TO W-IDRT-BSEQ                              
162200     MOVE MSGI-IDRTLOP        TO W-IDRTLOP-BSEQ                           
162300     MOVE MSGI-IDKOLLI        TO W-IDKOLLI-BSEQ                           
162400                                                                          
162500     PERFORM IMS-GU-RETA3-BSEQ                                            
162600     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
162700                                                                          
162800       MOVE RET-IDDISTR   TO W-IDDISTR                                    
162900       MOVE RET-IDKUNDNR  TO W-IDKUNDNR                                   
163000       MOVE RET-IDRAPPNR  TO W-IDRAPPNR                                   
163100                                                                          
163200       PERFORM IMS-GHU-WLKREE01                                           
163300       IF SEGMENT-FINNS AND ANM-KDLEVANM < 7                              
163400         MOVE ANM-IDDISTR    TO  WS-IDDISTR-4                             
163500         MOVE WS-IDDISTR-4   TO  MOD4794-MID-IDDISTR(4794-IX)             
163600         MOVE ANM-IDKUNDNR   TO  WS-IDKUNDNR-6                            
163700         MOVE WS-IDKUNDNR-6  TO  MOD4794-MID-IDKUNDNR(4794-IX)            
163800         MOVE ANM-IDRAPPNR   TO  MOD4794-MID-IDRAPPNR(4794-IX)            
163900                                                                          
164000         ADD +1             TO 4794-IX                                    
164100                                                                          
164200         IF 4794-IX  > 4794-MAX-IX                                        
164300           PERFORM HAB-STARTA-4794                                        
164400           MOVE +1        TO 4794-IX                                      
164500           MOVE JA        TO PRINTAT-SW                                   
164600         END-IF                                                           
164700                                                                          
164800         IF ANM-KDLEVANM = W-ANM-MOT                                      
164900            MOVE W-ANM-PAAB      TO ANM-KDLEVANM                          
165000            PERFORM IMS-REPL-WLKREE01                                     
165100            IF CDC-SE                                                     
165200              PERFORM S05-LAES-WLKREE11                                   
165300                                                                          
165400              PERFORM UNTIL SEGMENT-SAKNAS                                
165500                MOVE MID-IDANSTNR TO LEV-IDANSTNR-RET                     
165600                PERFORM IMS-REPL-WLKREE11                                 
165700                PERFORM S05-LAES-WLKREE11                                 
165800              END-PERFORM                                                 
165900            END-IF                                                        
166000         END-IF                                                           
166100                                                                          
166200       END-IF                                                             
166300       PERFORM IMS-GN-RETA3-BSEQ                                          
166400     END-PERFORM                                                          
166500                                                                          
166600     IF 4794-IX > +1                                                      
166700       PERFORM HAB-STARTA-4794                                            
166800       MOVE JA          TO PRINTAT-SW                                     
166900     END-IF                                                               
167000                                                                          
167100*SO - SKALL MAN ÄNDRA KOLLISTATUS HÄR ??? IDAG ÄR DETTA BORT-             
167200*SO - STJÄRNAT I SECTION HAD-UPPDATERA-STATUS. SKALL EJ UPPDATERA         
167300*SO - KOLLISTATUS HÄR HELLER ENLIGT SUSSI 010508.                         
167400*                                                                         
167500*    IF PRINTAT                                                           
167600*      PERFORM IMS-GHU-RETA3-BSEQ                                         
167700*      PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                       
167800*         MOVE '8'        TO RET-KDKOLSTA                                 
167900*         PERFORM IMS-REPL-RETA3                                          
168000*         PERFORM IMS-GHN-RETA3-BSEQ                                      
168100*      END-PERFORM                                                        
168200*    END-IF                                                               
168300     .                                                                    
168400                                                                          
168500     EJECT                                                                
168600 S01-INSERT-ALTMSG SECTION.                                               
168700                                                                          
168800     MOVE P-TO-P-SW            TO MSG-IO-AREA                             
168900     PERFORM IMS-CHANGE-ALTMSG                                            
169000     IF STATUS-OK                                                         
169100       PERFORM IMS-INSERT-ALTMSG                                          
169200     ELSE                                                                 
169300       MOVE LOW-VALUE          TO MSG-AREA                                
169400       MOVE 'W4O73501'         TO MFS-IDMOD                               
169500       MOVE '4735'             TO MOD-IDTRANS                             
169600       MOVE P-TO-P-KDTRANS (2:1) TO W-BILD (1:1)                          
169700       MOVE P-TO-P-KDTRANS (4:3) TO W-BILD (2:3)                          
169800       IF SECURITY-FEL                                                    
169900         STRING 'NOT AUTHORIZED TO USE '                                  
170000                W-BILD                                                    
170100                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
170200       ELSE                                                               
170300         STRING 'WRONG PICTURE '                                          
170400                 W-BILD                                                   
170500                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
170600       END-IF                                                             
170700       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O73501 + 4                      
170800       PERFORM MFS-ROER-EJ-FAELT-IN                                       
170900       PERFORM MFS-ROER-EJ-FAELT-UT                                       
171000       PERFORM IMS-INSERT-MSG                                             
171100     END-IF                                                               
171200     .                                                                    
171300                                                                          
171400     EJECT                                                                
171500 S02-KOLLA-OM-FLERA-KOLLIN SECTION.                                       
171600                                                                          
171700     MOVE ZERO TO ANTAL-KOLLIN                                            
171800     PERFORM IMS-GU-WLRETG01                                              
171900     PERFORM UNTIL SEGMENT-SAKNAS                                         
172000         ADD +1 TO ANTAL-KOLLIN                                           
172100         PERFORM IMS-GN-WLRETG01                                          
172200     END-PERFORM                                                          
172300     .                                                                    
172400                                                                          
172500     EJECT                                                                
172600 S03-FIXA-ENTER-KEY SECTION.                                              
172700                                                                          
172800     IF SEGMENT-FINNS                                                     
172900         MOVE RET-IDDC      TO W-MINKEYB1-IDDC                            
173000         MOVE RET-IDRT      TO W-MINKEYB1-IDRT                            
173100         MOVE RET-IDRTLOP   TO W-MINKEYB1-IDRTLOP                         
173200         MOVE RET-IDKOLLI   TO W-MINKEYB1-IDKOLLI                         
173300         MOVE RET-DAREGDAT  TO W-MINKEYB1-DAREGDAT                        
173400         MOVE RET-TIKLOCK   TO W-MINKEYB1-TIKLOCK                         
173500         MOVE RET-IDDC      TO W-MINKEYF1-IDDC                            
173600         MOVE RET-IDDISTR   TO W-MINKEYF1-IDDISTR                         
173700         MOVE RET-IDKUNDNR  TO W-MINKEYF1-IDKUNDNR                        
173800         MOVE RET-IDRAPPNR  TO W-MINKEYF1-IDRAPPNR                        
173900     ELSE                                                                 
174000         MOVE MSGI-IDDC     TO W-MINKEYB1-IDDC                            
174100         MOVE ZERO          TO W-MINKEYB1-IDRT                            
174200                               W-MINKEYB1-IDRTLOP                         
174300                               W-MINKEYB1-IDKOLLI                         
174400                               W-MINKEYB1-DAREGDAT                        
174500                               W-MINKEYB1-TIKLOCK                         
174600         MOVE MSGI-IDDC     TO W-MINKEYF1-IDDC                            
174700         MOVE ZERO          TO W-MINKEYF1-IDDISTR                         
174800                               W-MINKEYF1-IDKUNDNR                        
174900                               W-MINKEYF1-IDRAPPNR                        
175000     END-IF                                                               
175100     .                                                                    
175200                                                                          
175300     EJECT                                                                
175400 S04-FIXA-NEXT-KEY SECTION.                                               
175500                                                                          
175600     IF SEGMENT-FINNS                                                     
175700         IF MED-IDMFSINF = SPACE OR '006'                                 
175800           MOVE INF-MORE-INFO-FINNS   TO MED-IDMFSINF                     
175900           CALL WMEDKONV USING MED-WMEDAREA                               
176000           MOVE MED-TEMFSINF          TO MOD-TEMFSINF                     
176100         END-IF                                                           
176200                                                                          
176300         MOVE RET-IDDC              TO W-MINKEYB1-IDDC-NEXT               
176400         MOVE RET-IDRT              TO W-MINKEYB1-IDRT-NEXT               
176500         MOVE RET-IDRTLOP           TO W-MINKEYB1-IDRTLOP-NEXT            
176600         MOVE RET-IDKOLLI           TO W-MINKEYB1-IDKOLLI-NEXT            
176700         MOVE RET-DAREGDAT          TO W-MINKEYB1-DAREGDAT-NEXT           
176800         MOVE RET-TIKLOCK           TO W-MINKEYB1-TIKLOCK-NEXT            
176900         MOVE RET-IDDC              TO W-MINKEYF1-IDDC-NEXT               
177000         MOVE RET-IDDISTR           TO W-MINKEYF1-IDDISTR-NEXT            
177100         MOVE RET-IDKUNDNR          TO W-MINKEYF1-IDKUNDNR-NEXT           
177200         MOVE RET-IDRAPPNR          TO W-MINKEYF1-IDRAPPNR-NEXT           
177300     ELSE                                                                 
177400         MOVE MSGI-IDDC             TO W-MINKEYB1-IDDC-NEXT               
177500         MOVE W-MINKEYB1-IDRT       TO W-MINKEYB1-IDRT-NEXT               
177600         MOVE W-MINKEYB1-IDRTLOP    TO W-MINKEYB1-IDRTLOP-NEXT            
177700         MOVE W-MINKEYB1-IDKOLLI    TO W-MINKEYB1-IDKOLLI-NEXT            
177800         MOVE W-MINKEYB1-DAREGDAT   TO W-MINKEYB1-DAREGDAT-NEXT           
177900         MOVE W-MINKEYB1-TIKLOCK    TO W-MINKEYB1-TIKLOCK-NEXT            
178000         MOVE MSGI-IDDC             TO W-MINKEYF1-IDDC-NEXT               
178100         MOVE ZERO                  TO W-MINKEYF1-IDDISTR-NEXT            
178200                                       W-MINKEYF1-IDKUNDNR-NEXT           
178300                                       W-MINKEYF1-IDRAPPNR-NEXT           
178400     END-IF                                                               
178500                                                                          
178600     MOVE '4735'            TO MSGI-IDTRANS                               
178700     MOVE '002'             TO MSGI-KDCALL                                
178800     MOVE '4735'            TO W-MINKEY-IDTRANS                           
178900     MOVE W-MINKEY-WDA3     TO MSGI-SPAR-AREA                             
179000     CALL W005INIT   USING MSGI-WMSGINIT  USEA-PCB                        
179100     .                                                                    
179200                                                                          
179300     EJECT                                                                
179400 S05-LAES-WLKREE11        SECTION.                                        
179500                                                                          
179600     MOVE NEJ                    TO OKOD-FL-RETILL                        
179700                                    OKOD-FL-INTERNUPPACKNING              
179800     PERFORM IMS-GHNP-WLKREE11                                            
179900     PERFORM UNTIL OKOD-FL-RETILL = 'J' OR SEGMENT-SAKNAS                 
180000                OR OKOD-FL-INTERNUPPACKNING = 'J'                         
180100        IF LEV-KDKREBEH(1:1) = 'Y' OR                                     
180200           LEV-KDKREBEH(1:1) = 'J' OR                                     
180300           LEV-KDKREBEH(1:1) = 'C'                                        
180400          IF LEV-IDARTNR NOT = 100                                        
180500            MOVE LEV-KDANMORS TO OKOD-KDANMORS                            
180600*--ANROPA KONTROLL AV ORSAKSKODER                                         
180700            CALL W418OKOD USING OKOD-W418OKOD                             
180800          END-IF                                                          
180900        END-IF                                                            
181000        IF OKOD-FL-RETILL = 'J' OR                                        
181100           OKOD-FL-INTERNUPPACKNING = 'J'                                 
181200           CONTINUE                                                       
181300        ELSE                                                              
181400          PERFORM IMS-GHNP-WLKREE11                                       
181500        END-IF                                                            
181600     END-PERFORM                                                          
181700     .                                                                    
181800     EJECT                                                                
181900 MFS-RENSA-FAELT-UT SECTION.                                              
182000                                                                          
182100*    --- ALLA UTDATA-FÄLT                                                 
182200     MOVE +1 TO RAD-IX                                                    
182300     PERFORM UNTIL RAD-IX > MAX-IX                                        
182400     MOVE MFS-RENSA-FAELT         TO MOD-IDKOLLI (RAD-IX)                 
182500                                     MOD-IDDISTR (RAD-IX)                 
182600                                     MOD-IDKUNDNR(RAD-IX)                 
182700                                     MOD-IDRAPPNR (RAD-IX)                
182800                                     MOD-KVKOLLI  (RAD-IX)                
182900                                     MOD-FLFARLIG (RAD-IX)                
183000                                     MOD-IDFRASED-AAF (RAD-IX)            
183100                                     MOD-TERETNOT (RAD-IX)                
183200                                     MOD-KVKOLLI-DEL (RAD-IX)             
183300                                     MOD-KDLEVANM (RAD-IX)                
183400         ADD +1 TO RAD-IX                                                 
183500     END-PERFORM                                                          
183600     .                                                                    
183700     SKIP3                                                                
183800 MFS-RENSA-FAELT-IN SECTION.                                              
183900                                                                          
184000*    --- ALLA INDATA-FÄLT                                                 
184100     MOVE MFS-RENSA-FAELT TO MOD-IDANSTNR                                 
184200                             MOD-IDPRT-UPD                                
184300                                                                          
184400     MOVE +1 TO RAD-IX                                                    
184500     PERFORM UNTIL RAD-IX > MAX-IX                                        
184600         MOVE MFS-RENSA-FAELT TO MOD-KDCMD (RAD-IX)                       
184700                                                                          
184800         IF MFS-UPDATE                                                    
184900           CONTINUE                                                       
185000         ELSE                                                             
185100             MOVE MFS-RENSA-FAELT TO MOD-TERETNOT (RAD-IX)                
185200         END-IF                                                           
185300                                                                          
185400         ADD +1 TO RAD-IX                                                 
185500     END-PERFORM                                                          
185600     .                                                                    
185700     EJECT                                                                
185800 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
185900                                                                          
186000*    --- ALLA UTDATA-FÄLT                                                 
186100     MOVE MFS-ROER-EJ-FAELT TO MOD-IDPRT-UPD                              
186200     MOVE MFS-ROER-EJ-FAELT TO MOD-IDANSTNR                               
186300                                                                          
186400     MOVE +1 TO RAD-IX                                                    
186500     PERFORM UNTIL RAD-IX > MAX-IX                                        
186600         MOVE MFS-ROER-EJ-FAELT       TO MOD-IDKOLLI (RAD-IX)             
186700                                         MOD-IDDISTR (RAD-IX)             
186800                                         MOD-IDKUNDNR(RAD-IX)             
186900                                         MOD-IDRAPPNR (RAD-IX)            
187000                                         MOD-KVKOLLI (RAD-IX)             
187100                                         MOD-FLFARLIG (RAD-IX)            
187200                                         MOD-IDFRASED-AAF (RAD-IX)        
187300                                         MOD-TERETNOT (RAD-IX)            
187400                                         MOD-KVKOLLI-DEL (RAD-IX)         
187500                                         MOD-KDLEVANM (RAD-IX)            
187600         ADD +1 TO RAD-IX                                                 
187700     END-PERFORM                                                          
187800     .                                                                    
187900     EJECT                                                                
188000 MFS-ROER-EJ-RADFAELT-UT  SECTION.                                        
188100                                                                          
188200*    --- ALLA UTDATA-FÄLT                                                 
188300                                                                          
188400     MOVE +1 TO RAD-IX                                                    
188500     PERFORM UNTIL RAD-IX > MAX-IX                                        
188600         MOVE MFS-ROER-EJ-FAELT       TO MOD-IDKOLLI (RAD-IX)             
188700                                         MOD-IDDISTR (RAD-IX)             
188800                                         MOD-IDKUNDNR(RAD-IX)             
188900                                         MOD-IDRAPPNR (RAD-IX)            
189000                                         MOD-KVKOLLI (RAD-IX)             
189100                                         MOD-FLFARLIG (RAD-IX)            
189200                                         MOD-IDFRASED-AAF (RAD-IX)        
189300                                         MOD-KVKOLLI-DEL (RAD-IX)         
189400                                         MOD-KDLEVANM (RAD-IX)            
189500                                                                          
189600         IF MID-TERETNOT (RAD-IX) = ALL '+'                               
189700           MOVE MFS-ROER-EJ-FAELT     TO MOD-TERETNOT (RAD-IX)            
189800         ELSE                                                             
189900           MOVE MID-TERETNOT (RAD-IX) TO MOD-TERETNOT (RAD-IX)            
190000         END-IF                                                           
190100                                                                          
190200         ADD +1 TO RAD-IX                                                 
190300     END-PERFORM                                                          
190400     .                                                                    
190500     EJECT                                                                
190600 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
190700                                                                          
190800*    --- ALLA INDATA-FÄLT                                                 
190900     MOVE MFS-ROER-EJ-FAELT TO MOD-IDPRT-UPD                              
191000     MOVE MFS-ROER-EJ-FAELT TO MOD-IDANSTNR                               
191100     MOVE +1 TO RAD-IX                                                    
191200     PERFORM UNTIL RAD-IX > MAX-IX                                        
191300         MOVE MFS-ROER-EJ-FAELT   TO MOD-KDCMD (RAD-IX)                   
191400         ADD +1 TO RAD-IX                                                 
191500     END-PERFORM                                                          
191600     .                                                                    
191700     SKIP3                                                                
191800 MFS-LAES-IN-IGEN SECTION.                                                
191900                                                                          
192000*    --- ALLA INDATA-FÄLT                                                 
192100                                                                          
192200     MOVE +1 TO RAD-IX                                                    
192300     PERFORM UNTIL RAD-IX > MAX-IX                                        
192400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-ATTR (RAD-IX)              
192500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TERETNOT-ATTR (RAD-IX)           
192600       ADD +1 TO RAD-IX                                                   
192700     END-PERFORM                                                          
192800     .                                                                    
192900     SKIP3                                                                
193000 MFS-FORM-ATTR SECTION.                                                   
193100                                                                          
193200*    --- ALLA INDATA-FÄLT                                                 
193300     MOVE MFS-FORMATETS-ATTR TO MOD-IDPRT-ATTR                            
193400     MOVE MFS-FORMATETS-ATTR TO MOD-IDANSTNR-ATTR                         
193500                                                                          
193600     MOVE +1 TO RAD-IX                                                    
193700     PERFORM UNTIL RAD-IX > MAX-IX                                        
193800         MOVE MFS-FORMATETS-ATTR TO MOD-KDCMD-ATTR (RAD-IX)               
193900         MOVE MFS-FORMATETS-ATTR TO MOD-TERETNOT-ATTR (RAD-IX)            
194000         ADD +1 TO RAD-IX                                                 
194100     END-PERFORM                                                          
194200     .                                                                    
194300     EJECT                                                                
194400* --- IMS SEKTIONER ---                                                   
194500     SKIP3                                                                
194600 IMS-GET-MSG SECTION.                                                     
194700                                                                          
194800     MOVE '  QC' TO GODK-STATUSKODER                                      
194900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
195000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
195100     PERFORM IMS-STATUSKONTROLL                                           
195200     .                                                                    
195300     SKIP3                                                                
195400 IMS-INSERT-MSG SECTION.                                                  
195500                                                                          
195600     IF MSGI-IDLAND-SPR = 'GB'                                            
195700       MOVE 'N' TO MFS-KDHUVOMR                                           
195800     END-IF                                                               
195900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
196000     MOVE SPACE TO GODK-STATUSKODER                                       
196100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
196200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
196300     PERFORM IMS-STATUSKONTROLL                                           
196400     .                                                                    
196500     EJECT                                                                
196600 IMS-ISRT-MSG-ALT-4794 SECTION.                                           
196700                                                                          
196800     MOVE SPACE              TO GODK-STATUSKODER                          
196900     CALL CBLTDLI USING      PURG W4794-PCB                               
197000                                  P-TO-P-T94                              
197100     MOVE W4794-STATUS-CODE   TO STATUS-WS                                
197200     PERFORM IMS-STATUSKONTROLL                                           
197300     .                                                                    
197400     SKIP3                                                                
197500 IMS-CHANGE-ALTMSG SECTION.                                               
197600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
197700     MOVE '  A1A4' TO GODK-STATUSKODER                                    
197800     CALL CBLTDLI USING CHNG ALT-PCB MSG-KDTRANS-1                        
197900     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
198000     PERFORM IMS-STATUSKONTROLL                                           
198100     .                                                                    
198200     SKIP3                                                                
198300 IMS-INSERT-ALTMSG SECTION.                                               
198400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
198500     MOVE SPACE TO GODK-STATUSKODER                                       
198600     CALL CBLTDLI USING ISRT ALT-PCB MSG-IO-AREA                          
198700     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
198800     PERFORM IMS-STATUSKONTROLL                                           
198900     .                                                                    
199000     EJECT                                                                
199100 IMS-INSERT-ALTMSG-SOP SECTION.                                           
199200     MOVE SPACE TO GODK-STATUSKODER                                       
199300     CALL CBLTDLI USING PURG ALT2-PCB PROG-TO-PROG-SW                     
199400     MOVE ALT2-STATUS-CODE TO STATUS-WS                                   
199500     PERFORM IMS-STATUSKONTROLL                                           
199600     .                                                                    
199700     EJECT                                                                
199800 IMS-GU-WLKREE01 SECTION.                                                 
199900                                                                          
200000     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
200100          DELIMITED BY SIZE INTO SSA1                                     
200200     MOVE '  GE' TO GODK-STATUSKODER                                      
200300     CALL CBLTDLI USING GU KREE-PCB DLI-IO-A201 SSA1                      
200400     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
200500     PERFORM IMS-STATUSKONTROLL                                           
200600     .                                                                    
200700     SKIP3                                                                
200800 IMS-GHU-WLKREE01 SECTION.                                                
200900                                                                          
201000     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
201100          DELIMITED BY SIZE INTO SSA1                                     
201200     MOVE '    ' TO GODK-STATUSKODER                                      
201300     CALL CBLTDLI USING GHU KREE-PCB DLI-IO-A201 SSA1                     
201400     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
201500     PERFORM IMS-STATUSKONTROLL                                           
201600     .                                                                    
201700     EJECT                                                                
201800 IMS-REPL-WLKREE01 SECTION.                                               
201900                                                                          
202000     MOVE '  ' TO GODK-STATUSKODER                                        
202100     CALL CBLTDLI USING REPL KREE-PCB DLI-IO-A201                         
202200     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
202300     PERFORM IMS-STATUSKONTROLL                                           
202400     .                                                                    
202500     EJECT                                                                
202600 IMS-GHNP-WLKREE11 SECTION.                                               
202700                                                                          
202800     MOVE   'WLKREE11'  TO SSA1                                           
202900     MOVE '  GE' TO GODK-STATUSKODER                                      
203000     CALL CBLTDLI USING GHNP KREE-PCB WDA211-AREA SSA1                    
203100     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
203200     PERFORM IMS-STATUSKONTROLL                                           
203300     .                                                                    
203400     SKIP3                                                                
203500 IMS-REPL-WLKREE11 SECTION.                                               
203600                                                                          
203700     MOVE '  ' TO GODK-STATUSKODER                                        
203800     CALL CBLTDLI USING REPL KREE-PCB WDA211-AREA                         
203900     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
204000     PERFORM IMS-STATUSKONTROLL                                           
204100     .                                                                    
204200     EJECT                                                                
204300 IMS-GU-WLRETC01 SECTION.                                                 
204400                                                                          
204500     STRING 'WLRETC01(WDA3B1KY>=' W-WDA3B1-MIN-X                          
204600                    '&WDA3B1KY<=' W-WDA3B1-MAX-X ')'                      
204700          DELIMITED BY SIZE INTO SSA1                                     
204800     MOVE '  GE' TO GODK-STATUSKODER                                      
204900     CALL CBLTDLI USING GU RETC-PCB DLI-IO-A3B1 SSA1                      
205000     MOVE RETC-STATUS-CODE TO STATUS-WS                                   
205100     PERFORM IMS-STATUSKONTROLL                                           
205200     .                                                                    
205300     SKIP3                                                                
205400 IMS-GN-WLRETC01 SECTION.                                                 
205500                                                                          
205600     STRING 'WLRETC01(WDA3B1KY>=' W-WDA3B1-MIN-X                          
205700                    '&WDA3B1KY<=' W-WDA3B1-MAX-X ')'                      
205800          DELIMITED BY SIZE INTO SSA1                                     
205900     MOVE '  GE' TO GODK-STATUSKODER                                      
206000     CALL CBLTDLI USING GN RETC-PCB DLI-IO-A3B1 SSA1                      
206100     MOVE RETC-STATUS-CODE TO STATUS-WS                                   
206200     PERFORM IMS-STATUSKONTROLL                                           
206300     .                                                                    
206400     SKIP3                                                                
206500 IMS-GU-WLRETA01 SECTION.                                                 
206600                                                                          
206700     STRING 'WLRETA01(WDA301KY =' W-WDA301KY-X ')'                        
206800          DELIMITED BY SIZE INTO SSA1                                     
206900     MOVE '  GE' TO GODK-STATUSKODER                                      
207000     CALL CBLTDLI USING GU RETA1-PCB DLI-IO-A301 SSA1                     
207100     MOVE RETA1-STATUS-CODE TO STATUS-WS                                  
207200     PERFORM IMS-STATUSKONTROLL                                           
207300     .                                                                    
207400     SKIP2                                                                
207500 IMS-GHU-WLRETA01 SECTION.                                                
207600                                                                          
207700     STRING 'WLRETA01(WDA301KY =' W-WDA301KY-X ')'                        
207800          DELIMITED BY SIZE INTO SSA1                                     
207900     MOVE '  ' TO GODK-STATUSKODER                                        
208000     CALL CBLTDLI USING GHU RETA1-PCB DLI-IO-A301 SSA1                    
208100     MOVE RETA1-STATUS-CODE TO STATUS-WS                                  
208200     PERFORM IMS-STATUSKONTROLL                                           
208300     .                                                                    
208400     EJECT                                                                
208500 IMS-DLET-WLRETA01 SECTION.                                               
208600                                                                          
208700     MOVE '  ' TO GODK-STATUSKODER                                        
208800     CALL CBLTDLI USING DLET RETA1-PCB DLI-IO-A301                        
208900     MOVE RETA1-STATUS-CODE TO STATUS-WS                                  
209000     PERFORM IMS-STATUSKONTROLL                                           
209100     .                                                                    
209200     EJECT                                                                
209300 IMS-GU-SEQF-WLRETA01 SECTION.                                            
209400                                                                          
209500     STRING 'WLRETA01(WDA3FSEQ>=' W-WDA3FSEQ-MIN-X                        
209600                    '&WDA3FSEQ<=' W-WDA3FSEQ-MAX-X ')'                    
209700          DELIMITED BY SIZE INTO SSA1                                     
209800     MOVE '  GE' TO GODK-STATUSKODER                                      
209900     CALL CBLTDLI USING GU RETA2-PCB DLI-IO-A301 SSA1                     
210000     MOVE RETA2-STATUS-CODE TO STATUS-WS                                  
210100     PERFORM IMS-STATUSKONTROLL                                           
210200     .                                                                    
210300     SKIP3                                                                
210400 IMS-GN-SEQF-WLRETA01 SECTION.                                            
210500                                                                          
210600     STRING 'WLRETA01(WDA3FSEQ>=' W-WDA3FSEQ-MIN-X                        
210700                    '&WDA3FSEQ<=' W-WDA3FSEQ-MAX-X ')'                    
210800          DELIMITED BY SIZE INTO SSA1                                     
210900     MOVE '  GE' TO GODK-STATUSKODER                                      
211000     CALL CBLTDLI USING GN RETA2-PCB DLI-IO-A301 SSA1                     
211100     MOVE RETA2-STATUS-CODE TO STATUS-WS                                  
211200     PERFORM IMS-STATUSKONTROLL                                           
211300     .                                                                    
211400     EJECT                                                                
211500 IMS-GU-WLRETG01    SECTION.                                              
211600                                                                          
211700     STRING 'WLRETG01(WDA3F1KY>=' W-WDA3F1KY-MIN-X                        
211800                    '&WDA3F1KY<=' W-WDA3F1KY-MAX-X ')'                    
211900          DELIMITED BY SIZE INTO SSA1                                     
212000     MOVE '  GE' TO GODK-STATUSKODER                                      
212100     CALL CBLTDLI USING GU RETG-PCB DLI-IO-A3F1 SSA1                      
212200     MOVE RETG-STATUS-CODE TO STATUS-WS                                   
212300     PERFORM IMS-STATUSKONTROLL                                           
212400     .                                                                    
212500     SKIP2                                                                
212600 IMS-GN-WLRETG01    SECTION.                                              
212700                                                                          
212800     STRING 'WLRETG01(WDA3F1KY>=' W-WDA3F1KY-MIN-X                        
212900                    '&WDA3F1KY<=' W-WDA3F1KY-MAX-X ')'                    
213000          DELIMITED BY SIZE INTO SSA1                                     
213100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
213200     CALL CBLTDLI USING GN RETG-PCB DLI-IO-A3F1 SSA1                      
213300     MOVE RETG-STATUS-CODE TO STATUS-WS                                   
213400     PERFORM IMS-STATUSKONTROLL                                           
213500     .                                                                    
213600     SKIP2                                                                
213700 IMS-GU-RETA3-BSEQ SECTION.                                               
213800                                                                          
213900     STRING 'WLRETA01(WDA3BSEQ =' W-WDA3BSEQ-X ')'                        
214000          DELIMITED BY SIZE INTO SSA1                                     
214100     MOVE '  GE' TO GODK-STATUSKODER                                      
214200     CALL CBLTDLI USING GU RETA3-PCB DLI-IO-A301 SSA1                     
214300     MOVE RETA3-STATUS-CODE TO STATUS-WS                                  
214400     PERFORM IMS-STATUSKONTROLL                                           
214500     .                                                                    
214600     SKIP2                                                                
214700 IMS-GN-RETA3-BSEQ SECTION.                                               
214800                                                                          
214900     STRING 'WLRETA01(WDA3BSEQ =' W-WDA3BSEQ-X ')'                        
215000          DELIMITED BY SIZE INTO SSA1                                     
215100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
215200     CALL CBLTDLI USING GN RETA3-PCB DLI-IO-A301 SSA1                     
215300     MOVE RETA3-STATUS-CODE TO STATUS-WS                                  
215400     PERFORM IMS-STATUSKONTROLL                                           
215500     .                                                                    
215600     SKIP2                                                                
215700 IMS-GHU-RETA3-BSEQ SECTION.                                              
215800                                                                          
215900     STRING 'WLRETA01(WDA3BSEQ =' W-WDA3BSEQ-X ')'                        
216000          DELIMITED BY SIZE INTO SSA1                                     
216100     MOVE '  GE' TO GODK-STATUSKODER                                      
216200     CALL CBLTDLI USING GHU RETA3-PCB DLI-IO-A301 SSA1                    
216300     MOVE RETA3-STATUS-CODE TO STATUS-WS                                  
216400     PERFORM IMS-STATUSKONTROLL                                           
216500     .                                                                    
216600     SKIP2                                                                
216700 IMS-GHN-RETA3-BSEQ SECTION.                                              
216800                                                                          
216900     STRING 'WLRETA01(WDA3BSEQ =' W-WDA3BSEQ-X ')'                        
217000          DELIMITED BY SIZE INTO SSA1                                     
217100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
217200     CALL CBLTDLI USING GHN RETA3-PCB DLI-IO-A301 SSA1                    
217300     MOVE RETA3-STATUS-CODE TO STATUS-WS                                  
217400     PERFORM IMS-STATUSKONTROLL                                           
217500     .                                                                    
217600     SKIP2                                                                
217700 IMS-REPL-RETA3     SECTION.                                              
217800                                                                          
217900     MOVE '  ' TO GODK-STATUSKODER                                        
218000     CALL CBLTDLI USING REPL RETA3-PCB DLI-IO-A301                        
218100     MOVE RETA3-STATUS-CODE TO STATUS-WS                                  
218200     PERFORM IMS-STATUSKONTROLL                                           
218300     .                                                                    
218400     SKIP2                                                                
218500 IMS-GHU-RETA4-FSEQ SECTION.                                              
218600                                                                          
218700     STRING 'WLRETA01(WDA3FSEQ>=' W-WDA3FSEQ-MIN-X                        
218800                    '&WDA3FSEQ<=' W-WDA3FSEQ-MAX-X ')'                    
218900          DELIMITED BY SIZE INTO SSA1                                     
219000     MOVE '  GE' TO GODK-STATUSKODER                                      
219100     CALL CBLTDLI USING GHU RETA4-PCB DLI-IO-A301 SSA1                    
219200     MOVE RETA4-STATUS-CODE TO STATUS-WS                                  
219300     PERFORM IMS-STATUSKONTROLL                                           
219400     .                                                                    
219500     SKIP2                                                                
219600 IMS-GHN-RETA4-FSEQ SECTION.                                              
219700                                                                          
219800     STRING 'WLRETA01(WDA3FSEQ>=' W-WDA3FSEQ-MIN-X                        
219900                    '&WDA3FSEQ<=' W-WDA3FSEQ-MAX-X ')'                    
220000          DELIMITED BY SIZE INTO SSA1                                     
220100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
220200     CALL CBLTDLI USING GHN RETA4-PCB DLI-IO-A301 SSA1                    
220300     MOVE RETA4-STATUS-CODE TO STATUS-WS                                  
220400     PERFORM IMS-STATUSKONTROLL                                           
220500     .                                                                    
220600     SKIP2                                                                
220700 IMS-REPL-RETA4     SECTION.                                              
220800                                                                          
220900     MOVE '  ' TO GODK-STATUSKODER                                        
221000     CALL CBLTDLI USING REPL RETA4-PCB DLI-IO-A301                        
221100     MOVE RETA4-STATUS-CODE TO STATUS-WS                                  
221200     PERFORM IMS-STATUSKONTROLL                                           
221300     .                                                                    
221400     SKIP2                                                                
221500 IMS-STATUSKONTROLL SECTION.                                              
221600                                                                          
221700     SET STATUS-IX TO 1                                                   
221800     SEARCH GODK-STATUS                                                   
221900       AT END                                                             
222000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
222100         DELIMITED BY SIZE INTO FELTEXT                                   
222200         CALL FELLOG                                                      
222300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
222400         CONTINUE                                                         
222500     END-SEARCH                                                           
222600     .                                                                    
