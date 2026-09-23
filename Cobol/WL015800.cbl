000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     WL015800.                                                
000400 AUTHOR.         TAPAS KUMAR GHOSH.                                       
000500 DATE-WRITTEN.   2004/09/17.                                              
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        SKAPAR SÄNDNING MED VALDA KOLLIN. VISAR ÄVEN SÄNDNINGS-          
001000*        INNEHÅLL.                                                        
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WLRETA (WDA3)                              
001300*        PROGRAMMET UPPDATERAR WL4111 (WDR1)                              
001400*        PROGRAMMET LÄSER      WLRETD (WDA3)                              
001500*        PROGRAMMET LÄSER      WLRETF (WDA3E1)                            
001600*        PROGRAMMET LÄSER      WDA3G1                                     
001700*        PROGRAMMET LÄSER      WDA3B1                                     
001800*                                                                         
001900*        WL015800 PROGRAM IS A REPLICA OF W4074300 PROGRAM                
002000*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
002100*                                                                         
002200*                                                                         
002300*    ADDRESS: 'CARPARTS.LDC.RETURNRELEASE'                                
002400*                                                                         
002500*    E-TRACKER: 4230251 2007-01    SUSANNE OLSSON                         
002600*    E'TRACKER:10143271 2011-09    CHINA WAREHOUSE PROJECT-1              
002700*                                                                         
002800*    INDATA.                                                              
002900*        TRANSAKTION: WL0158U                                             
003000*        REQUEST:     WZ01REQU                                            
003100*                     WL0158I1                                            
003200*                                                                         
003300*    UTDATA.                                                              
003400*        RESPONSE:    WZ01RESP                                            
003500*                     WL0158O1                                            
003600                                                                          
003700     SKIP3                                                                
003800 ENVIRONMENT DIVISION.                                                    
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300*    -- CHECKED BY WY2000                                                 
004400 77  IDPGM                       PIC X(08)   VALUE 'WL015800'.            
004500                                                                          
004600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004700 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004800 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004900 77  KDRC-DISPLAY                PIC Z(5)   VALUE ZERO.                   
005000                                                                          
005100 77  JA                          PIC X       VALUE 'J'.                   
005200 77  YES                         PIC X       VALUE 'Y'.                   
005300 77  NEJ                         PIC X       VALUE 'N'.                   
005400 77  WS-CDC-SE                   PIC X(2)    VALUE '11'.                  
005500 77  W-DELETE                    PIC X(03)   VALUE 'DEL'.                 
005600 77  W-LASTA                     PIC X(03)   VALUE 'LOA'.                 
005700 77  W-SELECT                    PIC X(03)   VALUE 'SEL'.                 
005800 77  W-DATUM                     PIC 9(8)    VALUE ZERO.                  
005900 77  W-DATUM-6                   PIC 9(6)    VALUE ZERO.                  
006000 77  ANTAL-REPL                  PIC S9(3)  COMP-3 VALUE ZERO.            
006100 77  MAX-ANTAL-REPL              PIC S9(3)  COMP-3 VALUE +200.            
006200 77  WS-IDKOLLI                  PIC 9(5)    VALUE ZERO.                  
006300 77  WS-TIKLOCK                  PIC 9(8)    VALUE ZERO.                  
006400 77  W-ANTAL-RADER-RET1          PIC S9(3)   VALUE +0 COMP-3.             
006500 77  W-ANTAL-RADER-RET2          PIC S9(3)   VALUE +0 COMP-3.             
006600 77  W-ANTAL-RADER-TRANSIT       PIC S9(3)   VALUE +0 COMP-3.             
006700 77  W-SPAR-IDDC-RET             PIC X(2)    VALUE SPACE.                 
006800                                                                          
006900 77  WS-IDELMT-ERROR             PIC X(16).                               
007000 77  WS-IDMSG-ERROR              PIC X(03).                               
007100 77  WS-IDMSG-INFO               PIC X(03).                               
007200                                                                          
007300*    --- INDEX FÖR BLÄDDRINGSRADER                                        
007400 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
007500 77  WS-INDX-REC                 PIC S9(4)  VALUE +0    COMP SYNC.        
007600 77  WS-COUNT                    PIC S9(4)  VALUE +0    COMP SYNC.        
007700 77  MAX-INDX                    PIC S9(4)  VALUE +500  COMP SYNC.        
007800 77  K-IX                        PIC S9(4)  VALUE +0    COMP SYNC.        
007900*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
008000                                                                          
008100 77  INDATA-SW                   PIC X       VALUE 'J'.                   
008200     88  INDATA-OK                           VALUE 'J'.                   
008300     88  INDATA-FEL                          VALUE 'N'.                   
008400                                                                          
008500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
008600     88  NYCKLAR-OK                          VALUE 'J'.                   
008700     88  NYCKLAR-FEL                         VALUE 'N'.                   
008800                                                                          
008900 77  KDCMD-VAL-SW                PIC X       VALUE 'J'.                   
009000     88  KDCMD-VAL-OK                        VALUE 'J'.                   
009100     88  KDCMD-VAL-FEL                       VALUE 'N'.                   
009200                                                                          
009300 77  OMSTART-SW                  PIC X       VALUE 'N'.                   
009400     88  OMSTART                             VALUE 'J'.                   
009500                                                                          
009600 77  RADER-KVAR-SW               PIC X       VALUE 'J'.                   
009700     88  RADER-KVAR                          VALUE 'J'.                   
009800     88  INGA-RADER-KVAR                     VALUE 'N'.                   
009900     EJECT                                                                
010000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
010200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010300     EJECT                                                                
010400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
010500 01  GENERELLA-SUBPROGRAM.                                                
010600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
010700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
011000     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
011100     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
011200     EJECT                                                                
011300                                                                          
011400 01  ABENDKODER.                                                          
011500     03  FILLER                  PIC X(16) VALUE 'ABENDKODER'.            
011600     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4) COMP SYNC VALUE   +16.         
011700     03  RKOD-ABEND-MED-DUMP     PIC S9(4) COMP SYNC VALUE   +33.         
011800     03  RKOD-FELTEXT            PIC X(32) VALUE SPACE.                   
011900                                                                          
012000*    --- PARAMETRAR TILL SUBPROGRAM W4074310                              
012100*01 -COPY W4074310                                                        
012200     EJECT                                                                
012300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
012400*01 -COPY WMEDAREA                                                        
012500     SKIP3                                                                
012600 01  MESSAGE-CODES.                                                       
012700     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
012800     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
012900     03  ERR-OTILL-UPD           PIC X(3)    VALUE '007'.                 
013000     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
013100     03  ERR-OTILL-STATUS        PIC X(3)    VALUE '079'.                 
013200     03  ERR-FLERA-FUNKTIONER    PIC X(3)    VALUE '097'.                 
013300     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
013400     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
013500     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
013600     03  ERR-REDAN-LASTAD        PIC X(3)    VALUE '310'.                 
013700     03  ERR-EJ-LASTAD           PIC X(3)    VALUE '311'.                 
013800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
013900     03  ERR-KOLLI-SAKNAS        PIC X(3)    VALUE '758'.                 
014000     EJECT                                                                
014100     SKIP3                                                                
014200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
014300*                                                                         
014400     EJECT                                                                
014500 01  FILLER                      PIC X(16)   VALUE 'WZ01SUB '.            
014600*01  -COPY WZ01SUB                                                        
014700     EJECT                                                                
014800 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
014900*01  -COPY WZ01SEND                                                       
015000     EJECT                                                                
015100*    - SEND AREA FOR RESTARTING THIS PROGRAM                              
015200 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
015300 01  SEND-AREA.                                                           
015400*    03  -COPY WZ01REQU -PRE SEND-                                        
015500*    03  -COPY WL0158I1 -PRE SEND-                                        
015600     EJECT                                                                
015700 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
015800 01  REQU-AREA.                                                           
015900*    03  -COPY WZ01REQU                                                   
016000*    03  -COPY WL0158I1                                                   
016100     EJECT                                                                
016200 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
016300 01  RESP-AREA.                                                           
016400*    03  -COPY WZ01RESP                                                   
016500*    03  -COPY WL0158O1                                                   
016600     EJECT                                                                
016700 77  SW-VISA-SND                 PIC X       VALUE 'N'.                   
016800     88  VISA-SND                            VALUE 'J'.                   
016900                                                                          
017000 77  SW-RAD-CMD                  PIC X       VALUE 'N'.                   
017100     88  RAD-CMD                             VALUE 'J'.                   
017200                                                                          
017300 77  SW-SKAPA-SNDDOK             PIC X       VALUE 'N'.                   
017400     88  SKAPA-SNDDOK                        VALUE 'J'.                   
017500                                                                          
017600 77  SW-SKAPA-NY-SND             PIC X       VALUE 'N'.                   
017700     88  SKAPA-NY-SND                        VALUE 'J'.                   
017800                                                                          
017900 77  WS-REC-LIMIT                PIC X       VALUE 'N'.                   
018000     88  REC-LIMIT                           VALUE 'J'.                   
018100                                                                          
018200 77  W-UPDATE-SW                 PIC X       VALUE 'N'.                   
018300     88  W-UPDATE-OK                         VALUE 'J'.                   
018400                                                                          
018500                                                                          
018600 77  W-IDRTLOP-NUM               PIC  9(3)   VALUE ZERO.                  
018700 77  W-FLVISA                    PIC X       VALUE 'N'.                   
018800 77  W-KLI-PACKAT                PIC S9(1)   VALUE +1   COMP-3.           
018900 77  W-KLI-LASTAT                PIC S9(1)   VALUE +2   COMP-3.           
019000 77  W-KLI-SAENT                 PIC S9(1)   VALUE +3   COMP-3.           
019100 77  W-SND-TERM                  PIC  X(1)   VALUE '1'.                   
019200 77  W-SND-TRANSIT               PIC  X(1)   VALUE '7'.                   
019300 77  W-SND-SAENT                 PIC  X(1)   VALUE '2'.                   
019400 77  W-SPAR-IDDISTR              PIC S9(5)   VALUE ZERO COMP-3.           
019500 77  W-SPAR-IDKUNDNR             PIC S9(7)   VALUE ZERO COMP-3.           
019600 77  W-SPAR-IDRAPPNR             PIC  9(7)   VALUE ZERO.                  
019700 77  W-FEL-IDKOLLI               PIC  9(5)   VALUE ZERO.                  
019800 77  W-SPAR-IDKOLLI              PIC S9(5)   VALUE ZERO COMP-3.           
019900 77  W-SPAR-IDRT                 PIC  X(3)   VALUE SPACE.                 
020000 77  W-SPAR-IDRTLOP              PIC  9(3)   VALUE ZERO.                  
020100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
020200*                                                                         
020300     EJECT                                                                
020400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
020500     SKIP3                                                                
020600 01  NYCKLAR-TILL-DLI.                                                    
020700     03  W-WDA301KY-X.                                                    
020800         05  W-IDDC              PIC  X(2)          VALUE SPACE.          
020900         05  W-DAREGDAT          PIC  9(8)          VALUE ZERO.           
021000         05  W-TIKLOCK           PIC S9(9)   COMP-3 VALUE ZERO.           
021100                                                                          
021200     03  W-WDA3B1-MIN-X.                                                  
021300         05  W-IDRT-B1-MIN       PIC  X(3)          VALUE SPACE.          
021400         05  W-IDDC-B1-MIN       PIC  X(2)          VALUE SPACE.          
021500         05  W-IDRTLOP-B1-MIN    PIC  9(3)          VALUE ZERO.           
021600         05  W-IDKOLLI-B1-MIN    PIC S9(5)   COMP-3 VALUE ZERO.           
021700         05  W-DAREGDAT-B1-MIN   PIC  9(8)          VALUE ZERO.           
021800         05  W-TIKLOCK-B1-MIN    PIC S9(9)   COMP-3 VALUE ZERO.           
021900                                                                          
022000     03  W-WDA3B1-MAX-X.                                                  
022100         05  W-IDRT-B1-MAX       PIC  X(3)          VALUE SPACE.          
022200         05  W-IDDC-B1-MAX       PIC  X(2)          VALUE SPACE.          
022300         05  W-IDRTLOP-B1-MAX    PIC  9(3)          VALUE ZERO.           
022400         05  W-IDKOLLI-B1-MAX    PIC S9(5)   COMP-3 VALUE ZERO.           
022500         05  W-DAREGDAT-B1-MAX   PIC  9(8)          VALUE ZERO.           
022600         05  W-TIKLOCK-B1-MAX    PIC S9(9)   COMP-3 VALUE ZERO.           
022700                                                                          
022800     03  W-WDA3BSEQ-X.                                                    
022900         05  W-IDRT-BSEQ         PIC  X(3)          VALUE SPACE.          
023000         05  W-IDDC-BSEQ         PIC  X(2)          VALUE SPACE.          
023100         05  W-IDRTLOP-BSEQ      PIC  9(3)          VALUE ZERO.           
023200         05  W-IDKOLLI-BSEQ      PIC S9(5)   COMP-3 VALUE ZERO.           
023300                                                                          
023400     03  W-WDA3BSEQ-MIN-X.                                                
023500         05  W-IDRT-BSEQ-MIN     PIC  X(3)          VALUE SPACE.          
023600         05  W-IDDC-BSEQ-MIN     PIC  X(2)          VALUE SPACE.          
023700         05  W-IDRTLOP-BSEQ-MIN  PIC  9(3)          VALUE ZERO.           
023800         05  W-IDKOLLI-BSEQ-MIN  PIC S9(5)   COMP-3 VALUE ZERO.           
023900                                                                          
024000     03  W-WDA3BSEQ-MAX-X.                                                
024100         05  W-IDRT-BSEQ-MAX     PIC  X(3)          VALUE SPACE.          
024200         05  W-IDDC-BSEQ-MAX     PIC  X(2)          VALUE SPACE.          
024300         05  W-IDRTLOP-BSEQ-MAX  PIC  9(3)          VALUE ZERO.           
024400         05  W-IDKOLLI-BSEQ-MAX  PIC S9(5)   COMP-3 VALUE ZERO.           
024500                                                                          
024600     03  W-WDA3C1KY-MIN-X.                                                
024700         05  W-IDDC-C1-MIN       PIC  X(2)          VALUE SPACE.          
024800         05  W-KDRETSTA-C1-MIN   PIC  X(1)          VALUE SPACE.          
024900         05  W-DARETANK-C1-MIN   PIC  9(8)          VALUE ZERO.           
025000         05  W-IDRT-C1-MIN       PIC  X(3)          VALUE SPACE.          
025100         05  W-IDRTLOP-C1-MIN    PIC  9(3)          VALUE ZERO.           
025200         05  W-IDDISTR-C1-MIN    PIC S9(5)   COMP-3 VALUE ZERO.           
025300         05  W-IDKUNDNR-C1-MIN   PIC S9(7)   COMP-3 VALUE ZERO.           
025400         05  W-IDRAPPNR-C1-MIN   PIC  9(7)          VALUE ZERO.           
025500         05  W-DAREGDAT-C1-MIN   PIC  9(8)          VALUE ZERO.           
025600         05  W-TIKLOCK-C1-MIN    PIC S9(9)   COMP-3 VALUE ZERO.           
025700                                                                          
025800     03  W-WDA3C1KY-MAX-X.                                                
025900         05  W-IDDC-C1-MAX       PIC  X(2)          VALUE SPACE.          
026000         05  W-KDRETSTA-C1-MAX   PIC  X(1)          VALUE SPACE.          
026100         05  W-DARETANK-C1-MAX   PIC  9(8)          VALUE ZERO.           
026200         05  W-IDRT-C1-MAX       PIC  X(3)          VALUE SPACE.          
026300         05  W-IDRTLOP-C1-MAX    PIC  9(3)          VALUE ZERO.           
026400         05  W-IDDISTR-C1-MAX    PIC S9(5)   COMP-3 VALUE ZERO.           
026500         05  W-IDKUNDNR-C1-MAX   PIC S9(7)   COMP-3 VALUE ZERO.           
026600         05  W-IDRAPPNR-C1-MAX   PIC  9(7)          VALUE ZERO.           
026700         05  W-DAREGDAT-C1-MAX   PIC  9(8)          VALUE ZERO.           
026800         05  W-TIKLOCK-C1-MAX    PIC S9(9)   COMP-3 VALUE ZERO.           
026900                                                                          
027000     03  W-WDA3E1KY-MIN-X.                                                
027100         05  W-IDDC-E1-MIN       PIC  X(2)          VALUE SPACE.          
027200         05  W-IDRT-E1-MIN       PIC  X(3)          VALUE SPACE.          
027300         05  W-IDDISTR-E1-MIN    PIC S9(5)   COMP-3 VALUE ZERO.           
027400         05  W-IDKUNDNR-E1-MIN   PIC S9(7)   COMP-3 VALUE ZERO.           
027500         05  W-IDRAPPNR-E1-MIN   PIC  9(7)          VALUE ZERO.           
027600         05  W-DAREGDAT-E1-MIN   PIC  9(8)          VALUE ZERO.           
027700         05  W-TIKLOCK-E1-MIN    PIC S9(9)   COMP-3 VALUE ZERO.           
027800                                                                          
027900     03  W-WDA3E1KY-MAX-X.                                                
028000         05  W-IDDC-E1-MAX       PIC  X(2)          VALUE SPACE.          
028100         05  W-IDRT-E1-MAX       PIC  X(3)          VALUE SPACE.          
028200         05  W-IDDISTR-E1-MAX    PIC S9(5)   COMP-3 VALUE ZERO.           
028300         05  W-IDKUNDNR-E1-MAX   PIC S9(7)   COMP-3 VALUE ZERO.           
028400         05  W-IDRAPPNR-E1-MAX   PIC  9(7)          VALUE ZERO.           
028500         05  W-DAREGDAT-E1-MAX   PIC  9(8)          VALUE ZERO.           
028600         05  W-TIKLOCK-E1-MAX    PIC S9(9)   COMP-3 VALUE ZERO.           
028700                                                                          
028800     03  W-WDA3F1KY-MIN-X.                                                
028900         05  W-IDDC-F1-MIN       PIC  X(2)          VALUE SPACE.          
029000         05  W-IDDISTR-F1-MIN    PIC S9(5)   COMP-3 VALUE ZERO.           
029100         05  W-IDKUNDNR-F1-MIN   PIC S9(7)   COMP-3 VALUE ZERO.           
029200         05  W-IDRAPPNR-F1-MIN   PIC  9(7)          VALUE ZERO.           
029300         05  FILLER              PIC  X(22)  VALUE LOW-VALUE.             
029400                                                                          
029500     03  W-WDA3F1KY-MAX-X.                                                
029600         05  W-IDDC-F1-MAX       PIC  X(2)          VALUE SPACE.          
029700         05  W-IDDISTR-F1-MAX    PIC S9(5)   COMP-3 VALUE ZERO.           
029800         05  W-IDKUNDNR-F1-MAX   PIC S9(7)   COMP-3 VALUE ZERO.           
029900         05  W-IDRAPPNR-F1-MAX   PIC  9(7)          VALUE ZERO.           
030000         05  FILLER              PIC  X(22)  VALUE HIGH-VALUE.            
030100                                                                          
030200     03  W-WDA3G1KY-MIN-X.                                                
030300         05  W-IDDC-G1-MIN       PIC  X(2)   VALUE SPACE.                 
030400         05  FILLER              PIC X(41)   VALUE LOW-VALUE.             
030500                                                                          
030600     03  W-WDA3G1KY-MAX-X.                                                
030700         05  W-IDDC-G1-MAX       PIC  X(2)   VALUE SPACE.                 
030800         05  FILLER              PIC X(41)   VALUE HIGH-VALUE.            
030900                                                                          
031000     03  W-WDGXKEY-X.                                                     
031100         05  W-IDHTYP            PIC  X(4)   VALUE '4111'.                
031200         05  W-IDRT-4111         PIC  X(3)   VALUE SPACE.                 
031300         05  FILLER              PIC X(23)   VALUE LOW-VALUE.             
031400                                                                          
031500                                                                          
031600     03  W-DAREGDAT-A3-X.                                                 
031700         05  W-DAREGDAT-A3       PIC  9(8)   VALUE ZERO.                  
031800                                                                          
031900     03  W-TIKLOCK-A3-X.                                                  
032000         05  W-TIKLOCK-A3        PIC S9(9)   COMP-3 VALUE ZERO.           
032100                                                                          
032200     03  W-IDRTLOP-B1-X.                                                  
032300         05  W-IDRTLOP-B1        PIC  9(3)   VALUE ZERO.                  
032400                                                                          
032500     03  W-IDRT-TRANSIT-X.                                                
032600         05  W-IDRT-TRANSIT      PIC  X(3)   VALUE SPACE.                 
032700                                                                          
032800     03  W-IDKOLLI-B1-X.                                                  
032900         05  W-IDKOLLI-B1        PIC S9(5)   COMP-3 VALUE ZERO.           
033000                                                                          
033100     03  W-KDRETSTA-G1-X.                                                 
033200         05  W-KDRETSTA-G1       PIC  X(1)   VALUE ZERO.                  
033300                                                                          
033400     SKIP2                                                                
033500*    --- STATUS-KOD FRÅN IMS                                              
033600 01  STATUS-WS                   PIC XX.                                  
033700     88  SEGMENT-FINNS                       VALUE '  '.                  
033800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
033900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
034000     88  SEGMENT-SLUT                        VALUE 'GB'.                  
034100     SKIP2                                                                
034200 01  GODK-STATUSKODER.                                                    
034300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
034400     SKIP3                                                                
034500 01  SSA1                        PIC X(256).                              
034600 01  SSA2                        PIC X(128).                              
034700     EJECT                                                                
034800*    --- IMS FUNKTIONSKODER                                               
034900*01  -COPY W0003                                                          
035000     EJECT                                                                
035100*    ---  DLI INPUT-OUTPUT AREA                                           
035200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
035300     SKIP3                                                                
035400 01  DLI-IO-AREA.                                                         
035500     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
035600     SKIP3                                                                
035700     03  WLRETA01 REDEFINES IO-AREA.                                      
035800*        05  -COPY WDA301                                                 
035900     EJECT                                                                
036000     03  WLRETC01 REDEFINES IO-AREA.                                      
036100*        05  -COPY WDA3B1                                                 
036200     EJECT                                                                
036300     03  WLRETD01 REDEFINES IO-AREA.                                      
036400*        05  -COPY WDA3C1                                                 
036500     EJECT                                                                
036600     03  WLRETF01 REDEFINES IO-AREA.                                      
036700*        05  -COPY WDA3E1                                                 
036800     EJECT                                                                
036900     03  WLRETG01 REDEFINES IO-AREA.                                      
037000*        05  -COPY WDA3F1                                                 
037100     EJECT                                                                
037200     SKIP3                                                                
037300 01  FILLER                    PIC X(16) VALUE 'WDGX4111-AREA'.           
037400 01  WL411101  -COPY WDGX4111                                             
037500     EJECT                                                                
037600 01  FILLER                    PIC X(16) VALUE 'WDGX4112-AREA'.           
037700 01  WL411111  -COPY WDGX4112                                             
037800     EJECT                                                                
037900 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDA3G1'.         
038000 01  DLI-IO-WDA3G1.                                                       
038100*    03  -COPY WDA3G1                                                     
038200     EJECT                                                                
038300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDA3B1'.         
038400 01  DLI-IO-WDA3B1.                                                       
038500*    03  -COPY WDA3B1   -PRE RET-                                         
038600     EJECT                                                                
038700                                                                          
038800*01  -COPY WWDC99                                                         
038900     EJECT                                                                
039000*01  -COPY WWDCKONS                                                       
039100     EJECT                                                                
039200 LINKAGE SECTION.                                                         
039300                                                                          
039400 01  MSG-PCB                     PIC X.                                   
039500*01  -COPY W0009  -PRE RETURNRE-                                          
039600     EJECT                                                                
039700*01  -COPY W0008  -PRE RETA-                                              
039800     05  FILLER                  PIC X.                                   
039900     EJECT                                                                
040000*01  -COPY W0008  -PRE RETD-                                              
040100     05  FILLER                  PIC X.                                   
040200     EJECT                                                                
040300*01  -COPY W0008  -PRE RETF-                                              
040400     05  FILLER                  PIC X.                                   
040500     EJECT                                                                
040600*01  -COPY W0008  -PRE RETG-                                              
040700     05  FILLER                  PIC X.                                   
040800     EJECT                                                                
040900*01  -COPY W0008  -PRE 4111-                                              
041000     05  FILLER                  PIC X.                                   
041100     EJECT                                                                
041200*01  -COPY W0008  -PRE WDA3G-                                             
041300     05  FILLER                  PIC X.                                   
041400     EJECT                                                                
041500*01  -COPY W0008  -PRE WDA3B-                                             
041600     05  FILLER                  PIC X.                                   
041700     EJECT                                                                
041800*01  -COPY W0008  -PRE WDA3-                                              
041900     05  FILLER                  PIC X.                                   
042000     EJECT                                                                
042100 PROCEDURE DIVISION  USING MSG-PCB  RETURNRE-PCB                          
042200                           RETA-PCB RETD-PCB RETF-PCB                     
042300                           RETG-PCB 4111-PCB WDA3G-PCB WDA3B-PCB          
042400                           WDA3-PCB.                                      
042500     ENTRY 'DLITCBL' USING MSG-PCB  RETURNRE-PCB                          
042600                           RETA-PCB RETD-PCB RETF-PCB                     
042700                           RETG-PCB 4111-PCB WDA3G-PCB WDA3B-PCB          
042800                           WDA3-PCB.                                      
042900                                                                          
043000     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
043100     IF SUB-KDRC = 0                                                      
043200       PERFORM A-INIT                                                     
043300       PERFORM B-KOLLA-NYCKLAR                                            
043400       IF NYCKLAR-OK                                                      
043500         IF REQU-KDPGMACT = 'C'                                           
043600            PERFORM C-RAKNA-IDDC-RADER                                    
043700         ELSE                                                             
043800           IF REQU-KDPGMACT = 'E' OR 'N'                                  
043900             PERFORM G-KOLLA-INPUT                                        
044000             IF INDATA-OK                                                 
044100               PERFORM H-UPPDATERA                                        
044200                                                                          
044300               IF ANTAL-REPL = MAX-ANTAL-REPL AND RADER-KVAR              
044400                  MOVE JA                  TO OMSTART-SW                  
044500                  MOVE RET-IDDC            TO REQU-IDDC-SPAR              
044600                  MOVE RET-IDRT            TO REQU-IDRT-SPAR              
044700                  MOVE RET-IDRTLOP         TO REQU-IDRTLOP-SPAR           
044800                  MOVE RET-IDKOLLI         TO WS-IDKOLLI                  
044900                  MOVE WS-IDKOLLI          TO REQU-IDKOLLI-SPAR           
045000                  MOVE RET-DAREGDAT        TO REQU-DAREGDAT-SPAR          
045100                  MOVE RET-TIKLOCK         TO WS-TIKLOCK                  
045200                  MOVE WS-TIKLOCK          TO REQU-TIKLOCK-SPAR           
045300               ELSE                                                       
045400                  MOVE NEJ                 TO OMSTART-SW                  
045500               END-IF                                                     
045600                                                                          
045700             END-IF                                                       
045800           END-IF                                                         
045900           IF OMSTART                                                     
046000             CONTINUE                                                     
046100           ELSE                                                           
046200             IF REQU-KDPGMACT = 'S'                                       
046300               IF REQU-IDDC-RET = 'TR'                                    
046400                 PERFORM I-LAES-VISA-TRANSIT                              
046500               ELSE                                                       
046600                 PERFORM F-LAES-VISA-INFO                                 
046700               END-IF                                                     
046800             END-IF                                                       
046900                                                                          
047000             IF REQU-KDPGMACT = 'N'                                       
047100                 PERFORM F-LAES-VISA-INFO                                 
047200             END-IF                                                       
047300           END-IF                                                         
047400         END-IF                                                           
047500       END-IF                                                             
047600       IF OMSTART                                                         
047700         MOVE REQU-AREA     TO SEND-AREA                                  
047800         PERFORM S03-SEND-TO-RESTART-THIS-PGM                             
047900       ELSE                                                               
048000                                                                          
048100         MOVE RESP-IDMSG-INFO    TO WS-IDMSG-INFO                         
048200         MOVE RESP-IDMSG-ERROR   TO WS-IDMSG-ERROR                        
048300         MOVE RESP-IDELMT-ERROR  TO WS-IDELMT-ERROR                       
048400         IF WS-IDMSG-INFO NOT = SPACE                                     
048500            MOVE SPACE            TO RESP-IDMSG-ERROR                     
048600            MOVE SPACE            TO RESP-IDELMT-ERROR                    
048700         ELSE                                                             
048800           IF WS-IDMSG-ERROR NOT = SPACE                                  
048900              MOVE ALL '+' TO RESP-WL0158O1 (1:41)                        
049000              MOVE WS-IDMSG-ERROR   TO RESP-IDMSG-ERROR                   
049100              MOVE WS-IDELMT-ERROR  TO RESP-IDELMT-ERROR                  
049200              MOVE WS-IDMSG-INFO    TO RESP-IDMSG-INFO                    
049300              MOVE  001             TO RESP-IDMSGVER                      
049400              IF  REQU-KDPGMACT = 'S'                                     
049500                MOVE ZERO             TO RESP-KVRADER                     
049600              ELSE                                                        
049700                IF REQU-KVRADER NUMERIC                                   
049800                  MOVE REQU-KVRADER     TO RESP-KVRADER                   
049900                ELSE                                                      
050000                  MOVE ZERO             TO RESP-KVRADER                   
050100                END-IF                                                    
050200              END-IF                                                      
050300           END-IF                                                         
050400         END-IF                                                           
050500         PERFORM S02-RETURN-RESPONSE                                      
050600       END-IF                                                             
050700     END-IF                                                               
050800                                                                          
050900     MOVE ZERO TO RETURN-CODE                                             
051000     GOBACK                                                               
051100     .                                                                    
051200     EJECT                                                                
051300 A-INIT SECTION.                                                          
051400                                                                          
051500     MOVE ALL '+' TO RESP-AREA                                            
051600     MOVE SPACE   TO RESP-IDMSG-INFO                                      
051700                     RESP-IDMSG-ERROR                                     
051800                     RESP-IDELMT-ERROR                                    
051900     MOVE 001     TO RESP-IDMSGVER                                        
052000     MOVE ZERO    TO RESP-KVRADER                                         
052100                                                                          
052200                                                                          
052300     MOVE FUNCTION CURRENT-DATE (1:8) TO W-DATUM                          
052400     MOVE FUNCTION CURRENT-DATE (3:6) TO W-DATUM-6                        
052500                                                                          
052600     MOVE LOW-VALUE         TO W-WDA3BSEQ-MIN-X                           
052700                               W-WDA3C1KY-MIN-X                           
052800                               W-WDA3E1KY-MIN-X                           
052900                               W-WDA3G1KY-MIN-X                           
053000                                                                          
053100     MOVE HIGH-VALUE        TO W-WDA3BSEQ-MAX-X                           
053200                               W-WDA3C1KY-MAX-X                           
053300                               W-WDA3E1KY-MAX-X                           
053400                               W-WDA3G1KY-MAX-X                           
053500                                                                          
053600     MOVE NEJ               TO OMSTART-SW                                 
053700     MOVE REQU-IDDC-RET     TO RESP-IDDC-RET                              
053800     .                                                                    
053900     EJECT                                                                
054000 B-KOLLA-NYCKLAR SECTION.                                                 
054100                                                                          
054200                                                                          
054300     MOVE JA TO NYCKLAR-SW                                                
054400                                                                          
054500     MOVE REQU-IDRT-KEY    TO W-IDRT-4111                                 
054600     MOVE REQU-IDDC-KEY    TO WS-IDDC                                     
054700     PERFORM IMS-GU-WL411101                                              
054800     IF SEGMENT-FINNS                                                     
054900       MOVE REQU-IDRT-KEY  TO W-IDRT-BSEQ-MIN                             
055000                              W-IDRT-BSEQ-MAX                             
055100                              W-IDRT-C1-MIN                               
055200                              W-IDRT-C1-MAX                               
055300                              W-IDRT-E1-MIN                               
055400                              W-IDRT-E1-MAX                               
055500     ELSE                                                                 
055600       MOVE NEJ            TO NYCKLAR-SW                                  
055700       MOVE 'IDRT'         TO RESP-IDELMT-ERROR                           
055800       MOVE '023'          TO RESP-IDMSG-ERROR                            
055900     END-IF                                                               
056000                                                                          
056100*    -- KONTROLL AV FLAGGA VISA SÄNDNINGSINNEHÅLL                         
056200     IF REQU-FLVISA-KEY      = ALL '+'                                    
056300       CONTINUE                                                           
056400     ELSE                                                                 
056500       MOVE REQU-FLVISA-KEY   TO W-FLVISA                                 
056600     END-IF                                                               
056700                                                                          
056800     IF W-FLVISA          = JA OR YES                                     
056900        MOVE JA           TO SW-VISA-SND                                  
057000     ELSE                                                                 
057100        MOVE NEJ          TO SW-VISA-SND                                  
057200                             W-FLVISA                                     
057300     END-IF                                                               
057400                                                                          
057500     IF VISA-SND                                                          
057600       IF REQU-IDDC-RET = 'TR' OR REQU-IDRTLOP-KEY = ALL '+'              
057700         PERFORM BA-KOLLA-VAL                                             
057800       ELSE                                                               
057900         IF REQU-IDRTLOP-KEY NOT NUMERIC                                  
058000            MOVE NEJ          TO NYCKLAR-SW                               
058100            MOVE 'IDRTLOP'    TO RESP-IDELMT-ERROR                        
058200            MOVE '024'        TO RESP-IDMSG-ERROR                         
058300         END-IF                                                           
058400         IF REQU-KDPGMACT = 'E'                                           
058500           CONTINUE                                                       
058600         ELSE                                                             
058700           IF REQU-KDPGMACT = 'N' OR                                      
058800              REQU-FLSNDDOK = 'J'                                         
058900             MOVE NEJ          TO NYCKLAR-SW                              
059000             MOVE '032'        TO RESP-IDMSG-ERROR                        
059100           END-IF                                                         
059200         END-IF                                                           
059300       END-IF                                                             
059400     END-IF                                                               
059500                                                                          
059600     IF REQU-KDPGMACT = 'N' AND                                           
059700        REQU-FLSNDDOK = 'J'                                               
059800       MOVE NEJ          TO NYCKLAR-SW                                    
059900       MOVE '032'        TO RESP-IDMSG-ERROR                              
060000     END-IF                                                               
060100                                                                          
060200     IF REQU-KDPGMACT = 'E' AND                                           
060300        REQU-FLSNDDOK = 'J' AND                                           
060400        REQU-IDRTLOP-KEY = ALL '+'                                        
060500       IF REQU-IDDC-RET = 'TR'                                            
060600         CONTINUE                                                         
060700       ELSE                                                               
060800         MOVE NEJ          TO NYCKLAR-SW                                  
060900         MOVE '043'        TO RESP-IDMSG-ERROR                            
061000       END-IF                                                             
061100     END-IF                                                               
061200                                                                          
061300     IF NYCKLAR-OK                                                        
061400       IF REQU-IDRTLOP-KEY  NUMERIC                                       
061500          MOVE REQU-IDRTLOP-KEY  TO RESP-IDRTLOP-KEY                      
061600                                    W-IDRTLOP-NUM                         
061700                                    W-IDRTLOP-B1                          
061800       END-IF                                                             
061900       IF W-FLVISA = JA                                                   
062000           MOVE JA               TO RESP-FLVISA-KEY                       
062100       ELSE                                                               
062200         MOVE W-FLVISA           TO RESP-FLVISA-KEY                       
062300       END-IF                                                             
062400     END-IF                                                               
062500                                                                          
062600*    -- KONTROLL AV IDRT-KEY                                              
062700     IF REQU-IDRT-KEY = ALL '+'                                           
062800        MOVE NEJ            TO NYCKLAR-SW                                 
062900        MOVE 'IDRT'         TO RESP-IDELMT-ERROR                          
063000        MOVE '023'          TO RESP-IDMSG-ERROR                           
063100     END-IF                                                               
063200                                                                          
063300       IF REQU-OMSTART-NYCKLAR NOT = ALL '+'                              
063400                                                                          
063500         IF REQU-IDKOLLI-SPAR NUMERIC  AND                                
063600            REQU-IDKOLLI-SPAR > ZERO                                      
063700                                                                          
063800           MOVE REQU-IDKOLLI-SPAR      TO W-IDKOLLI-BSEQ                  
063900           MOVE REQU-IDDC-SPAR         TO W-IDDC-BSEQ                     
064000                                          W-IDDC-BSEQ-MIN                 
064100                                          W-IDDC-BSEQ-MAX                 
064200           MOVE REQU-IDRT-SPAR         TO W-IDRT-BSEQ                     
064300                                          W-IDRT-BSEQ-MIN                 
064400                                          W-IDRT-BSEQ-MAX                 
064500           MOVE REQU-IDRTLOP-SPAR      TO W-IDRTLOP-BSEQ                  
064600                                          W-IDRTLOP-BSEQ-MIN              
064700                                          W-IDRTLOP-BSEQ-MAX              
064800           MOVE REQU-IDKOLLI-SPAR      TO W-IDKOLLI-BSEQ                  
064900           MOVE REQU-DAREGDAT-SPAR     TO W-DAREGDAT-A3                   
065000           MOVE REQU-TIKLOCK-SPAR      TO W-TIKLOCK-A3                    
065100           MOVE JA                     TO OMSTART-SW                      
065200         END-IF                                                           
065300       END-IF                                                             
065400     .                                                                    
065500     EJECT                                                                
065600 BA-KOLLA-VAL       SECTION.                                              
065700                                                                          
065800     IF REQU-IDKOLLI(1)= ALL '+'                                          
065900       MOVE NEJ          TO NYCKLAR-SW                                    
066000     END-IF                                                               
066100                                                                          
066200     MOVE NEJ TO KDCMD-VAL-SW                                             
066300                                                                          
066400     MOVE +1 TO INDX                                                      
066500     PERFORM UNTIL INDX > MAX-INDX                                        
066600        IF REQU-KDCMD(INDX) = 'X'                                         
066700          MOVE JA TO KDCMD-VAL-SW                                         
066800          MOVE MAX-INDX TO INDX                                           
066900        END-IF                                                            
067000        ADD +1 TO INDX                                                    
067100     END-PERFORM                                                          
067200                                                                          
067300     IF KDCMD-VAL-FEL                                                     
067400       MOVE NEJ          TO NYCKLAR-SW                                    
067500       IF REQU-KDPGMACT = 'E'                                             
067600         MOVE '007'        TO RESP-IDMSG-ERROR                            
067700       ELSE                                                               
067800         MOVE '032'        TO RESP-IDMSG-ERROR                            
067900       END-IF                                                             
068000     END-IF                                                               
068100                                                                          
068200     IF REQU-IDRTLOP-KEY NOT = ALL '+'                                    
068300       MOVE NEJ          TO NYCKLAR-SW                                    
068400     END-IF                                                               
068500                                                                          
068600     IF REQU-KDPGMACT = 'N' OR                                            
068700        REQU-FLSNDDOK = 'J'                                               
068800       MOVE NEJ          TO NYCKLAR-SW                                    
068900       MOVE '032'        TO RESP-IDMSG-ERROR                              
069000     END-IF                                                               
069100                                                                          
069200     IF NYCKLAR-OK                                                        
069300       CONTINUE                                                           
069400     ELSE                                                                 
069500       IF RESP-IDMSG-ERROR = '032' OR '007'                               
069600         CONTINUE                                                         
069700       ELSE                                                               
069800         MOVE 'KEY'        TO RESP-IDELMT-ERROR                           
069900         MOVE '023'        TO RESP-IDMSG-ERROR                            
070000       END-IF                                                             
070100     END-IF                                                               
070200     .                                                                    
070300     EJECT                                                                
070400 C-RAKNA-IDDC-RADER SECTION.                                              
070500                                                                          
070600     MOVE SPACE       TO RESP-IDDC-RET1                                   
070700                         RESP-IDDC-RET2                                   
070800                         RESP-IDDC-TRANSIT                                
070900     MOVE ZERO        TO W-ANTAL-RADER-RET1                               
071000                         W-ANTAL-RADER-RET2                               
071100                         W-ANTAL-RADER-TRANSIT                            
071200     MOVE 'MOD-1-DC'  TO RESP-IDMOD-RADER                                 
071300                                                                          
071400     PERFORM CA-RAKNA-RADER-TRANSIT                                       
071500                                                                          
071600     MOVE ZERO              TO W-SPAR-IDKOLLI                             
071700     MOVE LOW-VALUE         TO W-WDA3B1-MIN-X                             
071800     MOVE HIGH-VALUE        TO W-WDA3B1-MAX-X                             
071900     MOVE REQU-IDRT-KEY     TO W-IDRT-B1-MIN                              
072000                               W-IDRT-B1-MAX                              
072100     MOVE ZERO              TO W-IDRTLOP-B1                               
072200                                                                          
072300     PERFORM IMS-GU-WDA3B1                                                
072400                                                                          
072500     IF SEGMENT-FINNS                                                     
072600        MOVE RET-SEQB-IDDC TO RESP-IDDC-RET1                              
072700        PERFORM UNTIL SEGMENT-SAKNAS  OR SEGMENT-SLUT OR                  
072800                 RET-SEQB-IDDC NOT = RESP-IDDC-RET1                       
072900                                                                          
073000          IF RET-SEQB-IDKOLLI = W-SPAR-IDKOLLI OR                         
073100             RET-SEQB-IDKOLLI = ZERO                                      
073200             CONTINUE                                                     
073300          ELSE                                                            
073400             MOVE RET-SEQB-IDDC TO RESP-IDDC-RET1                         
073500             ADD +1 TO W-ANTAL-RADER-RET1                                 
073600             MOVE RET-SEQB-IDKOLLI TO W-SPAR-IDKOLLI                      
073700          END-IF                                                          
073800                                                                          
073900          PERFORM IMS-GN-WDA3B1                                           
074000        END-PERFORM                                                       
074100                                                                          
074200        IF W-ANTAL-RADER-RET1 = 0                                         
074300          MOVE SPACE TO RESP-IDDC-RET1                                    
074400        END-IF                                                            
074500                                                                          
074600        MOVE ZERO     TO W-SPAR-IDKOLLI                                   
074700        IF SEGMENT-FINNS                                                  
074800           MOVE RET-SEQB-IDDC TO RESP-IDDC-RET2                           
074900           PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                   
075000                                                                          
075100              IF RET-SEQB-IDKOLLI = W-SPAR-IDKOLLI OR                     
075200                 RET-SEQB-IDKOLLI = ZERO                                  
075300                 CONTINUE                                                 
075400              ELSE                                                        
075500                 MOVE RET-SEQB-IDDC TO RESP-IDDC-RET2                     
075600                 ADD +1 TO W-ANTAL-RADER-RET2                             
075700                 MOVE RET-SEQB-IDKOLLI TO W-SPAR-IDKOLLI                  
075800              END-IF                                                      
075900              PERFORM IMS-GN-WDA3B1                                       
076000                                                                          
076100           END-PERFORM                                                    
076200        END-IF                                                            
076300                                                                          
076400        IF W-ANTAL-RADER-RET2 = 0                                         
076500          MOVE SPACE TO RESP-IDDC-RET2                                    
076600        END-IF                                                            
076700                                                                          
076800        MOVE W-ANTAL-RADER-RET1 TO RESP-KVRADER-RET1                      
076900        MOVE W-ANTAL-RADER-RET2 TO RESP-KVRADER-RET2                      
077000     END-IF                                                               
077100                                                                          
077200     .                                                                    
077300     EJECT                                                                
077400 CA-RAKNA-RADER-TRANSIT SECTION.                                          
077500                                                                          
077600     IF NDC-CN OR LDC-CN                                                  
077700       MOVE WC-NDC-CN-71 TO W-IDDC-G1-MIN                                 
077800       MOVE WC-NDC-CN-74 TO W-IDDC-G1-MAX                                 
077900     ELSE                                                                 
078000       MOVE WS-CDC-SE    TO W-IDDC-G1-MIN                                 
078100                            W-IDDC-G1-MAX                                 
078200     END-IF                                                               
078300                                                                          
078400     MOVE '7'            TO W-KDRETSTA-G1                                 
078500                                                                          
078600     MOVE REQU-IDRT-KEY  TO W-IDRT-TRANSIT                                
078700                                                                          
078800     PERFORM IMS-GU-WDA3G1                                                
078900                                                                          
079000     IF SEGMENT-FINNS                                                     
079100       MOVE 'TR'         TO RESP-IDDC-TRANSIT                             
079200       MOVE SPACE        TO W-SPAR-IDRT                                   
079300       MOVE ZERO         TO W-SPAR-IDRTLOP                                
079400       PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                       
079500                                                                          
079600         IF SEQG-IDRT         =  W-SPAR-IDRT AND                          
079700            SEQG-IDRTLOP      =  W-SPAR-IDRTLOP                           
079800            CONTINUE                                                      
079900         ELSE                                                             
080000           ADD +1 TO W-ANTAL-RADER-TRANSIT                                
080100           MOVE SEQG-IDRT     TO W-SPAR-IDRT                              
080200           MOVE SEQG-IDRTLOP  TO W-SPAR-IDRTLOP                           
080300         END-IF                                                           
080400         PERFORM IMS-GN-WDA3G1                                            
080500                                                                          
080600       END-PERFORM                                                        
080700     END-IF                                                               
080800                                                                          
080900     MOVE W-ANTAL-RADER-TRANSIT TO RESP-KVRADER-TRANSIT                   
081000     .                                                                    
081100     EJECT                                                                
081200 F-LAES-VISA-INFO SECTION.                                                
081300                                                                          
081400     MOVE ZERO                 TO W-SPAR-IDKOLLI                          
081500     MOVE 'MOD-3-KO'           TO RESP-IDMOD-RADER                        
081600                                                                          
081700     MOVE HIGH-VALUE           TO W-WDA3BSEQ-MAX-X                        
081800                                                                          
081900     IF VISA-SND                                                          
082000        MOVE LOW-VALUE         TO W-WDA3B1-MIN-X                          
082100        MOVE HIGH-VALUE        TO W-WDA3B1-MAX-X                          
082200        MOVE REQU-IDRT-KEY     TO W-IDRT-B1-MIN                           
082300                                  W-IDRT-B1-MAX                           
082400     ELSE                                                                 
082500        MOVE REQU-IDRT-KEY     TO W-IDRT-BSEQ-MIN                         
082600                                  W-IDRT-BSEQ-MAX                         
082700        MOVE REQU-IDDC-RET     TO W-IDDC-BSEQ-MIN                         
082800                                  W-IDDC-BSEQ-MAX                         
082900        MOVE ZERO              TO W-IDRTLOP-BSEQ-MIN                      
083000                                  W-IDRTLOP-BSEQ-MAX                      
083100     END-IF                                                               
083200                                                                          
083300     IF VISA-SND                                                          
083400        PERFORM IMS-GU-WDA3B1                                             
083500        IF SEGMENT-FINNS                                                  
083600           PERFORM FA-LAES-WDA301                                         
083700        END-IF                                                            
083800     ELSE                                                                 
083900       PERFORM IMS-GHU-SEQB-WLRETA01                                      
084000     END-IF                                                               
084100                                                                          
084200     IF SEGMENT-SAKNAS                                                    
084300       IF REQU-IDRTLOP-KEY NOT = ALL '+' AND SW-VISA-SND = 'N'            
084400         MOVE '328'              TO RESP-IDMSG-ERROR                      
084500       ELSE                                                               
084600         MOVE '041'              TO RESP-IDMSG-ERROR                      
084700         MOVE 'IDRT-IDRTLOP'     TO RESP-IDELMT-ERROR                     
084800       END-IF                                                             
084900     ELSE                                                                 
085000       MOVE +1                  TO INDX                                   
085100       MOVE +0                  TO WS-COUNT                               
085200       MOVE RET-IDDC            TO RESP-IDDC-RETUR                        
085300       MOVE RET-IDDC            TO RESP-IDDC-RET                          
085400                                                                          
085500                                                                          
085600       PERFORM UNTIL INDX        > MAX-INDX                               
085700         IF SEGMENT-FINNS                                                 
085800            IF RET-IDKOLLI =  W-SPAR-IDKOLLI OR                           
085900               RET-IDKOLLI = ZERO                                         
086000               CONTINUE                                                   
086100            ELSE                                                          
086200               MOVE SPACE        TO RESP-KDCMD (INDX)                     
086300               MOVE RET-IDKOLLI  TO RESP-IDKOLLI(INDX)                    
086400                                    W-SPAR-IDKOLLI                        
086500               MOVE RET-FLFARLIG TO RESP-FLFARLIG(INDX)                   
086600                                                                          
086700               ADD 1             TO INDX                                  
086800               ADD +1            TO WS-COUNT                              
086900            END-IF                                                        
087000            IF VISA-SND                                                   
087100               PERFORM IMS-GN-WDA3B1                                      
087200               IF SEGMENT-FINNS                                           
087300                  PERFORM FA-LAES-WDA301                                  
087400               END-IF                                                     
087500            ELSE                                                          
087600              PERFORM IMS-GHN-SEQB-WLRETA01                               
087700            END-IF                                                        
087800         ELSE                                                             
087900            ADD 1                 TO INDX                                 
088000         END-IF                                                           
088100       END-PERFORM                                                        
088200       MOVE WS-COUNT          TO RESP-KVRADER                             
088300                                                                          
088400       IF WS-COUNT = ZERO                                                 
088500          MOVE '027'  TO RESP-IDMSG-ERROR                                 
088600       END-IF                                                             
088700       IF WS-COUNT = 500                                                  
088800          MOVE '028'  TO RESP-IDMSG-ERROR                                 
088900       END-IF                                                             
089000                                                                          
089100       MOVE 'N'            TO RESP-FLSNDDOK                               
089200     END-IF                                                               
089300     .                                                                    
089400     EJECT                                                                
089500 FA-LAES-WDA301     SECTION.                                              
089600                                                                          
089700     MOVE RET-SEQB-IDDC     TO W-IDDC                                     
089800     MOVE RET-SEQB-DAREGDAT TO W-DAREGDAT                                 
089900     MOVE RET-SEQB-TIKLOCK  TO W-TIKLOCK                                  
090000                                                                          
090100     PERFORM IMS-GU-WDA301                                                
090200     .                                                                    
090300     EJECT                                                                
090400 I-LAES-VISA-TRANSIT SECTION.                                             
090500                                                                          
090600     IF VISA-SND                                                          
090700       PERFORM IA-VISA-TRANSIT-KOLLI                                      
090800     ELSE                                                                 
090900       PERFORM IB-VISA-TRANSIT-SND                                        
091000     END-IF                                                               
091100     .                                                                    
091200     EJECT                                                                
091300 IA-VISA-TRANSIT-KOLLI  SECTION.                                          
091400                                                                          
091500     MOVE LOW-VALUE            TO W-WDA3BSEQ-MIN-X                        
091600     MOVE HIGH-VALUE           TO W-WDA3BSEQ-MAX-X                        
091700     MOVE 'MOD-3-KO'           TO RESP-IDMOD-RADER                        
091800                                                                          
091900     MOVE REQU-IDDC(INDX)      TO W-IDDC-BSEQ-MIN                         
092000                                  W-IDDC-BSEQ-MAX                         
092100                                                                          
092200     IF REQU-KVRADER NUMERIC AND REQU-KVRADER > 0                         
092300       MOVE +1                            TO INDX                         
092400       MOVE REQU-KVRADER                  TO WS-INDX-REC                  
092500       MOVE NEJ                           TO WS-REC-LIMIT                 
092600       MOVE NEJ                           TO SW-RAD-CMD                   
092700                                                                          
092800       PERFORM UNTIL INDX     >  MAX-INDX OR REC-LIMIT                    
092900         IF REQU-KDCMD(INDX)  NOT = ALL '+' OR SPACE                      
093000           IF REQU-KDCMD(INDX) = 'X'                                      
093100             MOVE REQU-IDRT(INDX)      TO W-IDRT-BSEQ-MIN                 
093200                                          W-IDRT-BSEQ-MAX                 
093300             MOVE REQU-IDRTLOP(INDX)   TO W-IDRTLOP-BSEQ-MIN              
093400                                          W-IDRTLOP-BSEQ-MAX              
093500                                                                          
093600             MOVE JA       TO SW-RAD-CMD                                  
093700             MOVE MAX-INDX TO INDX                                        
093800           ELSE                                                           
093900              MOVE 'CMD'        TO RESP-IDELMT-ERROR                      
094000              MOVE '023'        TO RESP-IDMSG-ERROR                       
094100                                   RESP-IDMSG-ERROR-LINE(INDX)            
094200           END-IF                                                         
094300         END-IF                                                           
094400         IF INDX = WS-INDX-REC                                            
094500            MOVE JA TO WS-REC-LIMIT                                       
094600         ELSE                                                             
094700            ADD +1          TO INDX                                       
094800         END-IF                                                           
094900       END-PERFORM                                                        
095000     ELSE                                                                 
095100       IF REQU-KVRADER = 0                                                
095200          MOVE 'KVRADER' TO RESP-IDELMT-ERROR                             
095300          MOVE '126'     TO RESP-IDMSG-ERROR                              
095400       ELSE                                                               
095500          MOVE 'KVRADER' TO RESP-IDELMT-ERROR                             
095600          MOVE '024'     TO RESP-IDMSG-ERROR                              
095700       END-IF                                                             
095800     END-IF                                                               
095900                                                                          
096000     IF RAD-CMD                                                           
096100       MOVE ZERO                 TO W-SPAR-IDKOLLI                        
096200                                                                          
096300       IF NDC-CN OR LDC-CN                                                
096400         CONTINUE                                                         
096500       ELSE                                                               
096600         MOVE WS-CDC-SE          TO W-IDDC-BSEQ-MIN                       
096700                                    W-IDDC-BSEQ-MAX                       
096800       END-IF                                                             
096900                                                                          
097000       PERFORM IMS-GHU-SEQB-WLRETA01                                      
097100                                                                          
097200       IF SEGMENT-SAKNAS                                                  
097300          MOVE '041'              TO RESP-IDMSG-ERROR                     
097400          MOVE 'IDRT-IDRTLOP'     TO RESP-IDELMT-ERROR                    
097500       ELSE                                                               
097600         MOVE +1                  TO INDX                                 
097700         MOVE +0                  TO WS-COUNT                             
097800         MOVE 'TR'                TO RESP-IDDC-RET                        
097900                                     RESP-IDDC-RETUR                      
098000                                                                          
098100         PERFORM UNTIL INDX        > MAX-INDX                             
098200           IF SEGMENT-FINNS                                               
098300              IF RET-IDKOLLI       =  W-SPAR-IDKOLLI                      
098400                 CONTINUE                                                 
098500              ELSE                                                        
098600                 MOVE SPACE        TO RESP-KDCMD (INDX)                   
098700                 MOVE RET-IDKOLLI  TO RESP-IDKOLLI(INDX)                  
098800                                      W-SPAR-IDKOLLI                      
098900                 MOVE RET-FLFARLIG TO RESP-FLFARLIG(INDX)                 
099000                 MOVE SPACE        TO RESP-IDRT(INDX)                     
099100                 MOVE ZERO         TO RESP-IDRTLOP(INDX)                  
099200                                                                          
099300                 ADD 1             TO INDX                                
099400                 ADD +1            TO WS-COUNT                            
099500              END-IF                                                      
099600              PERFORM IMS-GHN-SEQB-WLRETA01                               
099700           ELSE                                                           
099800              ADD 1                 TO INDX                               
099900           END-IF                                                         
100000         END-PERFORM                                                      
100100         MOVE WS-COUNT          TO RESP-KVRADER                           
100200                                                                          
100300         IF WS-COUNT = ZERO                                               
100400            MOVE '027'  TO RESP-IDMSG-ERROR                               
100500         END-IF                                                           
100600         IF WS-COUNT = 500                                                
100700            MOVE '028'  TO RESP-IDMSG-ERROR                               
100800         END-IF                                                           
100900                                                                          
101000         MOVE 'N'            TO RESP-FLSNDDOK                             
101100       END-IF                                                             
101200     END-IF                                                               
101300     .                                                                    
101400     EJECT                                                                
101500 IB-VISA-TRANSIT-SND SECTION.                                             
101600                                                                          
101700     IF NDC-CN   OR  LDC-CN                                               
101800       MOVE WC-NDC-CN-71 TO W-IDDC-G1-MIN                                 
101900       MOVE WC-NDC-CN-74 TO W-IDDC-G1-MAX                                 
102000     ELSE                                                                 
102100       MOVE WS-CDC-SE    TO W-IDDC-G1-MIN                                 
102200                            W-IDDC-G1-MAX                                 
102300     END-IF                                                               
102400                                                                          
102500     MOVE '7'            TO W-KDRETSTA-G1                                 
102600     MOVE REQU-IDRT-KEY  TO W-IDRT-TRANSIT                                
102700     MOVE 'MOD-2-TR'     TO RESP-IDMOD-RADER                              
102800                                                                          
102900     MOVE +1             TO INDX                                          
103000     MOVE +0             TO WS-COUNT                                      
103100     MOVE SPACE          TO W-SPAR-IDRT                                   
103200     MOVE ZERO           TO W-SPAR-IDRTLOP                                
103300                                                                          
103400     PERFORM IMS-GU-WDA3G1                                                
103500                                                                          
103600     IF SEGMENT-FINNS                                                     
103700       MOVE SEQG-IDDC          TO RESP-IDDC-TRANSIT                       
103800       PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                    
103900                                    INDX > MAX-INDX                       
104000                                                                          
104100          IF SEQG-IDRT         =  W-SPAR-IDRT AND                         
104200             SEQG-IDRTLOP      =  W-SPAR-IDRTLOP                          
104300             CONTINUE                                                     
104400          ELSE                                                            
104500            MOVE SEQG-IDRT     TO W-SPAR-IDRT                             
104600            MOVE SEQG-IDRTLOP  TO W-SPAR-IDRTLOP                          
104700                                                                          
104800            MOVE SPACE         TO RESP-KDCMD  (INDX)                      
104900            MOVE SEQG-IDDC     TO RESP-IDDC   (INDX)                      
105000            MOVE SEQG-IDRT     TO RESP-IDRT   (INDX)                      
105100            MOVE SEQG-IDRTLOP  TO RESP-IDRTLOP(INDX)                      
105200            MOVE ZERO          TO RESP-IDKOLLI (INDX)                     
105300            MOVE SPACE         TO RESP-FLFARLIG(INDX)                     
105400                                                                          
105500            ADD +1             TO INDX                                    
105600            ADD +1             TO WS-COUNT                                
105700          END-IF                                                          
105800                                                                          
105900         PERFORM IMS-GN-WDA3G1                                            
106000       END-PERFORM                                                        
106100                                                                          
106200       MOVE WS-COUNT          TO RESP-KVRADER                             
106300                                                                          
106400       IF WS-COUNT = ZERO                                                 
106500          MOVE '027'  TO RESP-IDMSG-ERROR                                 
106600       END-IF                                                             
106700       IF WS-COUNT = 500                                                  
106800          MOVE '028'  TO RESP-IDMSG-ERROR                                 
106900       END-IF                                                             
107000                                                                          
107100       MOVE 'N'                TO RESP-FLSNDDOK                           
107200     ELSE                                                                 
107300       MOVE '041'              TO RESP-IDMSG-ERROR                        
107400       MOVE 'IDRT-IDRTLOP'     TO RESP-IDELMT-ERROR                       
107500     END-IF                                                               
107600                                                                          
107700     MOVE 'TR'                 TO RESP-IDDC-RET                           
107800                                  RESP-IDDC-RETUR                         
107900                                                                          
108000     MOVE 'N'                  TO RESP-FLSNDDOK                           
108100     .                                                                    
108200     EJECT                                                                
108300 G-KOLLA-INPUT SECTION.                                                   
108400                                                                          
108500     MOVE NEJ TO SW-SKAPA-NY-SND                                          
108600     MOVE JA  TO INDATA-SW                                                
108700                                                                          
108800     IF REQU-KDPGMACT = 'N'                                               
108900       MOVE JA          TO SW-SKAPA-NY-SND                                
109000                                                                          
109100       IF REQU-FLSNDDOK = 'J' OR 'Y'                                      
109200         MOVE '014'     TO RESP-IDMSG-ERROR                               
109300         MOVE NEJ       TO INDATA-SW                                      
109400       END-IF                                                             
109500     ELSE                                                                 
109600       IF REQU-IDDC-RET = 'TR'                                            
109700         PERFORM GC-KONTROLL-TRANSIT                                      
109800       ELSE                                                               
109900         PERFORM GA-FORMELL-KONTROLL                                      
110000         IF INDATA-OK                                                     
110100            PERFORM GB-LOGISK-KONTROLL                                    
110200         END-IF                                                           
110300       END-IF                                                             
110400     END-IF                                                               
110500                                                                          
110600     .                                                                    
110700     EJECT                                                                
110800 GA-FORMELL-KONTROLL SECTION.                                             
110900                                                                          
111000     IF REQU-INPUTLINE(1)        = ALL '+' AND                            
111100        REQU-FLSNDDOK = '+'                                               
111200                                                                          
111300       MOVE '014'      TO RESP-IDMSG-ERROR                                
111400                          RESP-IDMSG-ERROR-LINE(1)                        
111500       MOVE NEJ                  TO INDATA-SW                             
111600     ELSE                                                                 
111700                                                                          
111800       PERFORM GAB-KOLLA-KDCMD                                            
111900                                                                          
112000       PERFORM GAC-KOLLA-SNDDOK                                           
112100                                                                          
112200       PERFORM GAD-KOLLA-ANT-FUNKTIONER                                   
112300     END-IF                                                               
112400                                                                          
112500     .                                                                    
112600     EJECT                                                                
112700 GAB-KOLLA-KDCMD      SECTION.                                            
112800                                                                          
112900     IF REQU-KVRADER NUMERIC AND REQU-KVRADER > 0                         
113000       MOVE +1                            TO INDX                         
113100       MOVE REQU-KVRADER                  TO WS-INDX-REC                  
113200       MOVE NEJ                           TO WS-REC-LIMIT                 
113300       MOVE NEJ                           TO SW-RAD-CMD                   
113400                                                                          
113500       PERFORM UNTIL INDX     >  MAX-INDX OR REC-LIMIT                    
113600        IF REQU-KDCMD(INDX)  NOT = ALL '+'                                
113700           MOVE JA          TO  SW-RAD-CMD                                
113800                                                                          
113900           IF VISA-SND                                                    
114000              IF REQU-KDCMD(INDX)         = W-DELETE                      
114100                 CONTINUE                                                 
114200              ELSE                                                        
114300                 MOVE NEJ                  TO INDATA-SW                   
114400                 MOVE 'CMD'                TO RESP-IDELMT-ERROR           
114500                 MOVE '023'        TO RESP-IDMSG-ERROR                    
114600                                      RESP-IDMSG-ERROR-LINE(INDX)         
114700              END-IF                                                      
114800           ELSE                                                           
114900              IF REQU-KDCMD(INDX)          =  W-LASTA                     
115000                 CONTINUE                                                 
115100              ELSE                                                        
115200                 MOVE NEJ                  TO INDATA-SW                   
115300                 MOVE '009'        TO RESP-IDMSG-ERROR                    
115400                                      RESP-IDMSG-ERROR-LINE(INDX)         
115500              END-IF                                                      
115600                                                                          
115700              IF W-IDRTLOP-NUM              =  ZERO                       
115800                MOVE NEJ                  TO INDATA-SW                    
115900                MOVE 'CMD'                TO RESP-IDELMT-ERROR            
116000                MOVE '023'        TO RESP-IDMSG-ERROR                     
116100                                     RESP-IDMSG-ERROR-LINE(INDX)          
116200              END-IF                                                      
116300                                                                          
116400           END-IF                                                         
116500        END-IF                                                            
116600        IF INDX = WS-INDX-REC                                             
116700           MOVE JA TO WS-REC-LIMIT                                        
116800        ELSE                                                              
116900           ADD +1          TO INDX                                        
117000        END-IF                                                            
117100       END-PERFORM                                                        
117200     END-IF                                                               
117300                                                                          
117400     .                                                                    
117500     EJECT                                                                
117600 GAC-KOLLA-SNDDOK     SECTION.                                            
117700                                                                          
117800     MOVE NEJ                     TO SW-SKAPA-SNDDOK                      
117900                                                                          
118000     IF REQU-FLSNDDOK              NOT = ALL '+'                          
118100       IF REQU-FLSNDDOK            =  JA OR YES OR NEJ                    
118200          IF REQU-FLSNDDOK         =  JA OR YES                           
118300             MOVE JA                   TO SW-SKAPA-SNDDOK                 
118400          END-IF                                                          
118500       ELSE                                                               
118600         MOVE NEJ                  TO INDATA-SW                           
118700         MOVE '249'                TO RESP-IDMSG-ERROR                    
118800       END-IF                                                             
118900     END-IF                                                               
119000                                                                          
119100     .                                                                    
119200     EJECT                                                                
119300 GAD-KOLLA-ANT-FUNKTIONER SECTION.                                        
119400                                                                          
119500     IF RAD-CMD                                                           
119600        IF SKAPA-SNDDOK                                                   
119700           MOVE NEJ                TO INDATA-SW                           
119800           MOVE '042'                TO RESP-IDMSG-ERROR                  
119900        END-IF                                                            
120000     END-IF                                                               
120100                                                                          
120200     IF SKAPA-SNDDOK                                                      
120300        IF RAD-CMD                                                        
120400           MOVE NEJ                TO INDATA-SW                           
120500           MOVE '042'                TO RESP-IDMSG-ERROR                  
120600        END-IF                                                            
120700     END-IF                                                               
120800                                                                          
120900     .                                                                    
121000     EJECT                                                                
121100 GB-LOGISK-KONTROLL SECTION.                                              
121200                                                                          
121300     IF RAD-CMD                                                           
121400        PERFORM GBA-KOLLA-VALDA-RADER                                     
121500     END-IF                                                               
121600                                                                          
121700     IF SKAPA-SNDDOK                                                      
121800       PERFORM GBB-KOLLA-ALLA-ING-RT                                      
121900     END-IF                                                               
122000                                                                          
122100     .                                                                    
122200     EJECT                                                                
122300 GBA-KOLLA-VALDA-RADER    SECTION.                                        
122400                                                                          
122500     PERFORM GBAA-KOLLA-SNDSTATUS                                         
122600     MOVE +1                            TO INDX                           
122700     MOVE REQU-KVRADER                  TO WS-INDX-REC                    
122800     MOVE NEJ                           TO WS-REC-LIMIT                   
122900     PERFORM UNTIL INDX      >  MAX-INDX OR REC-LIMIT                     
123000        IF REQU-KDCMD(INDX)  =  W-LASTA OR W-DELETE                       
123100           PERFORM GBAB-KOLLA-VALT-KOLLI                                  
123200        END-IF                                                            
123300        IF INDX = WS-INDX-REC                                             
123400           MOVE JA TO WS-REC-LIMIT                                        
123500        ELSE                                                              
123600           ADD +1          TO INDX                                        
123700        END-IF                                                            
123800     END-PERFORM                                                          
123900                                                                          
124000     .                                                                    
124100     EJECT                                                                
124200 GBAA-KOLLA-SNDSTATUS           SECTION.                                  
124300                                                                          
124400     MOVE REQU-IDRTLOP-KEY             TO W-IDRTLOP-BSEQ-MIN              
124500                                          W-IDRTLOP-BSEQ-MAX              
124600     PERFORM GBAAA-KOLLA-IDRTLOP                                          
124700                                                                          
124800     PERFORM GBAAB-KOLLA-IDRTLOP-DC                                       
124900                                                                          
125000     IF INDATA-OK                                                         
125100       PERFORM IMS-GHU-SEQB-WLRETA01                                      
125200                                                                          
125300       IF SEGMENT-FINNS                                                   
125400         IF RET-KDRETSTA          NOT   = W-SND-TERM                      
125500             MOVE +1                    TO  INDX                          
125600             MOVE REQU-KVRADER          TO WS-INDX-REC                    
125700             MOVE NEJ                   TO WS-REC-LIMIT                   
125800             PERFORM UNTIL INDX    >  MAX-INDX OR REC-LIMIT               
125900                IF REQU-KDCMD(INDX)        =  W-LASTA                     
126000                   MOVE NEJ                TO INDATA-SW                   
126100                   MOVE 'KDRETSTA'  TO RESP-IDELMT-ERROR                  
126200                   MOVE '023'       TO RESP-IDMSG-ERROR                   
126300                                       RESP-IDMSG-ERROR-LINE(INDX)        
126400                END-IF                                                    
126500                IF RET-KDRETSTA = W-SND-TRANSIT                           
126600                  IF REQU-KDCMD(INDX)      =  W-DELETE                    
126700                     MOVE NEJ              TO INDATA-SW                   
126800                     MOVE 'KDRETSTA'    TO RESP-IDELMT-ERROR              
126900                     MOVE '023'     TO RESP-IDMSG-ERROR                   
127000                                       RESP-IDMSG-ERROR-LINE(INDX)        
127100                  END-IF                                                  
127200                END-IF                                                    
127300                IF INDX = WS-INDX-REC                                     
127400                   MOVE JA TO WS-REC-LIMIT                                
127500                ELSE                                                      
127600                   ADD +1          TO INDX                                
127700                END-IF                                                    
127800             END-PERFORM                                                  
127900         END-IF                                                           
128000       END-IF                                                             
128100     END-IF                                                               
128200                                                                          
128300     .                                                                    
128400     EJECT                                                                
128500 GBAAA-KOLLA-IDRTLOP               SECTION.                               
128600                                                                          
128700     PERFORM IMS-GHNP-WL411111                                            
128800     IF W-IDRTLOP-NUM                  >  4112-IDRTLOP                    
128900       MOVE +1                       TO INDX                              
129000       MOVE REQU-KVRADER             TO WS-INDX-REC                       
129100       MOVE NEJ                      TO WS-REC-LIMIT                      
129200       PERFORM UNTIL INDX       >  MAX-INDX OR REC-LIMIT                  
129300          IF REQU-KDCMD(INDX)        =  W-LASTA                           
129400             MOVE NEJ                TO INDATA-SW                         
129500             MOVE 'IDRTLOP'   TO RESP-IDELMT-ERROR                        
129600             MOVE '023'       TO RESP-IDMSG-ERROR                         
129700                                 RESP-IDMSG-ERROR-LINE(INDX)              
129800          END-IF                                                          
129900          IF INDX = WS-INDX-REC                                           
130000             MOVE JA TO WS-REC-LIMIT                                      
130100          ELSE                                                            
130200             ADD +1          TO INDX                                      
130300          END-IF                                                          
130400       END-PERFORM                                                        
130500     END-IF                                                               
130600                                                                          
130700     .                                                                    
130800     EJECT                                                                
130900 GBAAB-KOLLA-IDRTLOP-DC            SECTION.                               
131000                                                                          
131100*- KOLLAR OM DET ÄR EN GAMMAL SÄNDNING.                                   
131200*- MAN FÅR INTE BLANDA OLIKA RETUR-DC PÅ SAMMA SKEPPNING.                 
131300                                                                          
131400     MOVE LOW-VALUE          TO W-WDA3B1-MIN-X                            
131500     MOVE HIGH-VALUE         TO W-WDA3B1-MAX-X                            
131600     MOVE REQU-IDRT-KEY      TO W-IDRT-B1-MIN                             
131700                                W-IDRT-B1-MAX                             
131800                                                                          
131900     PERFORM IMS-GU-WDA3B1                                                
132000     IF SEGMENT-FINNS                                                     
132100       IF RET-SEQB-IDDC = REQU-IDDC-RET                                   
132200         MOVE RET-SEQB-IDDC  TO W-IDDC-BSEQ-MIN                           
132300                                W-IDDC-BSEQ-MAX                           
132400       ELSE                                                               
132500         MOVE NEJ            TO INDATA-SW                                 
132600         MOVE 'IDDC'         TO RESP-IDELMT-ERROR                         
132700         MOVE '023'          TO RESP-IDMSG-ERROR                          
132800                                RESP-IDMSG-ERROR-LINE(INDX)               
132900       END-IF                                                             
133000     ELSE                                                                 
133100       MOVE REQU-IDDC-RET   TO W-IDDC-BSEQ-MIN                            
133200                               W-IDDC-BSEQ-MAX                            
133300     END-IF                                                               
133400     .                                                                    
133500     EJECT                                                                
133600 GBAB-KOLLA-VALT-KOLLI             SECTION.                               
133700                                                                          
133800     MOVE LOW-VALUE            TO W-WDA3BSEQ-MIN-X                        
133900     MOVE HIGH-VALUE           TO W-WDA3BSEQ-MAX-X                        
134000     MOVE REQU-IDRT-KEY        TO W-IDRT-BSEQ-MIN                         
134100                                  W-IDRT-BSEQ-MAX                         
134200     MOVE REQU-IDDC-RET        TO W-IDDC-BSEQ-MIN                         
134300                                  W-IDDC-BSEQ-MAX                         
134400                                                                          
134500     IF VISA-SND                                                          
134600        MOVE REQU-IDRTLOP-KEY  TO W-IDRTLOP-BSEQ-MIN                      
134700                                  W-IDRTLOP-BSEQ-MAX                      
134800     ELSE                                                                 
134900        MOVE ZERO              TO W-IDRTLOP-BSEQ-MIN                      
135000                                  W-IDRTLOP-BSEQ-MAX                      
135100     END-IF                                                               
135200                                                                          
135300     MOVE REQU-IDKOLLI(INDX)       TO W-IDKOLLI-BSEQ-MIN                  
135400                                      W-IDKOLLI-BSEQ-MAX                  
135500     PERFORM IMS-GHU-SEQB-WLRETA01                                        
135600     IF SEGMENT-SAKNAS                                                    
135700        MOVE NEJ                TO INDATA-SW                              
135800        MOVE 'IDKOLLI'          TO RESP-IDELMT-ERROR                      
135900        MOVE '041'       TO RESP-IDMSG-ERROR                              
136000                            RESP-IDMSG-ERROR-LINE(INDX)                   
136100     END-IF                                                               
136200     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
136300        IF REQU-KDCMD(INDX)            = W-LASTA                          
136400           IF RET-KDKOLSTA             NOT = W-KLI-PACKAT                 
136500             MOVE NEJ                TO INDATA-SW                         
136600             MOVE 'KDKOLSTA'  TO RESP-IDELMT-ERROR                        
136700             MOVE '023'       TO RESP-IDMSG-ERROR                         
136800                                 RESP-IDMSG-ERROR-LINE(INDX)              
136900           ELSE                                                           
137000             MOVE RET-IDDC         TO W-IDDC-F1-MIN                       
137100                                      W-IDDC-F1-MAX                       
137200             MOVE RET-IDDISTR      TO W-IDDISTR-F1-MIN                    
137300                                      W-IDDISTR-F1-MAX                    
137400             MOVE RET-IDKUNDNR     TO W-IDKUNDNR-F1-MIN                   
137500                                      W-IDKUNDNR-F1-MAX                   
137600             MOVE RET-IDRAPPNR     TO W-IDRAPPNR-F1-MIN                   
137700                                      W-IDRAPPNR-F1-MAX                   
137800             PERFORM IMS-GU-WLRETG01                                      
137900             PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                 
138000               IF SEQF-IDRTLOP  = ZERO  OR REQU-IDRTLOP-KEY               
138100                  CONTINUE                                                
138200               ELSE                                                       
138300                  MOVE NEJ                TO INDATA-SW                    
138400                  MOVE '188'       TO RESP-IDMSG-ERROR                    
138500                                      RESP-IDMSG-ERROR-LINE(INDX)         
138600               END-IF                                                     
138700               PERFORM IMS-GN-WLRETG01                                    
138800             END-PERFORM                                                  
138900           END-IF                                                         
139000        END-IF                                                            
139100                                                                          
139200        IF REQU-KDCMD(INDX)            = W-DELETE                         
139300           IF RET-KDKOLSTA             NOT = W-KLI-LASTAT                 
139400               MOVE NEJ                TO INDATA-SW                       
139500               MOVE 'KDKOLSTA'  TO RESP-IDELMT-ERROR                      
139600               MOVE '023'       TO RESP-IDMSG-ERROR                       
139700                                   RESP-IDMSG-ERROR-LINE(INDX)            
139800           END-IF                                                         
139900        END-IF                                                            
140000        PERFORM IMS-GHN-SEQB-WLRETA01                                     
140100     END-PERFORM                                                          
140200                                                                          
140300     .                                                                    
140400     EJECT                                                                
140500 GBB-KOLLA-ALLA-ING-RT SECTION.                                           
140600                                                                          
140700     PERFORM GBBA-KOLLA-IDDC-RET                                          
140800                                                                          
140900     MOVE ZERO                TO  W-SPAR-IDDISTR                          
141000                                  W-SPAR-IDKUNDNR                         
141100                                  W-SPAR-IDRAPPNR                         
141200                                                                          
141300     MOVE W-SND-TERM          TO  W-KDRETSTA-C1-MIN                       
141400                                  W-KDRETSTA-C1-MAX                       
141500     MOVE ZERO                TO  W-DARETANK-C1-MIN                       
141600                                  W-DARETANK-C1-MAX                       
141700     MOVE REQU-IDRTLOP-KEY    TO  W-IDRTLOP-C1-MIN                        
141800                                  W-IDRTLOP-C1-MAX                        
141900                                                                          
142000     IF INDATA-OK                                                         
142100       PERFORM IMS-GU-WLRETD01                                            
142200                                                                          
142300       IF SEGMENT-SAKNAS                                                  
142400          MOVE NEJ           TO INDATA-SW                                 
142500          MOVE '023'       TO RESP-IDMSG-ERROR                            
142600          MOVE 'KDRETSTA'  TO RESP-IDELMT-ERROR                           
142700       END-IF                                                             
142800       PERFORM UNTIL SEGMENT-SAKNAS OR INDATA-FEL                         
142900          IF SEQC-IDDISTR           = W-SPAR-IDDISTR  AND                 
143000             SEQC-IDKUNDNR          = W-SPAR-IDKUNDNR AND                 
143100             SEQC-IDRAPPNR          = W-SPAR-IDRAPPNR                     
143200              CONTINUE                                                    
143300          ELSE                                                            
143400              MOVE SEQC-IDDC        TO W-IDDC-E1-MIN                      
143500                                       W-IDDC-E1-MAX                      
143600              MOVE SEQC-IDDISTR     TO W-IDDISTR-E1-MIN                   
143700                                       W-IDDISTR-E1-MAX                   
143800                                       W-SPAR-IDDISTR                     
143900              MOVE SEQC-IDKUNDNR    TO W-IDKUNDNR-E1-MIN                  
144000                                       W-IDKUNDNR-E1-MAX                  
144100                                       W-SPAR-IDKUNDNR                    
144200              MOVE SEQC-IDRAPPNR    TO W-IDRAPPNR-E1-MIN                  
144300                                       W-IDRAPPNR-E1-MAX                  
144400                                       W-SPAR-IDRAPPNR                    
144500                                                                          
144600              PERFORM IMS-GU-WLRETF01                                     
144700                                                                          
144800              IF SEGMENT-FINNS                                            
144900                 MOVE SEQE-IDKOLLI  TO W-FEL-IDKOLLI                      
145000                 MOVE NEJ           TO INDATA-SW                          
145100                 MOVE '100'         TO RESP-IDMSG-ERROR                   
145200                 MOVE 1             TO K-IX                               
145300                 PERFORM UNTIL K-IX > MAX-INDX OR REQU-KVRADER            
145400                   IF W-FEL-IDKOLLI = REQU-IDKOLLI(K-IX)                  
145500                     MOVE '100'     TO RESP-IDMSG-ERROR-LINE(K-IX)        
145600                     MOVE REQU-KVRADER TO K-IX                            
145700                   END-IF                                                 
145800                   ADD 1            TO K-IX                               
145900                 END-PERFORM                                              
146000              END-IF                                                      
146100          END-IF                                                          
146200          PERFORM IMS-GN-WLRETD01                                         
146300       END-PERFORM                                                        
146400     END-IF                                                               
146500                                                                          
146600     .                                                                    
146700     EJECT                                                                
146800 GBBA-KOLLA-IDDC-RET  SECTION.                                            
146900                                                                          
147000     MOVE SPACE             TO W-SPAR-IDDC-RET                            
147100     MOVE LOW-VALUE         TO W-WDA3B1-MIN-X                             
147200     MOVE HIGH-VALUE        TO W-WDA3B1-MAX-X                             
147300     MOVE REQU-IDRT-KEY     TO W-IDRT-B1-MIN                              
147400                               W-IDRT-B1-MAX                              
147500                                                                          
147600     PERFORM IMS-GU-WDA3B1                                                
147700                                                                          
147800     IF SEGMENT-FINNS                                                     
147900       MOVE RET-SEQB-IDDC   TO W-IDDC-C1-MIN                              
148000                               W-IDDC-C1-MAX                              
148100                               W-SPAR-IDDC-RET                            
148200     ELSE                                                                 
148300        MOVE NEJ           TO INDATA-SW                                   
148400        MOVE '023'       TO RESP-IDMSG-ERROR                              
148500        MOVE 'KDRETSTA'  TO RESP-IDELMT-ERROR                             
148600     END-IF                                                               
148700     .                                                                    
148800     EJECT                                                                
148900 GC-KONTROLL-TRANSIT SECTION.                                             
149000                                                                          
149100     IF REQU-INPUTLINE(1)        = ALL '+'                                
149200       MOVE '014'      TO RESP-IDMSG-ERROR                                
149300                          RESP-IDMSG-ERROR-LINE(1)                        
149400       MOVE NEJ                  TO INDATA-SW                             
149500     ELSE                                                                 
149600                                                                          
149700       PERFORM GCA-KOLLA-KDCMD-TR                                         
149800                                                                          
149900       PERFORM GAC-KOLLA-SNDDOK                                           
150000                                                                          
150100       IF SKAPA-SNDDOK AND RAD-CMD                                        
150200         CONTINUE                                                         
150300       ELSE                                                               
150400         MOVE NEJ                 TO INDATA-SW                            
150500         MOVE '046'               TO RESP-IDMSG-ERROR                     
150600       END-IF                                                             
150700     END-IF                                                               
150800                                                                          
150900     .                                                                    
151000     EJECT                                                                
151100 GCA-KOLLA-KDCMD-TR   SECTION.                                            
151200                                                                          
151300     IF REQU-KVRADER NUMERIC AND REQU-KVRADER > 0                         
151400       MOVE +1                            TO INDX                         
151500       MOVE REQU-KVRADER                  TO WS-INDX-REC                  
151600       MOVE NEJ                           TO WS-REC-LIMIT                 
151700       MOVE NEJ                           TO SW-RAD-CMD                   
151800                                                                          
151900       PERFORM UNTIL INDX     >  MAX-INDX OR REC-LIMIT                    
152000        IF REQU-KDCMD(INDX)  NOT = ALL '+'                                
152100           MOVE JA          TO  SW-RAD-CMD                                
152200                                                                          
152300           IF VISA-SND                                                    
152400              MOVE NEJ                  TO INDATA-SW                      
152500              MOVE 'CMD'                TO RESP-IDELMT-ERROR              
152600              MOVE '023'        TO RESP-IDMSG-ERROR                       
152700                                   RESP-IDMSG-ERROR-LINE(INDX)            
152800           ELSE                                                           
152900              IF REQU-KDCMD(INDX)          =  'X'                         
153000                 CONTINUE                                                 
153100              ELSE                                                        
153200                 MOVE NEJ                  TO INDATA-SW                   
153300                 MOVE 'CMD'                TO RESP-IDELMT-ERROR           
153400                 MOVE '023'        TO RESP-IDMSG-ERROR                    
153500                                      RESP-IDMSG-ERROR-LINE(INDX)         
153600              END-IF                                                      
153700                                                                          
153800           END-IF                                                         
153900        END-IF                                                            
154000        IF INDX = WS-INDX-REC                                             
154100           MOVE JA TO WS-REC-LIMIT                                        
154200        ELSE                                                              
154300           ADD +1          TO INDX                                        
154400        END-IF                                                            
154500       END-PERFORM                                                        
154600     ELSE                                                                 
154700       MOVE NEJ           TO INDATA-SW                                    
154800       IF REQU-KVRADER = 0                                                
154900          MOVE 'KVRADER' TO RESP-IDELMT-ERROR                             
155000          MOVE '126'     TO RESP-IDMSG-ERROR                              
155100       ELSE                                                               
155200          MOVE 'KVRADER' TO RESP-IDELMT-ERROR                             
155300          MOVE '024'     TO RESP-IDMSG-ERROR                              
155400       END-IF                                                             
155500     END-IF                                                               
155600                                                                          
155700     .                                                                    
155800     EJECT                                                                
155900 H-UPPDATERA SECTION.                                                     
156000                                                                          
156100     IF SKAPA-NY-SND                                                      
156200        PERFORM HA-TA-UT-IDRTLOP                                          
156300     END-IF                                                               
156400                                                                          
156500     IF RAD-CMD                                                           
156600        PERFORM HB-UPPDATERA-VALDA-KOLLIN                                 
156700     END-IF                                                               
156800                                                                          
156900     IF SKAPA-SNDDOK                                                      
157000       IF REQU-IDDC-RET = 'TR' AND RAD-CMD                                
157100         PERFORM HD-UPPDAT-VALDA-SANDNINGAR                               
157200       ELSE                                                               
157300         PERFORM HC-UPPDATERA-SND-KLAR                                    
157400       END-IF                                                             
157500     END-IF                                                               
157600                                                                          
157700     IF W-UPDATE-OK                                                       
157800       IF SKAPA-NY-SND                                                    
157900          MOVE '292'             TO RESP-IDMSG-INFO                       
158000       ELSE                                                               
158100          MOVE '001'             TO RESP-IDMSG-INFO                       
158200       END-IF                                                             
158300     ELSE                                                                 
158400        MOVE '004'               TO RESP-IDMSG-INFO                       
158500     END-IF                                                               
158600     .                                                                    
158700     EJECT                                                                
158800                                                                          
158900 HA-TA-UT-IDRTLOP SECTION.                                                
159000                                                                          
159100     PERFORM IMS-GHNP-WL411111                                            
159200     COMPUTE 4112-IDRTLOP = 4112-IDRTLOP + 1                              
159300     IF 4112-IDRTLOP = ZERO                                               
159400       ADD +1 TO 4112-IDRTLOP                                             
159500     END-IF                                                               
159600                                                                          
159700     MOVE NEJ            TO W-UPDATE-SW                                   
159800     PERFORM IMS-REPL-WL411111                                            
159900     MOVE JA             TO W-UPDATE-SW                                   
160000                                                                          
160100     MOVE 4112-IDRTLOP    TO W-IDRTLOP-NUM                                
160200     MOVE W-IDRTLOP-NUM   TO RESP-IDRTLOP-KEY                             
160300                                                                          
160400     .                                                                    
160500     EJECT                                                                
160600                                                                          
160700 HB-UPPDATERA-VALDA-KOLLIN    SECTION.                                    
160800                                                                          
160900     MOVE +1                 TO INDX                                      
161000     MOVE REQU-KVRADER       TO WS-INDX-REC                               
161100     MOVE NEJ                TO WS-REC-LIMIT                              
161200     MOVE NEJ                TO W-UPDATE-SW                               
161300     PERFORM UNTIL INDX      >  MAX-INDX OR REC-LIMIT                     
161400        IF REQU-KDCMD(INDX)  =  W-LASTA                                   
161500           PERFORM HBA-LASTA-VALT-KLI-I-SANDN                             
161600        END-IF                                                            
161700                                                                          
161800        IF REQU-KDCMD(INDX)   =  W-DELETE                                 
161900           PERFORM HBB-TA-BORT-VALT-KLI-UR-SANDN                          
162000        END-IF                                                            
162100        IF INDX = WS-INDX-REC                                             
162200           MOVE JA TO WS-REC-LIMIT                                        
162300        ELSE                                                              
162400           ADD +1          TO INDX                                        
162500        END-IF                                                            
162600     END-PERFORM                                                          
162700                                                                          
162800     .                                                                    
162900     EJECT                                                                
163000                                                                          
163100 HBA-LASTA-VALT-KLI-I-SANDN  SECTION.                                     
163200                                                                          
163300     MOVE REQU-IDDC-RET            TO W-IDDC-BSEQ-MIN                     
163400                                      W-IDDC-BSEQ-MAX                     
163500     MOVE ZERO                     TO W-IDRTLOP-BSEQ-MIN                  
163600                                      W-IDRTLOP-BSEQ-MAX                  
163700     MOVE REQU-IDKOLLI(INDX)       TO W-IDKOLLI-BSEQ-MIN                  
163800                                      W-IDKOLLI-BSEQ-MAX                  
163900     PERFORM IMS-GHU-SEQB-WLRETA01                                        
164000     PERFORM UNTIL SEGMENT-SAKNAS                                         
164100        MOVE REQU-IDRTLOP-KEY    TO RET-IDRTLOP                           
164200        MOVE W-KLI-LASTAT        TO RET-KDKOLSTA                          
164300        PERFORM IMS-REPL-SEQB-WLRETA01                                    
164400        MOVE JA             TO W-UPDATE-SW                                
164500        PERFORM IMS-GHN-SEQB-WLRETA01                                     
164600     END-PERFORM                                                          
164700     .                                                                    
164800     EJECT                                                                
164900                                                                          
165000 HBB-TA-BORT-VALT-KLI-UR-SANDN  SECTION.                                  
165100                                                                          
165200     MOVE REQU-IDDC-RET            TO W-IDDC-BSEQ-MIN                     
165300                                      W-IDDC-BSEQ-MAX                     
165400     MOVE REQU-IDRTLOP-KEY         TO W-IDRTLOP-BSEQ-MIN                  
165500                                      W-IDRTLOP-BSEQ-MAX                  
165600     MOVE REQU-IDKOLLI(INDX)       TO W-IDKOLLI-BSEQ-MIN                  
165700                                      W-IDKOLLI-BSEQ-MAX                  
165800     PERFORM IMS-GHU-SEQB-WLRETA01                                        
165900     MOVE NEJ                TO W-UPDATE-SW                               
166000     PERFORM UNTIL SEGMENT-SAKNAS                                         
166100        MOVE ZERO                  TO RET-IDRTLOP                         
166200        MOVE W-KLI-PACKAT          TO RET-KDKOLSTA                        
166300        PERFORM IMS-REPL-SEQB-WLRETA01                                    
166400        MOVE JA        TO W-UPDATE-SW                                     
166500        PERFORM IMS-GHN-SEQB-WLRETA01                                     
166600     END-PERFORM                                                          
166700     .                                                                    
166800     EJECT                                                                
166900 HC-UPPDATERA-SND-KLAR  SECTION.                                          
167000                                                                          
167100     MOVE ZERO                  TO ANTAL-REPL                             
167200     MOVE JA                    TO RADER-KVAR-SW                          
167300                                                                          
167400     IF NOT OMSTART                                                       
167500       MOVE W-SPAR-IDDC-RET     TO  W-IDDC-BSEQ-MIN                       
167600                                    W-IDDC-BSEQ-MAX                       
167700       MOVE REQU-IDRTLOP-KEY    TO  W-IDRTLOP-BSEQ-MIN                    
167800                                    W-IDRTLOP-BSEQ-MAX                    
167900       PERFORM IMS-GHU-SEQB-WLRETA01                                      
168000     ELSE                                                                 
168100       PERFORM IMS-GHU-SEQB-WLRETA01-KVAL                                 
168200       MOVE NEJ                 TO OMSTART-SW                             
168300     END-IF                                                               
168400                                                                          
168500     PERFORM UNTIL SEGMENT-SAKNAS                                         
168600                OR (ANTAL-REPL = MAX-ANTAL-REPL)                          
168700                                                                          
168800        MOVE W-SND-SAENT       TO RET-KDRETSTA                            
168900        MOVE W-KLI-SAENT       TO RET-KDKOLSTA                            
169000        MOVE W-DATUM           TO RET-DASNDDAT                            
169100                                                                          
169200        PERFORM IMS-REPL-SEQB-WLRETA01                                    
169300                                                                          
169400        ADD +1                 TO ANTAL-REPL                              
169500        MOVE JA                TO W-UPDATE-SW                             
169600                                                                          
169700        PERFORM IMS-GHN-SEQB-WLRETA01                                     
169800     END-PERFORM                                                          
169900                                                                          
170000     IF SEGMENT-SAKNAS                                                    
170100       MOVE NEJ           TO RADER-KVAR-SW                                
170200                                                                          
170300     END-IF                                                               
170400                                                                          
170500     .                                                                    
170600     EJECT                                                                
170700 HD-UPPDAT-VALDA-SANDNINGAR SECTION.                                      
170800                                                                          
170900     MOVE +1                 TO INDX                                      
171000     MOVE REQU-KVRADER       TO WS-INDX-REC                               
171100     MOVE NEJ                TO WS-REC-LIMIT                              
171200     MOVE NEJ                TO W-UPDATE-SW                               
171300     PERFORM UNTIL INDX      >  MAX-INDX OR REC-LIMIT                     
171400        IF REQU-KDCMD(INDX)  =  'X'                                       
171500           PERFORM HDA-UPPDAT-TR-SND-KLAR                                 
171600                                                                          
171700           MOVE MAX-INDX TO INDX                                          
171800        END-IF                                                            
171900                                                                          
172000        IF INDX = WS-INDX-REC                                             
172100           MOVE JA TO WS-REC-LIMIT                                        
172200        ELSE                                                              
172300           ADD +1          TO INDX                                        
172400        END-IF                                                            
172500     END-PERFORM                                                          
172600                                                                          
172700     .                                                                    
172800     EJECT                                                                
172900 HDA-UPPDAT-TR-SND-KLAR  SECTION.                                         
173000                                                                          
173100     MOVE ZERO                  TO ANTAL-REPL                             
173200     MOVE JA                    TO RADER-KVAR-SW                          
173300                                                                          
173400     IF NOT OMSTART                                                       
173500       IF NDC-CN OR LDC-CN                                                
173600         MOVE REQU-IDDC(INDX)   TO W-IDDC-BSEQ-MIN                        
173700                                   W-IDDC-BSEQ-MAX                        
173800       ELSE                                                               
173900         MOVE WS-CDC-SE         TO  W-IDDC-BSEQ-MIN                       
174000                                    W-IDDC-BSEQ-MAX                       
174100       END-IF                                                             
174200       MOVE REQU-IDRT(INDX)     TO  W-IDRT-BSEQ-MIN                       
174300                                    W-IDRT-BSEQ-MAX                       
174400       MOVE REQU-IDRTLOP(INDX)  TO  W-IDRTLOP-BSEQ-MIN                    
174500                                    W-IDRTLOP-BSEQ-MAX                    
174600       PERFORM IMS-GHU-SEQB-WLRETA01                                      
174700     ELSE                                                                 
174800       PERFORM IMS-GHU-SEQB-WLRETA01-KVAL                                 
174900       MOVE NEJ                 TO OMSTART-SW                             
175000     END-IF                                                               
175100                                                                          
175200     PERFORM UNTIL SEGMENT-SAKNAS                                         
175300                OR (ANTAL-REPL = MAX-ANTAL-REPL)                          
175400                                                                          
175500        MOVE W-SND-SAENT       TO RET-KDRETSTA                            
175600        MOVE W-DATUM-6         TO RET-TISNDDAT-TRRT                       
175700                                                                          
175800                                                                          
175900        PERFORM IMS-REPL-SEQB-WLRETA01                                    
176000                                                                          
176100        ADD +1                 TO ANTAL-REPL                              
176200        MOVE JA                TO W-UPDATE-SW                             
176300                                                                          
176400        PERFORM IMS-GHN-SEQB-WLRETA01                                     
176500     END-PERFORM                                                          
176600                                                                          
176700     IF SEGMENT-SAKNAS                                                    
176800       MOVE NEJ           TO RADER-KVAR-SW                                
176900                                                                          
177000     END-IF                                                               
177100                                                                          
177200     .                                                                    
177300     EJECT                                                                
177400                                                                          
177500 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
177600                                                                          
177700     MOVE 'GETARG'               TO SUB-KDFUNC                            
177800     MOVE 'CARPARTS.LDC.RETURNRELEASE'  TO SUB-ADDISPABS                  
177900     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
178000                                                                          
178100     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
178200                                                                          
178300     IF SUB-KDRC > 0                                                      
178400       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
178500       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
178600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
178700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
178800     END-IF                                                               
178900     .                                                                    
179000     SKIP3                                                                
179100 S02-RETURN-RESPONSE SECTION.                                             
179200                                                                          
179300     MOVE 'RETURN'                   TO SUB-KDFUNC                        
179400     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
179500                                                                          
179600     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
179700                                                                          
179800     IF SUB-KDRC > 0                                                      
179900       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
180000       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
180100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
180200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
180300     END-IF                                                               
180400     .                                                                    
180500     EJECT                                                                
180600 S03-SEND-TO-RESTART-THIS-PGM  SECTION .                                  
180700                                                                          
180800     PERFORM S03-RESTART-OPEN                                             
180900     PERFORM S03-RESTART-SEND                                             
181000     PERFORM S03-RESTART-CLOSE                                            
181100     .                                                                    
181200     SKIP3                                                                
181300 S03-RESTART-OPEN  SECTION.                                               
181400                                                                          
181500     MOVE 'OPEN'                     TO SEND-KDFUNC                       
181600     MOVE 'CARPARTS.LDC.RETURNRELEASE'  TO SEND-ADDISPABS                 
181700     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
181800                                                                          
181900     IF SEND-KDRC > 0                                                     
182000       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
182100       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
182200       DELIMITED BY SIZE INTO FELTEXT                                     
182300       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
182400     END-IF                                                               
182500     .                                                                    
182600     SKIP3                                                                
182700 S03-RESTART-SEND SECTION.                                                
182800                                                                          
182900     MOVE 'PUT'                      TO SEND-KDFUNC                       
183000     MOVE LENGTH OF SEND-AREA        TO SEND-KVDLEN                       
183100     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN                    
183200                                           SEND-AREA                      
183300                                                                          
183400     IF SEND-KDRC > 0                                                     
183500       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
183600       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
183700       DELIMITED BY SIZE INTO FELTEXT                                     
183800       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
183900     END-IF                                                               
184000     .                                                                    
184100     SKIP3                                                                
184200 S03-RESTART-CLOSE SECTION.                                               
184300                                                                          
184400     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
184500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
184600                                                                          
184700     IF SEND-KDRC > 0                                                     
184800       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
184900       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
185000       DELIMITED BY SIZE INTO FELTEXT                                     
185100       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
185200     END-IF                                                               
185300     .                                                                    
185400     EJECT                                                                
185500* --- IMS SEKTIONER ---                                                   
185600 IMS-GHU-SEQB-WLRETA01-KVAL  SECTION.                                     
185700                                                                          
185800     STRING 'WLRETA01(WDA3BSEQ =' W-WDA3BSEQ-X                            
185900                    '&DAREGDAT =' W-DAREGDAT-A3-X                         
186000                    '&TIKLOCK  =' W-TIKLOCK-A3-X ')'                      
186100          DELIMITED BY SIZE INTO SSA1                                     
186200     MOVE '  GE'           TO GODK-STATUSKODER                            
186300     CALL CBLTDLI USING GHU RETA-PCB DLI-IO-AREA SSA1                     
186400     MOVE RETA-STATUS-CODE TO STATUS-WS                                   
186500     PERFORM IMS-STATUSKONTROLL                                           
186600     .                                                                    
186700                                                                          
186800 IMS-GHU-SEQB-WLRETA01       SECTION.                                     
186900                                                                          
187000     STRING 'WLRETA01(WDA3BSEQ>=' W-WDA3BSEQ-MIN-X                        
187100                    '&WDA3BSEQ<=' W-WDA3BSEQ-MAX-X ')'                    
187200          DELIMITED BY SIZE INTO SSA1                                     
187300     MOVE '  GE'           TO GODK-STATUSKODER                            
187400     CALL CBLTDLI USING GHU RETA-PCB DLI-IO-AREA SSA1                     
187500     MOVE RETA-STATUS-CODE TO STATUS-WS                                   
187600     PERFORM IMS-STATUSKONTROLL                                           
187700     .                                                                    
187800                                                                          
187900 IMS-GHN-SEQB-WLRETA01       SECTION.                                     
188000                                                                          
188100     STRING 'WLRETA01(WDA3BSEQ>=' W-WDA3BSEQ-MIN-X                        
188200                    '&WDA3BSEQ<=' W-WDA3BSEQ-MAX-X ')'                    
188300          DELIMITED BY SIZE INTO SSA1                                     
188400     MOVE '  GEGB'           TO GODK-STATUSKODER                          
188500     CALL CBLTDLI USING GHN RETA-PCB DLI-IO-AREA SSA1                     
188600     MOVE RETA-STATUS-CODE TO STATUS-WS                                   
188700     PERFORM IMS-STATUSKONTROLL                                           
188800     .                                                                    
188900     EJECT                                                                
189000                                                                          
189100 IMS-REPL-SEQB-WLRETA01      SECTION.                                     
189200                                                                          
189300     MOVE '    '           TO GODK-STATUSKODER                            
189400     CALL CBLTDLI USING REPL RETA-PCB DLI-IO-AREA                         
189500     MOVE RETA-STATUS-CODE TO STATUS-WS                                   
189600     PERFORM IMS-STATUSKONTROLL                                           
189700     .                                                                    
189800                                                                          
189900                                                                          
190000 IMS-GU-WLRETD01       SECTION.                                           
190100                                                                          
190200     STRING 'WLRETD01(WDA3C1KY>=' W-WDA3C1KY-MIN-X                        
190300                    '&WDA3C1KY<=' W-WDA3C1KY-MAX-X ')'                    
190400          DELIMITED BY SIZE INTO SSA1                                     
190500     MOVE '  GE'         TO GODK-STATUSKODER                              
190600     CALL CBLTDLI USING GU RETD-PCB DLI-IO-AREA SSA1                      
190700     MOVE RETD-STATUS-CODE TO STATUS-WS                                   
190800     PERFORM IMS-STATUSKONTROLL                                           
190900     .                                                                    
191000     EJECT                                                                
191100                                                                          
191200 IMS-GN-WLRETD01       SECTION.                                           
191300                                                                          
191400     STRING 'WLRETD01(WDA3C1KY>=' W-WDA3C1KY-MIN-X                        
191500                    '&WDA3C1KY<=' W-WDA3C1KY-MAX-X ')'                    
191600          DELIMITED BY SIZE INTO SSA1                                     
191700     MOVE '  GEGB'           TO GODK-STATUSKODER                          
191800     CALL CBLTDLI USING GN RETD-PCB DLI-IO-AREA SSA1                      
191900     MOVE RETD-STATUS-CODE TO STATUS-WS                                   
192000     PERFORM IMS-STATUSKONTROLL                                           
192100     .                                                                    
192200     EJECT                                                                
192300                                                                          
192400 IMS-GU-WLRETF01       SECTION.                                           
192500                                                                          
192600     STRING 'WLRETF01(WDA3E1KY>=' W-WDA3E1KY-MIN-X                        
192700                    '&WDA3E1KY<=' W-WDA3E1KY-MAX-X ')'                    
192800          DELIMITED BY SIZE INTO SSA1                                     
192900     MOVE '  GE'           TO GODK-STATUSKODER                            
193000     CALL CBLTDLI USING GU RETF-PCB DLI-IO-AREA SSA1                      
193100     MOVE RETF-STATUS-CODE TO STATUS-WS                                   
193200     PERFORM IMS-STATUSKONTROLL                                           
193300     .                                                                    
193400     EJECT                                                                
193500 IMS-GU-WLRETG01       SECTION.                                           
193600                                                                          
193700     STRING 'WLRETG01(WDA3F1KY>=' W-WDA3F1KY-MIN-X                        
193800                    '&WDA3F1KY<=' W-WDA3F1KY-MAX-X ')'                    
193900          DELIMITED BY SIZE INTO SSA1                                     
194000     MOVE '  GE'           TO GODK-STATUSKODER                            
194100     CALL CBLTDLI USING GU RETG-PCB DLI-IO-AREA SSA1                      
194200     MOVE RETG-STATUS-CODE TO STATUS-WS                                   
194300     PERFORM IMS-STATUSKONTROLL                                           
194400     .                                                                    
194500                                                                          
194600 IMS-GN-WLRETG01       SECTION.                                           
194700                                                                          
194800     STRING 'WLRETG01(WDA3F1KY>=' W-WDA3F1KY-MIN-X                        
194900                    '&WDA3F1KY<=' W-WDA3F1KY-MAX-X ')'                    
195000          DELIMITED BY SIZE INTO SSA1                                     
195100     MOVE '  GEGB'         TO GODK-STATUSKODER                            
195200     CALL CBLTDLI USING GN RETG-PCB DLI-IO-AREA SSA1                      
195300     MOVE RETG-STATUS-CODE TO STATUS-WS                                   
195400     PERFORM IMS-STATUSKONTROLL                                           
195500     .                                                                    
195600                                                                          
195700 IMS-GU-WDA3G1   SECTION.                                                 
195800                                                                          
195900     STRING 'WDA3G1  (WDA3G1KY>=' W-WDA3G1KY-MIN-X                        
196000                    '&WDA3G1KY<=' W-WDA3G1KY-MAX-X                        
196100                    '&KDRETSTA =' W-KDRETSTA-G1-X                         
196200                    '&IDRTTR   =' W-IDRT-TRANSIT-X ')'                    
196300          DELIMITED BY SIZE INTO SSA1                                     
196400     MOVE '  GE' TO GODK-STATUSKODER                                      
196500     CALL CBLTDLI USING GU WDA3G-PCB DLI-IO-WDA3G1 SSA1                   
196600     MOVE WDA3G-STATUS-CODE TO STATUS-WS                                  
196700     PERFORM IMS-STATUSKONTROLL                                           
196800     .                                                                    
196900     EJECT                                                                
197000 IMS-GN-WDA3G1   SECTION.                                                 
197100                                                                          
197200     STRING 'WDA3G1  (WDA3G1KY>=' W-WDA3G1KY-MIN-X                        
197300                    '&WDA3G1KY<=' W-WDA3G1KY-MAX-X                        
197400                    '&KDRETSTA =' W-KDRETSTA-G1-X                         
197500                    '&IDRTTR   =' W-IDRT-TRANSIT-X ')'                    
197600          DELIMITED BY SIZE INTO SSA1                                     
197700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
197800     CALL CBLTDLI USING GN WDA3G-PCB DLI-IO-WDA3G1 SSA1                   
197900     MOVE WDA3G-STATUS-CODE TO STATUS-WS                                  
198000     PERFORM IMS-STATUSKONTROLL                                           
198100     .                                                                    
198200     EJECT                                                                
198300 IMS-GU-WDA3B1               SECTION.                                     
198400                                                                          
198500     STRING 'WDA3B1  (WDA3B1KY>=' W-WDA3B1-MIN-X                          
198600                    '&WDA3B1KY<=' W-WDA3B1-MAX-X                          
198700                    '&IDRTLOP  =' W-IDRTLOP-B1-X ')'                      
198800          DELIMITED BY SIZE INTO SSA1                                     
198900     MOVE '  GE'           TO GODK-STATUSKODER                            
199000     CALL CBLTDLI USING GU WDA3B-PCB DLI-IO-WDA3B1 SSA1                   
199100     MOVE WDA3B-STATUS-CODE TO STATUS-WS                                  
199200     PERFORM IMS-STATUSKONTROLL                                           
199300     .                                                                    
199400                                                                          
199500 IMS-GN-WDA3B1               SECTION.                                     
199600                                                                          
199700     STRING 'WDA3B1  (WDA3B1KY>=' W-WDA3B1-MIN-X                          
199800                    '&WDA3B1KY<=' W-WDA3B1-MAX-X                          
199900                    '&IDRTLOP  =' W-IDRTLOP-B1-X ')'                      
200000          DELIMITED BY SIZE INTO SSA1                                     
200100     MOVE '  GEGB'           TO GODK-STATUSKODER                          
200200     CALL CBLTDLI USING GN WDA3B-PCB DLI-IO-WDA3B1 SSA1                   
200300     MOVE WDA3B-STATUS-CODE TO STATUS-WS                                  
200400     PERFORM IMS-STATUSKONTROLL                                           
200500     .                                                                    
200600     EJECT                                                                
200700 IMS-GU-WDA301               SECTION.                                     
200800                                                                          
200900     STRING 'WDA301  (WDA301KY =' W-WDA301KY-X ')'                        
201000          DELIMITED BY SIZE INTO SSA1                                     
201100     MOVE '  GE'           TO GODK-STATUSKODER                            
201200     CALL CBLTDLI USING GU WDA3-PCB DLI-IO-AREA SSA1                      
201300     MOVE WDA3-STATUS-CODE TO STATUS-WS                                   
201400     PERFORM IMS-STATUSKONTROLL                                           
201500     .                                                                    
201600     EJECT                                                                
201700 IMS-GU-WL411101  SECTION.                                                
201800                                                                          
201900     STRING 'WL411101(WDGXKEY  =' W-WDGXKEY-X ')'                         
202000                      DELIMITED BY SIZE INTO SSA1                         
202100     MOVE '  GE' TO GODK-STATUSKODER                                      
202200     CALL CBLTDLI USING GU 4111-PCB WL411101 SSA1                         
202300     MOVE 4111-STATUS-CODE TO STATUS-WS                                   
202400     PERFORM IMS-STATUSKONTROLL                                           
202500     .                                                                    
202600                                                                          
202700 IMS-GHNP-WL411111  SECTION.                                              
202800                                                                          
202900     MOVE  'WL411111*F' TO SSA1                                           
203000     MOVE '    ' TO GODK-STATUSKODER                                      
203100     CALL CBLTDLI USING GHNP 4111-PCB WL411111 SSA1                       
203200     MOVE 4111-STATUS-CODE TO STATUS-WS                                   
203300     PERFORM IMS-STATUSKONTROLL                                           
203400     .                                                                    
203500     EJECT                                                                
203600 IMS-REPL-WL411111  SECTION.                                              
203700                                                                          
203800     MOVE '  ' TO GODK-STATUSKODER                                        
203900     CALL CBLTDLI USING REPL 4111-PCB WL411111                            
204000     MOVE 4111-STATUS-CODE TO STATUS-WS                                   
204100     PERFORM IMS-STATUSKONTROLL                                           
204200     .                                                                    
204300     SKIP2                                                                
204400 IMS-STATUSKONTROLL SECTION.                                              
204500                                                                          
204600     SET STATUS-IX TO 1                                                   
204700     SEARCH GODK-STATUS                                                   
204800       AT END                                                             
204900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
205000         DELIMITED BY SIZE INTO FELTEXT                                   
205100         CALL FELLOG                                                      
205200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
205300         CONTINUE                                                         
205400     END-SEARCH                                                           
205500     .                                                                    
