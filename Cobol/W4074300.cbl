000100*                                                                         
000200******************************************************************        
000300*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0158      *        
000400******************************************************************        
000500*                                                                         
000600 ID DIVISION.                                                             
000700                                                                          
000800 PROGRAM-ID.     W4074300.                                                
000900 AUTHOR.         LARS THELL.                                              
001000 DATE-WRITTEN.   95/05/30.                                                
001100 DATE-COMPILED.                                                           
001200                                                                          
001300*    FUNKTION:                                                            
001400*        SKAPAR SÄNDNING MED VALDA KOLLIN. VISAR ÄVEN SÄNDNINGS-          
001500*        INNEHÅLL.                                                        
001600*                                                                         
001700*        PROGRAMMET UPPDATERAR WLRETA (WDA3)                              
001800*        PROGRAMMET UPPDATERAR WL4111 (WDR1)                              
001900*        PROGRAMMET LÄSER      WLRETD (WDA3)                              
002000*        PROGRAMMET LÄSER      WLRETF (WDA3)                              
002100*                                                                         
002200*    INDATA.                                                              
002300*        TRANSAKTION: W4T743                                              
002400*        MID:         W4I74301                                            
002500*                                                                         
002600*    UTDATA.                                                              
002700*        MOD:         W4O74301                                            
002800*                                                                         
002900*    E'TRACKER 3852148 20060912                                           
003000*    E'TRACKER 4230251 20070207                                           
003100*                                                                         
003200                                                                          
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900*    -- CHECKED BY WY2000                                                 
004000 77  IDPGM                       PIC X(08)   VALUE 'W4074300'.            
004100                                                                          
004200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004400                                                                          
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  YES                         PIC X       VALUE 'Y'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800 77  W-BORTTAG                   PIC X       VALUE 'B'.                   
004900 77  W-DELETE                    PIC X       VALUE 'D'.                   
005000 77  W-LASTA                     PIC X       VALUE 'L'.                   
005100 77  W-IDPRTLST                  PIC X(8)    VALUE SPACE.                 
005200 77  W-DATUM                     PIC 9(8)    VALUE ZERO.                  
005300 77  ANTAL-REPL                  PIC S9(3)  COMP-3 VALUE ZERO.            
005400 77  MAX-ANTAL-REPL              PIC S9(3)  COMP-3 VALUE +200.            
005500 77  WS-IDKOLLI                  PIC 9(5)    VALUE ZERO.                  
005600 77  WS-TIKLOCK                  PIC 9(8)    VALUE ZERO.                  
005700                                                                          
005800*    --- INDEX FÖR BLÄDDRINGSRADER                                        
005900 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
006000 77  MAX-INDX                    PIC S9(4)  VALUE +11   COMP SYNC.        
006100*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006200                                                                          
006300 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006400     88  INDATA-OK                           VALUE 'J'.                   
006500     88  INDATA-FEL                          VALUE 'N'.                   
006600                                                                          
006700 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006800     88  NYCKLAR-OK                          VALUE 'J'.                   
006900     88  NYCKLAR-FEL                         VALUE 'N'.                   
007000                                                                          
007100 77  OMSTART-SW                  PIC X       VALUE 'N'.                   
007200     88  OMSTART                             VALUE 'J'.                   
007300                                                                          
007400 77  RADER-KVAR-SW               PIC X       VALUE 'J'.                   
007500     88  RADER-KVAR                          VALUE 'J'.                   
007600     88  INGA-RADER-KVAR                     VALUE 'N'.                   
007700                                                                          
007800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007900     88  EGEN-MID                            VALUE '4743'.                
008000     88  GODK-MID                            VALUE '4743'.                
008100     88  HELP-MID                            VALUE '0551'.                
008200     EJECT                                                                
008300*      --- VALID IDDC CODES                                               
008400*                                                                         
008500*01    -COPY WWDCKONS                                                     
008600       EJECT                                                              
008700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008800 01  GENERELLA-SUBPROGRAM.                                                
008900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009300     03  W4074310                PIC X(8)    VALUE 'W4074310'.            
009400     EJECT                                                                
009500*    --- PARAMETRAR TILL SUBPROGRAM W4074310                              
009600*01 -COPY W4074310                                                        
009700     EJECT                                                                
009800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009900*01 -COPY WMEDAREA                                                        
010000     SKIP3                                                                
010100 01  MESSAGE-CODES.                                                       
010200     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
010300     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
010400     03  ERR-OTILL-UPD           PIC X(3)    VALUE '007'.                 
010500     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
010600     03  ERR-OTILL-STATUS        PIC X(3)    VALUE '079'.                 
010700     03  ERR-FLERA-FUNKTIONER    PIC X(3)    VALUE '097'.                 
010800     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
010900     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
011000     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
011100     03  ERR-REDAN-LASTAD        PIC X(3)    VALUE '310'.                 
011200     03  ERR-EJ-LASTAD           PIC X(3)    VALUE '311'.                 
011300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011400     03  ERR-KOLLI-SAKNAS        PIC X(3)    VALUE '758'.                 
011500     EJECT                                                                
011600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
011700*                                                                         
011800 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
011900     SKIP3                                                                
012000*01 -COPY WMSGINIT                                                        
012100     SKIP3                                                                
012200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
012300*                                                                         
012400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012500     SKIP3                                                                
012600*01  MID -COPY W4I74301                                                   
012700     EJECT                                                                
012800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012900     SKIP3                                                                
013000*01  -COPY WMSGAREA                                                       
013100     EJECT                                                                
013200     03  MOD REDEFINES MSG-AREA.                                          
013300*      05  -COPY W4O74301    -PRE MOD-                                    
013400     EJECT                                                                
013500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013600     SKIP3                                                                
013700*01  -COPY WMFSAREA                                                       
013800     EJECT                                                                
013900 77  SW-GAMMAL-SND              PIC X       VALUE 'N'.                    
014000     88  GAMMAL-SND                         VALUE 'J'.                    
014100                                                                          
014200 77  SW-VISA-SND                 PIC X       VALUE 'N'.                   
014300     88  VISA-SND                            VALUE 'J'.                   
014400                                                                          
014500 77  SW-RAD-CMD                  PIC X       VALUE 'N'.                   
014600     88  RAD-CMD                             VALUE 'J'.                   
014700                                                                          
014800 77  SW-SKAPA-SNDDOK             PIC X       VALUE 'N'.                   
014900     88  SKAPA-SNDDOK                        VALUE 'J'.                   
015000                                                                          
015100 77  SW-SKAPA-NY-SND             PIC X       VALUE 'N'.                   
015200     88  SKAPA-NY-SND                        VALUE 'J'.                   
015300                                                                          
015400 77  W-IDRTLOP-NUM               PIC  9(3)   VALUE ZERO.                  
015500 77  W-FLVISA                    PIC X       VALUE 'N'.                   
015600 77  W-KLI-PACKAT                PIC S9(1)   VALUE +1   COMP-3.           
015700 77  W-KLI-LASTAT                PIC S9(1)   VALUE +2   COMP-3.           
015800 77  W-KLI-SAENT                 PIC S9(1)   VALUE +3   COMP-3.           
015900 77  W-SND-TERM                  PIC  X(1)   VALUE '1'.                   
016000 77  W-SND-SAENT                 PIC  X(1)   VALUE '2'.                   
016100 77  W-SPAR-IDDISTR              PIC S9(5)   VALUE ZERO COMP-3.           
016200 77  W-SPAR-IDKUNDNR             PIC S9(7)   VALUE ZERO COMP-3.           
016300 77  W-SPAR-IDRAPPNR             PIC  9(7)   VALUE ZERO.                  
016400 77  W-FEL-IDKOLLI               PIC  9(5)   VALUE ZERO.                  
016500 77  W-SPAR-IDKOLLI              PIC S9(5)   VALUE ZERO COMP-3.           
016600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016700*                                                                         
016800     EJECT                                                                
016900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017000     SKIP3                                                                
017100 01  W-MINKEY-X.                                                          
017200     03  W-MINKEY-IDTRANS          PIC  X(4)   VALUE '4743'.              
017300     03  W-MINKEY-WDA3BSEQ-ENTER.                                         
017500         05  W-MINKEYB1-IDRT       PIC  X(3)          VALUE SPACE.        
017600         05  W-MINKEYB1-IDDC       PIC  X(2)          VALUE SPACE.        
017700         05  W-MINKEYB1-IDRTLOP    PIC  9(3)          VALUE ZERO.         
017800         05  W-MINKEYB1-IDKOLLI    PIC S9(5)   COMP-3 VALUE ZERO.         
017900     03  W-MINKEY-WDA3BSEQ-NEXT.                                          
018100         05  W-MINKEYB1-IDRT-NEXT  PIC  X(3)          VALUE SPACE.        
018110         05  W-MINKEYB1-IDDC-NEXT  PIC  X(2)          VALUE SPACE.        
018200         05  W-MINKEYB1-IDRTLOP-NEXT PIC  9(3)        VALUE ZERO.         
018300         05  W-MINKEYB1-IDKOLLI-NEXT PIC S9(5) COMP-3 VALUE ZERO.         
018400     SKIP3                                                                
018500 01  NYCKLAR-TILL-DLI.                                                    
018600     03  W-WDA301KY-X.                                                    
018700         05  W-IDDC              PIC  X(2)          VALUE SPACE.          
018800         05  W-DAREGDAT          PIC  9(8)          VALUE ZERO.           
018900         05  W-TIKLOCK           PIC S9(9)   COMP-3 VALUE ZERO.           
019000                                                                          
019100     03  W-WDA3BSEQ-X.                                                    
019200         05  W-IDRT-BSEQ         PIC  X(3)          VALUE SPACE.          
019300         05  W-IDDC-BSEQ         PIC  X(2)          VALUE SPACE.          
019400         05  W-IDRTLOP-BSEQ      PIC  9(3)          VALUE ZERO.           
019500         05  W-IDKOLLI-BSEQ      PIC S9(5)   COMP-3 VALUE ZERO.           
019600                                                                          
019700     03  W-WDA3BSEQ-MIN-X.                                                
019800         05  W-IDRT-BSEQ-MIN     PIC  X(3)          VALUE SPACE.          
019900         05  W-IDDC-BSEQ-MIN     PIC  X(2)          VALUE SPACE.          
020000         05  W-IDRTLOP-BSEQ-MIN  PIC  9(3)          VALUE ZERO.           
020100         05  W-IDKOLLI-BSEQ-MIN  PIC S9(5)   COMP-3 VALUE ZERO.           
020200                                                                          
020300     03  W-WDA3BSEQ-MAX-X.                                                
020400         05  W-IDRT-BSEQ-MAX     PIC  X(3)          VALUE SPACE.          
020500         05  W-IDDC-BSEQ-MAX     PIC  X(2)          VALUE SPACE.          
020600         05  W-IDRTLOP-BSEQ-MAX  PIC  9(3)          VALUE ZERO.           
020700         05  W-IDKOLLI-BSEQ-MAX  PIC S9(5)   COMP-3 VALUE ZERO.           
020800                                                                          
020900     03  W-WDA3C1KY-MIN-X.                                                
021000         05  W-IDDC-C1-MIN       PIC  X(2)          VALUE SPACE.          
021100         05  W-KDRETSTA-C1-MIN   PIC  X(1)          VALUE SPACE.          
021200         05  W-DARETANK-C1-MIN   PIC  9(8)          VALUE ZERO.           
021300         05  W-IDRT-C1-MIN       PIC  X(3)          VALUE SPACE.          
021400         05  W-IDRTLOP-C1-MIN    PIC  9(3)          VALUE ZERO.           
021500         05  W-IDDISTR-C1-MIN    PIC S9(5)   COMP-3 VALUE ZERO.           
021600         05  W-IDKUNDNR-C1-MIN   PIC S9(7)   COMP-3 VALUE ZERO.           
021700         05  W-IDRAPPNR-C1-MIN   PIC  9(7)          VALUE ZERO.           
021800         05  W-DAREGDAT-C1-MIN   PIC  9(8)          VALUE ZERO.           
021900         05  W-TIKLOCK-C1-MIN    PIC S9(9)   COMP-3 VALUE ZERO.           
022000                                                                          
022100     03  W-WDA3C1KY-MAX-X.                                                
022200         05  W-IDDC-C1-MAX       PIC  X(2)          VALUE SPACE.          
022300         05  W-KDRETSTA-C1-MAX   PIC  X(1)          VALUE SPACE.          
022400         05  W-DARETANK-C1-MAX   PIC  9(8)          VALUE ZERO.           
022500         05  W-IDRT-C1-MAX       PIC  X(3)          VALUE SPACE.          
022600         05  W-IDRTLOP-C1-MAX    PIC  9(3)          VALUE ZERO.           
022700         05  W-IDDISTR-C1-MAX    PIC S9(5)   COMP-3 VALUE ZERO.           
022800         05  W-IDKUNDNR-C1-MAX   PIC S9(7)   COMP-3 VALUE ZERO.           
022900         05  W-IDRAPPNR-C1-MAX   PIC  9(7)          VALUE ZERO.           
023000         05  W-DAREGDAT-C1-MAX   PIC  9(8)          VALUE ZERO.           
023100         05  W-TIKLOCK-C1-MAX    PIC S9(9)   COMP-3 VALUE ZERO.           
023200                                                                          
023300     03  W-WDA3E1KY-MIN-X.                                                
023400         05  W-IDDC-E1-MIN       PIC  X(2)          VALUE SPACE.          
023500         05  W-IDRT-E1-MIN       PIC  X(3)          VALUE SPACE.          
023600         05  W-IDDISTR-E1-MIN    PIC S9(5)   COMP-3 VALUE ZERO.           
023700         05  W-IDKUNDNR-E1-MIN   PIC S9(7)   COMP-3 VALUE ZERO.           
023800         05  W-IDRAPPNR-E1-MIN   PIC  9(7)          VALUE ZERO.           
023900         05  W-DAREGDAT-E1-MIN   PIC  9(8)          VALUE ZERO.           
024000         05  W-TIKLOCK-E1-MIN    PIC S9(9)   COMP-3 VALUE ZERO.           
024100                                                                          
024200     03  W-WDA3E1KY-MAX-X.                                                
024300         05  W-IDDC-E1-MAX       PIC  X(2)          VALUE SPACE.          
024400         05  W-IDRT-E1-MAX       PIC  X(3)          VALUE SPACE.          
024500         05  W-IDDISTR-E1-MAX    PIC S9(5)   COMP-3 VALUE ZERO.           
024600         05  W-IDKUNDNR-E1-MAX   PIC S9(7)   COMP-3 VALUE ZERO.           
024700         05  W-IDRAPPNR-E1-MAX   PIC  9(7)          VALUE ZERO.           
024800         05  W-DAREGDAT-E1-MAX   PIC  9(8)          VALUE ZERO.           
024900         05  W-TIKLOCK-E1-MAX    PIC S9(9)   COMP-3 VALUE ZERO.           
025000                                                                          
025100     03  W-WDA3F1KY-MIN-X.                                                
025200         05  W-IDDC-F1-MIN       PIC  X(2)          VALUE SPACE.          
025300         05  W-IDDISTR-F1-MIN    PIC S9(5)   COMP-3 VALUE ZERO.           
025400         05  W-IDKUNDNR-F1-MIN   PIC S9(7)   COMP-3 VALUE ZERO.           
025500         05  W-IDRAPPNR-F1-MIN   PIC  9(7)          VALUE ZERO.           
025600         05  FILLER              PIC  X(22)  VALUE LOW-VALUE.             
025700                                                                          
025800     03  W-WDA3F1KY-MAX-X.                                                
025900         05  W-IDDC-F1-MAX       PIC  X(2)          VALUE SPACE.          
026000         05  W-IDDISTR-F1-MAX    PIC S9(5)   COMP-3 VALUE ZERO.           
026100         05  W-IDKUNDNR-F1-MAX   PIC S9(7)   COMP-3 VALUE ZERO.           
026200         05  W-IDRAPPNR-F1-MAX   PIC  9(7)          VALUE ZERO.           
026300         05  FILLER              PIC  X(22)  VALUE HIGH-VALUE.            
026400                                                                          
026500     03  W-WDGXKEY-X.                                                     
026600         05  W-IDHTYP            PIC  X(4)   VALUE '4111'.                
026700         05  W-IDRT-4111         PIC  X(3)   VALUE SPACE.                 
026800         05  FILLER              PIC X(23)   VALUE LOW-VALUE.             
026900                                                                          
027000                                                                          
027100     03  W-DAREGDAT-A3-X.                                                 
027200         05  W-DAREGDAT-A3       PIC  9(8)   VALUE ZERO.                  
027300                                                                          
027400     03  W-TIKLOCK-A3-X.                                                  
027500         05  W-TIKLOCK-A3        PIC S9(9)   COMP-3 VALUE ZERO.           
027600                                                                          
027700     SKIP2                                                                
027800*    --- STATUS-KOD FRÅN IMS                                              
027900 01  STATUS-WS                   PIC XX.                                  
028000     88  SEGMENT-FINNS                       VALUE '  '.                  
028100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
028200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
028300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
028400     SKIP2                                                                
028500 01  GODK-STATUSKODER.                                                    
028600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
028700     SKIP3                                                                
028800 01  SSA1                        PIC X(128).                              
028900 01  SSA2                        PIC X(128).                              
029000     EJECT                                                                
029100*    --- IMS FUNKTIONSKODER                                               
029200*01  -COPY W0003                                                          
029300     EJECT                                                                
029400*    ---  DLI INPUT-OUTPUT AREA                                           
029500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
029600     SKIP3                                                                
029700 01  DLI-IO-AREA.                                                         
029800     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
029900     SKIP3                                                                
030000     03  WLRETA01 REDEFINES IO-AREA.                                      
030100*        05  -COPY WDA301                                                 
030200     EJECT                                                                
030300     03  WLRETC01 REDEFINES IO-AREA.                                      
030400*        05  -COPY WDA3B1                                                 
030500     EJECT                                                                
030600     03  WLRETD01 REDEFINES IO-AREA.                                      
030700*        05  -COPY WDA3C1                                                 
030800     EJECT                                                                
030900     03  WLRETF01 REDEFINES IO-AREA.                                      
031000*        05  -COPY WDA3E1                                                 
031100     EJECT                                                                
031200     03  WLRETG01 REDEFINES IO-AREA.                                      
031300*        05  -COPY WDA3F1                                                 
031400     EJECT                                                                
031500     SKIP3                                                                
031600 01    FILLER                    PIC X(16) VALUE 'WDGX4111-AREA'.         
031700 01    WL411101  -COPY WDGX4111                                           
031800     EJECT                                                                
031900 01    FILLER                    PIC X(16) VALUE 'WDGX4112-AREA'.         
032000 01    WL411111  -COPY WDGX4112                                           
032100                                                                          
032200     EJECT                                                                
032300 LINKAGE SECTION.                                                         
032400                                                                          
032500*01  -COPY W0009   -PRE MSG-                                              
032600*01  -COPY W0009   -PRE 4743-                                             
032700*01  -COPY W0009   -PRE UTSKR-ALT-                                        
032800     EJECT                                                                
032900*01  -COPY W0008   -PRE USEA-                                             
033000     05  FILLER                  PIC X.                                   
033100     EJECT                                                                
033200*01  -COPY W0008  -PRE RETA-                                              
033300     05  FILLER                  PIC X.                                   
033400     EJECT                                                                
033500*01  -COPY W0008  -PRE RETD-                                              
033600     05  FILLER                  PIC X.                                   
033700     EJECT                                                                
033800*01  -COPY W0008  -PRE RETF-                                              
033900     05  FILLER                  PIC X.                                   
034000     EJECT                                                                
034100*01  -COPY W0008  -PRE RETG-                                              
034200     05  FILLER                  PIC X.                                   
034300     EJECT                                                                
034400*01  -COPY W0008  -PRE 4111-                                              
034500     05  FILLER                  PIC X.                                   
034600     EJECT                                                                
034700*01  -COPY W0008  -PRE UTSKR-RETA-                                        
034800     05  FILLER                  PIC X.                                   
034900     EJECT                                                                
035000*01  -COPY W0008  -PRE  UTSKR-LISB-                                       
035100     05  FILLER                  PIC X.                                   
035200     EJECT                                                                
035300 PROCEDURE DIVISION  USING MSG-PCB  4743-PCB UTSKR-ALT-PCB                
035400                           USEA-PCB RETA-PCB                              
035500                           RETD-PCB RETF-PCB RETG-PCB 4111-PCB            
035600                           UTSKR-RETA-PCB UTSKR-LISB-PCB.                 
035700     ENTRY 'DLITCBL' USING MSG-PCB  4743-PCB UTSKR-ALT-PCB                
035800                           USEA-PCB RETA-PCB                              
035900                           RETD-PCB RETF-PCB RETG-PCB 4111-PCB            
036000                           UTSKR-RETA-PCB UTSKR-LISB-PCB.                 
036100                                                                          
036200     PERFORM IMS-GET-MSG                                                  
036300     IF SEGMENT-FINNS                                                     
036400       PERFORM A-INIT                                                     
036500       PERFORM B-KOLLA-NYCKLAR                                            
036600       IF NYCKLAR-OK                                                      
036700         IF MFS-UPDATE                                                    
036800           PERFORM G-KOLLA-INPUT                                          
036900           IF INDATA-OK                                                   
037000             PERFORM H-UPPDATERA                                          
037100                                                                          
037200             IF ANTAL-REPL = MAX-ANTAL-REPL AND RADER-KVAR                
037300                 MOVE JA                  TO OMSTART-SW                   
037400                 MOVE RET-IDDC            TO MID-IDDC-SPAR                
037500                 MOVE RET-IDRT            TO MID-IDRT-SPAR                
037600                 MOVE RET-IDRTLOP         TO MID-IDRTLOP-SPAR             
037700                 MOVE RET-IDKOLLI         TO WS-IDKOLLI                   
037800                 MOVE WS-IDKOLLI          TO MID-IDKOLLI-SPAR             
037900                 MOVE RET-DAREGDAT        TO MID-DAREGDAT-SPAR            
038000                 MOVE RET-TIKLOCK         TO WS-TIKLOCK                   
038100                 MOVE WS-TIKLOCK          TO MID-TIKLOCK-SPAR             
038200             END-IF                                                       
038300                                                                          
038400           END-IF                                                         
038500         ELSE                                                             
038600           IF MFS-FIRST                                                   
038700             PERFORM C-FOERSTA-SIDA                                       
038800           ELSE                                                           
038900             IF MFS-NEXT                                                  
039000               PERFORM D-NAESTA-SIDA                                      
039100             ELSE                                                         
039200               PERFORM E-SAMMA-SIDA                                       
039300             END-IF                                                       
039400           END-IF                                                         
039500         END-IF                                                           
039600         IF OMSTART                                                       
039700           CONTINUE                                                       
039800         ELSE                                                             
039900           IF INDATA-OK                                                   
040000             PERFORM F-LAES-VISA-INFO                                     
040100           END-IF                                                         
040200         END-IF                                                           
040300       END-IF                                                             
040400       IF OMSTART                                                         
040500         MOVE 'W4T743U '          TO MSG-KDTRANS-1                        
040600         MOVE '4743'              TO MSG-IDTRANS-1                        
040700         COMPUTE MSG-KVLL = LENGTH OF MID + 17                            
040800         MOVE MID       TO MSG-INDATA-MINUS-1-TRANSKOD                    
040900         PERFORM IMS-ISRT-ALT43                                           
041000       ELSE                                                               
041100         COMPUTE MSG-KVLL = LENGTH OF MOD-W4O74301 + 4                    
041200         PERFORM IMS-INSERT-MSG                                           
041300       END-IF                                                             
041400     END-IF                                                               
041500                                                                          
041600     MOVE ZERO TO RETURN-CODE                                             
041700     GOBACK                                                               
041800     .                                                                    
041900     EJECT                                                                
042000 A-INIT SECTION.                                                          
042100                                                                          
042200     IF MSG-DUBBLA-TRANSKODER                                             
042300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I74301                 
042400       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
042500       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
042600     ELSE                                                                 
042700       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I74301                 
042800       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
042900       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
043000     END-IF                                                               
043100                                                                          
043200     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
043300     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
043400     MOVE MFS-IDTRANS TO W-IDTRANS                                        
043500                                                                          
043600     MOVE LOW-VALUE TO MSG-AREA                                           
043700     MOVE 'W4O74301' TO MFS-IDMOD                                         
043800     MOVE '4743' TO MOD-IDTRANS                                           
043900     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
044000                                                                          
044100     IF EGEN-MID OR HELP-MID                                              
044200       CONTINUE                                                           
044300     ELSE                                                                 
044400       MOVE SPACE TO MFS-KDTRTYP                                          
044500       MOVE '7' TO MFS-IDPFK                                              
044600     END-IF                                                               
044700                                                                          
044800     MOVE FUNCTION CURRENT-DATE (1:8) TO W-DATUM                          
044900                                                                          
045000     MOVE LOW-VALUE         TO W-WDA3BSEQ-MIN-X                           
045100                               W-WDA3C1KY-MIN-X                           
045200                               W-WDA3E1KY-MIN-X                           
045300                                                                          
045400     MOVE HIGH-VALUE        TO W-WDA3BSEQ-MAX-X                           
045500                               W-WDA3C1KY-MAX-X                           
045600                               W-WDA3E1KY-MAX-X                           
045700                                                                          
045800     MOVE NEJ               TO OMSTART-SW                                 
045900     .                                                                    
046000     EJECT                                                                
046100 B-KOLLA-NYCKLAR SECTION.                                                 
046200                                                                          
046300     MOVE ALL '+'              TO MSGI-WMSGINIT                           
046400     MOVE '001'                TO MSGI-KDCALL                             
046500     MOVE MSG-SIGNON-USERID    TO MSGI-IDUSER                             
046600     MOVE '4743'               TO MSGI-IDTRANS                            
046700     MOVE MSG-LTERM-NAME       TO MSGI-IDLTERM-USER                       
046800     IF EGEN-MID                                                          
046900         MOVE MID-IDRTLOP-IN   TO MSGI-IDRTLOP                            
047000     END-IF                                                               
047100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
047200                                                                          
047300     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
047400                                                                          
047500     MOVE JA TO NYCKLAR-SW                                                
047600                                                                          
047700     MOVE MSGI-IDRT-KEY   TO  W-IDRT-BSEQ-MIN                             
047800                              W-IDRT-BSEQ-MAX                             
047900                              W-IDRT-C1-MIN                               
048000                              W-IDRT-C1-MAX                               
048100                              W-IDRT-E1-MIN                               
048200                              W-IDRT-E1-MAX                               
048300                              W-IDRT-4111                                 
048400                                                                          
048500     PERFORM IMS-GU-WL411101                                              
048600                                                                          
048700     MOVE WC-CDC-SE       TO  W-IDDC-BSEQ-MIN                             
048800                              W-IDDC-BSEQ-MAX                             
048900                              W-IDDC-C1-MIN                               
049000                              W-IDDC-C1-MAX                               
049100                              W-IDDC-E1-MIN                               
049200                              W-IDDC-E1-MAX                               
049300                              W-IDDC-F1-MIN                               
049400                              W-IDDC-F1-MAX                               
049500                              W-IDDC                                      
049600                                                                          
049700*    -- KONTROLL AV IDRTLOP                                               
049800     MOVE MFS-RENSA-FAELT TO MOD-IDRTLOP-IN                               
049900     MOVE NEJ             TO SW-GAMMAL-SND                                
050000                                                                          
050100     IF MID-IDRTLOP-IN    NOT = ALL '+'                                   
050200       MOVE '7'           TO MFS-IDPFK                                    
050300       MOVE SPACE         TO MFS-KDTRTYP                                  
050400     END-IF                                                               
050500                                                                          
050600     IF MSGI-IDRTLOP      NUMERIC AND                                     
050700        MSGI-IDRTLOP      > ZERO                                          
050800        MOVE JA           TO SW-GAMMAL-SND                                
050900     END-IF                                                               
051000                                                                          
051100*    -- KONTROLL AV FLAGGA VISA SÄNDNINGSINNEHÅLL                         
051200     MOVE MFS-RENSA-FAELT TO MOD-FLVISA-IN                                
051300                                                                          
051400     IF MID-FLVISA-IN      = ALL '+'                                      
051500       MOVE MID-FLVISA-UT TO W-FLVISA                                     
051600     ELSE                                                                 
051700       MOVE '7'           TO MFS-IDPFK                                    
051800       MOVE SPACE         TO MFS-KDTRTYP                                  
051900       MOVE MID-FLVISA-IN TO W-FLVISA                                     
052000     END-IF                                                               
052100                                                                          
052200     IF W-FLVISA          = JA OR YES                                     
052300        MOVE JA           TO SW-VISA-SND                                  
052400     ELSE                                                                 
052500        MOVE NEJ          TO SW-VISA-SND                                  
052600                             W-FLVISA                                     
052700     END-IF                                                               
052800                                                                          
052900     IF VISA-SND AND MSGI-IDRTLOP NOT NUMERIC                             
053000        MOVE NEJ          TO NYCKLAR-SW                                   
053100     END-IF                                                               
053200                                                                          
053300     IF GODK-MID OR NYCKLAR-OK                                            
053400       IF MSGI-IDRTLOP NUMERIC                                            
053500          MOVE MSGI-IDRTLOP      TO MOD-IDRTLOP-UT                        
053600                                    W-IDRTLOP-NUM                         
053700          INSPECT MOD-IDRTLOP-UT REPLACING LEADING ZERO BY SPACE          
053800       ELSE                                                               
053900          MOVE MFS-RENSA-FAELT   TO MOD-IDRTLOP-UT                        
054000       END-IF                                                             
054100       IF W-FLVISA = JA                                                   
054200         IF MSGI-IDLAND-SPR = 'GB '                                       
054300           MOVE YES                  TO MOD-FLVISA-UT                     
054400         ELSE                                                             
054500           MOVE W-FLVISA             TO MOD-FLVISA-UT                     
054600         END-IF                                                           
054700       ELSE                                                               
054800         MOVE W-FLVISA             TO MOD-FLVISA-UT                       
054900       END-IF                                                             
055000     ELSE                                                                 
055100       MOVE MFS-RENSA-FAELT      TO MOD-IDRTLOP-UT                        
055200                                    MOD-FLVISA-UT                         
055300     END-IF                                                               
055400                                                                          
055500*    -- KONTROLL AV IDRT-KEY                                              
055600     IF MSGI-IDRT-KEY = SPACE OR                                          
055700        MSGI-IDRT-KEY = 'CDC' OR                                          
055800        MSGI-IDRT-KEY = 'US1' OR                                          
056000        MSGI-IDRT-KEY = 'US3' OR                                          
056100        MSGI-IDRT-KEY = 'US4' OR                                          
056101        MSGI-IDRT-KEY = 'US5' OR                                          
056102        MSGI-IDRT-KEY = 'US6' OR                                          
056103        MSGI-IDRT-KEY = 'ET2' OR                                          
056110        MSGI-IDRT-KEY = 'CA1'                                             
056200        MOVE NEJ            TO NYCKLAR-SW                                 
056300     END-IF                                                               
056400                                                                          
056500     IF NYCKLAR-FEL                                                       
056600       IF MSGI-IDRT-KEY = SPACE OR                                        
056700          MSGI-IDRT-KEY = 'CDC' OR                                        
056800          MSGI-IDRT-KEY = 'US1' OR                                        
057000          MSGI-IDRT-KEY = 'US3' OR                                        
057100          MSGI-IDRT-KEY = 'US4' OR                                        
057101          MSGI-IDRT-KEY = 'US5' OR                                        
057102          MSGI-IDRT-KEY = 'US6' OR                                        
057103          MSGI-IDRT-KEY = 'ET2' OR                                        
057110          MSGI-IDRT-KEY = 'CA1'                                           
057200         MOVE MFS-RENSA-FAELT TO MOD-IDRTLOP-UT                           
057300                                 MOD-FLVISA-UT                            
057400         MOVE ERR-OTILL-UPD   TO MED-IDMFSFEL                             
057500       ELSE                                                               
057600         MOVE ERR-WRONG-KEY   TO MED-IDMFSFEL                             
057700       END-IF                                                             
057800       CALL WMEDKONV USING MED-WMEDAREA                                   
057900       MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                 
058000       PERFORM MFS-RENSA-FAELT-IN                                         
058100       PERFORM MFS-RENSA-FAELT-UT                                         
058200     END-IF                                                               
058300                                                                          
058400     IF EGEN-MID                                                          
058500       IF MID-OMSTART-NYCKLAR NOT = ALL '+'                               
058600                                                                          
058700         IF MID-IDKOLLI-SPAR NUMERIC  AND                                 
058800            MID-IDKOLLI-SPAR > ZERO                                       
058900                                                                          
059000           MOVE MID-IDKOLLI-SPAR      TO W-IDKOLLI-BSEQ                   
059100           MOVE MID-IDDC-SPAR         TO W-IDDC-BSEQ                      
059200                                         W-IDDC-BSEQ-MIN                  
059300                                         W-IDDC-BSEQ-MAX                  
059400           MOVE MID-IDRT-SPAR         TO W-IDRT-BSEQ                      
059500                                         W-IDRT-BSEQ-MIN                  
059600                                         W-IDRT-BSEQ-MAX                  
059700           MOVE MID-IDRTLOP-SPAR      TO W-IDRTLOP-BSEQ                   
059800                                         W-IDRTLOP-BSEQ-MIN               
059900                                         W-IDRTLOP-BSEQ-MAX               
060000           MOVE MID-IDKOLLI-SPAR      TO W-IDKOLLI-BSEQ                   
060100           MOVE MID-DAREGDAT-SPAR     TO W-DAREGDAT-A3                    
060200           MOVE MID-TIKLOCK-SPAR      TO W-TIKLOCK-A3                     
060300           MOVE JA                    TO OMSTART-SW                       
060400         END-IF                                                           
060500       END-IF                                                             
060600     END-IF                                                               
060700     .                                                                    
060800     EJECT                                                                
060900 C-FOERSTA-SIDA SECTION.                                                  
061000                                                                          
061100     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
061200     CALL WMEDKONV USING MED-WMEDAREA                                     
061300     MOVE MED-MFSINF     TO MOD-TEMFSFEL                                  
061400                                                                          
061500*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
061600     PERFORM MFS-RENSA-FAELT-IN                                           
061700     .                                                                    
061800     EJECT                                                                
061900 D-NAESTA-SIDA SECTION.                                                   
062000                                                                          
062100     MOVE MSGI-SPAR-AREA            TO W-MINKEY-X                         
062200     IF W-MINKEY-IDTRANS = '4743'                                         
062300       MOVE W-MINKEY-WDA3BSEQ-NEXT  TO W-WDA3BSEQ-MIN-X                   
062400     ELSE                                                                 
062500        MOVE LOW-VALUE              TO W-WDA3BSEQ-MIN-X                   
062600        MOVE WC-CDC-SE              TO W-IDDC-BSEQ-MIN                    
062700        PERFORM MFS-RENSA-FAELT-IN                                        
062800     END-IF                                                               
062900     .                                                                    
063000     EJECT                                                                
063100 E-SAMMA-SIDA SECTION.                                                    
063200                                                                          
063300      MOVE MSGI-SPAR-AREA     TO W-MINKEY-X                               
063400      IF W-MINKEY-IDTRANS = '4743'                                        
063500        MOVE W-MINKEY-WDA3BSEQ-ENTER  TO W-WDA3BSEQ-MIN-X                 
063600        IF MID-INPUT             =  ALL '+'                               
063700           PERFORM MFS-RENSA-FAELT-IN                                     
063800        ELSE                                                              
063900           MOVE INF-PRESS-PF11   TO MED-IDMFSINF                          
064000           CALL WMEDKONV USING MED-WMEDAREA                               
064100           MOVE MED-MFSINF       TO MOD-TEMFSFEL                          
064200           PERFORM EA-MID-INDATA-TILL-MOD                                 
064300        END-IF                                                            
064400     ELSE                                                                 
064500        MOVE LOW-VALUE           TO W-WDA3BSEQ-MIN-X                      
064600        MOVE WC-CDC-SE           TO W-IDDC-BSEQ-MIN                       
064700        PERFORM MFS-RENSA-FAELT-IN                                        
064800     END-IF                                                               
064900     .                                                                    
065000     EJECT                                                                
065100 EA-MID-INDATA-TILL-MOD SECTION.                                          
065200                                                                          
065300     IF MID-FLNYSNDN               NOT = ALL '+'                          
065400        MOVE MID-FLNYSNDN          TO MOD-FLNYSNDN                        
065500        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLNYSNDN-ATTR                   
065600     ELSE                                                                 
065700        MOVE MFS-RENSA-FAELT       TO MOD-FLNYSNDN                        
065800     END-IF                                                               
065900                                                                          
066000     IF MID-FLSNDDOK               NOT = ALL '+'                          
066100        MOVE MID-FLSNDDOK          TO MOD-FLSNDDOK                        
066200        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLSNDDOK-ATTR                   
066300     ELSE                                                                 
066400        MOVE MFS-RENSA-FAELT       TO MOD-FLSNDDOK                        
066500     END-IF                                                               
066600                                                                          
066700     MOVE +1                          TO INDX                             
066800     PERFORM UNTIL INDX               >  MAX-INDX                         
066900        IF MID-KDCMD(INDX)            NOT = ALL '+'                       
067000           MOVE MID-KDCMD(INDX)       TO MOD-KDCMD(INDX)                  
067100           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-ATTR(INDX)             
067200        ELSE                                                              
067300           MOVE MFS-RENSA-FAELT       TO MOD-KDCMD(INDX)                  
067400        END-IF                                                            
067500        ADD +1                        TO INDX                             
067600     END-PERFORM                                                          
067700     .                                                                    
067800     EJECT                                                                
067900 F-LAES-VISA-INFO SECTION.                                                
068000                                                                          
068100     MOVE HIGH-VALUE           TO W-WDA3BSEQ-MAX-X                        
068200     MOVE MSGI-IDRT-KEY        TO W-IDRT-BSEQ-MAX                         
068300     MOVE WC-CDC-SE            TO W-IDDC-BSEQ-MAX                         
068400                                                                          
068500     IF VISA-SND                                                          
068600        MOVE MSGI-IDRTLOP      TO W-IDRTLOP-BSEQ-MIN                      
068700                                  W-IDRTLOP-BSEQ-MAX                      
068800     ELSE                                                                 
068900        MOVE ZERO              TO W-IDRTLOP-BSEQ-MIN                      
069000                                  W-IDRTLOP-BSEQ-MAX                      
069100     END-IF                                                               
069200                                                                          
069300     PERFORM IMS-GHU-SEQB-WLRETA01                                        
069400                                                                          
069500     PERFORM FA-FIXA-ENTER-KEY                                            
069600     IF SEGMENT-SAKNAS                                                    
069700        MOVE ERR-KOLLI-SAKNAS   TO MED-IDMFSFEL                           
069800        CALL WMEDKONV USING MED-WMEDAREA                                  
069900        MOVE MED-MFSFEL         TO MOD-TEMFSFEL                           
070000        PERFORM MFS-RENSA-FAELT-UT                                        
070100     ELSE                                                                 
070200       MOVE +1                  TO INDX                                   
070300                                                                          
070400       PERFORM UNTIL INDX        > MAX-INDX                               
070500         IF SEGMENT-FINNS                                                 
070600            IF RET-IDKOLLI       =  W-SPAR-IDKOLLI                        
070700               CONTINUE                                                   
070800            ELSE                                                          
070900               MOVE RET-IDKOLLI  TO MOD-IDKOLLI(INDX)                     
071000                                    W-SPAR-IDKOLLI                        
071100               MOVE RET-FLFARLIG TO MOD-FLFARLIG(INDX)                    
071200                                                                          
071300               ADD 1             TO INDX                                  
071400            END-IF                                                        
071500            PERFORM IMS-GHN-SEQB-WLRETA01                                 
071600         ELSE                                                             
071700            MOVE MFS-RENSA-FAELT  TO MOD-KDCMD    (INDX)                  
071800            MOVE MFS-STAENG-FAELT TO MOD-KDCMD-ATTR(INDX)                 
071900            MOVE MFS-RENSA-FAELT  TO MOD-IDKOLLI  (INDX)                  
072000                                     MOD-FLFARLIG (INDX)                  
072100            ADD 1                 TO INDX                                 
072200         END-IF                                                           
072300       END-PERFORM                                                        
072400                                                                          
072500     END-IF                                                               
072600                                                                          
072700     PERFORM FB-FIXA-NEXT-KEY                                             
072800                                                                          
072900     MOVE '002'                     TO MSGI-KDCALL                        
073000     MOVE '4743'                    TO MSGI-IDTRANS                       
073100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
073200                                                                          
073300     .                                                                    
073400     EJECT                                                                
073500                                                                          
073600 FA-FIXA-ENTER-KEY        SECTION.                                        
073700                                                                          
073800     IF SEGMENT-FINNS                                                     
073900        MOVE RET-IDDC               TO W-MINKEYB1-IDDC                    
074000        MOVE RET-IDRT               TO W-MINKEYB1-IDRT                    
074100        MOVE RET-IDRTLOP            TO W-MINKEYB1-IDRTLOP                 
074200        MOVE RET-IDKOLLI            TO W-MINKEYB1-IDKOLLI                 
074300     ELSE                                                                 
074400        MOVE WC-CDC-SE              TO W-MINKEYB1-IDDC                    
074500        MOVE ZERO                   TO W-MINKEYB1-IDRT                    
074600                                       W-MINKEYB1-IDRTLOP                 
074700                                       W-MINKEYB1-IDKOLLI                 
074800     END-IF                                                               
074900     MOVE '4743'                    TO W-MINKEY-IDTRANS                   
075000     MOVE W-MINKEY-X                TO MSGI-SPAR-AREA                     
075100                                                                          
075200     .                                                                    
075300     EJECT                                                                
075400                                                                          
075500 FB-FIXA-NEXT-KEY        SECTION.                                         
075600                                                                          
075700     IF SEGMENT-FINNS                                                     
075800        MOVE INF-MORE-INFO-EXISTS   TO MED-IDMFSINF                       
075900        CALL WMEDKONV USING MED-WMEDAREA                                  
076000        MOVE MED-TEMFSINF           TO MOD-TEMFSINF                       
076100                                                                          
076200        MOVE RET-IDDC               TO W-MINKEYB1-IDDC-NEXT               
076300        MOVE RET-IDRT               TO W-MINKEYB1-IDRT-NEXT               
076400        MOVE RET-IDRTLOP            TO W-MINKEYB1-IDRTLOP-NEXT            
076500        MOVE RET-IDKOLLI            TO W-MINKEYB1-IDKOLLI-NEXT            
076600     ELSE                                                                 
076700        MOVE WC-CDC-SE              TO W-MINKEYB1-IDDC-NEXT               
076800        MOVE ZERO                   TO W-MINKEYB1-IDRT-NEXT               
076900                                       W-MINKEYB1-IDRTLOP-NEXT            
077000                                       W-MINKEYB1-IDKOLLI-NEXT            
077100     END-IF                                                               
077200     MOVE '4743'                    TO W-MINKEY-IDTRANS                   
077300     MOVE W-MINKEY-X                TO MSGI-SPAR-AREA                     
077400                                                                          
077500     .                                                                    
077600     EJECT                                                                
077700 G-KOLLA-INPUT SECTION.                                                   
077800                                                                          
077900     MOVE ZERO  TO MED-IDMFSFEL                                           
078000     MOVE JA  TO INDATA-SW                                                
078100                                                                          
078200     PERFORM GA-FORMELL-KONTROLL                                          
078300     IF INDATA-OK                                                         
078400        PERFORM GB-LOGISK-KONTROLL                                        
078500     END-IF                                                               
078600                                                                          
078700     IF INDATA-FEL                                                        
078800        IF MED-IDMFSFEL = ZERO                                            
078900          MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                       
079000        END-IF                                                            
079100        CALL WMEDKONV USING MED-WMEDAREA                                  
079200        MOVE MED-MFSFEL           TO MOD-TEMFSFEL                         
079300        IF MED-IDMFSFEL = ERR-EJ-LASTAD                                   
079400          IF MED-IDSKYLT = 'S'                                            
079500            MOVE 'KOLLINR '       TO MOD-TEMFSFEL (15:8)                  
079600          ELSE                                                            
079700            MOVE 'CASE NO '       TO MOD-TEMFSFEL (15:8)                  
079800          END-IF                                                          
079900          MOVE W-FEL-IDKOLLI    TO MOD-TEMFSFEL (23:5)                    
080000        END-IF                                                            
080100        PERFORM MFS-ROER-EJ-FAELT-UT                                      
080200        PERFORM MFS-ROER-EJ-FAELT-IN                                      
080300     END-IF                                                               
080400                                                                          
080500     .                                                                    
080600     EJECT                                                                
080700 GA-FORMELL-KONTROLL SECTION.                                             
080800                                                                          
080900     IF MID-INPUT                = ALL '+'                                
081000       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
081100       CALL WMEDKONV USING MED-WMEDAREA                                   
081200       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
081300       PERFORM MFS-ROER-EJ-FAELT-IN                                       
081400       PERFORM MFS-ROER-EJ-FAELT-UT                                       
081500       MOVE NEJ                  TO INDATA-SW                             
081600     ELSE                                                                 
081700                                                                          
081800       PERFORM GAA-KOLLA-NY-SND                                           
081900                                                                          
082000       PERFORM GAB-KOLLA-KDCMD                                            
082100                                                                          
082200       PERFORM GAC-KOLLA-SNDDOK                                           
082300                                                                          
082400       PERFORM GAD-KOLLA-ANT-FUNKTIONER                                   
082500     END-IF                                                               
082600                                                                          
082700     .                                                                    
082800     EJECT                                                                
082900 GAA-KOLLA-NY-SND     SECTION.                                            
083000                                                                          
083100     MOVE NEJ                     TO SW-SKAPA-NY-SND                      
083200                                                                          
083300     IF MID-FLNYSNDN               NOT = ALL '+'                          
083400       IF MID-FLNYSNDN             =  JA OR YES                           
083500         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLNYSNDN-ATTR                   
083600         MOVE JA                   TO SW-SKAPA-NY-SND                     
083700       ELSE                                                               
083800         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLNYSNDN-ATTR                   
083900         MOVE NEJ                  TO INDATA-SW                           
084000       END-IF                                                             
084100     END-IF                                                               
084200                                                                          
084300     .                                                                    
084400     EJECT                                                                
084500 GAB-KOLLA-KDCMD      SECTION.                                            
084600                                                                          
084700     MOVE NEJ               TO  SW-RAD-CMD                                
084800     MOVE +1                TO INDX                                       
084900                                                                          
085000     PERFORM UNTIL INDX     >  MAX-INDX                                   
085100        IF MID-KDCMD(INDX)  NOT = ALL '+'                                 
085200           MOVE JA          TO  SW-RAD-CMD                                
085300                                                                          
085400           IF VISA-SND                                                    
085500              IF MID-KDCMD(INDX)          = W-BORTTAG OR W-DELETE         
085600                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-ATTR(INDX)        
085700              ELSE                                                        
085800                 MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-ATTR(INDX)        
085900                 MOVE NEJ                  TO INDATA-SW                   
086000              END-IF                                                      
086100           ELSE                                                           
086200              IF MID-KDCMD(INDX)           =  W-LASTA                     
086300                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-ATTR(INDX)        
086400              ELSE                                                        
086500                 MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-ATTR(INDX)        
086600                 MOVE NEJ                  TO INDATA-SW                   
086700              END-IF                                                      
086800                                                                          
086900              IF W-IDRTLOP-NUM              =  ZERO                       
087000                MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-ATTR(INDX)         
087100                MOVE NEJ                  TO INDATA-SW                    
087200                MOVE ERR-WRONG-KEY        TO MED-IDMFSFEL                 
087300              END-IF                                                      
087400                                                                          
087500           END-IF                                                         
087600        END-IF                                                            
087700        ADD +1             TO INDX                                        
087800     END-PERFORM                                                          
087900                                                                          
088000     .                                                                    
088100     EJECT                                                                
088200 GAC-KOLLA-SNDDOK     SECTION.                                            
088300                                                                          
088400     MOVE NEJ                     TO SW-SKAPA-SNDDOK                      
088500                                                                          
088600     IF MID-FLSNDDOK               NOT = ALL '+'                          
088700       IF MID-FLSNDDOK             =  JA OR YES                           
088800         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLSNDDOK-ATTR                   
088900         MOVE JA                   TO SW-SKAPA-SNDDOK                     
089000       ELSE                                                               
089100         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLSNDDOK-ATTR                   
089200         MOVE NEJ                  TO INDATA-SW                           
089300       END-IF                                                             
089400     END-IF                                                               
089500                                                                          
089600     .                                                                    
089700     EJECT                                                                
089800 GAD-KOLLA-ANT-FUNKTIONER SECTION.                                        
089900                                                                          
090000     IF SKAPA-NY-SND                                                      
090100        IF RAD-CMD OR SKAPA-SNDDOK                                        
090200           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLNYSNDN-ATTR                   
090300           MOVE NEJ                TO INDATA-SW                           
090400           MOVE ERR-FLERA-FUNKTIONER TO MED-IDMFSFEL                      
090500        END-IF                                                            
090600     END-IF                                                               
090700                                                                          
090800     IF RAD-CMD                                                           
090900        IF SKAPA-NY-SND                                                   
091000           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLNYSNDN-ATTR                   
091100           MOVE NEJ                TO INDATA-SW                           
091200           MOVE ERR-FLERA-FUNKTIONER TO MED-IDMFSFEL                      
091300        END-IF                                                            
091400                                                                          
091500        IF SKAPA-SNDDOK                                                   
091600           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLSNDDOK-ATTR                   
091700           MOVE NEJ                TO INDATA-SW                           
091800           MOVE ERR-FLERA-FUNKTIONER TO MED-IDMFSFEL                      
091900        END-IF                                                            
092000     END-IF                                                               
092100                                                                          
092200     IF SKAPA-SNDDOK                                                      
092300        IF SKAPA-NY-SND                                                   
092400           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLNYSNDN-ATTR                   
092500           MOVE NEJ                TO INDATA-SW                           
092600           MOVE ERR-FLERA-FUNKTIONER TO MED-IDMFSFEL                      
092700        END-IF                                                            
092800                                                                          
092900        IF RAD-CMD                                                        
093000           MOVE MFS-NUM-FAELT-FEL  TO MOD-FLSNDDOK-ATTR                   
093100           MOVE NEJ                TO INDATA-SW                           
093200           MOVE ERR-FLERA-FUNKTIONER TO MED-IDMFSFEL                      
093300        END-IF                                                            
093400     END-IF                                                               
093500                                                                          
093600     .                                                                    
093700     EJECT                                                                
093800 GB-LOGISK-KONTROLL SECTION.                                              
093900                                                                          
094000     IF RAD-CMD                                                           
094100        PERFORM GBA-KOLLA-VALDA-RADER                                     
094200     END-IF                                                               
094300                                                                          
094400     IF SKAPA-SNDDOK                                                      
094500       PERFORM GBB-KOLLA-ALLA-ING-RT                                      
094600     END-IF                                                               
094700                                                                          
094800     .                                                                    
094900     EJECT                                                                
095000 GBA-KOLLA-VALDA-RADER    SECTION.                                        
095100                                                                          
095200     PERFORM GBAA-KOLLA-SNDSTATUS                                         
095300     MOVE +1                 TO INDX                                      
095400     PERFORM UNTIL INDX      >  MAX-INDX                                  
095500        IF MID-KDCMD(INDX)   =  W-LASTA OR W-BORTTAG OR W-DELETE          
095600           PERFORM GBAB-KOLLA-VALT-KOLLI                                  
095700        END-IF                                                            
095800        ADD +1               TO INDX                                      
095900     END-PERFORM                                                          
096000                                                                          
096100     .                                                                    
096200     EJECT                                                                
096300 GBAA-KOLLA-SNDSTATUS           SECTION.                                  
096400                                                                          
096500     MOVE MSGI-IDRTLOP                 TO W-IDRTLOP-BSEQ-MIN              
096600                                          W-IDRTLOP-BSEQ-MAX              
096700     PERFORM GBAAA-KOLLA-IDRTLOP                                          
096800                                                                          
096900     PERFORM IMS-GHU-SEQB-WLRETA01                                        
097000                                                                          
097100     IF SEGMENT-FINNS                                                     
097200        IF RET-KDRETSTA          NOT   = W-SND-TERM                       
097300            MOVE +1                    TO  INDX                           
097400            PERFORM UNTIL INDX         >  MAX-INDX                        
097500               IF MID-KDCMD(INDX)         =  W-LASTA                      
097600                  MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR(INDX)         
097700                  MOVE NEJ                TO INDATA-SW                    
097800                  MOVE ERR-OTILL-STATUS   TO MED-IDMFSFEL                 
097900               END-IF                                                     
098000               ADD +1                  TO INDX                            
098100            END-PERFORM                                                   
098200        END-IF                                                            
098300     END-IF                                                               
098400                                                                          
098500     .                                                                    
098600     EJECT                                                                
098700 GBAAA-KOLLA-IDRTLOP               SECTION.                               
098800                                                                          
098900     PERFORM IMS-GHNP-WL411111                                            
099000     IF W-IDRTLOP-NUM                  >  4112-IDRTLOP                    
099100         MOVE +1                       TO INDX                            
099200         PERFORM UNTIL INDX            >  MAX-INDX                        
099300            IF MID-KDCMD(INDX)         =  W-LASTA                         
099400               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR(INDX)            
099500               MOVE NEJ                TO INDATA-SW                       
099600               MOVE ERR-WRONG-KEY      TO MED-IDMFSFEL                    
099700            END-IF                                                        
099800            ADD +1                     TO INDX                            
099900         END-PERFORM                                                      
100000     END-IF                                                               
100100                                                                          
100200     .                                                                    
100300     EJECT                                                                
100400 GBAB-KOLLA-VALT-KOLLI             SECTION.                               
100500                                                                          
100600     MOVE LOW-VALUE            TO W-WDA3BSEQ-MIN-X                        
100700     MOVE HIGH-VALUE           TO W-WDA3BSEQ-MAX-X                        
100800     MOVE MSGI-IDRT-KEY        TO W-IDRT-BSEQ-MIN                         
100900                                  W-IDRT-BSEQ-MAX                         
101000     MOVE WC-CDC-SE            TO W-IDDC-BSEQ-MIN                         
101100                                  W-IDDC-BSEQ-MAX                         
101200                                                                          
101300     IF VISA-SND                                                          
101400        MOVE MSGI-IDRTLOP      TO W-IDRTLOP-BSEQ-MIN                      
101500                                  W-IDRTLOP-BSEQ-MAX                      
101600     ELSE                                                                 
101700        MOVE ZERO              TO W-IDRTLOP-BSEQ-MIN                      
101800                                  W-IDRTLOP-BSEQ-MAX                      
101900     END-IF                                                               
102000                                                                          
102100     MOVE MID-IDKOLLI(INDX)        TO W-IDKOLLI-BSEQ-MIN                  
102200                                      W-IDKOLLI-BSEQ-MAX                  
102300     PERFORM IMS-GHU-SEQB-WLRETA01                                        
102400     IF SEGMENT-SAKNAS                                                    
102500        MOVE ERR-OTILL-UPD      TO MED-IDMFSFEL                           
102600        MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR(INDX)                   
102700        MOVE NEJ                TO INDATA-SW                              
102800     END-IF                                                               
102900     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
103000        IF MID-KDCMD(INDX)             = W-LASTA                          
103100           IF RET-KDKOLSTA             NOT = W-KLI-PACKAT                 
103200             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR(INDX)              
103300             MOVE NEJ                TO INDATA-SW                         
103400             MOVE ERR-OTILL-STATUS   TO MED-IDMFSFEL                      
103500           ELSE                                                           
103600             MOVE RET-IDDC         TO W-IDDC-F1-MIN                       
103700                                      W-IDDC-F1-MAX                       
103800             MOVE RET-IDDISTR      TO W-IDDISTR-F1-MIN                    
103900                                      W-IDDISTR-F1-MAX                    
104000             MOVE RET-IDKUNDNR     TO W-IDKUNDNR-F1-MIN                   
104100                                      W-IDKUNDNR-F1-MAX                   
104200             MOVE RET-IDRAPPNR     TO W-IDRAPPNR-F1-MIN                   
104300                                      W-IDRAPPNR-F1-MAX                   
104400             PERFORM IMS-GU-WLRETG01                                      
104500             PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                 
104600               IF SEQF-IDRTLOP  = ZERO  OR MSGI-IDRTLOP                   
104700                  CONTINUE                                                
104800               ELSE                                                       
104900                  MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR(INDX)         
105000                  MOVE NEJ                TO INDATA-SW                    
105100                  MOVE ERR-REDAN-LASTAD   TO MED-IDMFSFEL                 
105200               END-IF                                                     
105300               PERFORM IMS-GN-WLRETG01                                    
105400             END-PERFORM                                                  
105500           END-IF                                                         
105600        END-IF                                                            
105700                                                                          
105800        IF MID-KDCMD(INDX)             = W-BORTTAG OR W-DELETE            
105900           IF RET-KDKOLSTA             NOT = W-KLI-LASTAT                 
106000               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR(INDX)            
106100               MOVE NEJ                TO INDATA-SW                       
106200               MOVE ERR-OTILL-STATUS   TO MED-IDMFSFEL                    
106300           END-IF                                                         
106400        END-IF                                                            
106500        PERFORM IMS-GHN-SEQB-WLRETA01                                     
106600     END-PERFORM                                                          
106700                                                                          
106800     .                                                                    
106900     EJECT                                                                
107000 GBB-KOLLA-ALLA-ING-RT SECTION.                                           
107100                                                                          
107200     MOVE ZERO                TO  W-SPAR-IDDISTR                          
107300                                  W-SPAR-IDKUNDNR                         
107400                                  W-SPAR-IDRAPPNR                         
107500                                                                          
107600     MOVE W-SND-TERM          TO  W-KDRETSTA-C1-MIN                       
107700                                  W-KDRETSTA-C1-MAX                       
107800     MOVE ZERO                TO  W-DARETANK-C1-MIN                       
107900                                  W-DARETANK-C1-MAX                       
108000     MOVE MSGI-IDRTLOP        TO  W-IDRTLOP-C1-MIN                        
108100                                  W-IDRTLOP-C1-MAX                        
108200     MOVE WC-CDC-SE           TO  W-IDDC-C1-MIN                           
108300                                  W-IDDC-C1-MAX                           
108400                                                                          
108500     PERFORM IMS-GU-WLRETD01                                              
108600     IF SEGMENT-SAKNAS                                                    
108700        MOVE NEJ           TO INDATA-SW                                   
108800        MOVE ERR-OTILL-STATUS TO MED-IDMFSFEL                             
108900     END-IF                                                               
109000     PERFORM UNTIL SEGMENT-SAKNAS OR INDATA-FEL                           
109100        IF SEQC-IDDISTR           = W-SPAR-IDDISTR  AND                   
109200           SEQC-IDKUNDNR          = W-SPAR-IDKUNDNR AND                   
109300           SEQC-IDRAPPNR          = W-SPAR-IDRAPPNR                       
109400            CONTINUE                                                      
109500        ELSE                                                              
109600            MOVE SEQC-IDDC        TO W-IDDC-E1-MIN                        
109700                                     W-IDDC-E1-MAX                        
109800            MOVE SEQC-IDDISTR     TO W-IDDISTR-E1-MIN                     
109900                                     W-IDDISTR-E1-MAX                     
110000                                     W-SPAR-IDDISTR                       
110100            MOVE SEQC-IDKUNDNR    TO W-IDKUNDNR-E1-MIN                    
110200                                     W-IDKUNDNR-E1-MAX                    
110300                                     W-SPAR-IDKUNDNR                      
110400            MOVE SEQC-IDRAPPNR    TO W-IDRAPPNR-E1-MIN                    
110500                                     W-IDRAPPNR-E1-MAX                    
110600                                     W-SPAR-IDRAPPNR                      
110700                                                                          
110800            PERFORM IMS-GU-WLRETF01                                       
110900                                                                          
111000            IF SEGMENT-FINNS                                              
111100               MOVE SEQE-IDKOLLI  TO W-FEL-IDKOLLI                        
111200               MOVE NEJ           TO INDATA-SW                            
111300               MOVE ERR-EJ-LASTAD TO MED-IDMFSFEL                         
111400            END-IF                                                        
111500        END-IF                                                            
111600        PERFORM IMS-GN-WLRETD01                                           
111700     END-PERFORM                                                          
111800                                                                          
111900     .                                                                    
112000     EJECT                                                                
112100 H-UPPDATERA SECTION.                                                     
112200                                                                          
112300     IF SKAPA-NY-SND                                                      
112400        PERFORM HA-TA-UT-IDRTLOP                                          
112500     END-IF                                                               
112600                                                                          
112700     IF RAD-CMD                                                           
112800        PERFORM HB-UPPDATERA-VALDA-KOLLIN                                 
112900     END-IF                                                               
113000                                                                          
113100     IF SKAPA-SNDDOK                                                      
113200       PERFORM HC-UPPDATERA-SND-KLAR                                      
113300     END-IF                                                               
113400                                                                          
113500     MOVE INF-UPDATE-DONE        TO MED-IDMFSINF                          
113600     CALL WMEDKONV USING MED-WMEDAREA                                     
113700     MOVE MED-MFSINF             TO MOD-TEMFSINF                          
113800     PERFORM MFS-FORM-ATTR                                                
113900     PERFORM MFS-RENSA-FAELT-IN                                           
114000     .                                                                    
114100     EJECT                                                                
114200                                                                          
114300 HA-TA-UT-IDRTLOP SECTION.                                                
114400                                                                          
114500     PERFORM IMS-GHNP-WL411111                                            
114600     COMPUTE 4112-IDRTLOP = 4112-IDRTLOP + 1                              
114700     IF 4112-IDRTLOP = ZERO                                               
114800       ADD +1 TO 4112-IDRTLOP                                             
114900     END-IF                                                               
115000                                                                          
115100     PERFORM IMS-REPL-WL411111                                            
115200                                                                          
115300     MOVE 4112-IDRTLOP    TO W-IDRTLOP-NUM                                
115400     MOVE W-IDRTLOP-NUM   TO MOD-IDRTLOP-UT                               
115500                             MSGI-IDRTLOP                                 
115600     INSPECT MOD-IDRTLOP-UT REPLACING LEADING ZERO BY SPACE               
115700                                                                          
115800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
115900     .                                                                    
116000     EJECT                                                                
116100                                                                          
116200 HB-UPPDATERA-VALDA-KOLLIN    SECTION.                                    
116300                                                                          
116400     MOVE +1                 TO INDX                                      
116500     PERFORM UNTIL INDX      >  MAX-INDX                                  
116600        IF MID-KDCMD(INDX)   =  W-LASTA                                   
116700           PERFORM HBA-LASTA-VALT-KLI-I-SANDN                             
116800        END-IF                                                            
116900                                                                          
117000        IF MID-KDCMD(INDX)   =  W-BORTTAG OR W-DELETE                     
117100           PERFORM HBB-TA-BORT-VALT-KLI-UR-SANDN                          
117200        END-IF                                                            
117300        ADD +1               TO INDX                                      
117400     END-PERFORM                                                          
117500                                                                          
117600     .                                                                    
117700     EJECT                                                                
117800                                                                          
117900 HBA-LASTA-VALT-KLI-I-SANDN  SECTION.                                     
118000                                                                          
118100     MOVE WC-CDC-SE                TO W-IDDC-BSEQ-MIN                     
118200                                      W-IDDC-BSEQ-MAX                     
118300     MOVE ZERO                     TO W-IDRTLOP-BSEQ-MIN                  
118400                                      W-IDRTLOP-BSEQ-MAX                  
118500     INSPECT MID-IDKOLLI(INDX) REPLACING LEADING SPACE BY ZERO            
118600     MOVE MID-IDKOLLI(INDX)        TO W-IDKOLLI-BSEQ-MIN                  
118700                                      W-IDKOLLI-BSEQ-MAX                  
118800     PERFORM IMS-GHU-SEQB-WLRETA01                                        
118900     PERFORM UNTIL SEGMENT-SAKNAS                                         
119000        MOVE MSGI-IDRTLOP        TO RET-IDRTLOP                           
119100        MOVE W-KLI-LASTAT        TO RET-KDKOLSTA                          
119200        PERFORM IMS-REPL-SEQB-WLRETA01                                    
119300        PERFORM IMS-GHN-SEQB-WLRETA01                                     
119400     END-PERFORM                                                          
119500     .                                                                    
119600     EJECT                                                                
119700                                                                          
119800 HBB-TA-BORT-VALT-KLI-UR-SANDN  SECTION.                                  
119900                                                                          
120000     MOVE WC-CDC-SE                TO W-IDDC-BSEQ-MIN                     
120100                                      W-IDDC-BSEQ-MAX                     
120200     MOVE MSGI-IDRTLOP             TO W-IDRTLOP-BSEQ-MIN                  
120300                                      W-IDRTLOP-BSEQ-MAX                  
120400     INSPECT MID-IDKOLLI(INDX) REPLACING LEADING SPACE BY ZERO            
120500     MOVE MID-IDKOLLI(INDX)        TO W-IDKOLLI-BSEQ-MIN                  
120600                                      W-IDKOLLI-BSEQ-MAX                  
120700     PERFORM IMS-GHU-SEQB-WLRETA01                                        
120800     PERFORM UNTIL SEGMENT-SAKNAS                                         
120900        MOVE ZERO                  TO RET-IDRTLOP                         
121000        MOVE W-KLI-PACKAT          TO RET-KDKOLSTA                        
121100        PERFORM IMS-REPL-SEQB-WLRETA01                                    
121200        PERFORM IMS-GHN-SEQB-WLRETA01                                     
121300     END-PERFORM                                                          
121400     .                                                                    
121500     EJECT                                                                
121600 HC-UPPDATERA-SND-KLAR  SECTION.                                          
121700                                                                          
121800     MOVE ZERO                  TO ANTAL-REPL                             
121900     MOVE JA                    TO RADER-KVAR-SW                          
122000                                                                          
122100     IF NOT OMSTART                                                       
122200       MOVE WC-CDC-SE           TO  W-IDDC-BSEQ-MIN                       
122300                                    W-IDDC-BSEQ-MAX                       
122400       MOVE MSGI-IDRTLOP        TO  W-IDRTLOP-BSEQ-MIN                    
122500                                    W-IDRTLOP-BSEQ-MAX                    
122600       PERFORM IMS-GHU-SEQB-WLRETA01                                      
122700     ELSE                                                                 
122800       PERFORM IMS-GHU-SEQB-WLRETA01-KVAL                                 
122900                                                                          
123000       MOVE NEJ                 TO OMSTART-SW                             
123100     END-IF                                                               
123200                                                                          
123300     PERFORM UNTIL SEGMENT-SAKNAS                                         
123400                OR (ANTAL-REPL = MAX-ANTAL-REPL)                          
123500                                                                          
123600        MOVE W-SND-SAENT       TO RET-KDRETSTA                            
123700        MOVE W-KLI-SAENT       TO RET-KDKOLSTA                            
123800        MOVE W-DATUM           TO RET-DASNDDAT                            
123900                                                                          
124000        PERFORM IMS-REPL-SEQB-WLRETA01                                    
124100                                                                          
124200        ADD +1                 TO ANTAL-REPL                              
124300                                                                          
124400        PERFORM IMS-GHN-SEQB-WLRETA01                                     
124500     END-PERFORM                                                          
124600                                                                          
124700     IF SEGMENT-SAKNAS                                                    
124800       MOVE NEJ           TO RADER-KVAR-SW                                
124900                                                                          
125000* SKALL EJ SKRIVA UT NÅGON LISTA FÖR NORGE ENLIGT SUSSI 991202.           
125100* EJ HELLER FÖR LDC-SVERIGE=1A ,ENLIGT SUSSI 000419.                      
125200* EJ HELLER FÖR LDC-SVERIGE=1B ,ENLIGT SUSSI 020625.                      
125300* EJ HELLER FÖR LDC-ENGLAND=2A ,ENLIGT SUSSI 020625.                      
125400* EJ HELLER FÖR DANMARK,ENLIGT SUSSI 060406.                              
125500* EJ HELLER FÖR TYSKLAND,ENLIGT SUSSI 060912.                             
125600       IF MSGI-IDRT-KEY = 'NO1' OR 'SE1' OR 'SE2' OR 'GB2' OR             
125700                          'DK1' OR 'DK2' OR 'DE1'                         
125800         CONTINUE                                                         
125900       ELSE                                                               
126000         PERFORM HCA-STARTA-UTSKRIFT                                      
126100       END-IF                                                             
126200     END-IF                                                               
126300                                                                          
126400     .                                                                    
126500     EJECT                                                                
126600                                                                          
126700 HCA-STARTA-UTSKRIFT     SECTION.                                         
126800                                                                          
126900     MOVE WC-CDC-SE           TO  W407-IDDC                               
127000     MOVE MSGI-IDRT-KEY       TO  W407-IDRT                               
127100     MOVE MSGI-IDRTLOP        TO  W407-IDRTLOP                            
127200     MOVE SPACE               TO  W407-IDPRTLST                           
127300     MOVE '4RT'               TO  W407-IDPRTLST (1:3)                     
127400     MOVE MSGI-IDRT-KEY       TO  W407-IDPRTLST (4:3)                     
127500                                                                          
127600     CALL W4074310 USING W407-W4074310 UTSKR-ALT-PCB                      
127700                                       UTSKR-RETA-PCB                     
127800                                       UTSKR-LISB-PCB                     
127900     .                                                                    
128000     EJECT                                                                
128100                                                                          
128200 MFS-RENSA-FAELT-UT SECTION.                                              
128300                                                                          
128400     MOVE MFS-RENSA-FAELT       TO MOD-IDDC-SPAR                          
128500                                   MOD-IDRT-SPAR                          
128600                                   MOD-IDRTLOP-SPAR                       
128700                                   MOD-IDKOLLI-SPAR                       
128800                                   MOD-DAREGDAT-SPAR                      
128900                                   MOD-TIKLOCK-SPAR                       
129000                                                                          
129100     MOVE +1                    TO INDX                                   
129200     PERFORM UNTIL INDX         >  MAX-INDX                               
129300        PERFORM MFS-RENSA-RAD-FAELT-UT                                    
129400        ADD +1                  TO INDX                                   
129500     END-PERFORM                                                          
129600     .                                                                    
129700     SKIP3                                                                
129800 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
129900                                                                          
130000     MOVE MFS-RENSA-FAELT       TO MOD-IDKOLLI     (INDX)                 
130100                                   MOD-FLFARLIG    (INDX)                 
130200     .                                                                    
130300     SKIP3                                                                
130400 MFS-RENSA-FAELT-IN SECTION.                                              
130500                                                                          
130600*    --- ALLA INDATA-FÄLT                                                 
130700     MOVE MFS-RENSA-FAELT       TO MOD-FLNYSNDN                           
130800                                   MOD-FLSNDDOK                           
130900     MOVE +1 TO INDX                                                      
131000     PERFORM UNTIL INDX         >  MAX-INDX                               
131100       MOVE MFS-RENSA-FAELT     TO MOD-KDCMD(INDX)                        
131200       ADD +1                   TO INDX                                   
131300     END-PERFORM                                                          
131400     .                                                                    
131500     EJECT                                                                
131600 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
131700                                                                          
131800     MOVE MFS-ROER-EJ-FAELT   TO MOD-IDDC-SPAR                            
131900                                 MOD-IDRT-SPAR                            
132000                                 MOD-IDRTLOP-SPAR                         
132100                                 MOD-IDKOLLI-SPAR                         
132200                                 MOD-DAREGDAT-SPAR                        
132300                                 MOD-TIKLOCK-SPAR                         
132400                                                                          
132500     MOVE +1                  TO INDX                                     
132600     PERFORM UNTIL INDX       >  MAX-INDX                                 
132700       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
132800       ADD +1                 TO INDX                                     
132900     END-PERFORM                                                          
133000     .                                                                    
133100                                                                          
133200                                                                          
133300 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
133400                                                                          
133500     MOVE MFS-ROER-EJ-FAELT     TO MOD-IDKOLLI     (INDX)                 
133600                                   MOD-FLFARLIG    (INDX)                 
133700     .                                                                    
133800                                                                          
133900 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
134000                                                                          
134100*    --- ALLA INDATA-FÄLT                                                 
134200     MOVE MFS-ROER-EJ-FAELT     TO MOD-FLNYSNDN                           
134300                                   MOD-FLSNDDOK                           
134400     MOVE +1 TO INDX                                                      
134500     PERFORM UNTIL INDX         >  MAX-INDX                               
134600       MOVE MFS-ROER-EJ-FAELT   TO MOD-KDCMD(INDX)                        
134700       ADD +1                   TO INDX                                   
134800     END-PERFORM                                                          
134900     .                                                                    
135000     EJECT                                                                
135100 MFS-FORM-ATTR SECTION.                                                   
135200                                                                          
135300*    --- ALLA INDATA-FÄLT                                                 
135400     MOVE MFS-FORMATETS-ATTR    TO MOD-FLNYSNDN-ATTR                      
135500                                   MOD-FLSNDDOK-ATTR                      
135600     MOVE +1 TO INDX                                                      
135700     PERFORM UNTIL INDX         >  MAX-INDX                               
135800       MOVE MFS-FORMATETS-ATTR  TO MOD-KDCMD-ATTR(INDX)                   
135900       ADD +1                   TO INDX                                   
136000     END-PERFORM                                                          
136100     .                                                                    
136200     EJECT                                                                
136300                                                                          
136400* --- IMS SEKTIONER ---                                                   
136500     SKIP3                                                                
136600 IMS-GET-MSG SECTION.                                                     
136700                                                                          
136800     MOVE '  QC' TO GODK-STATUSKODER                                      
136900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
137000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
137100     PERFORM IMS-STATUSKONTROLL                                           
137200     .                                                                    
137300     SKIP3                                                                
137400 IMS-INSERT-MSG SECTION.                                                  
137500                                                                          
137600     IF ENGLISH-TEXT                                                      
137700       MOVE 'N' TO MFS-KDHUVOMR                                           
137800     END-IF                                                               
137900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
138000     MOVE SPACE TO GODK-STATUSKODER                                       
138100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
138200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
138300     PERFORM IMS-STATUSKONTROLL                                           
138400     .                                                                    
138500     EJECT                                                                
138600                                                                          
138700 IMS-GHU-SEQB-WLRETA01-KVAL  SECTION.                                     
138800                                                                          
138900     STRING 'WLRETA01(WDA3BSEQ =' W-WDA3BSEQ-X                            
139000                    '&DAREGDAT =' W-DAREGDAT-A3-X                         
139100                    '&TIKLOCK  =' W-TIKLOCK-A3-X ')'                      
139200          DELIMITED BY SIZE INTO SSA1                                     
139300     MOVE '  GE'           TO GODK-STATUSKODER                            
139400     CALL CBLTDLI USING GHU RETA-PCB DLI-IO-AREA SSA1                     
139500     MOVE RETA-STATUS-CODE TO STATUS-WS                                   
139600     PERFORM IMS-STATUSKONTROLL                                           
139700     .                                                                    
139800                                                                          
139900 IMS-GHU-SEQB-WLRETA01       SECTION.                                     
140000                                                                          
140100     STRING 'WLRETA01(WDA3BSEQ>=' W-WDA3BSEQ-MIN-X                        
140200                    '&WDA3BSEQ<=' W-WDA3BSEQ-MAX-X ')'                    
140300          DELIMITED BY SIZE INTO SSA1                                     
140400     MOVE '  GE'           TO GODK-STATUSKODER                            
140500     CALL CBLTDLI USING GHU RETA-PCB DLI-IO-AREA SSA1                     
140600     MOVE RETA-STATUS-CODE TO STATUS-WS                                   
140700     PERFORM IMS-STATUSKONTROLL                                           
140800     .                                                                    
140900                                                                          
141000 IMS-GHN-SEQB-WLRETA01       SECTION.                                     
141100                                                                          
141200     STRING 'WLRETA01(WDA3BSEQ>=' W-WDA3BSEQ-MIN-X                        
141300                    '&WDA3BSEQ<=' W-WDA3BSEQ-MAX-X ')'                    
141400          DELIMITED BY SIZE INTO SSA1                                     
141500     MOVE '  GEGB'           TO GODK-STATUSKODER                          
141600     CALL CBLTDLI USING GHN RETA-PCB DLI-IO-AREA SSA1                     
141700     MOVE RETA-STATUS-CODE TO STATUS-WS                                   
141800     PERFORM IMS-STATUSKONTROLL                                           
141900     .                                                                    
142000     EJECT                                                                
142100                                                                          
142200 IMS-REPL-SEQB-WLRETA01      SECTION.                                     
142300                                                                          
142400     MOVE '    '           TO GODK-STATUSKODER                            
142500     CALL CBLTDLI USING REPL RETA-PCB DLI-IO-AREA                         
142600     MOVE RETA-STATUS-CODE TO STATUS-WS                                   
142700     PERFORM IMS-STATUSKONTROLL                                           
142800     .                                                                    
142900                                                                          
143000                                                                          
143100 IMS-GU-WLRETD01       SECTION.                                           
143200                                                                          
143300     STRING 'WLRETD01(WDA3C1KY>=' W-WDA3C1KY-MIN-X                        
143400                    '&WDA3C1KY<=' W-WDA3C1KY-MAX-X ')'                    
143500          DELIMITED BY SIZE INTO SSA1                                     
143600     MOVE '  GE'         TO GODK-STATUSKODER                              
143700     CALL CBLTDLI USING GU RETD-PCB DLI-IO-AREA SSA1                      
143800     MOVE RETD-STATUS-CODE TO STATUS-WS                                   
143900     PERFORM IMS-STATUSKONTROLL                                           
144000     .                                                                    
144100     EJECT                                                                
144200                                                                          
144300 IMS-GN-WLRETD01       SECTION.                                           
144400                                                                          
144500     STRING 'WLRETD01(WDA3C1KY>=' W-WDA3C1KY-MIN-X                        
144600                    '&WDA3C1KY<=' W-WDA3C1KY-MAX-X ')'                    
144700          DELIMITED BY SIZE INTO SSA1                                     
144800     MOVE '  GEGB'           TO GODK-STATUSKODER                          
144900     CALL CBLTDLI USING GN RETD-PCB DLI-IO-AREA SSA1                      
145000     MOVE RETD-STATUS-CODE TO STATUS-WS                                   
145100     PERFORM IMS-STATUSKONTROLL                                           
145200     .                                                                    
145300     EJECT                                                                
145400                                                                          
145500 IMS-GU-WLRETF01       SECTION.                                           
145600                                                                          
145700     STRING 'WLRETF01(WDA3E1KY>=' W-WDA3E1KY-MIN-X                        
145800                    '&WDA3E1KY<=' W-WDA3E1KY-MAX-X ')'                    
145900          DELIMITED BY SIZE INTO SSA1                                     
146000     MOVE '  GE'           TO GODK-STATUSKODER                            
146100     CALL CBLTDLI USING GU RETF-PCB DLI-IO-AREA SSA1                      
146200     MOVE RETF-STATUS-CODE TO STATUS-WS                                   
146300     PERFORM IMS-STATUSKONTROLL                                           
146400     .                                                                    
146500                                                                          
146600 IMS-GU-WLRETG01       SECTION.                                           
146700                                                                          
146800     STRING 'WLRETG01(WDA3F1KY>=' W-WDA3F1KY-MIN-X                        
146900                    '&WDA3F1KY<=' W-WDA3F1KY-MAX-X ')'                    
147000          DELIMITED BY SIZE INTO SSA1                                     
147100     MOVE '  GE'           TO GODK-STATUSKODER                            
147200     CALL CBLTDLI USING GU RETG-PCB DLI-IO-AREA SSA1                      
147300     MOVE RETG-STATUS-CODE TO STATUS-WS                                   
147400     PERFORM IMS-STATUSKONTROLL                                           
147500     .                                                                    
147600                                                                          
147700 IMS-GN-WLRETG01       SECTION.                                           
147800                                                                          
147900     STRING 'WLRETG01(WDA3F1KY>=' W-WDA3F1KY-MIN-X                        
148000                    '&WDA3F1KY<=' W-WDA3F1KY-MAX-X ')'                    
148100          DELIMITED BY SIZE INTO SSA1                                     
148200     MOVE '  GEGB'         TO GODK-STATUSKODER                            
148300     CALL CBLTDLI USING GN RETG-PCB DLI-IO-AREA SSA1                      
148400     MOVE RETG-STATUS-CODE TO STATUS-WS                                   
148500     PERFORM IMS-STATUSKONTROLL                                           
148600     .                                                                    
148700     EJECT                                                                
148800 IMS-GU-WL411101  SECTION.                                                
148900                                                                          
149000     STRING 'WL411101(WDGXKEY  =' W-WDGXKEY-X ')'                         
149100                      DELIMITED BY SIZE INTO SSA1                         
149200     MOVE '    ' TO GODK-STATUSKODER                                      
149300     CALL CBLTDLI USING GU 4111-PCB WL411101 SSA1                         
149400     MOVE 4111-STATUS-CODE TO STATUS-WS                                   
149500     PERFORM IMS-STATUSKONTROLL                                           
149600     .                                                                    
149700     SKIP2                                                                
149800 IMS-GHNP-WL411111  SECTION.                                              
149900                                                                          
150000     MOVE  'WL411111*F' TO SSA1                                           
150100     MOVE '    ' TO GODK-STATUSKODER                                      
150200     CALL CBLTDLI USING GHNP 4111-PCB WL411111 SSA1                       
150300     MOVE 4111-STATUS-CODE TO STATUS-WS                                   
150400     PERFORM IMS-STATUSKONTROLL                                           
150500     .                                                                    
150600     SKIP2                                                                
150700 IMS-REPL-WL411111  SECTION.                                              
150800                                                                          
150900     MOVE '  ' TO GODK-STATUSKODER                                        
151000     CALL CBLTDLI USING REPL 4111-PCB WL411111                            
151100     MOVE 4111-STATUS-CODE TO STATUS-WS                                   
151200     PERFORM IMS-STATUSKONTROLL                                           
151300     .                                                                    
151400     SKIP2                                                                
151500 IMS-ISRT-ALT43  SECTION.                                                 
151600     SKIP2                                                                
151700     MOVE SPACE TO GODK-STATUSKODER                                       
151800     CALL CBLTDLI USING ISRT 4743-PCB MSG-IO-AREA                         
151900     MOVE 4743-STATUS-CODE TO STATUS-WS                                   
152000     PERFORM IMS-STATUSKONTROLL                                           
152100     .                                                                    
152200     EJECT                                                                
152300 IMS-STATUSKONTROLL SECTION.                                              
152400                                                                          
152500     SET STATUS-IX TO 1                                                   
152600     SEARCH GODK-STATUS                                                   
152700       AT END                                                             
152800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
152900         DELIMITED BY SIZE INTO FELTEXT                                   
153000         CALL FELLOG                                                      
153100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
153200         CONTINUE                                                         
153300     END-SEARCH                                                           
153400     .                                                                    
