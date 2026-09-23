000100*                                                                         
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4075300.                                                
000400 AUTHOR.         KJELLSON GÖRAN.                                          
000500 DATE-WRITTEN.   13/09/02.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        UNDERHÅLL AV LEDTIDER FÖR LEVERANSANMÄRKNINGAR (CLAIM)           
001000*        OCH REURER (RETURN)                                              
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WDR5 (WDGX4128)                            
001300*                   LÄSER      WDB2                                       
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W4T753                                              
001700*        MID:         W4I75301                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W4O75301                                            
002100                                                                          
002200                                                                          
002300 ENVIRONMENT DIVISION.                                                    
002400 DATA DIVISION.                                                           
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700 77  IDPGM                       PIC X(08)   VALUE 'W4075300'.            
002800                                                                          
002900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003100                                                                          
003200 77  JA                          PIC X       VALUE 'J'.                   
003300 77  NEJ                         PIC X       VALUE 'N'.                   
003301 01  SW-REGEL-VALD               PIC X       VALUE 'N'.                   
003310 01  CURR-IDDISTR-FOM            PIC 9(5)    VALUE ZERO.                  
003320 01  CURR-IDDISTR-TOM            PIC 9(5)    VALUE 9999.                  
003321 01  CURR-IDKUNDNR-FOM           PIC 9(7)    VALUE ZERO.                  
003322 01  CURR-IDKUNDNR-TOM           PIC 9(7)    VALUE 999999.                
003323 01  CURR-KDANMORS               PIC X(2)    VALUE SPACE.                 
003324 01  WS-IDDISTR                  PIC 9(5)    VALUE ZERO.                  
003326 01  WS-IDKUNDNR                 PIC 9(7)    VALUE ZERO.                  
003328 01  WS-KDANMORS                 PIC X(2)    VALUE SPACE.                 
003330                                                                          
003500 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
003600 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
003700                                                                          
003800*    --- INDEX FÖR BLÄDDRINGSRADER                                        
003900 77  INDX                        PIC 9(4)   VALUE 0.                      
004000 77  MAX-INDX                    PIC 9(4)   VALUE 8.                      
004100 77  CURR-INDX                   PIC 9(4)   VALUE 0.                      
004200 77  CURR-KDCMD                  PIC X(1)   VALUE SPACE.                  
004300                                                                          
004400 01  W-IDDISTR                   PIC S9(5)   VALUE ZERO COMP-3.           
004500 01  W-IDDISTR-NUM               PIC  9(4).                               
004510 01  W-IDDISTR-DEF-FOM-X.                                                 
004511     03 W-IDDISTR-DEF-FOM        PIC S9(5)   VALUE 1    COMP-3.           
004520 01  W-IDDISTR-DEF-TOM-X.                                                 
004530     03 W-IDDISTR-DEF-TOM        PIC S9(5)   VALUE 9999 COMP-3.           
004600 01  W-IDKUNDNR                  PIC S9(7)   VALUE ZERO COMP-3.           
004700 01  W-IDKUNDNR-NUM              PIC  9(6).                               
004710 01  W-IDKUNDNR-DEF-FOM-X.                                                
004720     03 W-IDKUNDNR-DEF-FOM       PIC S9(7)   VALUE ZERO COMP-3.           
004730 01  W-IDKUNDNR-DEF-TOM-X.                                                
004740     03 W-IDKUNDNR-DEF-TOM       PIC S9(7)   VALUE 999999 COMP-3.         
004800 01  W-KDANMORS.                                                          
004900     03  W-KDANMORS-POS1         PIC 9(1).                                
005000     03  W-KDANMORS-POS2         PIC X(1).                                
005010 01  W-KDANMORS-SPACE            PIC X(2)    VALUE SPACE.                 
005011 01  W-KDANMORS-NX.                                                       
005020     03  W-KDANMORS-NX-POS1      PIC 9(1).                                
005030     03  W-KDANMORS-NX-POS2      PIC X(1).                                
005100 01  W-FLVISA                    PIC X(1).                                
005200                                                                          
005300                                                                          
005400*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005500                                                                          
005600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005700     88  NYCKLAR-OK                          VALUE 'J'.                   
005800     88  NYCKLAR-FEL                         VALUE 'N'.                   
005900                                                                          
006000 77  INPUT-SW                    PIC X       VALUE 'J'.                   
006100     88  INPUT-FINNS                         VALUE 'J'.                   
006200     88  INPUT-SAKNAS                        VALUE 'N'.                   
006300                                                                          
006400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006500     88  INDATA-OK                           VALUE 'J'.                   
006600     88  INDATA-FEL                          VALUE 'N'.                   
006700                                                                          
006800 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006900     88  ALLT-OK                             VALUE 'J'.                   
007000     88  ALLT-EJ-OK                          VALUE 'N'.                   
007100                                                                          
007200 77  INTERVALL-SW                PIC X       VALUE 'N'.                   
007300     88  KUNDINTERVALL                       VALUE 'J'.                   
007400                                                                          
007500 77  DISTRIKT-SW                 PIC X       VALUE 'N'.                   
007600     88  DISTRIKT-OK                         VALUE 'J'.                   
007700                                                                          
007800 77  CMD-VALUES                  PIC X       VALUE ' '.                   
007900     88  CMD-UPDATE                          VALUE 'C'.                   
008000     88  CMD-NEW                             VALUE 'N'.                   
008100     88  CMD-DELETE                          VALUE 'D'.                   
008200                                                                          
008300 77  RAD-SW                      PIC X       VALUE 'N'.                   
008400     88  RAD-VALD                            VALUE 'J'.                   
008500     88  RAD-EJ-VALD                         VALUE 'N'.                   
008600                                                                          
008700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008800     88  EGEN-MID                            VALUE '4753'.                
008900     88  GODK-MID                            VALUE '4751' '4752'          
009000                                                   '4753' '4754'          
009100                                                   '4755' '4756'          
009200                                                   '4757' '4758'          
009300                                                   '4759'.                
009400     88  HELP-MID                            VALUE '0551'.                
009500                                                                          
009600 77  W-VISA                      PIC 9(1)    VALUE ZERO.                  
009700     88  VISA-DISTRIKT                       VALUE 1.                     
009800     88  VISA-KUND                           VALUE 2.                     
009900     88  VISA-ORSAK                          VALUE 3.                     
010000     88  VISA-DISTRIKT-ORSAK                 VALUE 4.                     
010100     88  VISA-ALLT                           VALUE 5.                     
010200                                                                          
010300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
010400 01  GENERELLA-SUBPROGRAM.                                                
010500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
010600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
010700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010900                                                                          
011000                                                                          
011100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
011200*01 -COPY WMEDAREA                                                        
011300                                                                          
011400 01  MESSAGE-CODES.                                                       
011500     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
011600     03  ERR-SPECIFY-ONE-CHOICE  PIC X(3)    VALUE '004'.                 
011700     03  ERR-KEY-MISSING         PIC X(3)    VALUE '005'.                 
011800     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
011900     03  ERR-NOT-NUMERIC         PIC X(3)    VALUE '020'.                 
012000     03  ERR-DISTR-CUST-MISSING  PIC X(3)    VALUE '040'.                 
012100     03  ERR-WRONG-CODE          PIC X(3)    VALUE '132'.                 
012200     03  ERR-TOM-LESS-THAN-FOM   PIC X(3)    VALUE '240'.                 
012300     03  ERR-DUPLICATE-RECORD    PIC X(3)    VALUE '245'.                 
012310     03  ERR-NO-LINE-SELECTED    PIC X(3)    VALUE '362'.                 
012400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
012500     03  ERR-INVALID-VALUE       PIC X(3)    VALUE '492'.                 
012600     03  ERR-INTERVAL            PIC X(3)    VALUE '738'.                 
012700     03  ERR-WRONG-DISTRICT      PIC X(3)    VALUE '747'.                 
012800     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
012900     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
013000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
013100     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
013200     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
013300********************                                                      
013400                                                                          
013500                                                                          
013600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
013700*                                                                         
013800 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
013900                                                                          
014000*01 -COPY WMSGINIT                                                        
014100                                                                          
014300*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
014400*                                                                         
014500 01  SPAR-AREA.                                                           
014600     03  SPAR-IDTRANS                PIC X(4)    VALUE '4753'.            
014700     03  SPAR-KEY4128-ENTER.                                              
014800         05  SPAR-IDDISTR-FKY-ENTER  PIC S9(5)        COMP-3.             
014900         05  SPAR-IDDISTR-TKY-ENTER  PIC S9(5)        COMP-3.             
015000         05  SPAR-IDKUNDNR-FKY-ENTER PIC S9(7)        COMP-3.             
015100         05  SPAR-IDKUNDNR-TKY-ENTER PIC S9(7)        COMP-3.             
015200         05  SPAR-KDANMORS-KY-ENTER  PIC  X(2).                           
015300     03  SPAR-KEY4128-NEXT.                                               
015400         05  SPAR-IDDISTR-FKY-NEXT   PIC S9(5)        COMP-3.             
015500         05  SPAR-IDDISTR-TKY-NEXT   PIC S9(5)        COMP-3.             
015600         05  SPAR-IDKUNDNR-FKY-NEXT  PIC S9(7)        COMP-3.             
015700         05  SPAR-IDKUNDNR-TKY-NEXT  PIC S9(7)        COMP-3.             
015800         05  SPAR-KDANMORS-KY-NEXT   PIC  X(2).                           
015900     03  SPAR-IDDISTR-FOM            PIC S9(5)   COMP-3.                  
016000     03  SPAR-IDDISTR-TOM            PIC S9(5)   COMP-3.                  
016100     03  SPAR-IDKUNDNR-FOM           PIC S9(7)   COMP-3.                  
016200     03  SPAR-IDKUNDNR-TOM           PIC S9(7)   COMP-3.                  
016300     03  SPAR-KDANMORS-FOM           PIC X(2).                            
016400     03  SPAR-KDANMORS-TOM           PIC X(2).                            
016500     03  SPAR-FLVISA                 PIC X(1).                            
016600                                                                          
016700                                                                          
016800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
016900*                                                                         
017000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
017100                                                                          
017200*01  MID -COPY W4I75301                                                   
017300                                                                          
017400                                                                          
017500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
017600                                                                          
017700*01  -COPY WMSGAREA                                                       
017800     03  MOD REDEFINES MSG-AREA.                                          
017900*      05  -COPY W4O75301                                                 
018000                                                                          
018100                                                                          
018200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
018300                                                                          
018400*01  -COPY WMFSAREA                                                       
018500                                                                          
018600                                                                          
018700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018800*                                                                         
018900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
019000                                                                          
019100 01  NYCKLAR-TILL-DLI.                                                    
019200*    --- VÄRDE PÅ BLÄDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN             
019300                                                                          
019400     03  W-WDGXKEY-4127-X.                                                
019500         05  W-IDHTYP-4127       PIC  X(4)   VALUE '4127'.                
019600         05  FILLER              PIC  X(26)  VALUE LOW-VALUE.             
019700                                                                          
019800     03  W-KEY4128-X.                                                     
019900         05  W-4128-IDDISTR-FKY  PIC S9(5)        COMP-3.                 
020000         05  W-4128-IDDISTR-TKY  PIC S9(5)        COMP-3.                 
020100         05  W-4128-IDKUNDNR-FKY PIC S9(7)        COMP-3.                 
020200         05  W-4128-IDKUNDNR-TKY PIC S9(7)        COMP-3.                 
020300         05  W-4128-KDANMORS-KY  PIC  X(2).                               
020400                                                                          
020500     03  W-4128-IDDISTR-FOM-X.                                            
020600         05  W-4128-IDDISTR-FOM  PIC S9(5)        COMP-3.                 
020700                                                                          
020800     03  W-4128-IDDISTR-TOM-X.                                            
020900         05  W-4128-IDDISTR-TOM  PIC S9(5)        COMP-3.                 
021000                                                                          
021100     03  W-4128-IDKUNDNR-FOM-X.                                           
021200         05  W-4128-IDKUNDNR-FOM PIC S9(7)        COMP-3.                 
021300                                                                          
021400     03  W-4128-IDKUNDNR-TOM-X.                                           
021500         05  W-4128-IDKUNDNR-TOM PIC S9(7)        COMP-3.                 
021600                                                                          
021700     03  W-4128-KDANMORS-FOM-X.                                           
021800         05  W-4128-KDANMORS-FOM PIC  X(2).                               
021900                                                                          
022000     03  W-4128-KDANMORS-TOM-X.                                           
022100         05  W-4128-KDANMORS-TOM PIC  X(2).                               
022200                                                                          
022300     03  W-IDGMT-FOM-X.                                                   
022400         05  W-IDDISTR-FOM       PIC S9(5)   COMP-3 VALUE ZERO.           
022500         05  W-IDKUNDNR-FOM      PIC S9(7)   COMP-3 VALUE ZERO.           
022600                                                                          
022700     03  W-IDGMT-TOM-X.                                                   
022800         05  W-IDDISTR-TOM       PIC S9(5)   COMP-3 VALUE ZERO.           
022900         05  W-IDKUNDNR-TOM      PIC S9(7)   COMP-3 VALUE ZERO.           
023000                                                                          
023100                                                                          
023200*    --- STATUS KODER FRÅN IMS                                            
023300 01  STATUS-WS                   PIC XX.                                  
023400     88  SEGMENT-FINNS                       VALUE '  '.                  
023500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
023600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
023610     88  BASEN-SLUT                          VALUE 'GB'.                  
023700                                                                          
023800 01  GODK-STATUSKODER.                                                    
023900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024000                                                                          
024100                                                                          
024200 01  ALL-SSA.                                                             
024300     03 SSA1                     PIC X(400).                              
024400     03 SSA2                     PIC X(400).                              
024500                                                                          
024600                                                                          
024700*    --- IMS FUNKTIONSKODER                                               
024800*01  -COPY W0003                                                          
024900                                                                          
025000*    ---  DLI INPUT-OUTPUT AREA                                           
025100                                                                          
025200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR101'.                      
025300 01  DLI-IO-WDR101.                                                       
025400*    03  -COPY WDGX01                                                     
025500                                                                          
025600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4128'.                    
025700 01  DLI-IO-WDGX4128.                                                     
025800*    03  -COPY WDGX4128                                                   
025900                                                                          
026000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
026100 01  DLI-IO-WDB201.                                                       
026200*    03  -COPY WDB201                                                     
026300                                                                          
026400                                                                          
026500                                                                          
026600 LINKAGE SECTION.                                                         
026700*01  -COPY W0009   -PRE MSG-                                              
026800*01  -COPY W0008   -PRE WDP7-                                             
026900     05  FILLER                  PIC X.                                   
027000                                                                          
027100*01  -COPY W0008   -PRE 4128-                                             
027200     05  FILLER                  PIC X.                                   
027300                                                                          
027400*01  -COPY W0008   -PRE WDB2-                                             
027500     05  FILLER                  PIC X.                                   
027600                                                                          
027700                                                                          
027800 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB 4128-PCB WDB2-PCB.            
027900 MAIN SECTION.                                                            
028000     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB 4128-PCB WDB2-PCB.            
028100                                                                          
028200     PERFORM IMS-GET-MSG                                                  
028300     IF SEGMENT-FINNS                                                     
028400        PERFORM A-INIT                                                    
028500        PERFORM B-KOLLA-NYCKLAR                                           
028600        IF NYCKLAR-OK                                                     
028700           IF MFS-UPDATE                                                  
028800              PERFORM G-KOLLA-INDATA                                      
028900              IF INDATA-OK                                                
029000                 PERFORM H-UPPDATERA                                      
029100              ELSE                                                        
029200                 MOVE NEJ TO ALLT-SW                                      
029300              END-IF                                                      
029400           ELSE                                                           
029500              IF MFS-FIRST                                                
029600                 PERFORM C-FOERSTA-SIDA                                   
029700              ELSE                                                        
029800                 IF MFS-NEXT                                              
029900                    PERFORM D-NAESTA-SIDA                                 
030000                 ELSE                                                     
030100                    PERFORM E-SAMMA-SIDA                                  
030200                 END-IF                                                   
030300              END-IF                                                      
030400           END-IF                                                         
030500           IF ALLT-OK                                                     
030600              PERFORM F-LAES-VISA-INFO                                    
030700           END-IF                                                         
030800        END-IF                                                            
030900                                                                          
031000        COMPUTE MSG-KVLL = LENGTH OF MOD-W4O75301 + 4                     
031100        PERFORM IMS-INSERT-MSG                                            
031200     END-IF                                                               
031300                                                                          
031400     MOVE ZERO TO RETURN-CODE                                             
031500     GOBACK                                                               
031600     .                                                                    
031700                                                                          
031800                                                                          
031900 A-INIT SECTION.                                                          
032000     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
032100                                                                          
032200     IF MSG-DUBBLA-TRANSKODER                                             
032300        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I75301                
032400        MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                 
032500        MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                
032600     ELSE                                                                 
032700        MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I75301                
032800        MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                 
032900        MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                
033000     END-IF                                                               
033100                                                                          
033200     MOVE MSG-KDTRTYP     TO MFS-KDTRTYP                                  
033300     MOVE MSG-IDPFK       TO MFS-IDPFK                                    
033400     MOVE MFS-IDTRANS     TO W-IDTRANS                                    
033500                                                                          
033600     MOVE LOW-VALUE       TO MSG-AREA                                     
033700     MOVE 'W4O753N1'      TO MFS-IDMOD                                    
033800     MOVE '4753'          TO MOD-IDTRANS                                  
033900     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
034000                                                                          
034100     IF EGEN-MID OR HELP-MID                                              
034200        CONTINUE                                                          
034300     ELSE                                                                 
034400        MOVE SPACE        TO MFS-KDTRTYP                                  
034500        MOVE '7'          TO MFS-IDPFK                                    
034600     END-IF                                                               
034610                                                                          
034620     PERFORM AA-NOLLUTTFYLL-MID                                           
034700     .                                                                    
034900                                                                          
035293                                                                          
035294 AA-NOLLUTTFYLL-MID SECTION.                                              
035295     MOVE 'AA-NOLLUTFYLL   ' TO CURRENT-SECTION                           
035296                                                                          
035297*    FÖR ATT SLIPPA INSPECT PÅ FLERA STÄLLEN I PROGRAMMET                 
035298*    FIXAR VI ALLT MED EN GÅNG                                            
035299*    NÄR DATA KOMMER TILLBAKA FRÅN SKÄRMEN ÄR ALLA NUMERISKA FÄLT         
035300*    HÖGERJUSTERADE MED INLEDANDE BLANKA                                  
035301*    OCH SÅ KAN VI INTE HA DET!                                           
035302                                                                          
035303     INSPECT MID-IDDISTR-FOM-UPD                                          
035304             REPLACING LEADING SPACE BY ZERO                              
035305     INSPECT MID-IDDISTR-TOM-UPD                                          
035306             REPLACING LEADING SPACE BY ZERO                              
035307     INSPECT MID-IDKUNDNR-FOM-UPD                                         
035308             REPLACING LEADING SPACE BY ZERO                              
035309     INSPECT MID-IDKUNDNR-TOM-UPD                                         
035310             REPLACING LEADING SPACE BY ZERO                              
035311     INSPECT MID-KVDAGAR-LTRP-UPD                                         
035312             REPLACING LEADING SPACE BY ZERO                              
035313     INSPECT MID-KVDAGAR-LEVANM-UPD                                       
035314             REPLACING LEADING SPACE BY ZERO                              
035315     INSPECT MID-KVDAGAR-LTRP-LDC-UPD                                     
035316             REPLACING LEADING SPACE BY ZERO                              
035317     INSPECT MID-KVDAGAR-LEVANM-LDC-UPD                                   
035318             REPLACING LEADING SPACE BY ZERO                              
035319     INSPECT MID-KVDAGAR-RET-UPD                                          
035320             REPLACING LEADING SPACE BY ZERO                              
035321     INSPECT MID-KVDAGAR-RTRP-UPD                                         
035322             REPLACING LEADING SPACE BY ZERO                              
035323     INSPECT MID-KVDAGAR-RET-LDC-UPD                                      
035324             REPLACING LEADING SPACE BY ZERO                              
035325     INSPECT MID-KVDAGAR-RTRP-LDC-UPD                                     
035326             REPLACING LEADING SPACE BY ZERO                              
035327                                                                          
035328     MOVE 1 TO INDX                                                       
035329     PERFORM UNTIL INDX > MAX-INDX                                        
035330        INSPECT MID-IDDISTR-FOM (INDX)                                    
035331                REPLACING LEADING SPACE BY ZERO                           
035332        INSPECT MID-IDDISTR-TOM (INDX)                                    
035333                REPLACING LEADING SPACE BY ZERO                           
035334        INSPECT MID-IDKUNDNR-FOM (INDX)                                   
035335                REPLACING LEADING SPACE BY ZERO                           
035336        INSPECT MID-IDKUNDNR-TOM (INDX)                                   
035337                REPLACING LEADING SPACE BY ZERO                           
035338        INSPECT MID-KVDAGAR-LTRP (INDX)                                   
035339                REPLACING LEADING SPACE BY ZERO                           
035340        INSPECT MID-KVDAGAR-LEVANM (INDX)                                 
035341                REPLACING LEADING SPACE BY ZERO                           
035342        INSPECT MID-KVDAGAR-LTRP-LDC (INDX)                               
035343                REPLACING LEADING SPACE BY ZERO                           
035344        INSPECT MID-KVDAGAR-LEVANM-LDC (INDX)                             
035345                REPLACING LEADING SPACE BY ZERO                           
035346        INSPECT MID-KVDAGAR-RET (INDX)                                    
035347                REPLACING LEADING SPACE BY ZERO                           
035348        INSPECT MID-KVDAGAR-RTRP (INDX)                                   
035349                REPLACING LEADING SPACE BY ZERO                           
035350        INSPECT MID-KVDAGAR-RET-LDC (INDX)                                
035351                REPLACING LEADING SPACE BY ZERO                           
035352        INSPECT MID-KVDAGAR-RTRP-LDC (INDX)                               
035353                REPLACING LEADING SPACE BY ZERO                           
035354        ADD 1 TO INDX                                                     
035355     END-PERFORM                                                          
035356     .                                                                    
035357                                                                          
035358                                                                          
035359 B-KOLLA-NYCKLAR SECTION.                                                 
035360     MOVE 'B-KOLLA-NYCKLAR ' TO CURRENT-SECTION                           
035361                                                                          
035370     MOVE ALL '+'               TO MSGI-WMSGINIT                          
035400     MOVE '001'                 TO MSGI-KDCALL                            
035500     MOVE MSG-LTERM-NAME        TO MSGI-IDLTERM-USER                      
035600     MOVE MSG-SIGNON-USERID     TO MSGI-IDUSER                            
035700     MOVE '4753'                TO MSGI-IDTRANS                           
035800     IF EGEN-MID                                                          
035900        MOVE MID-IDDISTR-IN     TO MSGI-IDDISTR                           
036000        MOVE MID-IDKUNDNR-IN    TO MSGI-IDKUNDNR                          
036100        MOVE MID-KDANMORS-IN    TO MSGI-KDANMORS                          
036200        MOVE MID-FLVISA-IN      TO MSGI-FLVISA                            
036300                                                                          
036400        PERFORM BA-KOLLA-INMATAT                                          
036500     END-IF                                                               
036600     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
036700     MOVE MSGI-SPAR-AREA        TO SPAR-AREA                              
036800                                                                          
036900     MOVE MSGI-IDDISTR            TO W-IDDISTR                            
037000     MOVE MSGI-IDKUNDNR           TO W-IDKUNDNR                           
037100     MOVE MSGI-KDANMORS           TO W-KDANMORS                           
037200     MOVE MSGI-FLVISA             TO W-FLVISA                             
037300                                                                          
037400*    - SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                 
037500     MOVE 'GB'                  TO MED-IDSKYLT                            
037600                                                                          
037700     MOVE JA TO NYCKLAR-SW                                                
037800                                                                          
037900     MOVE MFS-RENSA-FAELT       TO MOD-IDDISTR-IN                         
038000     MOVE MFS-RENSA-FAELT       TO MOD-IDKUNDNR-IN                        
038100     MOVE MFS-RENSA-FAELT       TO MOD-KDANMORS-IN                        
038200     MOVE MFS-RENSA-FAELT       TO MOD-FLVISA-IN                          
038300     IF NYCKLAR-OK                                                        
038400*    -- KONTROLL AV IDDISTR                                               
038500                                                                          
038600        IF MID-FLVISA-IN = '+'                                            
038700           IF MID-IDDISTR-IN NOT = ALL '+'                                
038800              MOVE '7'       TO MFS-IDPFK                                 
038900              MOVE SPACE     TO MFS-KDTRTYP                               
039000           END-IF                                                         
039100                                                                          
039200           INSPECT MSGI-IDDISTR REPLACING LEADING SPACE BY ZERO           
039300           IF MSGI-IDDISTR NUMERIC                                        
039400              MOVE MSGI-IDDISTR TO W-IDDISTR-FOM                          
039410                                   W-IDDISTR-TOM                          
039500                                   W-IDDISTR                              
039600           ELSE                                                           
039700              MOVE NEJ       TO NYCKLAR-SW                                
039800           END-IF                                                         
039900                                                                          
040000*       -- KONTROLL AV IDKUNDNR                                           
040100                                                                          
040200           IF MID-IDKUNDNR-IN NOT = ALL '+'                               
040300              MOVE '7'       TO MFS-IDPFK                                 
040400              MOVE SPACE     TO MFS-KDTRTYP                               
040500           END-IF                                                         
040600           INSPECT MSGI-IDKUNDNR REPLACING LEADING SPACE BY ZERO          
040700           IF MSGI-IDKUNDNR NUMERIC                                       
040800              MOVE MSGI-IDKUNDNR TO W-IDKUNDNR-FOM                        
040900                                    W-IDKUNDNR-TOM                        
041000           ELSE                                                           
041100              MOVE NEJ       TO NYCKLAR-SW                                
041200           END-IF                                                         
041300                                                                          
041400*       -- KONTROLL AV KDANMORS                                           
041500                                                                          
041600           IF MID-KDANMORS-IN NOT = ALL '+'                               
041700              MOVE '7'       TO MFS-IDPFK                                 
041800              MOVE SPACE     TO MFS-KDTRTYP                               
041900           END-IF                                                         
042000           MOVE MSGI-KDANMORS TO W-KDANMORS                               
042100           IF W-KDANMORS NOT = SPACE                                      
042200              IF  W-KDANMORS NUMERIC                                      
042300              OR  W-KDANMORS-POS1 NUMERIC AND                             
042400                  W-KDANMORS-POS2 = 'X'                                   
042500                  CONTINUE                                                
042600              ELSE                                                        
042700                 MOVE NEJ    TO NYCKLAR-SW                                
042800              END-IF                                                      
042900           END-IF                                                         
043000                                                                          
043100        ELSE                                                              
043200           MOVE ZERO         TO W-IDDISTR                                 
043300                                W-IDDISTR-FOM                             
043400                                W-IDDISTR-TOM                             
043500                                W-IDKUNDNR                                
043600                                W-IDKUNDNR-FOM                            
043700                                W-IDKUNDNR-TOM                            
043800           MOVE SPACE        TO W-KDANMORS                                
043900                                                                          
044000*       -- KONTROLL AV FLVISA                                             
044100                                                                          
044200           IF MID-FLVISA-IN NOT = ALL '+'                                 
044300              MOVE '7'       TO MFS-IDPFK                                 
044400              MOVE SPACE     TO MFS-KDTRTYP                               
044500           END-IF                                                         
044600           IF MSGI-FLVISA NOT = ' ' AND 'Y' AND 'J'                       
044700              MOVE NEJ       TO NYCKLAR-SW                                
044800           END-IF                                                         
044900        END-IF                                                            
045000     END-IF                                                               
045100                                                                          
045200     IF NYCKLAR-OK                                                        
045300        PERFORM BB-KOLLA-KOMBINATIONER                                    
045400     END-IF                                                               
045500                                                                          
045600     IF EGEN-MID OR NYCKLAR-OK                                            
045700        IF MSGI-FLVISA = 'Y' OR 'J'                                       
045800           MOVE MFS-RENSA-FAELT  TO MOD-IDDISTR-UT                        
045900                                    MOD-IDKUNDNR-UT                       
046000                                    MOD-KDANMORS-UT                       
046100        ELSE                                                              
046200           MOVE MSGI-IDDISTR     TO MOD-IDDISTR-UT                        
046300           INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE         
046400           MOVE MSGI-IDKUNDNR    TO MOD-IDKUNDNR-UT                       
046500           INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE        
046600           IF MSGI-KDANMORS = ZERO                                        
046700              MOVE SPACE         TO MOD-KDANMORS-UT                       
046800           ELSE                                                           
046900              MOVE MSGI-KDANMORS TO MOD-KDANMORS-UT                       
047000           END-IF                                                         
047100        END-IF                                                            
047200        MOVE MSGI-FLVISA         TO MOD-FLVISA-UT                         
047300     ELSE                                                                 
047400        MOVE MFS-RENSA-FAELT     TO MOD-IDDISTR-UT                        
047500                                    MOD-IDKUNDNR-UT                       
047600                                    MOD-KDANMORS-UT                       
047700                                    MOD-FLVISA-UT                         
047800     END-IF                                                               
047900                                                                          
048000     IF NYCKLAR-FEL                                                       
048100        MOVE ERR-WRONG-KEY    TO MED-IDMFSFEL                             
048200        CALL WMEDKONV USING MED-WMEDAREA                                  
048300        MOVE MED-MFSFEL       TO MOD-TEMFSFEL                             
048400        PERFORM MFS-RENSA-FAELT-IN                                        
048500        PERFORM MFS-RENSA-FAELT-UT                                        
048510     ELSE                                                                 
048520        PERFORM BC-KOLLA-SPARAREA                                         
048600     END-IF                                                               
048700     .                                                                    
048800                                                                          
048900                                                                          
049000 BA-KOLLA-INMATAT       SECTION.                                          
049100     MOVE 'BA-KOLLA-INMATAT' TO CURRENT-SECTION                           
049200                                                                          
049300     MOVE JA TO NYCKLAR-SW                                                
049400                                                                          
049500     IF  MID-IDDISTR-IN   = ALL '+'                                       
049600     AND MID-IDKUNDNR-IN  = ALL '+'                                       
049700     AND MID-KDANMORS-IN  = ALL '+'                                       
049800     AND MID-FLVISA-IN    = ALL '+'                                       
049900                                                                          
050000        CONTINUE                                                          
050100     ELSE                                                                 
050200        IF MID-FLVISA-IN NOT = '+'                                        
050300           IF MID-IDDISTR-IN  NOT = ALL '+'                               
050400           OR MID-IDKUNDNR-IN NOT = ALL '+'                               
050500           OR MID-KDANMORS-IN NOT = ALL '+'                               
050600                MOVE NEJ           TO NYCKLAR-SW                          
050700           ELSE                                                           
050800                MOVE ZERO          TO MSGI-IDDISTR                        
050900                                      MSGI-IDKUNDNR                       
051000                                      MSGI-KDANMORS                       
051100           END-IF                                                         
051200        ELSE                                                              
051300           MOVE SPACE              TO MSGI-FLVISA                         
051400        END-IF                                                            
051500     END-IF                                                               
051600     .                                                                    
051700                                                                          
051800                                                                          
051900 BB-KOLLA-KOMBINATIONER SECTION.                                          
052000     MOVE 'BB-KOMBINATIONER' TO CURRENT-SECTION                           
052100                                                                          
052200     IF W-IDDISTR = ZERO                                                  
052300        IF W-FLVISA = SPACE                                               
052400           MOVE NEJ TO NYCKLAR-SW                                         
052500        END-IF                                                            
052600     ELSE                                                                 
052700        IF W-KDANMORS > ZERO                                              
052800           IF W-IDKUNDNR = ZERO                                           
052900              MOVE 4   TO W-VISA                                          
053000           ELSE                                                           
053100              MOVE 3   TO W-VISA                                          
053200           END-IF                                                         
053300        ELSE                                                              
053400           IF W-IDKUNDNR > ZERO                                           
053500              MOVE 2   TO W-VISA                                          
053600           ELSE                                                           
053700              MOVE 1   TO W-VISA                                          
053800           END-IF                                                         
053900        END-IF                                                            
054000     END-IF                                                               
054100                                                                          
054200     IF W-FLVISA NOT = SPACE                                              
054300        IF W-IDDISTR > ZERO                                               
054400           MOVE NEJ TO NYCKLAR-SW                                         
054500        END-IF                                                            
054600        IF W-IDKUNDNR > ZERO                                              
054700           MOVE NEJ TO NYCKLAR-SW                                         
054800        END-IF                                                            
054900        IF W-KDANMORS > ZERO                                              
055000           MOVE NEJ TO NYCKLAR-SW                                         
055100        END-IF                                                            
055200        IF NYCKLAR-OK                                                     
055300           MOVE 5   TO W-VISA                                             
055400        END-IF                                                            
055500     END-IF                                                               
055610     .                                                                    
055700                                                                          
055800                                                                          
055900 BC-KOLLA-SPARAREA SECTION.                                               
056000     MOVE 'BC-KOLLA-SPARARE' TO CURRENT-SECTION                           
056100                                                                          
056101     IF SPAR-IDDISTR-FKY-ENTER  NOT NUMERIC                               
056102        MOVE ZERO        TO SPAR-IDDISTR-FKY-ENTER                        
056103     END-IF                                                               
056104     IF SPAR-IDDISTR-TKY-ENTER  NOT NUMERIC                               
056105        MOVE 9998        TO SPAR-IDDISTR-TKY-ENTER                        
056106     END-IF                                                               
056107     IF SPAR-IDKUNDNR-FKY-ENTER NOT NUMERIC                               
056108        MOVE ZERO        TO SPAR-IDKUNDNR-FKY-ENTER                       
056109     END-IF                                                               
056110     IF SPAR-IDKUNDNR-TKY-ENTER NOT NUMERIC                               
056111        MOVE 999999      TO SPAR-IDKUNDNR-TKY-ENTER                       
056112     END-IF                                                               
056113     IF SPAR-KDANMORS-KY-ENTER  NOT NUMERIC                               
056114        MOVE LOW-VALUE   TO SPAR-KDANMORS-KY-ENTER                        
056115     END-IF                                                               
056116     IF SPAR-IDDISTR-FKY-NEXT   NOT NUMERIC                               
056117        MOVE ZERO        TO SPAR-IDDISTR-FKY-NEXT                         
056118     END-IF                                                               
056119     IF SPAR-IDDISTR-TKY-NEXT   NOT NUMERIC                               
056120        MOVE 9998        TO SPAR-IDDISTR-TKY-NEXT                         
056121     END-IF                                                               
056122     IF SPAR-IDKUNDNR-FKY-NEXT  NOT NUMERIC                               
056123        MOVE ZERO        TO SPAR-IDKUNDNR-FKY-NEXT                        
056124     END-IF                                                               
056125     IF SPAR-IDKUNDNR-TKY-NEXT  NOT NUMERIC                               
056126        MOVE 999999      TO SPAR-IDKUNDNR-TKY-NEXT                        
056127     END-IF                                                               
056128     IF SPAR-KDANMORS-KY-NEXT   NOT NUMERIC                               
056129        MOVE LOW-VALUE  TO SPAR-KDANMORS-KY-NEXT                          
056130     END-IF                                                               
056131     IF SPAR-IDDISTR-FOM        NOT NUMERIC                               
056132        MOVE ZERO       TO SPAR-IDDISTR-FOM                               
056133     END-IF                                                               
056134     IF SPAR-IDDISTR-TOM        NOT NUMERIC                               
056135        MOVE 9998       TO SPAR-IDDISTR-TOM                               
056136     END-IF                                                               
056137     IF SPAR-IDKUNDNR-FOM       NOT NUMERIC                               
056138        MOVE ZERO       TO SPAR-IDKUNDNR-FOM                              
056139     END-IF                                                               
056140     IF SPAR-IDKUNDNR-TOM       NOT NUMERIC                               
056141        MOVE 999999     TO SPAR-IDKUNDNR-TOM                              
056142     END-IF                                                               
056143     IF SPAR-KDANMORS-FOM       NOT NUMERIC                               
056144        MOVE LOW-VALUE  TO SPAR-KDANMORS-FOM                              
056145     END-IF                                                               
056146     IF SPAR-KDANMORS-TOM       NOT NUMERIC                               
056147        MOVE HIGH-VALUE TO SPAR-KDANMORS-TOM                              
056148     END-IF                                                               
056149     .                                                                    
056150                                                                          
056151                                                                          
056152 C-FOERSTA-SIDA SECTION.                                                  
056153     MOVE 'C-FOERSTA-SIDA  ' TO CURRENT-SECTION                           
056160                                                                          
056200     MOVE INF-FIRST-PAGE     TO MED-IDMFSINF                              
056300     CALL WMEDKONV USING MED-WMEDAREA                                     
056400     MOVE MED-MFSINF         TO MOD-TEMFSFEL                              
056500                                                                          
056600     MOVE ZERO               TO W-4128-IDDISTR-FKY                        
056700     MOVE ZERO               TO W-4128-IDDISTR-TKY                        
056800     MOVE ZERO               TO W-4128-IDKUNDNR-FKY                       
056900     MOVE ZERO               TO W-4128-IDKUNDNR-TKY                       
057000     MOVE LOW-VALUE          TO W-4128-KDANMORS-KY                        
057100     PERFORM MFS-RENSA-FAELT-IN                                           
057200     .                                                                    
057300                                                                          
057400                                                                          
057500 D-NAESTA-SIDA SECTION.                                                   
057600     MOVE 'D-NAESTA-SIDA   '    TO CURRENT-SECTION                        
057700                                                                          
057800     PERFORM MFS-RENSA-FAELT-IN                                           
057900                                                                          
058000     MOVE SPAR-IDDISTR-FKY-NEXT      TO W-4128-IDDISTR-FKY                
058100     MOVE SPAR-IDDISTR-TKY-NEXT      TO W-4128-IDDISTR-TKY                
058200     MOVE SPAR-IDKUNDNR-FKY-NEXT     TO W-4128-IDKUNDNR-FKY               
058300     MOVE SPAR-IDKUNDNR-TKY-NEXT     TO W-4128-IDKUNDNR-TKY               
058400     MOVE SPAR-KDANMORS-KY-NEXT      TO W-4128-KDANMORS-KY                
058500     .                                                                    
058600                                                                          
058700                                                                          
058800 E-SAMMA-SIDA SECTION.                                                    
058900     MOVE 'E-SAMMA-SIDA    '      TO CURRENT-SECTION                      
059000                                                                          
059010     MOVE SPAR-IDDISTR-FKY-ENTER     TO W-4128-IDDISTR-FKY                
059020     MOVE SPAR-IDDISTR-TKY-ENTER     TO W-4128-IDDISTR-TKY                
059030     MOVE SPAR-IDKUNDNR-FKY-ENTER    TO W-4128-IDKUNDNR-FKY               
059040     MOVE SPAR-IDKUNDNR-TKY-ENTER    TO W-4128-IDKUNDNR-TKY               
059050     MOVE SPAR-KDANMORS-KY-ENTER     TO W-4128-KDANMORS-KY                
059060                                                                          
059100     IF SPAR-IDTRANS = '4753' OR '0551'                                   
059200        IF MID-INPUT = ALL '+'                                            
059300           PERFORM MFS-RENSA-FAELT-IN                                     
059400        ELSE                                                              
059500           PERFORM EA-MID-INDATA-TILL-MOD                                 
059600           PERFORM EB-KOLLA-FLYTTA-VALD-RAD                               
059700           IF RAD-EJ-VALD                                                 
059800              MOVE NEJ                TO ALLT-SW                          
059900              PERFORM MFS-ROER-EJ-FAELT-IN                                
060000              PERFORM MFS-ROER-EJ-FAELT-UT                                
060100           END-IF                                                         
060200        END-IF                                                            
060300     ELSE                                                                 
060400        PERFORM MFS-RENSA-FAELT-IN                                        
060500     END-IF                                                               
060600     .                                                                    
060700                                                                          
060800                                                                          
060900 EA-MID-INDATA-TILL-MOD SECTION.                                          
061000     MOVE 'EA-MID-TILL-MOD ' TO CURRENT-SECTION                           
061100                                                                          
061200     MOVE 1 TO INDX                                                       
061300     PERFORM UNTIL INDX > MAX-INDX                                        
061400       IF MID-KDCMD (INDX) NOT = ALL '+' AND SPACE                        
061500         MOVE MID-KDCMD (INDX)        TO MOD-KDCMD (INDX)                 
061600         MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-KDCMD-ATTR (INDX)            
061700       ELSE                                                               
061800         MOVE MFS-ROER-EJ-FAELT    TO MOD-KDCMD (INDX)                    
061900         IF MID-IDDISTR-FOM(INDX) = ALL '+' OR SPACE                      
062000            MOVE MFS-STAENG-FAELT     TO MOD-KDCMD-ATTR (INDX)            
062100         END-IF                                                           
062200       END-IF                                                             
062300                                                                          
062400       IF MID-IDDISTR-FOM(INDX) NOT = ALL '+' AND SPACE                   
062500         MOVE MID-IDDISTR-FOM (INDX)  TO MOD-IDDISTR-FOM (INDX)           
062600         MOVE MFS-ADD-LAES-IN-FAELT   TO                                  
062700                                    MOD-IDDISTR-FOM-ATTR (INDX)           
062800       ELSE                                                               
062900         MOVE MFS-ROER-EJ-FAELT       TO MOD-IDDISTR-FOM (INDX)           
063000       END-IF                                                             
063100                                                                          
063200       IF MID-IDDISTR-TOM(INDX) NOT = ALL '+'  AND SPACE                  
063300         MOVE MID-IDDISTR-TOM (INDX)  TO MOD-IDDISTR-TOM (INDX)           
063400         MOVE MFS-ADD-LAES-IN-FAELT   TO                                  
063500                                    MOD-IDDISTR-TOM-ATTR (INDX)           
063600       ELSE                                                               
063700         MOVE MFS-ROER-EJ-FAELT       TO MOD-IDDISTR-TOM (INDX)           
063800       END-IF                                                             
063900                                                                          
064000       IF MID-IDKUNDNR-FOM(INDX) NOT = ALL '+'  AND SPACE                 
064100         MOVE MID-IDKUNDNR-FOM (INDX) TO MOD-IDKUNDNR-FOM (INDX)          
064200         MOVE MFS-ADD-LAES-IN-FAELT   TO                                  
064300                                    MOD-IDKUNDNR-FOM-ATTR (INDX)          
064400       ELSE                                                               
064500         MOVE MFS-ROER-EJ-FAELT       TO MOD-IDKUNDNR-FOM (INDX)          
064600       END-IF                                                             
064700                                                                          
064800       IF MID-IDKUNDNR-TOM(INDX) NOT = ALL '+'  AND SPACE                 
064900         MOVE MID-IDKUNDNR-TOM (INDX) TO MOD-IDKUNDNR-TOM (INDX)          
065000         MOVE MFS-ADD-LAES-IN-FAELT   TO                                  
065100                                    MOD-IDKUNDNR-TOM-ATTR (INDX)          
065200       ELSE                                                               
065300         MOVE MFS-ROER-EJ-FAELT       TO MOD-IDKUNDNR-TOM (INDX)          
065400       END-IF                                                             
065500                                                                          
065600       IF MID-KDANMORS(INDX) NOT = ALL '+'  AND SPACE                     
065700         MOVE MID-KDANMORS (INDX)     TO MOD-KDANMORS (INDX)              
065800         MOVE MFS-ADD-LAES-IN-FAELT   TO                                  
065900                                    MOD-KDANMORS-ATTR (INDX)              
066000       ELSE                                                               
066100         MOVE MFS-ROER-EJ-FAELT       TO MOD-KDANMORS (INDX)              
066200       END-IF                                                             
073600                                                                          
073700       ADD 1 TO INDX                                                      
073800     END-PERFORM                                                          
073900                                                                          
074000     IF MID-KDCMD-DEF  NOT = ALL '+' AND SPACE                            
074100       MOVE MID-KDCMD-DEF          TO MOD-KDCMD-DEF                       
074200       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KDCMD-DEF-ATTR                  
074300     ELSE                                                                 
074400       MOVE MFS-ROER-EJ-FAELT      TO MOD-KDCMD-DEF                       
074500     END-IF                                                               
074600                                                                          
074700     IF MID-KDCMD-UPD  NOT = ALL '+' AND SPACE                            
074800       MOVE MID-KDCMD-UPD          TO MOD-KDCMD-UPD                       
074900       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KDCMD-UPD-ATTR                  
075000     ELSE                                                                 
075100       MOVE MFS-ROER-EJ-FAELT      TO MOD-KDCMD-UPD                       
075200     END-IF                                                               
075300                                                                          
075400     IF MID-IDDISTR-FOM-UPD  NOT = ALL '+' AND SPACE                      
075500       MOVE MID-IDDISTR-FOM-UPD    TO MOD-IDDISTR-FOM-UPD                 
075600       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDDISTR-FOM-UPD-ATTR            
075700     ELSE                                                                 
075800       MOVE MFS-ROER-EJ-FAELT      TO MOD-IDDISTR-FOM-UPD                 
075900     END-IF                                                               
076000                                                                          
076100     IF MID-IDDISTR-TOM-UPD  NOT = ALL '+' AND SPACE                      
076200       MOVE MID-IDDISTR-TOM-UPD    TO MOD-IDDISTR-TOM-UPD                 
076300       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDDISTR-TOM-UPD-ATTR            
076400     ELSE                                                                 
076500       MOVE MFS-ROER-EJ-FAELT      TO MOD-IDDISTR-TOM-UPD                 
076600     END-IF                                                               
076700                                                                          
076800     IF MID-IDKUNDNR-FOM-UPD  NOT = ALL '+' AND SPACE                     
076900       MOVE MID-IDKUNDNR-FOM-UPD   TO MOD-IDKUNDNR-FOM-UPD                
077000       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDKUNDNR-FOM-UPD-ATTR           
077100     ELSE                                                                 
077200       MOVE MFS-ROER-EJ-FAELT      TO MOD-IDKUNDNR-FOM-UPD                
077300     END-IF                                                               
077400                                                                          
077500     IF MID-IDKUNDNR-TOM-UPD  NOT = ALL '+' AND SPACE                     
077600       MOVE MID-IDKUNDNR-TOM-UPD   TO MOD-IDKUNDNR-TOM-UPD                
077700       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDKUNDNR-TOM-UPD-ATTR           
077800     ELSE                                                                 
077900       MOVE MFS-ROER-EJ-FAELT      TO MOD-IDKUNDNR-TOM-UPD                
078000     END-IF                                                               
078100                                                                          
078200     IF MID-KDANMORS-UPD  NOT = ALL '+' AND SPACE                         
078300       MOVE MID-KDANMORS-UPD       TO MOD-KDANMORS-UPD                    
078400       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KDANMORS-UPD-ATTR               
078500     ELSE                                                                 
078600       MOVE MFS-ROER-EJ-FAELT      TO MOD-KDANMORS-UPD                    
078700     END-IF                                                               
078800                                                                          
082100     IF MID-KVDAGAR-RET-UPD NOT = ALL '+' AND SPACE                       
082200       MOVE MID-KVDAGAR-RET-UPD    TO MOD-KVDAGAR-RET-UPD                 
082300       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KVDAGAR-RET-UPD-ATTR            
082400     ELSE                                                                 
082500       MOVE MFS-ROER-EJ-FAELT      TO MOD-KVDAGAR-RET-UPD                 
082600     END-IF                                                               
082700                                                                          
082800     IF MID-KVDAGAR-RTRP-UPD NOT = ALL '+' AND SPACE                      
082900       MOVE MID-KVDAGAR-RTRP-UPD   TO MOD-KVDAGAR-RTRP-UPD                
083000       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KVDAGAR-RTRP-UPD-ATTR           
083100     ELSE                                                                 
083200       MOVE MFS-ROER-EJ-FAELT      TO MOD-KVDAGAR-RTRP-UPD                
083300     END-IF                                                               
083400                                                                          
083500     IF MID-KVDAGAR-RET-LDC-UPD NOT = ALL '+' AND SPACE                   
083600       MOVE MID-KVDAGAR-RET-LDC-UPD                                       
083700                                   TO MOD-KVDAGAR-RET-LDC-UPD             
083800       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KVDAGAR-RET-LDC-UPD-ATTR        
083900     ELSE                                                                 
084000       MOVE MFS-ROER-EJ-FAELT      TO MOD-KVDAGAR-RET-LDC-UPD             
084100     END-IF                                                               
084200                                                                          
084300     IF MID-KVDAGAR-RTRP-LDC-UPD NOT = ALL '+' AND SPACE                  
084400       MOVE MID-KVDAGAR-RTRP-LDC-UPD                                      
084500                                   TO MOD-KVDAGAR-RTRP-LDC-UPD            
084600       MOVE MFS-ADD-LAES-IN-FAELT  TO                                     
084700                                   MOD-KVDAGAR-RTRP-LDC-UPD-ATTR          
084800     ELSE                                                                 
084900       MOVE MFS-ROER-EJ-FAELT      TO MOD-KVDAGAR-RTRP-LDC-UPD            
085000     END-IF                                                               
085100     .                                                                    
085200                                                                          
085300                                                                          
085400 EB-KOLLA-FLYTTA-VALD-RAD SECTION.                                        
085500     MOVE 'EB-KOLLA-FLYTTA ' TO CURRENT-SECTION                           
085600                                                                          
085700     MOVE NEJ TO RAD-SW                                                   
085800                                                                          
085900*    KOLLA ATT BARA EN RAD ÄR IFYLLD OCH ATT KODEN ÄR C ELLER D           
086000     MOVE 1   TO INDX                                                     
086100     PERFORM UNTIL INDX > MAX-INDX                                        
086200        IF MID-KDCMD (INDX) NOT = ALL '+' AND SPACE                       
086300           IF RAD-VALD                                                    
086400              MOVE NEJ          TO ALLT-SW                                
086500              MOVE ERR-SPECIFY-ONE-CHOICE                                 
086600                                TO MED-IDMFSINF                           
086700              MOVE MFS-ALFA-FAELT-FEL                                     
086800                                TO MOD-KDCMD-ATTR (INDX)                  
086900           ELSE                                                           
087000              IF MID-KDCMD (INDX) = 'C'                                   
087100                 MOVE JA        TO RAD-SW                                 
087200                 MOVE INDX      TO CURR-INDX                              
087300                 MOVE MID-KDCMD (INDX)                                    
087400                                TO CURR-KDCMD                             
087500              ELSE                                                        
087600                 IF MID-KDCMD (INDX) = 'D'                                
087700                    MOVE JA     TO RAD-SW                                 
087800                    MOVE NEJ    TO ALLT-SW                                
088100                    MOVE INF-PRESS-PF11                                   
088200                                TO MED-IDMFSINF                           
088300                    MOVE INDX   TO CURR-INDX                              
088400                    MOVE MID-KDCMD (INDX)                                 
088500                                TO CURR-KDCMD                             
088600                    MOVE MFS-ALFA-FAELT-FEL                               
088700                                TO MOD-KDCMD-ATTR (INDX)                  
088710                    PERFORM MFS-ROER-EJ-FAELT-IN                          
088720                    PERFORM MFS-ROER-EJ-FAELT-UT                          
088800                 ELSE                                                     
088900                    MOVE NEJ    TO ALLT-SW                                
089000                    MOVE ERR-WRONG-CODE                                   
089100                                TO MED-IDMFSINF                           
089200                    MOVE MFS-ALFA-FAELT-FEL                               
089300                                TO MOD-KDCMD-ATTR (INDX)                  
089400                 END-IF                                                   
089500              END-IF                                                      
089600           END-IF                                                         
089700        END-IF                                                            
089800        ADD 1 TO INDX                                                     
089900     END-PERFORM                                                          
090000                                                                          
090100     IF MID-KDCMD-DEF NOT = ALL '+' AND SPACE                             
090200        IF RAD-VALD                                                       
090300           MOVE NEJ          TO ALLT-SW                                   
090400           MOVE ERR-SPECIFY-ONE-CHOICE                                    
090500                             TO MED-IDMFSINF                              
090600           MOVE MFS-ALFA-FAELT-FEL                                        
090700                             TO MOD-KDCMD-DEF-ATTR                        
090800        ELSE                                                              
090900           IF MID-KDCMD-DEF = 'C'                                         
091000              MOVE JA        TO RAD-SW                                    
091100              MOVE MFS-ALFA-FAELT-RAETT                                   
091200                             TO MOD-KDCMD-DEF-ATTR                        
091300           ELSE                                                           
091400              MOVE NEJ TO ALLT-SW                                         
091500              MOVE ERR-WRONG-CODE                                         
091600                             TO MED-IDMFSINF                              
091700              MOVE MFS-ALFA-FAELT-FEL                                     
091800                             TO MOD-KDCMD-DEF-ATTR                        
091900           END-IF                                                         
092000        END-IF                                                            
092100     END-IF                                                               
092200                                                                          
092300     IF  (MID-KDCMD-UPD NOT = ALL '+' AND SPACE)                          
092400     AND RAD-EJ-VALD                                                      
092500                                                                          
092600        MOVE NEJ TO ALLT-SW                                               
092700        MOVE INF-PRESS-PF11  TO MED-IDMFSINF                              
092800        PERFORM MFS-ROER-EJ-FAELT-IN                                      
092900        PERFORM MFS-ROER-EJ-FAELT-UT                                      
093000        MOVE MFS-ALFA-FAELT-FEL                                           
093100                             TO MOD-KDCMD-UPD-ATTR                        
093200     END-IF                                                               
093300                                                                          
093400     IF ALLT-OK AND RAD-VALD                                              
093500                                                                          
093600*       VI HAR EN VALD RAD URVAL ELLER DEFAULT MED CMD C ELLER D          
093700*       C PÅ DEFAULTRADEN ÄR OK, FLYTTA RADEN TILL UPD-RADEN              
093800*       C PÅ EN URVALSRAD ÄR OK, FLYTTA RADEN TILL UPD-RADEN              
093900*       D PÅ EN URVALSRAD ÄR INTE OK, FELMARKERA TRYCK PF11               
094000                                                                          
094100        IF MID-KDCMD-DEF = 'C'                                            
094200           PERFORM EBA-FLYTTA-DEF-RAD                                     
094300        ELSE                                                              
094400           IF MID-KDCMD (CURR-INDX) = 'C'                                 
094500              PERFORM EBB-FLYTTA-RAD                                      
094600           END-IF                                                         
094700        END-IF                                                            
094800     END-IF                                                               
094900                                                                          
095000     IF ALLT-EJ-OK                                                        
095100        CALL WMEDKONV USING MED-WMEDAREA                                  
095200        MOVE MED-MFSINF     TO MOD-TEMFSINF                               
095300     ELSE                                                                 
095400        MOVE MFS-ADD-SAETT-CURSOR                                         
095500                            TO MOD-KVDAGAR-LTRP-UPD-ATTR                  
095600     END-IF                                                               
095700     .                                                                    
095800                                                                          
095900                                                                          
096000 EBA-FLYTTA-DEF-RAD SECTION.                                              
096100     MOVE 'EBA-FLYTTA-DEF  ' TO CURRENT-SECTION                           
096200                                                                          
096300     MOVE MID-KDCMD-DEF          TO MOD-KDCMD-UPD                         
096500                                                                          
096600     MOVE MFS-STAENG-FAELT       TO MOD-KDCMD-UPD-ATTR                    
096700     MOVE MID-IDDISTR-FOM-DEF    TO MOD-IDDISTR-FOM-UPD                   
096800     MOVE MFS-STAENG-FAELT       TO MOD-IDDISTR-FOM-UPD-ATTR              
096900     MOVE MID-IDDISTR-TOM-DEF    TO MOD-IDDISTR-TOM-UPD                   
097000     MOVE MFS-STAENG-FAELT       TO MOD-IDDISTR-TOM-UPD-ATTR              
097100     MOVE MFS-ERASE-FIELD        TO MOD-IDKUNDNR-FOM-UPD                  
097200     MOVE MFS-STAENG-FAELT       TO MOD-IDKUNDNR-FOM-UPD-ATTR             
097300     MOVE MFS-ERASE-FIELD        TO MOD-IDKUNDNR-TOM-UPD                  
097400     MOVE MFS-STAENG-FAELT       TO MOD-IDKUNDNR-TOM-UPD-ATTR             
097500     MOVE MFS-ERASE-FIELD        TO MOD-KDANMORS-UPD                      
097600     MOVE MFS-STAENG-FAELT       TO MOD-KDANMORS-UPD-ATTR                 
097800     MOVE MID-KVDAGAR-LTRP-DEF   TO MOD-KVDAGAR-LTRP-UPD                  
098100     MOVE MID-KVDAGAR-LEVANM-DEF TO MOD-KVDAGAR-LEVANM-UPD                
098200     MOVE MFS-ERASE-FIELD        TO MOD-KVDAGAR-LTRP-LDC-UPD              
098300     MOVE MFS-STAENG-FAELT       TO MOD-KVDAGAR-LTRP-LDC-UPD-ATTR         
098400     MOVE MFS-ERASE-FIELD        TO MOD-KVDAGAR-LEVANM-LDC-UPD            
098500     MOVE MFS-STAENG-FAELT       TO MOD-KVDAGAR-LEVNM-LDC-UPD-ATTR        
098700     MOVE MID-KVDAGAR-RET-DEF    TO MOD-KVDAGAR-RET-UPD                   
098900     MOVE MID-KVDAGAR-RTRP-DEF   TO MOD-KVDAGAR-RTRP-UPD                  
099000     MOVE MFS-ERASE-FIELD        TO MOD-KVDAGAR-RET-LDC-UPD               
099100     MOVE MFS-STAENG-FAELT       TO MOD-KVDAGAR-RET-LDC-UPD-ATTR          
099200     MOVE MFS-ERASE-FIELD        TO MOD-KVDAGAR-RTRP-LDC-UPD              
099300     MOVE MFS-STAENG-FAELT       TO MOD-KVDAGAR-RTRP-LDC-UPD-ATTR         
099400     .                                                                    
099500                                                                          
099600                                                                          
099700 EBB-FLYTTA-RAD  SECTION.                                                 
099800     MOVE 'EBB-FLYTTA-RAD  ' TO CURRENT-SECTION                           
099900                                                                          
100000     MOVE MID-KDCMD (CURR-INDX)       TO MOD-KDCMD-UPD                    
100200                                                                          
100300     MOVE MFS-STAENG-FAELT                   TO                           
100400          MOD-KDCMD-UPD-ATTR                                              
100700     MOVE MID-IDDISTR-FOM (CURR-INDX)        TO                           
100800          MOD-IDDISTR-FOM-UPD                                             
100900     MOVE MFS-STAENG-FAELT                   TO                           
101000          MOD-IDDISTR-FOM-UPD-ATTR                                        
101100                                                                          
101400     MOVE MID-IDDISTR-TOM (CURR-INDX)        TO                           
101500          MOD-IDDISTR-TOM-UPD                                             
101600     MOVE MFS-STAENG-FAELT                   TO                           
101700          MOD-IDDISTR-TOM-UPD-ATTR                                        
102000     MOVE MID-IDKUNDNR-FOM (CURR-INDX)       TO                           
102100          MOD-IDKUNDNR-FOM-UPD                                            
102200     MOVE MFS-STAENG-FAELT                   TO                           
102300          MOD-IDKUNDNR-FOM-UPD-ATTR                                       
102600     MOVE MID-IDKUNDNR-TOM (CURR-INDX)       TO                           
102700          MOD-IDKUNDNR-TOM-UPD                                            
102800     MOVE MFS-STAENG-FAELT                   TO                           
102900          MOD-IDKUNDNR-TOM-UPD-ATTR                                       
103000     MOVE MID-KDANMORS (CURR-INDX)           TO                           
103100          MOD-KDANMORS-UPD                                                
103200     MOVE MFS-STAENG-FAELT                   TO                           
103300          MOD-KDANMORS-UPD-ATTR                                           
103400                                                                          
103700     MOVE MID-KVDAGAR-LTRP (CURR-INDX)       TO                           
103800          MOD-KVDAGAR-LTRP-UPD                                            
103900     MOVE MFS-ADD-LAES-IN-FAELT              TO                           
104000          MOD-KVDAGAR-LTRP-UPD-ATTR                                       
104100                                                                          
104400     MOVE MID-KVDAGAR-LEVANM (CURR-INDX)     TO                           
104500          MOD-KVDAGAR-LEVANM-UPD                                          
104600     MOVE MFS-ADD-LAES-IN-FAELT              TO                           
104700          MOD-KVDAGAR-LEVANM-UPD-ATTR                                     
104800                                                                          
105600     MOVE MID-KVDAGAR-LTRP-LDC (CURR-INDX)   TO                           
105700          MOD-KVDAGAR-LTRP-LDC-UPD                                        
105800     MOVE MFS-ADD-LAES-IN-FAELT              TO                           
105900          MOD-KVDAGAR-LTRP-LDC-UPD-ATTR                                   
106100                                                                          
106800     MOVE MID-KVDAGAR-LEVANM-LDC (CURR-INDX) TO                           
106900          MOD-KVDAGAR-LEVANM-LDC-UPD                                      
107000     MOVE MFS-ADD-LAES-IN-FAELT              TO                           
107100          MOD-KVDAGAR-LEVNM-LDC-UPD-ATTR                                  
107300                                                                          
107600     MOVE MID-KVDAGAR-RET (CURR-INDX)        TO                           
107700          MOD-KVDAGAR-RET-UPD                                             
107800     MOVE MFS-ADD-LAES-IN-FAELT              TO                           
107900          MOD-KVDAGAR-RET-UPD-ATTR                                        
108000                                                                          
108300     MOVE MID-KVDAGAR-RTRP (CURR-INDX)       TO                           
108400          MOD-KVDAGAR-RTRP-UPD                                            
108500     MOVE MFS-ADD-LAES-IN-FAELT              TO                           
108600          MOD-KVDAGAR-RTRP-UPD-ATTR                                       
108700                                                                          
109400     MOVE MID-KVDAGAR-RET-LDC (CURR-INDX)    TO                           
109500          MOD-KVDAGAR-RET-LDC-UPD                                         
109600     MOVE MFS-ADD-LAES-IN-FAELT              TO                           
109700          MOD-KVDAGAR-RET-LDC-UPD-ATTR                                    
109900                                                                          
110600     MOVE MID-KVDAGAR-RTRP-LDC (CURR-INDX)   TO                           
110700          MOD-KVDAGAR-RTRP-LDC-UPD                                        
110800     MOVE MFS-ADD-LAES-IN-FAELT              TO                           
110900          MOD-KVDAGAR-RTRP-LDC-UPD-ATTR                                   
111100     .                                                                    
111200                                                                          
111300                                                                          
111400 F-LAES-VISA-INFO SECTION.                                                
111500     MOVE 'F-LAES-VISA-INFO' TO CURRENT-SECTION                           
111600                                                                          
111700     PERFORM IMS-GU-WDR501                                                
111800                                                                          
111900     EVALUATE TRUE                                                        
112000        WHEN VISA-ALLT                                                    
112100             PERFORM FA-VISA-ALLT                                         
112200        WHEN VISA-DISTRIKT                                                
112300             PERFORM FB-VISA-DISTRIKT                                     
112400        WHEN VISA-KUND                                                    
112500             PERFORM FC-VISA-KUND                                         
112600        WHEN VISA-ORSAK                                                   
112700             PERFORM FD-VISA-ORSAK                                        
112800        WHEN VISA-DISTRIKT-ORSAK                                          
112900             PERFORM FE-VISA-DISTRIKT-ORSAK                               
113000     END-EVALUATE                                                         
113100                                                                          
113200     PERFORM FF-VISA-DEFAULT-RAD                                          
113300     MOVE '002'         TO MSGI-KDCALL                                    
113400     MOVE '4753'        TO SPAR-IDTRANS                                   
113500     MOVE SPAR-AREA     TO MSGI-SPAR-AREA                                 
113600     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
113700     .                                                                    
113800                                                                          
113900 FA-VISA-ALLT   SECTION.                                                  
114000     MOVE 'FA-VISA-ALLT    ' TO CURRENT-SECTION                           
114100                                                                          
114200     IF MFS-FIRST                                                         
114300        MOVE ZERO             TO SPAR-IDDISTR-FOM                         
114400        MOVE 9998             TO SPAR-IDDISTR-TOM                         
114500        MOVE ZERO             TO SPAR-IDKUNDNR-FOM                        
114600        MOVE 999999           TO SPAR-IDKUNDNR-TOM                        
114700        MOVE LOW-VALUE        TO SPAR-KDANMORS-FOM                        
114800        MOVE HIGH-VALUE       TO SPAR-KDANMORS-TOM                        
114900     END-IF                                                               
115000                                                                          
115100     MOVE SPAR-IDDISTR-FOM    TO W-4128-IDDISTR-FOM                       
115200     MOVE SPAR-IDDISTR-TOM    TO W-4128-IDDISTR-TOM                       
115300     MOVE SPAR-IDKUNDNR-FOM   TO W-4128-IDKUNDNR-FOM                      
115400     MOVE SPAR-IDKUNDNR-TOM   TO W-4128-IDKUNDNR-TOM                      
115500     MOVE SPAR-KDANMORS-FOM   TO W-4128-KDANMORS-FOM                      
115600     MOVE SPAR-KDANMORS-TOM   TO W-4128-KDANMORS-TOM                      
115700                                                                          
115800     MOVE 1          TO INDX                                              
115900                                                                          
116000     PERFORM IMS-GNP-WDGX4128                                             
116100     IF SEGMENT-FINNS                                                     
116200        MOVE 4128-IDDISTR-FOM      TO SPAR-IDDISTR-FKY-ENTER              
116300        MOVE 4128-IDDISTR-TOM      TO SPAR-IDDISTR-TKY-ENTER              
116400        MOVE 4128-IDKUNDNR-FOM     TO SPAR-IDKUNDNR-FKY-ENTER             
116500        MOVE 4128-IDKUNDNR-TOM     TO SPAR-IDKUNDNR-TKY-ENTER             
116600        MOVE 4128-KDANMORS         TO SPAR-KDANMORS-KY-ENTER              
116700     END-IF                                                               
116800                                                                          
116900     PERFORM UNTIL INDX > MAX-INDX                                        
117000        IF SEGMENT-FINNS                                                  
117100           MOVE 4128-IDDISTR-FOM   TO MOD-IDDISTR-FOM  (INDX)             
117200           MOVE 4128-IDDISTR-TOM   TO MOD-IDDISTR-TOM  (INDX)             
117300           MOVE 4128-IDKUNDNR-FOM  TO MOD-IDKUNDNR-FOM (INDX)             
117400           MOVE 4128-IDKUNDNR-TOM  TO MOD-IDKUNDNR-TOM (INDX)             
117500           MOVE 4128-KDANMORS      TO MOD-KDANMORS     (INDX)             
117600           MOVE 4128-KVDAGAR-LTRP  TO MOD-KVDAGAR-LTRP (INDX)             
117700           MOVE 4128-KVDAGAR-LEVANM TO                                    
117800                                    MOD-KVDAGAR-LEVANM (INDX)             
117900           MOVE 4128-KVDAGAR-LTRP-LDC TO                                  
118000                                  MOD-KVDAGAR-LTRP-LDC (INDX)             
118100           MOVE 4128-KVDAGAR-LEVANM-LDC TO                                
118200                                MOD-KVDAGAR-LEVANM-LDC (INDX)             
118300           MOVE 4128-KVDAGAR-RET   TO MOD-KVDAGAR-RET  (INDX)             
118400           MOVE 4128-KVDAGAR-RTRP  TO MOD-KVDAGAR-RTRP (INDX)             
118500           MOVE 4128-KVDAGAR-RET-LDC TO                                   
118600                                   MOD-KVDAGAR-RET-LDC (INDX)             
118700           MOVE 4128-KVDAGAR-RTRP-LDC TO                                  
118800                                   MOD-KVDAGAR-RTRP-LDC (INDX)            
118900           ADD 1 TO INDX                                                  
119000           PERFORM IMS-GNP-WDGX4128                                       
119100        ELSE                                                              
119200           MOVE MFS-STAENG-FAELT   TO MOD-KDCMD-ATTR   (INDX)             
119300           MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-FOM  (INDX)                
119400           MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-TOM  (INDX)                
119500           MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-FOM (INDX)                
119600           MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-TOM (INDX)                
119700           MOVE MFS-RENSA-FAELT TO MOD-KDANMORS     (INDX)                
119800           MOVE MFS-RENSA-FAELT TO MOD-KVDAGAR-LTRP (INDX)                
119900           MOVE MFS-RENSA-FAELT TO  MOD-KVDAGAR-LEVANM (INDX)             
120000           MOVE MFS-RENSA-FAELT TO                                        
120100                                  MOD-KVDAGAR-LTRP-LDC (INDX)             
120200           MOVE MFS-RENSA-FAELT TO                                        
120300                                MOD-KVDAGAR-LEVANM-LDC (INDX)             
120400           MOVE MFS-RENSA-FAELT TO MOD-KVDAGAR-RET  (INDX)                
120500           MOVE MFS-RENSA-FAELT TO MOD-KVDAGAR-RTRP (INDX)                
120600           MOVE MFS-RENSA-FAELT TO MOD-KVDAGAR-RET-LDC (INDX)             
120700           MOVE MFS-RENSA-FAELT TO MOD-KVDAGAR-RTRP-LDC (INDX)            
120800           ADD 1 TO INDX                                                  
120900        END-IF                                                            
121000     END-PERFORM                                                          
121100     IF SEGMENT-FINNS                                                     
121200        MOVE 4128-IDDISTR-FOM      TO SPAR-IDDISTR-FKY-NEXT               
121300        MOVE 4128-IDDISTR-TOM      TO SPAR-IDDISTR-TKY-NEXT               
121400        MOVE 4128-IDKUNDNR-FOM     TO SPAR-IDKUNDNR-FKY-NEXT              
121500        MOVE 4128-IDKUNDNR-TOM     TO SPAR-IDKUNDNR-TKY-NEXT              
121600        MOVE 4128-KDANMORS         TO SPAR-KDANMORS-KY-NEXT               
121700        IF MFS-UPDATE                                                     
121800           CONTINUE                                                       
121900        ELSE                                                              
122000           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
122100           CALL WMEDKONV USING MED-WMEDAREA                               
122200           MOVE MED-TEMFSINF         TO MOD-TEMFSINF                      
122300        END-IF                                                            
122400     ELSE                                                                 
122500        MOVE SPAR-IDDISTR-FKY-ENTER  TO SPAR-IDDISTR-FKY-NEXT             
122600        MOVE SPAR-IDDISTR-TKY-ENTER  TO SPAR-IDDISTR-TKY-NEXT             
122700        MOVE SPAR-IDKUNDNR-FKY-ENTER TO SPAR-IDKUNDNR-FKY-NEXT            
122800        MOVE SPAR-IDKUNDNR-TKY-ENTER TO SPAR-IDKUNDNR-TKY-NEXT            
122900        MOVE SPAR-KDANMORS-KY-ENTER  TO SPAR-KDANMORS-KY-NEXT             
123000     END-IF                                                               
123100     .                                                                    
123200                                                                          
123300                                                                          
123400 FB-VISA-DISTRIKT SECTION.                                                
123500     MOVE 'FB-VISA-DISTRIKT' TO CURRENT-SECTION                           
123600                                                                          
123700     IF MFS-FIRST                                                         
123800        MOVE W-IDDISTR             TO W-IDDISTR-NUM                       
123900        MOVE W-IDDISTR-NUM         TO SPAR-IDDISTR-FOM                    
124000                                      SPAR-IDDISTR-TOM                    
124100        MOVE ZERO                  TO SPAR-IDKUNDNR-FOM                   
124200        MOVE 999999                TO SPAR-IDKUNDNR-TOM                   
124300        MOVE LOW-VALUE             TO SPAR-KDANMORS-FOM                   
124400        MOVE HIGH-VALUE            TO SPAR-KDANMORS-TOM                   
124500     END-IF                                                               
124600                                                                          
124700     MOVE SPAR-IDDISTR-FOM         TO W-4128-IDDISTR-FOM                  
124800     MOVE SPAR-IDDISTR-TOM         TO W-4128-IDDISTR-TOM                  
124900     MOVE SPAR-IDKUNDNR-FOM        TO W-4128-IDKUNDNR-FOM                 
125000     MOVE SPAR-IDKUNDNR-TOM        TO W-4128-IDKUNDNR-TOM                 
125100     MOVE SPAR-KDANMORS-FOM        TO W-4128-KDANMORS-FOM                 
125200     MOVE SPAR-KDANMORS-TOM        TO W-4128-KDANMORS-TOM                 
125300                                                                          
125400     MOVE 1             TO INDX                                           
125500                                                                          
125600     PERFORM IMS-GNP-WDGX4128-DI                                          
125700     IF SEGMENT-FINNS                                                     
125800        MOVE 4128-IDDISTR-FOM      TO SPAR-IDDISTR-FKY-ENTER              
125900        MOVE 4128-IDDISTR-TOM      TO SPAR-IDDISTR-TKY-ENTER              
126000        MOVE 4128-IDKUNDNR-FOM     TO SPAR-IDKUNDNR-FKY-ENTER             
126100        MOVE 4128-IDKUNDNR-TOM     TO SPAR-IDKUNDNR-TKY-ENTER             
126200        MOVE 4128-KDANMORS         TO SPAR-KDANMORS-KY-ENTER              
126300     END-IF                                                               
126400                                                                          
126500     PERFORM UNTIL INDX > MAX-INDX                                        
126600        IF SEGMENT-FINNS                                                  
126610           IF  4128-IDDISTR-FOM = 1                                       
126620           AND 4128-IDDISTR-TOM = 9999                                    
126630*             DEFAULT-RAD, VISAS I SECTION FF-                            
126640              CONTINUE                                                    
126650           ELSE                                                           
126700              MOVE 4128-IDDISTR-FOM    TO MOD-IDDISTR-FOM (INDX)          
126800              MOVE 4128-IDDISTR-TOM    TO MOD-IDDISTR-TOM (INDX)          
126900              MOVE 4128-IDKUNDNR-FOM   TO MOD-IDKUNDNR-FOM (INDX)         
127000              MOVE 4128-IDKUNDNR-TOM   TO MOD-IDKUNDNR-TOM (INDX)         
127100              MOVE 4128-KDANMORS       TO MOD-KDANMORS     (INDX)         
127200              MOVE 4128-KVDAGAR-LTRP   TO MOD-KVDAGAR-LTRP (INDX)         
127300              MOVE 4128-KVDAGAR-LEVANM TO                                 
127400                                       MOD-KVDAGAR-LEVANM (INDX)          
127500              MOVE 4128-KVDAGAR-LTRP-LDC TO                               
127600                                     MOD-KVDAGAR-LTRP-LDC (INDX)          
127700              MOVE 4128-KVDAGAR-LEVANM-LDC TO                             
127800                                   MOD-KVDAGAR-LEVANM-LDC (INDX)          
127900              MOVE 4128-KVDAGAR-RET    TO MOD-KVDAGAR-RET (INDX)          
128000              MOVE 4128-KVDAGAR-RTRP   TO MOD-KVDAGAR-RTRP (INDX)         
128100              MOVE 4128-KVDAGAR-RET-LDC TO                                
128200                                      MOD-KVDAGAR-RET-LDC (INDX)          
128300              MOVE 4128-KVDAGAR-RTRP-LDC TO                               
128400                                      MOD-KVDAGAR-RTRP-LDC (INDX)         
128500              ADD 1 TO INDX                                               
128510           END-IF                                                         
128600           PERFORM IMS-GNP-WDGX4128-DI                                    
128700        ELSE                                                              
128800           MOVE MFS-STAENG-FAELT   TO MOD-KDCMD-ATTR   (INDX)             
128900           MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-FOM  (INDX)                
129000           MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-TOM  (INDX)                
129100           MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-FOM (INDX)                
129200           MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-TOM (INDX)                
129300           MOVE MFS-RENSA-FAELT TO MOD-KDANMORS     (INDX)                
129400           MOVE MFS-RENSA-FAELT TO MOD-KVDAGAR-LTRP (INDX)                
129500           MOVE MFS-RENSA-FAELT TO  MOD-KVDAGAR-LEVANM (INDX)             
129600           MOVE MFS-RENSA-FAELT TO                                        
129700                                  MOD-KVDAGAR-LTRP-LDC (INDX)             
129800           MOVE MFS-RENSA-FAELT TO                                        
129900                                MOD-KVDAGAR-LEVANM-LDC (INDX)             
130000           MOVE MFS-RENSA-FAELT TO MOD-KVDAGAR-RET  (INDX)                
130100           MOVE MFS-RENSA-FAELT TO MOD-KVDAGAR-RTRP (INDX)                
130200           MOVE MFS-RENSA-FAELT TO MOD-KVDAGAR-RET-LDC (INDX)             
130300           MOVE MFS-RENSA-FAELT TO MOD-KVDAGAR-RTRP-LDC (INDX)            
130400           ADD 1 TO INDX                                                  
130500        END-IF                                                            
130600     END-PERFORM                                                          
130700     IF SEGMENT-FINNS                                                     
130800        MOVE 4128-IDDISTR-FOM   TO SPAR-IDDISTR-FKY-NEXT                  
130900        MOVE 4128-IDDISTR-TOM   TO SPAR-IDDISTR-TKY-NEXT                  
131000        MOVE 4128-IDKUNDNR-FOM  TO SPAR-IDKUNDNR-FKY-NEXT                 
131100        MOVE 4128-IDKUNDNR-TOM  TO SPAR-IDKUNDNR-TKY-NEXT                 
131200        MOVE 4128-KDANMORS      TO SPAR-KDANMORS-KY-NEXT                  
131300        IF MFS-UPDATE                                                     
131400           CONTINUE                                                       
131500        ELSE                                                              
131600           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
131700           CALL WMEDKONV USING MED-WMEDAREA                               
131800           MOVE MED-TEMFSINF         TO MOD-TEMFSINF                      
131900        END-IF                                                            
132000     ELSE                                                                 
132100        MOVE SPAR-IDDISTR-FKY-ENTER  TO SPAR-IDDISTR-FKY-NEXT             
132200        MOVE SPAR-IDDISTR-TKY-ENTER  TO SPAR-IDDISTR-TKY-NEXT             
132300        MOVE SPAR-IDKUNDNR-FKY-ENTER TO SPAR-IDKUNDNR-FKY-NEXT            
132400        MOVE SPAR-IDKUNDNR-TKY-ENTER TO SPAR-IDKUNDNR-TKY-NEXT            
132500        MOVE SPAR-KDANMORS-KY-ENTER  TO SPAR-KDANMORS-KY-NEXT             
132600     END-IF                                                               
132700     .                                                                    
132800                                                                          
132900                                                                          
133000 FC-VISA-KUND     SECTION.                                                
133100     MOVE 'FC-VISA-KUND    ' TO CURRENT-SECTION                           
133200                                                                          
133300     IF MFS-FIRST                                                         
133400        MOVE W-IDDISTR             TO W-IDDISTR-NUM                       
133500        MOVE W-IDDISTR-NUM         TO SPAR-IDDISTR-FOM                    
133600                                      SPAR-IDDISTR-TOM                    
133700        MOVE W-IDKUNDNR            TO W-IDKUNDNR-NUM                      
133800        MOVE W-IDKUNDNR-NUM        TO SPAR-IDKUNDNR-FOM                   
133900                                      SPAR-IDKUNDNR-TOM                   
134000        MOVE LOW-VALUE             TO SPAR-KDANMORS-FOM                   
134100        MOVE HIGH-VALUE            TO SPAR-KDANMORS-TOM                   
134200     END-IF                                                               
134300                                                                          
134400     MOVE SPAR-IDDISTR-FOM         TO W-4128-IDDISTR-FOM                  
134500     MOVE SPAR-IDDISTR-TOM         TO W-4128-IDDISTR-TOM                  
134600     MOVE SPAR-IDKUNDNR-FOM        TO W-4128-IDKUNDNR-FOM                 
134700     MOVE SPAR-IDKUNDNR-TOM        TO W-4128-IDKUNDNR-TOM                 
134800     MOVE SPAR-KDANMORS-FOM        TO W-4128-KDANMORS-FOM                 
134900     MOVE SPAR-KDANMORS-TOM        TO W-4128-KDANMORS-TOM                 
135000                                                                          
135100     MOVE 1             TO INDX                                           
135200                                                                          
135300     PERFORM IMS-GNP-WDGX4128-KU                                          
135400     IF SEGMENT-FINNS                                                     
135500        MOVE 4128-IDDISTR-FOM      TO SPAR-IDDISTR-FKY-ENTER              
135600        MOVE 4128-IDDISTR-TOM      TO SPAR-IDDISTR-TKY-ENTER              
135700        MOVE 4128-IDKUNDNR-FOM     TO SPAR-IDKUNDNR-FKY-ENTER             
135800        MOVE 4128-IDKUNDNR-TOM     TO SPAR-IDKUNDNR-TKY-ENTER             
135900        MOVE 4128-KDANMORS         TO SPAR-KDANMORS-KY-ENTER              
136000     END-IF                                                               
136100                                                                          
136200     PERFORM UNTIL INDX > MAX-INDX                                        
136300        IF SEGMENT-FINNS                                                  
136400           MOVE 4128-IDDISTR-FOM   TO MOD-IDDISTR-FOM  (INDX)             
136500           MOVE 4128-IDDISTR-TOM   TO MOD-IDDISTR-TOM  (INDX)             
136600           MOVE 4128-IDKUNDNR-FOM  TO MOD-IDKUNDNR-FOM (INDX)             
136700           MOVE 4128-IDKUNDNR-TOM  TO MOD-IDKUNDNR-TOM (INDX)             
136800           MOVE 4128-KDANMORS      TO MOD-KDANMORS     (INDX)             
136900           MOVE 4128-KVDAGAR-LTRP  TO MOD-KVDAGAR-LTRP (INDX)             
137000           MOVE 4128-KVDAGAR-LEVANM TO                                    
137100                                    MOD-KVDAGAR-LEVANM (INDX)             
137200           MOVE 4128-KVDAGAR-LTRP-LDC TO                                  
137300                                  MOD-KVDAGAR-LTRP-LDC (INDX)             
137400           MOVE 4128-KVDAGAR-LEVANM-LDC TO                                
137500                                MOD-KVDAGAR-LEVANM-LDC (INDX)             
137600           MOVE 4128-KVDAGAR-RET   TO MOD-KVDAGAR-RET  (INDX)             
137700           MOVE 4128-KVDAGAR-RTRP  TO MOD-KVDAGAR-RTRP (INDX)             
137800           MOVE 4128-KVDAGAR-RET-LDC TO                                   
137900                                   MOD-KVDAGAR-RET-LDC (INDX)             
138000           MOVE 4128-KVDAGAR-RTRP-LDC TO                                  
138100                                   MOD-KVDAGAR-RTRP-LDC (INDX)            
138200           ADD 1 TO INDX                                                  
138300           PERFORM IMS-GNP-WDGX4128-KU                                    
138400        ELSE                                                              
138500           MOVE MFS-STAENG-FAELT   TO MOD-KDCMD-ATTR   (INDX)             
138600           MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-FOM  (INDX)                
138700           MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-TOM  (INDX)                
138800           MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-FOM (INDX)                
138900           MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-TOM (INDX)                
139000           MOVE MFS-RENSA-FAELT TO MOD-KDANMORS     (INDX)                
139100           MOVE MFS-RENSA-FAELT TO MOD-KVDAGAR-LTRP (INDX)                
139200           MOVE MFS-RENSA-FAELT TO  MOD-KVDAGAR-LEVANM (INDX)             
139300           MOVE MFS-RENSA-FAELT TO                                        
139400                                  MOD-KVDAGAR-LTRP-LDC (INDX)             
139500           MOVE MFS-RENSA-FAELT TO                                        
139600                                MOD-KVDAGAR-LEVANM-LDC (INDX)             
139700           MOVE MFS-RENSA-FAELT TO MOD-KVDAGAR-RET  (INDX)                
139800           MOVE MFS-RENSA-FAELT TO MOD-KVDAGAR-RTRP (INDX)                
139900           MOVE MFS-RENSA-FAELT TO MOD-KVDAGAR-RET-LDC (INDX)             
140000           MOVE MFS-RENSA-FAELT TO MOD-KVDAGAR-RTRP-LDC (INDX)            
140100           ADD 1 TO INDX                                                  
140200        END-IF                                                            
140300     END-PERFORM                                                          
140400     IF SEGMENT-FINNS                                                     
140500        MOVE 4128-IDDISTR-FOM      TO SPAR-IDDISTR-FKY-NEXT               
140600        MOVE 4128-IDDISTR-TOM      TO SPAR-IDDISTR-TKY-NEXT               
140700        MOVE 4128-IDKUNDNR-FOM     TO SPAR-IDKUNDNR-FKY-NEXT              
140800        MOVE 4128-IDKUNDNR-TOM     TO SPAR-IDKUNDNR-TKY-NEXT              
140900        MOVE 4128-KDANMORS         TO SPAR-KDANMORS-KY-NEXT               
141000        IF MFS-UPDATE                                                     
141100           CONTINUE                                                       
141200        ELSE                                                              
141300           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
141400           CALL WMEDKONV USING MED-WMEDAREA                               
141500           MOVE MED-TEMFSINF         TO MOD-TEMFSINF                      
141600        END-IF                                                            
141700     ELSE                                                                 
141800        MOVE SPAR-IDDISTR-FKY-ENTER  TO SPAR-IDDISTR-FKY-NEXT             
141900        MOVE SPAR-IDDISTR-TKY-ENTER  TO SPAR-IDDISTR-TKY-NEXT             
142000        MOVE SPAR-IDKUNDNR-FKY-ENTER TO SPAR-IDKUNDNR-FKY-NEXT            
142100        MOVE SPAR-IDKUNDNR-TKY-ENTER TO SPAR-IDKUNDNR-TKY-NEXT            
142200        MOVE SPAR-KDANMORS-KY-ENTER  TO SPAR-KDANMORS-KY-NEXT             
142300     END-IF                                                               
142400     .                                                                    
142500                                                                          
142600                                                                          
142700 FD-VISA-ORSAK    SECTION.                                                
142800     MOVE 'FD-VISA-ORSAK   ' TO CURRENT-SECTION                           
142900                                                                          
142910     MOVE W-KDANMORS               TO W-KDANMORS-NX                       
142920     MOVE 'X'                      TO W-KDANMORS-NX-POS2                  
143000     IF MFS-FIRST                                                         
143100        MOVE W-IDDISTR             TO W-IDDISTR-NUM                       
143200        MOVE W-IDDISTR-NUM         TO SPAR-IDDISTR-FOM                    
143300                                      SPAR-IDDISTR-TOM                    
143400        MOVE W-IDKUNDNR            TO W-IDKUNDNR-NUM                      
143500        MOVE W-IDKUNDNR-NUM        TO SPAR-IDKUNDNR-FOM                   
143600                                      SPAR-IDKUNDNR-TOM                   
143700        IF W-KDANMORS-POS2 = 'X'                                          
143900           MOVE W-KDANMORS         TO SPAR-KDANMORS-FOM                   
144000           MOVE 9                  TO W-KDANMORS-POS2                     
144100           MOVE W-KDANMORS         TO SPAR-KDANMORS-TOM                   
144200        ELSE                                                              
144300           MOVE W-KDANMORS         TO SPAR-KDANMORS-FOM                   
144400                                      SPAR-KDANMORS-TOM                   
144500        END-IF                                                            
144600     END-IF                                                               
144700                                                                          
144800     MOVE SPAR-IDDISTR-FOM         TO W-4128-IDDISTR-FOM                  
144900     MOVE SPAR-IDDISTR-TOM         TO W-4128-IDDISTR-TOM                  
145000     MOVE SPAR-IDKUNDNR-FOM        TO W-4128-IDKUNDNR-FOM                 
145100     MOVE SPAR-IDKUNDNR-TOM        TO W-4128-IDKUNDNR-TOM                 
145200     MOVE SPAR-KDANMORS-FOM        TO W-4128-KDANMORS-FOM                 
145300     MOVE SPAR-KDANMORS-TOM        TO W-4128-KDANMORS-TOM                 
145400                                                                          
145500     MOVE 1             TO INDX                                           
145600                                                                          
145700     PERFORM IMS-GNP-WDGX4128-KD                                          
145800     IF SEGMENT-FINNS                                                     
145900        MOVE NEJ TO INTERVALL-SW                                          
146000     ELSE                                                                 
146100        MOVE JA  TO INTERVALL-SW                                          
146200        PERFORM IMS-GU-WDR501                                             
146300        PERFORM IMS-GNP-WDGX4128-KDI                                      
146400     END-IF                                                               
146410                                                                          
146500     IF SEGMENT-FINNS                                                     
146600        MOVE 4128-IDDISTR-FOM      TO SPAR-IDDISTR-FKY-ENTER              
146700        MOVE 4128-IDDISTR-TOM      TO SPAR-IDDISTR-TKY-ENTER              
146800        MOVE 4128-IDKUNDNR-FOM     TO SPAR-IDKUNDNR-FKY-ENTER             
146900        MOVE 4128-IDKUNDNR-TOM     TO SPAR-IDKUNDNR-TKY-ENTER             
147000        MOVE 4128-KDANMORS         TO SPAR-KDANMORS-KY-ENTER              
147100     END-IF                                                               
147200                                                                          
147300     PERFORM UNTIL INDX > MAX-INDX                                        
147400        IF SEGMENT-FINNS                                                  
147500           MOVE 4128-IDDISTR-FOM   TO MOD-IDDISTR-FOM  (INDX)             
147600           MOVE 4128-IDDISTR-TOM   TO MOD-IDDISTR-TOM  (INDX)             
147700           MOVE 4128-IDKUNDNR-FOM  TO MOD-IDKUNDNR-FOM (INDX)             
147800           MOVE 4128-IDKUNDNR-TOM  TO MOD-IDKUNDNR-TOM (INDX)             
147900           MOVE 4128-KDANMORS      TO MOD-KDANMORS     (INDX)             
148000           MOVE 4128-KVDAGAR-LTRP  TO MOD-KVDAGAR-LTRP (INDX)             
148100           MOVE 4128-KVDAGAR-LEVANM TO                                    
148200                                    MOD-KVDAGAR-LEVANM (INDX)             
148300           MOVE 4128-KVDAGAR-LTRP-LDC TO                                  
148400                                  MOD-KVDAGAR-LTRP-LDC (INDX)             
148500           MOVE 4128-KVDAGAR-LEVANM-LDC TO                                
148600                                MOD-KVDAGAR-LEVANM-LDC (INDX)             
148700           MOVE 4128-KVDAGAR-RET   TO MOD-KVDAGAR-RET  (INDX)             
148800           MOVE 4128-KVDAGAR-RTRP  TO MOD-KVDAGAR-RTRP (INDX)             
148900           MOVE 4128-KVDAGAR-RET-LDC TO                                   
149000                                   MOD-KVDAGAR-RET-LDC (INDX)             
149100           MOVE 4128-KVDAGAR-RTRP-LDC TO                                  
149200                                   MOD-KVDAGAR-RTRP-LDC (INDX)            
149300           ADD 1 TO INDX                                                  
149400           IF KUNDINTERVALL                                               
149500              PERFORM IMS-GNP-WDGX4128-KDI                                
149600           ELSE                                                           
149700              PERFORM IMS-GNP-WDGX4128-KD                                 
149800           END-IF                                                         
149900        ELSE                                                              
150000           MOVE MFS-STAENG-FAELT   TO MOD-KDCMD-ATTR   (INDX)             
150100           MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-FOM  (INDX)                
150200           MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-TOM  (INDX)                
150300           MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-FOM (INDX)                
150400           MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-TOM (INDX)                
150500           MOVE MFS-RENSA-FAELT TO MOD-KDANMORS     (INDX)                
150600           MOVE MFS-RENSA-FAELT TO MOD-KVDAGAR-LTRP (INDX)                
150700           MOVE MFS-RENSA-FAELT TO  MOD-KVDAGAR-LEVANM (INDX)             
150800           MOVE MFS-RENSA-FAELT TO                                        
150900                                  MOD-KVDAGAR-LTRP-LDC (INDX)             
151000           MOVE MFS-RENSA-FAELT TO                                        
151100                                MOD-KVDAGAR-LEVANM-LDC (INDX)             
151200           MOVE MFS-RENSA-FAELT TO MOD-KVDAGAR-RET  (INDX)                
151300           MOVE MFS-RENSA-FAELT TO MOD-KVDAGAR-RTRP (INDX)                
151400           MOVE MFS-RENSA-FAELT TO MOD-KVDAGAR-RET-LDC (INDX)             
151500           MOVE MFS-RENSA-FAELT TO MOD-KVDAGAR-RTRP-LDC (INDX)            
151600           ADD 1 TO INDX                                                  
151700        END-IF                                                            
151800     END-PERFORM                                                          
151900     IF SEGMENT-FINNS                                                     
152000        MOVE 4128-IDDISTR-FOM      TO SPAR-IDDISTR-FKY-NEXT               
152100        MOVE 4128-IDDISTR-TOM      TO SPAR-IDDISTR-TKY-NEXT               
152200        MOVE 4128-IDKUNDNR-FOM     TO SPAR-IDKUNDNR-FKY-NEXT              
152300        MOVE 4128-IDKUNDNR-TOM     TO SPAR-IDKUNDNR-TKY-NEXT              
152400        MOVE 4128-KDANMORS         TO SPAR-KDANMORS-KY-NEXT               
152500        IF MFS-UPDATE                                                     
152600           CONTINUE                                                       
152700        ELSE                                                              
152800           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
152900           CALL WMEDKONV USING MED-WMEDAREA                               
153000           MOVE MED-TEMFSINF         TO MOD-TEMFSINF                      
153100        END-IF                                                            
153200     ELSE                                                                 
153300        MOVE SPAR-IDDISTR-FKY-ENTER  TO SPAR-IDDISTR-FKY-NEXT             
153400        MOVE SPAR-IDDISTR-TKY-ENTER  TO SPAR-IDDISTR-TKY-NEXT             
153500        MOVE SPAR-IDKUNDNR-FKY-ENTER TO SPAR-IDKUNDNR-FKY-NEXT            
153600        MOVE SPAR-IDKUNDNR-TKY-ENTER TO SPAR-IDKUNDNR-TKY-NEXT            
153700        MOVE SPAR-KDANMORS-KY-ENTER  TO SPAR-KDANMORS-KY-NEXT             
153800     END-IF                                                               
153900     .                                                                    
154000                                                                          
154100                                                                          
154200 FE-VISA-DISTRIKT-ORSAK SECTION.                                          
154300     MOVE 'FE-VISA-DIST-ORS' TO CURRENT-SECTION                           
154400                                                                          
154410     MOVE W-KDANMORS               TO W-KDANMORS-NX                       
154420     MOVE 'X'                      TO W-KDANMORS-NX-POS2                  
154500     IF MFS-FIRST                                                         
154600        MOVE W-IDDISTR             TO W-IDDISTR-NUM                       
154700        MOVE W-IDDISTR-NUM         TO SPAR-IDDISTR-FOM                    
154800                                      SPAR-IDDISTR-TOM                    
154900        MOVE ZERO                  TO SPAR-IDKUNDNR-FOM                   
155000        MOVE 999999                TO SPAR-IDKUNDNR-TOM                   
155100        IF W-KDANMORS-POS2 = 'X'                                          
155300           MOVE W-KDANMORS         TO SPAR-KDANMORS-FOM                   
155400           MOVE 9                  TO W-KDANMORS-POS2                     
155500           MOVE W-KDANMORS         TO SPAR-KDANMORS-TOM                   
155600        ELSE                                                              
155700           MOVE W-KDANMORS         TO SPAR-KDANMORS-FOM                   
155800                                      SPAR-KDANMORS-TOM                   
155900        END-IF                                                            
156000     END-IF                                                               
156100                                                                          
156200     MOVE SPAR-IDDISTR-FOM         TO W-4128-IDDISTR-FOM                  
156300     MOVE SPAR-IDDISTR-TOM         TO W-4128-IDDISTR-TOM                  
156400     MOVE SPAR-IDKUNDNR-FOM        TO W-4128-IDKUNDNR-FOM                 
156500     MOVE SPAR-IDKUNDNR-TOM        TO W-4128-IDKUNDNR-TOM                 
156600     MOVE SPAR-KDANMORS-FOM        TO W-4128-KDANMORS-FOM                 
156700     MOVE SPAR-KDANMORS-TOM        TO W-4128-KDANMORS-TOM                 
156800                                                                          
156900     MOVE 1             TO INDX                                           
157000                                                                          
157100     PERFORM IMS-GNP-WDGX4128-DIK                                         
157200     IF SEGMENT-FINNS                                                     
157300        MOVE 4128-IDDISTR-FOM      TO SPAR-IDDISTR-FKY-ENTER              
157400        MOVE 4128-IDDISTR-TOM      TO SPAR-IDDISTR-TKY-ENTER              
157500        MOVE 4128-IDKUNDNR-FOM     TO SPAR-IDKUNDNR-FKY-ENTER             
157600        MOVE 4128-IDKUNDNR-TOM     TO SPAR-IDKUNDNR-TKY-ENTER             
157700        MOVE 4128-KDANMORS         TO SPAR-KDANMORS-KY-ENTER              
157800     END-IF                                                               
157900                                                                          
158000     PERFORM UNTIL INDX > MAX-INDX                                        
158100        IF SEGMENT-FINNS                                                  
158200           MOVE 4128-IDDISTR-FOM   TO MOD-IDDISTR-FOM  (INDX)             
158300           MOVE 4128-IDDISTR-TOM   TO MOD-IDDISTR-TOM  (INDX)             
158400           MOVE 4128-IDKUNDNR-FOM  TO MOD-IDKUNDNR-FOM (INDX)             
158500           MOVE 4128-IDKUNDNR-TOM  TO MOD-IDKUNDNR-TOM (INDX)             
158600           MOVE 4128-KDANMORS      TO MOD-KDANMORS     (INDX)             
158700           MOVE 4128-KVDAGAR-LTRP  TO MOD-KVDAGAR-LTRP (INDX)             
158800           MOVE 4128-KVDAGAR-LEVANM TO                                    
158900                                    MOD-KVDAGAR-LEVANM (INDX)             
159000           MOVE 4128-KVDAGAR-LTRP-LDC TO                                  
159100                                  MOD-KVDAGAR-LTRP-LDC (INDX)             
159200           MOVE 4128-KVDAGAR-LEVANM-LDC TO                                
159300                                MOD-KVDAGAR-LEVANM-LDC (INDX)             
159400           MOVE 4128-KVDAGAR-RET   TO MOD-KVDAGAR-RET  (INDX)             
159500           MOVE 4128-KVDAGAR-RTRP  TO MOD-KVDAGAR-RTRP (INDX)             
159600           MOVE 4128-KVDAGAR-RET-LDC TO                                   
159700                                   MOD-KVDAGAR-RET-LDC (INDX)             
159800           MOVE 4128-KVDAGAR-RTRP-LDC TO                                  
159900                                   MOD-KVDAGAR-RTRP-LDC (INDX)            
160000           ADD 1 TO INDX                                                  
160100           PERFORM IMS-GNP-WDGX4128-DIK                                   
160200        ELSE                                                              
160300           MOVE MFS-STAENG-FAELT   TO MOD-KDCMD-ATTR   (INDX)             
160400           MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-FOM  (INDX)                
160500           MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-TOM  (INDX)                
160600           MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-FOM (INDX)                
160700           MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-TOM (INDX)                
160800           MOVE MFS-RENSA-FAELT TO MOD-KDANMORS     (INDX)                
160900           MOVE MFS-RENSA-FAELT TO MOD-KVDAGAR-LTRP (INDX)                
161000           MOVE MFS-RENSA-FAELT TO  MOD-KVDAGAR-LEVANM (INDX)             
161100           MOVE MFS-RENSA-FAELT TO                                        
161200                                  MOD-KVDAGAR-LTRP-LDC (INDX)             
161300           MOVE MFS-RENSA-FAELT TO                                        
161400                                MOD-KVDAGAR-LEVANM-LDC (INDX)             
161500           MOVE MFS-RENSA-FAELT TO MOD-KVDAGAR-RET  (INDX)                
161600           MOVE MFS-RENSA-FAELT TO MOD-KVDAGAR-RTRP (INDX)                
161700           MOVE MFS-RENSA-FAELT TO MOD-KVDAGAR-RET-LDC (INDX)             
161800           MOVE MFS-RENSA-FAELT TO MOD-KVDAGAR-RTRP-LDC (INDX)            
161900           ADD 1 TO INDX                                                  
162000        END-IF                                                            
162100     END-PERFORM                                                          
162200     IF SEGMENT-FINNS                                                     
162300        MOVE 4128-IDDISTR-FOM      TO SPAR-IDDISTR-FKY-NEXT               
162400        MOVE 4128-IDDISTR-TOM      TO SPAR-IDDISTR-TKY-NEXT               
162500        MOVE 4128-IDKUNDNR-FOM     TO SPAR-IDKUNDNR-FKY-NEXT              
162600        MOVE 4128-IDKUNDNR-TOM     TO SPAR-IDKUNDNR-TKY-NEXT              
162700        MOVE 4128-KDANMORS         TO SPAR-KDANMORS-KY-NEXT               
162800        IF MFS-UPDATE                                                     
162900           CONTINUE                                                       
163000        ELSE                                                              
163100           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
163200           CALL WMEDKONV USING MED-WMEDAREA                               
163300           MOVE MED-TEMFSINF         TO MOD-TEMFSINF                      
163400        END-IF                                                            
163500     ELSE                                                                 
163600        MOVE SPAR-IDDISTR-FKY-ENTER  TO SPAR-IDDISTR-FKY-NEXT             
163700        MOVE SPAR-IDDISTR-TKY-ENTER  TO SPAR-IDDISTR-TKY-NEXT             
163800        MOVE SPAR-IDKUNDNR-FKY-ENTER TO SPAR-IDKUNDNR-FKY-NEXT            
163900        MOVE SPAR-IDKUNDNR-TKY-ENTER TO SPAR-IDKUNDNR-TKY-NEXT            
164000        MOVE SPAR-KDANMORS-KY-ENTER  TO SPAR-KDANMORS-KY-NEXT             
164100     END-IF                                                               
164200     .                                                                    
164300                                                                          
164400                                                                          
164500 FF-VISA-DEFAULT-RAD SECTION.                                             
164600     MOVE 'FF-VISA-DEFAULT ' TO CURRENT-SECTION                           
164700                                                                          
164800     MOVE 1                      TO W-4128-IDDISTR-FKY                    
164900     MOVE 9999                   TO W-4128-IDDISTR-TKY                    
165000     MOVE ZERO                   TO W-4128-IDKUNDNR-FKY                   
165100     MOVE 999999                 TO W-4128-IDKUNDNR-TKY                   
165200     MOVE SPACE                  TO W-4128-KDANMORS-KY                    
165300     PERFORM IMS-GU-WDGX4128                                              
165400                                                                          
165500     IF SEGMENT-FINNS                                                     
165600        MOVE 4128-IDDISTR-FOM    TO MOD-IDDISTR-FOM-DEF                   
165700        MOVE 4128-IDDISTR-TOM    TO MOD-IDDISTR-TOM-DEF                   
165800        MOVE 4128-KVDAGAR-LTRP   TO MOD-KVDAGAR-LTRP-DEF                  
165900        MOVE 4128-KVDAGAR-LEVANM TO MOD-KVDAGAR-LEVANM-DEF                
166000        MOVE 4128-KVDAGAR-RET    TO MOD-KVDAGAR-RET-DEF                   
166100        MOVE 4128-KVDAGAR-RTRP   TO MOD-KVDAGAR-RTRP-DEF                  
166200     END-IF                                                               
166300     .                                                                    
166400                                                                          
166500                                                                          
166600 G-KOLLA-INDATA SECTION.                                                  
166700     MOVE 'G-KOLLA-INDATA  ' TO CURRENT-SECTION                           
166800                                                                          
166900     MOVE JA   TO INDATA-SW                                               
167000                                                                          
167100     PERFORM GA-KOLLA-OM-INPUT                                            
167200                                                                          
167300     IF INDATA-FEL                                                        
167400        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
167500        MOVE NEJ                  TO INDATA-SW                            
167600     ELSE                                                                 
167700       PERFORM GB-KOLLA-CMD                                               
167800       IF INDATA-OK                                                       
167900          IF CMD-UPDATE                                                   
168000             PERFORM GC-KOLLA-UPDATE                                      
168100          ELSE                                                            
168200             IF CMD-NEW                                                   
168300                PERFORM GD-KOLLA-NEW                                      
168310             ELSE                                                         
168320                IF CMD-DELETE                                             
168330                   PERFORM GE-KOLLA-DELETE                                
168340                END-IF                                                    
168400             END-IF                                                       
168500          END-IF                                                          
168600       END-IF                                                             
168700     END-IF                                                               
168800                                                                          
168900     IF INDATA-FEL                                                        
169000        CALL WMEDKONV USING MED-WMEDAREA                                  
169100        MOVE MED-MFSFEL  TO MOD-TEMFSFEL                                  
169200        PERFORM MFS-ROER-EJ-FAELT-IN                                      
169300        PERFORM MFS-ROER-EJ-FAELT-UT                                      
169400     END-IF                                                               
169500     .                                                                    
169600                                                                          
169700                                                                          
169800 GA-KOLLA-OM-INPUT SECTION.                                               
169900     MOVE 'GA-KOLLA-OM-INP ' TO CURRENT-SECTION                           
170000                                                                          
170100     MOVE NEJ TO  INDATA-SW                                               
170200     MOVE 1 TO INDX                                                       
170300     PERFORM UNTIL INDX > MAX-INDX                                        
170400                OR INDATA-OK                                              
170500        IF MID-KDCMD (INDX) NOT = ALL '+' AND SPACE                       
170600           MOVE JA TO INDATA-SW                                           
170700        END-IF                                                            
170800        ADD 1 TO INDX                                                     
170900     END-PERFORM                                                          
171000                                                                          
171100     IF INDATA-FEL                                                        
171200        IF MID-KDCMD-DEF NOT = ALL '+' AND SPACE                          
171300           MOVE JA TO INDATA-SW                                           
171400        END-IF                                                            
171500     END-IF                                                               
171600                                                                          
171700     IF INDATA-FEL                                                        
171800        IF MID-KDCMD-UPD = 'C'                                            
171900           IF MID-KVDAGAR-LTRP-UPD    NOT = ALL '+'                       
172000           OR MID-KVDAGAR-LEVANM-UPD  NOT = ALL '+'                       
172100           OR MID-KVDAGAR-LTRP-LDC-UPD NOT = ALL '+'                      
172200           OR MID-KVDAGAR-LEVANM-LDC-UPD NOT = ALL '+'                    
172300           OR MID-KVDAGAR-RET-UPD     NOT = ALL '+'                       
172400           OR MID-KVDAGAR-RTRP-UPD    NOT = ALL '+'                       
172500           OR MID-KVDAGAR-RET-LDC-UPD NOT = ALL '+'                       
172600           OR MID-KVDAGAR-RTRP-LDC-UPD NOT = ALL '+'                      
172700                                                                          
172800              MOVE JA TO INDATA-SW                                        
172900           END-IF                                                         
173000        ELSE                                                              
173100           IF MID-KDCMD-UPD           NOT = ALL '+'                       
173200           OR MID-IDDISTR-FOM-UPD     NOT = ALL '+'                       
173300           OR MID-IDDISTR-TOM-UPD     NOT = ALL '+'                       
173400           OR MID-IDKUNDNR-FOM-UPD    NOT = ALL '+'                       
173500           OR MID-IDKUNDNR-TOM-UPD    NOT = ALL '+'                       
173600           OR MID-KDANMORS-UPD        NOT = ALL '+'                       
173700           OR MID-KVDAGAR-LTRP-UPD    NOT = ALL '+'                       
173800           OR MID-KVDAGAR-LEVANM-UPD  NOT = ALL '+'                       
173900           OR MID-KVDAGAR-LTRP-LDC-UPD NOT = ALL '+'                      
174000           OR MID-KVDAGAR-LEVANM-LDC-UPD NOT = ALL '+'                    
174100           OR MID-KVDAGAR-RET-UPD     NOT = ALL '+'                       
174200           OR MID-KVDAGAR-RTRP-UPD    NOT = ALL '+'                       
174300           OR MID-KVDAGAR-RET-LDC-UPD NOT = ALL '+'                       
174400           OR MID-KVDAGAR-RTRP-LDC-UPD NOT = ALL '+'                      
174500                                                                          
174600              MOVE JA TO INDATA-SW                                        
174700           END-IF                                                         
174800        END-IF                                                            
174900     END-IF                                                               
175000     .                                                                    
175100                                                                          
175200                                                                          
175300 GB-KOLLA-CMD  SECTION.                                                   
175400     MOVE 'GB-KOLLA-CMD    ' TO CURRENT-SECTION                           
175500                                                                          
175600*    VID UPDATE FÅR BARA ETT CMD-FÄLT VARA IFYLLT                         
175700*    D PÅ NÅGON AV TABELLRADERNA                                          
175800*    ELLER                                                                
175900*    C PÅ DEFAULTRADEN                                                    
176000*    ELLER                                                                
176100*    C ELLER N PÅ UPPDATERINGSRADEN                                       
176200                                                                          
176300     MOVE NEJ TO RAD-SW                                                   
176400                                                                          
176500*    KOLLA ATT BARA EN RAD ÄR IFYLLD OCH ATT KODEN ÄR C ELLER D           
176600     MOVE 1   TO INDX                                                     
176700     PERFORM UNTIL INDX > MAX-INDX                                        
176800        IF MID-KDCMD (INDX) NOT = ALL '+' AND SPACE                       
176900           IF RAD-VALD                                                    
177000              MOVE ERR-SPECIFY-ONE-CHOICE                                 
177100                             TO MED-IDMFSFEL                              
177200              MOVE NEJ       TO INDATA-SW                                 
177300              MOVE MFS-ALFA-FAELT-FEL                                     
177400                             TO MOD-KDCMD-ATTR (INDX)                     
177500           ELSE                                                           
177600              IF MID-KDCMD (INDX) = 'D'                                   
177700                 MOVE JA               TO RAD-SW                          
177800                 MOVE MFS-ALFA-FAELT-RAETT                                
177900                             TO MOD-KDCMD-ATTR (INDX)                     
178000                 MOVE MID-KDCMD (INDX) TO CMD-VALUES                      
178100                 MOVE INDX             TO CURR-INDX                       
178200              ELSE                                                        
178201                 IF MID-KDCMD (INDX) = 'C'                                
178202                    IF MID-KDCMD-UPD = 'C'                                
178203                       MOVE INDX       TO CURR-INDX                       
178204                    ELSE                                                  
178205                       MOVE NEJ                                           
178206                             TO INDATA-SW                                 
178208                       MOVE ERR-WRONG-CODE                                
178209                             TO MED-IDMFSFEL                              
178210                       MOVE MFS-ALFA-FAELT-FEL                            
178211                             TO MOD-KDCMD-ATTR (INDX)                     
178212                    END-IF                                                
178220                 ELSE                                                     
178300                    MOVE NEJ TO INDATA-SW                                 
178400                    MOVE ERR-WRONG-CODE                                   
178500                             TO MED-IDMFSFEL                              
178600                    MOVE MFS-ALFA-FAELT-FEL                               
178700                             TO MOD-KDCMD-ATTR (INDX)                     
178710                 END-IF                                                   
178800              END-IF                                                      
178900           END-IF                                                         
179000        END-IF                                                            
179100        ADD 1 TO INDX                                                     
179200     END-PERFORM                                                          
179300                                                                          
179400     IF MID-KDCMD-DEF NOT = ALL '+' AND SPACE                             
179500        IF RAD-VALD                                                       
179510           IF MID-KDCMD-DEF = 'C'                                         
179520              CONTINUE                                                    
179530           ELSE                                                           
179600              MOVE NEJ           TO INDATA-SW                             
179700              MOVE ERR-SPECIFY-ONE-CHOICE                                 
179800                             TO MED-IDMFSFEL                              
179900              MOVE MFS-ALFA-FAELT-FEL                                     
180000                             TO MOD-KDCMD-DEF-ATTR                        
180010           END-IF                                                         
180100        ELSE                                                              
180200           MOVE JA               TO RAD-SW                                
180300           IF MID-KDCMD-UPD = 'C'                                         
180400              MOVE JA            TO RAD-SW                                
180500              MOVE MID-KDCMD-DEF TO CMD-VALUES                            
180600              MOVE MFS-ALFA-FAELT-RAETT                                   
180700                             TO MOD-KDCMD-DEF-ATTR                        
180800           ELSE                                                           
180810                                                                          
180900              MOVE NEJ TO INDATA-SW                                       
181000              MOVE ERR-WRONG-CODE                                         
181100                             TO MED-IDMFSFEL                              
181200              MOVE MFS-ALFA-FAELT-FEL                                     
181300                             TO MOD-KDCMD-DEF-ATTR                        
181400           END-IF                                                         
181500        END-IF                                                            
182600     END-IF                                                               
182700                                                                          
182800     IF MID-KDCMD-UPD NOT = ALL '+' AND SPACE                             
183600        IF (MID-KDCMD-UPD = 'C' AND CURR-INDX > 0) OR                     
183610           MID-KDCMD-UPD = 'N'                                            
183700           MOVE JA                   TO RAD-SW                            
183800           MOVE MID-KDCMD-UPD TO CMD-VALUES                               
184000           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-UPD-ATTR                
184100        ELSE                                                              
184110           IF (MID-KDCMD-UPD = 'C' AND CURR-INDX = 0)                     
184111             MOVE NEJ TO INDATA-SW                                        
184112             MOVE ERR-NO-LINE-SELECTED TO MED-IDMFSFEL                    
184113             MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-UPD-ATTR              
184120           ELSE                                                           
184200             MOVE NEJ TO INDATA-SW                                        
184400             MOVE ERR-WRONG-CODE     TO MED-IDMFSFEL                      
184600             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-UPD-ATTR                
184610           END-IF                                                         
184700        END-IF                                                            
184900     ELSE                                                                 
185000        IF RAD-EJ-VALD                                                    
185100           MOVE NEJ TO INDATA-SW                                          
185300           MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                      
185400           MOVE MFS-ADD-READ-HILIGHT-FIELD                                
185500                                     TO MOD-KDCMD-UPD                     
185600        ELSE                                                              
185700           MOVE NEJ TO RAD-SW                                             
185800        END-IF                                                            
185900     END-IF                                                               
186000     .                                                                    
186100                                                                          
186200                                                                          
186300 GC-KOLLA-UPDATE SECTION.                                                 
186400     MOVE 'GC-KOLLA-UPDATE ' TO CURRENT-SECTION                           
186500                                                                          
186600     IF MID-KDCMD-DEF = 'C'                                               
186800        PERFORM S10-KOLLA-DAGAR                                           
186900     ELSE                                                                 
187000        PERFORM S10-KOLLA-DAGAR                                           
187100                                                                          
187400        MOVE MID-IDDISTR-FOM-UPD  TO W-IDDISTR-FOM                        
187500                                                                          
187800        MOVE MID-IDDISTR-TOM-UPD  TO W-IDDISTR-TOM                        
187900                                                                          
188200        MOVE MID-IDKUNDNR-FOM-UPD  TO W-IDKUNDNR-FOM                      
188300                                                                          
188600        MOVE MID-IDKUNDNR-TOM-UPD TO W-IDKUNDNR-TOM                       
188700                                                                          
188800        PERFORM IMS-GU-WDB201                                             
188900        IF SEGMENT-SAKNAS                                                 
189000           MOVE NEJ TO GMT-FLLDCKND                                       
189100        END-IF                                                            
189200        PERFORM S11-KOLLA-LDC                                             
189300     END-IF                                                               
189400     .                                                                    
189500                                                                          
189600                                                                          
189700 GD-KOLLA-NEW SECTION.                                                    
189800     MOVE 'GD-KOLLA-NEW    ' TO CURRENT-SECTION                           
189900                                                                          
190000     PERFORM GDA-KOLLA-DISTRIKT                                           
190100     PERFORM GDB-KOLLA-KUND                                               
190200     PERFORM GDC-KOLLA-ORSAK                                              
190300                                                                          
190400     IF INDATA-OK                                                         
190500        PERFORM GDD-KOLLA-INTERVALL                                       
190600     END-IF                                                               
190700                                                                          
190800     IF INDATA-OK                                                         
190900       PERFORM S10-KOLLA-DAGAR                                            
191000       PERFORM S11-KOLLA-LDC                                              
191100     END-IF                                                               
191200     .                                                                    
191300                                                                          
191400                                                                          
191500 GDA-KOLLA-DISTRIKT SECTION.                                              
191600     MOVE 'GDA-KOLLA-DISTR ' TO CURRENT-SECTION                           
191700                                                                          
191900     IF MID-IDDISTR-FOM-UPD = ALL '+'                                     
192000     OR MID-IDDISTR-FOM-UPD NOT NUMERIC                                   
192100        MOVE NEJ                    TO INDATA-SW                          
192200        MOVE ERR-WRONG-DISTRICT     TO MED-IDMFSFEL                       
192300        MOVE MFS-NUM-FAELT-FEL      TO MOD-IDDISTR-FOM-UPD-ATTR           
192400     ELSE                                                                 
192500        MOVE MID-IDDISTR-FOM-UPD    TO W-IDDISTR-FOM                      
192600        MOVE MFS-NUM-FAELT-RAETT    TO MOD-IDDISTR-FOM-UPD-ATTR           
192700        MOVE ZERO                   TO W-IDKUNDNR-FOM                     
192800     END-IF                                                               
192900                                                                          
193000     IF INDATA-OK                                                         
193100       IF MID-IDDISTR-TOM-UPD = ALL '+'                                   
193200          MOVE MID-IDDISTR-FOM-UPD  TO W-IDDISTR-TOM                      
193300          MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDDISTR-TOM-UPD-ATTR           
193400          MOVE 999999               TO W-IDKUNDNR-TOM                     
193500       ELSE                                                               
193600          IF MID-IDDISTR-TOM-UPD NOT NUMERIC                              
193700             MOVE NEJ               TO INDATA-SW                          
193800             MOVE ERR-NOT-NUMERIC   TO MED-IDMFSFEL                       
193900             MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-TOM-UPD-ATTR           
194000          ELSE                                                            
194100             MOVE MID-IDDISTR-TOM-UPD TO W-IDDISTR-TOM                    
194200             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-TOM-UPD-ATTR         
194300             MOVE 999999            TO W-IDKUNDNR-TOM                     
194400          END-IF                                                          
194500       END-IF                                                             
194600                                                                          
194700       IF W-IDDISTR-FOM > W-IDDISTR-TOM                                   
194800          MOVE NEJ                  TO INDATA-SW                          
194900          MOVE ERR-INTERVAL         TO MED-IDMFSFEL                       
195000          MOVE MFS-NUM-FAELT-FEL    TO MOD-IDDISTR-FOM-UPD-ATTR           
195100          MOVE MFS-NUM-FAELT-FEL    TO MOD-IDDISTR-TOM-UPD-ATTR           
195200       ELSE                                                               
195300          PERFORM IMS-GU-WDB201                                           
195400          IF SEGMENT-SAKNAS                                               
195500             MOVE NEJ               TO INDATA-SW                          
195600             MOVE ERR-DISTR-CUST-MISSING                                  
195700                                      TO MED-IDMFSFEL                     
195800             MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-FOM-UPD-ATTR           
195900             MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-TOM-UPD-ATTR           
196000          ELSE                                                            
196100             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-FOM-UPD-ATTR         
196200             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-TOM-UPD-ATTR         
196300          END-IF                                                          
196400       END-IF                                                             
196410     END-IF                                                               
196500     .                                                                    
196600                                                                          
196700                                                                          
196800 GDB-KOLLA-KUND     SECTION.                                              
196900     MOVE 'GDB-KOLLA-KUND  ' TO CURRENT-SECTION                           
197000                                                                          
197100     IF INDATA-OK                                                         
197200                                                                          
197300        IF MID-IDKUNDNR-FOM-UPD NOT = ALL '+'                             
197400           IF MID-IDKUNDNR-FOM-UPD NOT NUMERIC                            
197500              MOVE NEJ              TO INDATA-SW                          
197600              MOVE ERR-NOT-NUMERIC  TO MED-IDMFSFEL                       
197700              MOVE MFS-NUM-FAELT-FEL                                      
197800                                    TO MOD-IDKUNDNR-FOM-UPD-ATTR          
197900           ELSE                                                           
198000              MOVE MID-IDKUNDNR-FOM-UPD                                   
198100                                    TO W-IDKUNDNR-FOM                     
198200              MOVE MFS-NUM-FAELT-RAETT                                    
198300                                    TO MOD-IDKUNDNR-FOM-UPD-ATTR          
198400              IF MID-IDKUNDNR-TOM-UPD NOT = ALL '+'                       
198500                 IF MID-IDKUNDNR-TOM-UPD NOT NUMERIC                      
198600                    MOVE NEJ        TO INDATA-SW                          
198700                    MOVE MFS-NUM-FAELT-FEL                                
198800                                    TO MOD-IDKUNDNR-FOM-UPD-ATTR          
198900                    MOVE MFS-NUM-FAELT-FEL                                
199000                                    TO MOD-IDKUNDNR-TOM-UPD-ATTR          
199100                 ELSE                                                     
199200                    MOVE MID-IDKUNDNR-TOM-UPD                             
199300                                    TO W-IDKUNDNR-TOM                     
199400                    MOVE MFS-NUM-FAELT-RAETT                              
199500                                    TO MOD-IDKUNDNR-FOM-UPD-ATTR          
199600                    MOVE MFS-NUM-FAELT-RAETT                              
199700                                    TO MOD-IDKUNDNR-TOM-UPD-ATTR          
199800                 END-IF                                                   
199810              ELSE                                                        
199820                 MOVE MID-IDKUNDNR-FOM-UPD                                
199830                                    TO W-IDKUNDNR-TOM                     
199900              END-IF                                                      
200000           END-IF                                                         
200100        ELSE                                                              
200200           MOVE ZERO                TO MID-IDKUNDNR-FOM-UPD               
200300           IF MID-IDKUNDNR-TOM-UPD NOT = ALL '+'                          
200400              MOVE NEJ              TO INDATA-SW                          
200500              MOVE ERR-INTERVAL     TO MED-IDMFSFEL                       
200600              MOVE MFS-NUM-FAELT-FEL                                      
200700                                    TO MOD-IDKUNDNR-FOM-UPD-ATTR          
200800              MOVE MFS-NUM-FAELT-FEL                                      
200900                                    TO MOD-IDKUNDNR-TOM-UPD-ATTR          
201000           ELSE                                                           
201100              MOVE 999999           TO MID-IDKUNDNR-TOM-UPD               
201200             MOVE MFS-NUM-FAELT-RAETT                                     
201300                                    TO MOD-IDKUNDNR-FOM-UPD-ATTR          
201400             MOVE MFS-NUM-FAELT-RAETT                                     
201500                                    TO MOD-IDKUNDNR-TOM-UPD-ATTR          
201600           END-IF                                                         
201700        END-IF                                                            
201800                                                                          
201900        IF W-IDKUNDNR-FOM > W-IDKUNDNR-TOM                                
202000           MOVE NEJ                 TO INDATA-SW                          
202100           MOVE ERR-INTERVAL        TO MED-IDMFSFEL                       
202200           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKUNDNR-FOM-UPD-ATTR          
202300           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKUNDNR-TOM-UPD-ATTR          
202400        ELSE                                                              
202500           IF W-IDKUNDNR-FOM NOT = ZERO AND                               
202600              W-IDDISTR-FOM NOT = W-IDDISTR-TOM                           
202700                                                                          
202800              MOVE NEJ              TO INDATA-SW                          
202900              MOVE ERR-INTERVAL     TO MED-IDMFSFEL                       
203000              MOVE MFS-NUM-FAELT-FEL                                      
203100                                    TO MOD-IDDISTR-FOM-UPD-ATTR           
203300                                       MOD-IDDISTR-TOM-UPD-ATTR           
203500                                       MOD-IDKUNDNR-FOM-UPD-ATTR          
203700                                       MOD-IDKUNDNR-TOM-UPD-ATTR          
203800           ELSE                                                           
203900              IF ALLT-OK                                                  
204000                 PERFORM IMS-GU-WDB201                                    
204100                                                                          
204200                 IF SEGMENT-SAKNAS                                        
204300                    MOVE NEJ        TO INDATA-SW                          
204400                    MOVE ERR-DISTR-CUST-MISSING                           
204500                                    TO MED-IDMFSFEL                       
204600                    MOVE MFS-NUM-FAELT-FEL                                
204700                                    TO MOD-IDDISTR-FOM-UPD-ATTR           
204900                                       MOD-IDDISTR-TOM-UPD-ATTR           
205100                                       MOD-IDKUNDNR-FOM-UPD-ATTR          
205300                                       MOD-IDKUNDNR-TOM-UPD-ATTR          
205400                 ELSE                                                     
205500                    MOVE MFS-NUM-FAELT-RAETT                              
205600                                    TO MOD-IDDISTR-FOM-UPD-ATTR           
205800                                       MOD-IDDISTR-TOM-UPD-ATTR           
206000                                       MOD-IDKUNDNR-FOM-UPD-ATTR          
206200                                       MOD-IDKUNDNR-TOM-UPD-ATTR          
206300                 END-IF                                                   
206400              ELSE                                                        
206500                 MOVE MFS-NUM-FAELT-FEL                                   
206600                                    TO MOD-IDDISTR-FOM-UPD-ATTR           
206800                                       MOD-IDDISTR-TOM-UPD-ATTR           
207000                                       MOD-IDKUNDNR-FOM-UPD-ATTR          
207200                                       MOD-IDKUNDNR-TOM-UPD-ATTR          
207300              END-IF                                                      
207400           END-IF                                                         
207500        END-IF                                                            
207600     END-IF                                                               
207700     .                                                                    
207800                                                                          
207900                                                                          
208000 GDC-KOLLA-ORSAK    SECTION.                                              
208100     MOVE 'GDC-KOLLA-ORSAK ' TO CURRENT-SECTION                           
208200                                                                          
208300     IF INDATA-OK                                                         
208400        IF MID-KDANMORS-UPD NOT = ALL '+' AND SPACE                       
208500           MOVE MID-KDANMORS-UPD TO W-KDANMORS                            
208600           IF W-KDANMORS NOT NUMERIC                                      
208700              IF W-KDANMORS-POS2 NOT = 'X'                                
208800                 MOVE NEJ               TO INDATA-SW                      
208900                 MOVE ERR-WRONG-CODE    TO MED-IDMFSFEL                   
209000                 MOVE MFS-NUM-FAELT-FEL TO MOD-KDANMORS-UPD-ATTR          
209100              ELSE                                                        
209200                 MOVE MFS-NUM-FAELT-RAETT                                 
209300                                        TO MOD-KDANMORS-UPD-ATTR          
209400              END-IF                                                      
209500           ELSE                                                           
209600              MOVE MFS-NUM-FAELT-RAETT  TO MOD-KDANMORS-UPD-ATTR          
209700           END-IF                                                         
209800        ELSE                                                              
209900           MOVE SPACE                   TO MID-KDANMORS-UPD               
210000           MOVE MFS-NUM-FAELT-RAETT     TO MOD-KDANMORS-UPD-ATTR          
210100        END-IF                                                            
210200     END-IF                                                               
210300     .                                                                    
210400                                                                          
210500                                                                          
210600 GDD-KOLLA-INTERVALL SECTION.                                             
210700     MOVE 'GDD-KOLLA-INTERV' TO CURRENT-SECTION                           
210800                                                                          
210900     MOVE W-IDDISTR-FOM           TO W-4128-IDDISTR-FKY                   
211000     MOVE W-IDDISTR-TOM           TO W-4128-IDDISTR-TKY                   
211100     MOVE W-IDKUNDNR-FOM          TO W-4128-IDKUNDNR-FKY                  
211500     MOVE W-IDKUNDNR-TOM          TO W-4128-IDKUNDNR-TKY                  
211600     MOVE MID-KDANMORS-UPD        TO W-4128-KDANMORS-KY                   
211700     PERFORM IMS-GU-WDGX4128                                              
211800                                                                          
211900     IF  SEGMENT-FINNS                                                    
212000        MOVE NEJ                  TO INDATA-SW                            
212100        MOVE ERR-DUPLICATE-RECORD TO MED-IDMFSFEL                         
212200        MOVE MFS-NUM-FAELT-FEL    TO MOD-IDDISTR-FOM-UPD-ATTR             
212300                                     MOD-IDDISTR-TOM-UPD-ATTR             
212400                                     MOD-IDKUNDNR-FOM-UPD-ATTR            
212500                                     MOD-IDKUNDNR-TOM-UPD-ATTR            
212600                                     MOD-KDANMORS-UPD-ATTR                
212700     ELSE                                                                 
212800        PERFORM GDDA-KOLLA-DISTRIKT-INTERVALL                             
212900        IF INDATA-OK                                                      
213000           PERFORM GDDB-KOLLA-KUND-INTERVALL                              
213100        END-IF                                                            
213200     END-IF                                                               
213300                                                                          
213400     IF INDATA-OK                                                         
213500        MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDDISTR-FOM-UPD-ATTR             
213600                                     MOD-IDDISTR-TOM-UPD-ATTR             
213700                                     MOD-IDKUNDNR-FOM-UPD-ATTR            
213800                                     MOD-IDKUNDNR-TOM-UPD-ATTR            
213900     END-IF                                                               
214000     .                                                                    
214100                                                                          
214200                                                                          
214300 GDDA-KOLLA-DISTRIKT-INTERVALL SECTION.                                   
214400     MOVE 'GDDA-DIST-INT   ' TO CURRENT-SECTION                           
214500                                                                          
214510     PERFORM IMS-GU-WDR501                                                
214520     MOVE W-IDDISTR-FOM              TO W-4128-IDDISTR-FOM                
214530     MOVE W-IDDISTR-TOM              TO W-4128-IDDISTR-TOM                
214540     PERFORM IMS-GU-WDGX4128-DI                                           
214550                                                                          
214600     IF W-IDKUNDNR-FOM = ZERO                                             
216100        IF SEGMENT-SAKNAS                                                 
216110           IF MID-KDANMORS-UPD NOT = SPACE                                
216120              MOVE NEJ               TO INDATA-SW                         
216130                                                                          
216150              MOVE ERR-INTERVAL      TO MED-IDMFSFEL                      
216160              MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-FOM-UPD-ATTR          
216170                                        MOD-IDDISTR-TOM-UPD-ATTR          
216180           ELSE                                                           
216200              PERFORM IMS-GU-WDR501                                       
216300              PERFORM IMS-GNP-WDGX4128-DI-CH                              
216400              IF SEGMENT-FINNS                                            
216500                 MOVE NEJ            TO INDATA-SW                         
216600                 MOVE ERR-INTERVAL   TO MED-IDMFSFEL                      
216800                MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-FOM-UPD-ATTR        
217000                                          MOD-IDDISTR-TOM-UPD-ATTR        
217100              END-IF                                                      
217110           END-IF                                                         
217200        END-IF                                                            
217300     ELSE                                                                 
217310        IF SEGMENT-SAKNAS                                                 
217350           MOVE NEJ                  TO INDATA-SW                         
217351                                                                          
217360           MOVE ERR-INTERVAL         TO MED-IDMFSFEL                      
217370           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDDISTR-FOM-UPD-ATTR          
217380                                        MOD-IDDISTR-TOM-UPD-ATTR          
217391        END-IF                                                            
217400     END-IF                                                               
217500     .                                                                    
217600                                                                          
217700                                                                          
217800 GDDB-KOLLA-KUND-INTERVALL SECTION.                                       
217900     MOVE 'GDDB-KUND-INT   ' TO CURRENT-SECTION                           
218000                                                                          
218800     IF W-IDKUNDNR-FOM > ZERO                                             
218900        MOVE W-IDDISTR-FOM        TO W-4128-IDDISTR-FOM                   
219000                                     W-4128-IDDISTR-TOM                   
219100        MOVE W-IDKUNDNR-FOM       TO W-4128-IDKUNDNR-FOM                  
219200        MOVE W-IDKUNDNR-TOM       TO W-4128-IDKUNDNR-TOM                  
219300                                                                          
219400        PERFORM IMS-GU-WDR501                                             
219500        PERFORM IMS-GU-WDGX4128-KU                                        
219600        IF SEGMENT-SAKNAS                                                 
219700           PERFORM IMS-GU-WDR501                                          
219800           PERFORM IMS-GNP-WDGX4128-KU-CH                                 
219810                                                                          
219900           IF  SEGMENT-FINNS                                              
220000              MOVE NEJ               TO INDATA-SW                         
220100              MOVE ERR-INTERVAL      TO MED-IDMFSFEL                      
220101                                                                          
220200              MOVE MFS-NUM-FAELT-FEL TO MOD-IDKUNDNR-FOM-UPD-ATTR         
220300                                        MOD-IDKUNDNR-TOM-UPD-ATTR         
220400           END-IF                                                         
220500        END-IF                                                            
220600     END-IF                                                               
220700     .                                                                    
220800                                                                          
220900                                                                          
220910 GE-KOLLA-DELETE  SECTION.                                                
220920     MOVE 'GE-KOLLA-DELETE ' TO CURRENT-SECTION                           
220930                                                                          
220990     IF MID-IDKUNDNR-FOM (CURR-INDX) = ZERO AND                           
220991        MID-KDANMORS     (CURR-INDX) = SPACE                              
220992        PERFORM IMS-GU-WDR501                                             
220995        MOVE MID-IDDISTR-FOM (CURR-INDX)  TO W-4128-IDDISTR-FOM           
221001        MOVE MID-IDDISTR-TOM (CURR-INDX) TO W-4128-IDDISTR-TOM            
221003        PERFORM IMS-GNP-WDGX4128-DI-CH                                    
221010                                                                          
221011        IF SEGMENT-FINNS                                                  
221012           IF  4128-IDKUNDNR-FOM = MID-IDKUNDNR-FOM (CURR-INDX)           
221013           AND 4128-IDKUNDNR-TOM = MID-IDKUNDNR-TOM (CURR-INDX)           
221014* VI HAR FÅTT TRÄFF PÅ SEGMENTET VI VILL TA BORT                          
221015              PERFORM IMS-GNP-WDGX4128-DI-CH                              
221021           END-IF                                                         
221022           IF SEGMENT-FINNS                                               
221023              IF  4128-IDKUNDNR-FOM = ZERO                                
221024              AND 4128-IDKUNDNR-TOM = 999999                              
221025              AND 4128-KDANMORS = SPACE                                   
221026* VI HAR FÅTT TRÄFF PÅ DISTRIKTS-DEFAULT, ALLT OK                         
221027                 CONTINUE                                                 
221028              ELSE                                                        
221031                 MOVE NEJ            TO INDATA-SW                         
221032                 MOVE ERR-WRONG-CODE TO MED-IDMFSFEL                      
221033                 MOVE MFS-ALFA-FAELT-FEL                                  
221034                                     TO MOD-KDCMD-ATTR (CURR-INDX)        
221036              END-IF                                                      
221037           END-IF                                                         
221038        END-IF                                                            
221039     END-IF                                                               
221040     .                                                                    
221041                                                                          
221042                                                                          
221050 H-UPPDATERA   SECTION.                                                   
221100     MOVE 'H-UPPDATERA     ' TO CURRENT-SECTION                           
221200                                                                          
221300     IF CMD-DELETE                                                        
221400        PERFORM HA-DELETE-LEDTID                                          
221500     ELSE                                                                 
221600        IF CMD-UPDATE                                                     
221700           PERFORM HB-UPDATE-LEDTID                                       
221800        ELSE                                                              
221900           PERFORM HC-NEW-LEDTID                                          
222000        END-IF                                                            
222100     END-IF                                                               
222200                                                                          
222300     PERFORM HD-RENSA-UPD-RAD                                             
222400                                                                          
223100     MOVE INF-UPDATE-DONE  TO MED-IDMFSINF                                
223200     CALL WMEDKONV USING MED-WMEDAREA                                     
223300     MOVE MED-MFSINF       TO MOD-TEMFSINF                                
223400     .                                                                    
223500                                                                          
223600                                                                          
223700 HA-DELETE-LEDTID   SECTION.                                              
223800     MOVE 'HA-DELETE-LEDTID' TO CURRENT-SECTION                           
223900                                                                          
224000     MOVE MID-IDDISTR-FOM  (CURR-INDX) TO W-4128-IDDISTR-FKY              
224100     MOVE MID-IDDISTR-TOM  (CURR-INDX) TO W-4128-IDDISTR-TKY              
224200     MOVE MID-IDKUNDNR-FOM (CURR-INDX) TO W-4128-IDKUNDNR-FKY             
224300     MOVE MID-IDKUNDNR-TOM (CURR-INDX) TO W-4128-IDKUNDNR-TKY             
224400     MOVE MID-KDANMORS     (CURR-INDX) TO W-4128-KDANMORS-KY              
224410                                                                          
224411     MOVE W-4128-IDDISTR-FKY         TO SPAR-IDDISTR-FKY-ENTER            
224420     MOVE W-4128-IDDISTR-TKY         TO SPAR-IDDISTR-TKY-ENTER            
224430     MOVE W-4128-IDKUNDNR-FKY        TO SPAR-IDKUNDNR-FKY-ENTER           
224440     MOVE W-4128-IDKUNDNR-TKY        TO SPAR-IDKUNDNR-TKY-ENTER           
224450     MOVE W-4128-KDANMORS-KY         TO SPAR-KDANMORS-KY-ENTER            
224500                                                                          
224600     PERFORM IMS-GHU-WDGX4128                                             
224610     IF SEGMENT-FINNS                                                     
224700       PERFORM IMS-DLET-WDGX4128                                          
224710     END-IF                                                               
224800     .                                                                    
224900                                                                          
225000                                                                          
225100 HB-UPDATE-LEDTID   SECTION.                                              
225200     MOVE 'HB-UPDATE-LEDTID' TO CURRENT-SECTION                           
225300                                                                          
225400     MOVE MID-IDDISTR-FOM-UPD   TO W-4128-IDDISTR-FKY                     
225500     MOVE MID-IDDISTR-TOM-UPD   TO W-4128-IDDISTR-TKY                     
225600     IF  W-4128-IDDISTR-FKY = 1                                           
225700     AND W-4128-IDDISTR-TKY = 9999                                        
225800*  DEFAULTRAD                                                             
226100        MOVE ZERO               TO W-4128-IDKUNDNR-FKY                    
226110        MOVE 999999             TO W-4128-IDKUNDNR-TKY                    
226200        MOVE SPACE              TO W-4128-KDANMORS-KY                     
226210        MOVE MFS-ADD-LYS-UPP-FAELT                                        
226220                                TO MOD-IDDISTR-FOM-DEF-ATTR               
226230                                   MOD-IDDISTR-TOM-DEF-ATTR               
226300     ELSE                                                                 
226400     MOVE MID-IDKUNDNR-FOM-UPD  TO W-4128-IDKUNDNR-FKY                    
226540     MOVE MID-IDKUNDNR-TOM (CURR-INDX) TO W-4128-IDKUNDNR-TKY             
226600     MOVE MID-KDANMORS-UPD      TO W-4128-KDANMORS-KY                     
226610     MOVE MFS-ADD-LYS-UPP-FAELT TO                                        
226620                             MOD-IDDISTR-FOM-ATTR  (1)                    
226630                             MOD-IDDISTR-TOM-ATTR  (1)                    
226640                             MOD-IDKUNDNR-FOM-ATTR (1)                    
226650                             MOD-IDKUNDNR-TOM-ATTR (1)                    
226660                             MOD-KDANMORS-ATTR     (1)                    
226700     END-IF                                                               
226710                                                                          
226720     MOVE W-4128-IDDISTR-FKY         TO SPAR-IDDISTR-FKY-ENTER            
226730     MOVE W-4128-IDDISTR-TKY         TO SPAR-IDDISTR-TKY-ENTER            
226740     MOVE W-4128-IDKUNDNR-FKY        TO SPAR-IDKUNDNR-FKY-ENTER           
226750     MOVE W-4128-IDKUNDNR-TKY        TO SPAR-IDKUNDNR-TKY-ENTER           
226760     MOVE W-4128-KDANMORS-KY         TO SPAR-KDANMORS-KY-ENTER            
226800                                                                          
226900     PERFORM IMS-GHU-WDGX4128                                             
226910     IF MID-KVDAGAR-LTRP-UPD NOT = 4128-KVDAGAR-LTRP                      
227000        MOVE MID-KVDAGAR-LTRP-UPD    TO 4128-KVDAGAR-LTRP                 
227001        IF MID-KDCMD-DEF = 'C'                                            
227010           MOVE MFS-ADD-LYS-UPP-FAELT TO                                  
227020                MOD-KVDAGAR-LTRP-DEF-ATTR                                 
227021        ELSE                                                              
227022           MOVE MFS-ADD-LYS-UPP-FAELT TO                                  
227023                MOD-KVDAGAR-LTRP-ATTR (1)                                 
227024        END-IF                                                            
227030     END-IF                                                               
227031                                                                          
227040     IF MID-KVDAGAR-LEVANM-UPD NOT = 4128-KVDAGAR-LEVANM                  
227100        MOVE MID-KVDAGAR-LEVANM-UPD  TO 4128-KVDAGAR-LEVANM               
227101        IF MID-KDCMD-DEF = 'C'                                            
227110           MOVE MFS-ADD-LYS-UPP-FAELT TO                                  
227120                MOD-KVDAGAR-LEVANM-DEF-ATTR                               
227121        ELSE                                                              
227122           MOVE MFS-ADD-LYS-UPP-FAELT TO                                  
227123                MOD-KVDAGAR-LEVANM-ATTR (1)                               
227124        END-IF                                                            
227130     END-IF                                                               
227140                                                                          
227150     IF  MID-KVDAGAR-LTRP-LDC-UPD NOT = ALL '+'                           
227160     AND MID-KVDAGAR-LTRP-LDC-UPD NOT = 4128-KVDAGAR-LTRP-LDC             
227200        MOVE MID-KVDAGAR-LTRP-LDC-UPD TO 4128-KVDAGAR-LTRP-LDC            
227210        MOVE MFS-ADD-LYS-UPP-FAELT   TO                                   
227220             MOD-KVDAGAR-LTRP-LDC-ATTR (1)                                
227230     END-IF                                                               
227240                                                                          
227250     IF  MID-KVDAGAR-LEVANM-LDC-UPD NOT = ALL '+'                         
227260     AND MID-KVDAGAR-LEVANM-LDC-UPD NOT = 4128-KVDAGAR-LEVANM-LDC         
227300        MOVE MID-KVDAGAR-LEVANM-LDC-UPD TO 4128-KVDAGAR-LEVANM-LDC        
227310        MOVE MFS-ADD-LYS-UPP-FAELT   TO                                   
227320             MOD-KVDAGAR-LEVANM-LDC-ATTR (1)                              
227330     END-IF                                                               
227340                                                                          
227350     IF MID-KVDAGAR-RET-UPD NOT = 4128-KVDAGAR-RET                        
227400        MOVE MID-KVDAGAR-RET-UPD     TO 4128-KVDAGAR-RET                  
227401        IF MID-KDCMD-DEF = 'C'                                            
227410           MOVE MFS-ADD-LYS-UPP-FAELT TO                                  
227420                MOD-KVDAGAR-RET-DEF-ATTR                                  
227421        ELSE                                                              
227422           MOVE MFS-ADD-LYS-UPP-FAELT TO                                  
227423                MOD-KVDAGAR-RET-ATTR (1)                                  
227424        END-IF                                                            
227430     END-IF                                                               
227440                                                                          
227450     IF MID-KVDAGAR-RTRP-UPD NOT = 4128-KVDAGAR-RTRP                      
227500        MOVE MID-KVDAGAR-RTRP-UPD    TO 4128-KVDAGAR-RTRP                 
227501        IF MID-KDCMD-DEF = 'C'                                            
227510           MOVE MFS-ADD-LYS-UPP-FAELT TO                                  
227520                MOD-KVDAGAR-RTRP-DEF-ATTR                                 
227521        ELSE                                                              
227522           MOVE MFS-ADD-LYS-UPP-FAELT TO                                  
227523                MOD-KVDAGAR-RTRP-ATTR (1)                                 
227524        END-IF                                                            
227530     END-IF                                                               
227540                                                                          
227550     IF  MID-KVDAGAR-RET-LDC-UPD NOT = ALL '+'                            
227560     AND MID-KVDAGAR-RET-LDC-UPD NOT = 4128-KVDAGAR-RET-LDC               
227600        MOVE MID-KVDAGAR-RET-LDC-UPD TO 4128-KVDAGAR-RET-LDC              
227610        MOVE MFS-ADD-LYS-UPP-FAELT   TO                                   
227620             MOD-KVDAGAR-RET-LDC-ATTR (1)                                 
227630     END-IF                                                               
227640                                                                          
227650     IF  MID-KVDAGAR-RTRP-LDC-UPD NOT = ALL '+'                           
227660     AND MID-KVDAGAR-RTRP-LDC-UPD NOT = 4128-KVDAGAR-RTRP-LDC             
227700        MOVE MID-KVDAGAR-RTRP-LDC-UPD TO 4128-KVDAGAR-RTRP-LDC            
227710        MOVE MFS-ADD-LYS-UPP-FAELT   TO                                   
227720             MOD-KVDAGAR-RTRP-LDC-ATTR (1)                                
227730     END-IF                                                               
227740                                                                          
227800     PERFORM IMS-REPL-WDGX4128                                            
227900     .                                                                    
228000                                                                          
228100                                                                          
228200 HC-NEW-LEDTID   SECTION.                                                 
228300     MOVE 'HC-NEW-LEDTID   ' TO CURRENT-SECTION                           
228400                                                                          
228500     MOVE W-IDDISTR-FOM              TO 4128-IDDISTR-FOM                  
228600     MOVE W-IDDISTR-TOM              TO 4128-IDDISTR-TOM                  
228700     MOVE W-IDKUNDNR-FOM             TO 4128-IDKUNDNR-FOM                 
228800     MOVE W-IDKUNDNR-TOM             TO 4128-IDKUNDNR-TOM                 
228900     MOVE MID-KDANMORS-UPD           TO 4128-KDANMORS                     
229000                                                                          
229100     MOVE MID-KVDAGAR-LTRP-UPD       TO 4128-KVDAGAR-LTRP                 
229200     MOVE MID-KVDAGAR-LEVANM-UPD     TO 4128-KVDAGAR-LEVANM               
229300     MOVE MID-KVDAGAR-LTRP-LDC-UPD   TO 4128-KVDAGAR-LTRP-LDC             
229400     MOVE MID-KVDAGAR-LEVANM-LDC-UPD TO 4128-KVDAGAR-LEVANM-LDC           
229500     MOVE MID-KVDAGAR-RET-UPD        TO 4128-KVDAGAR-RET                  
229600     MOVE MID-KVDAGAR-RTRP-UPD       TO 4128-KVDAGAR-RTRP                 
229700     MOVE MID-KVDAGAR-RET-LDC-UPD    TO 4128-KVDAGAR-RET-LDC              
229800     MOVE MID-KVDAGAR-RTRP-LDC-UPD   TO 4128-KVDAGAR-RTRP-LDC             
229810                                                                          
229900     PERFORM IMS-ISRT-WDGX4128                                            
229901                                                                          
229902     IF  MOD-FLVISA-UT = 'J' OR 'Y'                                       
229903     OR (MOD-IDDISTR-UT NOT < MID-IDDISTR-FOM-UPD                         
229904     AND MOD-IDDISTR-UT NOT > MID-IDDISTR-TOM-UPD)                        
229906                                                                          
229910     MOVE MFS-ADD-LYS-UPP-FAELT TO                                        
229920                             MOD-IDDISTR-FOM-ATTR  (1)                    
229930                             MOD-IDDISTR-TOM-ATTR  (1)                    
229940                             MOD-IDKUNDNR-FOM-ATTR (1)                    
229950                             MOD-IDKUNDNR-TOM-ATTR (1)                    
229960                             MOD-KDANMORS-ATTR     (1)                    
229970                             MOD-KVDAGAR-LTRP-ATTR (1)                    
229980                             MOD-KVDAGAR-LEVANM-ATTR     (1)              
229990                             MOD-KVDAGAR-LTRP-LDC-ATTR   (1)              
229991                             MOD-KVDAGAR-LEVANM-LDC-ATTR (1)              
229992                             MOD-KVDAGAR-RET-ATTR        (1)              
229993                             MOD-KVDAGAR-RTRP-ATTR       (1)              
229994                             MOD-KVDAGAR-RET-LDC-ATTR    (1)              
229995                             MOD-KVDAGAR-RTRP-LDC-ATTR   (1)              
229996     END-IF                                                               
230000     .                                                                    
230100                                                                          
230200                                                                          
230300 HD-RENSA-UPD-RAD SECTION.                                                
230400     MOVE 'HD-RENSA-UPD-RAD' TO CURRENT-SECTION                           
230500                                                                          
230600     MOVE MFS-RENSA-FAELT TO MOD-KDCMD-UPD                                
230700                             MOD-IDDISTR-FOM-UPD                          
230800                             MOD-IDDISTR-TOM-UPD                          
230900                             MOD-IDKUNDNR-FOM-UPD                         
231000                             MOD-IDKUNDNR-TOM-UPD                         
231100                             MOD-KDANMORS-UPD                             
231200                             MOD-KVDAGAR-LTRP-UPD                         
231300                             MOD-KVDAGAR-LEVANM-UPD                       
231400                             MOD-KVDAGAR-LTRP-LDC-UPD                     
231500                             MOD-KVDAGAR-LEVANM-LDC-UPD                   
231600                             MOD-KVDAGAR-RET-UPD                          
231700                             MOD-KVDAGAR-RTRP-UPD                         
231800                             MOD-KVDAGAR-RET-LDC-UPD                      
231900                             MOD-KVDAGAR-RTRP-LDC-UPD                     
232000     .                                                                    
232100                                                                          
232200                                                                          
232300 I-SPARA-DEF-RAD SECTION.                                                 
232400     MOVE 'I-SPAR-DEF-RAD  ' TO CURRENT-SECTION                           
232500                                                                          
232600     MOVE MFS-ROER-EJ-FAELT  TO MOD-IDDISTR-FOM-DEF                       
232700                                MOD-IDDISTR-TOM-DEF                       
232800                                MOD-KVDAGAR-LTRP-DEF                      
232900                                MOD-KVDAGAR-LEVANM-DEF                    
233000                                MOD-KVDAGAR-RET-DEF                       
233100                                MOD-KVDAGAR-RTRP-DEF                      
233200     .                                                                    
233300                                                                          
233400                                                                          
233500 S10-KOLLA-DAGAR    SECTION.                                              
233600     MOVE 'S10-KOLLA-DAGAR ' TO CURRENT-SECTION                           
233700                                                                          
234200     IF MID-KVDAGAR-LTRP-UPD NOT NUMERIC                                  
234300     OR MID-KVDAGAR-LTRP-UPD = ZERO                                       
234400     OR MID-KVDAGAR-LTRP-UPD = ALL '+'                                    
234500        MOVE NEJ                    TO INDATA-SW                          
234600        MOVE MFS-NUM-FAELT-FEL      TO MOD-KVDAGAR-LTRP-UPD-ATTR          
234700        MOVE ERR-CORR-HILITE-FLDS   TO MED-IDMFSFEL                       
234800     END-IF                                                               
234900                                                                          
235400     IF MID-KVDAGAR-LEVANM-UPD NOT NUMERIC                                
235500     OR MID-KVDAGAR-LEVANM-UPD = ZERO                                     
235600     OR MID-KVDAGAR-LEVANM-UPD = ALL '+'                                  
235700        MOVE NEJ                    TO INDATA-SW                          
235800        MOVE MFS-NUM-FAELT-FEL      TO                                    
235900             MOD-KVDAGAR-LEVANM-UPD-ATTR                                  
236000        MOVE ERR-CORR-HILITE-FLDS   TO MED-IDMFSFEL                       
236100     END-IF                                                               
236200                                                                          
236700     IF MID-KVDAGAR-RET-UPD NOT NUMERIC                                   
236800     OR MID-KVDAGAR-RET-UPD = ZERO                                        
236900     OR MID-KVDAGAR-RET-UPD = ALL '+'                                     
237000        MOVE NEJ                    TO INDATA-SW                          
237100        MOVE MFS-NUM-FAELT-FEL      TO MOD-KVDAGAR-RET-UPD-ATTR           
237200        MOVE ERR-CORR-HILITE-FLDS   TO MED-IDMFSFEL                       
237300     END-IF                                                               
237400                                                                          
237900     IF MID-KVDAGAR-RTRP-UPD NOT NUMERIC                                  
238000     OR MID-KVDAGAR-RTRP-UPD = ZERO                                       
238100     OR MID-KVDAGAR-RTRP-UPD = ALL '+'                                    
238200        MOVE NEJ                    TO INDATA-SW                          
238300        MOVE MFS-NUM-FAELT-FEL      TO MOD-KVDAGAR-RTRP-UPD-ATTR          
238400        MOVE ERR-CORR-HILITE-FLDS   TO MED-IDMFSFEL                       
238500     END-IF                                                               
238600                                                                          
238700     .                                                                    
238800                                                                          
238900                                                                          
239000 S11-KOLLA-LDC      SECTION.                                              
239100     MOVE 'S11-KOLLA-LDC   ' TO CURRENT-SECTION                           
239200                                                                          
239300     IF MID-KVDAGAR-LTRP-LDC-UPD NOT = ALL '+'                            
239800        IF MID-KVDAGAR-LTRP-LDC-UPD NOT NUMERIC                           
239900           MOVE NEJ                  TO INDATA-SW                         
240000           MOVE MFS-NUM-FAELT-FEL    TO                                   
240100                MOD-KVDAGAR-LTRP-LDC-UPD-ATTR                             
240200           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
240300        ELSE                                                              
240400           IF W-IDKUNDNR-FOM > ZERO                                       
240410           AND W-IDKUNDNR-FOM = W-IDKUNDNR-TOM                            
240500           AND MID-KVDAGAR-LTRP-LDC-UPD > ZERO                            
240600               IF GMT-FLLDCKND = NEJ                                      
240700                 MOVE NEJ            TO INDATA-SW                         
240800                 MOVE MFS-NUM-FAELT-FEL TO                                
240900                      MOD-KVDAGAR-LTRP-LDC-UPD-ATTR                       
241000                 MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                
241100               END-IF                                                     
241200           END-IF                                                         
241300        END-IF                                                            
241301     ELSE                                                                 
241302        MOVE ZERO TO MID-KVDAGAR-LTRP-LDC-UPD                             
241310     END-IF                                                               
241400                                                                          
241500     IF MID-KVDAGAR-LEVANM-LDC-UPD NOT = ALL '+'                          
241900        IF MID-KVDAGAR-LEVANM-LDC-UPD NOT NUMERIC                         
242000           MOVE NEJ                 TO INDATA-SW                          
242100           MOVE MFS-NUM-FAELT-FEL   TO                                    
242200                MOD-KVDAGAR-LEVNM-LDC-UPD-ATTR                            
242300           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
242400        ELSE                                                              
242500           IF W-IDKUNDNR-FOM > ZERO                                       
242510           AND W-IDKUNDNR-FOM = W-IDKUNDNR-TOM                            
242600           AND MID-KVDAGAR-LEVANM-LDC-UPD > ZERO                          
242700              IF GMT-FLLDCKND = NEJ                                       
242800                 MOVE NEJ            TO INDATA-SW                         
242900                 MOVE MFS-NUM-FAELT-FEL TO                                
243000                      MOD-KVDAGAR-LEVNM-LDC-UPD-ATTR                      
243100                 MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                
243200              END-IF                                                      
243300           END-IF                                                         
243400        END-IF                                                            
243401     ELSE                                                                 
243402        MOVE ZERO TO MID-KVDAGAR-LEVANM-LDC-UPD                           
243410     END-IF                                                               
243500                                                                          
243600     IF MID-KVDAGAR-RET-LDC-UPD NOT = ALL '+'                             
244000        IF MID-KVDAGAR-RET-LDC-UPD NOT NUMERIC                            
244100           MOVE NEJ                 TO INDATA-SW                          
244200           MOVE MFS-NUM-FAELT-FEL   TO                                    
244300           MOD-KVDAGAR-RET-LDC-UPD-ATTR                                   
244400           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
244500        ELSE                                                              
244600           IF W-IDKUNDNR-FOM > ZERO                                       
244610           AND W-IDKUNDNR-FOM = W-IDKUNDNR-TOM                            
244700           AND MID-KVDAGAR-RET-LDC-UPD > ZERO                             
244800              IF GMT-FLLDCKND = NEJ                                       
244900                 MOVE NEJ            TO INDATA-SW                         
245000                 MOVE MFS-NUM-FAELT-FEL TO                                
245100                      MOD-KVDAGAR-RET-LDC-UPD-ATTR                        
245200                 MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                
245300              END-IF                                                      
245400           END-IF                                                         
245500        END-IF                                                            
245501     ELSE                                                                 
245502        MOVE ZERO TO MID-KVDAGAR-RET-LDC-UPD                              
245510     END-IF                                                               
245600                                                                          
245700     IF MID-KVDAGAR-RTRP-LDC-UPD NOT = ALL '+'                            
246100        IF MID-KVDAGAR-RTRP-LDC-UPD NOT NUMERIC                           
246200           MOVE NEJ                 TO INDATA-SW                          
246300           MOVE MFS-NUM-FAELT-FEL   TO                                    
246400                MOD-KVDAGAR-RTRP-LDC-UPD-ATTR                             
246500           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
246600        ELSE                                                              
246700           IF W-IDKUNDNR-FOM > ZERO                                       
246710           AND W-IDKUNDNR-FOM = W-IDKUNDNR-TOM                            
246800           AND MID-KVDAGAR-RTRP-LDC-UPD > ZERO                            
246900              IF GMT-FLLDCKND = NEJ                                       
247000                 MOVE NEJ            TO INDATA-SW                         
247100                 MOVE MFS-NUM-FAELT-FEL TO                                
247200                      MOD-KVDAGAR-RTRP-LDC-UPD-ATTR                       
247300                 MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                
247400              END-IF                                                      
247500           END-IF                                                         
247600        END-IF                                                            
247601     ELSE                                                                 
247602        MOVE ZERO TO MID-KVDAGAR-RTRP-LDC-UPD                             
247610     END-IF                                                               
247700     .                                                                    
247800                                                                          
247900                                                                          
248000 MFS-RENSA-FAELT-UT SECTION.                                              
248100                                                                          
248200*    --- ALLA UTDATA-FÄLT                                                 
248300*    --- INKL. BLÄDDRINGSNYCKLAR                                          
248400     MOVE MFS-RENSA-FAELT TO MOD-KDCMD-DEF                                
248500                             MOD-IDDISTR-FOM-DEF                          
248600                             MOD-IDDISTR-TOM-DEF                          
248700                             MOD-KVDAGAR-LTRP-DEF                         
248800                             MOD-KVDAGAR-LEVANM-DEF                       
248900                             MOD-KVDAGAR-RET-DEF                          
249000                             MOD-KVDAGAR-RTRP-DEF                         
249100                             MOD-KDCMD-UPD                                
249200                             MOD-IDDISTR-FOM-UPD                          
249300                             MOD-IDDISTR-TOM-UPD                          
249400                             MOD-IDKUNDNR-FOM-UPD                         
249500                             MOD-IDKUNDNR-TOM-UPD                         
249600                             MOD-KDANMORS-UPD                             
249700                             MOD-KVDAGAR-LTRP-UPD                         
249800                             MOD-KVDAGAR-LEVANM-UPD                       
249900                             MOD-KVDAGAR-LTRP-LDC-UPD                     
250000                             MOD-KVDAGAR-LEVANM-LDC-UPD                   
250100                             MOD-KVDAGAR-RET-UPD                          
250200                             MOD-KVDAGAR-RTRP-UPD                         
250300                             MOD-KVDAGAR-RET-LDC-UPD                      
250400                             MOD-KVDAGAR-RTRP-LDC-UPD                     
250500     MOVE 1 TO INDX                                                       
250600     PERFORM UNTIL INDX > MAX-INDX                                        
250700        MOVE MFS-RENSA-FAELT TO MOD-KDCMD              (INDX)             
250800                                MOD-IDDISTR-FOM        (INDX)             
250900                                MOD-IDDISTR-TOM        (INDX)             
251000                                MOD-IDKUNDNR-FOM       (INDX)             
251100                                MOD-IDKUNDNR-TOM       (INDX)             
251200                                MOD-KDANMORS           (INDX)             
251300                                MOD-KVDAGAR-LTRP       (INDX)             
251400                                MOD-KVDAGAR-LEVANM     (INDX)             
251500                                MOD-KVDAGAR-LTRP-LDC   (INDX)             
251600                                MOD-KVDAGAR-LEVANM-LDC (INDX)             
251700                                MOD-KVDAGAR-RET        (INDX)             
251800                                MOD-KVDAGAR-RTRP       (INDX)             
251900                                MOD-KVDAGAR-RET-LDC    (INDX)             
252000                                MOD-KVDAGAR-RTRP-LDC   (INDX)             
252100        ADD 1 TO INDX                                                     
252200     END-PERFORM                                                          
252300     .                                                                    
252400                                                                          
252500 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
252600                                                                          
252700*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
252800     MOVE 1 TO INDX                                                       
252900     PERFORM UNTIL INDX > MAX-INDX                                        
253000        MOVE MFS-RENSA-FAELT TO MOD-KDCMD              (INDX)             
253100                                MOD-IDDISTR-FOM        (INDX)             
253200                                MOD-IDDISTR-TOM        (INDX)             
253300                                MOD-IDKUNDNR-FOM       (INDX)             
253400                                MOD-IDKUNDNR-TOM       (INDX)             
253500                                MOD-KDANMORS           (INDX)             
253600                                MOD-KVDAGAR-LTRP       (INDX)             
253700                                MOD-KVDAGAR-LEVANM     (INDX)             
253800                                MOD-KVDAGAR-LTRP-LDC   (INDX)             
253900                                MOD-KVDAGAR-LEVANM-LDC (INDX)             
254000                                MOD-KVDAGAR-RET        (INDX)             
254100                                MOD-KVDAGAR-RTRP       (INDX)             
254200                                MOD-KVDAGAR-RET-LDC    (INDX)             
254300                                MOD-KVDAGAR-RTRP-LDC   (INDX)             
254400        ADD 1 TO INDX                                                     
254500     END-PERFORM                                                          
254600     .                                                                    
254700                                                                          
254800 MFS-RENSA-FAELT-IN SECTION.                                              
254900                                                                          
255000*    --- ALLA INDATA-FÄLT                                                 
255100     MOVE MFS-RENSA-FAELT TO MOD-KDCMD-DEF                                
255200                             MOD-IDDISTR-FOM-DEF                          
255300                             MOD-IDDISTR-TOM-DEF                          
255400                             MOD-KVDAGAR-LTRP-DEF                         
255500                             MOD-KVDAGAR-LEVANM-DEF                       
255600                             MOD-KVDAGAR-RET-DEF                          
255700                             MOD-KVDAGAR-RTRP-DEF                         
255800                             MOD-KDCMD-UPD                                
255900                             MOD-IDDISTR-FOM-UPD                          
256000                             MOD-IDDISTR-TOM-UPD                          
256100                             MOD-IDKUNDNR-FOM-UPD                         
256200                             MOD-IDKUNDNR-TOM-UPD                         
256300                             MOD-KDANMORS-UPD                             
256400                             MOD-KVDAGAR-LTRP-UPD                         
256500                             MOD-KVDAGAR-LEVANM-UPD                       
256600                             MOD-KVDAGAR-LTRP-LDC-UPD                     
256700                             MOD-KVDAGAR-LEVANM-LDC-UPD                   
256800                             MOD-KVDAGAR-RET-UPD                          
256900                             MOD-KVDAGAR-RTRP-UPD                         
257000                             MOD-KVDAGAR-RET-LDC-UPD                      
257100                             MOD-KVDAGAR-RTRP-LDC-UPD                     
257200                             MOD-TEMFSINF                                 
257300     MOVE 1 TO INDX                                                       
257400     PERFORM UNTIL INDX > MAX-INDX                                        
257500        MOVE MFS-RENSA-FAELT TO MOD-KDCMD              (INDX)             
257600                                MOD-IDDISTR-FOM        (INDX)             
257700                                MOD-IDDISTR-TOM        (INDX)             
257800                                MOD-IDKUNDNR-FOM       (INDX)             
257900                                MOD-IDKUNDNR-TOM       (INDX)             
258000                                MOD-KDANMORS           (INDX)             
258100        ADD 1 TO INDX                                                     
258200     END-PERFORM                                                          
258300     .                                                                    
258400                                                                          
258500 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
258600                                                                          
258700*    --- ALLA UTDATA-FÄLT                                                 
258800*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
259000     MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR-FOM-DEF                        
259100                               MOD-IDDISTR-TOM-DEF                        
259200                               MOD-KVDAGAR-LTRP-DEF                       
259300                               MOD-KVDAGAR-LEVANM-DEF                     
259400                               MOD-KVDAGAR-RET-DEF                        
259500                               MOD-KVDAGAR-RTRP-DEF                       
261100                                                                          
261200     MOVE +1 TO INDX                                                      
261300     PERFORM UNTIL INDX > MAX-INDX                                        
261400        PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                  
261500        ADD +1 TO INDX                                                    
261600     END-PERFORM                                                          
261700     .                                                                    
261800                                                                          
261900 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
262000                                                                          
262100*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
262300     MOVE MFS-ROER-EJ-FAELT TO  MOD-IDDISTR-FOM        (INDX)             
262400                                MOD-IDDISTR-TOM        (INDX)             
262500                                MOD-IDKUNDNR-FOM       (INDX)             
262600                                MOD-IDKUNDNR-TOM       (INDX)             
262700                                MOD-KDANMORS           (INDX)             
262800                                MOD-KVDAGAR-LTRP       (INDX)             
262900                                MOD-KVDAGAR-LEVANM     (INDX)             
263000                                MOD-KVDAGAR-LTRP-LDC   (INDX)             
263100                                MOD-KVDAGAR-LEVANM-LDC (INDX)             
263200                                MOD-KVDAGAR-RET        (INDX)             
263300                                MOD-KVDAGAR-RTRP       (INDX)             
263400                                MOD-KVDAGAR-RET-LDC    (INDX)             
263500                                MOD-KVDAGAR-RTRP-LDC   (INDX)             
263600     .                                                                    
263700                                                                          
263800 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
263900                                                                          
264000*    --- ALLA INDATA-FÄLT                                                 
264100     MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD-DEF                              
264800                               MOD-KDCMD-UPD                              
264900                               MOD-IDDISTR-FOM-UPD                        
265000                               MOD-IDDISTR-TOM-UPD                        
265100                               MOD-IDKUNDNR-FOM-UPD                       
265200                               MOD-IDKUNDNR-TOM-UPD                       
265300                               MOD-KDANMORS-UPD                           
265400                               MOD-KVDAGAR-LTRP-UPD                       
265500                               MOD-KVDAGAR-LEVANM-UPD                     
265600                               MOD-KVDAGAR-LTRP-LDC-UPD                   
265700                               MOD-KVDAGAR-LEVANM-LDC-UPD                 
265800                               MOD-KVDAGAR-RET-UPD                        
265900                               MOD-KVDAGAR-RTRP-UPD                       
266000                               MOD-KVDAGAR-RET-LDC-UPD                    
266100                               MOD-KVDAGAR-RTRP-LDC-UPD                   
266200     MOVE 1 TO INDX                                                       
266300     PERFORM UNTIL INDX > MAX-INDX                                        
266400        MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD        (INDX)                 
267000        ADD 1 TO INDX                                                     
267100     END-PERFORM                                                          
267200     .                                                                    
267300                                                                          
267400 MFS-FORM-ATTR SECTION.                                                   
267500                                                                          
267600*    --- ALLA INDATA-FÄLT                                                 
267700     MOVE MFS-FORMATETS-ATTR TO MOD-KDCMD-DEF-ATTR                        
267800                                MOD-KDCMD-UPD-ATTR                        
267900                                MOD-IDDISTR-FOM-UPD-ATTR                  
268000                                MOD-IDDISTR-TOM-UPD-ATTR                  
268100                                MOD-IDKUNDNR-FOM-UPD-ATTR                 
268200                                MOD-IDKUNDNR-TOM-UPD-ATTR                 
268300                                MOD-KDANMORS-UPD-ATTR                     
268400                                MOD-KVDAGAR-LTRP-UPD-ATTR                 
268500                                MOD-KVDAGAR-LEVANM-UPD-ATTR               
268600                                MOD-KVDAGAR-LTRP-LDC-UPD-ATTR             
268700                                MOD-KVDAGAR-LEVNM-LDC-UPD-ATTR            
268800                                MOD-KVDAGAR-RET-UPD-ATTR                  
268900                                MOD-KVDAGAR-RTRP-UPD-ATTR                 
269000                                MOD-KVDAGAR-RET-LDC-UPD-ATTR              
269100                                MOD-KVDAGAR-RTRP-LDC-UPD-ATTR             
269200     MOVE 1 TO INDX                                                       
269300     PERFORM UNTIL INDX > MAX-INDX                                        
269400        MOVE MFS-FORMATETS-ATTR TO MOD-KDCMD-ATTR        (INDX)           
269500                                   MOD-IDDISTR-FOM-ATTR  (INDX)           
269600                                   MOD-IDDISTR-TOM-ATTR  (INDX)           
269700                                   MOD-IDKUNDNR-FOM-ATTR (INDX)           
269800                                   MOD-IDKUNDNR-TOM-ATTR (INDX)           
269900                                   MOD-KDANMORS-ATTR     (INDX)           
270000        ADD 1 TO INDX                                                     
270100     END-PERFORM                                                          
270200     .                                                                    
270300                                                                          
270400 MFS-LAES-IN-IGEN SECTION.                                                
270500                                                                          
270600*    --- ALLA INDATA-FÄLT                                                 
270700     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-DEF-ATTR                     
270800                                   MOD-KDCMD-UPD-ATTR                     
270900                                   MOD-IDDISTR-FOM-UPD-ATTR               
271000                                   MOD-IDDISTR-TOM-UPD-ATTR               
271100                                   MOD-IDKUNDNR-FOM-UPD-ATTR              
271200                                   MOD-IDKUNDNR-TOM-UPD-ATTR              
271300                                   MOD-KDANMORS-UPD-ATTR                  
271400                                   MOD-KVDAGAR-LTRP-UPD-ATTR              
271500                                   MOD-KVDAGAR-LEVANM-UPD-ATTR            
271600                                   MOD-KVDAGAR-LTRP-LDC-UPD-ATTR          
271700                                   MOD-KVDAGAR-LEVNM-LDC-UPD-ATTR         
271800                                   MOD-KVDAGAR-RET-UPD-ATTR               
271900                                   MOD-KVDAGAR-RTRP-UPD-ATTR              
272000                                   MOD-KVDAGAR-RET-LDC-UPD-ATTR           
272100                                   MOD-KVDAGAR-RTRP-LDC-UPD-ATTR          
272200     MOVE 1 TO INDX                                                       
272300     PERFORM UNTIL INDX > MAX-INDX                                        
272400        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-ATTR        (INDX)        
272500                                      MOD-IDDISTR-FOM-ATTR  (INDX)        
272600                                      MOD-IDDISTR-TOM-ATTR  (INDX)        
272700                                      MOD-IDKUNDNR-FOM-ATTR (INDX)        
272800                                      MOD-IDKUNDNR-TOM-ATTR (INDX)        
272900                                      MOD-KDANMORS-ATTR     (INDX)        
273000        ADD 1 TO INDX                                                     
273100     END-PERFORM                                                          
273200     .                                                                    
273300                                                                          
273400                                                                          
273500* --- IMS SEKTIONER ---                                                   
273600                                                                          
273700 IMS-GET-MSG SECTION.                                                     
273800                                                                          
273900     MOVE '  QC'          TO GODK-STATUSKODER                             
274000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
274100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
274200     PERFORM IMS-STATUSKONTROLL                                           
274300     .                                                                    
274400                                                                          
274500 IMS-INSERT-MSG SECTION.                                                  
274600                                                                          
274700     MOVE LOW-VALUE       TO MSG-KDZ1 MSG-KDZ2                            
274800     MOVE SPACE           TO GODK-STATUSKODER                             
274900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
275000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
275100     PERFORM IMS-STATUSKONTROLL                                           
275200     .                                                                    
275300                                                                          
275400                                                                          
275500 IMS-GU-WDR501 SECTION.                                                   
275600     MOVE 'IMS-GU-WDR501   '  TO CURRENT-IMS-SECTION                      
275700                                                                          
275800     MOVE SPACE               TO ALL-SSA                                  
275900     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4127-X ')'                    
276000          DELIMITED BY SIZE INTO SSA1                                     
276100     MOVE '  '                TO GODK-STATUSKODER                         
276200     CALL CBLTDLI USING GU 4128-PCB DLI-IO-WDR101 SSA1                    
276300     MOVE 4128-STATUS-CODE    TO STATUS-WS                                
276400     PERFORM IMS-STATUSKONTROLL                                           
276500     .                                                                    
276600                                                                          
276700                                                                          
276800 IMS-GNP-WDGX4128 SECTION.                                                
276900     MOVE 'IMS-GNP-WDGX4128'  TO CURRENT-IMS-SECTION                      
277000                                                                          
277100     MOVE SPACE               TO ALL-SSA                                  
277200     STRING 'WDGX4128(KY4128  >=' W-KEY4128-X                             
277300                    '&IDDISTRF>=' W-4128-IDDISTR-FOM-X                    
277400                    '&IDDISTRT<=' W-4128-IDDISTR-TOM-X                    
277500                    '&IDKUNDNF>=' W-4128-IDKUNDNR-FOM-X                   
277600                    '&IDKUNDNT<=' W-4128-IDKUNDNR-TOM-X                   
277700                    '&KDANMORS>=' W-4128-KDANMORS-FOM-X                   
277800                    '&KDANMORS<=' W-4128-KDANMORS-TOM-X ')'               
277900          DELIMITED BY SIZE INTO SSA1                                     
278000     MOVE '  GE'              TO GODK-STATUSKODER                         
278100     CALL CBLTDLI USING GNP 4128-PCB DLI-IO-WDGX4128 SSA1                 
278200     MOVE 4128-STATUS-CODE    TO STATUS-WS                                
278300     PERFORM IMS-STATUSKONTROLL                                           
278400     .                                                                    
278500                                                                          
278600                                                                          
278700 IMS-GNP-WDGX4128-DI SECTION.                                             
278800     MOVE 'GNP-WDGX4128-DI '  TO CURRENT-IMS-SECTION                      
278900                                                                          
279000     MOVE SPACE               TO ALL-SSA                                  
279100     STRING 'WDGX4128(KY4128  >=' W-KEY4128-X                             
279200                    '&IDDISTRF<=' W-4128-IDDISTR-FOM-X                    
279300                    '&IDDISTRT>=' W-4128-IDDISTR-TOM-X                    
279400                    '&IDKUNDNF>=' W-4128-IDKUNDNR-FOM-X                   
279500                    '&IDKUNDNT<=' W-4128-IDKUNDNR-TOM-X                   
279600                    '&KDANMORS>=' W-4128-KDANMORS-FOM-X                   
279700                    '&KDANMORS<=' W-4128-KDANMORS-TOM-X ')'               
279800          DELIMITED BY SIZE INTO SSA1                                     
279900     MOVE '  GE'              TO GODK-STATUSKODER                         
280000     CALL CBLTDLI USING GNP 4128-PCB DLI-IO-WDGX4128 SSA1                 
280100     MOVE 4128-STATUS-CODE    TO STATUS-WS                                
280200     PERFORM IMS-STATUSKONTROLL                                           
280300     .                                                                    
280400                                                                          
280500                                                                          
280600 IMS-GU-WDGX4128-DI SECTION.                                              
280700     MOVE 'GU-WDGX4128-DI  '  TO CURRENT-IMS-SECTION                      
280800                                                                          
280900     MOVE SPACE               TO ALL-SSA                                  
280910     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4127-X ')'                    
280920          DELIMITED BY SIZE INTO SSA1                                     
281000     STRING 'WDGX4128(IDDISTRF =' W-4128-IDDISTR-FOM-X                    
281100                    '&IDDISTRT =' W-4128-IDDISTR-TOM-X ')'                
281200          DELIMITED BY SIZE INTO SSA2                                     
281300     MOVE '  GE'              TO GODK-STATUSKODER                         
281400     CALL CBLTDLI USING GU 4128-PCB DLI-IO-WDGX4128 SSA1 SSA2             
281500     MOVE 4128-STATUS-CODE    TO STATUS-WS                                
281600     PERFORM IMS-STATUSKONTROLL                                           
281700     .                                                                    
281800                                                                          
281900                                                                          
282000 IMS-GNP-WDGX4128-DI-CH SECTION.                                          
282100     MOVE 'GNP-WDGX4128-DIC'  TO CURRENT-IMS-SECTION                      
282200                                                                          
282300     MOVE SPACE               TO ALL-SSA                                  
282400     STRING 'WDGX4128(IDDISTRFNE' W-IDDISTR-DEF-FOM-X                     
282500                    '&IDDISTRTNE' W-IDDISTR-DEF-TOM-X                     
282510                    '&IDDISTRF>=' W-4128-IDDISTR-FOM-X                    
282520                    '&IDDISTRT<=' W-4128-IDDISTR-TOM-X                    
282610                    '!IDDISTRFNE' W-IDDISTR-DEF-FOM-X                     
282620                    '&IDDISTRTNE' W-IDDISTR-DEF-TOM-X                     
282630                    '&IDDISTRF<=' W-4128-IDDISTR-FOM-X                    
282700                    '&IDDISTRT>=' W-4128-IDDISTR-FOM-X                    
282710                    '!IDDISTRFNE' W-IDDISTR-DEF-FOM-X                     
282720                    '&IDDISTRTNE' W-IDDISTR-DEF-TOM-X                     
282800                    '&IDDISTRF<=' W-4128-IDDISTR-TOM-X                    
282900                    '&IDDISTRT>=' W-4128-IDDISTR-TOM-X ')'                
283200          DELIMITED BY SIZE INTO SSA1                                     
283300     MOVE '  GE'              TO GODK-STATUSKODER                         
283400     CALL CBLTDLI USING GNP 4128-PCB DLI-IO-WDGX4128 SSA1                 
283500     MOVE 4128-STATUS-CODE    TO STATUS-WS                                
283600     PERFORM IMS-STATUSKONTROLL                                           
283700     .                                                                    
283800                                                                          
283900                                                                          
284000 IMS-GNP-WDGX4128-KU SECTION.                                             
284100     MOVE 'GNP-WDGX4128-KU '  TO CURRENT-IMS-SECTION                      
284200                                                                          
284300     MOVE SPACE               TO ALL-SSA                                  
284400     STRING 'WDGX4128(KY4128  >=' W-KEY4128-X                             
284500                    '&IDDISTRF>=' W-4128-IDDISTR-FOM-X                    
284600                    '&IDDISTRT<=' W-4128-IDDISTR-TOM-X                    
284700                    '&IDKUNDNF<=' W-4128-IDKUNDNR-FOM-X                   
284800                    '&IDKUNDNT>=' W-4128-IDKUNDNR-TOM-X                   
284900                    '&KDANMORS>=' W-4128-KDANMORS-FOM-X                   
285000                    '&KDANMORS<=' W-4128-KDANMORS-TOM-X ')'               
285100          DELIMITED BY SIZE INTO SSA1                                     
285200     MOVE '  GE'              TO GODK-STATUSKODER                         
285300     CALL CBLTDLI USING GNP 4128-PCB DLI-IO-WDGX4128 SSA1                 
285400     MOVE 4128-STATUS-CODE    TO STATUS-WS                                
285500     PERFORM IMS-STATUSKONTROLL                                           
285600     .                                                                    
285700                                                                          
285800                                                                          
285900 IMS-GU-WDGX4128-KU SECTION.                                              
286000     MOVE 'GU-WDGX4128-KU  '  TO CURRENT-IMS-SECTION                      
286100                                                                          
286200     MOVE SPACE               TO ALL-SSA                                  
286210     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4127-X ')'                    
286220          DELIMITED BY SIZE INTO SSA1                                     
286300     STRING 'WDGX4128(IDDISTRF =' W-4128-IDDISTR-FOM-X                    
286400                    '&IDDISTRT =' W-4128-IDDISTR-TOM-X                    
286500                    '&IDKUNDNF =' W-4128-IDKUNDNR-FOM-X                   
286600                    '&IDKUNDNT =' W-4128-IDKUNDNR-TOM-X ')'               
286700          DELIMITED BY SIZE INTO SSA2                                     
286800     MOVE '  GE'              TO GODK-STATUSKODER                         
286900     CALL CBLTDLI USING GU 4128-PCB DLI-IO-WDGX4128 SSA1 SSA2             
287000     MOVE 4128-STATUS-CODE    TO STATUS-WS                                
287100     PERFORM IMS-STATUSKONTROLL                                           
287200     .                                                                    
287300                                                                          
287400                                                                          
287500 IMS-GNP-WDGX4128-KU-CH SECTION.                                          
287600     MOVE 'GNP-WDGX4128-KUC'  TO CURRENT-IMS-SECTION                      
287700                                                                          
287800     MOVE SPACE               TO ALL-SSA                                  
287900     STRING 'WDGX4128(IDKUNDNFNE' W-IDKUNDNR-DEF-FOM-X                    
287901                    '&IDKUNDNTNE' W-IDKUNDNR-DEF-TOM-X                    
287910                    '&IDDISTRF =' W-4128-IDDISTR-FOM-X                    
288000                    '&IDDISTRT =' W-4128-IDDISTR-TOM-X                    
288100                    '&IDKUNDNF>=' W-4128-IDKUNDNR-FOM-X                   
288200                    '&IDKUNDNT<=' W-4128-IDKUNDNR-TOM-X                   
288201                    '!IDKUNDNFNE' W-IDKUNDNR-DEF-FOM-X                    
288202                    '&IDKUNDNTNE' W-IDKUNDNR-DEF-TOM-X                    
288210                    '&IDDISTRF =' W-4128-IDDISTR-FOM-X                    
288220                    '&IDDISTRT =' W-4128-IDDISTR-TOM-X                    
288300                    '&IDKUNDNF<=' W-4128-IDKUNDNR-FOM-X                   
288400                    '&IDKUNDNT>=' W-4128-IDKUNDNR-FOM-X                   
288401                    '!IDKUNDNFNE' W-IDKUNDNR-DEF-FOM-X                    
288402                    '&IDKUNDNTNE' W-IDKUNDNR-DEF-TOM-X                    
288410                    '&IDDISTRF =' W-4128-IDDISTR-FOM-X                    
288420                    '&IDDISTRT =' W-4128-IDDISTR-TOM-X                    
288500                    '&IDKUNDNF<=' W-4128-IDKUNDNR-TOM-X                   
288600                    '&IDKUNDNT>=' W-4128-IDKUNDNR-TOM-X ')'               
288700          DELIMITED BY SIZE INTO SSA1                                     
288800     MOVE '  GE'              TO GODK-STATUSKODER                         
288900     CALL CBLTDLI USING GNP 4128-PCB DLI-IO-WDGX4128 SSA1                 
289000     MOVE 4128-STATUS-CODE    TO STATUS-WS                                
289100     PERFORM IMS-STATUSKONTROLL                                           
289200     .                                                                    
289300                                                                          
289400                                                                          
289500 IMS-GNP-WDGX4128-KD SECTION.                                             
289600     MOVE 'GNP-WDGX4128-KD '  TO CURRENT-IMS-SECTION                      
289700                                                                          
289800     MOVE SPACE               TO ALL-SSA                                  
289900     STRING 'WDGX4128(IDDISTRFNE' W-IDDISTR-DEF-FOM-X                     
290000                    '&IDDISTRTNE' W-IDDISTR-DEF-TOM-X                     
290010                    '&IDDISTRF =' W-4128-IDDISTR-FOM-X                    
290020                    '&IDDISTRT =' W-4128-IDDISTR-TOM-X                    
290100                    '&IDKUNDNF =' W-4128-IDKUNDNR-FOM-X                   
290200                    '&IDKUNDNT =' W-4128-IDKUNDNR-TOM-X                   
290300                    '&KDANMORS>=' W-4128-KDANMORS-FOM-X                   
290400                    '&KDANMORS<=' W-4128-KDANMORS-TOM-X                   
290401                    '!IDDISTRFNE' W-IDDISTR-DEF-FOM-X                     
290402                    '&IDDISTRTNE' W-IDDISTR-DEF-TOM-X                     
290410                    '&IDDISTRF =' W-4128-IDDISTR-FOM-X                    
290420                    '&IDDISTRT =' W-4128-IDDISTR-TOM-X                    
290430                    '&IDKUNDNF =' W-4128-IDKUNDNR-FOM-X                   
290440                    '&IDKUNDNT =' W-4128-IDKUNDNR-TOM-X                   
290460                    '&KDANMORS =' W-KDANMORS-NX                           
290461                    '!IDDISTRFNE' W-IDDISTR-DEF-FOM-X                     
290462                    '&IDDISTRTNE' W-IDDISTR-DEF-TOM-X                     
290470                    '&IDDISTRF =' W-4128-IDDISTR-FOM-X                    
290480                    '&IDDISTRT =' W-4128-IDDISTR-TOM-X                    
290490                    '&IDKUNDNF =' W-4128-IDKUNDNR-FOM-X                   
290491                    '&IDKUNDNT =' W-4128-IDKUNDNR-TOM-X                   
290492                    '&KDANMORS =' W-KDANMORS-SPACE ')'                    
290500          DELIMITED BY SIZE INTO SSA1                                     
290600     MOVE '  GE'              TO GODK-STATUSKODER                         
290700     CALL CBLTDLI USING GNP 4128-PCB DLI-IO-WDGX4128 SSA1                 
290800     MOVE 4128-STATUS-CODE    TO STATUS-WS                                
290900     PERFORM IMS-STATUSKONTROLL                                           
291000     .                                                                    
291100                                                                          
291200                                                                          
291300 IMS-GNP-WDGX4128-KDI SECTION.                                            
291400     MOVE 'GNP-WDGX4128-KDI'  TO CURRENT-IMS-SECTION                      
291500                                                                          
291600     MOVE SPACE               TO ALL-SSA                                  
291700     STRING 'WDGX4128(IDDISTRFNE' W-IDDISTR-DEF-FOM-X                     
291800                    '&IDDISTRTNE' W-IDDISTR-DEF-TOM-X                     
291810                    '&IDDISTRF =' W-4128-IDDISTR-FOM-X                    
291820                    '&IDDISTRT =' W-4128-IDDISTR-TOM-X                    
291900                    '&IDKUNDNF<=' W-4128-IDKUNDNR-FOM-X                   
292000                    '&IDKUNDNT>=' W-4128-IDKUNDNR-TOM-X                   
292100                    '&KDANMORS>=' W-4128-KDANMORS-FOM-X                   
292200                    '&KDANMORS<=' W-4128-KDANMORS-TOM-X                   
292210                    '!IDDISTRFNE' W-IDDISTR-DEF-FOM-X                     
292220                    '&IDDISTRTNE' W-IDDISTR-DEF-TOM-X                     
292230                    '&IDDISTRF =' W-4128-IDDISTR-FOM-X                    
292240                    '&IDDISTRT =' W-4128-IDDISTR-TOM-X                    
292250                    '&IDKUNDNF<=' W-4128-IDKUNDNR-FOM-X                   
292260                    '&IDKUNDNT>=' W-4128-IDKUNDNR-TOM-X                   
292270                    '&KDANMORS =' W-KDANMORS-NX                           
292280                    '!IDDISTRFNE' W-IDDISTR-DEF-FOM-X                     
292290                    '&IDDISTRTNE' W-IDDISTR-DEF-TOM-X                     
292291                    '&IDDISTRF =' W-4128-IDDISTR-FOM-X                    
292292                    '&IDDISTRT =' W-4128-IDDISTR-TOM-X                    
292293                    '&IDKUNDNF<=' W-4128-IDKUNDNR-FOM-X                   
292294                    '&IDKUNDNT>=' W-4128-IDKUNDNR-TOM-X                   
292295                    '&KDANMORS =' W-KDANMORS-SPACE ')'                    
292300          DELIMITED BY SIZE INTO SSA1                                     
292400     MOVE '  GE'              TO GODK-STATUSKODER                         
292500     CALL CBLTDLI USING GNP 4128-PCB DLI-IO-WDGX4128 SSA1                 
292600     MOVE 4128-STATUS-CODE    TO STATUS-WS                                
292700     PERFORM IMS-STATUSKONTROLL                                           
292800     .                                                                    
292900                                                                          
293000                                                                          
293100 IMS-GNP-WDGX4128-DIK SECTION.                                            
293200     MOVE 'GNP-WDGX4128-DIK'  TO CURRENT-IMS-SECTION                      
293300                                                                          
293400     MOVE SPACE               TO ALL-SSA                                  
293500     STRING 'WDGX4128(IDDISTRFNE' W-IDDISTR-DEF-FOM-X                     
293600                    '&IDDISTRTNE' W-IDDISTR-DEF-TOM-X                     
293610                    '&IDDISTRF<=' W-4128-IDDISTR-FOM-X                    
293620                    '&IDDISTRT>=' W-4128-IDDISTR-TOM-X                    
293700                    '&KDANMORS>=' W-4128-KDANMORS-FOM-X                   
293800                    '&KDANMORS<=' W-4128-KDANMORS-TOM-X                   
293801                    '!IDDISTRFNE' W-IDDISTR-DEF-FOM-X                     
293802                    '&IDDISTRTNE' W-IDDISTR-DEF-TOM-X                     
293810                    '&IDDISTRF<=' W-4128-IDDISTR-FOM-X                    
293820                    '&IDDISTRT>=' W-4128-IDDISTR-TOM-X                    
293830                    '&KDANMORS =' W-KDANMORS-NX                           
293831                    '!IDDISTRFNE' W-IDDISTR-DEF-FOM-X                     
293832                    '&IDDISTRTNE' W-IDDISTR-DEF-TOM-X                     
293840                    '&IDDISTRF<=' W-4128-IDDISTR-FOM-X                    
293850                    '&IDDISTRT>=' W-4128-IDDISTR-TOM-X                    
293860                    '&KDANMORS =' W-KDANMORS-SPACE ')'                    
293900          DELIMITED BY SIZE INTO SSA1                                     
294000     MOVE '  GE'              TO GODK-STATUSKODER                         
294100     CALL CBLTDLI USING GNP 4128-PCB DLI-IO-WDGX4128 SSA1                 
294200     MOVE 4128-STATUS-CODE    TO STATUS-WS                                
294300     PERFORM IMS-STATUSKONTROLL                                           
294400     .                                                                    
294500                                                                          
294600                                                                          
294700 IMS-GU-WDGX4128 SECTION.                                                 
294800     MOVE 'IMS-GU-WDGX4128'   TO CURRENT-IMS-SECTION                      
294900                                                                          
295000     MOVE SPACE               TO ALL-SSA                                  
295100     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4127-X ')'                    
295200          DELIMITED BY SIZE INTO SSA1                                     
295300     STRING 'WDGX4128(KY4128   =' W-KEY4128-X ')'                         
295400          DELIMITED BY SIZE INTO SSA2                                     
295500     MOVE '  GE'              TO GODK-STATUSKODER                         
295600     CALL CBLTDLI USING GU 4128-PCB DLI-IO-WDGX4128 SSA1 SSA2             
295700     MOVE 4128-STATUS-CODE    TO STATUS-WS                                
295800     PERFORM IMS-STATUSKONTROLL                                           
295900     .                                                                    
296000                                                                          
296100                                                                          
296200 IMS-GHU-WDGX4128 SECTION.                                                
296300     MOVE 'IMS-GHU-WDGX4128'  TO CURRENT-IMS-SECTION                      
296400                                                                          
296500     MOVE SPACE               TO ALL-SSA                                  
296600     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4127-X ')'                    
296700          DELIMITED BY SIZE INTO SSA1                                     
296800     STRING 'WDGX4128(KY4128   =' W-KEY4128-X ')'                         
296900          DELIMITED BY SIZE INTO SSA2                                     
297000     MOVE '  GE'              TO GODK-STATUSKODER                         
297100     CALL CBLTDLI USING GHU 4128-PCB DLI-IO-WDGX4128 SSA1 SSA2            
297200     MOVE 4128-STATUS-CODE    TO STATUS-WS                                
297300     PERFORM IMS-STATUSKONTROLL                                           
297400     .                                                                    
297500                                                                          
297600                                                                          
297700 IMS-REPL-WDGX4128 SECTION.                                               
297800     MOVE 'IMS-REP-WDGX4128'  TO CURRENT-IMS-SECTION                      
297900                                                                          
298000     MOVE SPACE               TO ALL-SSA                                  
298100     MOVE '    '              TO GODK-STATUSKODER                         
298200     CALL CBLTDLI USING REPL 4128-PCB DLI-IO-WDGX4128                     
298300     MOVE 4128-STATUS-CODE    TO STATUS-WS                                
298400     PERFORM IMS-STATUSKONTROLL                                           
298500     .                                                                    
298600                                                                          
298700                                                                          
298800 IMS-DLET-WDGX4128 SECTION.                                               
298900     MOVE 'IMS-DEL-WDGX4128'  TO CURRENT-IMS-SECTION                      
299000                                                                          
299100     MOVE SPACE               TO ALL-SSA                                  
299200     MOVE '    '              TO GODK-STATUSKODER                         
299300     CALL CBLTDLI USING DLET 4128-PCB DLI-IO-WDGX4128                     
299400     MOVE 4128-STATUS-CODE    TO STATUS-WS                                
299500     PERFORM IMS-STATUSKONTROLL                                           
299600     .                                                                    
299700                                                                          
299800                                                                          
299900 IMS-ISRT-WDGX4128 SECTION.                                               
300000     MOVE 'IMS-REP-WDGX4128'  TO CURRENT-IMS-SECTION                      
300100                                                                          
300200     MOVE SPACE               TO ALL-SSA                                  
300300     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4127-X ')'                    
300400          DELIMITED BY SIZE INTO SSA1                                     
300500     MOVE 'WDGX4128 '         TO SSA2                                     
300600     MOVE '    '              TO GODK-STATUSKODER                         
300700     CALL CBLTDLI USING ISRT 4128-PCB DLI-IO-WDGX4128 SSA1 SSA2           
300800     MOVE 4128-STATUS-CODE    TO STATUS-WS                                
300900     PERFORM IMS-STATUSKONTROLL                                           
301000     .                                                                    
301100                                                                          
301200                                                                          
301300 IMS-GU-WDB201 SECTION.                                                   
301400     MOVE 'IMS-GU-WDB201   '  TO CURRENT-IMS-SECTION                      
301500                                                                          
301600     MOVE SPACE               TO ALL-SSA                                  
301700     STRING 'WDB201  (IDGMT   >=' W-IDGMT-FOM-X                           
301800                    '&IDGMT   <=' W-IDGMT-TOM-X ')'                       
301900          DELIMITED BY SIZE INTO SSA1                                     
302000     MOVE '  GE'              TO GODK-STATUSKODER                         
302100     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
302200     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
302300     PERFORM IMS-STATUSKONTROLL                                           
302400     .                                                                    
302500                                                                          
302749 IMS-GNP-WDGX4128-DISTRIKT SECTION.                                       
302750     MOVE 'GNP-4128-DISTRIK'  TO CURRENT-IMS-SECTION                      
302751                                                                          
302752     MOVE SPACE               TO ALL-SSA                                  
302753     STRING 'WDGX4128(IDDISTRF =' W-4128-IDDISTR-FOM-X                    
302754                    '&IDDISTRT =' W-4128-IDDISTR-TOM-X ')'                
302757          DELIMITED BY SIZE INTO SSA1                                     
302758     MOVE '  GEGB'            TO GODK-STATUSKODER                         
302759     CALL CBLTDLI USING GNP 4128-PCB DLI-IO-WDGX4128 SSA1                 
302760     MOVE 4128-STATUS-CODE    TO STATUS-WS                                
302761     PERFORM IMS-STATUSKONTROLL                                           
302762     .                                                                    
302770                                                                          
302790 IMS-STATUSKONTROLL SECTION.                                              
302800                                                                          
302900     SET STATUS-IX TO 1                                                   
303000     SEARCH GODK-STATUS                                                   
303100       AT END                                                             
303200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
303300         DELIMITED BY SIZE INTO FELTEXT                                   
303400         CALL FELLOG                                                      
303500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
303600         CONTINUE                                                         
303700     END-SEARCH                                                           
304000     .                                                                    
