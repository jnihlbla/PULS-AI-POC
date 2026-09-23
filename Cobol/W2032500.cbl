000100*                                                                         
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W2032500.                                                
000400 AUTHOR.         BODIL LINDAHL                                            
000500 DATE-WRITTEN.   MARS 2007                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        SPFU MANUAL UPDATE                                               
001000*                                                                         
001100*    INDATA.                                                              
001200*        TRANSAKTION: W2T325                                              
001300*        MID:         W2I32501                                            
001400*                                                                         
001500*    UTDATA.                                                              
001600*        MOD:         W2O32501                                            
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     EJECT                                                                
002100 DATA DIVISION.                                                           
002200 WORKING-STORAGE SECTION.                                                 
002300                                                                          
002400 77  IDPGM                       PIC X(08)   VALUE 'W2032500'.            
002500 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
002600 77  JA                          PIC X       VALUE 'J'.                   
002700 77  NEJ                         PIC X       VALUE 'N'.                   
002800 77  RAD-IX                      PIC S9(3)   VALUE ZERO.                  
002900 77  MAX-RAD                     PIC S9(3)   VALUE +14.                   
       77  IX1                         PIC S9(3)   VALUE ZERO.                  
       77  IX2                         PIC S9(3)   VALUE ZERO.                  
       77  IX-SPAR                     PIC S9(3)   VALUE ZERO.                  
003000 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
003100 77  WS-IDARTNR                  PIC S9(9)   COMP-3 VALUE ZERO.           
003200 77  WS-TIAAMMDD                 PIC 9(6)    VALUE ZERO.                  
003300 77  WS-KVAVIS                   PIC S9(7)   COMP-3 VALUE ZERO.           
003400 77  WS-KVANTMOT                 PIC S9(7)   COMP-3 VALUE ZERO.           
003500 77  WS-TYP                      PIC X       VALUE SPACE.                 
003600                                                                          
003700 77  W-TID                       PIC 9(8)    VALUE ZERO.                  
003800 77  W-DAGENS-DATUM              PIC 9(6)    VALUE ZERO.                  
003800 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
003900 01  W-TIME-X.                                                            
004000     03  W-TIME-TT       PIC 9(2).                                        
004100     03  FILLER          PIC 9(6).                                        
004200 01  W-TIME-N            REDEFINES W-TIME-X PIC 9(8).                     
004300                                                                          
004400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004500     88  INDATA-OK                           VALUE 'J'.                   
004600     88  INDATA-FEL                          VALUE 'N'.                   
004700                                                                          
004800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004900     88  EGEN-MID                            VALUE '2325'.                
005000     88  GODK-MID                            VALUE '2325'.                
005100     88  HELP-MID                            VALUE '0551'.                
005200                                                                          
005300 01  GENERELLA-SUBPROGRAM.                                                
005400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
005500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
005600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
005900     EJECT                                                                
006000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
006100*01 -COPY WMEDAREA                                                        
006200     SKIP3                                                                
006300 01  MESSAGE-CODES.                                                       
006400     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
006500     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
006600     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
006700     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
006800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
006900     03  INF-URVAL-SAKNAS        PIC X(3)    VALUE '005'.                 
007000     EJECT                                                                
007100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
007200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
007300     SKIP3                                                                
007400*01 -COPY WMSGINIT                                                        
007500     EJECT                                                                
007600*01 -COPY WDATAREA                                                        
007700     EJECT                                                                
007800*01 -COPY W23686  -PRE FILC-                                              
007900     EJECT                                                                
008000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
008200     SKIP3                                                                
008300*01  MID -COPY W2I32501                                                   
008400     EJECT                                                                
008500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
008600     SKIP3                                                                
008700*01  -COPY WMSGAREA                                                       
008800     EJECT                                                                
008900     03  MOD REDEFINES MSG-AREA.                                          
009000*      05  -COPY W2O32501                                                 
009100     EJECT                                                                
009200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
009300     SKIP3                                                                
009400*01  -COPY WMFSAREA                                                       
009500     EJECT                                                                
009600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009700     SKIP2                                                                
009800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009900     SKIP2                                                                
010000*    --- STATUS-KOD FRÅN IMS                                              
010100 01  STATUS-WS                   PIC XX.                                  
010200     88  SEGMENT-FINNS                       VALUE '  '.                  
010300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010500     SKIP2                                                                
010600 01  GODK-STATUSKODER.                                                    
010700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010800     EJECT                                                                
010900 01  NYCKLAR-TILL-DLI.                                                    
011000   03  W-IDLEVNR-X.                                                       
011100     05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                     
011200   03  W-IDARTNR-X.                                                       
011300     05  W-IDARTNR           PIC S9(9)   COMP-3 VALUE ZERO.               
011400     SKIP3                                                                
011500 01  SSA1                        PIC X(64).                               
011600     EJECT                                                                
011700*    --- IMS FUNKTIONSKODER                                               
011800*01  -COPY W0003                                                          
011900     EJECT                                                                
012000*    ---  DLI INPUT-OUTPUT AREA                                           
012100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR301'.                      
012200 01  DLI-IO-WDR301.                                                       
012300*    03  -COPY WDR301 -PRE FILC-                                          
012400     EJECT                                                                
012500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
012600 01  DLI-IO-WDK601.                                                       
012700*    03  -COPY WDK601                                                     
012800     EJECT                                                                
012900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF101'.                      
013000 01  DLI-IO-WDF101.                                                       
013100*    03  -COPY WDF101                                                     
013200     EJECT                                                                
013300 LINKAGE SECTION.                                                         
013400*01  -COPY W0009  -PRE MSG-                                               
013500     EJECT                                                                
013600*01  -COPY W0008  -PRE USEA-                                              
013700     05  FILLER                  PIC X.                                   
013800     EJECT                                                                
013900*01  -COPY W0008  -PRE WLFILC-                                            
014000     05  FILLER                  PIC X.                                   
014100     EJECT                                                                
014200*01  -COPY W0008  -PRE WDF1-                                              
014300     05  FILLER                  PIC X.                                   
014400     EJECT                                                                
014500*01  -COPY W0008  -PRE WDK6-                                              
014600     05  FILLER                  PIC X.                                   
014700     EJECT                                                                
014800 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WLFILC-PCB                    
014900                           WDF1-PCB WDK6-PCB.                             
015000 MAIN SECTION.                                                            
015100     ENTRY 'DLITCBL' USING MSG-PCB  USEA-PCB WLFILC-PCB                   
015200                           WDF1-PCB WDK6-PCB.                             
015300                                                                          
015400     PERFORM IMS-GET-MSG                                                  
015500     IF SEGMENT-FINNS                                                     
015600        PERFORM A-INIT                                                    
015700        IF MFS-UPDATE                                                     
015800           PERFORM G-KOLLA-INPUT                                          
015900           IF INDATA-OK                                                   
016000              PERFORM H-UPPDATERA                                         
016100           END-IF                                                         
016200        ELSE                                                              
016300           IF MFS-FIRST                                                   
016400             PERFORM C-FORSTA-SIDA                                        
016500           ELSE                                                           
016600             IF MFS-NEXT                                                  
016700                PERFORM E-SAMMA-SIDA                                      
016800             ELSE                                                         
016810                IF MFS-ENTER                                              
016900                   PERFORM E-SAMMA-SIDA                                   
016910                ELSE                                                      
016920                   PERFORM C-FORSTA-SIDA                                  
016930                END-IF                                                    
017000             END-IF                                                       
017100          END-IF                                                          
017200        END-IF                                                            
017300        COMPUTE MSG-KVLL = LENGTH OF MOD-W2O32501-CTX + 4                 
017400        PERFORM IMS-INSERT-MSG                                            
017500     END-IF                                                               
017600                                                                          
017700     MOVE ZERO TO RETURN-CODE                                             
017800     GOBACK                                                               
017900     .                                                                    
018000     EJECT                                                                
018100 A-INIT SECTION.                                                          
018200                                                                          
018300     IF MSG-DUBBLA-TRANSKODER                                             
018400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I32501-CTX             
018500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
018600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
018700     ELSE                                                                 
018800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I32501-CTX              
018900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
019000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
019100     END-IF                                                               
019200                                                                          
019300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
019400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
019500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
019600                                                                          
019700     MOVE LOW-VALUE TO MSG-AREA                                           
019800     MOVE 'W2O325N1' TO MFS-IDMOD                                         
019900     MOVE '2325' TO MOD-IDTRANS                                           
020000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
020100                                                                          
020200     IF EGEN-MID                                                          
020300       CONTINUE                                                           
020400     ELSE                                                                 
020500       MOVE SPACE TO MFS-KDTRTYP                                          
020600       MOVE '7' TO MFS-IDPFK                                              
020700     END-IF                                                               
020800                                                                          
020900     MOVE 'IDAG'          TO DAT-KDDATFORM                                
021000     CALL WDATKONV USING     DAT-KDDATFORM                                
021100                             DAT-I-TIDATUM                                
021200                             DAT-O-TIDATUM                                
021300                             DAT-KDSVAR                                   
021400                                                                          
021500     MOVE DAT-TIAAMMDD    TO W-DAGENS-DATUM                               
                                   DAGENS-DATUM                                 
021600     ACCEPT W-TIME-X      FROM TIME                                       
021700                                                                          
021800     MOVE 'W2032500'      TO FILC-FIL-IDPGM                               
021900     MOVE W-DAGENS-DATUM  TO FILC-FIL-TIREGDAT                            
022000     MOVE 'W23686  '      TO FILC-FIL-IDCPYTXT                            
022100     MOVE ZERO            TO FILC-FIL-TIKLOCK                             
022200     .                                                                    
022300     EJECT                                                                
022400 C-FORSTA-SIDA SECTION.                                                   
022401                                                                          
022402     PERFORM MFS-RENSA-FAELT-IN                                           
022403     .                                                                    
022404     EJECT                                                                
022410 E-SAMMA-SIDA SECTION.                                                    
022500                                                                          
022600     IF EGEN-MID                                                          
022700       IF MID-W2I32501-CTX = ALL '+'                                      
022800         PERFORM MFS-RENSA-FAELT-IN                                       
022900       ELSE                                                               
023000         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
023100         CALL WMEDKONV USING MED-WMEDAREA                                 
023200         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
023300         PERFORM EA-MID-INDATA-TILL-MOD                                   
023400       END-IF                                                             
023500     ELSE                                                                 
023600       PERFORM MFS-RENSA-FAELT-IN                                         
023700     END-IF                                                               
023800     .                                                                    
023900     EJECT                                                                
024000 EA-MID-INDATA-TILL-MOD SECTION.                                          
024100                                                                          
024200     MOVE +1 TO RAD-IX                                                    
024300     PERFORM UNTIL RAD-IX > MAX-RAD                                       
024400                                                                          
024500         IF MID-IDLEVNR(RAD-IX) = ALL '+'                                 
024600             MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR(RAD-IX)                  
024700         ELSE                                                             
024800             MOVE MID-IDLEVNR(RAD-IX) TO MOD-IDLEVNR(RAD-IX)              
024900             MOVE MFS-ADD-LAES-IN-FAELT                                   
025000                                TO MOD-IDLEVNR-ATTR(RAD-IX)               
025100         END-IF                                                           
025200                                                                          
025300         IF MID-IDARTNR(RAD-IX) = ALL '+'                                 
025400             MOVE MFS-RENSA-FAELT TO MOD-IDARTNR(RAD-IX)                  
025500         ELSE                                                             
025600             MOVE MID-IDARTNR(RAD-IX) TO MOD-IDARTNR(RAD-IX)              
025700             MOVE MFS-ADD-LAES-IN-FAELT                                   
025800                                TO MOD-IDARTNR-ATTR(RAD-IX)               
025900             INSPECT MOD-IDARTNR(RAD-IX) REPLACING LEADING ZERO           
026000                                         BY SPACE                         
026100         END-IF                                                           
026200                                                                          
026300         IF MID-TIAAMMDD(RAD-IX) = ALL '+'                                
026400             MOVE MFS-RENSA-FAELT TO MOD-TIAAMMDD(RAD-IX)                 
026500         ELSE                                                             
026600             MOVE MID-TIAAMMDD(RAD-IX) TO MOD-TIAAMMDD(RAD-IX)            
026700             MOVE MFS-ADD-LAES-IN-FAELT                                   
026800                                TO MOD-TIAAMMDD-ATTR(RAD-IX)              
026900         END-IF                                                           
027000                                                                          
027100         IF  MID-KVANTMOT(RAD-IX) = ALL '+'                               
027200             MOVE MFS-RENSA-FAELT TO MOD-KVANTMOT(RAD-IX)                 
027300         ELSE                                                             
027400             MOVE MID-KVANTMOT(RAD-IX) TO MOD-KVANTMOT(RAD-IX)            
027500             MOVE MFS-ADD-LAES-IN-FAELT                                   
027600                                TO MOD-KVANTMOT-ATTR(RAD-IX)              
027700             INSPECT MOD-KVANTMOT(RAD-IX) REPLACING LEADING ZERO          
027800                                         BY SPACE                         
027900         END-IF                                                           
028000                                                                          
028100         IF  MID-KVAVIS(RAD-IX) = ALL '+'                                 
028200             MOVE MFS-RENSA-FAELT TO MOD-KVAVIS(RAD-IX)                   
028300         ELSE                                                             
028400             MOVE MID-KVAVIS(RAD-IX) TO MOD-KVAVIS(RAD-IX)                
028500             MOVE MFS-ADD-LAES-IN-FAELT                                   
028600                                TO MOD-KVAVIS-ATTR(RAD-IX)                
028700             INSPECT MOD-KVAVIS(RAD-IX) REPLACING LEADING ZERO            
028800                                         BY SPACE                         
028900         END-IF                                                           
029000                                                                          
029100         IF  MID-TYP(RAD-IX) = ALL '+'                                    
029200             MOVE MFS-RENSA-FAELT TO MOD-TYP(RAD-IX)                      
029300         ELSE                                                             
029400             MOVE MID-TYP(RAD-IX) TO MOD-TYP(RAD-IX)                      
029500             MOVE MFS-ADD-LAES-IN-FAELT                                   
029600                                TO MOD-TYP-ATTR(RAD-IX)                   
029700         END-IF                                                           
029800                                                                          
029900         ADD +1 TO RAD-IX                                                 
030000     END-PERFORM                                                          
030100     .                                                                    
030200     EJECT                                                                
030300 G-KOLLA-INPUT SECTION.                                                   
030400                                                                          
030500     MOVE JA TO INDATA-SW                                                 
030600     IF MID-W2I32501-CTX = ALL '+'                                        
030700        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
030800        CALL WMEDKONV USING MED-WMEDAREA                                  
030900        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
031000        PERFORM MFS-ROER-EJ-FAELT-IN                                      
031100        MOVE NEJ TO INDATA-SW                                             
031200     ELSE                                                                 
031300        MOVE +1 TO RAD-IX                                                 
031400        PERFORM UNTIL RAD-IX > MAX-RAD                                    
031500                                                                          
031600         IF MID-IDARTNR(RAD-IX) NOT = ALL '+'                             
031700           INSPECT MID-IDARTNR(RAD-IX) REPLACING LEADING SPACE            
031800                                       BY ZERO                            
031900           IF MID-IDARTNR(RAD-IX) NOT NUMERIC                             
032000             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-ATTR(RAD-IX)          
032100             MOVE NEJ TO INDATA-SW                                        
032200           ELSE                                                           
032300             MOVE MFS-ALFA-FAELT-RAETT                                    
032400                                TO MOD-IDARTNR-ATTR(RAD-IX)               
032500             MOVE MID-IDARTNR(RAD-IX) TO WS-IDARTNR                       
032600             IF WS-IDARTNR = ZERO                                         
032700                MOVE MFS-ALFA-FAELT-FEL                                   
032800                                TO MOD-IDARTNR-ATTR(RAD-IX)               
032900                 MOVE NEJ TO INDATA-SW                                    
033000             END-IF                                                       
033100           END-IF                                                         
033200         END-IF                                                           
033300                                                                          
033400         IF MID-IDLEVNR(RAD-IX) NOT = ALL '+'                             
033500            MOVE MFS-ALFA-FAELT-RAETT                                     
033600                               TO MOD-IDLEVNR-ATTR(RAD-IX)                
033700         END-IF                                                           
033800                                                                          
033900         IF MID-TIAAMMDD(RAD-IX) NOT = ALL '+'                            
034000            IF MID-TIAAMMDD(RAD-IX) NOT NUMERIC                           
034100             MOVE MFS-ALFA-FAELT-FEL TO MOD-TIAAMMDD-ATTR(RAD-IX)         
034200             MOVE NEJ TO INDATA-SW                                        
034300            ELSE                                                          
034400               MOVE MID-TIAAMMDD(RAD-IX) TO DAT-I-TIDATUM                 
034410               MOVE 'AAMMDD' TO DAT-KDDATFORM                             
034500               CALL WDATKONV USING DAT-KDDATFORM                          
034600                                   DAT-I-TIDATUM                          
034700                                   DAT-O-TIDATUM                          
034800                                   DAT-KDSVAR                             
034900               IF DAT-KDSVAR-OK                                           
                        IF MID-TIAAMMDD(RAD-IX) > DAGENS-DATUM                  
035300                     MOVE MFS-ALFA-FAELT-FEL                              
035400                               TO MOD-TIAAMMDD-ATTR(RAD-IX)               
035500                     MOVE NEJ TO INDATA-SW                                
                        ELSE                                                    
035000                     MOVE MFS-ALFA-FAELT-RAETT                            
035100                              TO MOD-TIAAMMDD-ATTR(RAD-IX)                
                        END-IF                                                  
035200               ELSE                                                       
035300                  MOVE MFS-ALFA-FAELT-FEL                                 
035400                              TO MOD-TIAAMMDD-ATTR(RAD-IX)                
035500                 MOVE NEJ TO INDATA-SW                                    
035600               END-IF                                                     
035700            END-IF                                                        
035800         END-IF                                                           
035900                                                                          
036000         IF MID-KVANTMOT(RAD-IX) NOT = ALL '+'                            
036100           INSPECT MID-KVANTMOT(RAD-IX) REPLACING LEADING SPACE           
036200                                       BY ZERO                            
036300           IF MID-KVANTMOT(RAD-IX) NOT NUMERIC                            
036400             MOVE MFS-ALFA-FAELT-FEL TO MOD-KVANTMOT-ATTR(RAD-IX)         
036500             MOVE NEJ TO INDATA-SW                                        
036600           ELSE                                                           
037200             MOVE MFS-ALFA-FAELT-RAETT                                    
037300                         TO MOD-KVANTMOT-ATTR(RAD-IX)                     
037500           END-IF                                                         
037600         END-IF                                                           
037700                                                                          
037800         IF MID-KVAVIS(RAD-IX) NOT = ALL '+'                              
037900           INSPECT MID-KVAVIS(RAD-IX) REPLACING LEADING SPACE             
038000                                       BY ZERO                            
038100           IF MID-KVAVIS(RAD-IX) NOT NUMERIC                              
038200             MOVE MFS-ALFA-FAELT-FEL TO MOD-KVAVIS-ATTR(RAD-IX)           
038300             MOVE NEJ TO INDATA-SW                                        
038400           ELSE                                                           
039000             MOVE MFS-ALFA-FAELT-RAETT                                    
039100                       TO MOD-KVAVIS-ATTR(RAD-IX)                         
039300           END-IF                                                         
039400         END-IF                                                           
039500                                                                          
039600         IF MID-TYP(RAD-IX) NOT = ALL '+'                                 
039700           IF MID-TYP(RAD-IX)= 'A' OR 'D'                                 
039800              MOVE MFS-ALFA-FAELT-RAETT                                   
039900                         TO MOD-TYP-ATTR(RAD-IX)                          
040000           ELSE                                                           
040100              MOVE MFS-ALFA-FAELT-FEL                                     
040200                         TO MOD-TYP-ATTR(RAD-IX)                          
040300              MOVE NEJ TO INDATA-SW                                       
040400           END-IF                                                         
040500         END-IF                                                           
040600                                                                          
040700         IF  MID-IDLEVNR(RAD-IX) NOT = ALL '+'                            
040800            IF MID-IDARTNR(RAD-IX) = ALL '+'                              
040900               MOVE MFS-ALFA-FAELT-FEL                                    
041000                           TO MOD-IDARTNR-ATTR(RAD-IX)                    
041100               MOVE NEJ TO INDATA-SW                                      
041200            END-IF                                                        
041300            IF MID-TIAAMMDD(RAD-IX) = ALL '+'                             
041400               MOVE MFS-ALFA-FAELT-FEL                                    
041500                           TO MOD-TIAAMMDD-ATTR(RAD-IX)                   
041600               MOVE NEJ TO INDATA-SW                                      
041700            END-IF                                                        
041800            IF MID-KVANTMOT(RAD-IX) = ALL '+'                             
041900               MOVE MFS-ALFA-FAELT-FEL                                    
042000                           TO MOD-KVANTMOT-ATTR(RAD-IX)                   
042100               MOVE NEJ TO INDATA-SW                                      
042200            END-IF                                                        
042300            IF MID-KVAVIS(RAD-IX) = ALL '+'                               
042400               MOVE MFS-ALFA-FAELT-FEL                                    
042500                           TO MOD-KVAVIS-ATTR(RAD-IX)                     
042600               MOVE NEJ TO INDATA-SW                                      
042700            END-IF                                                        
042800            IF MID-TYP(RAD-IX) = ALL '+'                                  
042900               MOVE MFS-ALFA-FAELT-FEL                                    
043000                           TO MOD-TYP-ATTR(RAD-IX)                        
043100               MOVE NEJ TO INDATA-SW                                      
043200            END-IF                                                        
043300         ELSE                                                             
043400            IF MID-IDARTNR(RAD-IX) NOT = ALL '+'                          
043500               MOVE MFS-ALFA-FAELT-FEL                                    
043600                           TO MOD-IDLEVNR-ATTR(RAD-IX)                    
043700               MOVE NEJ TO INDATA-SW                                      
043800            END-IF                                                        
043900            IF MID-TIAAMMDD(RAD-IX) NOT = ALL '+'                         
044000               MOVE MFS-ALFA-FAELT-FEL                                    
044100                           TO MOD-IDLEVNR-ATTR(RAD-IX)                    
044200               MOVE NEJ TO INDATA-SW                                      
044300            END-IF                                                        
044400            IF MID-KVANTMOT(RAD-IX) NOT = ALL '+'                         
044500               MOVE MFS-ALFA-FAELT-FEL                                    
044600                           TO MOD-IDLEVNR-ATTR(RAD-IX)                    
044700               MOVE NEJ TO INDATA-SW                                      
044800            END-IF                                                        
044900            IF MID-KVAVIS(RAD-IX) NOT = ALL '+'                           
045000               MOVE MFS-ALFA-FAELT-FEL                                    
045100                           TO MOD-IDLEVNR-ATTR(RAD-IX)                    
045200               MOVE NEJ TO INDATA-SW                                      
045300            END-IF                                                        
045400            IF MID-TYP(RAD-IX) NOT = ALL '+'                              
045500               MOVE MFS-ALFA-FAELT-FEL                                    
045600                           TO MOD-IDLEVNR-ATTR(RAD-IX)                    
045700               MOVE NEJ TO INDATA-SW                                      
045800            END-IF                                                        
045900         END-IF                                                           
046000                                                                          
046010         IF MID-W2I325-001-GRP(RAD-IX) =  ALL '+'                         
046020            CONTINUE                                                      
046030         ELSE                                                             
046100            IF INDATA-OK                                                  
046200               MOVE WS-IDARTNR  TO W-IDARTNR                              
046300               PERFORM IMS-GET-WDK601                                     
046400               IF SEGMENT-SAKNAS                                          
046500                  MOVE MFS-ALFA-FAELT-FEL                                 
046600                         TO MOD-IDARTNR-ATTR(RAD-IX)                      
046700                  MOVE NEJ TO INDATA-SW                                   
046800               END-IF                                                     
046900               MOVE MID-IDLEVNR(RAD-IX) TO W-IDLEVNR                      
047000               PERFORM IMS-GET-WDF101                                     
047100               IF SEGMENT-SAKNAS                                          
047200                  MOVE MFS-ALFA-FAELT-FEL                                 
047300                         TO MOD-IDLEVNR-ATTR(RAD-IX)                      
047400                  MOVE NEJ TO INDATA-SW                                   
047500               END-IF                                                     
047510            END-IF                                                        
047600         END-IF                                                           
047800                                                                          
047900         ADD +1 TO RAD-IX                                                 
048000         END-PERFORM                                                      
                                                                                
               IF INDATA-OK                                                     
                  MOVE +1 TO IX1                                                
                  MOVE +2 TO IX2                                                
                             IX-SPAR                                            
                  PERFORM UNTIL IX1 > 14                                        
                     PERFORM UNTIL IX2 > 14                                     
                        IF MID-IDARTNR(IX1) NOT = ALL '+'                       
                           IF MID-IDARTNR(IX2) NOT = ALL '+'                    
                             IF MID-IDARTNR(IX1) = MID-IDARTNR(IX2)             
                             AND MID-IDLEVNR(IX1) = MID-IDLEVNR(IX2)            
                             AND MID-TIAAMMDD(IX1) = MID-TIAAMMDD(IX2)          
                                MOVE MFS-ALFA-FAELT-FEL TO                      
                                     MOD-IDLEVNR-ATTR(IX2)                      
                                     MOD-IDARTNR-ATTR(IX2)                      
                                     MOD-TIAAMMDD-ATTR(IX2)                     
                                MOVE NEJ TO INDATA-SW                           
                             END-IF                                             
                           END-IF                                               
                        END-IF                                                  
                        ADD +1 TO IX2                                           
                     END-PERFORM                                                
                     ADD +1 TO IX1                                              
                     MOVE IX-SPAR TO IX2                                        
                     ADD +1 TO IX2                                              
                               IX-SPAR                                          
                END-PERFORM                                                     
           END-IF                                                               
                                                                                
           IF INDATA-FEL                                                        
              MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                         
              CALL WMEDKONV USING MED-WMEDAREA                                  
              MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
              PERFORM MFS-ROER-EJ-FAELT-IN                                      
           END-IF                                                               
                                                                                
           END-IF                                                               
048800     .                                                                    
048900     EJECT                                                                
049000 H-UPPDATERA SECTION.                                                     
049100                                                                          
049200     MOVE +1 TO RAD-IX                                                    
049300     PERFORM UNTIL RAD-IX > MAX-RAD                                       
049400        IF MID-IDARTNR(RAD-IX) NUMERIC                                    
049500        AND MID-IDARTNR(RAD-IX) > ZERO                                    
049600                                                                          
049700           INSPECT MID-IDARTNR(RAD-IX) REPLACING                          
049800                       LEADING SPACE BY ZERO                              
049900           MOVE MID-IDARTNR(RAD-IX) TO WS-IDARTNR                         
050000                                                                          
050100           INSPECT MID-KVANTMOT(RAD-IX) REPLACING                         
050200                       LEADING SPACE BY ZERO                              
050300           MOVE MID-KVANTMOT(RAD-IX) TO WS-KVANTMOT                       
050400                                                                          
050500           INSPECT MID-KVAVIS(RAD-IX) REPLACING                           
050600                       LEADING SPACE BY ZERO                              
050700           MOVE MID-KVAVIS(RAD-IX) TO WS-KVAVIS                           
050800                                                                          
050900           MOVE MID-TIAAMMDD(RAD-IX) TO WS-TIAAMMDD                       
051000           MOVE MID-TYP(RAD-IX) TO WS-TYP                                 
051100           MOVE MID-IDLEVNR(RAD-IX) TO WS-IDLEVNR                         
051200                                                                          
051300           MOVE WS-IDLEVNR        TO FILC-IDLEVNR                         
051400           MOVE WS-IDARTNR        TO FILC-IDARTNR                         
051500           MOVE WS-TIAAMMDD       TO FILC-TIAAMMDD                        
051600           MOVE WS-KVANTMOT       TO FILC-KVANTMOT                        
051700           MOVE WS-KVAVIS         TO FILC-KVAVIS                          
051800           MOVE WS-TYP            TO FILC-TYP                             
051900           MOVE FILC-W23686       TO FILC-FIL-WDR301-DATA                 
052000           ACCEPT W-TID FROM TIME                                         
052100           IF W-TID = FILC-FIL-TIKLOCK                                    
052200              ADD +1              TO FILC-FIL-IDSEKVNR                    
052300           ELSE                                                           
052400              MOVE W-TID          TO FILC-FIL-TIKLOCK                     
052500              MOVE +1             TO FILC-FIL-IDSEKVNR                    
052600           END-IF                                                         
052700                                                                          
052800           PERFORM IMS-ISRT-WLFILC                                        
052900        END-IF                                                            
052910                                                                          
053000        ADD +1 TO RAD-IX                                                  
053200     END-PERFORM                                                          
053300                                                                          
053400     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
053500     CALL WMEDKONV USING MED-WMEDAREA                                     
053600     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
053700     PERFORM MFS-ROER-EJ-FAELT-IN                                         
053800     PERFORM MFS-STAENG-FAELT-IN                                          
053900     .                                                                    
054000     EJECT                                                                
054100                                                                          
054200 MFS-RENSA-FAELT-IN SECTION.                                              
054300                                                                          
054400     MOVE +1 TO RAD-IX                                                    
054500     PERFORM UNTIL RAD-IX > MAX-RAD                                       
054600        MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR(RAD-IX)                       
054700                                MOD-IDARTNR(RAD-IX)                       
054800                                MOD-TIAAMMDD(RAD-IX)                      
054900                                MOD-KVAVIS(RAD-IX)                        
055000                                MOD-KVANTMOT(RAD-IX)                      
055100                                MOD-TYP(RAD-IX)                           
055200        ADD +1 TO RAD-IX                                                  
055300     END-PERFORM                                                          
055400     .                                                                    
055500     EJECT                                                                
055600                                                                          
055700 MFS-ROER-EJ-FAELT-IN SECTION.                                            
055800                                                                          
055900     MOVE +1 TO RAD-IX                                                    
056000     PERFORM UNTIL RAD-IX > MAX-RAD                                       
056100        MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR(RAD-IX)                     
056200                                  MOD-IDARTNR(RAD-IX)                     
056300                                  MOD-TIAAMMDD(RAD-IX)                    
056400                                  MOD-KVAVIS(RAD-IX)                      
056500                                  MOD-KVANTMOT(RAD-IX)                    
056600                                  MOD-TYP(RAD-IX)                         
056700        ADD +1 TO RAD-IX                                                  
056800     END-PERFORM                                                          
056900     .                                                                    
057000     EJECT                                                                
057100 MFS-STAENG-FAELT-IN SECTION.                                             
057200                                                                          
057300     MOVE +1 TO RAD-IX                                                    
057400     PERFORM UNTIL RAD-IX > MAX-RAD                                       
057500        MOVE MFS-STAENG-FAELT-NOMOD TO MOD-IDLEVNR-ATTR(RAD-IX)           
057600                                       MOD-IDARTNR-ATTR(RAD-IX)           
057700                                       MOD-TIAAMMDD-ATTR(RAD-IX)          
057800                                       MOD-KVAVIS-ATTR(RAD-IX)            
057900                                       MOD-KVANTMOT-ATTR(RAD-IX)          
058000                                       MOD-TYP-ATTR(RAD-IX)               
058100        ADD +1 TO RAD-IX                                                  
058200     END-PERFORM                                                          
058300     .                                                                    
058400     EJECT                                                                
058500                                                                          
058600* --- IMS SEKTIONER ---                                                   
058700     SKIP3                                                                
058800                                                                          
058900 IMS-GET-MSG SECTION.                                                     
059000     MOVE '  QC' TO GODK-STATUSKODER                                      
059100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
059200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
059300     PERFORM IMS-STATUSKONTROLL                                           
059400     .                                                                    
059500     SKIP3                                                                
059600 IMS-INSERT-MSG SECTION.                                                  
059900     MOVE 'N' TO MFS-KDHUVOMR                                             
060200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
060300     MOVE SPACE TO GODK-STATUSKODER                                       
060400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
060500                                                                          
060600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
060700     PERFORM IMS-STATUSKONTROLL                                           
060800     .                                                                    
060900     EJECT                                                                
061000 IMS-GET-WDK601 SECTION.                                                  
061100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
061200          DELIMITED BY SIZE INTO SSA1                                     
061300     MOVE '  GE' TO GODK-STATUSKODER                                      
061400     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
061500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
061600     PERFORM IMS-STATUSKONTROLL                                           
061700     .                                                                    
061800     SKIP3                                                                
061900 IMS-GET-WDF101 SECTION.                                                  
062000     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
062100          DELIMITED BY SIZE INTO SSA1                                     
062200     MOVE '  GE' TO GODK-STATUSKODER                                      
062300     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF101 SSA1                    
062400     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
062500     PERFORM IMS-STATUSKONTROLL                                           
062600     .                                                                    
062700     SKIP3                                                                
062800 IMS-ISRT-WLFILC SECTION.                                                 
062900     STRING 'WLFILC01    '                                                
063000          DELIMITED BY SIZE INTO SSA1                                     
063100     MOVE '   ' TO GODK-STATUSKODER                                       
063200     CALL CBLTDLI USING ISRT WLFILC-PCB DLI-IO-WDR301 SSA1                
063300     MOVE WLFILC-STATUS-CODE TO STATUS-WS                                 
063400     PERFORM IMS-STATUSKONTROLL                                           
063500     .                                                                    
063600     EJECT                                                                
063700 IMS-STATUSKONTROLL SECTION.                                              
063800     SET STATUS-IX TO 1                                                   
063900     SEARCH GODK-STATUS                                                   
064000       AT END                                                             
064100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
064200         DELIMITED BY SIZE INTO FELTEXT                                   
064300         CALL FELLOG                                                      
064400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
064500         CONTINUE                                                         
064600     END-SEARCH                                                           
064700     .                                                                    
