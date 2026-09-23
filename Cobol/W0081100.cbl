000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W0081100.                                                
000400 AUTHOR.         CARINA VIKTORSSON.                                       
000500 DATE-WRITTEN.   MAJ 1988.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
001100*    FUNKTION.                                                            
001200*        RESTORDERSTYRNING 1                                              
001300*        REGISTRERING AV PRIORITET                                        
001400*    INDATA.                                                              
001500*        TRANSAKTION: W0T811                                              
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP3                                                                
002000 DATA DIVISION.                                                           
002100     EJECT                                                                
002200 WORKING-STORAGE SECTION.                                                 
002201                                                                          
002210*    -- CHECKED BY WY2000                                                 
002300 77  PROGRAM-NAMN                PIC X(08)   VALUE 'W0081100'.            
002400 77  JA                          PIC X       VALUE 'J'.                   
002500 77  NEJ                         PIC X       VALUE 'N'.                   
002600 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
002700 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
002800 77  INDX1                       PIC S9(9)   VALUE +0   COMP SYNC.        
002900 77  MAX-MOD-LAENGD              PIC S9(4)   VALUE +508 COMP SYNC.        
003000 77  KDRAPRIO-WS                 PIC X(3)    VALUE SPACE.                 
003100 77  FLAENDR-WS                  PIC X(1)    VALUE SPACE.                 
003200 77  IDDISTRF-WS                 PIC 9(4)    VALUE ZERO.                  
003300 77  IDDISTRT-WS                 PIC 9(4)    VALUE ZERO.                  
003400 77  KVVECKOR-TECK-WS            PIC 9(2)    VALUE ZERO.                  
003500 77  IDDISTRF-SPAR-WS            PIC 9(4)    VALUE ZERO.                  
003600 77  IDDISTRT-SPAR-WS            PIC 9(4)    VALUE ZERO.                  
003700 77  SPAR-KDTPOTYP               PIC S9(1)   COMP-3.                      
003800 77  SPAR-KDORDKL                PIC S9(1)   COMP-3.                      
003900 77  SPAR-IDDISTR-FOM            PIC S9(5)   COMP-3.                      
004000 77  SPAR-IDDISTR-TOM            PIC S9(5)   COMP-3.                      
004100     SKIP2                                                                
004200 01  INDATA-SW                   PIC X.                                   
004300   88  INDATA-OK                             VALUE 'J'.                   
004400   88  INDATA-FEL                            VALUE 'N'.                   
004500 01  PRIO-MAX-SW                 PIC X.                                   
004600   88  PRIO-MAX-OK                           VALUE 'J'.                   
004700 01  START-VISA-SW               PIC X.                                   
004800   88  START-JA                              VALUE 'J'.                   
004900   88  START-NEJ                             VALUE 'N'.                   
005000   SKIP1                                                                  
005100 01  TABELL.                                                              
005200     03  KANTKOD-TABELL              OCCURS 11.                           
005300         05  KANTKOD                   PIC X.                             
005400         05  TPOTYP                    PIC X.                             
005500         05  ORDKL                     PIC X.                             
005600         05  DISTRF                    PIC X.                             
005700         05  DISTRT                    PIC X.                             
005800     EJECT                                                                
005801 01  GENERELLA-SUBPROGRAM.                                                
005802     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
005803     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005810     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005820     EJECT                                                                
005900 01  NYCKLAR-TILL-DLI.                                                    
006000   03  W-IDHTYP-4501-X.                                                   
006100       05  W-IDHTYP-4501         PIC X(4)    VALUE '4501'.                
006200       05  W-LOWVALUE-4501       PIC X(26)   VALUE LOW-VALUE.             
006300                                                                          
006400   03  W-WDGXKEY-4502-X.                                                  
006500     05  W-KDRAPRIO-4502         PIC S9(3)   COMP-3.                      
006600     05  W-LOWVALUE              PIC X(3)    VALUE LOW-VALUE.             
006700                                                                          
006800   03  W-IDHTYP-4511-X.                                                   
006900       05  W-IDHTYP-4511         PIC X(4)    VALUE '4511'.                
007000       05  W-LOWVALUE-4511       PIC X(26)   VALUE LOW-VALUE.             
007100                                                                          
007200   03  W-WDGXKEY-4512-X.                                                  
007300      05  W-KDRAPRIO-4512-X.                                              
007400        07  W-KDRAPRIO-4512         PIC S9(3)   COMP-3.                   
007500      05  W-KDTPOTYP-X.                                                   
007600        07  W-KDTPOTYP              PIC S9(1)   COMP-3.                   
007700      05  W-KDORDKL-X.                                                    
007800        07  W-KDORDKL               PIC S9(1)   COMP-3.                   
007900      05  W-IDDISTR-FOM-X.                                                
008000        07  W-IDDISTR-FOM           PIC S9(5)   COMP-3.                   
008100      05  W-IDDISTR-TOM-X.                                                
008200        07  W-IDDISTR-TOM           PIC S9(5)   COMP-3.                   
008300     EJECT                                                                
008400*    -COPY WWTEXT01                                                       
008600     EJECT                                                                
008700******************************************************************        
008800*                                                                         
008900*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
009000*                                                                         
009100 01  FILLER                      PIC X(16)   VALUE 'MFS-WS'.              
009200     SKIP3                                                                
009300*    -COPY W0I81101                                                       
009500     EJECT                                                                
009600*01  -COPY WMSGAREA                                                       
009800     EJECT                                                                
009900*03  W0O81101 -COPY W0O81101     -RED MSG-AREA                            
010100     EJECT                                                                
010200*01  -COPY WMFSAREA                                                       
010400     EJECT                                                                
010500******************************************************************        
010600*                                                                         
010700*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010800*                                                                         
010900 01  IMS-WS.                                                              
011000   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
011100     SKIP3                                                                
011200*                        **** STATUS-KOD FRÅN IMS                         
011300   03  STATUS-WS                 PIC XX.                                  
011400     88  SEGMENT-FINNS                       VALUE '  '.                  
011500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011700     SKIP3                                                                
011800   03  GODK-STATUSKODER.                                                  
011900     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012000     SKIP3                                                                
012100 01    SSA1                      PIC X(96).                               
012200 01    SSA2                      PIC X(32).                               
012300 01    SSA3                      PIC X(224).                              
012400     EJECT                                                                
012500*                            IMS FUNKTIONSKODER                           
012600*01    -COPY W0003                                                        
012800     EJECT                                                                
012900 01  FILLER                PIC X(16) VALUE 'DLI-IO-AREA '.                
013000 01  DLI-IO-AREA.                                                         
013100   03  IO-AREA                   PIC X(100)  VALUE SPACE.                 
013200     SKIP3                                                                
013300*  03  RLXX -COPY WDGX01      -RED IO-AREA                                
013500     EJECT                                                                
013600*  03  WLXXJM -COPY WDGX4502    -RED IO-AREA                              
013800     EJECT                                                                
013900*  03  WLXXJN -COPY WDGX4512    -RED IO-AREA                              
014100     EJECT                                                                
014200*01 -COPY WDECAREAC0                                                      
014400     EJECT                                                                
014500 LINKAGE SECTION.                                                         
014600*01  -COPY W0009     -PRE MSG-                                            
014800     EJECT                                                                
014900*01  -COPY W0008     -PRE XXJM-                                           
015100     05  FILLER                  PIC X.                                   
015200     EJECT                                                                
015300*01  -COPY W0008     -PRE XXJN-                                           
015500     05  FILLER                  PIC X.                                   
015600     EJECT                                                                
015700 PROCEDURE DIVISION USING MSG-PCB XXJM-PCB XXJN-PCB.                      
015800     ENTRY 'DLITCBL' USING MSG-PCB XXJM-PCB XXJN-PCB.                     
015900                                                                          
016000     PERFORM IMS-GET-MSG                                                  
016100     IF SEGMENT-FINNS                                                     
016200        PERFORM A-INIT                                                    
016300        PERFORM C-KOLLA-NYCKLAR                                           
016400        IF INDATA-OK                                                      
016500           IF MFS-UPDATE                                                  
016600              PERFORM D-KOLLA-FEL                                         
016700              IF INDATA-OK                                                
016800                 PERFORM E-UPPDATERA                                      
016900                 MOVE ZERO                  TO W-KDTPOTYP                 
017000                                               W-KDORDKL                  
017100                                               W-IDDISTR-FOM              
017200                                               W-IDDISTR-TOM              
017300                 MOVE JA TO START-VISA-SW                                 
017400                 PERFORM F-LAES-VISA-INFO                                 
017500              ELSE                                                        
017600                 IF PRIO-MAX-OK                                           
017700                    MOVE TEXT-0409(SPRAK-IX) TO MOD-TEMFSFEL              
017800                    PERFORM MFS-ROR-EJ-FAELT                              
017900                 END-IF                                                   
018000              END-IF                                                      
018100           ELSE                                                           
018200              PERFORM IMS-GU-WLXXJM01-M11                                 
018300              IF SEGMENT-FINNS                                            
018400                 PERFORM G-FLYTTA-PRIOINFO                                
018500                 IF MFS-IDPFK = '7'                                       
018600                    MOVE TEXT-0410(SPRAK-IX) TO MOD-TEMFSINF              
018700                    MOVE JA TO START-VISA-SW                              
018800                 ELSE                                                     
018900                    IF MFS-IDPFK = '8'                                    
019000                       MOVE MID-KDTPOTYP-PF8  TO W-KDTPOTYP               
019100                       MOVE MID-KDORDKL-PF8   TO W-KDORDKL                
019200                       MOVE MID-IDDISTR-FOM-PF8                           
019300                                              TO IDDISTRF-WS              
019400                       MOVE IDDISTRF-WS       TO W-IDDISTR-FOM            
019500                       MOVE MID-IDDISTR-TOM-PF8                           
019600                                              TO IDDISTRT-WS              
019700                       MOVE IDDISTRT-WS       TO W-IDDISTR-TOM            
019800                       MOVE NEJ TO START-VISA-SW                          
019900                    ELSE                                                  
020000                       MOVE MID-KDTPOTYP-ENTER TO W-KDTPOTYP              
020100                       MOVE MID-KDORDKL-ENTER  TO W-KDORDKL               
020200                       MOVE MID-IDDISTR-FOM-ENTER TO                      
020300                                                 IDDISTRF-WS              
020400                       MOVE IDDISTRF-WS TO W-IDDISTR-FOM                  
020500                       MOVE MID-IDDISTR-TOM-ENTER TO                      
020600                                                 IDDISTRT-WS              
020700                       MOVE IDDISTRT-WS TO W-IDDISTR-TOM                  
020800                       MOVE NEJ TO START-VISA-SW                          
020900                    END-IF                                                
021000                    IF W-KDTPOTYP = ZERO AND                              
021100                       W-KDORDKL = ZERO AND                               
021200                       W-IDDISTR-FOM = ZERO AND                           
021300                       W-IDDISTR-TOM = ZERO                               
021400                       MOVE '7' TO MFS-IDPFK                              
021500                       MOVE TEXT-0410(SPRAK-IX) TO MOD-TEMFSINF           
021600                       MOVE JA TO START-VISA-SW                           
021700                    END-IF                                                
021800                 END-IF                                                   
021900                 PERFORM F-LAES-VISA-INFO                                 
022000              ELSE                                                        
022100                 MOVE TEXT-0406(SPRAK-IX) TO MOD-TEMFSFEL                 
022200                 PERFORM MFS-FELHANTERING                                 
022300              END-IF                                                      
022400           END-IF                                                         
022500        END-IF                                                            
022600        MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
022700        PERFORM IMS-INSERT-MSG                                            
022800     END-IF                                                               
022900                                                                          
023000     MOVE ZERO TO RETURN-CODE                                             
023100     GOBACK                                                               
023200     .                                                                    
023300     EJECT                                                                
023400 A-INIT SECTION.                                                          
023500                                                                          
023600     IF MSG-DUBBLA-TRANSKODER                                             
023700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I81101                 
023800       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
023900       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
024000       MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                  
024100       MOVE MSG-IDPFK                     TO MFS-IDPFK                    
024200     ELSE                                                                 
024300       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W0I81101                 
024400       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
024500       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
024600       MOVE SPACE                         TO MFS-KDTRTYP                  
024700                                             MFS-IDPFK                    
024800     END-IF                                                               
024900                                                                          
025000     MOVE LOW-VALUE       TO MSG-AREA                                     
025100     MOVE 'W0O81101'      TO MFS-IDMOD                                    
025200     MOVE '0811'          TO MOD-IDTRANS                                  
025300     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
025400                                                                          
025500     IF MFS-IDTRANS NOT = '0811'                                          
025600       MOVE SPACE         TO MFS-KDTRTYP                                  
025700       MOVE '7'           TO MFS-IDPFK                                    
025800       MOVE MFS-RENSA-FAELT TO MID-KDRAPRIO-IN                            
025900                               MID-KDRAPRIO-IN                            
026000                               MID-FLAENDR-IN                             
026100                               MID-FLAENDR-UT                             
026200     END-IF                                                               
026300     IF MFS-KDMFSFOR = '2'                                                
026400       MOVE +2            TO SPRAK-IX                                     
026500     ELSE                                                                 
026600       MOVE +1            TO SPRAK-IX                                     
026700     END-IF                                                               
026800     .                                                                    
026900     EJECT                                                                
027000 C-KOLLA-NYCKLAR SECTION.                                                 
027100                                                                          
027200     MOVE JA TO INDATA-SW                                                 
027300                                                                          
027400     IF MID-KDRAPRIO-IN = ALL '+'                                         
027500        MOVE MID-KDRAPRIO-UT TO KDRAPRIO-WS                               
027600        INSPECT KDRAPRIO-WS REPLACING LEADING SPACE BY ZERO               
027700                                                                          
027800        IF MFS-IDPFK = SPACE AND MFS-KDTRTYP      = SPACE                 
027900           PERFORM CA-ENTER-IFYLLDA-VARDEN                                
028000           IF INDATA-FEL                                                  
028100               MOVE TEXT-0407(SPRAK-IX) TO MOD-TEMFSINF                   
028200               PERFORM MFS-ROR-EJ-FAELT                                   
028300               PERFORM MFS-LAS-IN-IGEN                                    
028400           END-IF                                                         
028500        END-IF                                                            
028600     ELSE                                                                 
028700        MOVE '7' TO MFS-IDPFK                                             
028800        MOVE MID-KDRAPRIO-IN TO KDRAPRIO-WS                               
028900     END-IF                                                               
029000                                                                          
029100     IF KDRAPRIO-WS NUMERIC AND KDRAPRIO-WS > ZERO                        
029200        MOVE KDRAPRIO-WS TO W-KDRAPRIO-4502 W-KDRAPRIO-4512               
029300        IF MID-FLAENDR-IN = ALL '+'                                       
029400           MOVE SPACE          TO FLAENDR-WS                              
029500        ELSE                                                              
029600           MOVE MID-FLAENDR-IN TO FLAENDR-WS                              
029700        END-IF                                                            
029800        IF FLAENDR-WS NOT = SPACE                                         
029900          IF FLAENDR-WS NOT = 'J'                                         
030000             MOVE NEJ TO INDATA-SW                                        
030100             MOVE TEXT-0401(SPRAK-IX) TO MOD-TEMFSFEL                     
030200             PERFORM MFS-FELHANTERING                                     
030300          END-IF                                                          
030400        END-IF                                                            
030500     ELSE                                                                 
030600        MOVE NEJ TO INDATA-SW                                             
030700        MOVE TEXT-0401(SPRAK-IX) TO MOD-TEMFSFEL                          
030800        PERFORM MFS-FELHANTERING                                          
030900     END-IF                                                               
031000     MOVE KDRAPRIO-WS TO MOD-KDRAPRIO-UT                                  
031100     INSPECT MOD-KDRAPRIO-UT REPLACING LEADING ZERO BY SPACE              
031200                                                                          
031300     MOVE MFS-RENSA-FAELT    TO MOD-KDRAPRIO-IN                           
031400                                MOD-FLAENDR-IN                            
031500                                MOD-FLAENDR-UT                            
031600     .                                                                    
031700     EJECT                                                                
031800 CA-ENTER-IFYLLDA-VARDEN SECTION.                                         
031900     IF MID-BERAPRIO         NOT = ALL '+'                                
032000        OR MID-REROFORD      NOT = ALL '+'                                
032100        OR MID-FLPRIO        NOT = ALL '+'                                
032200        OR MID-KVVECKOR-TECK NOT = ALL '+'                                
032300        OR MID-RELEVFOR      NOT = ALL '+'                                
032400        OR MID-FLAENDR-IN    NOT = ALL '+'                                
032500        MOVE NEJ TO INDATA-SW                                             
032600     END-IF                                                               
032700     MOVE +1 TO INDX                                                      
032800     PERFORM UNTIL INDX = +12                                             
032900        IF MID-KDTPOTYP-RAD(INDX)       NOT = ALL '+'                     
033000           OR MID-KANTKOD-RAD(INDX)     NOT = ALL '+'                     
033100           OR MID-KDORDKL-RAD(INDX)     NOT = ALL '+'                     
033200           OR MID-IDDISTR-FOM-RAD(INDX) NOT = ALL '+'                     
033300           OR MID-IDDISTR-TOM-RAD(INDX) NOT = ALL '+'                     
033400           MOVE NEJ TO INDATA-SW                                          
033500        END-IF                                                            
033600        ADD +1 TO INDX                                                    
033700     END-PERFORM                                                          
033800     .                                                                    
033900     EJECT                                                                
034000 D-KOLLA-FEL SECTION.                                                     
034100                                                                          
034200     MOVE JA  TO INDATA-SW PRIO-MAX-SW                                    
034300     PERFORM IMS-GU-WLXXJM01-M11                                          
034400     IF SEGMENT-SAKNAS                                                    
034500        PERFORM DB-MAX-25-PRIO                                            
034600* NYUPPLÄGGNINGSKONTROLL (HUVUDINFORMATION MÅSTE VARA IFYLLD)             
034700        IF PRIO-MAX-OK                                                    
034800           IF MID-BERAPRIO      = ALL '+' OR                              
034900              MID-REROFORD      = ALL '+' OR                              
035000              MID-FLPRIO        = ALL '+' OR                              
035100              MID-KVVECKOR-TECK = ALL '+' OR                              
035200              MID-RELEVFOR      = ALL '+'                                 
035300              MOVE TEXT-0401(SPRAK-IX) TO MOD-TEMFSFEL                    
035400              MOVE NEJ TO PRIO-MAX-SW INDATA-SW                           
035500              PERFORM MFS-FELHANTERING                                    
035600           ELSE                                                           
035700              PERFORM DA-KOLLA-FEL-FALT                                   
035800           END-IF                                                         
035900        ELSE                                                              
036000           MOVE NEJ TO INDATA-SW                                          
036100           MOVE TEXT-0411(SPRAK-IX) TO MOD-TEMFSFEL                       
036200           PERFORM MFS-FELHANTERING                                       
036300        END-IF                                                            
036400     ELSE                                                                 
036500        PERFORM DA-KOLLA-FEL-FALT                                         
036600     END-IF                                                               
036700      IF PRIO-MAX-OK                                                      
036800         MOVE +1 TO INDX                                                  
036900         PERFORM UNTIL INDX = +12                                         
037000          IF MID-KANTKOD-RAD(INDX)     = ALL '+' AND                      
037100             MID-KDTPOTYP-RAD(INDX)    = ALL '+' AND                      
037200             MID-KDORDKL-RAD(INDX)     = ALL '+' AND                      
037300             MID-IDDISTR-FOM-RAD(INDX) = ALL '+' AND                      
037400             MID-IDDISTR-TOM-RAD(INDX) = ALL '+'                          
037500             MOVE MFS-NUM-FAELT-RAETT TO                                  
037600                                  MOD-KDTPOTYP-RAD-ATTR(INDX)             
037700                                  MOD-KDORDKL-RAD-ATTR(INDX)              
037800                                  MOD-IDDISTR-FOM-RAD-ATTR(INDX)          
037900                                  MOD-IDDISTR-TOM-RAD-ATTR(INDX)          
038000             MOVE MFS-ALFA-FAELT-RAETT TO                                 
038100                                  MOD-KANTKOD-RAD-ATTR(INDX)              
038200             MOVE 'E' TO KANTKOD(INDX)                                    
038300          ELSE                                                            
038400             IF MID-KANTKOD-RAD(INDX)      = 'N' AND                      
038500                MID-KDTPOTYP-SPAR(INDX)    = ZERO AND                     
038600                MID-KDORDKL-SPAR(INDX)     = ZERO AND                     
038700                MID-IDDISTR-FOM-SPAR(INDX) = ZERO AND                     
038800                MID-IDDISTR-TOM-SPAR(INDX) = ZERO                         
038900                MOVE 'N' TO KANTKOD(INDX)                                 
039000                MOVE MFS-ALFA-FAELT-RAETT TO                              
039100                                    MOD-KANTKOD-RAD-ATTR(INDX)            
039200             ELSE                                                         
039300                IF MID-KANTKOD-RAD(INDX)       = 'Ä' AND                  
039400                   MID-IDDISTR-FOM-SPAR(INDX)  NOT = ZERO AND             
039500                   MID-IDDISTR-TOM-SPAR(INDX)  NOT = ZERO                 
039600                   MOVE 'Ä' TO KANTKOD(INDX)                              
039700                   MOVE MFS-ALFA-FAELT-RAETT TO                           
039800                        MOD-KANTKOD-RAD-ATTR(INDX)                        
039900                ELSE                                                      
040000                   IF MID-KANTKOD-RAD(INDX)      = 'B'                    
040100                      MOVE 'B' TO KANTKOD(INDX)                           
040200                      MOVE MFS-ALFA-FAELT-RAETT TO                        
040300                           MOD-KANTKOD-RAD-ATTR(INDX)                     
040400                   ELSE                                                   
040500                      IF MID-KANTKOD-RAD(INDX) NOT = ALL '+'              
040600                         MOVE 'F' TO KANTKOD(INDX)                        
040700                         MOVE MFS-ALFA-FAELT-FEL TO                       
040800                              MOD-KANTKOD-RAD-ATTR(INDX)                  
040900                         MOVE NEJ TO INDATA-SW                            
041000                      END-IF                                              
041100                   END-IF                                                 
041200                END-IF                                                    
041300             END-IF                                                       
041400          END-IF                                                          
041500          IF MID-KDTPOTYP-RAD(INDX) NOT = ALL '+'                         
041600             IF MID-KDTPOTYP-RAD(INDX) NUMERIC AND                        
041700                MID-KDTPOTYP-RAD(INDX) >= 0    AND                        
041800                MID-KDTPOTYP-RAD(INDX) <= 6                               
041900                 MOVE MFS-NUM-FAELT-RAETT TO                              
042000                     MOD-KDTPOTYP-RAD-ATTR(INDX)                          
042100                 MOVE JA TO TPOTYP(INDX)                                  
042200             ELSE                                                         
042300                 MOVE MFS-NUM-FAELT-FEL TO                                
042400                     MOD-KDTPOTYP-RAD-ATTR(INDX)                          
042500                 MOVE NEJ TO INDATA-SW TPOTYP(INDX)                       
042600             END-IF                                                       
042700          ELSE                                                            
042800             IF KANTKOD(INDX) = 'N'                                       
042900               MOVE MFS-NUM-FAELT-FEL TO                                  
043000                 MOD-KDTPOTYP-RAD-ATTR(INDX)                              
043100               MOVE NEJ TO INDATA-SW TPOTYP(INDX)                         
043200             ELSE                                                         
043300               MOVE 'E' TO TPOTYP(INDX)                                   
043400             END-IF                                                       
043500          END-IF                                                          
043600          IF MID-KDORDKL-RAD(INDX) NOT = ALL '+'                          
043700             IF MID-KDORDKL-RAD(INDX) NUMERIC AND                         
043800                MID-KDORDKL-RAD(INDX) < 6                                 
043900                 MOVE MFS-NUM-FAELT-RAETT TO                              
044000                     MOD-KDORDKL-RAD-ATTR(INDX)                           
044100                 MOVE JA TO ORDKL(INDX)                                   
044200             ELSE                                                         
044300                 MOVE MFS-NUM-FAELT-FEL TO                                
044400                     MOD-KDORDKL-RAD-ATTR(INDX)                           
044500                 MOVE NEJ TO INDATA-SW ORDKL(INDX)                        
044600             END-IF                                                       
044700          ELSE                                                            
044800             IF KANTKOD(INDX) = 'N'                                       
044900               MOVE MFS-NUM-FAELT-FEL TO                                  
045000                 MOD-KDORDKL-RAD-ATTR(INDX)                               
045100               MOVE NEJ TO INDATA-SW ORDKL(INDX)                          
045200             ELSE                                                         
045300               MOVE 'E' TO ORDKL(INDX)                                    
045400             END-IF                                                       
045500          END-IF                                                          
045600          MOVE JA TO DISTRF(INDX) DISTRT(INDX)                            
045700          IF MID-IDDISTR-FOM-RAD(INDX) NOT = ALL '+'                      
045800             INSPECT MID-IDDISTR-FOM-RAD(INDX) REPLACING                  
045900              LEADING SPACE BY ZERO                                       
046000             IF MID-IDDISTR-FOM-RAD(INDX) NOT NUMERIC                     
046100                MOVE NEJ TO INDATA-SW DISTRF(INDX)                        
046200                MOVE MFS-NUM-FAELT-FEL TO                                 
046300                     MOD-IDDISTR-FOM-RAD-ATTR(INDX)                       
046400             ELSE                                                         
046500                MOVE MFS-NUM-FAELT-RAETT TO                               
046600                     MOD-IDDISTR-FOM-RAD-ATTR(INDX)                       
046700             END-IF                                                       
046800           END-IF                                                         
046900           IF MID-IDDISTR-TOM-RAD(INDX) NOT = ALL '+'                     
047000             INSPECT MID-IDDISTR-TOM-RAD(INDX) REPLACING                  
047100              LEADING SPACE BY ZERO                                       
047200              IF MID-IDDISTR-TOM-RAD(INDX) NOT NUMERIC                    
047300                 MOVE NEJ TO INDATA-SW DISTRT(INDX)                       
047400                 MOVE MFS-NUM-FAELT-FEL TO                                
047500                      MOD-IDDISTR-TOM-RAD-ATTR(INDX)                      
047600              ELSE                                                        
047700                 MOVE MFS-NUM-FAELT-RAETT TO                              
047800                      MOD-IDDISTR-TOM-RAD-ATTR(INDX)                      
047900              END-IF                                                      
048000           END-IF                                                         
048100           IF DISTRF(INDX) = JA AND DISTRT(INDX) = JA                     
048200              PERFORM DC-KOLLA-DISTRIKTS-INTERVALL                        
048300           END-IF                                                         
048400           ADD +1 TO INDX                                                 
048500         END-PERFORM                                                      
048600      END-IF                                                              
048700      .                                                                   
048800      EJECT                                                               
048900 DA-KOLLA-FEL-FALT SECTION.                                               
049000                                                                          
049100     IF MID-BERAPRIO NOT = ALL '+'                                        
049200       MOVE MFS-ALFA-FAELT-RAETT TO MOD-BERAPRIO-ATTR                     
049300     END-IF                                                               
049400     IF MID-REROFORD NOT = ALL '+'                                        
049500        INSPECT MID-REROFORD REPLACING LEADING SPACE BY ZERO              
049600        IF MID-REROFORD NOT NUMERIC                                       
049700           MOVE MFS-NUM-FAELT-FEL TO MOD-REROFORD-ATTR                    
049800           MOVE NEJ TO INDATA-SW                                          
049900        ELSE                                                              
050000          IF MID-REROFORD > ZERO                                          
050100           MOVE MFS-NUM-FAELT-RAETT TO MOD-REROFORD-ATTR                  
050200          ELSE                                                            
050300           MOVE MFS-NUM-FAELT-FEL TO MOD-REROFORD-ATTR                    
050400           MOVE NEJ TO INDATA-SW                                          
050500          END-IF                                                          
050600        END-IF                                                            
050700     END-IF                                                               
050800     IF MID-FLPRIO NOT = ALL '+'                                          
050900        IF NOT (MID-FLPRIO = 'J' OR 'N')                                  
051000           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLPRIO-ATTR                     
051100           MOVE NEJ TO INDATA-SW                                          
051200        ELSE                                                              
051300           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLPRIO-ATTR                   
051400        END-IF                                                            
051500     END-IF                                                               
051600     IF MID-KVVECKOR-TECK NOT = ALL '+'                                   
051700        INSPECT MID-KVVECKOR-TECK REPLACING LEADING SPACE BY ZERO         
051800        IF MID-KVVECKOR-TECK NOT NUMERIC                                  
051900           MOVE MFS-NUM-FAELT-FEL   TO MOD-KVVECKOR-TECK-ATTR             
052000           MOVE NEJ TO INDATA-SW                                          
052100        ELSE                                                              
052200           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVVECKOR-TECK-ATTR             
052300        END-IF                                                            
052400     END-IF                                                               
052500     IF MID-RELEVFOR NOT = ALL '+'                                        
052600        MOVE MID-RELEVFOR TO DEC-IDFRIDATA                                
052700        MOVE +1 TO DEC-KVHELTAL                                           
052800        MOVE +2 TO DEC-KVDECIMAL                                          
052900        CALL WDECEDIT USING DEC-WDECAREA                                  
053000        IF DEC-KDSVAR-FEL                                                 
053100           MOVE MFS-NUM-FAELT-FEL   TO MOD-RELEVFOR-ATTR                  
053200           MOVE NEJ TO INDATA-SW                                          
053300        ELSE                                                              
053400           MOVE MFS-NUM-FAELT-RAETT TO MOD-RELEVFOR-ATTR                  
053500        END-IF                                                            
053600     END-IF                                                               
053700     .                                                                    
053800     EJECT                                                                
053900 DB-MAX-25-PRIO SECTION.                                                  
054000                                                                          
054100     PERFORM IMS-GU-WLXXJM01                                              
054200     MOVE +0 TO INDX                                                      
054300     PERFORM IMS-GNP-WLXXJM11                                             
054400                                                                          
054500     PERFORM UNTIL SEGMENT-SAKNAS                                         
054600        ADD +1 TO INDX                                                    
054700        PERFORM IMS-GNP-WLXXJM11                                          
054800     END-PERFORM                                                          
054900                                                                          
055000     IF INDX > +24                                                        
055100        MOVE NEJ TO PRIO-MAX-SW                                           
055200     END-IF                                                               
055300     .                                                                    
055400     EJECT                                                                
055500 DC-KOLLA-DISTRIKTS-INTERVALL SECTION.                                    
055600                                                                          
055700     MOVE MID-IDDISTR-FOM-RAD(INDX) TO IDDISTRF-WS                        
055800     MOVE MID-IDDISTR-TOM-RAD(INDX) TO IDDISTRT-WS                        
055900     MOVE MID-IDDISTR-FOM-SPAR(INDX) TO IDDISTRF-SPAR-WS                  
056000     MOVE MID-IDDISTR-TOM-SPAR(INDX) TO IDDISTRT-SPAR-WS                  
056100                                                                          
056200     IF KANTKOD(INDX) = 'N' OR 'Ä' OR 'F'                                 
056300        IF MID-KDTPOTYP-RAD(INDX) NOT = ALL '+' AND                       
056400           TPOTYP(INDX) = JA                                              
056500           MOVE MID-KDTPOTYP-RAD(INDX)  TO W-KDTPOTYP                     
056600        ELSE                                                              
056700           MOVE MID-KDTPOTYP-SPAR(INDX) TO W-KDTPOTYP                     
056800        END-IF                                                            
056900        IF MID-KDORDKL-RAD(INDX) NOT = ALL '+' AND                        
057000           ORDKL(INDX) = JA                                               
057100           MOVE MID-KDORDKL-RAD(INDX)  TO W-KDORDKL                       
057200        ELSE                                                              
057300           MOVE MID-KDORDKL-SPAR(INDX) TO W-KDORDKL                       
057400        END-IF                                                            
057500*NYUPPLÄGGNING ENDAST DISTR.FOM IFYLLT                                    
057600        IF KANTKOD(INDX) = 'N' AND                                        
057700           MID-IDDISTR-FOM-RAD(INDX)  NOT = ALL '+' AND                   
057800           DISTRF(INDX) = JA AND                                          
057900           MID-IDDISTR-TOM-RAD(INDX)  = ALL '+'                           
058000           MOVE IDDISTRF-WS               TO W-IDDISTR-FOM                
058100                                             W-IDDISTR-TOM                
058200        ELSE                                                              
058300*BÅDE DISTR.FOM OCH DISTR.TOM IFYLLDA                                     
058400           IF MID-IDDISTR-FOM-RAD(INDX) NOT = ALL '+' AND                 
058500              DISTRF(INDX) = JA AND                                       
058600              MID-IDDISTR-TOM-RAD(INDX) NOT = ALL '+' AND                 
058700              DISTRT(INDX) = JA                                           
058800              MOVE IDDISTRF-WS               TO W-IDDISTR-FOM             
058900              MOVE IDDISTRT-WS               TO W-IDDISTR-TOM             
059000           ELSE                                                           
059100*BARA DISTR.FOM IFYLLD                                                    
059200              IF MID-IDDISTR-FOM-RAD(INDX) NOT = ALL '+' AND              
059300                 DISTRF(INDX) = JA                                        
059400                 MOVE IDDISTRF-WS            TO W-IDDISTR-FOM             
059500                 MOVE IDDISTRT-SPAR-WS       TO W-IDDISTR-TOM             
059600              ELSE                                                        
059700*BARA DISTR.TOM IFYLLD                                                    
059800                 IF MID-IDDISTR-TOM-RAD(INDX) NOT = ALL '+' AND           
059900                    DISTRT(INDX) = JA                                     
060000                    MOVE IDDISTRT-WS            TO W-IDDISTR-TOM          
060100                    MOVE IDDISTRF-SPAR-WS       TO W-IDDISTR-FOM          
060200                 ELSE                                                     
060300                    MOVE IDDISTRF-SPAR-WS       TO W-IDDISTR-FOM          
060400                    MOVE IDDISTRT-SPAR-WS       TO W-IDDISTR-TOM          
060500                 END-IF                                                   
060600              END-IF                                                      
060700           END-IF                                                         
060800        END-IF                                                            
060900        IF W-IDDISTR-FOM > W-IDDISTR-TOM                                  
061000           MOVE MFS-NUM-FAELT-FEL TO                                      
061100                                  MOD-IDDISTR-FOM-RAD-ATTR(INDX)          
061200                                  MOD-IDDISTR-TOM-RAD-ATTR(INDX)          
061300           MOVE NEJ TO INDATA-SW DISTRF(INDX) DISTRT(INDX)                
061400        ELSE                                                              
061500           PERFORM IMS-GU-WLXXJN11                                        
061600           IF SEGMENT-FINNS                                               
061700              IF MID-KDTPOTYP-SPAR(INDX) = 4512-KDTPOTYP AND              
061800                 MID-KDORDKL-SPAR(INDX)  = 4512-KDORDKL AND               
061900                 IDDISTRF-SPAR-WS        = 4512-IDDISTR-FOM AND           
062000                 IDDISTRT-SPAR-WS        = 4512-IDDISTR-TOM               
062100                 MOVE MFS-NUM-FAELT-RAETT TO                              
062200                                   MOD-IDDISTR-FOM-RAD-ATTR(INDX)         
062300                                   MOD-IDDISTR-TOM-RAD-ATTR(INDX)         
062400                 MOVE JA TO DISTRF(INDX) DISTRT(INDX)                     
062500                 MOVE +1 TO INDX1                                         
062600                 PERFORM UNTIL INDX1 = +12 OR INDX1 = INDX                
062700                   PERFORM DCA-INSLAGNA-DISTRIKT                          
062800                   ADD +1 TO INDX1                                        
062900                 END-PERFORM                                              
063000              ELSE                                                        
063100                 MOVE MFS-NUM-FAELT-FEL TO                                
063200                                   MOD-IDDISTR-FOM-RAD-ATTR(INDX)         
063300                                   MOD-IDDISTR-TOM-RAD-ATTR(INDX)         
063400                 MOVE NEJ TO INDATA-SW DISTRF(INDX) DISTRT(INDX)          
063500              END-IF                                                      
063600           ELSE                                                           
063700              MOVE +1 TO INDX1                                            
063800              PERFORM UNTIL INDX1 = +12 OR INDX1 = INDX                   
063900                PERFORM DCA-INSLAGNA-DISTRIKT                             
064000                ADD +1 TO INDX1                                           
064100              END-PERFORM                                                 
064200           END-IF                                                         
064300        END-IF                                                            
064400     ELSE                                                                 
064500        MOVE MFS-NUM-FAELT-RAETT TO                                       
064600                     MOD-IDDISTR-FOM-RAD-ATTR(INDX)                       
064700                     MOD-IDDISTR-TOM-RAD-ATTR(INDX)                       
064800        MOVE JA TO DISTRF(INDX) DISTRT(INDX)                              
064900     END-IF                                                               
065000     .                                                                    
065100     EJECT                                                                
065200 DCA-INSLAGNA-DISTRIKT SECTION.                                           
065300                                                                          
065400     MOVE MID-IDDISTR-FOM-RAD(INDX1) TO IDDISTRF-WS                       
065500     MOVE MID-IDDISTR-TOM-RAD(INDX1) TO IDDISTRT-WS                       
065600     MOVE MID-IDDISTR-FOM-SPAR(INDX1) TO IDDISTRF-SPAR-WS                 
065700     MOVE MID-IDDISTR-TOM-SPAR(INDX1) TO IDDISTRT-SPAR-WS                 
065800     IF KANTKOD(INDX1) = 'N' OR 'Ä' OR 'F'                                
065900        IF MID-KDTPOTYP-RAD(INDX1) NOT = ALL '+' AND                      
066000        TPOTYP(INDX1) = JA                                                
066100           MOVE MID-KDTPOTYP-RAD(INDX1)  TO SPAR-KDTPOTYP                 
066200        ELSE                                                              
066300           MOVE MID-KDTPOTYP-SPAR(INDX1) TO SPAR-KDTPOTYP                 
066400        END-IF                                                            
066500        IF MID-KDORDKL-RAD(INDX1) NOT = ALL '+' AND                       
066600        ORDKL(INDX1) = JA                                                 
066700           MOVE MID-KDORDKL-RAD(INDX1)  TO SPAR-KDORDKL                   
066800        ELSE                                                              
066900           MOVE MID-KDORDKL-SPAR(INDX1) TO SPAR-KDORDKL                   
067000        END-IF                                                            
067100*NYUPPLÄGGNING ENDAST DISTR.FOM IFYLLT                                    
067200          IF KANTKOD(INDX1) = 'N' AND                                     
067300             MID-IDDISTR-TOM-RAD(INDX1)  = ALL '+' AND                    
067400             DISTRF(INDX1) = JA                                           
067500             MOVE IDDISTRF-WS                TO SPAR-IDDISTR-FOM          
067600                                                SPAR-IDDISTR-TOM          
067700          ELSE                                                            
067800*BÅDE DISTR.FOM OCH DISTR.TOM IFYLLDA                                     
067900             IF MID-IDDISTR-FOM-RAD(INDX1) NOT = ALL '+' AND              
068000                DISTRF(INDX1) = JA AND                                    
068100                MID-IDDISTR-TOM-RAD(INDX1) NOT = ALL '+' AND              
068200                DISTRT(INDX1) = JA                                        
068300                MOVE IDDISTRF-WS  TO SPAR-IDDISTR-FOM                     
068400                MOVE IDDISTRT-WS  TO SPAR-IDDISTR-TOM                     
068500             ELSE                                                         
068600*BARA DISTR.FOM IFYLLD                                                    
068700                IF MID-IDDISTR-FOM-RAD(INDX1) NOT = ALL '+' AND           
068800                   DISTRF(INDX1) = JA                                     
068900                   MOVE IDDISTRF-WS  TO SPAR-IDDISTR-FOM                  
069000                   MOVE IDDISTRT-SPAR-WS TO SPAR-IDDISTR-TOM              
069100                ELSE                                                      
069200*BARA DISTR.TOM IFYLLD                                                    
069300                   IF MID-IDDISTR-TOM-RAD(INDX1) NOT = ALL '+'            
069400                      AND DISTRT(INDX1) = JA                              
069500                      MOVE IDDISTRT-WS TO SPAR-IDDISTR-TOM                
069600                      MOVE IDDISTRF-SPAR-WS TO SPAR-IDDISTR-FOM           
069700                   ELSE                                                   
069800                      MOVE IDDISTRF-SPAR-WS TO SPAR-IDDISTR-FOM           
069900                      MOVE IDDISTRT-SPAR-WS TO SPAR-IDDISTR-TOM           
070000                   END-IF                                                 
070100                END-IF                                                    
070200             END-IF                                                       
070300          END-IF                                                          
070400          IF SPAR-KDTPOTYP = W-KDTPOTYP                                   
070500             IF SPAR-KDORDKL = W-KDORDKL                                  
070600                IF (SPAR-IDDISTR-FOM < W-IDDISTR-FOM OR                   
070700                   SPAR-IDDISTR-FOM = W-IDDISTR-FOM) AND                  
070800                   (SPAR-IDDISTR-TOM = W-IDDISTR-FOM OR                   
070900                   SPAR-IDDISTR-TOM > W-IDDISTR-FOM)                      
071000                      MOVE MFS-NUM-FAELT-FEL TO                           
071100                           MOD-IDDISTR-FOM-RAD-ATTR(INDX)                 
071200                           MOD-IDDISTR-TOM-RAD-ATTR(INDX)                 
071300                      MOVE NEJ TO INDATA-SW                               
071400                                  DISTRF(INDX) DISTRT(INDX)               
071500                ELSE                                                      
071600                   IF (SPAR-IDDISTR-TOM = W-IDDISTR-TOM OR                
071700                      SPAR-IDDISTR-TOM > W-IDDISTR-TOM) AND               
071800                      (SPAR-IDDISTR-FOM < W-IDDISTR-TOM OR                
071900                      SPAR-IDDISTR-FOM = W-IDDISTR-TOM)                   
072000                         MOVE MFS-NUM-FAELT-FEL TO                        
072100                              MOD-IDDISTR-FOM-RAD-ATTR(INDX)              
072200                              MOD-IDDISTR-TOM-RAD-ATTR(INDX)              
072300                         MOVE NEJ TO INDATA-SW                            
072400                                  DISTRF(INDX) DISTRT(INDX)               
072500                   ELSE                                                   
072600                      IF (SPAR-IDDISTR-FOM = W-IDDISTR-FOM OR             
072700                         SPAR-IDDISTR-FOM > W-IDDISTR-FOM) AND            
072800                         (SPAR-IDDISTR-TOM = W-IDDISTR-TOM OR             
072900                         SPAR-IDDISTR-TOM < W-IDDISTR-TOM)                
073000                            MOVE MFS-NUM-FAELT-FEL TO                     
073100                                 MOD-IDDISTR-FOM-RAD-ATTR(INDX)           
073200                                 MOD-IDDISTR-TOM-RAD-ATTR(INDX)           
073300                            MOVE NEJ TO INDATA-SW                         
073400                                 DISTRF(INDX) DISTRT(INDX)                
073500                      ELSE                                                
073600                         MOVE MFS-NUM-FAELT-RAETT TO                      
073700                              MOD-IDDISTR-FOM-RAD-ATTR(INDX)              
073800                              MOD-IDDISTR-TOM-RAD-ATTR(INDX)              
073900                         MOVE JA TO DISTRF(INDX) DISTRT(INDX)             
074000                      END-IF                                              
074100                   END-IF                                                 
074200                END-IF                                                    
074300             ELSE                                                         
074400                MOVE MFS-NUM-FAELT-RAETT TO                               
074500                             MOD-IDDISTR-FOM-RAD-ATTR(INDX)               
074600                             MOD-IDDISTR-TOM-RAD-ATTR(INDX)               
074700                MOVE JA TO DISTRF(INDX) DISTRT(INDX)                      
074800             END-IF                                                       
074900          ELSE                                                            
075000             MOVE MFS-NUM-FAELT-RAETT TO                                  
075100                          MOD-IDDISTR-FOM-RAD-ATTR(INDX)                  
075200                          MOD-IDDISTR-TOM-RAD-ATTR(INDX)                  
075300             MOVE JA TO DISTRF(INDX) DISTRT(INDX)                         
075400          END-IF                                                          
075500     ELSE                                                                 
075600        MOVE MFS-NUM-FAELT-RAETT TO                                       
075700                     MOD-IDDISTR-FOM-RAD-ATTR(INDX)                       
075800                     MOD-IDDISTR-TOM-RAD-ATTR(INDX)                       
075900        MOVE JA TO DISTRF(INDX) DISTRT(INDX)                              
076000     END-IF                                                               
076100     .                                                                    
076200     EJECT                                                                
076300 E-UPPDATERA   SECTION.                                                   
076400                                                                          
076500* BORTTAG AV PRIORITETSRADER                                              
076600     IF FLAENDR-WS = 'J'                                                  
076700        PERFORM IMS-GU-WLXXJN01                                           
076800        PERFORM IMS-GHNP-WLXXJN11                                         
076900        PERFORM UNTIL SEGMENT-SAKNAS                                      
077000           PERFORM IMS-DLET-WLXXJN11                                      
077100           PERFORM IMS-GHNP-WLXXJN11                                      
077200        END-PERFORM                                                       
077300        PERFORM MFS-ROR-EJ-FAELT-HUVUD                                    
077400     ELSE                                                                 
077500* UPPDATERING AV ENSKILDA FÄLT                                            
077600        PERFORM IMS-GHU-WLXXJM11                                          
077700        IF SEGMENT-FINNS                                                  
077800           IF MID-BERAPRIO NOT = ALL '+'                                  
077900              MOVE MID-BERAPRIO TO 4502-BERAPRIO MOD-BERAPRIO             
078000           ELSE                                                           
078100              MOVE MFS-ROER-EJ-FAELT TO MOD-BERAPRIO                      
078200           END-IF                                                         
078300           IF MID-REROFORD NOT = ALL '+'                                  
078400              MOVE MID-REROFORD TO 4502-REROFORD MOD-REROFORD             
078500           ELSE                                                           
078600              MOVE MFS-ROER-EJ-FAELT TO MOD-REROFORD                      
078700           END-IF                                                         
078800           IF MID-FLPRIO NOT = ALL '+'                                    
078900              MOVE MID-FLPRIO TO 4502-FLPRIO MOD-FLPRIO                   
079000           ELSE                                                           
079100              MOVE MFS-ROER-EJ-FAELT TO MOD-FLPRIO                        
079200           END-IF                                                         
079300           IF MID-KVVECKOR-TECK NOT = ALL '+'                             
079400              MOVE MID-KVVECKOR-TECK TO KVVECKOR-TECK-WS                  
079500              MOVE KVVECKOR-TECK-WS  TO 4502-KVVECKOR-TECK                
079600                                        MOD-KVVECKOR-TECK                 
079700           ELSE                                                           
079800              MOVE MFS-ROER-EJ-FAELT TO MOD-KVVECKOR-TECK                 
079900           END-IF                                                         
080000           IF MID-RELEVFOR NOT = ALL '+'                                  
080100              MOVE DEC-IDEDITDATA TO 4502-RELEVFOR MOD-RELEVFOR           
080200           ELSE                                                           
080300              MOVE MFS-ROER-EJ-FAELT TO MOD-RELEVFOR                      
080400           END-IF                                                         
080500           PERFORM IMS-REPL-WLXXJM11                                      
080600        ELSE                                                              
080700* NYUPPLÄGGNING AV WLXXJM11 - SJÄLVA PRIORITETEN                          
080800           MOVE KDRAPRIO-WS       TO 4502-KDRAPRIO                        
080900           MOVE LOW-VALUE         TO 4502-LOWVALUE                        
081000           MOVE MID-BERAPRIO      TO 4502-BERAPRIO                        
081100                                     MOD-BERAPRIO                         
081200           MOVE MID-REROFORD      TO 4502-REROFORD                        
081300                                     MOD-REROFORD                         
081400           MOVE MID-FLPRIO        TO 4502-FLPRIO                          
081500                                     MOD-FLPRIO                           
081600           MOVE MID-KVVECKOR-TECK TO KVVECKOR-TECK-WS                     
081700           MOVE KVVECKOR-TECK-WS  TO 4502-KVVECKOR-TECK                   
081800                                     MOD-KVVECKOR-TECK                    
081900           MOVE DEC-IDEDITDATA    TO 4502-RELEVFOR                        
082000                                     MOD-RELEVFOR                         
082100           PERFORM IMS-ISRT-WLXXJM11                                      
082200        END-IF                                                            
082300        MOVE +1 TO INDX                                                   
082400        PERFORM UNTIL INDX = +12                                          
082500           PERFORM EA-UPPDATERA-RAD                                       
082600           ADD +1 TO INDX                                                 
082700        END-PERFORM                                                       
082800     END-IF                                                               
082900     MOVE TEXT-0404(SPRAK-IX) TO MOD-TEMFSINF                             
083000     PERFORM MFS-FORM-ATTR                                                
083100     .                                                                    
083200     EJECT                                                                
083300 EA-UPPDATERA-RAD SECTION.                                                
083400                                                                          
083500     MOVE MID-IDDISTR-FOM-RAD(INDX) TO IDDISTRF-WS                        
083600     MOVE MID-IDDISTR-TOM-RAD(INDX) TO IDDISTRT-WS                        
083700     MOVE MID-IDDISTR-FOM-SPAR(INDX) TO IDDISTRF-SPAR-WS                  
083800     MOVE MID-IDDISTR-TOM-SPAR(INDX) TO IDDISTRT-SPAR-WS                  
083900                                                                          
084000* BORTTAG AV PRIORITETSRAD                                                
084100     IF KANTKOD(INDX) = 'B'                                               
084200        MOVE MID-KDTPOTYP-SPAR(INDX)    TO W-KDTPOTYP                     
084300        MOVE MID-KDORDKL-SPAR(INDX)     TO W-KDORDKL                      
084400        MOVE IDDISTRF-SPAR-WS           TO W-IDDISTR-FOM                  
084500        MOVE IDDISTRT-SPAR-WS           TO W-IDDISTR-TOM                  
084600        PERFORM IMS-GHU-WLXXJN11                                          
084700        IF SEGMENT-FINNS                                                  
084800           PERFORM IMS-DLET-WLXXJN11                                      
084900        END-IF                                                            
085000     ELSE                                                                 
085100* NYUPPLÄGGNING AV PRIORITETSRAD                                          
085200        IF KANTKOD(INDX) = 'N'                                            
085300           MOVE KDRAPRIO-WS               TO 4512-KDRAPRIO                
085400           MOVE MID-KDTPOTYP-RAD(INDX)    TO 4512-KDTPOTYP                
085500           MOVE MID-KDORDKL-RAD(INDX)     TO 4512-KDORDKL                 
085600           MOVE IDDISTRF-WS               TO 4512-IDDISTR-FOM             
085700           IF MID-IDDISTR-TOM-RAD(INDX) = ALL '+'                         
085800              MOVE IDDISTRF-WS            TO 4512-IDDISTR-TOM             
085900           ELSE                                                           
086000              MOVE IDDISTRT-WS            TO 4512-IDDISTR-TOM             
086100           END-IF                                                         
086200           PERFORM IMS-ISRT-WLXXJN11                                      
086300        ELSE                                                              
086400* UPPDATERING AV ENSKILDA FÄLT                                            
086500           IF KANTKOD(INDX) = 'Ä'                                         
086600              MOVE MID-KDTPOTYP-SPAR(INDX)    TO W-KDTPOTYP               
086700              MOVE MID-KDORDKL-SPAR(INDX)     TO W-KDORDKL                
086800              MOVE IDDISTRF-SPAR-WS           TO W-IDDISTR-FOM            
086900              MOVE IDDISTRT-SPAR-WS           TO W-IDDISTR-TOM            
087000              PERFORM IMS-GHU-WLXXJN11                                    
087100              IF SEGMENT-FINNS                                            
087200                 PERFORM IMS-DLET-WLXXJN11                                
087300              END-IF                                                      
087400              MOVE KDRAPRIO-WS TO 4512-KDRAPRIO                           
087500              IF MID-KDTPOTYP-RAD(INDX) NOT = ALL '+'                     
087600                 MOVE MID-KDTPOTYP-RAD(INDX)  TO 4512-KDTPOTYP            
087700              ELSE                                                        
087800                 MOVE MID-KDTPOTYP-SPAR(INDX) TO 4512-KDTPOTYP            
087900              END-IF                                                      
088000              IF MID-KDORDKL-RAD(INDX) NOT = ALL '+'                      
088100                 MOVE MID-KDORDKL-RAD(INDX)  TO 4512-KDORDKL              
088200              ELSE                                                        
088300                 MOVE MID-KDORDKL-SPAR(INDX) TO 4512-KDORDKL              
088400              END-IF                                                      
088500              IF MID-IDDISTR-FOM-RAD(INDX) NOT = ALL '+'                  
088600                 MOVE IDDISTRF-WS      TO 4512-IDDISTR-FOM                
088700              ELSE                                                        
088800                 MOVE IDDISTRF-SPAR-WS TO 4512-IDDISTR-FOM                
088900              END-IF                                                      
089000              IF MID-IDDISTR-TOM-RAD(INDX) NOT = ALL '+'                  
089100                 MOVE IDDISTRT-WS      TO 4512-IDDISTR-TOM                
089200              ELSE                                                        
089300                 MOVE IDDISTRT-SPAR-WS  TO 4512-IDDISTR-TOM               
089400              END-IF                                                      
089500              PERFORM IMS-ISRT-WLXXJN11                                   
089600           END-IF                                                         
089700        END-IF                                                            
089800     END-IF                                                               
089900     .                                                                    
090000     EJECT                                                                
090100 F-LAES-VISA-INFO SECTION.                                                
090200                                                                          
090300     PERFORM IMS-GU-WLXXJN01                                              
090400     IF START-JA                                                          
090500        PERFORM IMS-GNP-WLXXJN11                                          
090600     ELSE                                                                 
090700        PERFORM IMS-GNP-WLXXJN11-KVAL                                     
090800     END-IF                                                               
090900     IF SEGMENT-FINNS                                                     
091000        MOVE 4512-KDTPOTYP        TO MOD-KDTPOTYP-ENTER                   
091100        MOVE 4512-KDORDKL         TO MOD-KDORDKL-ENTER                    
091200        MOVE 4512-IDDISTR-FOM     TO IDDISTRF-WS                          
091300        MOVE IDDISTRF-WS          TO MOD-IDDISTR-FOM-ENTER                
091400        MOVE 4512-IDDISTR-TOM     TO IDDISTRT-WS                          
091500        MOVE IDDISTRT-WS          TO MOD-IDDISTR-TOM-ENTER                
091600     ELSE                                                                 
091700        MOVE ZERO                 TO MOD-KDTPOTYP-ENTER                   
091800                                     MOD-KDORDKL-ENTER                    
091900                                     MOD-IDDISTR-FOM-ENTER                
092000                                     MOD-IDDISTR-TOM-ENTER                
092100     END-IF                                                               
092200     MOVE +1 TO INDX                                                      
092300     PERFORM UNTIL INDX > +11                                             
092400        IF SEGMENT-FINNS                                                  
092500           MOVE 4512-KDTPOTYP        TO MOD-KDTPOTYP-RAD(INDX)            
092600                                        MOD-KDTPOTYP-SPAR(INDX)           
092700           MOVE 4512-KDORDKL         TO MOD-KDORDKL-RAD(INDX)             
092800                                        MOD-KDORDKL-SPAR(INDX)            
092900           MOVE 4512-IDDISTR-FOM     TO IDDISTRF-WS                       
093000           MOVE IDDISTRF-WS          TO MOD-IDDISTR-FOM-RAD(INDX)         
093100                                        MOD-IDDISTR-FOM-SPAR(INDX)        
093200           MOVE 4512-IDDISTR-TOM     TO IDDISTRT-WS                       
093300           MOVE IDDISTRT-WS          TO MOD-IDDISTR-TOM-RAD(INDX)         
093400                                        MOD-IDDISTR-TOM-SPAR(INDX)        
093500           MOVE MFS-RENSA-FAELT      TO MOD-KANTKOD-RAD(INDX)             
093600           PERFORM IMS-GNP-WLXXJN11                                       
093700        ELSE                                                              
093800           MOVE MFS-RENSA-FAELT      TO MOD-KDTPOTYP-RAD(INDX)            
093900                                        MOD-KDORDKL-RAD(INDX)             
094000                                        MOD-IDDISTR-FOM-RAD(INDX)         
094100                                        MOD-IDDISTR-TOM-RAD(INDX)         
094200                                        MOD-KANTKOD-RAD(INDX)             
094300           MOVE ZERO                 TO MOD-KDTPOTYP-SPAR(INDX)           
094400                                        MOD-KDORDKL-SPAR(INDX)            
094500                                        MOD-IDDISTR-FOM-SPAR(INDX)        
094600                                        MOD-IDDISTR-TOM-SPAR(INDX)        
094700        END-IF                                                            
094800        ADD +1 TO INDX                                                    
094900     END-PERFORM                                                          
095000     IF SEGMENT-FINNS                                                     
095100        MOVE 4512-KDTPOTYP        TO MOD-KDTPOTYP-PF8                     
095200        MOVE 4512-KDORDKL         TO MOD-KDORDKL-PF8                      
095300        MOVE 4512-IDDISTR-FOM     TO IDDISTRF-WS                          
095400        MOVE IDDISTRF-WS          TO MOD-IDDISTR-FOM-PF8                  
095500        MOVE 4512-IDDISTR-TOM     TO IDDISTRT-WS                          
095600        MOVE IDDISTRT-WS          TO MOD-IDDISTR-TOM-PF8                  
095700        MOVE TEXT-0402(SPRAK-IX)      TO MOD-TEMFSINF                     
095800     ELSE                                                                 
095900        IF MOD-IDDISTR-FOM-SPAR(11) NOT = ZERO OR                         
096000           MOD-IDDISTR-TOM-SPAR(11) NOT = ZERO                            
096100           MOVE MOD-KDTPOTYP-RAD(11)     TO MOD-KDTPOTYP-PF8              
096200           MOVE MOD-KDORDKL-RAD(11)      TO MOD-KDORDKL-PF8               
096300           MOVE MOD-IDDISTR-FOM-RAD (11) TO MOD-IDDISTR-FOM-PF8           
096400           MOVE MOD-IDDISTR-TOM-RAD (11) TO MOD-IDDISTR-TOM-PF8           
096500        ELSE                                                              
096600          MOVE ZERO                 TO MOD-KDTPOTYP-PF8                   
096700                                       MOD-KDORDKL-PF8                    
096800                                       MOD-IDDISTR-FOM-PF8                
096900                                       MOD-IDDISTR-TOM-PF8                
097000        END-IF                                                            
097100     END-IF                                                               
097200     .                                                                    
097300     EJECT                                                                
097400 G-FLYTTA-PRIOINFO SECTION.                                               
097500                                                                          
097600     MOVE 4502-BERAPRIO      TO MOD-BERAPRIO                              
097700     MOVE 4502-REROFORD      TO MOD-REROFORD                              
097800     MOVE 4502-FLPRIO        TO MOD-FLPRIO                                
097900     MOVE 4502-KVVECKOR-TECK TO MOD-KVVECKOR-TECK                         
098000     MOVE 4502-RELEVFOR      TO MOD-RELEVFOR                              
098100     .                                                                    
098200     EJECT                                                                
098300 MFS-FELHANTERING SECTION.                                                
098400                                                                          
098500     MOVE MFS-RENSA-FAELT   TO MOD-KDTPOTYP-ENTER                         
098600                               MOD-KDORDKL-ENTER                          
098700                               MOD-IDDISTR-FOM-ENTER                      
098800                               MOD-IDDISTR-TOM-ENTER                      
098900                               MOD-KDTPOTYP-PF8                           
099000                               MOD-KDORDKL-PF8                            
099100                               MOD-IDDISTR-FOM-PF8                        
099200                               MOD-IDDISTR-TOM-PF8                        
099300                               MOD-BERAPRIO                               
099400                               MOD-REROFORD                               
099500                               MOD-FLPRIO                                 
099600                               MOD-KVVECKOR-TECK                          
099700                               MOD-RELEVFOR                               
099800     MOVE +1 TO INDX                                                      
099900     PERFORM UNTIL INDX = +12                                             
100000        MOVE MFS-RENSA-FAELT  TO MOD-KDTPOTYP-RAD(INDX)                   
100100                                 MOD-KDORDKL-RAD(INDX)                    
100200                                 MOD-IDDISTR-FOM-RAD(INDX)                
100300                                 MOD-IDDISTR-TOM-RAD(INDX)                
100400                                 MOD-KDTPOTYP-SPAR(INDX)                  
100500                                 MOD-KDORDKL-SPAR(INDX)                   
100600                                 MOD-IDDISTR-FOM-SPAR(INDX)               
100700                                 MOD-IDDISTR-TOM-SPAR(INDX)               
100800                                 MOD-KANTKOD-RAD(INDX)                    
100900        ADD +1 TO INDX                                                    
101000     END-PERFORM                                                          
101100     PERFORM MFS-FORM-ATTR                                                
101200     .                                                                    
101300     EJECT                                                                
101400 MFS-ROR-EJ-FAELT  SECTION.                                               
101500       MOVE MFS-ROER-EJ-FAELT TO MOD-BERAPRIO                             
101600                                 MOD-REROFORD                             
101700                                 MOD-FLPRIO                               
101800                                 MOD-KVVECKOR-TECK                        
101900                                 MOD-RELEVFOR                             
102000                                 MOD-KDTPOTYP-ENTER                       
102100                                 MOD-KDORDKL-ENTER                        
102200                                 MOD-IDDISTR-FOM-ENTER                    
102300                                 MOD-IDDISTR-TOM-ENTER                    
102400                                 MOD-KDTPOTYP-PF8                         
102500                                 MOD-KDORDKL-PF8                          
102600                                 MOD-IDDISTR-FOM-PF8                      
102700                                 MOD-IDDISTR-TOM-PF8                      
102800     MOVE +1 TO INDX                                                      
102900     PERFORM UNTIL INDX = +12                                             
103000       MOVE MFS-ROER-EJ-FAELT TO MOD-KDTPOTYP-RAD(INDX)                   
103100                                 MOD-KDORDKL-RAD(INDX)                    
103200                                 MOD-IDDISTR-FOM-RAD(INDX)                
103300                                 MOD-IDDISTR-TOM-RAD(INDX)                
103400                                 MOD-KDTPOTYP-SPAR(INDX)                  
103500                                 MOD-KDORDKL-SPAR(INDX)                   
103600                                 MOD-IDDISTR-FOM-SPAR(INDX)               
103700                                 MOD-IDDISTR-TOM-SPAR(INDX)               
103800                                 MOD-KANTKOD-RAD(INDX)                    
103900        ADD +1 TO INDX                                                    
104000     END-PERFORM                                                          
104100     .                                                                    
104200     EJECT                                                                
104300 MFS-ROR-EJ-FAELT-HUVUD SECTION.                                          
104400       MOVE MFS-ROER-EJ-FAELT TO MOD-BERAPRIO                             
104500                                 MOD-REROFORD                             
104600                                 MOD-FLPRIO                               
104700                                 MOD-KVVECKOR-TECK                        
104800                                 MOD-RELEVFOR                             
104900     .                                                                    
105000     EJECT                                                                
105100 MFS-LAS-IN-IGEN SECTION.                                                 
105200     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BERAPRIO-ATTR                      
105300                                   MOD-REROFORD-ATTR                      
105400                                   MOD-FLPRIO-ATTR                        
105500                                   MOD-KVVECKOR-TECK-ATTR                 
105600                                   MOD-RELEVFOR-ATTR                      
105700     MOVE +1 TO INDX                                                      
105800     PERFORM UNTIL INDX = +12                                             
105900        MOVE MFS-ADD-LAES-IN-FAELT TO                                     
106000                                 MOD-KDTPOTYP-RAD-ATTR(INDX)              
106100                                 MOD-KDORDKL-RAD-ATTR(INDX)               
106200                                 MOD-IDDISTR-FOM-RAD-ATTR(INDX)           
106300                                 MOD-IDDISTR-TOM-RAD-ATTR(INDX)           
106400                                 MOD-KANTKOD-RAD-ATTR(INDX)               
106500        ADD +1 TO INDX                                                    
106600     END-PERFORM                                                          
106700     .                                                                    
106800     SKIP2                                                                
106900 MFS-FORM-ATTR SECTION.                                                   
107000                                                                          
107100     MOVE MFS-FORMATETS-ATTR    TO MOD-BERAPRIO-ATTR                      
107200                                   MOD-REROFORD-ATTR                      
107300                                   MOD-FLPRIO-ATTR                        
107400                                   MOD-KVVECKOR-TECK-ATTR                 
107500                                   MOD-RELEVFOR-ATTR                      
107600     MOVE +1 TO INDX                                                      
107700     PERFORM UNTIL INDX = +12                                             
107800        MOVE MFS-FORMATETS-ATTR TO MOD-KDTPOTYP-RAD-ATTR(INDX)            
107900                                   MOD-KDORDKL-RAD-ATTR(INDX)             
108000                                   MOD-IDDISTR-FOM-RAD-ATTR(INDX)         
108100                                   MOD-IDDISTR-TOM-RAD-ATTR(INDX)         
108200                                   MOD-KANTKOD-RAD-ATTR(INDX)             
108300                                                                          
108400        ADD +1 TO INDX                                                    
108500     END-PERFORM                                                          
108600     .                                                                    
108700     EJECT                                                                
108800* IMS SEKTIONER                                                           
108900     SKIP3                                                                
109000 IMS-GET-MSG SECTION.                                                     
109100                                                                          
109200     MOVE '  QC' TO GODK-STATUSKODER                                      
109300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
109400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
109500     PERFORM IMS-STATUSKONTROLL                                           
109600     SKIP3                                                                
109700     .                                                                    
109800 IMS-INSERT-MSG SECTION.                                                  
109900                                                                          
110000     IF ENGLISH-TEXT                                                      
110100       MOVE 'N' TO MFS-KDHUVOMR                                           
110200     END-IF                                                               
110300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
110400     MOVE SPACE TO GODK-STATUSKODER                                       
110500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
110600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
110700     PERFORM IMS-STATUSKONTROLL                                           
110800     .                                                                    
110900     EJECT                                                                
111000 IMS-GU-WLXXJM01 SECTION.                                                 
111100                                                                          
111200     STRING 'WLXXJM01(WDGXKEY  =' W-IDHTYP-4501-X ')'                     
111300          DELIMITED BY SIZE INTO SSA1                                     
111400     MOVE '  ' TO GODK-STATUSKODER                                        
111500     CALL CBLTDLI USING GU XXJM-PCB DLI-IO-AREA SSA1                      
111600     MOVE XXJM-STATUS-CODE TO STATUS-WS                                   
111700     PERFORM IMS-STATUSKONTROLL                                           
111800     .                                                                    
111900     SKIP1                                                                
112000 IMS-GU-WLXXJM01-M11 SECTION.                                             
112100                                                                          
112200     STRING 'WLXXJM01(WDGXKEY  =' W-IDHTYP-4501-X ')'                     
112300          DELIMITED BY SIZE INTO SSA1                                     
112400     STRING 'WLXXJM11(WDGXKEY  =' W-WDGXKEY-4502-X ')'                    
112500          DELIMITED BY SIZE INTO SSA2                                     
112600     MOVE '  GE' TO GODK-STATUSKODER                                      
112700     CALL CBLTDLI USING GU XXJM-PCB DLI-IO-AREA SSA1 SSA2                 
112800     MOVE XXJM-STATUS-CODE TO STATUS-WS                                   
112900     PERFORM IMS-STATUSKONTROLL                                           
113000     .                                                                    
113100     SKIP1                                                                
113200 IMS-GHU-WLXXJM11 SECTION.                                                
113300                                                                          
113400     STRING 'WLXXJM01(WDGXKEY  =' W-IDHTYP-4501-X ')'                     
113500          DELIMITED BY SIZE INTO SSA1                                     
113600     STRING 'WLXXJM11(WDGXKEY  =' W-WDGXKEY-4502-X ')'                    
113700          DELIMITED BY SIZE INTO SSA2                                     
113800     MOVE '  GE' TO GODK-STATUSKODER                                      
113900     CALL CBLTDLI USING GHU XXJM-PCB DLI-IO-AREA SSA1 SSA2                
114000     MOVE XXJM-STATUS-CODE TO STATUS-WS                                   
114100     PERFORM IMS-STATUSKONTROLL                                           
114200     .                                                                    
114300     EJECT                                                                
114400 IMS-REPL-WLXXJM11 SECTION.                                               
114500                                                                          
114600     MOVE 'WLXXJM11 ' TO SSA1                                             
114700     MOVE '  ' TO GODK-STATUSKODER                                        
114800     CALL CBLTDLI USING REPL XXJM-PCB DLI-IO-AREA SSA1                    
114900     MOVE XXJM-STATUS-CODE TO STATUS-WS                                   
115000     PERFORM IMS-STATUSKONTROLL                                           
115100     .                                                                    
115200     SKIP1                                                                
115300 IMS-GNP-WLXXJM11 SECTION.                                                
115400                                                                          
115500     MOVE 'WLXXJM11 ' TO SSA1                                             
115600     MOVE '  GE' TO GODK-STATUSKODER                                      
115700     CALL CBLTDLI USING GNP XXJM-PCB DLI-IO-AREA SSA1                     
115800     MOVE XXJM-STATUS-CODE TO STATUS-WS                                   
115900     PERFORM IMS-STATUSKONTROLL                                           
116000     .                                                                    
116100     SKIP1                                                                
116200 IMS-ISRT-WLXXJM11 SECTION.                                               
116300                                                                          
116400     STRING 'WLXXJM01(WDGXKEY  =' W-IDHTYP-4501-X ')'                     
116500          DELIMITED BY SIZE INTO SSA1                                     
116600     MOVE 'WLXXJM11 ' TO SSA2                                             
116700     MOVE '  II' TO GODK-STATUSKODER                                      
116800     CALL CBLTDLI USING ISRT XXJM-PCB DLI-IO-AREA SSA1 SSA2               
116900     MOVE XXJM-STATUS-CODE TO STATUS-WS                                   
117000     PERFORM IMS-STATUSKONTROLL                                           
117100     .                                                                    
117200     EJECT                                                                
117300 IMS-GU-WLXXJN01 SECTION.                                                 
117400                                                                          
117500     STRING 'WLXXJN01(WDGXKEY  =' W-IDHTYP-4511-X ')'                     
117600          DELIMITED BY SIZE INTO SSA1                                     
117700     MOVE '  ' TO GODK-STATUSKODER                                        
117800     CALL CBLTDLI USING GU XXJN-PCB DLI-IO-AREA SSA1                      
117900     MOVE XXJN-STATUS-CODE TO STATUS-WS                                   
118000     PERFORM IMS-STATUSKONTROLL                                           
118100     .                                                                    
118200     SKIP1                                                                
118300 IMS-GHNP-WLXXJN11 SECTION.                                               
118400                                                                          
118500     STRING 'WLXXJN11(KDRAPRIO =' W-KDRAPRIO-4512-X ')'                   
118600          DELIMITED BY SIZE INTO SSA1                                     
118700     MOVE '  GE' TO GODK-STATUSKODER                                      
118800     CALL CBLTDLI USING GHNP XXJN-PCB DLI-IO-AREA SSA1                    
118900     MOVE XXJN-STATUS-CODE TO STATUS-WS                                   
119000     PERFORM IMS-STATUSKONTROLL                                           
119100     .                                                                    
119200     SKIP1                                                                
119300 IMS-ISRT-WLXXJN11 SECTION.                                               
119400                                                                          
119500     STRING 'WLXXJN01(WDGXKEY  =' W-IDHTYP-4511-X ')'                     
119600          DELIMITED BY SIZE INTO SSA1                                     
119700     MOVE 'WLXXJN11 ' TO SSA2                                             
119800     MOVE '  II' TO GODK-STATUSKODER                                      
119900     CALL CBLTDLI USING ISRT XXJN-PCB DLI-IO-AREA SSA1 SSA2               
120000     MOVE XXJN-STATUS-CODE TO STATUS-WS                                   
120100     PERFORM IMS-STATUSKONTROLL                                           
120200     .                                                                    
120300     EJECT                                                                
120400 IMS-DLET-WLXXJN11 SECTION.                                               
120500                                                                          
120600     MOVE '  ' TO GODK-STATUSKODER                                        
120700     CALL CBLTDLI USING DLET XXJN-PCB DLI-IO-AREA                         
120800     MOVE XXJN-STATUS-CODE TO STATUS-WS                                   
120900     PERFORM IMS-STATUSKONTROLL                                           
121000     .                                                                    
121100     SKIP1                                                                
121200 IMS-GHU-WLXXJN11 SECTION.                                                
121300                                                                          
121400     STRING 'WLXXJN01(WDGXKEY  =' W-IDHTYP-4511-X ')'                     
121500          DELIMITED BY SIZE INTO SSA1                                     
121600     STRING 'WLXXJN11(WDGXKEY  =' W-WDGXKEY-4512-X ')'                    
121700          DELIMITED BY SIZE INTO SSA2                                     
121800     MOVE '  GE' TO GODK-STATUSKODER                                      
121900     CALL CBLTDLI USING GHU XXJN-PCB DLI-IO-AREA SSA1 SSA2                
122000     MOVE XXJN-STATUS-CODE TO STATUS-WS                                   
122100     PERFORM IMS-STATUSKONTROLL                                           
122200     .                                                                    
122300     SKIP1                                                                
122400 IMS-GNP-WLXXJN11 SECTION.                                                
122500     STRING 'WLXXJN11(KDRAPRIO =' W-KDRAPRIO-4512-X ')'                   
122600          DELIMITED BY SIZE INTO SSA3                                     
122700     MOVE '  GE' TO GODK-STATUSKODER                                      
122800     CALL CBLTDLI USING GNP XXJN-PCB DLI-IO-AREA SSA3                     
122900     MOVE XXJN-STATUS-CODE TO STATUS-WS                                   
123000     PERFORM IMS-STATUSKONTROLL                                           
123100     .                                                                    
123200     SKIP1                                                                
123300 IMS-GNP-WLXXJN11-KVAL SECTION.                                           
123400     STRING 'WLXXJN11(KDRAPRIO =' W-KDRAPRIO-4512-X                       
123500                    '&KDTPOTYP =' W-KDTPOTYP-X                            
123600                    '&KDORDKL  =' W-KDORDKL-X                             
123700                    '&IDDISTRF =' W-IDDISTR-FOM-X                         
123800                    '&IDDISTRT =' W-IDDISTR-TOM-X ')'                     
123900          DELIMITED BY SIZE INTO SSA3                                     
124000     MOVE '  GE' TO GODK-STATUSKODER                                      
124100     CALL CBLTDLI USING GNP XXJN-PCB DLI-IO-AREA SSA3                     
124200     MOVE XXJN-STATUS-CODE TO STATUS-WS                                   
124300     PERFORM IMS-STATUSKONTROLL                                           
124400     .                                                                    
124500     EJECT                                                                
124600 IMS-GU-WLXXJN11 SECTION.                                                 
124700     STRING 'WLXXJN01(WDGXKEY  =' W-IDHTYP-4511-X ')'                     
124800          DELIMITED BY SIZE INTO SSA1                                     
124900     STRING 'WLXXJN11(KDRAPRIO =' W-KDRAPRIO-4512-X                       
125000                    '&KDTPOTYP =' W-KDTPOTYP-X                            
125100                    '&KDORDKL  =' W-KDORDKL-X                             
125200                    '&IDDISTRF<=' W-IDDISTR-FOM-X                         
125300                    '&IDDISTRT=>' W-IDDISTR-FOM-X                         
125400                    '!KDRAPRIO =' W-KDRAPRIO-4512-X                       
125500                    '&KDTPOTYP =' W-KDTPOTYP-X                            
125600                    '&KDORDKL  =' W-KDORDKL-X                             
125700                    '&IDDISTRT=>' W-IDDISTR-TOM-X                         
125800                    '&IDDISTRF<=' W-IDDISTR-TOM-X                         
125900                    '!KDRAPRIO =' W-KDRAPRIO-4512-X                       
126000                    '&KDTPOTYP =' W-KDTPOTYP-X                            
126100                    '&KDORDKL  =' W-KDORDKL-X                             
126200                    '&IDDISTRF=>' W-IDDISTR-FOM-X                         
126300                    '&IDDISTRT<=' W-IDDISTR-TOM-X ')'                     
126400          DELIMITED BY SIZE INTO SSA3                                     
126500     MOVE '  GE' TO GODK-STATUSKODER                                      
126600     CALL CBLTDLI USING GU XXJN-PCB DLI-IO-AREA SSA1 SSA3                 
126700     MOVE XXJN-STATUS-CODE TO STATUS-WS                                   
126800     PERFORM IMS-STATUSKONTROLL                                           
126900     .                                                                    
127000     EJECT                                                                
127100 IMS-STATUSKONTROLL SECTION.                                              
127200                                                                          
127300     SET STATUS-IX TO 1                                                   
127400     SEARCH GODK-STATUS AT END CALL FELLOG                                
127500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
127600     END-SEARCH                                                           
127700     .                                                                    
