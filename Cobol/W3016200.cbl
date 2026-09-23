000100 PROCESS DYNAM                                                            
000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3016200.                                                
000300 AUTHOR.         PETER D.                                                 
000400 DATE-WRITTEN.   JULI  89.                                                
000500                                                                          
000700*                                                                         
000710*                                                                         
000800*    FUNKTION                                                             
000900*    FRÅGEPROGRAM DB2 OCH DL1                                             
001000*    INMATNINGSFÄLT ÄR FR BETECKNING                                      
001100*    PROGRAMMET LÄSER  WLBENA-BASEN                                       
001200*    SAMT TABELLERNA BYART BYPRO OCH BYLEV                                
001300*                                                                         
001400*    INDATA                                                               
001500*    TRANSAKTION  W3T162                                                  
001600*    MID          W3I16201                                                
001700*    MOD          W3O16201                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 DATA DIVISION.                                                           
002100     EJECT                                                                
002200 WORKING-STORAGE SECTION.                                                 
002201                                                                          
002210*    -- CHECKED BY WY2000                                                 
002300 77  PROGRAM-NAMN                PIC X(8) VALUE 'W3016200'.               
002400 77  JA                          PIC X       VALUE 'J'.                   
002500 77  NEJ                         PIC X       VALUE 'N'.                   
002600 77  W-BELEV                     PIC X(18) VALUE SPACE.                   
002700 77  W-BELEV-FROM                PIC X(30) VALUE SPACE.                   
002800 77  RAD-INDX                    PIC S9(9)   VALUE +0   COMP SYNC.        
002900 77  MAX-RAD                     PIC S9(9)   VALUE +14  COMP SYNC.        
003000 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
003100 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +1181 COMP SYNC.        
003200 01  SW-NYCKLAR-OK               PIC X.                                   
003300   88  NYCKLAR-OK                          VALUE 'J'.                     
003400 01  WS-IDTRANS                  PIC X(4).                                
003500   88  EGEN-BILD                           VALUE '3162'.                  
003600 01  FILLER                      PIC X(16)   VALUE                        
003700                                            'NYCKLAR-TILL-DLI'.           
003800 01  NYCKLAR-TILL-DLI.                                                    
003900   03  W-IDARTNR-X.                                                       
004000     05  W-IDARTNR               PIC S9(9) COMP-3 VALUE ZERO.             
004100   03  W-IDSKYLT-X               PIC X(3)         VALUE 'S  '.            
004200     EJECT                                                                
004300 01  DYNAMISKA-SUBPROGRAM.                                                
004400   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
004500   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
004600   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
004700 01  MEDDELANDE.                                                          
004800   03  FEL1.                                                              
004900     05 FILLER                   PIC X(40)                                
005000          VALUE 'DB2-TABELL OTILLGÄNGLIG'.                                
005100     05 FILLER                   PIC X(40)                                
005200          VALUE 'DATABASE UNAVAILABLE'.                                   
005300   03  FILLER REDEFINES FEL1.                                             
005400     05  FEL-1                   PIC X(40)   OCCURS 2.                    
005500                                                                          
005600   03  FEL2.                                                              
005700     05 FILLER                   PIC X(40)                                
005800          VALUE 'BETECKNING FEL'.                                         
005900     05 FILLER                   PIC X(40)                                
006000          VALUE 'IDENTITY WRONG'.                                         
006100   03  FILLER REDEFINES FEL2.                                             
006200     05  FEL-2                   PIC X(40)   OCCURS 2.                    
006300                                                                          
006400   03  FEL3.                                                              
006500     05 FILLER                   PIC X(40)                                
006600          VALUE 'BETECKNING SAKNAS'.                                      
006700     05 FILLER                   PIC X(40)                                
006800          VALUE 'IDENTITY MISSING '.                                      
006900   03  FILLER REDEFINES FEL3.                                             
007000     05  FEL-3                   PIC X(40)   OCCURS 2.                    
007100                                                                          
007200   03  MED1.                                                              
007300     05 FILLER                   PIC X(40)                                
007400          VALUE 'FÖRSÖK SENARE, EV KONTAKTA SYSTEMANSV'.                  
007500     05 FILLER                   PIC X(40)                                
007600          VALUE 'TRY LATER OR NOTIFY THE DP-DEPARTMENT'.                  
007700   03  FILLER REDEFINES MED1.                                             
007800     05  MED-1                   PIC X(40)   OCCURS 2.                    
007900                                                                          
008000   03  MED2.                                                              
008100     05 FILLER                   PIC X(40)                                
008200          VALUE 'TRYCK PF8 FÖR FLERA RADER'.                              
008300     05 FILLER                   PIC X(40)                                
008400          VALUE 'PRESS PF8 FOR MORE LINES'.                               
008500   03  FILLER REDEFINES MED2.                                             
008600     05  MED-2                   PIC X(40)   OCCURS 2.                    
008700                                                                          
008800   03  MED3.                                                              
008900     05 FILLER                   PIC X(40)                                
009000          VALUE 'DETTA ÄR FÖRSTA SIDAN'.                                  
009100     05 FILLER                   PIC X(40)                                
009200          VALUE 'THIS IS THE FIRST PAGE'.                                 
009300   03  FILLER REDEFINES MED3.                                             
009400     05  MED-3                   PIC X(40)   OCCURS 2.                    
009500     EJECT                                                                
009600******************************************************************        
009700*                                                                         
009800*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
009900*                                                                         
010000 01  FILLER                      PIC X(16)   VALUE 'MFS-WS'.              
010100*01  MID -COPY W3I16201                                                   
010300*01  -COPY WMSGAREA                                                       
010500     EJECT                                                                
010600*  03  MOD -COPY W3O16201  -RED MSG-AREA.                                 
010800     EJECT                                                                
010900*01  -COPY WMFSAREA                                                       
011100     EJECT                                                                
011200******************************************************************        
011300*                                                                         
011400*        ARBETS-AREOR TILL DB2- OCH IMS-SEKTIONERNA                       
011500*                                                                         
011600 01  FILLER                  PIC X(16)   VALUE 'DB2-WS     '.             
011700*01  -COPY BYPRO      -PRE BYPRO-                                         
011900     EJECT                                                                
012000*01  -COPY BYLEV      -PRE BYLEV-                                         
012200     EJECT                                                                
012300 01  FILLER                  PIC X(16) VALUE 'BYPRO-AREA'.                
012400       EXEC SQL INCLUDE BYPRO END-EXEC.                                   
012500     SKIP3                                                                
012600 01  FILLER                  PIC X(16) VALUE 'BYLEV-AREA'.                
012700       EXEC SQL INCLUDE BYLEV END-EXEC.                                   
012800     SKIP3                                                                
012900 01  FILLER                  PIC X(16) VALUE 'SQLCA-AREA'.                
013000       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
013100*                        **** STATUS-KOD FRÅN DB2                         
013200 01  FILLER                     PIC X(16) VALUE 'SQLCODE-WS'.             
013300 01  DB2-WS.                                                              
013400   03  SQLCODE-WS                PIC 9(3) VALUE ZERO.                     
013500     88  CURSOR-OK                           VALUE 000.                   
013600     88  RADER-FINNS                         VALUE 000.                   
013700     88  RADER-SAKNAS                        VALUE 100.                   
013800     88  904-KOD                             VALUE 904.                   
013900     SKIP1                                                                
014000   03  GODK-SQLCODESKODER.                                                
014100     05  GODK-SQLCODE OCCURS 5                                            
014200         INDEXED BY SQLCODE-IX PIC 999.                                   
014300     EJECT                                                                
014400 01  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.           
014500 01  IMS-WS.                                                              
014600     SKIP3                                                                
014700*                        **** STATUS-KOD FRÅN IMS                         
014800   03  STATUS-WS                 PIC XX.                                  
014900     88  SEGMENT-FINNS                       VALUE '  '.                  
015000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015100     SKIP3                                                                
015200   03  GODK-STATUSKODER.                                                  
015300     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015400     SKIP3                                                                
015500 01    SSA1                      PIC X(64).                               
015600 01    SSA2                      PIC X(64).                               
015700 01    SSA3                      PIC X(64).                               
015800*                            IMS FUNKTIONSKODER                           
015900*01    -COPY W0003                                                        
016100     EJECT                                                                
016200 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA'.           
016300 01  DLI-IO-AREA.                                                         
016400   03  IO-AREA                   PIC X(120)  VALUE SPACE.                 
016500*   03 BENA11  -COPY WDD311       -PRE BENA11-   -RED IO-AREA             
016700     EJECT                                                                
016800 LINKAGE SECTION.                                                         
016900*01  -COPY W0009     -PRE MSG-                                            
017100     SKIP2                                                                
017200*01  -COPY W0008     -PRE BENA-                                           
017400     05  FILLER                  PIC X.                                   
017500     EJECT                                                                
017600 PROCEDURE DIVISION   USING  MSG-PCB BENA-PCB.                            
017700      ENTRY 'DLITCBL' USING  MSG-PCB BENA-PCB.                            
017800     PERFORM IMS-GET-MSG                                                  
017900     IF SEGMENT-FINNS                                                     
018000        PERFORM A-INIT                                                    
018100        IF EGEN-BILD                                                      
018200           PERFORM B-FIXA-NYCKL-KOLLA-PF-TRYCK                            
018300           MOVE +1                        TO RAD-INDX                     
018400           PERFORM DB2-DCL-OPN-CRS-BYLEV                                  
018500           IF 904-KOD                                                     
018600              MOVE FEL-1 (SPRAK-IX)       TO MOD-TEMFSFEL                 
018700              MOVE MED-1 (SPRAK-IX)       TO MOD-TEMFSINF                 
018800           ELSE                                                           
018900              PERFORM DB2-FETCH-BYLEV                                     
019000              IF RADER-FINNS                                              
019100                 PERFORM C-VISA-SIDAN                                     
019200              ELSE                                                        
019300                 MOVE FEL-3 (SPRAK-IX)       TO MOD-TEMFSFEL              
019400                 PERFORM D-RENSA-SIDAN                                    
019500              END-IF                                                      
019600              PERFORM DB2-CLOSE-BYLEV-CRS                                 
019700           END-IF                                                         
019800        ELSE                                                              
019900           MOVE MFS-RENSA-FAELT              TO MOD-BELEV-UT              
020000           PERFORM D-RENSA-SIDAN                                          
020100        END-IF                                                            
020200        MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
020300        PERFORM IMS-INSERT-MSG                                            
020400     END-IF                                                               
020500     MOVE ZERO                            TO RETURN-CODE                  
020600     GOBACK                                                               
020700     .                                                                    
020800     EJECT                                                                
020900 A-INIT SECTION.                                                          
021000     SKIP2                                                                
021100     IF MSG-DUBBLA-TRANSKODER                                             
021200        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I16201                
021300        MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                 
021400        MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                
021500        MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                 
021600        MOVE MSG-IDPFK                     TO MFS-IDPFK                   
021700     ELSE                                                                 
021800        MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W3I16201                
021900        MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                 
022000        MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                
022100        MOVE SPACE                         TO MFS-KDTRTYP                 
022200                                              MFS-IDPFK                   
022300     END-IF                                                               
022400     MOVE LOW-VALUE                        TO MSG-AREA                    
022500     MOVE 'W3O162N1'                       TO MFS-IDMOD                   
022600     MOVE '3162'                           TO MOD-IDTRANS                 
022700     MOVE MFS-IDTRANS                      TO WS-IDTRANS                  
022800     MOVE MFS-RENSA-FAELT                  TO MOD-TEMFSFEL                
022900                                              MOD-TEMFSINF                
023000                                              MOD-BELEV-IN                
023100     IF EGEN-BILD                                                         
023200        CONTINUE                                                          
023300     ELSE                                                                 
023400        MOVE SPACE                         TO MFS-KDTRTYP                 
023500        MOVE '7'                           TO MFS-IDPFK                   
023600     END-IF                                                               
023700     IF ENGLISH-TEXT                                                      
023800        MOVE +2                            TO SPRAK-IX                    
023900        MOVE 'GB '                         TO W-IDSKYLT-X                 
024000     ELSE                                                                 
024100        MOVE +1                            TO SPRAK-IX                    
024200     END-IF                                                               
024300     INITIALIZE GODK-SQLCODESKODER                                        
024400     .                                                                    
024500     EJECT                                                                
024600 B-FIXA-NYCKL-KOLLA-PF-TRYCK SECTION.                                     
024700     SKIP2                                                                
024800     IF MID-BELEV-IN = ALL '+'                                            
024900        INSPECT MID-BELEV-UT REPLACING ALL '  ' BY '%%'                   
025000        MOVE MID-BELEV-UT            TO W-BELEV                           
025100     ELSE                                                                 
025200        MOVE MID-BELEV-IN            TO W-BELEV                           
025300        MOVE '7'                     TO MFS-IDPFK                         
025400        MOVE SPACE                   TO MFS-KDTRTYP                       
025500     END-IF                                                               
025600     MOVE W-BELEV                    TO MOD-BELEV-UT                      
025700     INSPECT MOD-BELEV-UT REPLACING ALL '%%' BY '  '                      
025800     IF MFS-IDPFK = '8'                                                   
025900        MOVE MID-BELEV-HI            TO W-BELEV-FROM                      
026000     ELSE                                                                 
026100        IF MFS-IDPFK = ' '                                                
026200           MOVE MID-BELEV-LO         TO W-BELEV-FROM                      
026300        ELSE                                                              
026400           MOVE MED-3 (SPRAK-IX)     TO MOD-TEMFSFEL                      
026500           MOVE SPACE                TO W-BELEV-FROM                      
026600        END-IF                                                            
026700     END-IF                                                               
026800     .                                                                    
026900     EJECT                                                                
027000 C-VISA-SIDAN    SECTION.                                                 
027100     SKIP2                                                                
027200     MOVE BYLEV-BELEV                TO MOD-BELEV-LO                      
027300     PERFORM UNTIL RAD-INDX > MAX-RAD                                     
027400        IF RADER-FINNS                                                    
027500           MOVE BYLEV-BELEV          TO MOD-BELEV     (RAD-INDX)          
027600           MOVE BYLEV-IDARTNR-BYT    TO MOD-IDARTNR   (RAD-INDX)          
027700                                        W-IDARTNR                         
027800           PERFORM IMS-GU-BENA11                                          
027900           IF SEGMENT-FINNS                                               
028000              MOVE BENA11-TEXT-BEART TO MOD-BEART-SVE (RAD-INDX)          
028100           END-IF                                                         
028200           PERFORM DB2-FETCH-BYLEV                                        
028300        ELSE                                                              
028400           MOVE MFS-RENSA-FAELT      TO MOD-BELEV     (RAD-INDX)          
028500                                        MOD-IDARTNR   (RAD-INDX)          
028600                                        MOD-BEART-SVE (RAD-INDX)          
028700        END-IF                                                            
028800        ADD +1                       TO RAD-INDX                          
028900     END-PERFORM                                                          
029000     IF RADER-FINNS                                                       
029100        MOVE BYLEV-BELEV             TO MOD-BELEV-HI                      
029200        MOVE MED-2 (SPRAK-IX)        TO MOD-TEMFSINF                      
029300     END-IF                                                               
029400     .                                                                    
029500     EJECT                                                                
029600 D-RENSA-SIDAN SECTION.                                                   
029700     SKIP2                                                                
029800     PERFORM UNTIL RAD-INDX > MAX-RAD                                     
029900        MOVE MFS-RENSA-FAELT         TO MOD-BELEV     (RAD-INDX)          
030000                                        MOD-IDARTNR   (RAD-INDX)          
030100                                        MOD-BEART-SVE (RAD-INDX)          
030200        ADD +1                       TO RAD-INDX                          
030300     END-PERFORM                                                          
030400     .                                                                    
030500     EJECT                                                                
030600* IMS SEKTIONER                                                           
030700     SKIP3                                                                
030800 IMS-GET-MSG SECTION.                                                     
030900     SKIP1                                                                
031000     MOVE '  QC' TO GODK-STATUSKODER                                      
031100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
031200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031300     PERFORM IMS-STATUSKONTROLL                                           
031400     SKIP3                                                                
031500     .                                                                    
031600 IMS-INSERT-MSG SECTION.                                                  
031700     SKIP1                                                                
031800     IF NOT ENGLISH-TEXT                                                  
031900       MOVE '0' TO MFS-KDHUVOMR                                           
032000     END-IF                                                               
032100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
032200     MOVE SPACE TO GODK-STATUSKODER                                       
032300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
032400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
032500     PERFORM IMS-STATUSKONTROLL                                           
032600     EJECT                                                                
032700     .                                                                    
032800 IMS-GU-BENA11 SECTION.                                                   
032900     SKIP1                                                                
033000     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
033100            DELIMITED BY SIZE INTO SSA1                                   
033200     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
033300            DELIMITED BY SIZE INTO SSA2                                   
033400     MOVE '  GE' TO GODK-STATUSKODER                                      
033500     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2                 
033600     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
033700     PERFORM IMS-STATUSKONTROLL                                           
033800     SKIP3                                                                
033900     .                                                                    
034000 IMS-STATUSKONTROLL SECTION.                                              
034100     SKIP1                                                                
034200     SET STATUS-IX TO 1                                                   
034300     SEARCH GODK-STATUS                                                   
034310       AT END                                                             
034320         CALL FELLOG                                                      
034400        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                          
034500           CONTINUE                                                       
034600     END-SEARCH                                                           
034700     .                                                                    
034800     EJECT                                                                
034900 DB2-DCL-OPN-CRS-BYLEV SECTION.                                           
035000     EXEC SQL DECLARE BYLEV-CRS CURSOR FOR                                
035100              SELECT BELEV,                                               
035200                     IDARTNR_BYT                                          
035300              FROM BYLEV                                                  
035400              WHERE BELEV LIKE :W-BELEV                                   
035500                AND BELEV >= :W-BELEV-FROM                                
035600           ORDER BY BELEV                                                 
035700     END-EXEC                                                             
035800     MOVE 000904            TO GODK-SQLCODESKODER                         
035900     EXEC SQL OPEN BYLEV-CRS END-EXEC                                     
036000     MOVE SQLCODE           TO SQLCODE-WS                                 
036100     PERFORM DB2-STATUSKONTROLL                                           
036200     .                                                                    
036300     EJECT                                                                
036400 DB2-FETCH-BYLEV SECTION.                                                 
036500     MOVE 000100            TO GODK-SQLCODESKODER                         
036600     EXEC SQL FETCH BYLEV-CRS INTO                                        
036700            :BYLEV-BELEV,                                                 
036800            :BYLEV-IDARTNR-BYT                                            
036900     END-EXEC                                                             
037000     MOVE SQLCODE           TO SQLCODE-WS                                 
037100     PERFORM DB2-STATUSKONTROLL                                           
037200     .                                                                    
037300     EJECT                                                                
037400 DB2-CLOSE-BYLEV-CRS SECTION.                                             
037500     SKIP2                                                                
037600     EXEC SQL CLOSE BYLEV-CRS END-EXEC                                    
037700     .                                                                    
037800     EJECT                                                                
037900 DB2-STATUSKONTROLL SECTION.                                              
038000     SKIP2                                                                
038100     SET SQLCODE-IX          TO 1                                         
038200     SEARCH GODK-SQLCODE                                                  
038210       AT END                                                             
038220         CALL FELLOG                                                      
038300        WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS                       
038400           CONTINUE                                                       
038500     END-SEARCH                                                           
038600     .                                                                    
