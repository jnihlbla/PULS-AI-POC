000100 PROCESS DYNAM                                                            
000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3015200.                                                
000300 AUTHOR.         PETER D.                                                 
000400 DATE-WRITTEN.   MAR   89.                                                
000500                                                                          
000700*                                                                         
000800*    FUNKTION                                                             
000900*    FRÅGEPROGRAM DB2 OCH DL1                                             
001000*    INMATNINGSFÄLT ÄR PRODUKTNR(ARTNR)                                   
001100*    PROGRAMMET LÄSER  WLBENA-BASEN                                       
001200*                                                                         
001210*    FRÅN PULS:         TRANSAKTION W3T151                                
001220*                       MID         W3I15101                              
001230*                       MOD         W3O15101                              
001240*                                                                         
001250*    FRÅN VOLVO VISION: TRANSAKTION W30151T                               
001260*                       MID         W3I151V1                              
001270*                       MOD         W3O151V1                              
001700*    .                                                                    
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 DATA DIVISION.                                                           
002100     EJECT                                                                
002200 WORKING-STORAGE SECTION.                                                 
002201                                                                          
002210*    -- CHECKED BY WY2000                                                 
002300 77  PROGRAM-NAMN                PIC X(8) VALUE 'W3015200'.               
002400 77  WS-IDARTNR                  PIC X(9) VALUE ZERO.                     
002500 77  JA                          PIC X       VALUE 'J'.                   
002600 77  NEJ                         PIC X       VALUE 'N'.                   
002700 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
002800 77  RAD-INDX                    PIC S9(9)   VALUE +0   COMP SYNC.        
002900 77  MAX-RAD-PULS                PIC S9(9)   VALUE +8   COMP SYNC.        
002910 77  MAX-RAD-VOLVO-VISION        PIC S9(9)   VALUE +13  COMP SYNC.        
003000 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
003100 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +440  COMP SYNC.        
003101                                                                          
003110 77  FLAGGA-TRANS-TYP            PIC X       VALUE 'J'.                   
003120     88  PULS-TRANS                          VALUE 'J'.                   
003130     88  VOLVO-VISION-TRANS                  VALUE 'N'.                   
003140                                                                          
003141 01  FILLER.                                                              
003150   03  MAX-RAD                   PIC S9(9)   VALUE ZERO.                  
003160                                                                          
003200 01  SW-NYCKLAR-OK               PIC X.                                   
003300   88  NYCKLAR-OK                          VALUE 'J'.                     
003400 01  WS-IDTRANS                  PIC X(4).                                
003500   88  EGEN-BILD                           VALUE '3152'.                  
003600 01  FILLER                      PIC X(16)   VALUE                        
003700                                            'NYCKLAR-TILL-DLI'.           
003800 01  NYCKLAR-TILL-DLI-DB2.                                                
003900   03  W-IDARTNR                 PIC S9(9) COMP-3 VALUE ZERO.             
004000   03  W-IDARTNR-X.                                                       
004100     05  W-IDARTNR-BYT           PIC S9(9) COMP-3 VALUE ZERO.             
004200   03  W-IDSKYLT-X               PIC X(3)         VALUE 'S  '.            
004300     EJECT                                                                
004400 01  DYNAMISKA-SUBPROGRAM.                                                
004500   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
004600   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
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
005800          VALUE 'PRODUKTNR FEL   '.                                       
005900     05 FILLER                   PIC X(40)                                
006000          VALUE 'PART.NO WRONG   '.                                       
006100   03  FILLER REDEFINES FEL2.                                             
006200     05  FEL-2                   PIC X(40)   OCCURS 2.                    
006300                                                                          
006400   03  FEL3.                                                              
006500     05 FILLER                   PIC X(40)                                
006600          VALUE 'PRODUKTNR SAKNAS'.                                       
006700     05 FILLER                   PIC X(40)                                
006800          VALUE 'PART.NO MISSING '.                                       
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
007900     EJECT                                                                
008000******************************************************************        
008100*                                                                         
008200*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
008300*                                                                         
008400 01  FILLER                      PIC X(16)   VALUE 'MFS-WS'.              
008440 01  FILLER                      PIC X(16)  VALUE 'MID-AREA PULS'.        
008500*01  MID -COPY W3I15201                                                   
008510     EJECT                                                                
008600 01  FILLER                      PIC X(16)  VALUE 'MID-VOLVISION'.        
008610*01  MID -COPY W3I152V1   -PRE MID2-                                      
008620     EJECT                                                                
008630 01  FILLER                      PIC X(16)  VALUE 'MSG AREA     '.        
008700*01  -COPY WMSGAREA                                                       
008900     EJECT                                                                
009000*  03  MOD -COPY W3O15201  -RED MSG-AREA.                                 
009200     EJECT                                                                
009210*  03  MOD -COPY W3O152V1 -PRE MOD2- -RED MSG-AREA.                       
009220     EJECT                                                                
009300*01  -COPY WMFSAREA                                                       
009500     EJECT                                                                
009600*01  -COPY BYPRO      -PRE BYPRO-                                         
009800 01  FILLER                  PIC X(16) VALUE 'BYPRO-AREA'.                
009900       EXEC SQL INCLUDE BYPRO END-EXEC.                                   
010000     SKIP3                                                                
010100 01  FILLER                  PIC X(16) VALUE 'SQLCA-AREA'.                
010200       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
010300     SKIP2                                                                
010400 01  FILLER                    PIC X(16)   VALUE 'SQLCODE-WS '.           
010500 01  DB2-WS.                                                              
010600    03  SQLCODE-WS                PIC 9(3)    VALUE ZERO.                 
010700        88  CURSOR-OK                           VALUE 000.                
010800        88  RADER-FINNS                         VALUE 000.                
010900        88  RADER-SAKNAS                        VALUE 100.                
011000        88  904-KOD                             VALUE 904.                
011100    03  GODK-SQLCODEKODER.                                                
011200      05  GODK-SQLCODE OCCURS 5                                           
011300          INDEXED BY SQLCODE-IX PIC 999.                                  
011400     SKIP1                                                                
011500 01  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.           
011600 01  IMS-WS.                                                              
011700     SKIP3                                                                
011800*                        **** STATUS-KOD FRÅN IMS                         
011900   03  STATUS-WS                 PIC XX.                                  
012000     88  SEGMENT-FINNS                       VALUE '  '.                  
012100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012200     SKIP3                                                                
012300   03  GODK-STATUSKODER.                                                  
012400     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012500     SKIP3                                                                
012600 01    SSA1                      PIC X(64).                               
012700 01    SSA2                      PIC X(64).                               
012800     EJECT                                                                
012900*                            IMS FUNKTIONSKODER                           
013000*01    -COPY W0003                                                        
013200     EJECT                                                                
013300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA'.           
013400 01  DLI-IO-AREA.                                                         
013500   03  IO-AREA                   PIC X(128)   VALUE SPACE.                
013600*   03 BENA11  -COPY WDD311       -PRE BENA11-   -RED IO-AREA             
013800     SKIP2                                                                
013900     EJECT                                                                
014000 LINKAGE SECTION.                                                         
014100*01  -COPY W0009     -PRE MSG-                                            
014300     SKIP2                                                                
014400*01  -COPY W0008     -PRE BENA-                                           
014600     05  FILLER                  PIC X.                                   
014700     EJECT                                                                
014800 PROCEDURE DIVISION   USING  MSG-PCB BENA-PCB.                            
014900      ENTRY 'DLITCBL' USING  MSG-PCB BENA-PCB.                            
015000     PERFORM IMS-GET-MSG                                                  
015100     IF SEGMENT-FINNS                                                     
015200        PERFORM A-INIT                                                    
015300        PERFORM B-KOLLA-NYCKLAR                                           
015400        MOVE +1                     TO RAD-INDX                           
015500        IF NYCKLAR-OK                                                     
015600           PERFORM C-VISA-SIDAN                                           
015700        ELSE                                                              
015710           IF PULS-TRANS                                                  
015800              MOVE FEL-2 (SPRAK-IX) TO MOD-TEMFSFEL                       
015910           ELSE                                                           
015920              MOVE 'B10'       TO MOD2-IDMFSFEL                           
015930              MOVE ZERO        TO MOD2-COUNTER                            
016000           END-IF                                                         
016001           PERFORM S01-RENSA-SIDAN                                        
016002        END-IF                                                            
016010        IF PULS-TRANS                                                     
016100           MOVE MAX-MOD-LAENGD TO MSG-KVLL                                
016210        ELSE                                                              
016211*            (VOLVO-VISION-TRANS)                                         
016212           COMPUTE MSG-KVLL = LENGTH OF MOD-W3O15201 + 4                  
016220        END-IF                                                            
016230        PERFORM IMS-INSERT-MSG                                            
016300     END-IF                                                               
016400     MOVE ZERO                            TO RETURN-CODE                  
016500     GOBACK                                                               
016600     .                                                                    
016700     EJECT                                                                
016800 A-INIT SECTION.                                                          
016900     SKIP2                                                                
016910     IF MSG-KDTRANS-1 (3:1) = 'T'                                         
016920       MOVE JA             TO FLAGGA-TRANS-TYP                            
016930     ELSE                                                                 
016940       MOVE NEJ            TO FLAGGA-TRANS-TYP                            
016950     END-IF                                                               
016960                                                                          
016970     IF PULS-TRANS                                                        
016980                                                                          
016981        MOVE MAX-RAD-PULS                  TO MAX-RAD                     
016990                                                                          
017000        IF MSG-DUBBLA-TRANSKODER                                          
017100           MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I15201             
017200           MOVE MSG-IDTRANS-2              TO MFS-IDTRANS                 
017300           MOVE MSG-KDMFSFOR-2             TO MFS-KDMFSFOR                
017400           MOVE MSG-KDTRTYP                TO MFS-KDTRTYP                 
017500           MOVE MSG-IDPFK                  TO MFS-IDPFK                   
017600        ELSE                                                              
017700           MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W3I15201               
017800           MOVE MSG-IDTRANS-1              TO MFS-IDTRANS                 
017900           MOVE MSG-KDMFSFOR-1             TO MFS-KDMFSFOR                
018000           MOVE SPACE                      TO MFS-KDTRTYP                 
018100                                                 MFS-IDPFK                
018200        END-IF                                                            
018300        MOVE LOW-VALUE                     TO MSG-AREA                    
018410        MOVE 'W3O152N1'                    TO MFS-IDMOD                   
018500        MOVE '3152'                        TO MOD-IDTRANS                 
018600        MOVE MFS-IDTRANS                   TO WS-IDTRANS                  
018700        MOVE MFS-RENSA-FAELT               TO MOD-TEMFSFEL                
018800                                                 MOD-TEMFSINF             
018900                                                 MOD-IDARTNR-IN           
019000        IF EGEN-BILD                                                      
019100           CONTINUE                                                       
019200        ELSE                                                              
019300           MOVE SPACE                      TO MFS-KDTRTYP                 
019400           MOVE '7'                        TO MFS-IDPFK                   
019500        END-IF                                                            
019600        IF ENGLISH-TEXT                                                   
019700           MOVE +2                         TO SPRAK-IX                    
019800           MOVE 'GB '                      TO W-IDSKYLT-X                 
019900        ELSE                                                              
020000           MOVE +1                         TO SPRAK-IX                    
020100        END-IF                                                            
020110     ELSE                                                                 
020120*         (VOLVO-VISION-TRANS)                                            
020121        MOVE MAX-RAD-VOLVO-VISION          TO MAX-RAD                     
020124                                                                          
020125        MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID2-W3I152V1                 
020126        MOVE MSG-IDTRANS-1                  TO MFS-IDTRANS                
020127        MOVE MSG-KDMFSFOR-1                 TO MFS-KDMFSFOR               
020128        MOVE MSG-KDTRTYP                    TO MFS-KDTRTYP                
020129        MOVE MSG-IDPFK                      TO MFS-IDPFK                  
020130                                                                          
020131        MOVE LOW-VALUE                     TO MSG-AREA                    
020132        MOVE SPACE                         TO MFS-IDMOD                   
020133        MOVE '3152'                        TO MOD2-IDTRANS                
020134        MOVE MFS-IDTRANS                   TO WS-IDTRANS                  
020135        MOVE ZERO                          TO MOD2-IDMFSFEL               
020140     END-IF                                                               
020200     INITIALIZE GODK-SQLCODEKODER                                         
020300     .                                                                    
020400     EJECT                                                                
020500 B-KOLLA-NYCKLAR SECTION.                                                 
020600     SKIP3                                                                
020700     MOVE JA                         TO SW-NYCKLAR-OK                     
020710     IF PULS-TRANS                                                        
020800        IF MID-IDARTNR-IN = ALL '+'                                       
020900           INSPECT MID-IDARTNR-UT REPLACING LEADING SPACE BY ZERO         
021000           MOVE MID-IDARTNR-UT       TO WS-IDARTNR                        
021100        ELSE                                                              
021200           MOVE MID-IDARTNR-IN       TO WS-IDARTNR                        
021300           MOVE '7'                  TO MFS-IDPFK                         
021400           MOVE SPACE                TO MFS-KDTRTYP                       
021500        END-IF                                                            
021600        IF WS-IDARTNR NUMERIC                                             
021700           MOVE WS-IDARTNR           TO W-IDARTNR                         
021800        ELSE                                                              
021900           MOVE NEJ                  TO SW-NYCKLAR-OK                     
022000        END-IF                                                            
022100        MOVE WS-IDARTNR              TO MOD-IDARTNR-UT                    
022200        INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE            
022210     ELSE                                                                 
022220*         (VOLVO-VISION-TRANS)                                            
022221        MOVE MID2-IDARTNR            TO WS-IDARTNR                        
022222        IF WS-IDARTNR NUMERIC                                             
022223           MOVE WS-IDARTNR           TO W-IDARTNR                         
022224                                        MOD2-IDARTNR-INPUT                
022225        ELSE                                                              
022226           MOVE NEJ                  TO SW-NYCKLAR-OK                     
022227        END-IF                                                            
022230     END-IF                                                               
022300     .                                                                    
022400     EJECT                                                                
022500 C-VISA-SIDAN  SECTION.                                                   
022600     SKIP2                                                                
022700     PERFORM DB2-DCL-OPN-CRS-BYPRO                                        
022710     IF PULS-TRANS                                                        
022800        IF 904-KOD                                                        
022900           MOVE FEL-1 (SPRAK-IX) TO MOD-TEMFSFEL                          
023000           MOVE MED-1 (SPRAK-IX) TO MOD-TEMFSINF                          
023110        ELSE                                                              
023200           PERFORM DB2-FETCH-BYPRO                                        
023300           PERFORM UNTIL RAD-INDX > MAX-RAD                               
023400              IF RADER-FINNS                                              
023500                 PERFORM CA-FLYTTA-TILL-BILD                              
023600                 PERFORM DB2-FETCH-BYPRO                                  
023700                 ADD +1                TO RAD-INDX                        
023800              ELSE                                                        
023900                 IF RAD-INDX = +1                                         
024000                    MOVE FEL-3 (SPRAK-IX) TO MOD-TEMFSFEL                 
024100                 END-IF                                                   
024200                 PERFORM S01-RENSA-SIDAN                                  
024300              END-IF                                                      
024400           END-PERFORM                                                    
024500           PERFORM DB2-CLOSE-CRS-BYPRO                                    
024600        END-IF                                                            
024610     ELSE                                                                 
024620*         (VOLVO-VISION-TRANS)                                            
024621        IF 904-KOD                                                        
024630           MOVE 'B10'                     TO MOD2-IDMFSFEL                
024631        ELSE                                                              
024633           PERFORM DB2-FETCH-BYPRO                                        
024634           PERFORM UNTIL RAD-INDX > MAX-RAD                               
024635              IF RADER-FINNS                                              
024636                 PERFORM CA-FLYTTA-TILL-BILD                              
024637                 PERFORM DB2-FETCH-BYPRO                                  
024638                 ADD +1                   TO RAD-INDX                     
024639              ELSE                                                        
024640                 IF RAD-INDX = +1                                         
024641                    MOVE 'B10'            TO MOD2-IDMFSFEL                
024642                 END-IF                                                   
024643                 SUBTRACT +1 FROM RAD-INDX GIVING MOD2-COUNTER            
024644                 PERFORM S01-RENSA-SIDAN                                  
024645              END-IF                                                      
024646           END-PERFORM                                                    
024648           PERFORM DB2-CLOSE-CRS-BYPRO                                    
024649        END-IF                                                            
024650     END-IF                                                               
024700     .                                                                    
024800     EJECT                                                                
024900 CA-FLYTTA-TILL-BILD SECTION.                                             
025000     SKIP2                                                                
025010     IF PULS-TRANS                                                        
025100        MOVE BYPRO-IDARTNR-BYT  TO MOD-IDARTNR-BYT (RAD-INDX)             
025200                                      W-IDARTNR-BYT                       
025300        PERFORM IMS-GU-BENA11                                             
025400        IF SEGMENT-FINNS                                                  
025500           MOVE BENA11-TEXT-BEART TO MOD-BEART-SVE (RAD-INDX)             
025600        END-IF                                                            
025610     ELSE                                                                 
025620*         (VOLVO-VISION-TRANS)                                            
025621        MOVE BYPRO-IDARTNR-BYT  TO MOD2-IDARTNR-BYT (RAD-INDX)            
025622                                      W-IDARTNR-BYT                       
025630     END-IF                                                               
025700     .                                                                    
025800     EJECT                                                                
025900 S01-RENSA-SIDAN SECTION.                                                 
026000     SKIP2                                                                
026100     PERFORM UNTIL RAD-INDX > MAX-RAD                                     
026110        IF PULS-TRANS                                                     
026200           MOVE MFS-RENSA-FAELT      TO MOD-INFO-RAD (RAD-INDX)           
026300*          MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR-BYT(RAD-INDX)         
026400*                                         MOD-BEART-SVE (RAD-INDX)        
026410        ELSE                                                              
026420*            (VOLVO-VISION-TRANS)                                         
026421           MOVE ZERO                 TO MOD2-IDARTNR-BYT(RAD-INDX)        
026430        END-IF                                                            
026500        ADD +1                       TO RAD-INDX                          
026600     END-PERFORM                                                          
026700     .                                                                    
026800     EJECT                                                                
026900* IMS SEKTIONER                                                           
027000     SKIP3                                                                
027100 IMS-GET-MSG SECTION.                                                     
027200     SKIP1                                                                
027300     MOVE '  QC' TO GODK-STATUSKODER                                      
027400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
027500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
027600     PERFORM IMS-STATUSKONTROLL                                           
027700     SKIP3                                                                
027800     .                                                                    
027900 IMS-INSERT-MSG SECTION.                                                  
028000     SKIP1                                                                
028100     IF NOT ENGLISH-TEXT                                                  
028200       MOVE '0' TO MFS-KDHUVOMR                                           
028300     END-IF                                                               
028400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
028500     MOVE SPACE TO GODK-STATUSKODER                                       
028600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
028700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
028800     PERFORM IMS-STATUSKONTROLL                                           
028900     EJECT                                                                
029000     .                                                                    
029100 IMS-GU-BENA11 SECTION.                                                   
029200     SKIP1                                                                
029300     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
029400            DELIMITED BY SIZE INTO SSA1                                   
029500     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
029600            DELIMITED BY SIZE INTO SSA2                                   
029700     MOVE '  GE' TO GODK-STATUSKODER                                      
029800     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2                 
029900     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
030000     PERFORM IMS-STATUSKONTROLL                                           
030100     SKIP3                                                                
030200     .                                                                    
030300 IMS-STATUSKONTROLL SECTION.                                              
030400     SKIP1                                                                
030500     SET STATUS-IX TO 1                                                   
030600     SEARCH GODK-STATUS                                                   
030610       AT END                                                             
030620         CALL FELLOG                                                      
030700        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                          
030800           CONTINUE                                                       
030900     END-SEARCH                                                           
031000     .                                                                    
031100     EJECT                                                                
031200 DB2-DCL-OPN-CRS-BYPRO SECTION.                                           
031300     EXEC SQL DECLARE BYPRO-CRS CURSOR FOR                                
031400              SELECT IDARTNR_BYT,                                         
031500                     IDARTNR                                              
031600              FROM BYPRO                                                  
031700              WHERE IDARTNR = :W-IDARTNR                                  
031800              ORDER BY IDARTNR_BYT                                        
031900     END-EXEC                                                             
032000     MOVE 000100904            TO GODK-SQLCODEKODER                       
032100     EXEC SQL OPEN BYPRO-CRS END-EXEC                                     
032200     MOVE SQLCODE              TO SQLCODE-WS                              
032300*    DISPLAY 'SQLCODE-WS = ' SQLCODE-WS                                   
032400     PERFORM DB2-STATUSKONTROLL                                           
032500     .                                                                    
032600     EJECT                                                                
032700 DB2-FETCH-BYPRO SECTION.                                                 
032800     MOVE 000100               TO GODK-SQLCODEKODER                       
032900     EXEC SQL FETCH BYPRO-CRS INTO                                        
033000            :BYPRO-IDARTNR-BYT,                                           
033100            :BYPRO-IDARTNR                                                
033200     END-EXEC                                                             
033300     MOVE SQLCODE              TO SQLCODE-WS                              
033400     PERFORM DB2-STATUSKONTROLL                                           
033500     .                                                                    
033600 DB2-CLOSE-CRS-BYPRO SECTION.                                             
033700     EXEC SQL CLOSE BYPRO-CRS END-EXEC                                    
033800     .                                                                    
033900     EJECT                                                                
034000 DB2-STATUSKONTROLL  SECTION.                                             
034100     SET SQLCODE-IX                TO 1                                   
034200     SEARCH GODK-SQLCODE                                                  
034210       AT END                                                             
034220         CALL FELLOG                                                      
034300        WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS                       
034400           CONTINUE                                                       
034500     END-SEARCH                                                           
034600     .                                                                    
