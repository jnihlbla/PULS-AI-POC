000100 PROCESS DYNAM                                                            
000200*        - OVANSTÅENDE BEHÖVS FÖR LÄNKNING AV ETT BATCH-DB2-PGM           
000300*        - ANNARS BLIR DET LÄNKAT FÖR IMS-ONLINE                          
000400*                                                                         
000500 ID DIVISION.                                                             
000600 PROGRAM-ID.     W3718A00.                                                
000700 AUTHOR.         PETER D.                                                 
000800 DATE-WRITTEN.   MAR   90.                                                
000900                                                                          
001000*                                                                         
001100*                                                                         
001200*    FUNKTION                                                             
001300*    PROGRAM SOM LÄSER SAMTLIGA BYTESNR                                   
001400*    FRÅN DB2-BASERN BYART BYLEV OCH BYPRO                                
001500*                                                                         
001600*    CHANGE LOG:                                                          
001700*                                                                         
001800*      YY/MM/DD - NAME            - CHANGE DESCRIPTION                    
001900*      ----------------------------------------------------------         
002000*      14/11/03 - REDDY RAHUL     - CHANGES FOR CHINA EXCHANGE            
002100*                                   E'TRACKER 10242148                    
002200*                                                                         
002300 ENVIRONMENT DIVISION.                                                    
002400 INPUT-OUTPUT SECTION.                                                    
002500 FILE-CONTROL.                                                            
002600*    --- UTFILER:                                                         
002700*           --- UPPDATERAT REGISTER:                                      
002800     SELECT W3718A                       ASSIGN TO W3718AD1.              
002900     SKIP2                                                                
003000 DATA DIVISION.                                                           
003100     EJECT                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W3718A                                                               
003500     LABEL RECORD   STANDARD                                              
003600     RECORDING      V                                                     
003700     BLOCK CONTAINS 0.                                                    
003800     SKIP2                                                                
003900*01  POST -COPY W371002     -PRE U8A2-  -L.                               
004000     SKIP2                                                                
004100*01  POST -COPY W371004     -PRE U8A4-  -L.                               
004200     SKIP2                                                                
004300*01  POST -COPY W371001     -PRE U8A1-  -L.                               
004400     SKIP2                                                                
004500*01  POST -COPY W371003     -PRE U8A3-  -L.                               
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004900*    -- CHECKED BY WY2000                                                 
005000 77  PROGRAM-NAMN                PIC X(8) VALUE 'W3718A00'.               
005100 77  W-IDPRODNR                  PIC S9(9) COMP-3 VALUE ZERO.             
005200 77  W-BELEV                     PIC X(30) VALUE SPACE.                   
005300 77  W-IDARTNR-BYT               PIC S9(9) COMP-3 VALUE ZERO.             
005400 01  WS-IDLEVNR                  PIC 9(5).                                
005500 77  JA                          PIC X       VALUE 'J'.                   
005600 77  NEJ                         PIC X       VALUE 'N'.                   
005700 77  IX                          PIC S9(9)   VALUE +0   COMP SYNC.        
005800     EJECT                                                                
005900 01  DYNAMISKA-SUBPROGRAM.                                                
006000   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
006100   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
006200   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
006300     EJECT                                                                
006400 01  FILLER                      PIC X(16)   VALUE 'POSTSUM-AREA'.        
006500*01  -COPY W0005     -PRE POSTSUM-.                                       
006600     EJECT                                                                
006700 01  FILLER                  PIC X(16)   VALUE 'UTFIL-AREA '.             
006800     SKIP2                                                                
006900*01  AREA -COPY W371001    -PRE U8A1-                                     
007000     SKIP2                                                                
007100*01  AREA -COPY W371002    -PRE U8A2-                                     
007200     SKIP2                                                                
007300*01  AREA -COPY W371003    -PRE U8A3-                                     
007400     EJECT                                                                
007500*01  AREA -COPY W371004    -PRE U8A4-                                     
007600     EJECT                                                                
007700*        ARBETS-AREOR TILL DB2-SEKTIONERNA                                
007800*                                                                         
007900 01  FILLER                  PIC X(16)   VALUE 'DB2-WS     '.             
008000*01  -COPY BYART      -PRE BYART-                                         
008100     EJECT                                                                
008200*01  -COPY BYPRO      -PRE BYPRO-                                         
008300     EJECT                                                                
008400*01  -COPY BYLEV      -PRE BYLEV-                                         
008500     EJECT                                                                
008600 01  FILLER                  PIC X(16) VALUE 'BYART-AREA'.                
008700       EXEC SQL INCLUDE BYART END-EXEC.                                   
008800     SKIP3                                                                
008900 01  FILLER                  PIC X(16) VALUE 'BYPRO-AREA'.                
009000       EXEC SQL INCLUDE BYPRO END-EXEC.                                   
009100     SKIP3                                                                
009200 01  FILLER                  PIC X(16) VALUE 'BYLEV-AREA'.                
009300       EXEC SQL INCLUDE BYLEV END-EXEC.                                   
009400     SKIP3                                                                
009500 01  FILLER                  PIC X(16) VALUE 'SQLCA-AREA'.                
009600       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
009700*                        **** STATUS-KOD FRÅN DB2                         
009800 01  FILLER                     PIC X(16) VALUE 'SQLCODE-WS'.             
009900 01  DB2-WS.                                                              
010000   03  SQLCODE-WS                PIC 9(3) VALUE ZERO.                     
010100     88  CURSOR-OK                           VALUE 000.                   
010200     88  RADER-FINNS                         VALUE 000.                   
010300     88  RADER-SAKNAS                        VALUE 100.                   
010400     88  904-KOD                             VALUE 904.                   
010500     SKIP1                                                                
010600   03  GODK-SQLCODESKODER.                                                
010700     05  GODK-SQLCODE OCCURS 5                                            
010800         INDEXED BY SQLCODE-IX PIC 999.                                   
010900     EJECT                                                                
011000 PROCEDURE DIVISION.                                                      
011100      ENTRY 'DLITCBL'.                                                    
011200     SKIP2                                                                
011300     PERFORM A-INIT                                                       
011400     PERFORM B-HAEMTA-SKRIV-BYTES-INFO                                    
011500     PERFORM Z-FINIT                                                      
011600     MOVE ZERO                            TO RETURN-CODE                  
011700     GOBACK                                                               
011800     .                                                                    
011900     EJECT                                                                
012000 A-INIT   SECTION.                                                        
012100     SKIP2                                                                
012200     OPEN OUTPUT W3718A                                                   
012300     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
012400     .                                                                    
012500     EJECT                                                                
012600 B-HAEMTA-SKRIV-BYTES-INFO  SECTION.                                      
012700     SKIP2                                                                
012800     MOVE ZERO                       TO W-IDARTNR-BYT                     
012900                                        W-IDPRODNR                        
013000     MOVE LOW-VALUE                  TO W-BELEV                           
013100     PERFORM DB2-DCL-OPN-CRS-BYART                                        
013200     PERFORM DB2-FETCH-BYART                                              
013300     PERFORM UNTIL RADER-SAKNAS                                           
013400        PERFORM BA-LAS-BYART-BYPRO-BYLEV-SKRIV                            
013500        PERFORM DB2-FETCH-BYART                                           
013600     END-PERFORM                                                          
013700     PERFORM DB2-CLOSE-BYART-CRS                                          
013800     .                                                                    
013900     EJECT                                                                
014000 BA-LAS-BYART-BYPRO-BYLEV-SKRIV    SECTION.                               
014100     SKIP2                                                                
014200     MOVE BYART-IDARTNR-BYT               TO W-IDARTNR-BYT                
014300                                             U8A1-BYTESNR                 
014400                                             U8A4-BYTESNR                 
014500                                                                          
014600     MOVE ZERO                            TO U8A1-IDARTNR-EMB             
014700     MOVE BYART-IDDISTR-RENOV             TO U8A1-IDDISTR-RENOV           
014800     MOVE BYART-BETFLEV                   TO U8A1-BETFLEV                 
014900     MOVE BYART-TEBYTKVA1                 TO U8A1-TEBYTKVA1               
015000     MOVE BYART-TEBYTKVA2                 TO U8A1-TEBYTKVA2               
015010     MOVE BYART-TEBYTKVA3                 TO U8A1-TEBYTKVA3               
015020     MOVE BYART-TEBYTKVA4                 TO U8A1-TEBYTKVA4               
015100     MOVE BYART-KVLS-MAXCORE              TO U8A1-KVLS-MAXCORE            
015200     COMPUTE U8A1-OBJEKT-BRA    = BYART-IDARTNR-BYT + 3000                
015300     COMPUTE U8A1-OBJEKT-DALIGT = BYART-IDARTNR-BYT + 6000                
015400     PERFORM S01-SKRIV-001-POST                                           
015500     MOVE ZERO                            TO U8A4-IDARTNR-EMB             
015600     MOVE BYART-IDDISTR-RENOV           TO U8A4-IDDISTR-RENOV             
015700     MOVE BYART-IDDISTR-RENOV-NDC       TO U8A4-IDDISTR-RENOV-NDC         
015800     MOVE BYART-IDDISTR-RENOV-PAC       TO U8A4-IDDISTR-RENOV-PAC         
015900     MOVE BYART-IDDISTR-RENOV-CAN       TO U8A4-IDDISTR-RENOV-CAN         
016000     MOVE BYART-IDDISTR-RENOV-AUS       TO U8A4-IDDISTR-RENOV-AUS         
016100     MOVE BYART-IDDISTR-RENOV-CHN       TO U8A4-IDDISTR-RENOV-CHN         
016110     MOVE BYART-IDDISTR-RENOV-KOR       TO U8A4-IDDISTR-RENOV-KOR         
016120     MOVE BYART-IDDISTR-RENOV-MY        TO U8A4-IDDISTR-RENOV-MY          
016130     MOVE BYART-IDDISTR-RENOV-TW        TO U8A4-IDDISTR-RENOV-TW          
016140     MOVE BYART-IDDISTR-RENOV-TH        TO U8A4-IDDISTR-RENOV-TH          
016200     MOVE BYART-TEBYTKVA1               TO U8A4-TEBYTKVA1                 
016300     MOVE BYART-TEBYTKVA2               TO U8A4-TEBYTKVA2                 
016310     MOVE BYART-TEBYTKVA3               TO U8A4-TEBYTKVA3                 
016320     MOVE BYART-TEBYTKVA4               TO U8A4-TEBYTKVA4                 
016400     COMPUTE U8A4-OBJEKT-BRA    = BYART-IDARTNR-BYT + 3000                
016500     COMPUTE U8A4-OBJEKT-DALIGT = BYART-IDARTNR-BYT + 6000                
016600     PERFORM S01-SKRIV-004-POST                                           
016700     PERFORM DB2-DCL-OPN-CRS-BYLEV                                        
016800     PERFORM DB2-FETCH-BYLEV                                              
016900     MOVE +1                           TO IX                              
017000     PERFORM UNTIL RADER-SAKNAS AND IX > +3                               
017100        IF RADER-FINNS                                                    
017200           IF IX > +3                                                     
017300              PERFORM S02-SKRIV-002-POST                                  
017400              MOVE +1                  TO IX                              
017500           END-IF                                                         
017600           MOVE BYLEV-BELEV            TO U8A2-BELEV  (IX)                
017700           PERFORM DB2-FETCH-BYLEV                                        
017800        ELSE                                                              
017900           MOVE SPACE                  TO U8A2-BELEV  (IX)                
018000        END-IF                                                            
018100        ADD  +1                        TO IX                              
018200     END-PERFORM                                                          
018300     PERFORM DB2-CLOSE-BYLEV-CRS                                          
018400     IF U8A2-BELEV (1) = SPACE                                            
018500        CONTINUE                                                          
018600     ELSE                                                                 
018700        PERFORM S02-SKRIV-002-POST                                        
018800     END-IF                                                               
018900     PERFORM DB2-DCL-OPN-CRS-BYPRO                                        
019000     PERFORM DB2-FETCH-BYPRO                                              
019100     MOVE +1                           TO IX                              
019200     PERFORM UNTIL RADER-SAKNAS AND IX > +3                               
019300        IF RADER-FINNS                                                    
019400           IF IX > +3                                                     
019500              PERFORM S03-SKRIV-003-POST                                  
019600              MOVE +1                     TO IX                           
019700           END-IF                                                         
019800           MOVE BYPRO-IDARTNR             TO U8A3-PRODNR (IX)             
019900           PERFORM DB2-FETCH-BYPRO                                        
020000        ELSE                                                              
020100           MOVE ZERO                      TO U8A3-PRODNR (IX)             
020200        END-IF                                                            
020300        ADD  +1                           TO IX                           
020400     END-PERFORM                                                          
020500     PERFORM DB2-CLOSE-BYPRO-CRS                                          
020600     IF U8A3-PRODNR (1) = ZERO                                            
020700        CONTINUE                                                          
020800     ELSE                                                                 
020900        PERFORM S03-SKRIV-003-POST                                        
021000     END-IF                                                               
021100     .                                                                    
021200     EJECT                                                                
021300 S01-SKRIV-001-POST  SECTION.                                             
021400     SKIP2                                                                
021500     MOVE '001'                           TO U8A1-IDPTYP                  
021600     WRITE U8A1-POST FROM U8A1-AREA                                       
021700     MOVE '001 ' TO POSTSUM-TRANSTYP                                      
021800     MOVE 'W3718A' TO POSTSUM-FDNAMN                                      
021900     MOVE 'W3718AD1' TO POSTSUM-DDNAMN2                                   
022000     CALL POSTSUM USING POSTSUM-PARM                                      
022100     .                                                                    
022200     EJECT                                                                
022300 S01-SKRIV-004-POST  SECTION.                                             
022400     SKIP2                                                                
022500     MOVE '004'                           TO U8A4-IDPTYP                  
022600     WRITE U8A4-POST FROM U8A4-AREA                                       
022700     MOVE '004 ' TO POSTSUM-TRANSTYP                                      
022800     MOVE 'W3718A' TO POSTSUM-FDNAMN                                      
022900     MOVE 'W3718AD1' TO POSTSUM-DDNAMN2                                   
023000     CALL POSTSUM USING POSTSUM-PARM                                      
023100     .                                                                    
023200     EJECT                                                                
023300 S02-SKRIV-002-POST  SECTION.                                             
023400     SKIP2                                                                
023500     MOVE '002'                           TO U8A2-IDPTYP                  
023600     MOVE W-IDARTNR-BYT                   TO U8A2-BYTESNR                 
023700     WRITE U8A2-POST FROM U8A2-AREA                                       
023800     MOVE '002 ' TO POSTSUM-TRANSTYP                                      
023900     MOVE 'W3718A' TO POSTSUM-FDNAMN                                      
024000     MOVE 'W3718AD1' TO POSTSUM-DDNAMN2                                   
024100     CALL POSTSUM USING POSTSUM-PARM                                      
024200     .                                                                    
024300     EJECT                                                                
024400 S03-SKRIV-003-POST  SECTION.                                             
024500     SKIP2                                                                
024600     MOVE '003'                           TO U8A3-IDPTYP                  
024700     MOVE W-IDARTNR-BYT                   TO U8A3-BYTESNR                 
024800     WRITE U8A3-POST FROM U8A3-AREA                                       
024900     MOVE '003 ' TO POSTSUM-TRANSTYP                                      
025000     MOVE 'W3718A' TO POSTSUM-FDNAMN                                      
025100     MOVE 'W3718AD1' TO POSTSUM-DDNAMN2                                   
025200     CALL POSTSUM USING POSTSUM-PARM                                      
025300     .                                                                    
025400     EJECT                                                                
025500 Z-FINIT SECTION.                                                         
025600     SKIP2                                                                
025700     CLOSE   W3718A                                                       
025800     MOVE 'S' TO POSTSUM-OPKOD                                            
025900     CALL POSTSUM USING POSTSUM-PARM                                      
026000     .                                                                    
026100     EJECT                                                                
026200 DB2-DCL-OPN-CRS-BYART SECTION.                                           
026300* OBS!!! DECLARE GER INGEN SQLCODE I RETUR                                
026400     EXEC SQL DECLARE BYART-CRS CURSOR FOR                                
026500              SELECT                                                      
026600                  IDARTNR_BYT,                                            
026700                  BETFLEV,                                                
026800                  IDDISTR_RENOV,                                          
026900                  TEBYTKVA1,                                              
027000                  TEBYTKVA2,                                              
027100                  IDDISTR_RENOV_NDC,                                      
027200                  IDDISTR_RENOV_PAC,                                      
027300                  IDDISTR_RENOV_CAN,                                      
027400                  IDDISTR_RENOV_AUS,                                      
027500                  KVLS_MAXCORE,                                           
027600                  IDDISTR_RENOV_CHN,                                      
027610                  TEBYTKVA3,                                              
027620                  TEBYTKVA4,                                              
027630                  IDDISTR_RENOV_KOR,                                      
027640                  IDDISTR_RENOV_MY,                                       
027650                  IDDISTR_RENOV_TW,                                       
027660                  IDDISTR_RENOV_TH                                        
027700              FROM BYART                                                  
027800              WHERE IDARTNR_BYT >= :W-IDARTNR-BYT                         
027900              ORDER BY IDARTNR_BYT                                        
028000     END-EXEC                                                             
028100     MOVE 000               TO GODK-SQLCODESKODER                         
028200     EXEC SQL OPEN BYART-CRS END-EXEC                                     
028300     MOVE SQLCODE           TO SQLCODE-WS                                 
028400     PERFORM DB2-STATUSKONTROLL                                           
028500     .                                                                    
028600     EJECT                                                                
028700 DB2-DCL-OPN-CRS-BYPRO SECTION.                                           
028800* OBS!!! DECLARE GER INGEN SQLCODE I RETUR                                
028900     EXEC SQL DECLARE BYPRO-CRS CURSOR FOR                                
029000              SELECT IDARTNR                                              
029100              FROM BYPRO                                                  
029200              WHERE IDARTNR >= :W-IDPRODNR                                
029300              AND IDARTNR_BYT = :W-IDARTNR-BYT                            
029400              ORDER BY IDARTNR                                            
029500     END-EXEC                                                             
029600     MOVE 000               TO GODK-SQLCODESKODER                         
029700     EXEC SQL OPEN BYPRO-CRS END-EXEC                                     
029800     MOVE SQLCODE           TO SQLCODE-WS                                 
029900     PERFORM DB2-STATUSKONTROLL                                           
030000     .                                                                    
030100     EJECT                                                                
030200 DB2-DCL-OPN-CRS-BYLEV SECTION.                                           
030300     EXEC SQL DECLARE BYLEV-CRS CURSOR FOR                                
030400              SELECT BELEV                                                
030500              FROM BYLEV                                                  
030600              WHERE BELEV >= :W-BELEV                                     
030700              AND IDARTNR_BYT = :W-IDARTNR-BYT                            
030800              ORDER BY BELEV                                              
030900     END-EXEC                                                             
031000     MOVE 000               TO GODK-SQLCODESKODER                         
031100     EXEC SQL OPEN BYLEV-CRS END-EXEC                                     
031200     MOVE SQLCODE           TO SQLCODE-WS                                 
031300     PERFORM DB2-STATUSKONTROLL                                           
031400     .                                                                    
031500     EJECT                                                                
031600 DB2-FETCH-BYART SECTION.                                                 
031700     MOVE 000100            TO GODK-SQLCODESKODER                         
031800     EXEC SQL FETCH BYART-CRS INTO                                        
031900           :BYART-IDARTNR-BYT,                                            
032000           :BYART-BETFLEV,                                                
032100           :BYART-IDDISTR-RENOV,                                          
032200           :BYART-TEBYTKVA1,                                              
032300           :BYART-TEBYTKVA2,                                              
032400           :BYART-IDDISTR-RENOV-NDC,                                      
032500           :BYART-IDDISTR-RENOV-PAC,                                      
032600           :BYART-IDDISTR-RENOV-CAN,                                      
032700           :BYART-IDDISTR-RENOV-AUS,                                      
032800           :BYART-KVLS-MAXCORE,                                           
032900           :BYART-IDDISTR-RENOV-CHN,                                      
032910           :BYART-TEBYTKVA3,                                              
032920           :BYART-TEBYTKVA4,                                              
032930           :BYART-IDDISTR-RENOV-KOR,                                      
032940           :BYART-IDDISTR-RENOV-MY,                                       
032950           :BYART-IDDISTR-RENOV-TW,                                       
032960           :BYART-IDDISTR-RENOV-TH                                        
033000     END-EXEC                                                             
033100     MOVE SQLCODE           TO SQLCODE-WS                                 
033200     PERFORM DB2-STATUSKONTROLL                                           
033300     .                                                                    
033400 DB2-FETCH-BYPRO SECTION.                                                 
033500     MOVE 000100            TO GODK-SQLCODESKODER                         
033600     EXEC SQL FETCH BYPRO-CRS INTO                                        
033700            :BYPRO-IDARTNR                                                
033800     END-EXEC                                                             
033900     MOVE SQLCODE           TO SQLCODE-WS                                 
034000     PERFORM DB2-STATUSKONTROLL                                           
034100     .                                                                    
034200 DB2-FETCH-BYLEV SECTION.                                                 
034300     MOVE 000100            TO GODK-SQLCODESKODER                         
034400     EXEC SQL FETCH BYLEV-CRS INTO                                        
034500            :BYLEV-BELEV                                                  
034600     END-EXEC                                                             
034700     MOVE SQLCODE           TO SQLCODE-WS                                 
034800     PERFORM DB2-STATUSKONTROLL                                           
034900     .                                                                    
035000     EJECT                                                                
035100 DB2-CLOSE-BYART-CRS SECTION.                                             
035200     SKIP2                                                                
035300     EXEC SQL CLOSE BYART-CRS END-EXEC                                    
035400     .                                                                    
035500     EJECT                                                                
035600 DB2-CLOSE-BYLEV-CRS SECTION.                                             
035700     SKIP2                                                                
035800     EXEC SQL CLOSE BYLEV-CRS END-EXEC                                    
035900     .                                                                    
036000     EJECT                                                                
036100 DB2-CLOSE-BYPRO-CRS SECTION.                                             
036200     SKIP2                                                                
036300     EXEC SQL CLOSE BYPRO-CRS END-EXEC                                    
036400     .                                                                    
036500     EJECT                                                                
036600 DB2-STATUSKONTROLL SECTION.                                              
036700     SKIP2                                                                
036800     SET SQLCODE-IX          TO 1                                         
036900     SEARCH GODK-SQLCODE AT END CALL FELLOG                               
037000        WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS                       
037100           CONTINUE                                                       
037200     END-SEARCH                                                           
037300     .                                                                    
