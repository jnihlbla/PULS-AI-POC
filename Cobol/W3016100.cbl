000100 PROCESS DYNAM                                                            
000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3016100.                                                
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
001500*    TRANSAKTION  W3T161                                                  
001600*    MID          W3I16101                                                
001700*    MOD          W3O16101                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 DATA DIVISION.                                                           
002100     EJECT                                                                
002200 WORKING-STORAGE SECTION.                                                 
002201                                                                          
002210*    -- CHECKED BY WY2000                                                 
002300 77  PROGRAM-NAMN                PIC X(8) VALUE 'W3016100'.               
002400 77  W-BELEV                     PIC X(30) VALUE SPACE.                   
002500 77  JA                          PIC X       VALUE 'J'.                   
002600 77  NEJ                         PIC X       VALUE 'N'.                   
002700 77  RAD-INDX                    PIC S9(9)   VALUE +0   COMP SYNC.        
002800 77  TRE                         PIC S9(9)   VALUE +3   COMP SYNC.        
002900 77  FYRTIOATTA                  PIC S9(9)   VALUE +48  COMP SYNC.        
003000 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
003100 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +754  COMP SYNC.        
003200 77  W-IDPRODNR                  PIC S9(9) COMP-3 VALUE ZERO.             
003300 01  SW-NYCKLAR-OK               PIC X.                                   
003400   88  NYCKLAR-OK                          VALUE 'J'.                     
003500 01  WS-IDTRANS                  PIC X(4).                                
003600   88  EGEN-BILD                           VALUE '3161'.                  
003700   88  3162-BILDEN                         VALUE '3162'.                  
003800 01  FILLER                      PIC X(16)   VALUE                        
003900                                            'NYCKLAR-TILL-DLI'.           
004000 01  NYCKLAR-TILL-DLI.                                                    
004100   03  W-IDARTNR-X.                                                       
004200     05  W-IDARTNR-BYT           PIC S9(9) COMP-3 VALUE ZERO.             
004300   03  W-IDSKYLT-X               PIC X(3)         VALUE 'S  '.            
004400     EJECT                                                                
004500 01  DYNAMISKA-SUBPROGRAM.                                                
004600   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
004700   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
004800   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
004900 01  MEDDELANDE.                                                          
005000   03  FEL1.                                                              
005100     05 FILLER                   PIC X(40)                                
005200          VALUE 'DB2-TABELL OTILLGÄNGLIG'.                                
005300     05 FILLER                   PIC X(40)                                
005400          VALUE 'DATABASE UNAVAILABLE'.                                   
005500   03  FILLER REDEFINES FEL1.                                             
005600     05  FEL-1                   PIC X(40)   OCCURS 2.                    
005700                                                                          
005800   03  FEL2.                                                              
005900     05 FILLER                   PIC X(40)                                
006000          VALUE 'BETECKNING FEL'.                                         
006100     05 FILLER                   PIC X(40)                                
006200          VALUE 'IDENTITY WRONG'.                                         
006300   03  FILLER REDEFINES FEL2.                                             
006400     05  FEL-2                   PIC X(40)   OCCURS 2.                    
006500                                                                          
006600   03  FEL3.                                                              
006700     05 FILLER                   PIC X(40)                                
006800          VALUE 'BETECKNING SAKNAS'.                                      
006900     05 FILLER                   PIC X(40)                                
007000          VALUE 'IDENTITY MISSING '.                                      
007100   03  FILLER REDEFINES FEL3.                                             
007200     05  FEL-3                   PIC X(40)   OCCURS 2.                    
007300                                                                          
007400   03  MED1.                                                              
007500     05 FILLER                   PIC X(40)                                
007600          VALUE 'FÖRSÖK SENARE, EV KONTAKT SYSTEMANSV'.                   
007700     05 FILLER                   PIC X(40)                                
007800          VALUE 'TRY LATER OR NOTIFY THE DP-DEPARTMENT'.                  
007900   03  FILLER REDEFINES MED1.                                             
008000     05  MED-1                   PIC X(40)   OCCURS 2.                    
008100                                                                          
008200   03  MED2.                                                              
008300     05 FILLER                   PIC X(40)                                
008400          VALUE 'TRYCK PF8 FÖR FLERA RADER'.                              
008500     05 FILLER                   PIC X(40)                                
008600          VALUE 'PRESS PF8 FOR MORE LINES'.                               
008700   03  FILLER REDEFINES MED2.                                             
008800     05  MED-2                   PIC X(40)   OCCURS 2.                    
008900                                                                          
009000   03  MED3.                                                              
009100     05 FILLER                   PIC X(40)                                
009200          VALUE 'DETTA ÄR FÖRSTA SIDAN'.                                  
009300     05 FILLER                   PIC X(40)                                
009400          VALUE 'THIS IS THE FIRST PAGE'.                                 
009500   03  FILLER REDEFINES MED3.                                             
009600     05  MED-3                   PIC X(40)   OCCURS 2.                    
009700     EJECT                                                                
009800******************************************************************        
009900*                                                                         
010000*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
010100*                                                                         
010200 01  FILLER                      PIC X(16)   VALUE 'MFS-WS'.              
010300*01  MID -COPY W3I16101                                                   
010500*01  -COPY WMSGAREA                                                       
010700     EJECT                                                                
010800*  03  MOD -COPY W3O16101  -RED MSG-AREA.                                 
011000     EJECT                                                                
011100*01  -COPY WMFSAREA                                                       
011300     EJECT                                                                
011400******************************************************************        
011500*                                                                         
011600*        ARBETS-AREOR TILL DB2- OCH IMS-SEKTIONERNA                       
011700*                                                                         
011800 01  FILLER                  PIC X(16)   VALUE 'DB2-WS     '.             
011900*01  -COPY BYART -PRE BYART-                                              
012100     EJECT                                                                
012200*01  -COPY BYPRO -PRE BYPRO-                                              
012400     EJECT                                                                
012500*01  -COPY BYLEV -PRE BYLEV-                                              
012700     EJECT                                                                
012800 01  FILLER                  PIC X(16) VALUE 'BYART-AREA'.                
012900       EXEC SQL INCLUDE BYART END-EXEC.                                   
013000     SKIP3                                                                
013100 01  FILLER                  PIC X(16) VALUE 'BYPRO-AREA'.                
013200       EXEC SQL INCLUDE BYPRO END-EXEC.                                   
013300     SKIP3                                                                
013400 01  FILLER                  PIC X(16) VALUE 'BYLEV-AREA'.                
013500       EXEC SQL INCLUDE BYLEV END-EXEC.                                   
013600     SKIP3                                                                
013700 01  FILLER                  PIC X(16) VALUE 'SQLCA-AREA'.                
013800       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
013900*                        **** STATUS-KOD FRÅN DB2                         
014000 01  FILLER                     PIC X(16) VALUE 'SQLCODE-WS'.             
014100 01  DB2-WS.                                                              
014200   03  SQLCODE-WS                PIC 9(3) VALUE ZERO.                     
014300     88  CURSOR-OK                           VALUE 000.                   
014400     88  RADER-FINNS                         VALUE 000.                   
014500     88  RADER-SAKNAS                        VALUE 100.                   
014600     88  904-KOD                             VALUE 904.                   
014700     SKIP1                                                                
014800   03  GODK-SQLCODESKODER.                                                
014900     05  GODK-SQLCODE OCCURS 5                                            
015000         INDEXED BY SQLCODE-IX PIC 999.                                   
015100     EJECT                                                                
015200 01  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.           
015300 01  IMS-WS.                                                              
015400     SKIP3                                                                
015500*                        **** STATUS-KOD FRÅN IMS                         
015600   03  STATUS-WS                 PIC XX.                                  
015700     88  SEGMENT-FINNS                       VALUE '  '.                  
015800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015900     SKIP3                                                                
016000   03  GODK-STATUSKODER.                                                  
016100     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016200     SKIP3                                                                
016300 01    SSA1                      PIC X(64).                               
016400 01    SSA2                      PIC X(64).                               
016500 01    SSA3                      PIC X(64).                               
016600*                            IMS FUNKTIONSKODER                           
016700*01    -COPY W0003                                                        
016900     EJECT                                                                
017000 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA'.           
017100 01  DLI-IO-AREA.                                                         
017200   03  IO-AREA                   PIC X(120)   VALUE SPACE.                
017300*   03 BENA11  -COPY WDD311   -PRE BENA11-   -RED IO-AREA                 
017500     EJECT                                                                
017600 LINKAGE SECTION.                                                         
017700*01  -COPY W0009     -PRE MSG-                                            
017900     SKIP2                                                                
018000*01  -COPY W0008     -PRE BENA-                                           
018200     05  FILLER                  PIC X.                                   
018300     EJECT                                                                
018400 PROCEDURE DIVISION   USING  MSG-PCB BENA-PCB.                            
018500      ENTRY 'DLITCBL' USING  MSG-PCB BENA-PCB.                            
018600     PERFORM IMS-GET-MSG                                                  
018700     IF SEGMENT-FINNS                                                     
018800        PERFORM A-INIT                                                    
018900        PERFORM B-FIXA-NYCKL-KOLLA-PF-TRYCK                               
019000        PERFORM DB2-SELECT-BYLEV                                          
019100        IF RADER-FINNS                                                    
019200           PERFORM C-VISA-SIDAN                                           
019300        ELSE                                                              
019400           IF 904-KOD                                                     
019500              MOVE FEL-1 (SPRAK-IX)       TO MOD-TEMFSFEL                 
019600              MOVE MED-1 (SPRAK-IX)       TO MOD-TEMFSINF                 
019700           ELSE                                                           
019800              MOVE FEL-3 (SPRAK-IX)          TO MOD-TEMFSFEL              
019900              PERFORM S01-RENSA-SIDAN                                     
020000           END-IF                                                         
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
021200        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I16101                
021300        MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                 
021400        MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                
021500        MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                 
021600        MOVE MSG-IDPFK                     TO MFS-IDPFK                   
021700     ELSE                                                                 
021800        MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W3I16101                
021900        MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                 
022000        MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                
022100        MOVE SPACE                         TO MFS-KDTRTYP                 
022200                                              MFS-IDPFK                   
022300     END-IF                                                               
022400     MOVE LOW-VALUE                        TO MSG-AREA                    
022500     MOVE 'W3O161N1'                       TO MFS-IDMOD                   
022600     MOVE '3161'                           TO MOD-IDTRANS                 
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
024900        IF 3162-BILDEN                                                    
025000           INSPECT MID-BELEV-UT REPLACING ALL '%' BY SPACE                
025100        END-IF                                                            
025200        MOVE MID-BELEV-UT            TO W-BELEV                           
025300     ELSE                                                                 
025400        IF 3162-BILDEN                                                    
025500           INSPECT MID-BELEV-IN REPLACING ALL '%' BY SPACE                
025600        END-IF                                                            
025700        MOVE '7'                     TO MFS-IDPFK                         
025800        MOVE MID-BELEV-IN            TO W-BELEV                           
025900     END-IF                                                               
026000     MOVE W-BELEV                    TO MOD-BELEV-UT                      
026100     IF  MID-IDPRODNR-LO NUMERIC                                          
026200     AND MID-IDPRODNR-HI NUMERIC                                          
026300        CONTINUE                                                          
026400     ELSE                                                                 
026500        MOVE '7'                     TO MFS-IDPFK                         
026600     END-IF                                                               
026700     IF MFS-IDPFK = '8'                                                   
026800        MOVE MID-IDPRODNR-HI         TO W-IDPRODNR                        
026900     ELSE                                                                 
027000        IF MFS-IDPFK = ' '                                                
027100           MOVE MID-IDPRODNR-LO      TO W-IDPRODNR                        
027200        ELSE                                                              
027300           MOVE ZERO                 TO W-IDPRODNR                        
027400        END-IF                                                            
027500     END-IF                                                               
027600     IF W-IDPRODNR = ZERO                                                 
027700        MOVE SPACE                   TO MFS-KDTRTYP                       
027800        MOVE MED-3 (SPRAK-IX)        TO MOD-TEMFSFEL                      
027900     END-IF                                                               
028000     .                                                                    
028100     EJECT                                                                
028200 C-VISA-SIDAN    SECTION.                                                 
028300     SKIP2                                                                
028400     MOVE BYLEV-IDARTNR-BYT         TO MOD-IDARTNR-BYT                    
028500                                       W-IDARTNR-BYT                      
028600     PERFORM DB2-SELECT-BYART                                             
028700     IF RADER-FINNS                                                       
028800        PERFORM CA-FLYTT-BYART-T-BILD                                     
028900        PERFORM CB-FLYTT-BYPRO-T-BILD                                     
029000        PERFORM IMS-GU-BENA11                                             
029100        IF SEGMENT-FINNS                                                  
029200           MOVE BENA11-TEXT-BEART   TO MOD-BEART-SVE                      
029300        END-IF                                                            
029400     ELSE                                                                 
029500        MOVE FEL-3 (SPRAK-IX)       TO MOD-TEMFSFEL                       
029600        PERFORM S01-RENSA-SIDAN                                           
029700     END-IF                                                               
029800     .                                                                    
029900     EJECT                                                                
030000 CA-FLYTT-BYART-T-BILD SECTION.                                           
030100     SKIP2                                                                
030300     MOVE BYART-BETFLEV            TO MOD-BETFLEV                         
030400     MOVE BYART-TEBYTNOT1          TO MOD-TEBYTNOT (1)                    
030500     MOVE BYART-TEBYTNOT2          TO MOD-TEBYTNOT (2)                    
030600     MOVE BYART-TEBYTNOT3          TO MOD-TEBYTNOT (3)                    
030700     MOVE BYART-TEBYTNOT4          TO MOD-TEBYTNOT (4)                    
031600     .                                                                    
031700     EJECT                                                                
031800 CB-FLYTT-BYPRO-T-BILD SECTION.                                           
031900     SKIP1                                                                
032000     PERFORM DB2-DCL-OPN-CRS-BYPRO                                        
032100     IF CURSOR-OK                                                         
032200        PERFORM DB2-FETCH-BYPRO                                           
032300        IF RADER-FINNS                                                    
032400           PERFORM CBA-FLYTTA-FRA-BYPRO                                   
032500           PERFORM DB2-CLOSE-BYPRO-CRS                                    
032600        ELSE                                                              
032700           PERFORM CBB-RENSA-BYPRO                                        
032800        END-IF                                                            
032900     END-IF                                                               
033000     .                                                                    
033100     EJECT                                                                
033200 CBA-FLYTTA-FRA-BYPRO SECTION.                                            
033300     SKIP2                                                                
033400     MOVE BYPRO-IDARTNR               TO MOD-IDPRODNR-LO                  
033500     MOVE +1                          TO RAD-INDX                         
033600     PERFORM UNTIL  RAD-INDX > FYRTIOATTA                                 
033700        IF RADER-FINNS                                                    
033800           MOVE BYPRO-IDARTNR         TO MOD-IDARTNR (RAD-INDX)           
033900           PERFORM DB2-FETCH-BYPRO                                        
034000        ELSE                                                              
034100           MOVE MFS-RENSA-FAELT       TO MOD-IDARTNR (RAD-INDX)           
034200        END-IF                                                            
034300        ADD +1                        TO RAD-INDX                         
034400     END-PERFORM                                                          
034500     IF RADER-FINNS                                                       
034600        MOVE MED-2 (SPRAK-IX)         TO MOD-TEMFSINF                     
034700        MOVE BYPRO-IDARTNR            TO MOD-IDPRODNR-HI                  
034800     ELSE                                                                 
034900        MOVE ZERO                     TO MOD-IDPRODNR-HI                  
035000     END-IF                                                               
035100     .                                                                    
035200     EJECT                                                                
035300 CBB-RENSA-BYPRO SECTION.                                                 
035400     SKIP2                                                                
035500     MOVE ZERO                        TO MOD-IDPRODNR-LO                  
035600                                         MOD-IDPRODNR-HI                  
035700     MOVE +1                          TO RAD-INDX                         
035800     PERFORM UNTIL RAD-INDX > FYRTIOATTA                                  
035900        MOVE MFS-RENSA-FAELT          TO MOD-IDARTNR (RAD-INDX)           
036000        ADD +1                        TO RAD-INDX                         
036100     END-PERFORM                                                          
036200     .                                                                    
036300     EJECT                                                                
036400 S01-RENSA-SIDAN SECTION.                                                 
036500     SKIP2                                                                
036600     MOVE MFS-RENSA-FAELT            TO MOD-BEART-SVE                     
036800                                        MOD-BETFLEV                       
036900     MOVE +1                         TO RAD-INDX                          
037000     PERFORM UNTIL RAD-INDX > TRE                                         
037100        MOVE MFS-RENSA-FAELT         TO MOD-TEBYTNOT (RAD-INDX)           
037400        ADD +1                       TO RAD-INDX                          
037500     END-PERFORM                                                          
037510        MOVE MFS-RENSA-FAELT         TO MOD-TEBYTNOT (4)                  
037600     MOVE +1                         TO RAD-INDX                          
037700     PERFORM UNTIL RAD-INDX > FYRTIOATTA                                  
037800        MOVE MFS-RENSA-FAELT         TO MOD-IDARTNR (RAD-INDX)            
037900        ADD +1                       TO RAD-INDX                          
038000     END-PERFORM                                                          
038100     .                                                                    
038200     EJECT                                                                
038300* IMS SEKTIONER                                                           
038400     SKIP3                                                                
038500 IMS-GET-MSG SECTION.                                                     
038600     SKIP1                                                                
038700     MOVE '  QC' TO GODK-STATUSKODER                                      
038800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
038900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
039000     PERFORM IMS-STATUSKONTROLL                                           
039100     SKIP3                                                                
039200     .                                                                    
039300 IMS-INSERT-MSG SECTION.                                                  
039400     SKIP1                                                                
039500     IF NOT ENGLISH-TEXT                                                  
039600       MOVE '0' TO MFS-KDHUVOMR                                           
039700     END-IF                                                               
039800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
039900     MOVE SPACE TO GODK-STATUSKODER                                       
040000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
040100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
040200     PERFORM IMS-STATUSKONTROLL                                           
040300     EJECT                                                                
040400     .                                                                    
040500 IMS-GU-BENA11 SECTION.                                                   
040600     SKIP1                                                                
040700     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
040800            DELIMITED BY SIZE INTO SSA1                                   
040900     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
041000            DELIMITED BY SIZE INTO SSA2                                   
041100     MOVE '  GE' TO GODK-STATUSKODER                                      
041200     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2                 
041300     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
041400     PERFORM IMS-STATUSKONTROLL                                           
041500     SKIP3                                                                
041600     .                                                                    
041700 IMS-STATUSKONTROLL SECTION.                                              
041800     SKIP1                                                                
041900     SET STATUS-IX TO 1                                                   
042000     SEARCH GODK-STATUS                                                   
042010       AT END                                                             
042020         CALL FELLOG                                                      
042100        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                          
042200           CONTINUE                                                       
042300     END-SEARCH                                                           
042400     .                                                                    
042500     EJECT                                                                
042600 DB2-DCL-OPN-CRS-BYPRO SECTION.                                           
042700* OBS!!! DECLARE GER INGEN SQLCODE I RETUR                                
042800     EXEC SQL DECLARE BYPRO-CRS CURSOR FOR                                
042900              SELECT IDARTNR                                              
043000              FROM BYPRO                                                  
043100              WHERE IDARTNR >= :W-IDPRODNR                                
043200              AND IDARTNR_BYT = :W-IDARTNR-BYT                            
043300              ORDER BY IDARTNR                                            
043400     END-EXEC                                                             
043500     MOVE 000               TO GODK-SQLCODESKODER                         
043600     EXEC SQL OPEN BYPRO-CRS END-EXEC                                     
043700     MOVE SQLCODE           TO SQLCODE-WS                                 
043800     PERFORM DB2-STATUSKONTROLL                                           
043900     .                                                                    
044000     EJECT                                                                
044100 DB2-FETCH-BYPRO SECTION.                                                 
044200     MOVE 000100            TO GODK-SQLCODESKODER                         
044300     EXEC SQL FETCH BYPRO-CRS INTO                                        
044400            :BYPRO-IDARTNR                                                
044500     END-EXEC                                                             
044600     MOVE SQLCODE           TO SQLCODE-WS                                 
044700     PERFORM DB2-STATUSKONTROLL                                           
044800     .                                                                    
044900 DB2-SELECT-BYLEV SECTION.                                                
045000     MOVE 000100904         TO GODK-SQLCODESKODER                         
045100     EXEC SQL SELECT                                                      
045200                    BELEV,                                                
045300                    IDARTNR_BYT                                           
045400              INTO                                                        
045500                    :BYLEV-BELEV,                                         
045600                    :BYLEV-IDARTNR-BYT                                    
045700              FROM BYLEV                                                  
045800              WHERE BELEV = :W-BELEV                                      
045900     END-EXEC                                                             
046000     MOVE SQLCODE           TO SQLCODE-WS                                 
046100     PERFORM DB2-STATUSKONTROLL                                           
046200     .                                                                    
046300     EJECT                                                                
046400 DB2-SELECT-BYART SECTION.                                                
046500     MOVE 000100            TO GODK-SQLCODESKODER                         
046600     EXEC SQL SELECT                                                      
046700                  IDARTNR_BYT,                                            
046900                  BETFLEV, TEBYTNOT1,                                     
047000                  TEBYTNOT2, TEBYTNOT3,                                   
047100                  TEBYTNOT4                                               
047600              INTO                                                        
047700                  :BYART-IDARTNR-BYT,                                     
047900                  :BYART-BETFLEV, :BYART-TEBYTNOT1,                       
048000                  :BYART-TEBYTNOT2, :BYART-TEBYTNOT3,                     
048100                  :BYART-TEBYTNOT4                                        
048600            FROM BYART                                                    
048700            WHERE IDARTNR_BYT = :W-IDARTNR-BYT                            
048800     END-EXEC                                                             
048900     MOVE SQLCODE           TO SQLCODE-WS                                 
049000     PERFORM DB2-STATUSKONTROLL                                           
049100     .                                                                    
049200 DB2-CLOSE-BYPRO-CRS SECTION.                                             
049300     SKIP2                                                                
049400     EXEC SQL CLOSE BYPRO-CRS END-EXEC                                    
049500     .                                                                    
049600     EJECT                                                                
049700 DB2-STATUSKONTROLL SECTION.                                              
049800     SKIP2                                                                
049900     SET SQLCODE-IX          TO 1                                         
050000     SEARCH GODK-SQLCODE                                                  
050010       AT END                                                             
050020         CALL FELLOG                                                      
050100        WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS                       
050200           CONTINUE                                                       
050300     END-SEARCH                                                           
050400     .                                                                    
