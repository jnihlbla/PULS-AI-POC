000100 PROCESS DYNAM                                                            
000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W9020200.                                                
000400 AUTHOR.         BO HAMMARIN.                                             
000500 DATE-WRITTEN.   AUG 2000.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAMN:       CARPARTS.PULS.RADIOQUERY                                 
000900*                                                                         
001000*    PGM OMSKRIVET AV    SUSANNE OLSSON.                                  
001100*    DATE-WRITTEN.   03/03/28.                                            
001200*                                                                         
001300*    FUNKTION:                                                            
001400*        FRÅGA KOMMER IN FRÅN VPS (VCOM) ELLER VIPS/VV (VIA SNUF).        
001500*        PROGRAMMET HÄMTAR RADIOKODEN FRÅN RADIOREGISTER I DB2-           
001600*        TABELL OCH SKICKAR TILLBAKA VIA WZ01 (SYNCHRONOUS).              
001700*                                                                         
001800*        PROGRAMMET LÄSER      TABELL FRADIO                              
001900*        PROGRAMMET LÄSER      TABELL WRADIOT                             
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSAKTION: W90202T                                             
002300*        REQUEST:     W9I20201                                            
002400*                                                                         
002500*    UTDATA.                                                              
002600*        RESPONSE:    W9O20201                                            
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 DATA DIVISION.                                                           
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400 77  IDPGM                       PIC X(08)   VALUE 'W9020200'.            
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003700 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
003800 01  IX                          PIC 9       VALUE ZERO.                  
003900 01  MAX-IX                      PIC 9       VALUE 3.                     
004000                                                                          
004100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004300 77  KDRC-DISPLAY                PIC Z(5).                                
004400                                                                          
004500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004600     88  NYCKLAR-OK                          VALUE 'J'.                   
004700     88  NYCKLAR-FEL                         VALUE 'N'.                   
004800     EJECT                                                                
004900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
005000 01  GENERELLA-SUBPROGRAM.                                                
005100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005300     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
005400     SKIP3                                                                
005500*    --- PARAMETRAR TILL ABEND                                            
005600                                                                          
005700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
005900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006000 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
006100     SKIP3                                                                
006200 01  MESSAGE-CODES.                                                       
006300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
006400     EJECT                                                                
006500*                                                                         
006600 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
006700     SKIP3                                                                
006800*01  -COPY WZ01SUB                                                        
006900     EJECT                                                                
007000 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
007100     SKIP3                                                                
007200 01  REQU-AREA.                                                           
007300*    03  -COPY WMSGMIDP -PRE REQU-                                        
007400*    03  -COPY W9I20201 -PRE REQU-                                        
007500     EJECT                                                                
007600 01  PREFIX-AREA.                                                         
007700*    03  FILLER -COPY WMSGMIDP                                            
007800     EJECT                                                                
007900 01  MID-AREA.                                                            
008000*    03  FILLER -COPY W9I20201                                            
008100     EJECT                                                                
008200                                                                          
008300 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
008400     SKIP3                                                                
008500 01  RESP-AREA.                                                           
008600*    03  -COPY W9O20201                                                   
008700     EJECT                                                                
008800                                                                          
008900 01  DIVERSE.                                                             
009000     05  W-IDARTNR-NUM           PIC  9(9).                               
009100 01  NYCKLAR-TILL-DB2.                                                    
009200     05  W-SERIENR-DB2           PIC  X(10).                              
009300     05  W-APPARATTYP-DB2        PIC  X(11).                              
009400     05  W-IDARTNR-DB2           PIC S9(10) COMP-3.                       
009500                                                                          
009600     EJECT                                                                
009700 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
009800       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
009900                                                                          
010000 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
010100 01  DB2-WS.                                                              
010200     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
010300         88  CURSOR-OK                       VALUE 000.                   
010400         88  RADER-FINNS                     VALUE 000.                   
010500         88  RADER-SAKNAS                    VALUE 100.                   
010600         88  ATKOMST-FEL                     VALUE 904.                   
010700         88  DUBBLA-RADER                    VALUE 811.                   
010800     03  GODK-SQLCODEKODER.                                               
010900         05  GODK-SQLCODE OCCURS 5                                        
011000             INDEXED BY SQLCODE-IX PIC 9(3).                              
011100                                                                          
011200 01  FILLER                  PIC X(16) VALUE 'TEST-SQLCODE'.              
011300 01  TEST-SQLCODE            PIC 9(3)  VALUE ZERO.                        
011400     EJECT                                                                
011500                                                                          
011600 01  FILLER                      PIC X(16)  VALUE 'FRADIO-AREA'.          
011700                                                                          
011800*01  -COPY FRADIO -PRE FRADIO-                                            
011900     EJECT                                                                
012000 01  FILLER                      PIC X(16)  VALUE 'WRADIOT-AREA'.         
012100                                                                          
012200*01  -COPY WRADIOT -PRE WRADIOT-                                          
012300     EJECT                                                                
012400     EXEC SQL INCLUDE FRADIO END-EXEC.                                    
012500     EJECT                                                                
012600     EXEC SQL INCLUDE WRADIOT END-EXEC.                                   
012700     EJECT                                                                
012800 PROCEDURE DIVISION.                                                      
012900 MAIN SECTION.                                                            
013000                                                                          
013100     PERFORM S01-HAEMTA-ANROPSDATA                                        
013200     IF SUB-KDRC = 0                                                      
013300       PERFORM A-INIT                                                     
013400       PERFORM B-KOLLA-NYCKLAR                                            
013500       IF NYCKLAR-OK                                                      
013600         PERFORM C-EXECUTE                                                
013700       END-IF                                                             
013800       PERFORM S02-RETURNERA-SVAR                                         
013900     END-IF                                                               
014000                                                                          
014100                                                                          
014200     MOVE ZERO TO RETURN-CODE                                             
014300     GOBACK                                                               
014400     .                                                                    
014500     EJECT                                                                
014600 A-INIT SECTION.                                                          
014700                                                                          
015000                                                                          
015100     ACCEPT DAGENS-DATUM FROM DATE                                        
015200                                                                          
015300     INITIALIZE GODK-SQLCODEKODER                                         
015310     INITIALIZE RESP-AREA                                                 
015311                                                                          
015320     MOVE '9202'                       TO MOD-IDTRANS                     
015330     MOVE ZERO                         TO MOD-IDMFSFEL                    
015400                                                                          
015500     IF REQU-MID-IDTRANS = '9202' AND                                     
015600        REQU-MID-KDMFSFOR = '1' OR '2' OR '3'                             
015700                                                                          
015800       MOVE REQU-MID-WMSGMIDP TO PREFIX-AREA                              
015900       MOVE REQU-MID-W9I20201 TO MID-AREA                                 
016000     ELSE                                                                 
016100       MOVE SPACE TO PREFIX-AREA                                          
016200       MOVE REQU-AREA TO MID-AREA                                         
016300     END-IF                                                               
016400     .                                                                    
016500     EJECT                                                                
016600 B-KOLLA-NYCKLAR SECTION.                                                 
016700                                                                          
016800     MOVE JA TO NYCKLAR-SW                                                
016900                                                                          
017000     IF MID-IDARTNR NUMERIC                                               
017100        MOVE MID-IDARTNR    TO W-IDARTNR-NUM                              
017200     ELSE                                                                 
017300        MOVE NEJ            TO NYCKLAR-SW                                 
017400        MOVE 'B01'          TO MOD-IDMFSFEL                               
017500     END-IF                                                               
017600     MOVE MID-IDARTNR       TO MOD-IDARTNR                                
017700     MOVE MID-SERIENR       TO MOD-SERIENR                                
017800     MOVE MID-APPARATTYP    TO MOD-APPARATTYP                             
017900     .                                                                    
018000     EJECT                                                                
018100 C-EXECUTE SECTION.                                                       
018200                                                                          
018300     MOVE W-IDARTNR-NUM        TO W-IDARTNR-DB2                           
018400     MOVE MID-SERIENR          TO W-SERIENR-DB2                           
018500     MOVE MID-APPARATTYP       TO W-APPARATTYP-DB2                        
018600     MOVE +1                   TO IX                                      
018700                                                                          
018800     IF W-APPARATTYP-DB2 = SPACE OR ALL '+'                               
018900       PERFORM DB2-OPEN-ALT1-CURS                                         
019000       PERFORM DB2-FETCH-ALT1-CURS                                        
019100       PERFORM UNTIL RADER-SAKNAS OR IX > MAX-IX                          
019200         IF SQLCODE = ZERO                                                
019300           MOVE FRADIO-SKYDDSKOD TO MOD-SKYDDSKOD(IX)                     
019400           MOVE SPACE            TO MOD-IDMFSFEL                          
019500         ELSE                                                             
019600             MOVE SPACE          TO MOD-SKYDDSKOD(IX)                     
019700             MOVE 'B10'          TO MOD-IDMFSFEL                          
019800         END-IF                                                           
019900         ADD +1                  TO IX                                    
020000         PERFORM DB2-FETCH-ALT1-CURS                                      
020100       END-PERFORM                                                        
020110       IF RADER-SAKNAS AND IX = 1                                         
020111         MOVE SPACE          TO MOD-SKYDDSKOD(IX)                         
020112         MOVE 'B10'          TO MOD-IDMFSFEL                              
020113       END-IF                                                             
020120                                                                          
020200       PERFORM DB2-CLOSE-ALT1-CURS                                        
020300                                                                          
020400       PERFORM UNTIL IX > MAX-IX                                          
020500         ADD +1 TO IX                                                     
020600         MOVE SPACE TO MOD-SKYDDSKOD(IX)                                  
020700       END-PERFORM                                                        
020800     ELSE                                                                 
020900       IF W-IDARTNR-DB2 = ZERO                                            
021000**     PERFORM DB2-SELECT-RADIOCODE-ALT2                                  
021100         PERFORM DB2-OPEN-ALT2-CURS                                       
021200         PERFORM DB2-FETCH-ALT2-CURS                                      
021300         PERFORM UNTIL RADER-SAKNAS OR IX > MAX-IX                        
021400           IF SQLCODE = ZERO                                              
021500             MOVE FRADIO-SKYDDSKOD TO MOD-SKYDDSKOD(IX)                   
021600             MOVE SPACE            TO MOD-IDMFSFEL                        
021700           ELSE                                                           
021800               MOVE SPACE          TO MOD-SKYDDSKOD(IX)                   
021900               MOVE 'B10'          TO MOD-IDMFSFEL                        
022000           END-IF                                                         
022100           ADD +1                  TO IX                                  
022200           PERFORM DB2-FETCH-ALT2-CURS                                    
022300         END-PERFORM                                                      
022310         IF RADER-SAKNAS AND IX = 1                                       
022320           MOVE SPACE          TO MOD-SKYDDSKOD(IX)                       
022330           MOVE 'B10'          TO MOD-IDMFSFEL                            
022340         END-IF                                                           
022400         PERFORM DB2-CLOSE-ALT2-CURS                                      
022500                                                                          
022600         PERFORM UNTIL IX > MAX-IX                                        
022700           ADD +1 TO IX                                                   
022800           MOVE SPACE TO MOD-SKYDDSKOD(IX)                                
022900         END-PERFORM                                                      
023000       ELSE                                                               
023100*        PERFORM DB2-SELECT-RADIOCODE-ALT3                                
023200         PERFORM DB2-OPEN-ALT3-CURS                                       
023300         PERFORM DB2-FETCH-ALT3-CURS                                      
023400         PERFORM UNTIL RADER-SAKNAS OR IX > MAX-IX                        
023500           IF SQLCODE = ZERO                                              
023600             MOVE FRADIO-SKYDDSKOD TO MOD-SKYDDSKOD(IX)                   
023700             MOVE SPACE            TO MOD-IDMFSFEL                        
023800           ELSE                                                           
023900               MOVE SPACE          TO MOD-SKYDDSKOD(IX)                   
024000               MOVE 'B10'          TO MOD-IDMFSFEL                        
024100           END-IF                                                         
024200           ADD +1                  TO IX                                  
024300           PERFORM DB2-FETCH-ALT3-CURS                                    
024400         END-PERFORM                                                      
024410         IF RADER-SAKNAS AND IX = 1                                       
024420           MOVE SPACE          TO MOD-SKYDDSKOD(IX)                       
024430           MOVE 'B10'          TO MOD-IDMFSFEL                            
024440         END-IF                                                           
024500         PERFORM DB2-CLOSE-ALT3-CURS                                      
024600                                                                          
024700         PERFORM UNTIL IX > MAX-IX                                        
024800           ADD +1 TO IX                                                   
024900           MOVE SPACE TO MOD-SKYDDSKOD(IX)                                
025000         END-PERFORM                                                      
025100       END-IF                                                             
025200     END-IF                                                               
025300     .                                                                    
025400     EJECT                                                                
025500                                                                          
025600*    --- DISPATCHER-SEKTIONER                                             
025700 S01-HAEMTA-ANROPSDATA SECTION.                                           
025800                                                                          
025900     MOVE 'GETARG'               TO SUB-KDFUNC                            
026000     MOVE 'CARPARTS.PULS.RADIOQUERY'   TO SUB-ADDISPABS                   
026100     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
026200                                                                          
026300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
026400                                                                          
026500     IF SUB-KDRC > 0                                                      
026600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
026700       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
026800       DELIMITED BY SIZE INTO FELTEXT                                     
026900       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
027000     END-IF                                                               
027100     .                                                                    
027200     SKIP3                                                                
027300 S02-RETURNERA-SVAR SECTION.                                              
027400                                                                          
027500     MOVE 'RETURN'                   TO SUB-KDFUNC                        
027600     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
027700                                                                          
027800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
027900                                                                          
028000     IF SUB-KDRC > 0                                                      
028100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
028200       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
028300       DELIMITED BY SIZE INTO FELTEXT                                     
028400       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
028500     END-IF                                                               
028600     .                                                                    
028700     EJECT                                                                
028800 DB2-OPEN-ALT1-CURS SECTION.                                              
028900     MOVE 000100 TO GODK-SQLCODEKODER                                     
029000     EXEC SQL DECLARE ALT1-CUR CURSOR FOR                                 
029100       SELECT FRADIO.SKYDDSKOD                                            
029200       FROM    FRADIO,                                                    
029300               WRADIOT                                                    
029400       WHERE   FRADIO.APPARATTYP     = WRADIOT.APPARATTYP AND             
029500               FRADIO.SERIENR        = :W-SERIENR-DB2     AND             
029600               WRADIOT.IDARTNR_RADIO = :W-IDARTNR-DB2                     
029700                                                                          
029800     END-EXEC                                                             
029900     MOVE 000100 TO GODK-SQLCODEKODER                                     
030000     EXEC SQL OPEN ALT1-CUR END-EXEC                                      
030100                                                                          
030200     .                                                                    
030300     EJECT                                                                
030400 DB2-FETCH-ALT1-CURS SECTION.                                             
030500     MOVE 000100 TO GODK-SQLCODEKODER                                     
030600     EXEC SQL FETCH  ALT1-CUR INTO                                        
030700       :FRADIO-SKYDDSKOD                                                  
030800     END-EXEC                                                             
030900*    EXEC SQL OPEN ALT1-CUR END-EXEC                                      
031000     MOVE SQLCODE TO SQLCODE-WS                                           
031100                     TEST-SQLCODE                                         
031200                                                                          
031300     PERFORM DB2-STATUSCONTROL                                            
031400     .                                                                    
031500     EJECT                                                                
031600 DB2-CLOSE-ALT1-CURS SECTION.                                             
031700                                                                          
031800     EXEC SQL CLOSE  ALT1-CUR END-EXEC                                    
031900     .                                                                    
032000     EJECT                                                                
032100 DB2-OPEN-ALT2-CURS SECTION.                                              
032200     MOVE 000100 TO GODK-SQLCODEKODER                                     
032300     EXEC SQL DECLARE ALT2-CUR CURSOR FOR                                 
032400       SELECT FRADIO.SKYDDSKOD                                            
032500       FROM    FRADIO,                                                    
032600               WRADIOT                                                    
032700                                                                          
032800        WHERE   FRADIO.APPARATTYP  = WRADIOT.APPARATTYP AND               
032900                FRADIO.SERIENR     = :W-SERIENR-DB2     AND               
033000                WRADIOT.APPARATTYP = :W-APPARATTYP-DB2                    
033100     END-EXEC                                                             
033200     MOVE 000100 TO GODK-SQLCODEKODER                                     
033300     EXEC SQL OPEN ALT2-CUR END-EXEC                                      
033400                                                                          
033500     .                                                                    
033600     EJECT                                                                
033700 DB2-FETCH-ALT2-CURS SECTION.                                             
033800     MOVE 000100 TO GODK-SQLCODEKODER                                     
033900     EXEC SQL FETCH  ALT2-CUR INTO                                        
034000       :FRADIO-SKYDDSKOD                                                  
034100     END-EXEC                                                             
034200*    EXEC SQL OPEN ALT2-CUR END-EXEC                                      
034300     MOVE SQLCODE TO SQLCODE-WS                                           
034400                     TEST-SQLCODE                                         
034500                                                                          
034600     PERFORM DB2-STATUSCONTROL                                            
034700     .                                                                    
034800     EJECT                                                                
034900 DB2-CLOSE-ALT2-CURS SECTION.                                             
035000                                                                          
035100     EXEC SQL CLOSE  ALT2-CUR END-EXEC                                    
035200     .                                                                    
035300     EJECT                                                                
035400 DB2-OPEN-ALT3-CURS SECTION.                                              
035500     MOVE 000100 TO GODK-SQLCODEKODER                                     
035600     EXEC SQL DECLARE ALT3-CUR CURSOR FOR                                 
035700       SELECT FRADIO.SKYDDSKOD                                            
035800       FROM    FRADIO,                                                    
035900               WRADIOT                                                    
036000                                                                          
036100           WHERE   FRADIO.APPARATTYP     = WRADIOT.APPARATTYP AND         
036200                   FRADIO.SERIENR        = :W-SERIENR-DB2     AND         
036300                   FRADIO.APPARATTYP     = :W-APPARATTYP-DB2  AND         
036400                   WRADIOT.IDARTNR_RADIO = :W-IDARTNR-DB2                 
036500     END-EXEC                                                             
036600     MOVE 000100 TO GODK-SQLCODEKODER                                     
036700     EXEC SQL OPEN ALT3-CUR END-EXEC                                      
036800                                                                          
036900     .                                                                    
037000     EJECT                                                                
037100 DB2-FETCH-ALT3-CURS SECTION.                                             
037200     MOVE 000100 TO GODK-SQLCODEKODER                                     
037300     EXEC SQL FETCH  ALT3-CUR INTO                                        
037400       :FRADIO-SKYDDSKOD                                                  
037500     END-EXEC                                                             
037600*    EXEC SQL OPEN ALT3-CUR END-EXEC                                      
037700     MOVE SQLCODE TO SQLCODE-WS                                           
037800                     TEST-SQLCODE                                         
037900                                                                          
038000     PERFORM DB2-STATUSCONTROL                                            
038100     .                                                                    
038200     EJECT                                                                
038300 DB2-CLOSE-ALT3-CURS SECTION.                                             
038400                                                                          
038500     EXEC SQL CLOSE  ALT3-CUR END-EXEC                                    
038600     .                                                                    
038700     EJECT                                                                
038800 DB2-SELECT-RADIOCODE-ALT1 SECTION.                                       
038900                                                                          
039000     MOVE 000100811               TO GODK-SQLCODEKODER                    
039100                                                                          
039200     EXEC SQL                                                             
039300           SELECT  FRADIO.SKYDDSKOD                                       
039400           INTO   :FRADIO-SKYDDSKOD                                       
039500           FROM    FRADIO,                                                
039600                   WRADIOT                                                
039700           WHERE   FRADIO.APPARATTYP     = WRADIOT.APPARATTYP AND         
039800                   FRADIO.SERIENR        = :W-SERIENR-DB2     AND         
039900                   WRADIOT.IDARTNR_RADIO = :W-IDARTNR-DB2                 
040000     END-EXEC                                                             
040100                                                                          
040200     MOVE SQLCODE                 TO SQLCODE-WS                           
040300                                     TEST-SQLCODE                         
040400     PERFORM DB2-STATUSCONTROL                                            
040500     .                                                                    
040600     EJECT                                                                
040700 DB2-SELECT-RADIOCODE-ALT2 SECTION.                                       
040800                                                                          
040900     MOVE 000100811               TO GODK-SQLCODEKODER                    
041000                                                                          
041100     EXEC SQL                                                             
041200           SELECT  FRADIO.SKYDDSKOD                                       
041300           INTO   :FRADIO-SKYDDSKOD                                       
041400           FROM    FRADIO,                                                
041500                   WRADIOT                                                
041600           WHERE   FRADIO.APPARATTYP  = WRADIOT.APPARATTYP AND            
041700                   FRADIO.SERIENR     = :W-SERIENR-DB2     AND            
041800                   WRADIOT.APPARATTYP = :W-APPARATTYP-DB2                 
041900     END-EXEC                                                             
042000                                                                          
042100     MOVE SQLCODE                 TO SQLCODE-WS                           
042200                                     TEST-SQLCODE                         
042300     PERFORM DB2-STATUSCONTROL                                            
042400     .                                                                    
042500     EJECT                                                                
042600 DB2-SELECT-RADIOCODE-ALT3 SECTION.                                       
042700                                                                          
042800     MOVE 000100                  TO GODK-SQLCODEKODER                    
042900                                                                          
043000     EXEC SQL                                                             
043100           SELECT  FRADIO.SKYDDSKOD                                       
043200           INTO   :FRADIO-SKYDDSKOD                                       
043300           FROM    FRADIO,                                                
043400                   WRADIOT                                                
043500           WHERE   FRADIO.APPARATTYP     = WRADIOT.APPARATTYP AND         
043600                   FRADIO.SERIENR        = :W-SERIENR-DB2     AND         
043700                   FRADIO.APPARATTYP     = :W-APPARATTYP-DB2  AND         
043800                   WRADIOT.IDARTNR_RADIO = :W-IDARTNR-DB2                 
043900     END-EXEC                                                             
044000                                                                          
044100     MOVE SQLCODE                 TO SQLCODE-WS                           
044200                                     TEST-SQLCODE                         
044300     PERFORM DB2-STATUSCONTROL                                            
044400     .                                                                    
044500     EJECT                                                                
044600 DB2-STATUSCONTROL  SECTION.                                              
044700                                                                          
044800     SET SQLCODE-IX TO 1                                                  
044900     SEARCH GODK-SQLCODE                                                  
045000       AT END                                                             
045100          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
045200          DELIMITED BY SIZE INTO FELTEXT                                  
045300          CALL ABEND USING RKOD-ABEND-DB2                                 
045400       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
045500     END-SEARCH                                                           
045600     .                                                                    
