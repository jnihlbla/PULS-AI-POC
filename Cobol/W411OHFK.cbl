000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W411OHFK.                                                
000500 AUTHOR.         LASSI OLGRENER.                                          
000600 DATE-WRITTEN.   MARS -90.                                                
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION.                                                            
001100*        PROGRAMMET GÖR FORMELL KONTROLL AV SAMTLIGA FÄLT SOM             
001200*        SKALL LÄGGAS PÅ ORDERHUVUDET.                                    
001300*        OM NÅGOT FEL UPPTÄCKS, FELMÄRKS RESP OK-FLAGGA = NEJ.            
001400*                                                                         
001500*    LÄNKAREA: W411OHFKC0                                                 
001510* STORY 2217565 / NEW NDC ROLLOUT                                         
001520* STORY 2373879 / REPLACE DIST35- 88 LEVELS OF IN,KR,                     
001530*                AE,CN CDC RETURNS TO DIST35-CDC-RETURNS-NON-VCC          
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     EJECT                                                                
002000 DATA DIVISION.                                                           
002100 WORKING-STORAGE SECTION.                                                 
002200*    -COPY WY2000W1                                                       
002300     SKIP3                                                                
002400 77  IDPGM                       PIC X(08)   VALUE 'W411OHFK'.            
002500 77  FELTEXT                     PIC X(80)    VALUE SPACE.                
002600 77  JA                          PIC X       VALUE 'J'.                   
002700 77  YES                         PIC X       VALUE 'Y'.                   
002800 77  NEJ                         PIC X       VALUE 'N'.                   
002900 77  SPEC-FORBI                  PIC X       VALUE 'S'.                   
003000 77  RKOD-ABEND-MED-DUMP     PIC S9(4)   VALUE +33 COMP SYNC.             
003010 77  CHCK-DATE-YYMMDD        PIC S9(7)  PACKED-DECIMAL VALUE ZERO.        
003100                                                                          
003200*      --- VALID IDDC CODES                                               
003300*                                                                         
003400*01    -COPY WWDC99                                                       
003500       EJECT                                                              
003600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
003700                                                                          
003800 01  WS-TIRFS-TID                PIC X(4).                                
003900 01  FILLER REDEFINES WS-TIRFS-TID.                                       
004000   03  WS-TIMMAR                 PIC X(2).                                
004100   03  WS-MINUTER                PIC X(2).                                
004200 01 DB2-LASNING.                                                          
004300     03 FILLER                   PIC X(16)   VALUE                        
004400                                             'WS-DB2-SEKTION'.            
004500     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
004600                                                                          
004700     EJECT                                                                
004800 01 NYCKLAR-TP4TRAN.                                                      
004900     03 W-TP4TRAN-IDDISTR        PIC S9(5)   COMP-3 VALUE ZERO.           
005000                                                                          
005100 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
005200       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
005300                                                                          
005400 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
005500 01  DB2-WS.                                                              
005600     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
005700         88  CURSOR-OK                       VALUE 000.                   
005800         88  RADER-FINNS                     VALUE 000.                   
005900         88  RADER-SAKNAS                    VALUE 100.                   
006000         88  ATKOMST-FEL                     VALUE 904.                   
006100     03  GODK-SQLCODEKODER.                                               
006200         05  GODK-SQLCODE OCCURS 5                                        
006300             INDEXED BY SQLCODE-IX PIC 9(3).                              
006400 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
006500     EJECT                                                                
006600     EJECT                                                                
006700                                                                          
006800 01  GENERELLA-SUBPROGRAM.                                                
006900   03  WDATKONV                PIC X(8)     VALUE 'WDATKONV'.             
007000   03  ABEND                   PIC X(8)     VALUE 'ABEND   '.             
007100                                                                          
007200*   -COPY WDATAREA.                                                       
007300*                                                                         
007400     EJECT                                                                
007500 01  TEST-IDDISTR              PIC S9(5)    COMP-3.                       
007600 01  FILLER REDEFINES TEST-IDDISTR.                                       
007700*    03     -COPY WWDIST18.                                               
007800     EJECT                                                                
007900 01  FILLER REDEFINES TEST-IDDISTR.                                       
008000*    03     -COPY WWDIST19.                                               
008100     EJECT                                                                
008200 01  FILLER REDEFINES TEST-IDDISTR.                                       
008300*    03     -COPY WWDIST35.                                               
008400     EJECT                                                                
008800 01  FILLER REDEFINES TEST-IDDISTR.                                       
008900*    03     -COPY WWDIST78.                                               
009000     EJECT                                                                
009100 01  FILLER REDEFINES TEST-IDDISTR.                                       
009200*    03     -COPY WWDIST86.                                               
009300     EJECT                                                                
009400     EJECT                                                                
009500 01  FILLER                      PIC X(16)  VALUE 'TP4TRAN-AREA'.         
009600                                                                          
009700*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
009800     EJECT                                                                
009900     EXEC SQL INCLUDE TP4TRAN END-EXEC.                                   
010000     EJECT                                                                
010100                                                                          
010200 LINKAGE SECTION.                                                         
010300*                                                                         
010400*   -COPY W411OHFK                                                        
010500*                                                                         
010600     EJECT                                                                
010700                                                                          
010800 PROCEDURE DIVISION  USING OHFK-W411OHFK.                                 
010900                                                                          
011010     PERFORM A-KOLLA-NYCKLAR                                              
011100                                                                          
011200     PERFORM B-GENERELL-KONTROLL-AV-INDATA                                
011300                                                                          
011400     PERFORM C-LOGISK-KONTROLL-AV-INDATA                                  
011500                                                                          
011600     GOBACK                                                               
011700     .                                                                    
011800     EJECT                                                                
011900                                                                          
012000 A-KOLLA-NYCKLAR SECTION.                                                 
012100                                                                          
012200     MOVE JA                   TO OHFK-IDKONTO-OK                         
012300                                  OHFK-IDKST-OK                           
012400                                  OHFK-IDANALYS-OK                        
012500                                  OHFK-IDDISTR-OK                         
012600                                  OHFK-IDKUNDNR-OK                        
012700                                  OHFK-IDORDNR-OK                         
012800                                  OHFK-KDORDKL-OK                         
012900                                  OHFK-KDFRAKT-OK                         
013000                                  OHFK-IDDC-OK                            
013100                                  OHFK-KDPROTYP-OK                        
013200                                                                          
013300     IF OHFK-IDDISTR NOT = ALL '+' AND OHFK-IDDISTR NUMERIC               
013400                                                                          
013500        MOVE OHFK-IDDISTR TO TEST-IDDISTR                                 
013600                                                                          
013700        IF OHFK-FLORDSPE = JA                                             
013800           IF DIST19-SATS OR DIST35-REFILL                                
013900                          OR DIST35-NONVCC-REFILL                         
014100                          OR DIST35-NONVCC-VCC-TRANSFER                   
014200                          OR DIST35-NONVCC-NONVCC-TRANSFER                
013901                          OR DIST35-REFILL-INOM-NDC                       
013910                          OR DIST35-CN-TRANSFER                           
014000                          OR DIST35-NA-TRANSFER                           
014010                          OR DIST35-NA-NDC-RETURNS                        
014100                          OR DIST35-NA-CDC-RETURN                         
014120                          OR DIST35-CDC-RETURNS-NON-VCC                   
014130                          OR DIST35-CN-NDC-RETURNS                        
014200                          OR DIST18-SCRAP-NDC-QUAL                        
014300                          OR ( DIST18-SKROT-KVAL-CDC AND                  
014400                            NOT (OHFK-IDSYSTEM = 'W216' OR                
014500                                 OHFK-IDSYSTEM = 'W603' OR                
014510                                 OHFK-IDSYSTEM = 'W407' ))                
014600                          OR ( DIST35-REFILL-NA-JAP AND                   
014700                             (OHFK-IDSYSTEM NOT = 'IMS ' ))               
014800              MOVE NEJ TO OHFK-IDDISTR-OK                                 
014900           END-IF                                                         
015000        ELSE                                                              
015100                                                                          
015200******************************************************************        
015300*                                                                         
015400*  KOLLA OM TRANSFER (GER SVARET RADER-FINNS)                             
015500*  GÄLLER I PRAKTIKEN BARA EU-TRANSFER NOV-05                             
015600*                                                                         
015700******************************************************************        
015800                                                                          
015900          MOVE OHFK-IDDISTR    TO W-TP4TRAN-IDDISTR                       
016000                                                                          
016100          PERFORM DB2-SELECT-TP4TRAN                                      
016200                                                                          
016300          IF DIST19-SATS OR DIST86-SPECIALORDER  OR                       
016400                           ((DIST78-EJ-ORDER         OR                   
016500                             DIST18-SCRAP-NDC-SC     OR                   
016600                             DIST18-SCRAP-NDC-QUAL   OR                   
016700                             DIST35-NA-CDC-RETURN    OR                   
016720                             DIST35-CDC-RETURNS-NON-VCC OR                
016730                             DIST35-CN-NDC-RETURNS   OR                   
016800                             DIST35-NA-TRANSFER      OR                   
016810                             DIST35-NA-NDC-RETURNS   OR                   
016900                             DIST35-PACIFIC-TRANSFER OR                   
016910                             DIST35-CN-TRANSFER      OR                   
017000                             DIST35-REFILL           OR                   
017001                             DIST35-NONVCC-REFILL    OR                   
018500                             DIST35-NONVCC-VCC-TRANSFER OR                
018600                             DIST35-NONVCC-NONVCC-TRANSFER OR             
017010                             DIST35-REFILL-INOM-JP   OR                   
017020                             DIST35-REFILL-INOM-NDC  OR                   
017100                             DIST35-REFILL-NA-JAP    OR                   
017200                             RADER-FINNS)                                 
017300                       AND (OHFK-IDSYSTEM = 'IMS ' OR '4241'))            
017400             MOVE NEJ TO OHFK-IDDISTR-OK                                  
017500          END-IF                                                          
017600        END-IF                                                            
017700     ELSE                                                                 
017800       MOVE NEJ           TO OHFK-IDDISTR-OK                              
017900     END-IF                                                               
018000     EJECT                                                                
018100     IF OHFK-IDKUNDNR NOT = ALL '+'   AND                                 
018200               OHFK-IDKUNDNR NOT NUMERIC                                  
018300                                                                          
018400        MOVE NEJ   TO OHFK-IDKUNDNR-OK                                    
018500     END-IF                                                               
018600                                                                          
018700     IF OHFK-IDORDNR NOT = ALL '+'                                        
018800        IF OHFK-IDORDNR NUMERIC AND OHFK-IDORDNR > ZERO                   
018900           CONTINUE                                                       
019000        ELSE                                                              
019100           MOVE NEJ  TO OHFK-IDORDNR-OK                                   
019200        END-IF                                                            
019300     END-IF                                                               
019400                                                                          
019500     IF OHFK-IDSYSTEM = 'PROF' OR 'LDC ' OR 'TACD'                        
019600       IF OHFK-IDSYSTEM = 'PROF'                                          
019700         IF OHFK-KDORDKL = '1' OR '2' OR '3' OR '4'                       
019800                                                                          
019900            CONTINUE                                                      
020000         ELSE                                                             
020100            MOVE NEJ   TO OHFK-KDORDKL-OK                                 
020200         END-IF                                                           
020300       END-IF                                                             
020400       IF OHFK-IDSYSTEM = 'LDC ' OR 'TACD'                                
020500         IF OHFK-KDORDKL = '1'                                            
020600         OR OHFK-KDORDKL = '2'                                            
020700         OR OHFK-KDORDKL = '3'                                            
020800                                                                          
020900            CONTINUE                                                      
021000         ELSE                                                             
021100            MOVE NEJ   TO OHFK-KDORDKL-OK                                 
021200         END-IF                                                           
021300       END-IF                                                             
021400     ELSE                                                                 
021500        IF OHFK-KDORDKL = '0' OR '1' OR '2' OR '3' OR '4'                 
021600                                                                          
021700           CONTINUE                                                       
021800        ELSE                                                              
021900           MOVE NEJ    TO OHFK-KDORDKL-OK                                 
022000        END-IF                                                            
022100     END-IF                                                               
022200                                                                          
022300     IF OHFK-KDFRAKT NOT = ALL '+'                                        
022400       IF OHFK-KDFRAKT NUMERIC AND OHFK-KDFRAKT > ZERO                    
022500         IF OHFK-KDFRAKT = '88'                                           
022600           IF OHFK-IDSYSTEM = 'SOFT'                                      
022700             CONTINUE                                                     
022800           ELSE                                                           
022900             MOVE NEJ TO OHFK-KDFRAKT-OK                                  
023000           END-IF                                                         
023100         ELSE                                                             
023200           CONTINUE                                                       
023300         END-IF                                                           
023400       ELSE                                                               
023500         MOVE NEJ  TO OHFK-KDFRAKT-OK                                     
023600       END-IF                                                             
023700     END-IF                                                               
023800                                                                          
023900     IF OHFK-IDDC NOT = ALL '+'                                           
024000       MOVE OHFK-IDDC TO WS-IDDC                                          
024100*      IF (GOOD-DC AND NOT CDC-TR) OR GOOD-DDC                            
024200       IF GOOD-DC AND NOT CDC-TR                                          
024210         CONTINUE                                                         
024700       ELSE                                                               
024800         MOVE NEJ TO OHFK-IDDC-OK                                         
024900       END-IF                                                             
025000     ELSE                                                                 
025100       IF OHFK-FLORDSPE = JA                                              
025200          MOVE NEJ TO OHFK-IDDC-OK                                        
025300       END-IF                                                             
025400     END-IF                                                               
025500     .                                                                    
025600     EJECT                                                                
025700                                                                          
025800 B-GENERELL-KONTROLL-AV-INDATA SECTION.                                   
025900                                                                          
026000     IF OHFK-FLAUTFAK NOT = ALL '+'                                       
026100       IF OHFK-FLAUTFAK = 'J'  OR  'Y'  OR  'N'                           
026200         MOVE JA  TO OHFK-FLAUTFAK-OK                                     
026300       ELSE                                                               
026400         MOVE NEJ TO OHFK-FLAUTFAK-OK                                     
026500       END-IF                                                             
026600     ELSE                                                                 
026700       MOVE JA  TO OHFK-FLAUTFAK-OK                                       
026800     END-IF                                                               
026900                                                                          
027000     IF OHFK-FLLSBOK NOT = ALL '+'                                        
027100       IF OHFK-FLLSBOK = 'J'   OR  'Y'  OR  'N'                           
027200         MOVE JA  TO OHFK-FLLSBOK-OK                                      
027300       ELSE                                                               
027400         MOVE NEJ TO OHFK-FLLSBOK-OK                                      
027500       END-IF                                                             
027600     ELSE                                                                 
027700       MOVE JA  TO OHFK-FLLSBOK-OK                                        
027800     END-IF                                                               
027900                                                                          
028000     IF OHFK-FLAUTPAC NOT = ALL '+'                                       
028100       IF OHFK-FLAUTPAC = 'J'  OR  'Y'  OR  'N'                           
028200         MOVE JA  TO OHFK-FLAUTPAC-OK                                     
028300       ELSE                                                               
028400         MOVE NEJ TO OHFK-FLAUTPAC-OK                                     
028500       END-IF                                                             
028600     ELSE                                                                 
028700       MOVE JA  TO OHFK-FLAUTPAC-OK                                       
028800     END-IF                                                               
028900     EJECT                                                                
029000     IF OHFK-FLRESTN NOT = ALL '+'                                        
029100       IF OHFK-FLRESTN =  'J'  OR  'Y'  OR  'N'                           
029200         MOVE JA  TO OHFK-FLRESTN-OK                                      
029300       ELSE                                                               
029400         MOVE NEJ TO OHFK-FLRESTN-OK                                      
029500       END-IF                                                             
029600     ELSE                                                                 
029700       MOVE JA  TO OHFK-FLRESTN-OK                                        
029800     END-IF                                                               
029900                                                                          
030000     IF OHFK-IDSKYLT NOT = ALL '+'                                        
030100       IF OHFK-GODK-IDSKYLT                                               
030200         MOVE JA  TO OHFK-IDSKYLT-OK                                      
030300       ELSE                                                               
030400         MOVE NEJ TO OHFK-IDSKYLT-OK                                      
030500       END-IF                                                             
030600     ELSE                                                                 
030700       MOVE JA  TO OHFK-IDSKYLT-OK                                        
030800     END-IF                                                               
030900                                                                          
031000     IF OHFK-IDFTG NOT = ALL '+'                                          
031100       IF OHFK-IDFTG NUMERIC                                              
031200         MOVE JA  TO OHFK-IDFTG-OK                                        
031300       ELSE                                                               
031400         MOVE NEJ TO OHFK-IDFTG-OK                                        
031500       END-IF                                                             
031600     ELSE                                                                 
031700       MOVE JA  TO OHFK-IDFTG-OK                                          
031800     END-IF                                                               
031900                                                                          
031910     IF OHFK-IDKONTO NOT = ALL '+'                                        
031920       IF OHFK-IDKONTO NOT NUMERIC                                        
031990         MOVE NEJ TO OHFK-IDKONTO-OK                                      
031991       END-IF                                                             
031998     END-IF                                                               
031999                                                                          
032000*    IF NOT NDC-US AND NOT NDC-CA                                         
032100*      IF OHFK-IDKONTO NOT = ALL '+'                                      
032200*        IF OHFK-IDKONTO NUMERIC                                          
032300*          IF OHFK-KDFAKTYP = 'G' OR 'N'                                  
032400*            MOVE JA TO OHFK-IDKONTO-OK                                   
032500*          ELSE                                                           
032600*            MOVE NEJ TO OHFK-IDKONTO-OK                                  
032700*          END-IF                                                         
032800*        ELSE                                                             
032900*          MOVE NEJ TO OHFK-IDKONTO-OK                                    
033000*        END-IF                                                           
033100*      ELSE                                                               
033200*        IF OHFK-KDFAKTYP = 'G' OR 'N'                                    
033300*          MOVE NEJ TO OHFK-IDKONTO-OK                                    
033400*        ELSE                                                             
033500*          MOVE JA  TO OHFK-IDKONTO-OK                                    
033600*        END-IF                                                           
033700*      END-IF                                                             
033800*    END-IF                                                               
033900                                                                          
034000*    IF NOT NDC-US AND NOT NDC-CA                                         
034100*      IF OHFK-IDANALYS NOT = ALL '+'                                     
034200*        IF OHFK-IDANALYS NUMERIC                                         
034300*          IF OHFK-KDFAKTYP = 'G' OR 'N'                                  
034400*            MOVE JA TO OHFK-IDANALYS-OK                                  
034500*          ELSE                                                           
034600*            MOVE NEJ TO OHFK-IDANALYS-OK                                 
034700*          END-IF                                                         
034800*        ELSE                                                             
034900*          MOVE NEJ TO OHFK-IDANALYS-OK                                   
035000*        END-IF                                                           
035100*      ELSE                                                               
035200*        IF OHFK-KDFAKTYP = 'G' OR 'N'                                    
035300*          MOVE NEJ TO OHFK-IDANALYS-OK                                   
035400*        ELSE                                                             
035500*          MOVE JA  TO OHFK-IDANALYS-OK                                   
035600*        END-IF                                                           
035700*      END-IF                                                             
035800*    END-IF                                                               
035900                                                                          
036000*    IF NOT NDC-US AND NOT NDC-CA                                         
036100*      IF OHFK-IDKST NOT = ALL '+'                                        
036200*        IF OHFK-IDKST NUMERIC                                            
036300*          IF OHFK-KDFAKTYP = 'G' OR 'N'                                  
036400*            MOVE JA  TO OHFK-IDKST-OK                                    
036500*          ELSE                                                           
036600*            MOVE NEJ TO OHFK-IDKST-OK                                    
036700*          END-IF                                                         
036800*        ELSE                                                             
036900*          MOVE NEJ TO OHFK-IDKST-OK                                      
037000*        END-IF                                                           
037100*      ELSE                                                               
037200*        IF OHFK-KDFAKTYP = 'G' OR 'N'                                    
037300*          MOVE NEJ TO OHFK-IDKST-OK                                      
037400*        ELSE                                                             
037500*          MOVE JA  TO OHFK-IDKST-OK                                      
037600*        END-IF                                                           
037700*      END-IF                                                             
037800*    END-IF                                                               
037810                                                                          
038000     IF OHFK-IDKAMPRF NOT = ALL '+'                                       
038100       IF OHFK-IDKAMPRF NUMERIC                                           
038200         MOVE JA  TO OHFK-IDKAMPRF-OK                                     
038300       ELSE                                                               
038400         MOVE NEJ TO OHFK-IDKAMPRF-OK                                     
038500       END-IF                                                             
038600     ELSE                                                                 
038700       MOVE JA  TO OHFK-IDKAMPRF-OK                                       
038800     END-IF                                                               
038900                                                                          
039000     IF OHFK-IDSYSTEM = 'PROF'                                            
039100        IF OHFK-KDFAKTYP NOT = ALL '+'                                    
039200           IF OHFK-KDFAKTYP =  'R' OR 'G' OR 'K' OR 'N'                   
039300              MOVE JA  TO OHFK-KDFAKTYP-OK                                
039400           ELSE                                                           
039500              MOVE NEJ TO OHFK-KDFAKTYP-OK                                
039600           END-IF                                                         
039700        ELSE                                                              
039800           MOVE JA  TO OHFK-KDFAKTYP-OK                                   
039900        END-IF                                                            
040000     ELSE                                                                 
040100        IF OHFK-KDFAKTYP NOT = ALL '+'                                    
040200           IF OHFK-KDFAKTYP =  'R'  OR  'G'  OR  'K'  OR  'N'             
040300              MOVE JA  TO OHFK-KDFAKTYP-OK                                
040400           ELSE                                                           
040500              MOVE NEJ TO OHFK-KDFAKTYP-OK                                
040600           END-IF                                                         
040700        ELSE                                                              
040800           MOVE JA  TO OHFK-KDFAKTYP-OK                                   
040900        END-IF                                                            
041000     END-IF                                                               
041100                                                                          
041200     IF OHFK-KDROPACK NOT = ALL '+'                                       
041300       IF (OHFK-KDROPACK = '0' OR '1' OR '2' OR '3' OR                    
041400                 '4' OR '5' OR '6' OR '7' OR '8')                         
041500          OR (OHFK-KDROPACK = 'A' OR 'B' OR 'C' OR 'D' OR                 
041600            'E' OR 'F' OR 'G' OR 'H' OR 'I' OR 'J' OR 'K' OR              
041700            'L' OR 'M' OR 'N' OR 'P')                                     
041800         MOVE JA  TO OHFK-KDROPACK-OK                                     
041900       ELSE                                                               
042000         MOVE NEJ TO OHFK-KDROPACK-OK                                     
042100       END-IF                                                             
042200     ELSE                                                                 
042300       MOVE JA  TO OHFK-KDROPACK-OK                                       
042400     END-IF                                                               
042500                                                                          
042600     IF OHFK-KDTPOTYP NOT = ALL '+' AND ZERO                              
042700       IF OHFK-KDTPOTYP NUMERIC   AND                                     
042800             (OHFK-KDTPOTYP = '1' OR '2' OR '3' OR '4')                   
042900         MOVE JA  TO OHFK-KDTPOTYP-OK                                     
043000       ELSE                                                               
043100         MOVE NEJ TO OHFK-KDTPOTYP-OK                                     
043200       END-IF                                                             
043300     ELSE                                                                 
043400       MOVE JA  TO OHFK-KDTPOTYP-OK                                       
043500     END-IF                                                               
043600                                                                          
043700     IF OHFK-KDTULLVE NOT = ALL '+'                                       
043800       IF OHFK-KDTULLVE = '0' OR '1' OR '2' OR '3' OR '4'                 
043900                              OR '5' OR '6' OR '9'                        
044000         MOVE JA  TO OHFK-KDTULLVE-OK                                     
044100       ELSE                                                               
044200         MOVE NEJ TO OHFK-KDTULLVE-OK                                     
044300       END-IF                                                             
044400     ELSE                                                                 
044500       MOVE JA  TO OHFK-KDTULLVE-OK                                       
044600     END-IF                                                               
044700                                                                          
044800     IF OHFK-KDVRINFO NOT = ALL '+'                                       
044900       IF OHFK-KDVRINFO =  '0'  OR  '1'  OR  '2'                          
045000         MOVE JA  TO OHFK-KDVRINFO-OK                                     
045100       ELSE                                                               
045200         MOVE NEJ TO OHFK-KDVRINFO-OK                                     
045300       END-IF                                                             
045400     ELSE                                                                 
045500       MOVE JA  TO OHFK-KDVRINFO-OK                                       
045600     END-IF                                                               
045700                                                                          
045800     IF OHFK-IDSYSTEM = 'PROF'                                            
045900        IF OHFK-KDPROTYP = 'F'                                            
046000           MOVE 'AAMMDD'      TO DAT-KDDATFORM                            
046100           MOVE OHFK-TIFORDAT TO DAT-I-TIDATUM                            
046200           CALL WDATKONV   USING DAT-KDDATFORM                            
046300                                 DAT-I-TIDATUM                            
046400                                 DAT-O-TIDATUM                            
046500                                 DAT-KDSVAR                               
046600           MOVE DAT-TIAAMMDD    TO TMP1-YYMMDD                            
046700           MOVE OHFK-TIREGDAT   TO TMP2-YYMMDD                            
046800           PERFORM WY2000P1                                               
046900           IF DAT-KDSVAR-OK AND                                           
047000              TMP1-YYMMDD > TMP2-YYMMDD                                   
047100              MOVE JA  TO OHFK-TIFORDAT-OK                                
047200           ELSE                                                           
047300              MOVE NEJ TO OHFK-TIFORDAT-OK                                
047400           END-IF                                                         
047500        ELSE                                                              
047600                                                                          
047700           IF OHFK-KDPROTYP = 'L' OR 'O'                                  
047800              IF OHFK-TIFORDAT NOT = ALL '+' AND ZERO                     
047900                 MOVE 'AAMMDD'      TO DAT-KDDATFORM                      
048000                 MOVE OHFK-TIFORDAT TO DAT-I-TIDATUM                      
048100                 CALL WDATKONV   USING DAT-KDDATFORM                      
048200                                       DAT-I-TIDATUM                      
048300                                       DAT-O-TIDATUM                      
048400                                       DAT-KDSVAR                         
048500                 MOVE DAT-TIAAMMDD    TO TMP1-YYMMDD                      
048600                 MOVE OHFK-TIREGDAT   TO TMP2-YYMMDD                      
048700                 PERFORM WY2000P1                                         
048800                 IF DAT-KDSVAR-OK AND                                     
048900                    TMP1-YYMMDD > TMP2-YYMMDD                             
049000                    MOVE JA  TO OHFK-TIFORDAT-OK                          
049100                 ELSE                                                     
049200                    MOVE NEJ TO OHFK-TIFORDAT-OK                          
049300                 END-IF                                                   
049400              ELSE                                                        
049500                 MOVE NEJ TO OHFK-TIFORDAT-OK                             
049600              END-IF                                                      
049700           END-IF                                                         
049800        END-IF                                                            
049900     END-IF                                                               
050000                                                                          
050100     IF OHFK-TITPO NOT = ALL '+'  AND  ZERO                               
050200       MOVE 'AAMMDD'    TO DAT-KDDATFORM                                  
050300       MOVE OHFK-TITPO  TO DAT-I-TIDATUM                                  
050400       CALL WDATKONV USING DAT-KDDATFORM                                  
050500                           DAT-I-TIDATUM                                  
050600                           DAT-O-TIDATUM                                  
050700                           DAT-KDSVAR                                     
050800       IF DAT-KDSVAR-OK                                                   
050900         MOVE JA  TO OHFK-TITPO-OK                                        
051000       ELSE                                                               
051100         MOVE NEJ TO OHFK-TITPO-OK                                        
051200       END-IF                                                             
051300     ELSE                                                                 
051400       MOVE JA  TO OHFK-TITPO-OK                                          
051500     END-IF                                                               
051600                                                                          
051700     IF ((OHFK-TIRFSDAT NOT = ALL '+' AND                                 
051800       OHFK-TIRFSTID = ALL '+') OR                                        
051900        (OHFK-TIRFSDAT = ALL '+' AND                                      
052000       OHFK-TIRFSTID NOT = ALL '+')) AND                                  
052100       (OHFK-IDSYSTEM = 'IMS ' OR '4241')                                 
052200       MOVE NEJ TO OHFK-TIRFSDAT-OK                                       
052300                   OHFK-TIRFSTID-OK                                       
052400     ELSE                                                                 
052500       IF OHFK-TIRFSDAT NOT = ALL '+' AND ZERO                            
052600       AND OHFK-TIRFSDAT NUMERIC                                          
052700         MOVE 'AAMMDD'     TO DAT-KDDATFORM                               
052800         MOVE OHFK-TIRFSDAT TO DAT-I-TIDATUM                              
052900         CALL WDATKONV USING DAT-KDDATFORM                                
053000                             DAT-I-TIDATUM                                
053100                             DAT-O-TIDATUM                                
053200                             DAT-KDSVAR                                   
053300         MOVE OHFK-TIRFSDAT TO TMP1-YYMMDD                                
053400         MOVE OHFK-TIREGDAT  TO TMP2-YYMMDD                               
053500         PERFORM WY2000P1                                                 
053600         IF DAT-KDSVAR-OK AND (TMP1-YYMMDD >= TMP2-YYMMDD )               
053700           MOVE JA TO OHFK-TIRFSDAT-OK                                    
053710****   CHECKS IF RFS DATE IS MORE THEN 3 YEARS FROM CURRENT DATE          
053711****        BECAUSE WORKDAY HAS DATA FOR 3 YEARS.                         
053712                                                                          
053713           COMPUTE CHCK-DATE-YYMMDD =                                     
053720                                 TMP2-YYMMDD  + 30000                     
053734                                                                          
053735           IF CHCK-DATE-YYMMDD < TMP1-YYMMDD                              
053737             MOVE NEJ TO OHFK-TIRFSDAT-OK                                 
053738                                                                          
053740           ELSE                                                           
053750             IF OHFK-TIRFSTID NOT = ALL '+' AND ZERO                      
053760               MOVE OHFK-TIRFSTID TO WS-TIRFS-TID                         
053770                 IF WS-TIRFS-TID NUMERIC AND                              
053780                    WS-TIMMAR < '24'  AND  WS-MINUTER  <  '60'            
053790                   IF OHFK-TIRFSDAT = OHFK-TIREGDAT                       
053800                     IF OHFK-TIRFSTID > OHFK-TIHHMM                       
053900                         MOVE JA TO OHFK-TIRFSTID-OK                      
054000                     ELSE                                                 
054100                         MOVE NEJ TO OHFK-TIRFSTID-OK                     
054200                     END-IF                                               
054300                   ELSE                                                   
054400                       MOVE JA TO OHFK-TIRFSTID-OK                        
054500                   END-IF                                                 
054600                 ELSE                                                     
054700                     MOVE NEJ TO OHFK-TIRFSTID-OK                         
054800                 END-IF                                                   
054900             ELSE                                                         
055000               MOVE NEJ TO OHFK-TIRFSTID-OK                               
055100             END-IF                                                       
055110           END-IF                                                         
055200         ELSE                                                             
055300           MOVE NEJ TO OHFK-TIRFSDAT-OK                                   
055400         END-IF                                                           
055500       ELSE                                                               
055600         IF OHFK-TIRFSTID NOT = ALL '+' AND ZERO                          
055700            MOVE NEJ TO OHFK-TIRFSDAT-OK                                  
055800         ELSE                                                             
055900            MOVE JA TO OHFK-TIRFSDAT-OK                                   
056000         END-IF                                                           
056100       END-IF                                                             
056200     END-IF                                                               
056800     .                                                                    
056900     EJECT                                                                
057000                                                                          
057100 C-LOGISK-KONTROLL-AV-INDATA SECTION.                                     
057200                                                                          
057300     IF OHFK-FLRESTN = NEJ                                                
057400        IF OHFK-KDROPACK NOT = '+' AND ZERO                               
057500           MOVE NEJ TO OHFK-FLRESTN-OK                                    
057600           MOVE NEJ TO OHFK-KDROPACK-OK                                   
057700        END-IF                                                            
057800     END-IF                                                               
057900                                                                          
058000     IF OHFK-FLRESTN = JA AND OHFK-KDORDKL = ZERO                         
058100        MOVE NEJ    TO OHFK-FLRESTN-OK                                    
058200        MOVE NEJ    TO OHFK-KDORDKL-OK                                    
058300     END-IF                                                               
058400                                                                          
058500     IF OHFK-FLRESTN = NEJ AND OHFK-KDORDKL = '1'                         
058600                           AND OHFK-IDSYSTEM = '4241'                     
058700        MOVE NEJ    TO OHFK-FLRESTN-OK                                    
058800        MOVE NEJ    TO OHFK-KDORDKL-OK                                    
058900     END-IF                                                               
059000                                                                          
059100     IF OHFK-IDKAMPRF NOT = ALL '+'  AND   ZERO                           
059200                                                                          
059300       IF OHFK-KDORDKL = ZERO  OR  ALL '+'                                
059400         MOVE NEJ TO OHFK-KDORDKL-OK                                      
059500       END-IF                                                             
059600     END-IF                                                               
059700                                                                          
059800     PERFORM CB-KOLLA-KDROPACK-IDBIPREF                                   
059900     PERFORM CC-KOLLA-KDTPOTYP-TITPO                                      
060000     PERFORM CD-KOLLA-KDORDKL-VOR                                         
060100     PERFORM CE-KOLLA-KDPROTYP                                            
060200     .                                                                    
060300     EJECT                                                                
060400                                                                          
060500 CB-KOLLA-KDROPACK-IDBIPREF SECTION.                                      
060600                                                                          
060700     MOVE JA  TO OHFK-IDBIPREF-OK                                         
060800                                                                          
060900     IF (OHFK-KDROPACK = '5' OR '8' OR 'L')  AND                          
061000                  (OHFK-IDBIPREF = SPACE OR ALL '+')                      
061100       MOVE NEJ TO OHFK-IDBIPREF-OK                                       
061200     END-IF                                                               
061300                                                                          
061400     IF OHFK-KDTPOTYP NOT = ALL '+' AND ZERO                              
061500       IF OHFK-KDROPACK NOT = ALL '+' AND ZERO                            
061600         MOVE NEJ TO OHFK-KDROPACK-OK                                     
061700       END-IF                                                             
061800                                                                          
061900       IF OHFK-IDBIPREF NOT = ALL '+' AND SPACE                           
062000         MOVE NEJ TO OHFK-IDBIPREF-OK                                     
062100       END-IF                                                             
062200                                                                          
062300     END-IF                                                               
062400     .                                                                    
062500     EJECT                                                                
062600                                                                          
062700 CC-KOLLA-KDTPOTYP-TITPO SECTION.                                         
062800                                                                          
062900     IF OHFK-KDTPOTYP = ALL '+' OR ZERO                                   
063000       IF OHFK-TITPO =  ALL '+' OR ZERO                                   
063100         CONTINUE                                                         
063200       ELSE                                                               
063300         MOVE NEJ TO OHFK-TITPO-OK                                        
063400       END-IF                                                             
063500     END-IF                                                               
063600     .                                                                    
063700     EJECT                                                                
063800                                                                          
063900 CD-KOLLA-KDORDKL-VOR SECTION.                                            
064000                                                                          
064100     IF OHFK-FLFORBI = JA                                                 
064200     OR OHFK-FLFORBI = SPEC-FORBI                                         
064300     OR OHFK-FLVORKO = JA                                                 
064400     OR OHFK-FLVORKO = YES                                                
064500*      IF OHFK-KDORDKL NOT = '0' AND '1'                                  
064600* FÖR LDC-KONCEPTET BEHÖVER MAN KUNNA ANVÄNDA ALLA ORDERKLASSER           
064700* FÖR FÖRBIORDER  TINA051230                                              
064800       IF OHFK-KDORDKL NOT = '0' AND '1' AND '2' AND '3' AND '4'          
064900*******                                                                   
065000         MOVE NEJ TO OHFK-KDORDKL-OK                                      
065100       END-IF                                                             
065200       IF (NDC-NA OR NDC-PACIFIC OR NDC-NX OR NDC-NS) AND                 
065210          (OHFK-FLFORBI = JA OR OHFK-FLFORBI = SPEC-FORBI)                
065300         IF OHFK-KDORDKL NOT = '0'                                        
065400           MOVE NEJ TO OHFK-KDORDKL-OK                                    
065500         END-IF                                                           
065600       END-IF                                                             
065700     END-IF                                                               
065800     .                                                                    
065900     EJECT                                                                
066000 CE-KOLLA-KDPROTYP SECTION.                                               
066100                                                                          
066200     IF OHFK-IDSYSTEM = 'PROF'                                            
066300        IF OHFK-KDPROTYP NOT = 'F' AND 'L' AND 'O'                        
066400           MOVE NEJ TO OHFK-KDPROTYP-OK                                   
066500        END-IF                                                            
066600     END-IF                                                               
066700     .                                                                    
066800     EJECT                                                                
066900 DB2-SELECT-TP4TRAN     SECTION.                                          
067000     MOVE 'DB2-SELECT-TP4TRAN   ' TO  WS-DB2-SEKTION                      
067100                                                                          
067200     MOVE 000100 TO GODK-SQLCODEKODER                                     
067300                                                                          
067400     EXEC SQL                                                             
067500           SELECT  DISTINCT                                               
067600                   IDDC_REC                                               
067700                                                                          
067800           INTO   :TP4TRAN-IDDC-REC                                       
067900                                                                          
068000           FROM    TP4TRAN                                                
068100                                                                          
068200           WHERE IDDISTR   = :W-TP4TRAN-IDDISTR                           
068300     END-EXEC                                                             
068400                                                                          
068500     MOVE SQLCODE TO SQLCODE-WS                                           
068600     PERFORM DB2-STATUSKONTROLL                                           
068700     .                                                                    
068800     EJECT                                                                
068900 DB2-STATUSKONTROLL  SECTION.                                             
069000                                                                          
069100     SET SQLCODE-IX TO 1                                                  
069200     SEARCH GODK-SQLCODE                                                  
069300       AT END                                                             
069400          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
069500          DELIMITED BY SIZE INTO FELTEXT                                  
069600          CALL ABEND USING RKOD-ABEND-DB2                                 
069700       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
069800     END-SEARCH                                                           
069900     .                                                                    
070000     EJECT                                                                
070100*    -COPY WY2000P1                                                       
