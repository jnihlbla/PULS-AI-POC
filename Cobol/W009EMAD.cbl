000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W009EMAD.                                                
000300 AUTHOR.         JACOB WEINBERG.                                          
000400 DATE-WRITTEN.   00/05/10.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        SUBPROGRAM TO VALIDATE OR COMPLETE AN EMAIL ADDRESS.             
001000*                                                                         
001100*    RETURNS COMPLETE, LEFT ADJUSTED ADDRESS STRING.                      
001200*    RETURN CODES (IN EMAD-KDSVAR):                                       
001300*        SPACE - OK.                                                      
001400*        'F'   - ERROR FOUND.                                             
001500*                                                                         
001600*        CCID 10277114 - memo.volvo.se REMOVED AS VALID                   
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP3                                                                
002700 FILE SECTION.                                                            
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
003000                                                                          
003100                                                                          
003200*    -- CHECKED BY WY2000                                                 
003300 77  IDPGM                       PIC X(8)    VALUE 'W009EMAD'.            
003400 77  FELNR                       PIC 9(2).                                
003500     EJECT                                                                
003600 01  ERRTEXT.                                                             
003700     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
003800     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
003900                                                                          
004000 01  SMALL-LETTERS              PIC X(31)  VALUE                          
004100     'abcdefghijklmnopqrstuvwxyzåäöüé'.                                   
004200 01  CAPS-LETTERS               PIC X(31)  VALUE                          
004300     'ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖÜÉ'.                                   
004400                                                                          
004500 01  FILLER              PIC X(16) VALUE 'LAND-PARAMETRAR '.              
004600*01  -COPY WISOLAND                                                       
004700     EJECT                                                                
004800                                                                          
004900 01  FILLER.                                                              
005000     03  WISOLAND                PIC X(8)    VALUE 'WISOLAND'.            
005100     03  WS-DIST-GROUP           PIC X(8)     VALUE SPACE.                
005200         88  VALID-DIST-GROUP    VALUE                                    
005300                                       'FAXBEL '                          
005400                                       'FAXGOT '                          
005500                                       'FAXVNA '                          
005600                                       'IT     '                          
005700                                       'IT1    '                          
005800                                       'IT2    '                          
005900                                       'IT3    '                          
006000                                       'IT4    '                          
006100                                       'IT5    '                          
006200                                       'IT6    '                          
006300                                       'IT7    '                          
006400                                       'IT8    '                          
006500                                       'IT9    '                          
006600                                       'IT10   '                          
006700                                       'IT11   '                          
006800                                       'IT12   '                          
006900                                       'IT13   '                          
007000                                       'IT14   '                          
007100                                       'IT15   '                          
007200                                       'IT16   '                          
007300                                       'IT17   '                          
007400                                       'IT18   '                          
007500                                       'IT19   '                          
007600                                       'IT20   '                          
007700                                       'IT21   '                          
007800                                       'IT22   '                          
007900                                       'IT23   '                          
008000                                       'IT24   '                          
008100                                       'IT25   '                          
008200                                       'IT26   '                          
008300                                       'MCGOT  '                          
008400                                       'PARTS  '                          
008500                                       'PARTSAT'                          
008600                                       'PARTSBE'                          
008700                                       'PARTSES'                          
008800                                       'PARTSFI'                          
008900                                       'PARTSFR'                          
009000                                       'PARTSGB'                          
009100                                       'PARTSIT'                          
009200                                       'PARTSSE'                          
009300                                       'PARTSUS'                          
009400                                       'PARTS1 '                          
009500                                       'PARTS2 '                          
009600                                       'PARTS4 '                          
009700                                       'PARTS5 '                          
009800                                       'PARTS6 '                          
009900                                       'VCC    '                          
010000                                       'VCC1   '                          
010100                                       'VCC2   '                          
010200                                       'VCC3   '                          
010300                                       'VCC4   '                          
010400                                       'VCC5   '                          
010500                                       'VCC6   '                          
010600                                       'VCC7   '                          
010700                                       'VCC8   '                          
010800                                       'VCC9   '                          
010900                                       'VCC10  '                          
011000                                       'VCC11  '                          
011100                                       'VCC12  '                          
011200                                       'VNA    '                          
011300                                       'VTC    '                          
011400                                       'VTCC   '                          
011500                                       'VTC1   '                          
011600                                       'VTC2   '                          
011700                                       'VTC3   '                          
011800                                       'VTC4   '                          
011900                                       'VTC5   '                          
012000                                       'VTC6   '                          
012100                                       'VTC7   '                          
012200                                       'VTC8   '                          
012300                                       'VTC9   '                          
012400                                       'VTC10  '                          
012500                                       'VTC11  '                          
012600                                       'VTC12  '                          
012700                                       'VTC13  '                          
012800                                       'VTC14  '                          
012900                                       'VTNA   '                          
013000                                       'VT3    '.                         
013100         88  FAX-MC-DIST-GROUP    VALUE                                   
013200                                       'FAXBEL '                          
013300                                       'FAXGOT '                          
013400                                       'FAXVNA '                          
013500                                       'MCGOT  '.                         
013600     03  WS-TOP-DOMAIN           PIC X(6)     VALUE SPACE.                
013700         88  VALID-TOP-DOMAIN    VALUE                                    
013800                                       'COM'                              
013900                                       'EDU'                              
014000                                       'GOV'                              
014100                                       'INT'                              
014200                                       'MIL'                              
014300                                       'NET'                              
014400                                       'ORG'                              
014500                                       'GROUP'                            
014600                                       'PRO'                              
014700                                       'BIZ'                              
014800                                       'INFO'                             
014900                                       'AERO'                             
015000                                       'COOP'                             
015100                                       'NAME'                             
015200                                       'TRAVEL'                           
015210                                       'LTD'.                             
015300                                                                          
015400     03  WS-COUNTRY-CODE         PIC X(2)     VALUE SPACE.                
015500         88  COUNTRY-CODE-SWEDEN VALUE 'SE'.                              
015600         88  COUNTRY-CODE-UK     VALUE 'UK'.                              
015700                                                                          
015800     03 WS-IDMAIL                PIC X(60).                               
015900     03  WS-IDMAIL-AUX           PIC X(60).                               
016000     03  WS-IDMAIL-REVERSED      PIC X(60).                               
016100     03  WS-USERID               PIC X(50)    VALUE SPACE.                
016200     03  WS-DOMAIN               PIC X(20)    VALUE SPACE.                
016300         88  VALID-VOLVO-DOMAIN   VALUE 'VOLVOCARS.COM       '            
016400                                        'VOLVO.COM           '            
016500                                        'CONSULTANT.VOLVO.COM'            
016600                                        'PARTNER.VOLVO.COM   '.           
016700         88  VOLVOCARS-COM        VALUE 'VOLVOCARS.COM       '.           
016800         88  VOLVO-COM            VALUE 'VOLVO.COM           '.           
016900         88  CONSULTANT-VOLVO-COM VALUE 'CONSULTANT.VOLVO.COM'.           
017000         88  PARTNER-VOLVO-COM    VALUE 'PARTNER.VOLVO.COM   '.           
017100     03 COUNTER                  PIC 99  VALUE ZERO.                      
017200     03 COUNT-TRAILING-SPACES    PIC 99  VALUE ZERO.                      
017300     03 LAST-CHAR                PIC 99  VALUE ZERO.                      
017400                                                                          
017500     EJECT                                                                
017600 LINKAGE SECTION.                                                         
017700                                                                          
017800 01  -COPY W009EMAD                                                       
017900     EJECT                                                                
018000 PROCEDURE DIVISION USING EMAD-W009EMAD.                                  
018100 MAIN SECTION.                                                            
018200                                                                          
018300     MOVE SPACE       TO EMAD-KDSVAR                                      
018400     MOVE EMAD-IDMAIL TO WS-IDMAIL                                        
018500                                                                          
018600     INSPECT WS-IDMAIL CONVERTING                                         
018700             SMALL-LETTERS TO CAPS-LETTERS                                
018800                                                                          
018900     IF WS-IDMAIL NOT > SPACES                                            
019000       MOVE 1 TO FELNR                                                    
019100       MOVE 'F' TO EMAD-KDSVAR                                            
019200     ELSE                                                                 
019300       PERFORM A-LEFT-JUSTIFY-IDMAIL                                      
019400       MOVE ZERO TO COUNTER                                               
019500       INSPECT WS-IDMAIL                                                  
019600               TALLYING COUNTER FOR CHARACTERS BEFORE '(A)'               
019700       IF COUNTER < 57                                                    
019800         MOVE SPACES      TO WS-IDMAIL-AUX                                
019900         IF COUNTER = 0                                                   
020000           STRING '@' WS-IDMAIL(4:)                                       
020100                  DELIMITED BY SIZE INTO WS-IDMAIL-AUX                    
020200         ELSE                                                             
020300           STRING WS-IDMAIL(1:COUNTER) '@' WS-IDMAIL(COUNTER + 4:)        
020400                  DELIMITED BY SIZE INTO WS-IDMAIL-AUX                    
020500         END-IF                                                           
020600         MOVE WS-IDMAIL-AUX TO WS-IDMAIL                                  
020700       END-IF                                                             
020800       MOVE FUNCTION REVERSE (WS-IDMAIL) TO WS-IDMAIL-REVERSED            
020900       INSPECT WS-IDMAIL-REVERSED REPLACING FIRST X'7C' BY '@'            
021000                                                  X'B5' BY '@'            
021100                                                  X'80' BY '@'            
021200                                                  X'44' BY '@'            
021300                                                  X'AC' BY '@'            
021400                                                  X'AE' BY '@'            
021500       MOVE FUNCTION REVERSE (WS-IDMAIL-REVERSED) TO WS-IDMAIL            
021600       PERFORM B-DETECT-OBVIOUS-ERRORS                                    
021700       IF EMAD-KDSVAR NOT > SPACE                                         
021800         MOVE ZERO TO COUNTER                                             
021900         INSPECT WS-IDMAIL TALLYING COUNTER FOR ALL '@'                   
022000         IF COUNTER = 1                                                   
022100           PERFORM C-VALIDATE                                             
022200         ELSE                                                             
022300           IF COUNTER = ZERO                                              
022400             PERFORM D-COMPLETE                                           
022500           ELSE                                                           
022600             MOVE 2 TO FELNR                                              
022700             MOVE 'F' TO EMAD-KDSVAR                                      
022800           END-IF                                                         
022900         END-IF                                                           
023000       END-IF                                                             
023100     END-IF                                                               
023200*                                             RETURN E-MAIL               
023300     MOVE WS-IDMAIL TO EMAD-IDMAIL                                        
023400*    IF EMAD-KDSVAR = 'F'                                                 
023500*      DISPLAY 'W009EMAD ' FELNR ' ' EMAD-IDMAIL                          
023600*    END-IF                                                               
023700                                                                          
023800     GOBACK                                                               
023900     .                                                                    
024000                                                                          
024100 A-LEFT-JUSTIFY-IDMAIL       SECTION.                                     
024200                                                                          
024300     MOVE ZERO TO COUNTER                                                 
024400     INSPECT WS-IDMAIL                                                    
024500             TALLYING COUNTER FOR LEADING SPACE                           
024600     IF COUNTER > ZERO                                                    
024700       MOVE WS-IDMAIL(COUNTER + 1:) TO WS-IDMAIL-AUX                      
024800       MOVE WS-IDMAIL-AUX           TO WS-IDMAIL                          
024900     END-IF                                                               
025000     .                                                                    
025100                                                                          
025200 B-DETECT-OBVIOUS-ERRORS SECTION.                                         
025300                                                                          
025400     IF WS-IDMAIL(1:1) = '.' OR '@'                                       
025500       MOVE 'F' TO EMAD-KDSVAR                                            
025600     ELSE                                                                 
025700       MOVE ZERO TO COUNT-TRAILING-SPACES                                 
025800       INSPECT WS-IDMAIL-REVERSED                                         
025900             TALLYING COUNT-TRAILING-SPACES FOR LEADING SPACE             
026000       IF WS-IDMAIL(60 - COUNT-TRAILING-SPACES:1) = '.' OR '@'            
026100             MOVE 3 TO FELNR                                              
026200         MOVE 'F' TO EMAD-KDSVAR                                          
026300       ELSE                                                               
026400         MOVE ZERO TO COUNTER                                             
026500         INSPECT WS-IDMAIL                                                
026600                 TALLYING COUNTER FOR ALL SPACE                           
026700         IF COUNTER NOT = COUNT-TRAILING-SPACES                           
026800             MOVE 4 TO FELNR                                              
026900           MOVE 'F' TO EMAD-KDSVAR                                        
027000         ELSE                                                             
027100           MOVE ZERO TO COUNTER                                           
027200           INSPECT WS-IDMAIL TALLYING COUNTER FOR ALL '..'                
027300           IF COUNTER > ZERO                                              
027400             MOVE 5 TO FELNR                                              
027500             MOVE 'F' TO EMAD-KDSVAR                                      
027600           END-IF                                                         
027700         END-IF                                                           
027800       END-IF                                                             
027900     END-IF                                                               
028000     .                                                                    
028100                                                                          
028200 C-VALIDATE            SECTION.                                           
028300                                                                          
028400     MOVE ZERO TO COUNTER                                                 
028500     INSPECT WS-IDMAIL                                                    
028600             TALLYING COUNTER FOR CHARACTERS BEFORE '@'                   
028700     IF WS-IDMAIL(COUNTER:1) = '.'                                        
028800     OR WS-IDMAIL(COUNTER + 2:1) = '.'                                    
028900             MOVE 6 TO FELNR                                              
029000       MOVE 'F' TO EMAD-KDSVAR                                            
029100     ELSE                                                                 
029200       MOVE WS-IDMAIL(COUNTER + 2:) TO WS-DOMAIN                          
029300       IF VALID-VOLVO-DOMAIN                                              
029400       AND WS-IDMAIL(1:1) NOT = '&'                                       
029500*        -- VALIDATE VOLVO MAIL IDS (IF NOT SYMBOLIC PARM)                
029600         PERFORM CA-VALIDATE-VOLVO-USERID                                 
029700       ELSE                                                               
029800         IF COUNT-TRAILING-SPACES > ZERO                                  
029900           MOVE WS-IDMAIL-REVERSED(COUNT-TRAILING-SPACES + 1:)            
030000             TO WS-IDMAIL-AUX                                             
030100           MOVE WS-IDMAIL-AUX TO WS-IDMAIL-REVERSED                       
030200         END-IF                                                           
030300         MOVE ZERO TO COUNTER                                             
030400         INSPECT WS-IDMAIL-REVERSED                                       
030500                 TALLYING COUNTER FOR CHARACTERS BEFORE '.'               
030600         IF COUNTER >= 3 AND <= 6                                         
030700           PERFORM CB-VALIDATE-TOP-DOMAIN                                 
030800         ELSE                                                             
030900           IF COUNTER = 2                                                 
031000             PERFORM CC-VALIDATE-COUNTRY                                  
031100           ELSE                                                           
031200             MOVE 7 TO FELNR                                              
031300             MOVE 'F' TO EMAD-KDSVAR                                      
031400           END-IF                                                         
031500         END-IF                                                           
031600       END-IF                                                             
031700     END-IF                                                               
031800     .                                                                    
031900                                                                          
032000 CA-VALIDATE-VOLVO-USERID      SECTION.                                   
032100                                                                          
032200     PERFORM S-EXTRACT-DIST-GROUP                                         
032300     IF  FAX-MC-DIST-GROUP                                                
032400             MOVE 8 TO FELNR                                              
032500       MOVE 'F' TO EMAD-KDSVAR                                            
032600     ELSE                                                                 
032700       MOVE ZERO TO COUNTER                                               
032800       INSPECT WS-IDMAIL                                                  
032900               TALLYING COUNTER FOR CHARACTERS BEFORE '@'                 
033000       MOVE WS-IDMAIL(1:COUNTER) TO WS-USERID                             
033100       IF WS-IDMAIL(COUNTER:1) = '.'                                      
033200             MOVE 9 TO FELNR                                              
033300         MOVE 'F' TO EMAD-KDSVAR                                          
033400       ELSE                                                               
033500         MOVE ZERO TO COUNTER                                             
033600         INSPECT WS-USERID                                                
033700                 TALLYING COUNTER FOR ALL '.'                             
033800         IF VOLVOCARS-COM                                                 
033810         OR VOLVO-COM                                                     
033820         OR CONSULTANT-VOLVO-COM                                          
033830         OR PARTNER-VOLVO-COM                                             
033900           IF COUNTER > 5                                                 
034000             MOVE 10 TO FELNR                                             
034100             MOVE 'F' TO EMAD-KDSVAR                                      
034200           END-IF                                                         
034300         ELSE                                                             
035300           IF NOT VALID-DIST-GROUP                                        
035400             MOVE 13 TO FELNR                                             
035500             MOVE 'F' TO EMAD-KDSVAR                                      
035600           END-IF                                                         
035800         END-IF                                                           
035900       END-IF                                                             
036000     END-IF                                                               
036100     .                                                                    
036200                                                                          
036300 CB-VALIDATE-TOP-DOMAIN   SECTION.                                        
036400                                                                          
036500     MOVE FUNCTION REVERSE (WS-IDMAIL-REVERSED (1:COUNTER))               
036600       TO WS-TOP-DOMAIN                                                   
036700     IF NOT VALID-TOP-DOMAIN                                              
036800       MOVE 14 TO FELNR                                                   
036900       MOVE 'F' TO EMAD-KDSVAR                                            
037000     END-IF                                                               
037100     .                                                                    
037200 CC-VALIDATE-COUNTRY      SECTION.                                        
037300                                                                          
037400     MOVE FUNCTION REVERSE (WS-IDMAIL-REVERSED (1:2))                     
037500       TO WS-COUNTRY-CODE                                                 
037600     IF NOT (COUNTRY-CODE-SWEDEN OR COUNTRY-CODE-UK)                      
037700       MOVE WS-COUNTRY-CODE TO LAND-IDLANDX2                              
037800       MOVE SPACES          TO LAND-IDLANDX3                              
037900       CALL WISOLAND     USING LAND-WISOLAND                              
038000       IF LAND-KDSVAR > SPACE                                             
038100         MOVE 15 TO FELNR                                                 
038200         MOVE 'F' TO EMAD-KDSVAR                                          
038300       END-IF                                                             
038400     END-IF                                                               
038500     .                                                                    
038600                                                                          
038700 D-COMPLETE                SECTION.                                       
038800                                                                          
038900     PERFORM S-EXTRACT-DIST-GROUP                                         
039000     IF NOT FAX-MC-DIST-GROUP                                             
039100       SUBTRACT COUNT-TRAILING-SPACES FROM 60 GIVING LAST-CHAR            
039200       IF WS-IDMAIL(1:1) = '.'                                            
039300       OR WS-IDMAIL(LAST-CHAR:1) = '.'                                    
039400         MOVE 16 TO FELNR                                                 
039500         MOVE 'F' TO EMAD-KDSVAR                                          
039600       ELSE                                                               
039700         MOVE ZERO TO COUNTER                                             
039800         INSPECT WS-IDMAIL                                                
039900                 TALLYING COUNTER FOR ALL '.'                             
040000         IF COUNTER > 2                                                   
040100           MOVE 17 TO FELNR                                               
040200           MOVE 'F' TO EMAD-KDSVAR                                        
040300         ELSE                                                             
040400           IF COUNTER = 2                                                 
040500             MOVE '@VOLVO.COM' TO WS-DOMAIN                               
040600           ELSE                                                           
040700             IF COUNTER = ZERO                                            
040800               MOVE '@VOLVOCARS.COM' TO WS-DOMAIN                         
040900             ELSE                                                         
041000               IF VALID-DIST-GROUP                                        
041100                 MOVE '@MEMO.VOLVO.SE' TO WS-DOMAIN                       
041200               ELSE                                                       
041300                 MOVE '@VOLVO.COM' TO WS-DOMAIN                           
041400               END-IF                                                     
041500             END-IF                                                       
041600           END-IF                                                         
041700           MOVE SPACES    TO WS-IDMAIL-AUX                                
041800           STRING WS-IDMAIL DELIMITED BY SPACE                            
041900                  WS-DOMAIN DELIMITED BY SIZE                             
042000             INTO WS-IDMAIL-AUX                                           
042100           MOVE WS-IDMAIL-AUX TO WS-IDMAIL                                
042200         END-IF                                                           
042300       END-IF                                                             
042400     END-IF                                                               
042500     .                                                                    
042600                                                                          
042700 S-EXTRACT-DIST-GROUP      SECTION.                                       
042800                                                                          
042900     MOVE ZERO TO COUNTER                                                 
043000     INSPECT WS-IDMAIL                                                    
043100             TALLYING COUNTER FOR CHARACTERS BEFORE '.'                   
043200     MOVE WS-IDMAIL(1:COUNTER) TO WS-DIST-GROUP                           
043300     .                                                                    
