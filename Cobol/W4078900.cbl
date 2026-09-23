000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4078900.                                                
000400 AUTHOR.         HENRIKSSON ANDERS.                                       
000500 DATE-WRITTEN.   20080905.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNCTION:                                                            
000900*      - STARTS EVERY WEEK (SATURDAY) OR ORDERED                          
001000*      - DECIDES FOR EACH PARMANUMBER IF TO BE RUN,                       
001100*        START BY W00507 OR                                               
001200*      - START TO INVOICE A SPECIFIC PARMANUMBER BY W40719                
001300*                                                                         
001400*        THE PROGRAM UPDATES ROWS IN TABLE  TP8LRET                       
001500*        THE PROGRAM READS   ROWS IN TABLE  TP8IRET                       
001600*        THE PROGRAM READS   ROWS IN TABLE  TP8LRET                       
001700*        THE PROGRAM READS   ROWS IN BASE   T01PROC                       
001800*        THE PROGRAM SENDS   ROWS TO BILL-IT USING WZ                     
001900*                                                                         
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300 INPUT-OUTPUT SECTION.                                                    
002400 FILE-CONTROL.                                                            
002500 DATA DIVISION.                                                           
002600 FILE SECTION.                                                            
002700 WORKING-STORAGE SECTION.                                                 
002800                                                                          
002900 77  IDPGM                       PIC X(8)    VALUE 'W4078900'.            
003000                                                                          
003100*    --- WORK FIELD FOR ERROR MESSAGES WHEN CALLING ABEND.                
003200 77  KDRC-DISPLAY                PIC Z(5).                                
003300                                                                          
003400 77  YES                         PIC X       VALUE 'J'.                   
003500 77  NOO                         PIC X       VALUE 'N'.                   
003600                                                                          
003700 77  WS-IX                       PIC S9(7) VALUE +0     COMP-3.           
003800 77  MAX-LINES                   PIC S9(7) VALUE +95000 COMP-3.           
003900                                                                          
004200 77  RESTART-IX                  PIC S9(9) VALUE +0    COMP SYNC.         
004300 77  RESTART-MAX                 PIC S9(9) VALUE +100  COMP SYNC.         
004400 77  WS-IDMARKBO                 PIC X      VALUE SPACE.                  
004700                                                                          
004800 77  FAKT-PRKURS                PIC S9(6)V9(5) VALUE ZERO COMP-3.         
004810 77  W-DATE-AAMM                PIC 9(4)   VALUE ZERO.                    
004820 77  WS-KDVALISO-HUV            PIC X(3)   VALUE 'SEK'.                   
004900                                                                          
005000 01  WS-DATUM                    PIC  X(8).                               
005100 01  WS-TIDSKOLL                 PIC  X(14).                              
005200 01  WS-IDDISTR                  PIC  X(4).                               
005210 01  WS-IDKUNDNR                 PIC  X(6).                               
005220 01  WS-KDKUNDKAT                 PIC  9(2) VALUE 0.                      
005300                                                                          
005400     EJECT                                                                
005500 77  WS-TIME-WAIT                PIC S9(9)   COMP VALUE +60.              
005600                                                                          
005700 01  ERRORTEXT.                                                           
005800     03  FILLER                  PIC X(9)    VALUE 'ERRORTEXT'.           
005900     03  ERRORTEXT-STR           PIC X(72)   VALUE SPACE.                 
006000     EJECT                                                                
006100                                                                          
006200*    --- SUBPROGRAMS OCH PARAMETER AREAS                                  
006300 01  GENERAL-SUBPROGRAMS.                                                 
006400     03  ABEND                   PIC X(8)   VALUE 'ABEND   '.             
006500     03  WZ01SEND                PIC X(8)   VALUE 'WZ01SEND'.             
006600     EJECT                                                                
006700                                                                          
006800*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006900 77  RKOD-ABEND                  PIC S9(4)  COMP VALUE +0.                
007000 77  RKOD-ABEND-NO-DUMP          PIC S9(4)  COMP VALUE +16.               
007100 77  RKOD-ABEND-DB2              PIC S9(4)  COMP VALUE +998.              
007200 77  RKOD-ABEND-IMS              PIC S9(4)  COMP VALUE +1000.             
007300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)  COMP VALUE +1000.             
007400     SKIP2                                                                
007500                                                                          
007600 01  MESSAGE-CODES.                                                       
007700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
007800     EJECT                                                                
007900                                                                          
008000 01  DYNAMIC-SUBPROGRAMS.                                                 
008100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008200     03  W980SOP                 PIC X(8)    VALUE 'WW980SOP'.            
008300     03  W009WAIT                PIC X(8)    VALUE 'W009WAIT'.            
008310     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
008400     EJECT                                                                
008500                                                                          
008600 01  FILLER              PIC X(16)   VALUE ' WMSGSOP-AREA'.               
008700*01      -COPY WMSGSOP                                                    
008800     EJECT                                                                
008900                                                                          
009000*01      -COPY WMSGAREA                                                   
009001     EJECT                                                                
009010*01      -COPY W510CURR                                                   
009100                                                                          
009200*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009300*                                                                         
009400 01  SSA1                        PIC X(200).                              
009500 01  SSA2                        PIC X(64).                               
009600                                                                          
009700 01  IMS-WS.                                                              
009800   03    FILLER                  PIC X(16)   VALUE ' IMS-WS     '.        
009900     03  W-WDB101KY-X.                                                    
010000         05  W-WDB1-IDPARTNR     PIC X(9)    VALUE SPACE.                 
010100         05  W-WDB1-IDFTG        PIC X(2)    VALUE SPACE.                 
011200*                                                                         
011210     03 W-IDGMT-X.                                                        
011220        05 W-IDDISTR-WDB2         PIC S9(5) COMP-3 VALUE 0.               
011230        05 W-IDKUNDNR-WDB2        PIC S9(7) COMP-3 VALUE 0.               
011240*                                                                         
011300   03   GEN-IDSTATNR         PIC S9(9) COMP-3 VALUE 87089997.             
011400   03   WS-STATUS-MS         PIC X(2)    VALUE 'MS'.                      
011500   03   WS-STATUS-S          PIC X(2)    VALUE 'S '.                      
011600   03   WS-TP8LRET-SUM1      PIC S9(7)V9(2) COMP-3.                       
011700   03   WS-TP8LRET-SUM2      PIC S9(7)V9(2) COMP-3.                       
011800   03   WS-TP8IRET-SUM       PIC S9(7)V9(2) COMP-3.                       
011900   03   W-RAD-RAKNARE        PIC S9(7)      COMP-3.                       
012000                                                                          
012100   03    STATUS-WS           PIC XX.                                      
012200     88  STATUS-OK                       VALUE '  '.                      
012300     88  SEGMENT-FINNS                   VALUE '  '.                      
012400     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
012500     88  TRANSKOD-FEL                    VALUE 'A1'.                      
012600     88  SECURITY-FEL                    VALUE 'A4'.                      
012700     SKIP3                                                                
012800   03    GODK-STATUSKODER.                                                
012900     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013000     EJECT                                                                
013100                                                                          
013200 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
013300       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
013400                                                                          
013500 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
013600 01  DB2-WS.                                                              
013700     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
013800        88  CURSOR-OK                       VALUE 000.                    
013900        88  LINES-FOUND                     VALUE 000.                    
014000        88  LINES-MISSING                   VALUE 100.                    
014100        88  LINES-MISSING-EMPTY             VALUE 305.                    
014200        88  RESOURCE-WRONG                  VALUE 904.                    
014300                                                                          
014400     03  GOOD-SQLCODEKODER.                                               
014500         05  GOOD-SQLCODE OCCURS 5                                        
014600             INDEXED BY SQLCODE-IX PIC 9(3).                              
014700                                                                          
014800*    --- AREOR FÖR ANROP TILL WZ01  ------                                
014900 01  FILLER                      PIC X(16)   VALUE 'WZ01-SEND'.           
015000*01  -COPY WZ01SEND                                                       
015100                                                                          
015200 01  TEST-IDDISTR                PIC 9(5)    COMP-3 VALUE ZERO.           
015300*01  FILLER   -COPY WWDIST42    -RED TEST-IDDISTR.                        
015400     EJECT                                                                
015500*01  FILLER   -COPY WWDIST79    -RED TEST-IDDISTR.                        
015800     EJECT                                                                
015900                                                                          
016000 01  UT-AREA.                                                             
016100*    03  FILLER -COPY WZ01REQU  -PRE UT-                                  
016200*    03  FILLER -COPY WF0214I1  -PRE WF-                                  
016300     EJECT                                                                
016400                                                                          
016500 01  FILLER                    PIC X(16)   VALUE 'WDB101-AREA'.           
016600 01  DLI-IO-AREA-WDB101.                                                  
016700     03  WLBETC01.                                                        
016800         05  -COPY WDB101                                                 
016810 01  DLI-IO-AREA-WDB201.                                                  
016820     03  WDB201.                                                          
016830*       05 -COPY WDB201.                                                  
016840     EJECT                                                                
016900*                                                                         
017500 01  FILLER                    PIC X(16)  VALUE 'TP8IRET-AREA'.           
017600*01  -COPY TP8IRET -PRE TP8IRET-                                          
017700     EJECT                                                                
017800                                                                          
017900 01  FILLER                    PIC X(16)  VALUE 'TP8LRET-AREA'.           
018000*01  -COPY TP8LRET -PRE TP8LRET-                                          
018100     EJECT                                                                
018200                                                                          
018300     EXEC SQL INCLUDE TP8IRET END-EXEC.                                   
018400     EJECT                                                                
018500     EXEC SQL INCLUDE TP8LRET END-EXEC.                                   
018600     EJECT                                                                
018700                                                                          
018800*01      -COPY W0003                                                      
018900     EJECT                                                                
019000                                                                          
019100 LINKAGE SECTION.                                                         
019200*01  -COPY W0009     -PRE MSG-                                            
019300     EJECT                                                                
019400*01  -COPY W0009     -PRE ALTF014-                                        
019500     EJECT                                                                
019600*01  -COPY W0009     -PRE ALT4789-                                        
019700     EJECT                                                                
019800*01  -COPY W0008     -PRE WDG2-                                           
019900     05  FILLER                  PIC X.                                   
020000*01  -COPY W0008     -PRE WDB1-                                           
020100     05  FILLER                  PIC X.                                   
020200                                                                          
020210*01  -COPY W0008     -PRE WDB2-                                           
020220     05  FILLER                  PIC X.                                   
020230                                                                          
020300 PROCEDURE DIVISION USING  MSG-PCB ALTF014-PCB ALT4789-PCB                
020400                           WDG2-PCB WDB1-PCB WDB2-PCB.                    
020500 MAIN SECTION.                                                            
020600     ENTRY 'DLITCBL' USING MSG-PCB ALTF014-PCB ALT4789-PCB                
020700                           WDG2-PCB WDB1-PCB WDB2-PCB.                    
020800                                                                          
020900     PERFORM IMS-GET-MSG                                                  
021000     IF SEGMENT-FINNS                                                     
021100       IF MSG-IDTRANS-1 = '4719'                                          
021200* 4719 SHALL BE ABLE TO START THE PROGRAM, IF THERE IS NOTHING            
021300* TO PROCESS, THE PROGRAM SHALL END                                       
021400* MSG-IDTRANS-1 = REQU-IDMSGVER IN NEW WZ01SEND                           
021500         CALL W009WAIT USING WS-TIME-WAIT                                 
021600         PERFORM A-INIT                                                   
021700         PERFORM B-EXECUTE-SPECIAL                                        
021800       ELSE                                                               
021900* ORDINARY START FROM W00507                                              
022000         PERFORM A-INIT                                                   
022100         IF WS-DATUM(5:4) = '1231'                                        
022200           PERFORM B-EXECUTE-YEAR-END                                     
022300         ELSE                                                             
022400           PERFORM B-EXECUTE                                              
022500         END-IF                                                           
022600       END-IF                                                             
022700       IF RESTART-IX > RESTART-MAX                                        
022800         IF LINES-FOUND                                                   
022900           PERFORM C-RESTART-OF-OWN-TRANS                                 
023000         END-IF                                                           
023100       END-IF                                                             
023200     END-IF                                                               
023300     MOVE ZERO TO RETURN-CODE                                             
023400     GOBACK                                                               
023500     .                                                                    
023600     EJECT                                                                
023700                                                                          
023800 A-INIT SECTION.                                                          
023900     MOVE ZERO                   TO WS-IX                                 
024000     MOVE ZERO                   TO W-RAD-RAKNARE                         
024100     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DATUM                         
024200     MOVE FUNCTION CURRENT-DATE (3:2) TO W-DATE-AAMM(1:2)                 
024210     MOVE FUNCTION CURRENT-DATE (5:2) TO W-DATE-AAMM(3:2)                 
024300     INITIALIZE GOOD-SQLCODEKODER                                         
024400     INITIALIZE                    WF-WF0214I1                            
024500     MOVE +1                    TO RESTART-IX                             
024600     .                                                                    
024700     EJECT                                                                
024800                                                                          
024900 B-EXECUTE SECTION.                                                       
025000**** IS THERE LINES TO PROCESS                                            
025100     PERFORM DB2-OPEN-TP8LRET                                             
025200     PERFORM DB2-FETCH-TP8LRET                                            
025300**** CHECK IF THE SUM IS HIGH ENOUGH                                      
025400     PERFORM UNTIL LINES-MISSING OR RESTART-IX > RESTART-MAX              
025500       MOVE ZERO TO WS-TP8IRET-SUM                                        
025600       MOVE ZERO TO WS-TP8LRET-SUM1                                       
025700       PERFORM DB2-SELECT-TP8LRET-SUM                                     
025800       IF LINES-FOUND                                                     
025900         CONTINUE                                                         
026000       ELSE                                                               
026100         MOVE ZERO TO WS-TP8LRET-SUM1                                     
026200       END-IF                                                             
026300       PERFORM DB2-SELECT-TP8IRET-SUM                                     
026400       IF LINES-FOUND                                                     
026500         MOVE TP8IRET-SUNTO-TOT TO WS-TP8IRET-SUM                         
026600       ELSE                                                               
026700         PERFORM DB2-SELECT-TP8IRET-DEF                                   
026800         MOVE TP8IRET-SUNTO-TOT TO WS-TP8IRET-SUM                         
026900       END-IF                                                             
027000       IF WS-TP8LRET-SUM1 > WS-TP8IRET-SUM                                
027100         PERFORM S10-OPEN                                                 
027200         PERFORM DB2-OPEN-TP8LRET-PARMA                                   
027300         PERFORM DB2-FETCH-TP8LRET-PARMA                                  
027400         PERFORM UNTIL LINES-MISSING                                      
027500**** ADD INFORMATION NEEDED                                               
027600           PERFORM S01-ADD-INFORMATION                                    
027700**** SEND LINES TO BILL-IT                                                
027800           PERFORM S11-PUT                                                
027900           ADD +1 TO RESTART-IX                                           
028000           PERFORM DB2-UPDATE-TP8LRET2                                    
028100           PERFORM DB2-FETCH-TP8LRET-PARMA                                
028200         END-PERFORM                                                      
028300         PERFORM DB2-CLOSE-TP8LRET-PARMA                                  
028400         PERFORM S12-CLOSE                                                
028500       ELSE                                                               
028600         CONTINUE                                                         
028700       END-IF                                                             
028800       PERFORM DB2-FETCH-TP8LRET                                          
028900     END-PERFORM                                                          
029000     PERFORM DB2-CLOSE-TP8LRET                                            
029100     .                                                                    
029200     EJECT                                                                
029300                                                                          
029400 B-EXECUTE-YEAR-END SECTION.                                              
029500**** IS THERE LINES TO PROCESS                                            
029600     PERFORM DB2-OPEN-TP8LRET                                             
029700     PERFORM DB2-FETCH-TP8LRET                                            
029800**** CHECK IF THE SUM IS HIGH ENOUGH                                      
029900     PERFORM UNTIL LINES-MISSING OR RESTART-IX > RESTART-MAX              
030000       PERFORM S10-OPEN                                                   
030100       PERFORM DB2-OPEN-TP8LRET-PARMA                                     
030200       PERFORM DB2-FETCH-TP8LRET-PARMA                                    
030300       PERFORM UNTIL LINES-MISSING                                        
030400**** ADD INFORMATION NEEDED                                               
030500         PERFORM S01-ADD-INFORMATION                                      
030600**** SEND LINES TO BILL-IT                                                
030700          PERFORM S11-PUT                                                 
030800          ADD +1 TO RESTART-IX                                            
030900          PERFORM DB2-UPDATE-TP8LRET2                                     
031000          PERFORM DB2-FETCH-TP8LRET-PARMA                                 
031100       END-PERFORM                                                        
031200       PERFORM DB2-CLOSE-TP8LRET-PARMA                                    
031300       PERFORM S12-CLOSE                                                  
031400                                                                          
031500       PERFORM DB2-FETCH-TP8LRET                                          
031600     END-PERFORM                                                          
031700     PERFORM DB2-CLOSE-TP8LRET                                            
031800     .                                                                    
031900     EJECT                                                                
032000                                                                          
032100 B-EXECUTE-SPECIAL SECTION.                                               
032200**** IS THERE LINES TO PROCESS FOR GIVEN PARMANUMBER                      
032300     PERFORM DB2-OPEN-TP8LRET-2                                           
032400     PERFORM DB2-FETCH-TP8LRET-2                                          
032500**** CHECK IF THE SUM IS HIGH ENOUGH                                      
032600     PERFORM UNTIL LINES-MISSING OR RESTART-IX > RESTART-MAX              
032700       PERFORM S10-OPEN                                                   
032800       PERFORM DB2-OPEN-TP8LRET-PARMA                                     
032900       PERFORM DB2-FETCH-TP8LRET-PARMA                                    
033000       PERFORM UNTIL LINES-MISSING                                        
033100**** ADD INFORMATION NEEDED                                               
033200         PERFORM S01-ADD-INFORMATION                                      
033300**** SEND LINES TO BILL-IT                                                
033400         PERFORM S11-PUT                                                  
033500         ADD +1 TO RESTART-IX                                             
033600         PERFORM DB2-UPDATE-TP8LRET1                                      
033700         PERFORM DB2-FETCH-TP8LRET-PARMA                                  
033800       END-PERFORM                                                        
033900       PERFORM DB2-CLOSE-TP8LRET-PARMA                                    
034000       PERFORM S12-CLOSE                                                  
034100       PERFORM DB2-FETCH-TP8LRET-2                                        
034200     END-PERFORM                                                          
034300     PERFORM DB2-CLOSE-TP8LRET-2                                          
034400     .                                                                    
034500     EJECT                                                                
034600                                                                          
034700 C-RESTART-OF-OWN-TRANS SECTION.                                          
034800     ADD +17  TO MSG-KVLL                                                 
034900     MOVE 'W4T789X ' TO MSG-KDTRANS-1                                     
035000     IF MSG-IDTRANS-1 = '4719'                                            
035100       MOVE '4719' TO MSG-IDTRANS-1                                       
035200     ELSE                                                                 
035300       MOVE '4789' TO MSG-IDTRANS-1                                       
035400     END-IF                                                               
035500     MOVE '1' TO MSG-KDMFSFOR-1                                           
035600     MOVE SPACE TO MSG-MID-OUT                                            
035700     PERFORM IMS-INSERT-ALTMSG-W40789                                     
035800     .                                                                    
035900     EJECT                                                                
036000                                                                          
036100 S01-ADD-INFORMATION SECTION.                                             
036200     MOVE 'VCCS'                TO WF-IDLEGSEL                            
036300     MOVE 'INVKRE'              TO WF-IDBUNDLE                            
036400     MOVE 'N'                   TO WF-FLSOFT                              
036500     MOVE 'N'                   TO WF-FLSPECPR                            
036600     MOVE 'N'                   TO WF-FLFREE                              
036700     MOVE 'N'                   TO WF-FLPRIV                              
036800     IF MSG-IDTRANS-1 = '4719'                                            
036900       MOVE 'NOW'               TO WF-KDINVFRQ                            
037000     ELSE                                                                 
037100       MOVE 'DAY'               TO WF-KDINVFRQ                            
037200     END-IF                                                               
037300     MOVE 'INV3'                TO WF-KDFINDOC                            
037600     MOVE 'SE'                  TO WF-IDLANDX3-SEND                       
037700     MOVE GEN-IDSTATNR          TO WF-IDSTATNR                            
037800                                                                          
037900     MOVE TP8LRET-IDREF         TO WF-IDREF                               
038000     MOVE TP8LRET-IDPARTNR      TO WF-IDPARTNR                            
038100     MOVE TP8LRET-IDEXCUST-1    TO WF-IDEXCUST(1)                         
038200     MOVE TP8LRET-IDEXCUST-2    TO WF-IDEXCUST(2)                         
038300     MOVE TP8LRET-IDOPTION-1    TO WF-IDOPTION(1)                         
038400     MOVE TP8LRET-IDOPTION-2    TO WF-IDOPTION(2)                         
038500     MOVE TP8LRET-IDOPTION-3    TO WF-IDOPTION(3)                         
038600     MOVE TP8LRET-KVBEART       TO WF-KVBEART                             
038700     MOVE TP8LRET-KVLEVART      TO WF-KVLEVART                            
038800     MOVE TP8LRET-BEART         TO WF-BEART                               
038900     MOVE '11'                  TO WF-IDDC                                
039000     MOVE TP8LRET-IDUSER-1      TO WF-IDUSER                              
039100     MOVE TP8LRET-IDEXCUST-1    TO WF-IDSEQ(1)                            
039200     MOVE TP8LRET-IDEXCUST-2    TO WF-IDSEQ(2)                            
039300     MOVE TP8LRET-DAREFDAT      TO WF-DAREFDAT                            
039400                                                                          
039500     COMPUTE W-RAD-RAKNARE    = W-RAD-RAKNARE  +  1                       
039600     MOVE W-RAD-RAKNARE         TO WF-IDREFRAD                            
039700                                                                          
039800     IF TP8LRET-IDEXCUST-1(1:1) =  SPACE                                  
039900       MOVE '0000'        TO WS-IDDISTR                                   
040000     ELSE                                                                 
040100       IF TP8LRET-IDEXCUST-1(2:1) =  SPACE                                
040200         MOVE '000'                   TO WS-IDDISTR(1:3)                  
040300         MOVE TP8LRET-IDEXCUST-1(1:1) TO WS-IDDISTR(4:1)                  
040400       ELSE                                                               
040500         IF TP8LRET-IDEXCUST-1(3:1) =  SPACE                              
040600           MOVE '00'                    TO WS-IDDISTR(1:2)                
040700           MOVE TP8LRET-IDEXCUST-1(1:2) TO WS-IDDISTR(3:2)                
040800         ELSE                                                             
040900           IF TP8LRET-IDEXCUST-1(4:1) =  SPACE                            
041000             MOVE '0'                     TO WS-IDDISTR(1:1)              
041100             MOVE TP8LRET-IDEXCUST-1(1:3) TO WS-IDDISTR(2:3)              
041200           ELSE                                                           
041300             MOVE TP8LRET-IDEXCUST-1(1:4) TO WS-IDDISTR                   
041400           END-IF                                                         
041500         END-IF                                                           
041600       END-IF                                                             
041700     END-IF                                                               
041800                                                                          
041810     IF TP8LRET-IDEXCUST-2(1:1) =  SPACE                                  
041820       MOVE '000000'      TO WS-IDKUNDNR                                  
041830     ELSE                                                                 
041840       IF TP8LRET-IDEXCUST-2(2:1) =  SPACE                                
041850         MOVE '00000'                 TO WS-IDKUNDNR(1:5)                 
041860         MOVE TP8LRET-IDEXCUST-2(1:1) TO WS-IDKUNDNR(6:1)                 
041870       ELSE                                                               
041880         IF TP8LRET-IDEXCUST-2(3:1) =  SPACE                              
041890           MOVE '0000'                  TO WS-IDKUNDNR(1:4)               
041891           MOVE TP8LRET-IDEXCUST-2(1:2) TO WS-IDKUNDNR(5:2)               
041892         ELSE                                                             
041893           IF TP8LRET-IDEXCUST-2(4:1) =  SPACE                            
041894             MOVE '000'                   TO WS-IDKUNDNR(1:3)             
041895             MOVE TP8LRET-IDEXCUST-2(1:3) TO WS-IDKUNDNR(4:3)             
041896           ELSE                                                           
041897             IF TP8LRET-IDEXCUST-2(5:1) =  SPACE                          
041898               MOVE '00'                    TO WS-IDKUNDNR(1:2)           
041899               MOVE TP8LRET-IDEXCUST-2(1:4) TO WS-IDKUNDNR(3:4)           
041900             ELSE                                                         
041901               IF TP8LRET-IDEXCUST-2(6:1) =  SPACE                        
041902                 MOVE '0'                     TO WS-IDKUNDNR(1:1)         
041903                 MOVE TP8LRET-IDEXCUST-2(1:5) TO WS-IDKUNDNR(2:5)         
041904               ELSE                                                       
041905                 MOVE TP8LRET-IDEXCUST-2(1:6) TO WS-IDKUNDNR              
041906               END-IF                                                     
041907             END-IF                                                       
041908           END-IF                                                         
041909         END-IF                                                           
041910       END-IF                                                             
041911     END-IF                                                               
041912*                                                                         
041913     PERFORM S01A-GET-KDKUNDKAT                                           
041914                                                                          
041915     MOVE 'W41K'                TO WF-IDSYSTEM-SEND                       
041916     MOVE 'W41K'                TO WF-IDSYSTEM-REC                        
041917     IF WS-KDKUNDKAT = 18                                                 
041918        MOVE 'ECOM'                TO WF-IDSYSTEM-SEND                    
041921     END-IF                                                               
041924                                                                          
041930**** TA FRAM MOMS                                                         
042000     MOVE WS-IDDISTR            TO TEST-IDDISTR                           
042100     IF DIST42-EJ-EU-PLUS-SE                                              
042200       IF DIST42-EJ-EU                                                    
042300         MOVE '80'              TO WF-KDVAT                               
042400       ELSE                                                               
042500**** SVERIGE TILL SVERIGE BEHÖVER SVENSK MOMS                             
042600         MOVE '21'              TO WF-KDVAT                               
042700       END-IF                                                             
042800     ELSE                                                                 
042900       MOVE '60'                TO WF-KDVAT                               
043000     END-IF                                                               
043100                                                                          
043200     PERFORM S02-GET-KDVALISO                                             
043300                                                                          
043400**** CALCULATE PRICE                                                      
043500     MOVE TP8LRET-PRARTBTO      TO WF-PRARTBTO                            
043600     MOVE TP8LRET-PRARTNTO      TO WF-PRARTNTO                            
043700     PERFORM S03-GET-PRICE                                                
043800                                                                          
043900     MOVE SPACE                 TO WF-IDACCNT(1)                          
044000     MOVE SPACE                 TO WF-IDACCNT(2)                          
044100     MOVE SPACE                 TO WF-IDACCNT(3)                          
044200                                                                          
044300     MOVE SPACE                 TO WF-BEVOLREF                            
044400     MOVE SPACE                 TO WF-IDAPPEND                            
044500                                                                          
044600     MOVE SPACE                 TO WF-IDLANDX3-REC                        
044700     MOVE SPACE                 TO WF-IDARTNR-FINANCE                     
044800     MOVE TP8LRET-IDEXCUST-1    TO WF-IDBREAK(1)                          
044900     MOVE SPACE                 TO WF-IDBREAK(2)                          
045000     MOVE SPACE                 TO WF-IDLEVNR                             
045100     MOVE SPACE                 TO WF-KDARTURS                            
045200     MOVE SPACE                 TO WF-BELEVVIL                            
045300     MOVE SPACE                 TO WF-BEANST                              
045400     MOVE SPACE                 TO WF-BETEXT                              
045500                                                                          
045600     MOVE ZERO                  TO WF-KDFRAKT                             
045700     MOVE ZERO                  TO WF-REARTRAB                            
045800     MOVE ZERO                  TO WF-IDSTATNR                            
045900     MOVE ZERO                  TO WF-VKARTNTO                            
046000     .                                                                    
046100     EJECT                                                                
046200                                                                          
046300 S01A-GET-KDKUNDKAT SECTION.                                              
046301                                                                          
046305     MOVE WS-IDDISTR            TO W-IDDISTR-WDB2                         
046306     MOVE WS-IDKUNDNR           TO W-IDKUNDNR-WDB2                        
046307     MOVE ZEROES                TO WS-KDKUNDKAT                           
046308     PERFORM IMS-GU-WDB201                                                
046309     IF SEGMENT-FINNS                                                     
046310        MOVE GMT-KDKUNDKAT      TO WS-KDKUNDKAT                           
046312     END-IF                                                               
046313     .                                                                    
046320     EJECT                                                                
046330                                                                          
046340 S02-GET-KDVALISO SECTION.                                                
046400     MOVE SPACE                     TO WF-KDVALISO                        
046500     MOVE TP8LRET-IDPARTNR          TO W-WDB1-IDPARTNR                    
046600     MOVE '57'                      TO W-WDB1-IDFTG                       
046700     PERFORM IMS-GU-WDB101                                                
046800     IF SEGMENT-FINNS                                                     
046900       MOVE BET-KDVALISO           TO WF-KDVALISO                         
047100     ELSE                                                                 
047200       MOVE SPACE                  TO WF-KDVALISO                         
047300     END-IF                                                               
047400                                                                          
047500     MOVE WS-IDDISTR                TO TEST-IDDISTR                       
047600                                                                          
049200     IF NOT DIST79-DEALER-PRICE AND                                       
049220        NOT DIST79-ECOM-PRICE                                             
049300       MOVE 'SEK'                         TO WF-KDVALISO                  
049400     END-IF                                                               
049500     IF WF-KDVALISO = SPACE                                               
049600       MOVE 'SEK'                         TO WF-KDVALISO                  
049700     END-IF                                                               
049800     .                                                                    
049900     EJECT                                                                
050000                                                                          
050100 S03-GET-PRICE SECTION.                                                   
050200     IF WF-KDVALISO = 'SEK'                                               
050300       CONTINUE                                                           
050400     ELSE                                                                 
050500       MOVE WF-KDVALISO       TO CURR-KDVALISO-ROW                        
050600       MOVE W-DATE-AAMM       TO CURR-TIAAMM                              
050610       MOVE WS-KDVALISO-HUV   TO CURR-KDVALISO-HUV                        
050620       MOVE 'M'               TO CURR-KDVALTYP                            
050630                                                                          
050640       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
050650       IF CURR-KDSVAR = ' '                                               
050700         MOVE CURR-PRKURS-NEW TO FAKT-PRKURS                              
050710       ELSE                                                               
050711         MOVE 1               TO FAKT-PRKURS                              
050720       END-IF                                                             
050800       COMPUTE WF-PRARTNTO ROUNDED = WF-PRARTNTO / FAKT-PRKURS            
050900       COMPUTE WF-PRARTBTO ROUNDED = WF-PRARTBTO / FAKT-PRKURS            
051000     END-IF                                                               
051100     .                                                                    
051200     EJECT                                                                
051300                                                                          
051400 S10-OPEN      SECTION.                                                   
051500     MOVE 'OPEN'                     TO SEND-KDFUNC                       
051600     MOVE 'CARPARTS.BILLIT.RECEIVE5' TO SEND-ADDISPABS                    
051700                                                                          
051800     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
051900                                SEND-OPEN-AREA                            
052000     IF SEND-KDRC > 0                                                     
052100       MOVE SEND-KDRC           TO KDRC-DISPLAY                           
052200       STRING 'WZ01SEND-OPEN RC-ERR = ' KDRC-DISPLAY                      
052300            DELIMITED BY SIZE INTO ERRORTEXT                              
052400       CALL ABEND USING RKOD-ABEND-IMS                                    
052500     END-IF                                                               
052600     .                                                                    
052700     EJECT                                                                
052800                                                                          
052900 S11-PUT          SECTION.                                                
053000     MOVE 'PUT'                 TO SEND-KDFUNC                            
053100     MOVE LENGTH OF UT-AREA     TO SEND-KVDLEN                            
053200                                                                          
053300     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
053400                                SEND-KVDLEN                               
053500                                UT-AREA                                   
053600     IF SEND-KDRC > 1                                                     
053700       MOVE SEND-KDRC           TO KDRC-DISPLAY                           
053800       STRING 'WZ01SEND-PUT RC-ERR = ' KDRC-DISPLAY                       
053900            DELIMITED BY SIZE INTO ERRORTEXT                              
054000       CALL ABEND USING RKOD-ABEND-IMS                                    
054100     END-IF                                                               
054200     .                                                                    
054300     EJECT                                                                
054400                                                                          
054500 S12-CLOSE       SECTION.                                                 
054600     MOVE 'CLOSE'               TO SEND-KDFUNC                            
054700                                                                          
054800     CALL WZ01SEND   USING      SEND-CONTROL-AREA                         
054900     IF SEND-KDRC > 0                                                     
055000       MOVE SEND-KDRC           TO KDRC-DISPLAY                           
055100       STRING 'WZ01SEND-CLOSE RC-ERR = ' KDRC-DISPLAY                     
055200            DELIMITED BY SIZE INTO ERRORTEXT                              
055300       CALL ABEND USING RKOD-ABEND-IMS                                    
055400     END-IF                                                               
055500     .                                                                    
055600     EJECT                                                                
055700                                                                          
055800 DB2-OPEN-TP8LRET       SECTION.                                          
055900     EXEC SQL DECLARE TP8LRET1-CRS CURSOR FOR                             
056000     SELECT   DISTINCT IDPARTNR,IDFTG,IDEXCUST_2                          
056100                                                                          
056200     FROM     TP8LRET                                                     
056300                                                                          
056400     WHERE    DAFAKT   = '00000000'                                       
056500                                                                          
056600     END-EXEC                                                             
056700     EXEC SQL OPEN TP8LRET1-CRS                                           
056800     END-EXEC                                                             
056900                                                                          
057000     MOVE 000100305      TO GOOD-SQLCODEKODER                             
057100     MOVE SQLCODE        TO SQLCODE-WS                                    
057200     PERFORM DB2-STATUS-CHECK                                             
057300     .                                                                    
057400     EJECT                                                                
057500                                                                          
057600 DB2-FETCH-TP8LRET       SECTION.                                         
057700     EXEC SQL FETCH TP8LRET1-CRS INTO                                     
057800            :TP8LRET-IDPARTNR                                             
057900           ,:TP8LRET-IDFTG                                                
058000           ,:TP8LRET-IDEXCUST-2                                           
058100     END-EXEC                                                             
058200                                                                          
058300     MOVE 000100305      TO GOOD-SQLCODEKODER                             
058400     MOVE SQLCODE        TO SQLCODE-WS                                    
058500     PERFORM DB2-STATUS-CHECK                                             
058600     .                                                                    
058700     EJECT                                                                
058800                                                                          
058900 DB2-CLOSE-TP8LRET       SECTION.                                         
059000     EXEC SQL CLOSE TP8LRET1-CRS                                          
059100     END-EXEC                                                             
059200     .                                                                    
059300     EJECT                                                                
059400                                                                          
059500 DB2-OPEN-TP8LRET-2     SECTION.                                          
059600     EXEC SQL DECLARE TP8LRET3-CRS CURSOR FOR                             
059700     SELECT   DISTINCT IDPARTNR,IDFTG,IDEXCUST_2                          
059800                                                                          
059900     FROM     TP8LRET                                                     
060000                                                                          
060100     WHERE    DAFAKT    = '00000000'                                      
060200     AND      KDRAPPSTA = 'M '                                            
060300                                                                          
060400     END-EXEC                                                             
060500     EXEC SQL OPEN TP8LRET3-CRS                                           
060600     END-EXEC                                                             
060700                                                                          
060800     MOVE 000100305      TO GOOD-SQLCODEKODER                             
060900     MOVE SQLCODE        TO SQLCODE-WS                                    
061000     PERFORM DB2-STATUS-CHECK                                             
061100     .                                                                    
061200     EJECT                                                                
061300                                                                          
061400 DB2-FETCH-TP8LRET-2     SECTION.                                         
061500     EXEC SQL FETCH TP8LRET3-CRS INTO                                     
061600            :TP8LRET-IDPARTNR                                             
061700           ,:TP8LRET-IDFTG                                                
061800           ,:TP8LRET-IDEXCUST-2                                           
061900     END-EXEC                                                             
062000                                                                          
062100     MOVE 000100         TO GOOD-SQLCODEKODER                             
062200     MOVE SQLCODE        TO SQLCODE-WS                                    
062300     PERFORM DB2-STATUS-CHECK                                             
062400     .                                                                    
062500     EJECT                                                                
062600                                                                          
062700 DB2-CLOSE-TP8LRET-2     SECTION.                                         
062800     EXEC SQL CLOSE TP8LRET3-CRS                                          
062900     END-EXEC                                                             
063000     .                                                                    
063100     EJECT                                                                
063200                                                                          
063300 DB2-OPEN-TP8LRET-PARMA SECTION.                                          
063400     EXEC SQL DECLARE TP8LRET-CRS CURSOR FOR                              
063500     SELECT   IDPARTNR                                                    
063600             ,IDFTG                                                       
063700             ,KDANMORS                                                    
063800             ,DAREGDAT                                                    
063900             ,IDREF                                                       
064000             ,IDEXCUST_1                                                  
064100             ,IDEXCUST_2                                                  
064200             ,IDLANDX3_SEND                                               
064300             ,DAREFDAT                                                    
064400             ,BEART                                                       
064500             ,PRARTNTO                                                    
064600             ,PRARTBTO                                                    
064700             ,KVLEVART                                                    
064800             ,KVBEART                                                     
064900             ,KDVALISO                                                    
065000             ,IDOPTION_1                                                  
065100             ,IDOPTION_2                                                  
065200             ,IDOPTION_3                                                  
065300             ,IDDC                                                        
065400             ,IDUSER_1                                                    
065500             ,IDUSER_2                                                    
065600             ,IDSYSTEM_SEND                                               
065700             ,IDSYSTEM_REC                                                
065800             ,DAUPPDAT                                                    
065900             ,DADELDAT                                                    
066000             ,DAFAKT                                                      
066100             ,IDFINDOC                                                    
066200             ,KDRAPPSTA                                                   
066300                                                                          
066400     FROM     TP8LRET                                                     
066500                                                                          
066600     WHERE    IDPARTNR   = :TP8LRET-IDPARTNR                              
066800     AND      IDEXCUST_2 = :TP8LRET-IDEXCUST-2                            
066900     AND      DAFAKT     = '00000000'                                     
067000                                                                          
067100     FOR UPDATE OF DAFAKT                                                 
067200                  ,KDRAPPSTA                                              
067300     END-EXEC                                                             
067400                                                                          
067500     EXEC SQL OPEN TP8LRET-CRS                                            
067600     END-EXEC                                                             
067700                                                                          
067800     MOVE 000100305      TO GOOD-SQLCODEKODER                             
067900     MOVE SQLCODE        TO SQLCODE-WS                                    
068000     PERFORM DB2-STATUS-CHECK                                             
068100     .                                                                    
068200     EJECT                                                                
068300                                                                          
068400 DB2-FETCH-TP8LRET-PARMA SECTION.                                         
068500     EXEC SQL FETCH TP8LRET-CRS INTO                                      
068600            :TP8LRET-IDPARTNR                                             
068700           ,:TP8LRET-IDFTG                                                
068800           ,:TP8LRET-KDANMORS                                             
068900           ,:TP8LRET-DAREGDAT                                             
069000           ,:TP8LRET-IDREF                                                
069100           ,:TP8LRET-IDEXCUST-1                                           
069200           ,:TP8LRET-IDEXCUST-2                                           
069300           ,:TP8LRET-IDLANDX3-SEND                                        
069400           ,:TP8LRET-DAREFDAT                                             
069500           ,:TP8LRET-BEART                                                
069600           ,:TP8LRET-PRARTNTO                                             
069700           ,:TP8LRET-PRARTBTO                                             
069800           ,:TP8LRET-KVLEVART                                             
069900           ,:TP8LRET-KVBEART                                              
070000           ,:TP8LRET-KDVALISO                                             
070100           ,:TP8LRET-IDOPTION-1                                           
070200           ,:TP8LRET-IDOPTION-2                                           
070300           ,:TP8LRET-IDOPTION-3                                           
070400           ,:TP8LRET-IDDC                                                 
070500           ,:TP8LRET-IDUSER-1                                             
070600           ,:TP8LRET-IDUSER-2                                             
070700           ,:TP8LRET-IDSYSTEM-SEND                                        
070800           ,:TP8LRET-IDSYSTEM-REC                                         
070900           ,:TP8LRET-DAUPPDAT                                             
071000           ,:TP8LRET-DADELDAT                                             
071100           ,:TP8LRET-DAFAKT                                               
071200           ,:TP8LRET-IDFINDOC                                             
071300           ,:TP8LRET-KDRAPPSTA                                            
071400     END-EXEC                                                             
071500                                                                          
071600     MOVE 000100305      TO GOOD-SQLCODEKODER                             
071700     MOVE SQLCODE        TO SQLCODE-WS                                    
071800     PERFORM DB2-STATUS-CHECK                                             
071900     .                                                                    
072000     EJECT                                                                
072100                                                                          
072200 DB2-CLOSE-TP8LRET-PARMA SECTION.                                         
072300     EXEC SQL CLOSE TP8LRET-CRS                                           
072400     END-EXEC                                                             
072500     .                                                                    
072600     EJECT                                                                
072700                                                                          
072800 DB2-UPDATE-TP8LRET1      SECTION.                                        
072900     EXEC SQL                                                             
073000         UPDATE TP8LRET                                                   
073100         SET DAFAKT    = :WS-DATUM                                        
073200         ,   KDRAPPSTA = :WS-STATUS-MS                                    
073300         WHERE CURRENT OF TP8LRET-CRS                                     
073400     END-EXEC                                                             
073500                                                                          
073600     MOVE 000     TO GOOD-SQLCODEKODER                                    
073700     MOVE SQLCODE TO SQLCODE-WS                                           
073800     PERFORM DB2-STATUS-CHECK                                             
073900     .                                                                    
074000     EJECT                                                                
074100                                                                          
074200 DB2-UPDATE-TP8LRET2 SECTION.                                             
074300     EXEC SQL                                                             
074400         UPDATE TP8LRET                                                   
074500         SET DAFAKT    = :WS-DATUM                                        
074600         ,   KDRAPPSTA = :WS-STATUS-S                                     
074700         WHERE CURRENT OF TP8LRET-CRS                                     
074800     END-EXEC                                                             
074900                                                                          
075000     MOVE 000     TO GOOD-SQLCODEKODER                                    
075100     MOVE SQLCODE TO SQLCODE-WS                                           
075200     PERFORM DB2-STATUS-CHECK                                             
075300     .                                                                    
075400     EJECT                                                                
075500                                                                          
075600 DB2-SELECT-TP8LRET-SUM SECTION.                                          
075700     EXEC SQL                                                             
075800     SELECT   SUM(PRARTNTO * KVLEVART)                                    
075900             ,SUM(PRARTBTO * KVLEVART)                                    
076000                                                                          
076100     INTO     :WS-TP8LRET-SUM1                                            
076200             ,:WS-TP8LRET-SUM2                                            
076300                                                                          
076400     FROM     TP8LRET                                                     
076500                                                                          
076600     WHERE    IDPARTNR      = :TP8LRET-IDPARTNR                           
076800       AND    IDEXCUST_2    = :TP8LRET-IDEXCUST-2                         
076900       AND    DAFAKT        = '00000000'                                  
077000     END-EXEC                                                             
077100                                                                          
077200     MOVE 000100305      TO GOOD-SQLCODEKODER                             
077300     MOVE SQLCODE        TO SQLCODE-WS                                    
077400     PERFORM DB2-STATUS-CHECK                                             
077500     .                                                                    
077600     EJECT                                                                
077700                                                                          
077800 DB2-SELECT-TP8IRET-SUM SECTION.                                          
077900     EXEC SQL                                                             
078000     SELECT   SUNTO_TOT                                                   
078100             ,KDVALISO                                                    
078200                                                                          
078300     INTO     :TP8IRET-SUNTO-TOT                                          
078400             ,:TP8IRET-KDVALISO                                           
078500                                                                          
078600     FROM     TP8IRET                                                     
078700                                                                          
078800     WHERE    IDPARTNR      = :TP8LRET-IDPARTNR                           
079000     END-EXEC                                                             
079100                                                                          
079200     MOVE 000100305      TO GOOD-SQLCODEKODER                             
079300     MOVE SQLCODE        TO SQLCODE-WS                                    
079400     PERFORM DB2-STATUS-CHECK                                             
079500     .                                                                    
079600     EJECT                                                                
079700                                                                          
079800 DB2-SELECT-TP8IRET-DEF SECTION.                                          
079900     EXEC SQL                                                             
080000     SELECT   SUNTO_TOT                                                   
080100             ,KDVALISO                                                    
080200                                                                          
080300     INTO     :TP8IRET-SUNTO-TOT                                          
080400             ,:TP8IRET-KDVALISO                                           
080500                                                                          
080600     FROM     TP8IRET                                                     
080700                                                                          
080800     WHERE    IDPARTNR      = 'DEFAULT'                                   
080900     END-EXEC                                                             
081000                                                                          
081100     MOVE 000100305      TO GOOD-SQLCODEKODER                             
081200     MOVE SQLCODE        TO SQLCODE-WS                                    
081300     PERFORM DB2-STATUS-CHECK                                             
081400     .                                                                    
081500     EJECT                                                                
081600                                                                          
081700 DB2-STATUS-CHECK     SECTION.                                            
081800     SET SQLCODE-IX TO 1                                                  
081900     SEARCH GOOD-SQLCODE                                                  
082000       AT END                                                             
082100          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
082200          DELIMITED BY SIZE INTO ERRORTEXT                                
082300          CALL ABEND USING RKOD-ABEND-DB2                                 
082400       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
082500     END-SEARCH                                                           
082600     .                                                                    
082700     EJECT                                                                
082800                                                                          
082900 IMS-GET-MSG  SECTION.                                                    
083000     MOVE    '  QC'          TO    GODK-STATUSKODER                       
083100     CALL    CBLTDLI         USING GU   MSG-PCB MSG-IO-AREA               
083200     MOVE    MSG-STATUS-CODE TO    STATUS-WS                              
083300     PERFORM IMS-STATUSKONTROLL                                           
083400     .                                                                    
083500     EJECT                                                                
083600                                                                          
083700 IMS-GU-WDB101 SECTION.                                                   
083800     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
083900          DELIMITED BY SIZE INTO SSA1                                     
084000     MOVE '  GE'               TO GODK-STATUSKODER                        
084100     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA-WDB101 SSA1               
084200     MOVE WDB1-STATUS-CODE     TO STATUS-WS                               
084300     PERFORM IMS-STATUSKONTROLL                                           
084400     .                                                                    
084500     EJECT                                                                
084600                                                                          
084700 IMS-GU-WDB201 SECTION.                                                   
084800     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
084900          DELIMITED BY SIZE INTO SSA1                                     
085000     MOVE '  GE'               TO GODK-STATUSKODER                        
085100     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1               
085200     MOVE WDB2-STATUS-CODE     TO STATUS-WS                               
085300     PERFORM IMS-STATUSKONTROLL                                           
085400     .                                                                    
085500     EJECT                                                                
085600                                                                          
085900 IMS-INSERT-ALTMSG-W40789 SECTION.                                        
086000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
086100     MOVE SPACE TO GODK-STATUSKODER                                       
086200     CALL CBLTDLI USING ISRT ALT4789-PCB MSG-IO-AREA                      
086300     MOVE ALT4789-STATUS-CODE TO STATUS-WS                                
086400     PERFORM IMS-STATUSKONTROLL                                           
086500     .                                                                    
086600     SKIP3                                                                
086700                                                                          
086800 IMS-STATUSKONTROLL SECTION.                                              
086900     SET STATUS-IX TO 1                                                   
087000     SEARCH GODK-STATUS                                                   
087100       AT END                                                             
087200         MOVE 'FEL STATUSKOD FRÅN IMS ' TO ERRORTEXT                      
087300         CALL ABEND USING RKOD-ABEND-IMS                                  
087400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
087500         CONTINUE                                                         
087600     END-SEARCH                                                           
087700     .                                                                    
087800     EJECT                                                                
