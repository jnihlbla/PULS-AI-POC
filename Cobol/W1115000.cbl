000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1115000.                                                
000300 AUTHOR.         REDDY RAHUL.                                             
000400 DATE-WRITTEN.   16/06/14.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        RECEIVE XML FILE FROM MIC AND                                    
001000*        EXTRACT REQUIRED DATA.                                           
001100*                                                                         
001200*    ABENDCODES:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600                                                                          
001700 ENVIRONMENT DIVISION.                                                    
001800 CONFIGURATION SECTION.                                                   
001900*SOURCE-COMPUTER. IBM-z WITH DEBUGGING MODE.                              
002000 INPUT-OUTPUT SECTION.                                                    
002100 FILE-CONTROL.                                                            
002200                                                                          
002300*          --- INPUT FILE FROM MIC                                        
002400     SELECT W11150                     ASSIGN TO W11150D1.                
002500                                                                          
002600*          --- EXTRACT OF MIC                                             
002700     SELECT W11151                     ASSIGN TO W11150D2.                
002800                                                                          
002900 DATA DIVISION.                                                           
003000 FILE SECTION.                                                            
003100                                                                          
003200                                                                          
003300*01  IN-AREA.                                                             
003400*    03 IN-REC                   PIC X OCCURS 1 TO 251 DEPENDING          
003500*                                      ON REC-LEN.                        
003600 FD  W11150                                                               
003700     RECORD IS VARYING FROM 1 TO 9999 DEPENDING ON REC-LEN                
003800     RECORDING       V                                                    
003900     BLOCK CONTAINS  0.                                                   
004000 01  IN-RECORD                   PIC X(9999).                             
004100                                                                          
004200 FD  W11151                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500*01  RECORD -COPY W11151 -PRE  UT-  -L.                                   
004600                                                                          
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004900 77  IDPGM                       PIC X(8)    VALUE 'W1115000'.            
005000 77  YES                         PIC X       VALUE 'J'.                   
005100 77  NOO                         PIC X       VALUE 'N'.                   
005200                                                                          
005300 77  LF-UTF8                     PIC X       VALUE X'0A'.                 
005400 77  CR-UTF8                     PIC X       VALUE X'0D'.                 
005500 77  SPACE-UTF8                  PIC X       VALUE X'20'.                 
005600 77  LF-EBCDIC                   PIC X       VALUE X'25'.                 
005700 77  CR-EBCDIC                   PIC X       VALUE X'0D'.                 
005800 77  SPACE-EBCDIC                PIC X       VALUE X'40'.                 
005900                                                                          
006000 77  REC-LEN                     PIC 9(4).                                
006100                                                                          
006200 77  W11150-EOF-SW               PIC X       VALUE 'N'.                   
006300     88  END-OF-W11150                       VALUE 'J'.                   
006400                                                                          
006500 77  EXCEPTION-SW                PIC X       VALUE 'N'.                   
006600     88  EXCEPTION-YES                       VALUE 'J'.                   
006700     88  EXCEPTION-NO                        VALUE 'N'.                   
006800                                                                          
006900 77  CDATA-SW                    PIC X       VALUE ' '.                   
007000     88  CDATA-START                         VALUE 'S'.                   
007100     88  CDATA-END                           VALUE 'E'.                   
007200                                                                          
007300 77  CDATA-EXIST-SW              PIC X       VALUE 'N'.                   
007400     88  CDATA-EXIST                         VALUE 'Y'.                   
007500                                                                          
007600 77  WS-LINE                     PIC X(251).                              
007700                                                                          
007800 77  ERROR-POINT                 PIC 9(9)    VALUE ZERO.                  
007900 77  MAX-LRECL                   PIC 9(9)    VALUE 9999.                  
008000 77  PROCESSED-LINES             PIC 9(9)    VALUE ZERO.                  
008100 77  ERROR-LINE                  PIC 9(9)    VALUE ZERO.                  
008200 77  ERROR-POSITION              PIC 9(9)    VALUE ZERO.                  
008300 77  WS-XML-TEXT                 PIC X(300)  VALUE SPACES.                
008400 77  WS-XML-BUFF                 PIC X(300)  VALUE SPACES.                
008500 77  WS-POS                      PIC 9(9)    VALUE 1.                     
008600 77  WS-OCSRESP-COUNT            PIC 9(9)    VALUE ZERO.                  
008700 77  WS-XML-CODE                 PIC S9(9).                               
008800                                                                          
008900 01  WS-DATE                     PIC X(19).                               
009000 01  FILLER REDEFINES WS-DATE.                                            
009100     03  WS-DATE-CC              PIC 9(2).                                
009200     03  WS-DATE-YY              PIC 9(2).                                
009300     03  FILLER                  PIC X.                                   
009400     03  WS-DATE-MM              PIC 9(2).                                
009500     03  FILLER                  PIC X.                                   
009600     03  WS-DATE-DD              PIC 9(2).                                
009700     03  FILLER                  PIC X(9).                                
009800 01  FILLER REDEFINES WS-DATE.                                            
009900     03  WS-DATE2-DD             PIC 9(2).                                
010000     03  FILLER                  PIC X.                                   
010100     03  WS-DATE2-MM             PIC 9(2).                                
010200     03  FILLER                  PIC X.                                   
010300     03  WS-DATE2-CC             PIC 9(2).                                
010400     03  WS-DATE2-YY             PIC 9(2).                                
010500                                                                          
010600 01  WS-YYMMDD.                                                           
010700     03  WS-YY                   PIC 9(2).                                
010800     03  WS-MM                   PIC 9(2).                                
010900     03  WS-DD                   PIC 9(2).                                
011000                                                                          
011100 01  WS-IDARTNR                  PIC X(30).                               
011200 01  FILLER REDEFINES WS-IDARTNR.                                         
011300     03  WS-IDARTNR-X9           PIC X(9).                                
011400     03  FILLER                  PIC X(21).                               
011500                                                                          
011600 01  WS-LEN                      PIC S9(9) BINARY VALUE 0.                
011700 01  WS-TEXT-LEN                 PIC S9(9) BINARY VALUE 0.                
011800 01  WS-DATA-LEN                 PIC S9(9) BINARY VALUE 0.                
011900*01  WS-DATA                     PIC X(104857600) VALUE ALL X'40'.        
012000 01  WS-DATA.                                                             
012100     03 FILLER OCCURS 1 TO 104857600 TIMES                                
012200        DEPENDING ON WS-DATA-LEN PIC X VALUE X'40'.                       
012300                                                                          
012400 01  WS-CDATA-LEN                PIC S9(9) BINARY VALUE 0.                
012500*01  WS-CDATA                    PIC X(104857600) VALUE ALL X'40'.        
012600 01  WS-CDATA.                                                            
012700     03 FILLER OCCURS 1 TO 104857600 TIMES                                
012800        DEPENDING ON WS-CDATA-LEN PIC X VALUE X'40'.                      
012900                                                                          
013000 01  EL                          PIC S9(4) COMP.                          
013100 01  ELEMENT-GROUP.                                                       
013200     03  CURR-ELEMENT            PIC X(30) OCCURS 20 TIMES                
013300                                           VALUE SPACES.                  
013400                                                                          
013500 01  GENERAL-SUBPROGRAMS.                                                 
013600*                                                                         
013700     03  ABEND                   PIC X(8)    VALUE 'ABEND '.              
013800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
013900                                                                          
014000*    --- PARAMETERS TO ABEND                                              
014100                                                                          
014200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
014300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
014400 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
014500                                                                          
014600 01  ERROR-TEXT.                                                          
014700     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
014800     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
014900                                                                          
015000*    --- PARAMETRAR TILL POSTSUM                                          
015100*01  -COPY W0005   -PRE  POSTSUM-                                         
015200                                                                          
015300 01  UT-AREA-START               PIC X(24)   VALUE                        
015400                                 'UT-AREA-START  '.                       
015500                                                                          
015600*01  AREA -COPY W11151     -PRE UT-                                       
015700                                                                          
015800 PROCEDURE DIVISION.                                                      
015900 MAIN SECTION.                                                            
016000                                                                          
016100     PERFORM A-INIT                                                       
016200*Read complete file. So XML can be parsed as a single document.           
016300     PERFORM S01-READ-W11150                                              
016400                                                                          
016500     IF WS-DATA-LEN > 0                                                   
016600*First parse.                                                             
016700       XML PARSE WS-DATA (1 : WS-DATA-LEN)                                
016800         PROCESSING PROCEDURE B-HANDLE-PARSE                              
016900         ON EXCEPTION                                                     
017000           PERFORM S21-HANDLE-EXCEPTION                                   
017100       END-XML                                                            
017200                                                                          
017300*Second parse. If the first parse contained CDATA, then we parse          
017400*information inside CDATA again.                                          
017500       IF CDATA-EXIST AND EXCEPTION-NO                                    
017600         DISPLAY ' '                                                      
017700         DISPLAY 'CDATA LEN : ' WS-CDATA-LEN                              
017800         MOVE FUNCTION TRIM (WS-CDATA (1 : WS-CDATA-LEN))                 
017900                                 TO WS-CDATA (1 : WS-CDATA-LEN)           
018000         XML PARSE WS-CDATA (1 : WS-CDATA-LEN)                            
018100           PROCESSING PROCEDURE B-HANDLE-PARSE                            
018200           ON EXCEPTION                                                   
018300             PERFORM S21-HANDLE-EXCEPTION                                 
018400         END-XML                                                          
018500       END-IF                                                             
018600                                                                          
018700       DISPLAY ' '                                                        
018800       DISPLAY 'TOTAL NUMBER OF OCS RESPONSES RECEIVED : '                
018900                                    WS-OCSRESP-COUNT                      
019000     END-IF                                                               
019100                                                                          
019200     PERFORM Z-FINIT                                                      
019300                                                                          
019400     MOVE ZERO TO RETURN-CODE                                             
019500     GOBACK                                                               
019600     .                                                                    
019700                                                                          
019800 A-INIT SECTION.                                                          
019900                                                                          
020000     OPEN INPUT  W11150                                                   
020100                                                                          
020200     OPEN OUTPUT W11151                                                   
020300                                                                          
020400     MOVE IDPGM                  TO POSTSUM-PROGNAMN                      
020500     .                                                                    
020600                                                                          
020700 B-HANDLE-PARSE SECTION.                                                  
020800                                                                          
020900     COMPUTE WS-TEXT-LEN = FUNCTION LENGTH (XML-TEXT)                     
021000                                                                          
021100     MOVE SPACES                 TO WS-XML-TEXT                           
021200     MOVE XML-TEXT               TO WS-XML-TEXT                           
021300                                                                          
021400     IF XML-EVENT = 'EXCEPTION'                                           
021500       PERFORM S21-HANDLE-EXCEPTION                                       
021600     ELSE                                                                 
021700       IF WS-TEXT-LEN > 300                                               
021800*Exception for CDATA because it can contain large data and is             
021900*handled separately later.                                                
022000         IF CDATA-START                                                   
022100           CONTINUE                                                       
022200         ELSE                                                             
022300           DISPLAY 'XML-TEXT LONGER THAN WHAT CAN BE HANDLED.'            
022400           DISPLAY 'XML-EVENT = ' XML-EVENT                               
022500           DISPLAY 'XML-TEXT  = ' XML-TEXT                                
022600           PERFORM S99-ABEND                                              
022700         END-IF                                                           
022800       END-IF                                                             
022900                                                                          
023000       EVALUATE XML-EVENT                                                 
023100         WHEN 'CONTENT-CHARACTERS'                                        
023200           PERFORM BB-CONTENT-CHAR                                        
023300         WHEN 'START-OF-ELEMENT'                                          
023400           PERFORM BA-START-OF-ELEMENT                                    
023500         WHEN 'END-OF-ELEMENT'                                            
023600           PERFORM BC-END-OF-ELEMENT                                      
023700         WHEN 'START-OF-CDATA-SECTION'                                    
023800           IF CURR-ELEMENT (EL) = 'BODY'                                  
023900             SET CDATA-START     TO TRUE                                  
024000             SET CDATA-EXIST     TO TRUE                                  
024100           ELSE                                                           
024200             DISPLAY 'NOT EXPECTING CDATA FOR - '                         
024300                        CURR-ELEMENT (EL)                                 
024400           END-IF                                                         
024500         WHEN 'END-OF-CDATA-SECTION'                                      
024600           SET CDATA-END         TO TRUE                                  
024700         WHEN 'START-OF-DOCUMENT'                                         
024800           MOVE 0                TO EL                                    
024900         WHEN OTHER                                                       
025000           CONTINUE                                                       
025100       END-EVALUATE                                                       
025200     END-IF                                                               
025300     .                                                                    
025400                                                                          
025500 BA-START-OF-ELEMENT SECTION.                                             
025600                                                                          
025700     ADD 1                       TO EL                                    
025800     MOVE 1                      TO WS-POS                                
025900     MOVE FUNCTION UPPER-CASE (WS-XML-TEXT)                               
026000                                 TO CURR-ELEMENT (EL)                     
026100     MOVE SPACES                 TO WS-XML-BUFF                           
026200     IF CURR-ELEMENT (EL) = 'OCSRESPONSE'                                 
026300       ADD 1                     TO WS-OCSRESP-COUNT                      
026400     END-IF                                                               
026500     .                                                                    
026600                                                                          
026700 BB-CONTENT-CHAR SECTION.                                                 
026800                                                                          
026900     IF XML-INFORMATION = 2                                               
027000*      There is more data to come for the same element.                   
027100       PERFORM BBA-HANDLE-XML-TEXT                                        
027200     ELSE                                                                 
027300       PERFORM BBA-HANDLE-XML-TEXT                                        
027400                                                                          
027500       EVALUATE CURR-ELEMENT (EL)                                         
027600         WHEN 'COUNTRY'                                                   
027700           IF CURR-ELEMENT (EL - 1) = 'OCSRESPONSE'                       
027800             MOVE WS-XML-BUFF (1 : WS-POS - 1)                            
027900                                 TO UT-KDARTURS-PCOO                      
028000           END-IF                                                         
028100         WHEN 'AGREEMENT'                                                 
028200           MOVE WS-XML-BUFF (1 : WS-POS - 1)                              
028300                                 TO UT-BEAVTAL                            
028400         WHEN 'SUPPLIERNUMBER'                                            
028500           MOVE WS-XML-BUFF (1 : WS-POS - 1)                              
028600                                 TO UT-IDLEVNR                            
028700         WHEN 'PARTNUMBER'                                                
028800           MOVE WS-XML-BUFF (1 : WS-POS - 1)                              
028900                                 TO WS-IDARTNR                            
029000           MOVE WS-IDARTNR-X9    TO UT-IDARTNR                            
029100         WHEN 'EFFECTIVEDATE'                                             
029200           MOVE WS-XML-BUFF (1 : WS-POS - 1)                              
029300                                 TO WS-DATE                               
029400           PERFORM BBB-FORMAT-DATE                                        
029500           MOVE WS-YYMMDD        TO UT-TIGILTIG-FOM                       
029600         WHEN 'EXPIRATIONDATE'                                            
029700           MOVE WS-XML-BUFF (1 : WS-POS - 1)                              
029800                                 TO WS-DATE                               
029900           PERFORM BBB-FORMAT-DATE                                        
030000           MOVE WS-YYMMDD        TO UT-TIGILTIG-TOM                       
030100         WHEN 'CERTSTATUS'                                                
030200           MOVE WS-XML-BUFF (1 : WS-POS - 1)                              
030300                                 TO UT-IDSTAMIC                           
030400         WHEN 'DATETIME'                                                  
030500         WHEN 'TRANSACTIONID'                                             
030600           DISPLAY CURR-ELEMENT (EL) ' = '                                
030700                   WS-XML-BUFF (1 : WS-POS - 1)                           
030800         WHEN OTHER                                                       
030900           CONTINUE                                                       
031000       END-EVALUATE                                                       
031100       MOVE SPACES               TO WS-XML-BUFF                           
031200     END-IF                                                               
031300     .                                                                    
031400                                                                          
031500 BBA-HANDLE-XML-TEXT SECTION.                                             
031600                                                                          
031700     IF CURR-ELEMENT (EL) = 'BODY'                                        
031800       IF WS-TEXT-LEN > ZERO                                              
031900         COMPUTE WS-CDATA-LEN = WS-CDATA-LEN + WS-TEXT-LEN                
032000         MOVE XML-TEXT           TO WS-CDATA (WS-POS: WS-TEXT-LEN)        
032100         COMPUTE WS-POS = WS-POS + WS-TEXT-LEN                            
032200       END-IF                                                             
032300     ELSE                                                                 
032400       STRING WS-XML-TEXT (1 : WS-TEXT-LEN) DELIMITED BY SIZE             
032500                               INTO WS-XML-BUFF                           
032600                       WITH POINTER WS-POS                                
032700     END-IF                                                               
032800     .                                                                    
032900                                                                          
033000 BBB-FORMAT-DATE SECTION.                                                 
033100                                                                          
033200     IF WS-DATE (3:1) = '-'                                               
033300       MOVE WS-DATE2-YY          TO WS-YY                                 
033400       MOVE WS-DATE2-MM          TO WS-MM                                 
033500       MOVE WS-DATE2-DD          TO WS-DD                                 
033600     ELSE                                                                 
033700       MOVE WS-DATE-YY           TO WS-YY                                 
033800       MOVE WS-DATE-MM           TO WS-MM                                 
033900       MOVE WS-DATE-DD           TO WS-DD                                 
034000     END-IF                                                               
034100     .                                                                    
034200                                                                          
034300 BC-END-OF-ELEMENT SECTION.                                               
034400                                                                          
034500     IF FUNCTION UPPER-CASE (WS-XML-TEXT) = 'OCSRESPONSE'                 
034600       IF CURR-ELEMENT (EL) = 'OCSRESPONSE'                               
034700         PERFORM S11-WRITE-W11151                                         
034800       ELSE                                                               
034900         DISPLAY 'MISSING TAGS ??'                                        
035000         PERFORM S99-ABEND                                                
035100       END-IF                                                             
035200     END-IF                                                               
035300                                                                          
035400     MOVE SPACES                 TO CURR-ELEMENT (EL)                     
035500                                    WS-XML-BUFF                           
035600     SUBTRACT 1                FROM EL                                    
035700     .                                                                    
035800                                                                          
035900 S21-HANDLE-EXCEPTION SECTION.                                            
036000                                                                          
036100     SET EXCEPTION-YES           TO TRUE                                  
036200     MOVE SPACES                 TO WS-XML-TEXT                           
036300     MOVE XML-TEXT               TO WS-XML-TEXT                           
036400     ADD 1 TO LENGTH OF XML-TEXT GIVING ERROR-POINT                       
036500     MOVE 'PROCESSING ...'                                                
036600                                 TO WS-LINE                               
036700     DISPLAY WS-LINE                                                      
036800                                                                          
036900     MOVE '!!! AN EXCEPTION OCCURED WHILE PARSING INPUT XML !!!'          
037000                                 TO WS-LINE                               
037100     DISPLAY WS-LINE                                                      
037200     MOVE SPACES                 TO WS-LINE                               
037300                                                                          
037400     STRING 'EXCEPTION AT POSITION ' ERROR-POINT                          
037500             DELIMITED BY SIZE INTO WS-LINE                               
037600     DISPLAY WS-LINE                                                      
037700     MOVE SPACES                 TO WS-LINE                               
037800                                                                          
037900     COMPUTE PROCESSED-LINES = ERROR-POINT / MAX-LRECL                    
038000     COMPUTE ERROR-POSITION = FUNCTION MOD(ERROR-POINT, MAX-LRECL)        
038100     IF ERROR-POSITION = ZERO                                             
038200       MOVE PROCESSED-LINES      TO ERROR-LINE                            
038300       MOVE MAX-LRECL            TO ERROR-POSITION                        
038400     ELSE                                                                 
038500       COMPUTE ERROR-LINE = PROCESSED-LINES + 1                           
038600     END-IF                                                               
038700                                                                          
038800     STRING 'APPROX, ON INPUT REC ' ERROR-LINE                            
038900            ' AT POS ' ERROR-POSITION                                     
039000             DELIMITED BY SIZE INTO WS-LINE                               
039100     DISPLAY WS-LINE                                                      
039200     MOVE SPACES                 TO WS-LINE                               
039300                                                                          
039400     STRING 'EXCEPTION CODE (XML-CODE) = '                                
039500             FUNCTION HEX-OF (XML-CODE)                                   
039600             DELIMITED BY SIZE INTO WS-LINE                               
039700     DISPLAY WS-LINE                                                      
039800     MOVE SPACES                 TO WS-LINE                               
039900     PERFORM S99-ABEND                                                    
040000     .                                                                    
040100                                                                          
040200 Z-FINIT SECTION.                                                         
040300                                                                          
040400     CLOSE W11150                                                         
040500           W11151                                                         
040600                                                                          
040700     MOVE 'S' TO POSTSUM-OPKOD                                            
040800     CALL POSTSUM USING POSTSUM-PARM                                      
040900     .                                                                    
041000                                                                          
041100 S01-READ-W11150  SECTION.                                                
041200                                                                          
041300     PERFORM UNTIL END-OF-W11150                                          
041400       READ W11150                                                        
041500       AT END                                                             
041600         SET END-OF-W11150       TO TRUE                                  
041700         MOVE 0                  TO WS-LEN                                
041800       NOT AT END                                                         
041900*        When the message was converted from UTF8 to 278, some            
042000*        chars cant be translated to dest codepage and we get             
042100*        x'3F' instead. This will cause problems for XML parse.           
042200         INSPECT IN-RECORD REPLACING ALL X'3F' BY SPACES                  
042300         COMPUTE WS-LEN = REC-LEN                                         
042400         COMPUTE WS-DATA-LEN = WS-DATA-LEN + WS-LEN                       
042500         MOVE IN-RECORD (1 : WS-LEN)                                      
042600                                 TO WS-DATA (WS-POS : WS-LEN)             
042700         COMPUTE WS-POS = WS-POS + WS-LEN                                 
042800                                                                          
042900         MOVE 'W11150 '          TO POSTSUM-FDNAMN                        
043000         MOVE 'W11150D1'         TO POSTSUM-DDNAMN2                       
043100         MOVE SPACE              TO POSTSUM-TRANSTYP                      
043200         CALL POSTSUM         USING POSTSUM-PARM                          
043300       END-READ                                                           
043400     END-PERFORM                                                          
043500     DISPLAY 'TOTAL LEN = ' WS-DATA-LEN                                   
043600     DISPLAY ' '                                                          
043700     .                                                                    
043800                                                                          
043900 S11-WRITE-W11151 SECTION.                                                
044000                                                                          
044100     WRITE UT-RECORD           FROM UT-AREA                               
044200                                                                          
044300     MOVE SPACE                  TO POSTSUM-TRANSTYP                      
044400     MOVE 'W11151'               TO POSTSUM-FDNAMN                        
044500     MOVE 'W11150D2'             TO POSTSUM-DDNAMN2                       
044600     CALL POSTSUM             USING POSTSUM-PARM                          
044700                                                                          
044800     MOVE SPACES                 TO UT-AREA                               
044900     .                                                                    
045000                                                                          
045100 S99-ABEND SECTION.                                                       
045200                                                                          
045300     MOVE 'S' TO POSTSUM-OPKOD                                            
045400     CALL POSTSUM USING POSTSUM-PARM                                      
045500     CALL ABEND USING RKOD-ABEND-NO-DUMP                                  
045600     .                                                                    
