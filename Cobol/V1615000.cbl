000100***************************************************************           
000200 ID  DIVISION.                                                            
000300***************************************************************           
000400 PROGRAM-ID.    V1615000.                                                 
000500 AUTHOR.        CARINA HOLMQVIST                                          
000600 DATE-WRITTEN.  SEPTEMBER 1991                                            
000700***************************************************************           
000800*                                                                         
000900*REMARKS:                                                                 
001000*                                                                         
001100*  * RETURNS ON OUTFILE EXTRACTS VALID FOR CURRENT USER                   
001200*                                                                         
001300*  * DB2-TABLES:                                                          
001400*                                                                         
001500*        EXTRACT,EXTRACT_USER                                             
001600*                                                                         
001700*  * SUBPROGRAMS:                                                         
001800*                                                                         
001900*        V16196 - RETURNS CREATION DATE FOR EXTRACT-FILE                  
002000*                                                                         
002100*  * OUTFILE DESCRIPTION:                                                 
002200*                                                                         
002300*        EXTRACTID,DESCRIPTION,SIZE,                                      
002400*        LAST-UPDATED,VALID-TO-DATE,AUTH                                  
002500*                                                                         
002600*  * RETURNCODES:                                                         
002700*                                                                         
002800*         4 - NO VALID EXTRACT REGISTERED FOR CURRENT USER                
002900*        16 - SEVERE FILE-ERROR                                           
003000*        20 - SEVERE DB2-ERROR                                            
003100*                                                                         
003200***************************************************************           
003300     EJECT                                                                
003400***************************************************************           
003500 ENVIRONMENT DIVISION.                                                    
003600***************************************************************           
003700     SKIP2                                                                
003800*--------------------------------------------------------------           
003900 CONFIGURATION SECTION.                                                   
004000*--------------------------------------------------------------           
004100 SOURCE-COMPUTER. IBM-370.                                                
004200*SOURCE-COMPUTER. IBM-370 WITH DEBUGGING MODE.                            
004300     SKIP2                                                                
004400*--------------------------------------------------------------           
004500 INPUT-OUTPUT SECTION.                                                    
004600*--------------------------------------------------------------           
004700 FILE-CONTROL.                                                            
004800     SELECT INFILE ASSIGN  TO V16150D1.                                   
004900     SELECT OUTFILE ASSIGN TO V16150D2.                                   
005000     EJECT                                                                
005100***************************************************************           
005200 DATA DIVISION.                                                           
005300***************************************************************           
005400     SKIP2                                                                
005500*--------------------------------------------------------------           
005600 FILE SECTION.                                                            
005700*--------------------------------------------------------------           
005800 FD  INFILE                                                               
005900     BLOCK CONTAINS 0                                                     
006000     RECORDING F                                                          
006100     LABEL RECORD STANDARD.                                               
006200 01  INFILE-POST                  PIC X(80).                              
006300                                                                          
006400 FD  OUTFILE                                                              
006500     BLOCK CONTAINS 0                                                     
006600     RECORDING F                                                          
006700     LABEL RECORD STANDARD.                                               
006800 01  OUTFILE-POST                 PIC X(180).                             
006900     SKIP2                                                                
007000*--------------------------------------------------------------           
007100 WORKING-STORAGE SECTION.                                                 
007200*--------------------------------------------------------------           
007300 01  PROGRAM-NAME                 PIC X(8)  VALUE 'V1615000'.             
007400 01  ABEND-SECTION                PIC X(25) VALUE SPACE.                  
007500 01  RETURN-CODES.                                                        
007600     03  RCODE                    PIC S9(4) COMP SYNC VALUE ZERO.         
007700     03  RCODE-4                  PIC S9(4) COMP SYNC VALUE 4.            
007800     03  RCODE-8                  PIC S9(4) COMP SYNC VALUE 8.            
007900     03  RCODE-16                 PIC S9(4) COMP SYNC VALUE 16.           
008000     03  RCODE-20                 PIC S9(4) COMP SYNC VALUE 20.           
008100     SKIP2                                                                
008200 01  GENERAL-CONSTANTS.                                                   
008300     03 YES                       PIC X(1)  VALUE 'Y'.                    
008400     03 NOO                       PIC X(1)  VALUE 'N'.                    
008500     SKIP2                                                                
008600 01  WS-VARIABLER.                                                        
008700     03 WS-NULL-IND1              PIC S9(4) COMP.                         
008800     SKIP2                                                                
008900 01  ERROR-MESSAGES.                                                      
009000     03 EXT01.                                                            
009100        05 FILLER                 PIC X(3)  VALUE '***'.                  
009200        05 MESSAGE-ID             PIC X(5)  VALUE 'EXT01'.                
009300        05 MESSAGE-TEXT           PIC X(25) VALUE                         
009400        'NOT AUTHORIZED           '.                                      
009500     SKIP2                                                                
009600 01  SWITCHES.                                                            
009700     03 SW-EOF-INFILE             PIC X(1)  VALUE 'N'.                    
009800     03 SW-INFILE-OPEN            PIC X(1)  VALUE 'N'.                    
009900     03 SW-OUTFILE-OPEN           PIC X(1)  VALUE 'N'.                    
010000     03 SW-VALID-NO-DAYS-MISSING  PIC X(1)  VALUE 'N'.                    
010100     03 SW-EXTRACT-FOUND          PIC X(1)  VALUE 'N'.                    
010200     SKIP2                                                                
010300 01  DYNAMIC-SUBPROGRAMS.                                                 
010400     03 V1611010                  PIC X(8)  VALUE 'V1611010'.             
010500     03 V1615010                  PIC X(8)  VALUE 'V1615010'.             
010600     03 V16196                    PIC X(8)  VALUE 'V16196  '.             
010700     SKIP2                                                                
010800 01  V16196-PARMS.                                                        
010900     03 V16196-DSNAME             PIC X(44) VALUE SPACE.                  
011000     03 V16196-VALID-NO-DAYS      PIC 999   VALUE ZERO.                   
011100     03 V16196-LAST-UPDATED       PIC X(6)  VALUE SPACE.                  
011200     03 V16196-VALID-TO-DATE      PIC X(6)  VALUE SPACE.                  
011300     EJECT                                                                
011400*-------------------------------- COPYTEXT INFILE                         
011500 01  INFILE-V161002.                                                      
011600     10 INFILE-EXTRACTID          PIC X(8).                               
011700     10 INFILE-FILLER             PIC X(72).                              
011800     SKIP2                                                                
011900*-------------------------------- COPY TEXT OUTFILE                       
012000 01  OUTFILE-V161001.                                                     
012100     03 OUTFILE-EXTRACTID          PIC X(8).                              
012200     03 OUTFILE-DESCRIPTION        PIC X(25).                             
012300     03 OUTFILE-SIZE               PIC ZZZZ9.                             
012400     03 OUTFILE-LAST-UPDATED       PIC X(6).                              
012500     03 OUTFILE-VALID-TO-DATE      PIC X(6).                              
012600     03 OUTFILE-EXTRACT-FILE       PIC X(44).                             
012700     03 OUTFILE-TYPE               PIC X(8).                              
012800     03 OUTFILE-EXTRACT-JOB        PIC X(52).                             
012900     03 OUTFILE-JOB-TYPE           PIC X(1).                              
013000     03 OUTFILE-COPYTEXT           PIC X(8).                              
013100     03 OUTFILE-OWNER              PIC X(7).                              
013200     03 OUTFILE-AUTH               PIC X(1).                              
013300     SKIP2                                                                
013400*-------------------------------- DB2 ERROR HANDLING                      
013500*    -COPY V161WS                                                         
013600*-------------------------------- DB2-AREAS                               
013700     EXEC SQL                                                             
013800          INCLUDE SQLCA                                                   
013900     END-EXEC.                                                            
014000     EXEC SQL                                                             
014100          INCLUDE EXTRACT                                                 
014200     END-EXEC.                                                            
014300*    -COPY EXTRACT -PRE EXTRACT-                                          
014400     EXEC SQL                                                             
014500          INCLUDE EXTUSER                                                 
014600     END-EXEC.                                                            
014700*    -COPY EXTUSER -PRE EXTUSER-                                          
014800     EJECT                                                                
014900                                                                          
015000 01  PARM-CODE                    PIC X(1).                               
015100*    -COPY V16110 -PRE PARM-                                              
015200                                                                          
015300*************************************************************             
015400 PROCEDURE DIVISION.                                                      
015500*************************************************************             
015600     PERFORM A-INIT                                                       
015700     PERFORM S10-READ-INFILE                                              
015800     IF INFILE-EXTRACTID = '*' THEN                                       
015900                                                                          
016000        MOVE 'I' TO PARM-CODE                                             
016100        MOVE '*' TO PARM-EXTRACTID                                        
016200        CALL V1611010                                                     
016300        USING SW-EXTRACT-FOUND, PARM-CODE, PARM-V16110                    
016400        MOVE SPACE TO PARM-CODE                                           
016500                                                                          
016600        CALL V1611010                                                     
016700        USING SW-EXTRACT-FOUND, PARM-CODE, PARM-V16110                    
016800                                                                          
016900        IF PARM-CODE = HIGH-VALUE                                         
017000            MOVE NOO TO SW-EXTRACT-FOUND                                  
017100            MOVE RCODE-4 TO RCODE                                         
017200            PERFORM  C-INIT-OUTFILE-AREA                                  
017300            PERFORM  S10-WRITE-OUTFILE                                    
017400        END-IF                                                            
017500                                                                          
017600        PERFORM UNTIL PARM-CODE = HIGH-VALUE                              
017700                                                                          
017800           MOVE YES TO SW-EXTRACT-FOUND                                   
017900           PERFORM  C-INIT-OUTFILE-AREA                                   
018000           PERFORM  S10-WRITE-OUTFILE                                     
018100                                                                          
018200           CALL V1611010                                                  
018300           USING SW-EXTRACT-FOUND, PARM-CODE, PARM-V16110                 
018400                                                                          
018500        END-PERFORM                                                       
018600                                                                          
018700     ELSE                                                                 
018800       PERFORM UNTIL SW-EOF-INFILE = YES                                  
018900         MOVE INFILE-EXTRACTID TO PARM-EXTRACTID                          
019000         MOVE 'I' TO PARM-CODE                                            
019100         CALL V1615010                                                    
019200         USING SW-EXTRACT-FOUND, PARM-CODE, PARM-V16110                   
019300         MOVE SPACE TO PARM-CODE                                          
019400                                                                          
019500         CALL V1615010                                                    
019600         USING SW-EXTRACT-FOUND, PARM-CODE, PARM-V16110                   
019700                                                                          
019800         IF PARM-CODE = HIGH-VALUE                                        
019900            MOVE NOO TO SW-EXTRACT-FOUND                                  
020000            MOVE RCODE-4 TO RCODE                                         
020100            PERFORM  C-INIT-OUTFILE-AREA                                  
020200            PERFORM  S10-WRITE-OUTFILE                                    
020300         END-IF                                                           
020400                                                                          
020500         PERFORM UNTIL PARM-CODE = HIGH-VALUE                             
020600                                                                          
020700            MOVE YES TO SW-EXTRACT-FOUND                                  
020800            PERFORM  C-INIT-OUTFILE-AREA                                  
020900            PERFORM  S10-WRITE-OUTFILE                                    
021000                                                                          
021100            CALL V1615010                                                 
021200            USING SW-EXTRACT-FOUND, PARM-CODE, PARM-V16110                
021300                                                                          
021400         END-PERFORM                                                      
021500         PERFORM  S10-READ-INFILE                                         
021600       END-PERFORM                                                        
021700     END-IF                                                               
021800     PERFORM Z-FINIT                                                      
021900     GOBACK                                                               
022000     CONTINUE.                                                            
022100                                                                          
022200*--------------------------------------------------------------           
022300 A-INIT SECTION.                                                          
022400*--------------------------------------------------------------           
022500     MOVE 'A-INIT                  ' TO ABEND-SECTION                     
022600D    DISPLAY ABEND-SECTION                                                
022700     SKIP2                                                                
022800     PERFORM S05-OPEN-INFILE                                              
022900     PERFORM S05-OPEN-OUTFILE                                             
023000                                                                          
023100     MOVE SPACE TO PARM-EXTRACTID                                         
023200     CONTINUE.                                                            
023300     EJECT                                                                
023400*--------------------------------------------------------------           
023500 B-INIT-OUTFILE-AREA SECTION.                                             
023600*--------------------------------------------------------------           
023700     MOVE 'BA-EDIT-OUTFILE-AREA  ' TO ABEND-SECTION                       
023800D    DISPLAY ABEND-SECTION                                                
023900     SKIP2                                                                
024000     MOVE SPACE                     TO OUTFILE-V161001                    
024100     MOVE EXTRACT-EXTRACTID         TO OUTFILE-EXTRACTID                  
024200     IF SW-EXTRACT-FOUND = YES THEN                                       
024300       MOVE EXTRACT-DESCRIPTION       TO OUTFILE-DESCRIPTION              
024400       MOVE EXTRACT-SIZEX             TO OUTFILE-SIZE                     
024500       MOVE EXTRACT-EXTRACT-FILE      TO V16196-DSNAME                    
024600       MOVE EXTRACT-VALID-NO-DAYS     TO V16196-VALID-NO-DAYS             
024700       MOVE SPACE                     TO V16196-LAST-UPDATED              
024800       MOVE SPACE                     TO V16196-VALID-TO-DATE             
024900       CALL V16196 USING V16196-PARMS                                     
025000* ÄNDRAD 920716 MB                                                        
025100*      CALL V16196 USING V16196-DSNAME                                    
025200*                        V16196-VALID-NO-DAYS                             
025300*                        V16196-LAST-UPDATED                              
025400*                        V16196-VALID-TO-DATE                             
025500       IF RETURN-CODE = 0 THEN                                            
025600          MOVE V16196-LAST-UPDATED    TO OUTFILE-LAST-UPDATED             
025700          MOVE V16196-VALID-TO-DATE   TO OUTFILE-VALID-TO-DATE            
025800          MOVE V16196-DSNAME          TO OUTFILE-EXTRACT-FILE             
025900       END-IF                                                             
026000       MOVE EXTRACT-TYPEX             TO OUTFILE-TYPE                     
026100       MOVE EXTRACT-EXTRACT-JOB       TO OUTFILE-EXTRACT-JOB              
026200       MOVE EXTRACT-JOB-TYPE          TO OUTFILE-JOB-TYPE                 
026300       MOVE EXTRACT-COPYTEXT          TO OUTFILE-COPYTEXT                 
026400       MOVE EXTRACT-OWNER             TO OUTFILE-OWNER                    
026500       IF EXTUSER-AUTH = 'A'                                              
026600          MOVE 'I' TO OUTFILE-AUTH                                        
026700       ELSE                                                               
026800          MOVE EXTUSER-AUTH TO OUTFILE-AUTH                               
026900       END-IF                                                             
027000     ELSE                                                                 
027100        MOVE EXT01                    TO OUTFILE-DESCRIPTION              
027200     END-IF                                                               
027300     CONTINUE.                                                            
027400                                                                          
027500     EJECT                                                                
027600*--------------------------------------------------------------           
027700 C-INIT-OUTFILE-AREA SECTION.                                             
027800*--------------------------------------------------------------           
027900     MOVE 'C-INIT-OUTFILE-AREA   ' TO ABEND-SECTION                       
028000D    DISPLAY ABEND-SECTION                                                
028100     SKIP2                                                                
028200     MOVE SPACE                  TO OUTFILE-V161001                       
028300     MOVE PARM-EXTRACTID         TO OUTFILE-EXTRACTID                     
028400                                                                          
028500*    MOVE YES TO SW-EXTRACT-FOUND                                         
028600     IF SW-EXTRACT-FOUND = YES THEN                                       
028700       MOVE PARM-DESCRIPTION       TO OUTFILE-DESCRIPTION                 
028800       MOVE PARM-SIZE              TO OUTFILE-SIZE                        
028900       MOVE PARM-EXTRACT-FILE      TO V16196-DSNAME                       
029000       MOVE PARM-VALID-NO-DAYS     TO V16196-VALID-NO-DAYS                
029100       MOVE SPACE                  TO V16196-LAST-UPDATED                 
029200       MOVE SPACE                  TO V16196-VALID-TO-DATE                
029300       CALL V16196 USING V16196-PARMS                                     
029400* ÄNDRAD 920716 MB                                                        
029500*      CALL V16196 USING V16196-DSNAME                                    
029600*                        V16196-VALID-NO-DAYS                             
029700*                        V16196-LAST-UPDATED                              
029800*                        V16196-VALID-TO-DATE                             
029900       IF RETURN-CODE = 0 THEN                                            
030000          MOVE V16196-LAST-UPDATED    TO OUTFILE-LAST-UPDATED             
030100          MOVE V16196-VALID-TO-DATE   TO OUTFILE-VALID-TO-DATE            
030200          MOVE V16196-DSNAME          TO OUTFILE-EXTRACT-FILE             
030300       END-IF                                                             
030400       MOVE PARM-TYPE                 TO OUTFILE-TYPE                     
030500       MOVE PARM-EXTRACT-JOB          TO OUTFILE-EXTRACT-JOB              
030600       MOVE PARM-JOB-TYPE             TO OUTFILE-JOB-TYPE                 
030700       MOVE PARM-COPYTEXT             TO OUTFILE-COPYTEXT                 
030800       MOVE PARM-OWNER                TO OUTFILE-OWNER                    
030900       IF PARM-AUTH = 'A'                                                 
031000          MOVE 'I' TO OUTFILE-AUTH                                        
031100       ELSE                                                               
031200          MOVE PARM-AUTH                 TO OUTFILE-AUTH                  
031300       END-IF                                                             
031400                                                                          
031500     ELSE                                                                 
031600        MOVE EXT01                    TO OUTFILE-DESCRIPTION              
031700     END-IF                                                               
031800     CONTINUE.                                                            
031900     EJECT                                                                
032000*--------------------------------------------------------------           
032100 Z-FINIT SECTION.                                                         
032200*--------------------------------------------------------------           
032300     MOVE 'Z-FINIT                  ' TO ABEND-SECTION                    
032400D    DISPLAY ABEND-SECTION                                                
032500     SKIP2                                                                
032600     IF SW-INFILE-OPEN = YES THEN                                         
032700        PERFORM S15-CLOSE-INFILE                                          
032800     END-IF                                                               
032900     IF SW-OUTFILE-OPEN = YES THEN                                        
033000        PERFORM S15-CLOSE-OUTFILE                                         
033100     END-IF                                                               
033200     MOVE RCODE TO RETURN-CODE                                            
033300     CONTINUE.                                                            
033400     EJECT                                                                
033500                                                                          
033600*---------------------------------------------------------------*         
033700 S05-OPEN-INFILE SECTION.                                                 
033800*---------------------------------------------------------------*         
033900     MOVE 'S05-OPEN-INFILE           ' TO ABEND-SECTION                   
034000D    DISPLAY ABEND-SECTION                                                
034100     SKIP2                                                                
034200                                                                          
034300     MOVE NOO TO SW-INFILE-OPEN                                           
034400     OPEN INPUT INFILE                                                    
034500     IF RETURN-CODE = 0 THEN                                              
034600        MOVE YES TO SW-INFILE-OPEN                                        
034700     ELSE                                                                 
034800        MOVE RCODE-16 TO RCODE                                            
034900        PERFORM Z-FINIT                                                   
035000        GOBACK                                                            
035100     END-IF                                                               
035200     CONTINUE.                                                            
035300     EJECT                                                                
035400                                                                          
035500*--------------------------------------------------------------           
035600 S05-OPEN-OUTFILE SECTION.                                                
035700*--------------------------------------------------------------           
035800     MOVE 'S05-OPEN-OUTFILE           ' TO ABEND-SECTION                  
035900D    DISPLAY ABEND-SECTION                                                
036000     SKIP2                                                                
036100     MOVE NOO TO SW-OUTFILE-OPEN                                          
036200     OPEN OUTPUT OUTFILE                                                  
036300     IF RETURN-CODE = 0 THEN                                              
036400        MOVE YES TO SW-OUTFILE-OPEN                                       
036500     ELSE                                                                 
036600        MOVE RCODE-16 TO RCODE                                            
036700        PERFORM Z-FINIT                                                   
036800        GOBACK                                                            
036900     END-IF                                                               
037000     CONTINUE.                                                            
037100     SKIP2                                                                
037200*---------------------------------------------------------------*         
037300 S10-READ-INFILE SECTION.                                                 
037400*---------------------------------------------------------------*         
037500     MOVE 'S10-READ-INFILE           ' TO ABEND-SECTION                   
037600D    DISPLAY ABEND-SECTION                                                
037700     SKIP2                                                                
037800                                                                          
037900     READ                                                                 
038000        INFILE INTO INFILE-V161002                                        
038100        AT END MOVE YES TO SW-EOF-INFILE                                  
038200     END-READ                                                             
038300     IF RETURN-CODE > 0 THEN                                              
038400        MOVE RCODE-16 TO RCODE                                            
038500        PERFORM Z-FINIT                                                   
038600        GOBACK                                                            
038700     END-IF                                                               
038800D    DISPLAY INFILE-V161002                                               
038900     CONTINUE.                                                            
039000     EJECT                                                                
039100                                                                          
039200*--------------------------------------------------------------           
039300 S10-WRITE-OUTFILE SECTION.                                               
039400*--------------------------------------------------------------           
039500     MOVE 'S10-WRITE-OUTFILE          ' TO ABEND-SECTION                  
039600D    DISPLAY ABEND-SECTION                                                
039700     SKIP2                                                                
039800D    DISPLAY OUTFILE-V161001                                              
039900     WRITE OUTFILE-POST FROM OUTFILE-V161001                              
040000     IF RETURN-CODE > 0 THEN                                              
040100        PERFORM S15-CLOSE-OUTFILE                                         
040200        MOVE RCODE-16 TO RETURN-CODE                                      
040300        GOBACK                                                            
040400     END-IF                                                               
040500     CONTINUE.                                                            
040600     SKIP2                                                                
040700*---------------------------------------------------------------*         
040800 S15-CLOSE-INFILE SECTION.                                                
040900*---------------------------------------------------------------*         
041000     MOVE 'S15-CLOSE-INFILE          ' TO ABEND-SECTION                   
041100D    DISPLAY ABEND-SECTION                                                
041200     SKIP2                                                                
041300                                                                          
041400     CLOSE INFILE                                                         
041500     IF RETURN-CODE = 0 THEN                                              
041600        MOVE NOO TO SW-INFILE-OPEN                                        
041700     ELSE                                                                 
041800        MOVE NOO TO SW-INFILE-OPEN                                        
041900        MOVE RCODE-16 TO RCODE                                            
042000        PERFORM Z-FINIT                                                   
042100        GOBACK                                                            
042200     END-IF                                                               
042300     CONTINUE.                                                            
042400     EJECT                                                                
042500                                                                          
042600*--------------------------------------------------------------           
042700 S15-CLOSE-OUTFILE SECTION.                                               
042800*--------------------------------------------------------------           
042900     MOVE 'S15-CLOSE-OUTFILE          ' TO ABEND-SECTION                  
043000D    DISPLAY ABEND-SECTION                                                
043100     SKIP2                                                                
043200     CLOSE OUTFILE                                                        
043300     IF RETURN-CODE = 0 THEN                                              
043400        MOVE NOO TO SW-OUTFILE-OPEN                                       
043500     ELSE                                                                 
043600        MOVE NOO TO SW-OUTFILE-OPEN                                       
043700        MOVE RCODE-16 TO RCODE                                            
043800        PERFORM Z-FINIT                                                   
043900        GOBACK                                                            
044000     END-IF                                                               
044100     CONTINUE.                                                            
044200     EJECT                                                                
044300*--------------------------------------------------------------           
044400 T05-OPEN-DECLARE-C1 SECTION.                                             
044500*--------------------------------------------------------------           
044600     MOVE 'T05-OPEN-DECLARE-C1      ' TO ABEND-SECTION                    
044700D    DISPLAY ABEND-SECTION                                                
044800     SKIP2                                                                
044900     EXEC SQL                                                             
045000       DECLARE C1 CURSOR FOR                                              
045100       SELECT A.EXTRACTID, A.DESCRIPTION, A.SIZE,                         
045200              A.VALID_NO_DAYS, A.EXTRACT_FILE,                            
045300              A.TYPE, A.EXTRACT_JOB,                                      
045400              A.JOB_TYPE, A.COPYTEXT, A.OWNER, B.AUTH                     
045500       FROM EXTRACT      A,                                               
045600            EXTRACT_USER B                                                
045700       WHERE A.EXTRACTID = B.EXTRACTID                                    
045800       AND   B.USERID = USER                                              
045900       ORDER BY EXTRACTID                                                 
046000     END-EXEC                                                             
046100     EXEC SQL                                                             
046200       OPEN C1                                                            
046300     END-EXEC                                                             
046400     PERFORM S95-CONTROL-SQLCODE                                          
046500     CONTINUE.                                                            
046600     SKIP2                                                                
046700*--------------------------------------------------------------           
046800 T10-FETCH-EXTRACTID SECTION.                                             
046900*--------------------------------------------------------------           
047000     MOVE 'T10-FETCH-EXTRACTID      ' TO ABEND-SECTION                    
047100D    DISPLAY ABEND-SECTION                                                
047200     SKIP2                                                                
047300     EXEC SQL                                                             
047400       FETCH C1 INTO :EXTRACT-EXTRACTID,                                  
047500                     :EXTRACT-DESCRIPTION,                                
047600                     :EXTRACT-SIZEX,                                      
047700                     :EXTRACT-VALID-NO-DAYS,                              
047800                     :EXTRACT-EXTRACT-FILE,                               
047900                     :EXTRACT-TYPEX,                                      
048000                     :EXTRACT-EXTRACT-JOB,                                
048100                     :EXTRACT-JOB-TYPE,                                   
048200                     :EXTRACT-COPYTEXT:WS-NULL-IND1,                      
048300                     :EXTRACT-OWNER,                                      
048400                     :EXTUSER-AUTH                                        
048500     END-EXEC                                                             
048600     PERFORM S95-CONTROL-SQLCODE                                          
048700     IF SQLCODE = +000 THEN                                               
048800       IF WS-NULL-IND1 < 0 OR EXTRACT-COPYTEXT = SPACE THEN               
048900         MOVE '-' TO EXTRACT-COPYTEXT                                     
049000       END-IF                                                             
049100       MOVE YES  TO SW-EXTRACT-FOUND                                      
049200     ELSE                                                                 
049300       MOVE NOO  TO SW-EXTRACT-FOUND                                      
049400     END-IF                                                               
049500     CONTINUE.                                                            
049600     SKIP2                                                                
049700*--------------------------------------------------------------           
049800 T20-CLOSE-C1 SECTION.                                                    
049900*--------------------------------------------------------------           
050000     MOVE 'T20-CLOSE-C1             ' TO ABEND-SECTION                    
050100D    DISPLAY ABEND-SECTION                                                
050200     SKIP2                                                                
050300     EXEC SQL                                                             
050400       CLOSE C1                                                           
050500     END-EXEC                                                             
050600     CONTINUE.                                                            
050700     SKIP2                                                                
050800*---------------------------------------------------------------*         
050900 T25-SELECT-EXTRACTID SECTION.                                            
051000*---------------------------------------------------------------*         
051100     MOVE 'T25-SELECT-EXTRACTID      ' TO ABEND-SECTION                   
051200D    DISPLAY ABEND-SECTION                                                
051300     SKIP2                                                                
051400                                                                          
051500     EXEC SQL                                                             
051600       SELECT A.EXTRACTID, A.DESCRIPTION, A.SIZE, A.VALID_NO_DAYS,        
051700              A.EXTRACT_FILE, A.TYPE, A.EXTRACT_JOB, A.JOB_TYPE,          
051800              A.COPYTEXT, A.OWNER, B.AUTH                                 
051900       INTO  :EXTRACT-EXTRACTID,                                          
052000             :EXTRACT-DESCRIPTION,                                        
052100             :EXTRACT-SIZEX,                                              
052200             :EXTRACT-VALID-NO-DAYS,                                      
052300             :EXTRACT-EXTRACT-FILE,                                       
052400             :EXTRACT-TYPEX,                                              
052500             :EXTRACT-EXTRACT-JOB,                                        
052600             :EXTRACT-JOB-TYPE,                                           
052700             :EXTRACT-COPYTEXT:WS-NULL-IND1,                              
052800             :EXTRACT-OWNER,                                              
052900             :EXTUSER-AUTH                                                
053000       FROM   EXTRACT A,                                                  
053100              EXTRACT_USER B                                              
053200       WHERE  A.EXTRACTID     = :EXTRACT-EXTRACTID                        
053300       AND    A.EXTRACTID = B.EXTRACTID                                   
053400       AND    B.USERID = USER                                             
053500     END-EXEC                                                             
053600     PERFORM S95-CONTROL-SQLCODE                                          
053700     IF SQLCODE = +000 THEN                                               
053800       IF WS-NULL-IND1 < 0 OR EXTRACT-COPYTEXT = SPACE THEN               
053900         MOVE '-' TO EXTRACT-COPYTEXT                                     
054000       END-IF                                                             
054100       MOVE YES  TO SW-EXTRACT-FOUND                                      
054200     ELSE                                                                 
054300       MOVE NOO  TO SW-EXTRACT-FOUND                                      
054400     END-IF                                                               
054500     CONTINUE.                                                            
054600     EJECT                                                                
054700*-------------------------------- DB2 FELHANTERING SEKTION                
054800*    -COPY V161PS                                                         
