000100***************************************************************           
000200 ID  DIVISION.                                                            
000300***************************************************************           
000400 PROGRAM-ID.    V1615100.                                                 
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
004800     SELECT INFILE ASSIGN  TO V16151D1.                                   
004900     SELECT OUTFILE ASSIGN TO V16151D2.                                   
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
007300 01  PROGRAM-NAME                 PIC X(8)  VALUE 'V1615100'.             
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
010500     03 V1615110                  PIC X(8)  VALUE 'V1615110'.             
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
011600     10 INFILE-USERID             PIC X(8).                               
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
013600*++INCLUDE V161WS                                                         
013700*-------------------------------- DB2-AREAS                               
013800     EXEC SQL                                                             
013900          INCLUDE SQLCA                                                   
014000     END-EXEC.                                                            
014100     EXEC SQL                                                             
014200          INCLUDE EXTRACT                                                 
014300     END-EXEC.                                                            
014400*    -COPY EXTRACT -PRE EXTRACT-                                          
014500     EXEC SQL                                                             
014600          INCLUDE EXTUSER                                                 
014700     END-EXEC.                                                            
014800*    -COPY EXTUSER -PRE EXTUSER-                                          
014900     EJECT                                                                
015000                                                                          
015100 01  PARM-CODE                    PIC X(1).                               
015200                                                                          
015300 01  PARM-USERID                  PIC X(8).                               
015400                                                                          
015500*    -COPY V16110CC -PRE PARM-                                            
015600*++INCLUDE  V16110CC                                                      
015700                                                                          
015800*************************************************************             
015900 PROCEDURE DIVISION.                                                      
016000*************************************************************             
016100     PERFORM A-INIT                                                       
016200     PERFORM S10-READ-INFILE                                              
016300     IF INFILE-USERID = '*' THEN                                          
016400                                                                          
016500        MOVE 'I'   TO PARM-CODE                                           
016600        MOVE '*'   TO PARM-USERID                                         
016700        MOVE SPACE TO PARM-EXTRACTID                                      
016800        CALL V1611010                                                     
016900        USING SW-EXTRACT-FOUND, PARM-CODE, PARM-V16110                    
017000        MOVE SPACE TO PARM-CODE                                           
017100                                                                          
017200        CALL V1611010                                                     
017300        USING SW-EXTRACT-FOUND, PARM-CODE, PARM-V16110                    
017400                                                                          
017500        IF PARM-CODE = HIGH-VALUE                                         
017600            MOVE NOO TO SW-EXTRACT-FOUND                                  
017700            MOVE RCODE-4 TO RCODE                                         
017800            PERFORM  C-INIT-OUTFILE-AREA                                  
017900            PERFORM  S10-WRITE-OUTFILE                                    
018000        END-IF                                                            
018100                                                                          
018200        PERFORM UNTIL PARM-CODE = HIGH-VALUE                              
018300                                                                          
018400           MOVE YES TO SW-EXTRACT-FOUND                                   
018500           PERFORM  C-INIT-OUTFILE-AREA                                   
018600           PERFORM  S10-WRITE-OUTFILE                                     
018700                                                                          
018800           CALL V1611010                                                  
018900           USING SW-EXTRACT-FOUND, PARM-CODE, PARM-V16110                 
019000                                                                          
019100        END-PERFORM                                                       
019200                                                                          
019300     ELSE                                                                 
019400       PERFORM UNTIL SW-EOF-INFILE = YES                                  
019500         MOVE INFILE-USERID TO PARM-USERID                                
019600         MOVE 'I' TO PARM-CODE                                            
019700         CALL V1615110 USING                                              
019800         SW-EXTRACT-FOUND, PARM-CODE, PARM-USERID, PARM-V16110            
019900         MOVE SPACE TO PARM-CODE                                          
020000                                                                          
020100         CALL V1615110 USING                                              
020200         SW-EXTRACT-FOUND, PARM-CODE, PARM-USERID, PARM-V16110            
020300                                                                          
020400         IF PARM-CODE = HIGH-VALUE                                        
020500            MOVE NOO TO SW-EXTRACT-FOUND                                  
020600            MOVE RCODE-4 TO RCODE                                         
020700            PERFORM  C-INIT-OUTFILE-AREA                                  
020800            PERFORM  S10-WRITE-OUTFILE                                    
020900         END-IF                                                           
021000                                                                          
021100         PERFORM UNTIL PARM-CODE = HIGH-VALUE                             
021200                                                                          
021300            MOVE YES TO SW-EXTRACT-FOUND                                  
021400            PERFORM  C-INIT-OUTFILE-AREA                                  
021500            PERFORM  S10-WRITE-OUTFILE                                    
021600                                                                          
021700            CALL V1615110 USING                                           
021800            SW-EXTRACT-FOUND, PARM-CODE, PARM-USERID, PARM-V16110         
021900                                                                          
022000         END-PERFORM                                                      
022100         PERFORM  S10-READ-INFILE                                         
022200       END-PERFORM                                                        
022300     END-IF                                                               
022400     PERFORM Z-FINIT                                                      
022500     GOBACK                                                               
022600     CONTINUE.                                                            
022700                                                                          
022800*--------------------------------------------------------------           
022900 A-INIT SECTION.                                                          
023000*--------------------------------------------------------------           
023100     MOVE 'A-INIT                  ' TO ABEND-SECTION                     
023200D    DISPLAY ABEND-SECTION                                                
023300     SKIP2                                                                
023400     PERFORM S05-OPEN-INFILE                                              
023500     PERFORM S05-OPEN-OUTFILE                                             
023600                                                                          
023700     MOVE SPACE TO PARM-USERID                                            
023800     MOVE SPACE TO PARM-EXTRACTID                                         
023900     CONTINUE.                                                            
024000     EJECT                                                                
024100*--------------------------------------------------------------           
024200 B-INIT-OUTFILE-AREA SECTION.                                             
024300*--------------------------------------------------------------           
024400     MOVE 'BA-EDIT-OUTFILE-AREA  ' TO ABEND-SECTION                       
024500D    DISPLAY ABEND-SECTION                                                
024600     SKIP2                                                                
024700     MOVE SPACE                     TO OUTFILE-V161001                    
024800     MOVE EXTRACT-EXTRACTID         TO OUTFILE-EXTRACTID                  
024900     IF SW-EXTRACT-FOUND = YES THEN                                       
025000       MOVE EXTRACT-DESCRIPTION       TO OUTFILE-DESCRIPTION              
025100       MOVE EXTRACT-SIZEX             TO OUTFILE-SIZE                     
025200       MOVE EXTRACT-EXTRACT-FILE      TO V16196-DSNAME                    
025300       MOVE EXTRACT-VALID-NO-DAYS     TO V16196-VALID-NO-DAYS             
025400       MOVE SPACE                     TO V16196-LAST-UPDATED              
025500       MOVE SPACE                     TO V16196-VALID-TO-DATE             
025600       CALL V16196 USING V16196-PARMS                                     
025700* ÄNDRAD 920716 MB                                                        
025800*      CALL V16196 USING V16196-DSNAME                                    
025900*                        V16196-VALID-NO-DAYS                             
026000*                        V16196-LAST-UPDATED                              
026100*                        V16196-VALID-TO-DATE                             
026200       IF RETURN-CODE = 0 THEN                                            
026300          MOVE V16196-LAST-UPDATED    TO OUTFILE-LAST-UPDATED             
026400          MOVE V16196-VALID-TO-DATE   TO OUTFILE-VALID-TO-DATE            
026500          MOVE V16196-DSNAME          TO OUTFILE-EXTRACT-FILE             
026600       END-IF                                                             
026700       MOVE EXTRACT-TYPEX             TO OUTFILE-TYPE                     
026800       MOVE EXTRACT-EXTRACT-JOB       TO OUTFILE-EXTRACT-JOB              
026900       MOVE EXTRACT-JOB-TYPE          TO OUTFILE-JOB-TYPE                 
027000       MOVE EXTRACT-COPYTEXT          TO OUTFILE-COPYTEXT                 
027100       MOVE EXTRACT-OWNER             TO OUTFILE-OWNER                    
027200       IF EXTUSER-AUTH = 'A'                                              
027300          MOVE 'I' TO OUTFILE-AUTH                                        
027400       ELSE                                                               
027500          MOVE EXTUSER-AUTH TO OUTFILE-AUTH                               
027600       END-IF                                                             
027700     ELSE                                                                 
027800        MOVE EXT01                    TO OUTFILE-DESCRIPTION              
027900     END-IF                                                               
028000     CONTINUE.                                                            
028100                                                                          
028200     EJECT                                                                
028300*--------------------------------------------------------------           
028400 C-INIT-OUTFILE-AREA SECTION.                                             
028500*--------------------------------------------------------------           
028600     MOVE 'C-INIT-OUTFILE-AREA   ' TO ABEND-SECTION                       
028700D    DISPLAY ABEND-SECTION                                                
028800     SKIP2                                                                
028900     MOVE SPACE                  TO OUTFILE-V161001                       
029000     MOVE PARM-EXTRACTID         TO OUTFILE-EXTRACTID                     
029100                                                                          
029200*    MOVE YES TO SW-EXTRACT-FOUND                                         
029300     IF SW-EXTRACT-FOUND = YES THEN                                       
029400       MOVE PARM-DESCRIPTION       TO OUTFILE-DESCRIPTION                 
029500       MOVE PARM-SIZE              TO OUTFILE-SIZE                        
029600       MOVE PARM-EXTRACT-FILE      TO V16196-DSNAME                       
029700       MOVE PARM-VALID-NO-DAYS     TO V16196-VALID-NO-DAYS                
029800       MOVE SPACE                  TO V16196-LAST-UPDATED                 
029900       MOVE SPACE                  TO V16196-VALID-TO-DATE                
030000       CALL V16196 USING V16196-PARMS                                     
030100* ÄNDRAD 920716 MB                                                        
030200*      CALL V16196 USING V16196-DSNAME                                    
030300*                        V16196-VALID-NO-DAYS                             
030400*                        V16196-LAST-UPDATED                              
030500*                        V16196-VALID-TO-DATE                             
030600       IF RETURN-CODE = 0 THEN                                            
030700          MOVE V16196-LAST-UPDATED    TO OUTFILE-LAST-UPDATED             
030800          MOVE V16196-VALID-TO-DATE   TO OUTFILE-VALID-TO-DATE            
030900          MOVE V16196-DSNAME          TO OUTFILE-EXTRACT-FILE             
031000       END-IF                                                             
031100       MOVE PARM-TYPE                 TO OUTFILE-TYPE                     
031200       MOVE PARM-EXTRACT-JOB          TO OUTFILE-EXTRACT-JOB              
031300       MOVE PARM-JOB-TYPE             TO OUTFILE-JOB-TYPE                 
031400       MOVE PARM-COPYTEXT             TO OUTFILE-COPYTEXT                 
031500       MOVE PARM-OWNER                TO OUTFILE-OWNER                    
031600       IF PARM-AUTH = 'A'                                                 
031700          MOVE 'I' TO OUTFILE-AUTH                                        
031800       ELSE                                                               
031900          MOVE PARM-AUTH                 TO OUTFILE-AUTH                  
032000       END-IF                                                             
032100                                                                          
032200     ELSE                                                                 
032300        MOVE EXT01                    TO OUTFILE-DESCRIPTION              
032400     END-IF                                                               
032500     CONTINUE.                                                            
032600     EJECT                                                                
032700*--------------------------------------------------------------           
032800 Z-FINIT SECTION.                                                         
032900*--------------------------------------------------------------           
033000     MOVE 'Z-FINIT                  ' TO ABEND-SECTION                    
033100D    DISPLAY ABEND-SECTION                                                
033200     SKIP2                                                                
033300     IF SW-INFILE-OPEN = YES THEN                                         
033400        PERFORM S15-CLOSE-INFILE                                          
033500     END-IF                                                               
033600     IF SW-OUTFILE-OPEN = YES THEN                                        
033700        PERFORM S15-CLOSE-OUTFILE                                         
033800     END-IF                                                               
033900     MOVE RCODE TO RETURN-CODE                                            
034000     CONTINUE.                                                            
034100     EJECT                                                                
034200                                                                          
034300*---------------------------------------------------------------*         
034400 S05-OPEN-INFILE SECTION.                                                 
034500*---------------------------------------------------------------*         
034600     MOVE 'S05-OPEN-INFILE           ' TO ABEND-SECTION                   
034700D    DISPLAY ABEND-SECTION                                                
034800     SKIP2                                                                
034900                                                                          
035000     MOVE NOO TO SW-INFILE-OPEN                                           
035100     OPEN INPUT INFILE                                                    
035200     IF RETURN-CODE = 0 THEN                                              
035300        MOVE YES TO SW-INFILE-OPEN                                        
035400     ELSE                                                                 
035500        MOVE RCODE-16 TO RCODE                                            
035600        PERFORM Z-FINIT                                                   
035700        GOBACK                                                            
035800     END-IF                                                               
035900     CONTINUE.                                                            
036000     EJECT                                                                
036100                                                                          
036200*--------------------------------------------------------------           
036300 S05-OPEN-OUTFILE SECTION.                                                
036400*--------------------------------------------------------------           
036500     MOVE 'S05-OPEN-OUTFILE           ' TO ABEND-SECTION                  
036600D    DISPLAY ABEND-SECTION                                                
036700     SKIP2                                                                
036800     MOVE NOO TO SW-OUTFILE-OPEN                                          
036900     OPEN OUTPUT OUTFILE                                                  
037000     IF RETURN-CODE = 0 THEN                                              
037100        MOVE YES TO SW-OUTFILE-OPEN                                       
037200     ELSE                                                                 
037300        MOVE RCODE-16 TO RCODE                                            
037400        PERFORM Z-FINIT                                                   
037500        GOBACK                                                            
037600     END-IF                                                               
037700     CONTINUE.                                                            
037800     SKIP2                                                                
037900*---------------------------------------------------------------*         
038000 S10-READ-INFILE SECTION.                                                 
038100*---------------------------------------------------------------*         
038200     MOVE 'S10-READ-INFILE           ' TO ABEND-SECTION                   
038300D    DISPLAY ABEND-SECTION                                                
038400     SKIP2                                                                
038500                                                                          
038600     READ                                                                 
038700        INFILE INTO INFILE-V161002                                        
038800        AT END MOVE YES TO SW-EOF-INFILE                                  
038900     END-READ                                                             
039000     IF RETURN-CODE > 0 THEN                                              
039100        MOVE RCODE-16 TO RCODE                                            
039200        PERFORM Z-FINIT                                                   
039300        GOBACK                                                            
039400     END-IF                                                               
039500D    DISPLAY INFILE-V161002                                               
039600     CONTINUE.                                                            
039700     EJECT                                                                
039800                                                                          
039900*--------------------------------------------------------------           
040000 S10-WRITE-OUTFILE SECTION.                                               
040100*--------------------------------------------------------------           
040200     MOVE 'S10-WRITE-OUTFILE          ' TO ABEND-SECTION                  
040300D    DISPLAY ABEND-SECTION                                                
040400     SKIP2                                                                
040500D    DISPLAY OUTFILE-V161001                                              
040600     WRITE OUTFILE-POST FROM OUTFILE-V161001                              
040700     IF RETURN-CODE > 0 THEN                                              
040800        PERFORM S15-CLOSE-OUTFILE                                         
040900        MOVE RCODE-16 TO RETURN-CODE                                      
041000        GOBACK                                                            
041100     END-IF                                                               
041200     CONTINUE.                                                            
041300     SKIP2                                                                
041400*---------------------------------------------------------------*         
041500 S15-CLOSE-INFILE SECTION.                                                
041600*---------------------------------------------------------------*         
041700     MOVE 'S15-CLOSE-INFILE          ' TO ABEND-SECTION                   
041800D    DISPLAY ABEND-SECTION                                                
041900     SKIP2                                                                
042000                                                                          
042100     CLOSE INFILE                                                         
042200     IF RETURN-CODE = 0 THEN                                              
042300        MOVE NOO TO SW-INFILE-OPEN                                        
042400     ELSE                                                                 
042500        MOVE NOO TO SW-INFILE-OPEN                                        
042600        MOVE RCODE-16 TO RCODE                                            
042700        PERFORM Z-FINIT                                                   
042800        GOBACK                                                            
042900     END-IF                                                               
043000     CONTINUE.                                                            
043100     EJECT                                                                
043200                                                                          
043300*--------------------------------------------------------------           
043400 S15-CLOSE-OUTFILE SECTION.                                               
043500*--------------------------------------------------------------           
043600     MOVE 'S15-CLOSE-OUTFILE          ' TO ABEND-SECTION                  
043700D    DISPLAY ABEND-SECTION                                                
043800     SKIP2                                                                
043900     CLOSE OUTFILE                                                        
044000     IF RETURN-CODE = 0 THEN                                              
044100        MOVE NOO TO SW-OUTFILE-OPEN                                       
044200     ELSE                                                                 
044300        MOVE NOO TO SW-OUTFILE-OPEN                                       
044400        MOVE RCODE-16 TO RCODE                                            
044500        PERFORM Z-FINIT                                                   
044600        GOBACK                                                            
044700     END-IF                                                               
044800     CONTINUE.                                                            
044900     EJECT                                                                
045000*--------------------------------------------------------------           
045100 T05-OPEN-DECLARE-C1 SECTION.                                             
045200*--------------------------------------------------------------           
045300     MOVE 'T05-OPEN-DECLARE-C1      ' TO ABEND-SECTION                    
045400D    DISPLAY ABEND-SECTION                                                
045500     SKIP2                                                                
045600     EXEC SQL                                                             
045700       DECLARE C1 CURSOR FOR                                              
045800       SELECT A.EXTRACTID, A.DESCRIPTION, A.SIZE,                         
045900              A.VALID_NO_DAYS, A.EXTRACT_FILE,                            
046000              A.TYPE, A.EXTRACT_JOB,                                      
046100              A.JOB_TYPE, A.COPYTEXT, A.OWNER, B.AUTH                     
046200       FROM EXTRACT      A,                                               
046300            EXTRACT_USER B                                                
046400       WHERE A.EXTRACTID = B.EXTRACTID                                    
046500       AND   B.USERID = USER                                              
046600       ORDER BY EXTRACTID                                                 
046700     END-EXEC                                                             
046800     EXEC SQL                                                             
046900       OPEN C1                                                            
047000     END-EXEC                                                             
047100     PERFORM S95-CONTROL-SQLCODE                                          
047200     CONTINUE.                                                            
047300     SKIP2                                                                
047400*--------------------------------------------------------------           
047500 T10-FETCH-EXTRACTID SECTION.                                             
047600*--------------------------------------------------------------           
047700     MOVE 'T10-FETCH-EXTRACTID      ' TO ABEND-SECTION                    
047800D    DISPLAY ABEND-SECTION                                                
047900     SKIP2                                                                
048000     EXEC SQL                                                             
048100       FETCH C1 INTO :EXTRACT-EXTRACTID,                                  
048200                     :EXTRACT-DESCRIPTION,                                
048300                     :EXTRACT-SIZEX,                                      
048400                     :EXTRACT-VALID-NO-DAYS,                              
048500                     :EXTRACT-EXTRACT-FILE,                               
048600                     :EXTRACT-TYPEX,                                      
048700                     :EXTRACT-EXTRACT-JOB,                                
048800                     :EXTRACT-JOB-TYPE,                                   
048900                     :EXTRACT-COPYTEXT:WS-NULL-IND1,                      
049000                     :EXTRACT-OWNER,                                      
049100                     :EXTUSER-AUTH                                        
049200     END-EXEC                                                             
049300     PERFORM S95-CONTROL-SQLCODE                                          
049400     IF SQLCODE = +000 THEN                                               
049500       IF WS-NULL-IND1 < 0 OR EXTRACT-COPYTEXT = SPACE THEN               
049600         MOVE '-' TO EXTRACT-COPYTEXT                                     
049700       END-IF                                                             
049800       MOVE YES  TO SW-EXTRACT-FOUND                                      
049900     ELSE                                                                 
050000       MOVE NOO  TO SW-EXTRACT-FOUND                                      
050100     END-IF                                                               
050200     CONTINUE.                                                            
050300     SKIP2                                                                
050400*--------------------------------------------------------------           
050500 T20-CLOSE-C1 SECTION.                                                    
050600*--------------------------------------------------------------           
050700     MOVE 'T20-CLOSE-C1             ' TO ABEND-SECTION                    
050800D    DISPLAY ABEND-SECTION                                                
050900     SKIP2                                                                
051000     EXEC SQL                                                             
051100       CLOSE C1                                                           
051200     END-EXEC                                                             
051300     CONTINUE.                                                            
051400     SKIP2                                                                
051500*---------------------------------------------------------------*         
051600 T25-SELECT-EXTRACTID SECTION.                                            
051700*---------------------------------------------------------------*         
051800     MOVE 'T25-SELECT-EXTRACTID      ' TO ABEND-SECTION                   
051900D    DISPLAY ABEND-SECTION                                                
052000     SKIP2                                                                
052100                                                                          
052200     EXEC SQL                                                             
052300       SELECT A.EXTRACTID, A.DESCRIPTION, A.SIZE, A.VALID_NO_DAYS,        
052400              A.EXTRACT_FILE, A.TYPE, A.EXTRACT_JOB, A.JOB_TYPE,          
052500              A.COPYTEXT, A.OWNER, B.AUTH                                 
052600       INTO  :EXTRACT-EXTRACTID,                                          
052700             :EXTRACT-DESCRIPTION,                                        
052800             :EXTRACT-SIZEX,                                              
052900             :EXTRACT-VALID-NO-DAYS,                                      
053000             :EXTRACT-EXTRACT-FILE,                                       
053100             :EXTRACT-TYPEX,                                              
053200             :EXTRACT-EXTRACT-JOB,                                        
053300             :EXTRACT-JOB-TYPE,                                           
053400             :EXTRACT-COPYTEXT:WS-NULL-IND1,                              
053500             :EXTRACT-OWNER,                                              
053600             :EXTUSER-AUTH                                                
053700       FROM   EXTRACT A,                                                  
053800              EXTRACT_USER B                                              
053900       WHERE  A.EXTRACTID     = :EXTRACT-EXTRACTID                        
054000       AND    A.EXTRACTID = B.EXTRACTID                                   
054100       AND    B.USERID = USER                                             
054200     END-EXEC                                                             
054300     PERFORM S95-CONTROL-SQLCODE                                          
054400     IF SQLCODE = +000 THEN                                               
054500       IF WS-NULL-IND1 < 0 OR EXTRACT-COPYTEXT = SPACE THEN               
054600         MOVE '-' TO EXTRACT-COPYTEXT                                     
054700       END-IF                                                             
054800       MOVE YES  TO SW-EXTRACT-FOUND                                      
054900     ELSE                                                                 
055000       MOVE NOO  TO SW-EXTRACT-FOUND                                      
055100     END-IF                                                               
055200     CONTINUE.                                                            
055300     EJECT                                                                
055400*-------------------------------- DB2 FELHANTERING SEKTION                
055500*    -COPY V161PS                                                         
055600*++INCLUDE V161PS                                                         
