000100*****************************************************************         
000200 ID  DIVISION.                                                            
000300*****************************************************************         
000400 PROGRAM-ID.    V1613000.                                                 
000500 AUTHOR.        LEIF LJUNGKVIST                                           
000600 DATE-WRITTEN.  OCTOBER   1993                                            
000700***************************************************************           
000800*                                                                         
000900*REMARKS:                                                                 
001000*                                                                         
001100*  * COPIES ONE EXTRACT INCLUDING ALL AUTHORIZATIONS ON                   
001200*    THE EXTRACT                                                          
001300*                                                                         
001400*  * IF 'R' THEN DELETES OLD EXTRACT WITH AUTHORIZATIONS                  
001500*                                                                         
001600*  * DB2-TABLES:                                                          
001700*                                                                         
001800*        EXTRACT,EXTRACT_USER                                             
001900*                                                                         
002000*  * SUBPROGRAMS:                                                         
002100*                                                                         
002200*                                                                         
002300*                                                                         
002400*  * INPUT:                                                               
002500*                                                                         
002600*        CONTAINS A NEW AND OLD EXTRACT                                   
002700*                                                                         
002800*                                                                         
002900*  * RETURNCODES:                                                         
003000*                                                                         
003100*         4 - NO AUTHORIZATION FOR EXTRACTID                              
003200*        16 - SEVERE FILE-ERROR                                           
003300*        20 - SEVERE DB2-ERROR                                            
003400*                                                                         
003500***************************************************************           
003600                                                                          
003700*****************************************************************         
003800 ENVIRONMENT DIVISION.                                                    
003900*****************************************************************         
004000 CONFIGURATION SECTION.                                                   
004100 SOURCE-COMPUTER. IBM-370.                                                
004200*SOURCE-COMPUTER. IBM-370 WITH DEBUGGING MODE.                            
004300                                                                          
004400                                                                          
004500*****************************************************************         
004600 DATA DIVISION.                                                           
004700*****************************************************************         
004800                                                                          
004900 WORKING-STORAGE SECTION.                                                 
005000 01  PROGRAM-NAME                 PIC X(8)  VALUE 'V1614000'.             
005100 01  ABEND-SECTION                PIC X(25) VALUE SPACE.                  
005200 01  SWITCHES.                                                            
005300     03  SW-EOF-INFILE            PIC X(1)  VALUE 'N'.                    
005400     03  SW-INFILE-OPEN           PIC X(1)  VALUE 'N'.                    
005500     03  SW-OUTFILE-OPEN          PIC X(1)  VALUE 'N'.                    
005600     03  SW-EXTRACT-JOB-FOUND     PIC X(1)  VALUE 'Y'.                    
005700     03  SW-ERROR                 PIC X(1)  VALUE 'N'.                    
005800     03  SW-ENTER-PRESSED         PIC X(1)  VALUE 'N'.                    
005900*                                                                         
006000 01  RETURN-CODES.                                                        
006100     03  RCODE                    PIC S9(4) COMP SYNC VALUE ZERO.         
006200     03  RCODE-4                  PIC S9(4) COMP SYNC VALUE 4.            
006300     03  RCODE-7                  PIC S9(4) COMP SYNC VALUE 7.            
006400     03  RCODE-8                  PIC S9(4) COMP SYNC VALUE 8.            
006500     03  RCODE-16                 PIC S9(4) COMP SYNC VALUE 16.           
006600     03  RCODE-20                 PIC S9(4) COMP SYNC VALUE 20.           
006700     03  RCODE-DISPL              PIC Z(4)-.                              
006800*                                                                         
006900 01  GENERAL-CONSTANS.                                                    
007000     03 YES                       PIC X(1)  VALUE 'Y'.                    
007100     03 NOO                       PIC X(1)  VALUE 'N'.                    
007200*                                                                         
007300 01  WS-VARIABLER.                                                        
007400     03 W-PANEL                   PIC X(8)  VALUE SPACE.                  
007500     03 WS-USERID                 PIC X(7)  VALUE SPACE.                  
007600     03 W-EXTRACT                 PIC X(8).                               
007700     03 W-NEW-EXTRACT             PIC X(8).                               
007800     03 W-ERROR-MESSAGE           PIC X(40) VALUE SPACE.                  
007900*                                                                         
008000 01  ERROR-MESSAGES.                                                      
008100     03 EXT01.                                                            
008200        05 FILLER                 PIC X(3)  VALUE '***'.                  
008300        05 MESSAGE-ID             PIC X(5)  VALUE 'EXT01'.                
008400        05 MESSAGE-TEXT           PIC X(25) VALUE                         
008500        'NOT AUTHORIZED           '.                                      
008600*                                                                         
008700     SKIP2                                                                
008800 01  N-PANEL-V161300M.                                                    
008900     03 N-EXTRACTID               PIC X(8)  VALUE 'V161300A'.             
009000     03 N-DESCRIPTION             PIC X(8)  VALUE 'V161300B'.             
009100     03 N-NEW-EXTRACT             PIC X(8)  VALUE 'V161300C'.             
009200     SKIP2                                                                
009300 01  LTH-PANEL-V161300M.                                                  
009400     03 LTH-EXTRACTID             PIC S9(6) VALUE 8  COMP.                
009500     03 LTH-DESCRIPTION           PIC S9(6) VALUE 25 COMP.                
009600     03 LTH-NEW-EXTRACT           PIC S9(6) VALUE 8  COMP.                
009700     SKIP2                                                                
009800 01  ISPF-PANEL-V161300M.                                                 
009900     03 ISPF-EXTRACTID            PIC X(8)  VALUE SPACE.                  
010000     03 ISPF-DESCRIPTION          PIC X(25) VALUE SPACE.                  
010100     03 ISPF-NEW-EXTRACT          PIC X(8)  VALUE SPACE.                  
010200     SKIP2                                                                
010300*--------------------------------------------------------------           
010400*ISPF-CONSTANTS                                                           
010500*--------------------------------------------------------------           
010600 01  DISPLAYE                     PIC X(8)  VALUE 'DISPLAY '.             
010700 01  VDEFINE                      PIC X(8)  VALUE 'VDEFINE '.             
010800 01  VDELETE                      PIC X(8)  VALUE 'VDELETE '.             
010900 01  CHAR                         PIC X(8)  VALUE 'CHAR    '.             
011000                                                                          
011100 01  MESSAGES.                                                            
011200     03 MSG-VIOS101D              PIC X(8)  VALUE 'VIOS101D'.             
011300     03 MSG-VIOS101U              PIC X(8)  VALUE 'VIOS101U'.             
011400     03 MSG-VIOS101V              PIC X(8)  VALUE 'VIOS101V'.             
011500                                                                          
011600*--------------------------------------------------------------           
011700*ISPF-FIELDS                                                              
011800*--------------------------------------------------------------           
011900 01  V161300M                     PIC X(8)  VALUE 'V1613000'.             
012000 01  V161300N                     PIC X(8)  VALUE 'V1613100'.             
012100 01  ISPF-CURSOR                  PIC X(8)  VALUE SPACE.                  
012200 01  ISPF-MSG-ID                  PIC X(8)  VALUE SPACE.                  
012300*                                                                         
012400     EJECT                                                                
012500 01  DYNAMIC-SUBPROGRAMS.                                                 
012600     03 V16194                    PIC X(8)  VALUE 'V16194  '.             
012700     03 V16195                    PIC X(8)  VALUE 'V16195  '.             
012800     03 V1616060                  PIC X(8)  VALUE 'V1616060'.             
012900 01  VGETLID-PARMS.                                                       
013000     03 VGETLID-LENGTH            PIC S9(4) COMP VALUE ZERO.              
013100     03 VGETLID-LOGONID           PIC X(8)  VALUE SPACE.                  
013200*-------------------------------- DB2-AREAS                               
013300     EXEC SQL                                                             
013400          INCLUDE SQLCA                                                   
013500     END-EXEC.                                                            
013600     EXEC SQL                                                             
013700          INCLUDE EXTRACT                                                 
013800     END-EXEC.                                                            
013900*    -COPY EXTRACT -PRE EXTRACT-                                          
014000     EXEC SQL                                                             
014100          INCLUDE EXTUSER                                                 
014200     END-EXEC.                                                            
014300*    -COPY EXTUSER -PRE EXTUSER-                                          
014400*-------------------------------- DB2 ERROR HANDLING                      
014500*    -COPY V161WS                                                         
014600*++INCLUDE V161WSCCC0                                                     
014700                                                                          
014800 LINKAGE SECTION.                                                         
014900 01 PARM-CODE                    PIC X(1).                                
015000 01 PARM-EXTRACT                 PIC X(8).                                
015100 01 PARM-DESCRIPTION             PIC X(25).                               
015200 01 PARM-NEW-EXTRACT             PIC X(8).                                
015300                                                                          
015400     EJECT                                                                
015500*************************************************************             
015600 PROCEDURE DIVISION USING                                                 
015700                    PARM-CODE,                                            
015800                    PARM-EXTRACT,                                         
015900                    PARM-DESCRIPTION,                                     
016000                    PARM-NEW-EXTRACT.                                     
016100*************************************************************             
016200     PERFORM A-INIT                                                       
016300                                                                          
016400     PERFORM B-CHECK-AUTHORIZATION                                        
016500     IF SW-ERROR = NOO                                                    
016600        MOVE PARM-EXTRACT     TO ISPF-EXTRACTID                           
016700        MOVE PARM-DESCRIPTION TO ISPF-DESCRIPTION                         
016800        MOVE SPACE            TO ISPF-NEW-EXTRACT                         
016900                                                                          
017000        IF PARM-CODE = 'C'                                                
017100           MOVE V161300M TO W-PANEL                                       
017200        ELSE                                                              
017300           MOVE V161300N TO W-PANEL                                       
017400        END-IF                                                            
017500                                                                          
017600        PERFORM S01-DISPLAY-PANEL                                         
017700        PERFORM UNTIL  RCODE > 0                                          
017800           MOVE YES TO SW-ENTER-PRESSED                                   
017900           CALL V1616060 USING W-NEW-EXTRACT                              
018000           IF RETURN-CODE = +0                                            
018100              PERFORM T15-SELECT-EXTRACT                                  
018200              IF SQLCODE = +0                                             
018300                 MOVE RCODE-7 TO RCODE                                    
018400                 MOVE MSG-VIOS101D TO ISPF-MSG-ID                         
018500                 PERFORM S01-DISPLAY-PANEL                                
018600              ELSE                                                        
018700                 PERFORM C-COPY-EXTRACT                                   
018800                 IF PARM-CODE = 'R'                                       
018900                    PERFORM SQL-DELETE-OLD-EXTRACT-USER                   
019000                    PERFORM SQL-DELETE-OLD-EXTRACT                        
019100                 END-IF                                                   
019200                 PERFORM Z-FINIT                                          
019300                 GOBACK                                                   
019400              END-IF                                                      
019500           ELSE                                                           
019600              IF RETURN-CODE = +9                                         
019700                 MOVE MSG-VIOS101V TO ISPF-MSG-ID                         
019800              ELSE                                                        
019900                 MOVE MSG-VIOS101U TO ISPF-MSG-ID                         
020000              END-IF                                                      
020100              PERFORM S01-DISPLAY-PANEL                                   
020200           END-IF                                                         
020300        END-PERFORM                                                       
020400                                                                          
020500        MOVE NOO TO SW-ENTER-PRESSED                                      
020600        IF RCODE = 8                                                      
020700           MOVE RCODE-4 TO RCODE                                          
020800        END-IF                                                            
020900                                                                          
021000     END-IF                                                               
021100     PERFORM Z-FINIT                                                      
021200     GOBACK                                                               
021300     CONTINUE.                                                            
021400     EJECT                                                                
021500                                                                          
021600*---------------------------------------------------------------*         
021700 A-INIT SECTION.                                                          
021800*---------------------------------------------------------------*         
021900     MOVE 'A-INIT                   ' TO ABEND-SECTION                    
022000D    DISPLAY ABEND-SECTION                                                
022100     SKIP2                                                                
022200     PERFORM AA-VDEF-PANEL-V161300M                                       
022300                                                                          
022400     CONTINUE.                                                            
022500     EJECT                                                                
022600*--------------------------------------------------------------           
022700 AA-VDEF-PANEL-V161300M SECTION.                                          
022800*--------------------------------------------------------------           
022900     MOVE 'AB-VDEF-PANEL-V161300M    ' TO ABEND-SECTION                   
023000D    DISPLAY ABEND-SECTION                                                
023100     SKIP2                                                                
023200     CALL 'ISPLINK' USING VDEFINE N-EXTRACTID ISPF-EXTRACTID              
023300                          CHAR LTH-EXTRACTID                              
023400     CALL 'ISPLINK' USING VDEFINE N-DESCRIPTION ISPF-DESCRIPTION          
023500                          CHAR LTH-DESCRIPTION                            
023600     CALL 'ISPLINK' USING VDEFINE N-NEW-EXTRACT ISPF-NEW-EXTRACT          
023700                          CHAR LTH-NEW-EXTRACT                            
023800                                                                          
023900     CONTINUE.                                                            
024000     EJECT                                                                
024100*--------------------------------------------------------------           
024200 B-CHECK-AUTHORIZATION SECTION.                                           
024300*--------------------------------------------------------------           
024400     MOVE 'B-CHECK-AUTHORIZATION  ' TO ABEND-SECTION                      
024500D    DISPLAY ABEND-SECTION                                                
024600     SKIP2                                                                
024700     CALL V16194 USING PARM-EXTRACT                                       
024800     IF RETURN-CODE = 0                                                   
024900        MOVE NOO TO SW-ERROR                                              
025000     ELSE                                                                 
025100        MOVE YES TO SW-ERROR                                              
025200        MOVE RCODE-8 TO RCODE                                             
025300     END-IF                                                               
025400     CONTINUE.                                                            
025500                                                                          
025600*---------------------------------------------------------------*         
025700 C-COPY-EXTRACT      SECTION.                                             
025800*---------------------------------------------------------------*         
025900     MOVE 'C-COPY-EXTRACT           ' TO ABEND-SECTION                    
026000D    DISPLAY ABEND-SECTION                                                
026100     SKIP2                                                                
026200     MOVE PARM-EXTRACT     TO W-EXTRACT                                   
026300     MOVE PARM-EXTRACT     TO W-NEW-EXTRACT                               
026400     PERFORM  T15-SELECT-EXTRACT                                          
026500     MOVE ISPF-NEW-EXTRACT TO W-NEW-EXTRACT                               
026600     MOVE ISPF-NEW-EXTRACT TO PARM-NEW-EXTRACT                            
026700                                                                          
026800     PERFORM T16-INSERT-EXTRACT                                           
026900     PERFORM SQL-DECLARE-OPEN-EXTRACT-USER                                
027000     PERFORM SQL-FETCH-EXTRACT-USER                                       
027100     PERFORM UNTIL SQLCODE NOT = +0                                       
027200        PERFORM SQL-INSERT-EXTRACT-USER                                   
027300        PERFORM SQL-FETCH-EXTRACT-USER                                    
027400     END-PERFORM                                                          
027500     PERFORM SQL-CLOSE-EXTRACT                                            
027600     MOVE ZERO TO RCODE                                                   
027700                                                                          
027800     PERFORM SQL-COMMIT                                                   
027900                                                                          
028000                                                                          
028100     CONTINUE.                                                            
028200     EJECT                                                                
028300                                                                          
028400*---------------------------------------------------------------*         
028500 Z-FINIT SECTION.                                                         
028600*---------------------------------------------------------------*         
028700     MOVE 'Z-FINIT                  ' TO ABEND-SECTION                    
028800D    DISPLAY ABEND-SECTION                                                
028900     SKIP2                                                                
029000                                                                          
029100     PERFORM SQL-COMMIT                                                   
029200                                                                          
029300     MOVE RCODE TO RETURN-CODE                                            
029400     CONTINUE.                                                            
029500     EJECT                                                                
029600*--------------------------------------------------------------           
029700 S01-DISPLAY-PANEL SECTION.                                               
029800*--------------------------------------------------------------           
029900     MOVE 'S01-DISPLAY-PANEL           ' TO ABEND-SECTION                 
030000D    DISPLAY ABEND-SECTION                                                
030100     SKIP2                                                                
030200     CALL 'ISPLINK' USING DISPLAYE W-PANEL                                
030300                          ISPF-MSG-ID ISPF-CURSOR                         
030400     MOVE RETURN-CODE TO RCODE RCODE-DISPL                                
030500     IF RCODE > +12                                                       
030600        PERFORM S99-ERROR-ROUTINE                                         
030700        MOVE YES TO SW-ERROR                                              
030800        MOVE RCODE-16 TO RCODE                                            
030900     ELSE                                                                 
031000        MOVE ISPF-NEW-EXTRACT TO W-NEW-EXTRACT                            
031100     END-IF                                                               
031200     CONTINUE.                                                            
031300     EJECT                                                                
031400*--------------------------------------------------------------           
031500 S99-ERROR-ROUTINE SECTION.                                               
031600*--------------------------------------------------------------           
031700     SKIP2                                                                
031800     STRING 'ERROR IN ' ABEND-SECTION ' RCODE ='                          
031900     RCODE-DISPL DELIMITED BY SIZE                                        
032000     INTO W-ERROR-MESSAGE                                                 
032100     DISPLAY W-ERROR-MESSAGE                                              
032200     CONTINUE.                                                            
032300                                                                          
032400*---------------------------------------------------------------*         
032500 T15-SELECT-EXTRACT SECTION.                                              
032600*---------------------------------------------------------------*         
032700     MOVE 'T15-SELECT-EXTJOB       ' TO ABEND-SECTION                     
032800D    DISPLAY ABEND-SECTION                                                
032900     SKIP2                                                                
033000                                                                          
033100     EXEC SQL                                                             
033200     SELECT    TYPE,                                                      
033300               DESCRIPTION,                                               
033400               COPYTEXT,                                                  
033500               SIZE,                                                      
033600               VALID_NO_DAYS,                                             
033700               EXTRACT_FILE,                                              
033800               EXTRACT_JOB,                                               
033900               JOB_TYPE,                                                  
034000               OWNER                                                      
034100       INTO   :EXTRACT-TYPEX,                                             
034200              :EXTRACT-DESCRIPTION,                                       
034300              :EXTRACT-COPYTEXT,                                          
034400              :EXTRACT-SIZEX,                                             
034500              :EXTRACT-VALID-NO-DAYS,                                     
034600              :EXTRACT-EXTRACT-FILE,                                      
034700              :EXTRACT-EXTRACT-JOB,                                       
034800              :EXTRACT-JOB-TYPE,                                          
034900              :EXTRACT-OWNER                                              
035000     FROM EXTRACT                                                         
035100     WHERE EXTRACTID = :W-NEW-EXTRACT                                     
035200     END-EXEC                                                             
035300     PERFORM S95-CONTROL-SQLCODE                                          
035400     IF SQLCODE = +000 THEN                                               
035500       MOVE YES  TO SW-EXTRACT-JOB-FOUND                                  
035600     ELSE                                                                 
035700       MOVE NOO TO SW-EXTRACT-JOB-FOUND                                   
035800     END-IF                                                               
035900     CONTINUE.                                                            
036000     EJECT                                                                
036100                                                                          
036200*---------------------------------------------------------------*         
036300 T16-INSERT-EXTRACT SECTION.                                              
036400*---------------------------------------------------------------*         
036500     MOVE 'T16-INSERT-EXTACT       ' TO ABEND-SECTION                     
036600D    DISPLAY ABEND-SECTION                                                
036700     SKIP2                                                                
036800                                                                          
036900     EXEC SQL                                                             
037000       INSERT INTO EXTRACT                                                
037100        VALUES (                                                          
037200              :W-NEW-EXTRACT,                                             
037300              :EXTRACT-TYPEX,                                             
037400              :EXTRACT-DESCRIPTION,                                       
037500              :EXTRACT-COPYTEXT,                                          
037600              :EXTRACT-SIZEX,                                             
037700              :EXTRACT-VALID-NO-DAYS,                                     
037800              :EXTRACT-EXTRACT-FILE,                                      
037900              :EXTRACT-EXTRACT-JOB,                                       
038000              :EXTRACT-JOB-TYPE,                                          
038100              :EXTRACT-OWNER                                              
038200               )                                                          
038300     END-EXEC                                                             
038400     PERFORM S95-CONTROL-SQLCODE                                          
038500     IF SQLCODE = +000 THEN                                               
038600       MOVE YES  TO SW-EXTRACT-JOB-FOUND                                  
038700     ELSE                                                                 
038800       MOVE NOO TO SW-EXTRACT-JOB-FOUND                                   
038900     END-IF                                                               
039000     CONTINUE.                                                            
039100     EJECT                                                                
039200*--------------------------------------------------------------           
039300 SQL-DECLARE-OPEN-EXTRACT-USER SECTION.                                   
039400*--------------------------------------------------------------           
039500     MOVE 'SQL-DECLARE-OPEN-EXTRACT-USER' TO ABEND-SECTION                
039600D    DISPLAY ABEND-SECTION                                                
039700     SKIP2                                                                
039800     EXEC SQL                                                             
039900        DECLARE CRS-EXTRACT-USER CURSOR FOR                               
040000        SELECT EXTRACTID, USERID, AUTH                                    
040100        FROM EXTRACT_USER                                                 
040200        WHERE EXTRACTID = :W-EXTRACT                                      
040300     END-EXEC.                                                            
040400     EXEC SQL                                                             
040500         OPEN CRS-EXTRACT-USER                                            
040600     END-EXEC                                                             
040700     PERFORM S95-CONTROL-SQLCODE                                          
040800     CONTINUE.                                                            
040900     EJECT                                                                
041000*--------------------------------------------------------------           
041100 SQL-FETCH-EXTRACT-USER SECTION.                                          
041200*--------------------------------------------------------------           
041300     MOVE 'SQL-FETCH-EXTRACT-USER      ' TO ABEND-SECTION                 
041400D    DISPLAY ABEND-SECTION                                                
041500     SKIP2                                                                
041600     EXEC SQL                                                             
041700        FETCH CRS-EXTRACT-USER INTO :EXTUSER-EXTRACTID,                   
041800                               :EXTUSER-USERID,                           
041900                               :EXTUSER-AUTH                              
042000     END-EXEC                                                             
042100     PERFORM S95-CONTROL-SQLCODE                                          
042200     CONTINUE.                                                            
042300     EJECT                                                                
042400*--------------------------------------------------------------           
042500 SQL-CLOSE-EXTRACT SECTION.                                               
042600*--------------------------------------------------------------           
042700     MOVE 'SQL-CLOSE-EXTRACT-USER      ' TO ABEND-SECTION                 
042800D    DISPLAY ABEND-SECTION                                                
042900     SKIP2                                                                
043000     EXEC SQL                                                             
043100         CLOSE CRS-EXTRACT-USER                                           
043200     END-EXEC                                                             
043300     PERFORM S95-CONTROL-SQLCODE                                          
043400     CONTINUE.                                                            
043500     EJECT                                                                
043600                                                                          
043700*--------------------------------------------------------------           
043800 SQL-DELETE-OLD-EXTRACT SECTION.                                          
043900*--------------------------------------------------------------           
044000     MOVE 'SQL-DELETE-OLD-EXTRACT     ' TO ABEND-SECTION                  
044100D    DISPLAY ABEND-SECTION                                                
044200     SKIP2                                                                
044300     EXEC SQL                                                             
044400       DELETE FROM EXTRACT WHERE EXTRACTID = :PARM-EXTRACT                
044500     END-EXEC                                                             
044600     PERFORM S95-CONTROL-SQLCODE                                          
044700     CONTINUE.                                                            
044800     EJECT                                                                
044900                                                                          
045000*--------------------------------------------------------------           
045100 SQL-DELETE-OLD-EXTRACT-USER SECTION.                                     
045200*--------------------------------------------------------------           
045300     MOVE 'SQL-DELETE-OLD-EXTRACT     ' TO ABEND-SECTION                  
045400D    DISPLAY ABEND-SECTION                                                
045500     SKIP2                                                                
045600     EXEC SQL                                                             
045700       DELETE FROM EXTRACT_USER WHERE EXTRACTID = :PARM-EXTRACT           
045800     END-EXEC                                                             
045900     PERFORM S95-CONTROL-SQLCODE                                          
046000     CONTINUE.                                                            
046100     EJECT                                                                
046200                                                                          
046300*---------------------------------------------------------------*         
046400 SQL-INSERT-EXTRACT-USER SECTION.                                         
046500*---------------------------------------------------------------*         
046600     MOVE 'SQL-INSERT-EXTACT-USER  ' TO ABEND-SECTION                     
046700D    DISPLAY ABEND-SECTION                                                
046800     SKIP2                                                                
046900                                                                          
047000     EXEC SQL                                                             
047100       INSERT INTO EXTRACT_USER                                           
047200        VALUES (                                                          
047300              :W-NEW-EXTRACT,                                             
047400              :EXTUSER-USERID,                                            
047500              :EXTUSER-AUTH                                               
047600               )                                                          
047700     END-EXEC                                                             
047800     PERFORM S95-CONTROL-SQLCODE                                          
047900     CONTINUE.                                                            
048000*--------------------------------------------------------------           
048100 SQL-COMMIT SECTION.                                                      
048200*--------------------------------------------------------------           
048300     MOVE 'SQL-COMMIT                  ' TO ABEND-SECTION                 
048400D    DISPLAY ABEND-SECTION                                                
048500     SKIP2                                                                
048600     EXEC SQL                                                             
048700         COMMIT                                                           
048800     END-EXEC                                                             
048900     PERFORM S95-CONTROL-SQLCODE                                          
049000     CONTINUE.                                                            
049100*-------------------------------- DB2 ERROR HANDLING                      
049200*    -COPY V161PS                                                         
049300*++INCLUDE V161PSCCC0                                                     
049400                                                                          
